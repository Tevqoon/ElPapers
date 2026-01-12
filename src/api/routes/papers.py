from fastapi import APIRouter, HTTPException
from src.db.connection import db_manager
from src.api.models import PaperIngest
from src.core.embeddings import embedder
from src.core.fulltext import chunk_text, chunk_by_paper_id
import logging

logger = logging.getLogger(__name__)
router = APIRouter(prefix="/papers", tags=["papers"])
embedder = embedder
BATCH_LIMIT = 1000

def _get_existing_paper(paper_id: str) -> dict | None:
    """Helper: Retrieve existing paper from database if it exists."""
    try:
        table = db_manager.db.open_table("sources")
        results = (
            table.search()
            .where(f"id = '{paper_id}'")
            .limit(1)
            .to_list()
        )
        return results[0] if results else None
    except Exception:
        return None


    

@router.get("/stats")
def get_stats():
    """Get basic statistics about the papers database."""
    try:
        table = db_manager.db.open_table("sources")
        total_count = table.count_rows()
        
        return {
            "total_papers": total_count
        }
    
    except Exception as e:
        logger.error(f"Error getting stats: {str(e)}")
        raise HTTPException(status_code=500, detail=f"Stats failed: {str(e)}")

@router.get("/allpapers")
def get_all_papers():
    """
    Retrieve all papers from the database.
    Returns list of papers with basic metadata (id, title, source_type, url).
    """
    try:
        table = db_manager.db.open_table("sources")
        results = table.search().limit(None).to_list()
        
        return [{
            "id": r["id"],
            "title": r["title"],
            "embeddings": len(r["embeddings"])
        } for r in results]
    
    except Exception as e:
        logger.error(f"Error retrieving all papers: {str(e)}")
        raise HTTPException(status_code=500, detail=f"Failed to retrieve papers: {str(e)}")

def _decide_embeddings(
    paper: PaperIngest,
    include_fulltext: bool,
) -> list[list[float]]:
    """
    Decide what embeddings to store.
    """
    existing = _get_existing_paper(paper.id)
    
    # Force regeneration
    if include_fulltext:
        embeddings = []
        logger.info(f"Regenerating embeddings for {paper.id}")
        if paper.abstract:
            embeddings.append(embedder.embed(paper.abstract))
        if paper.full_text:
            chunks = chunk_text(paper.full_text)
            embeddings.extend(embedder.embed_batch(chunks))
        # For arXiv entries, pull chunks directly
        elif paper.source_type == "arxiv":
            chunks = chunk_by_paper_id(paper.id)
            embeddings.extend(embedder.embed_batch(chunks))
            
        return embeddings

    # Leave existing embeddings alone
    if existing and existing.get("embeddings"):
        return existing["embeddings"]

    # New or missing embeddings → embed abstract only
    if paper.abstract:
        logger.info(f"Generating new abstract embedding for {paper.id}")
        return [embedder.embed(paper.abstract)]

    return []

@router.post("/ingest")
def ingest_paper(paper: PaperIngest, include_fulltext: bool = False):
    try:
        embeddings = _decide_embeddings(paper, include_fulltext)

        logger.info(f"Ingesting {paper.id} with fulltext: {include_fulltext}")

        if not embeddings:
            raise HTTPException(
                status_code=400,
                detail="No embeddings generated or retained"
            )

        record = {
            "id": paper.id,
            "elfeed_feed_id": paper.elfeed_feed_id,
            "elfeed_entry_id": paper.elfeed_entry_id,
            "title": paper.title,
            "source_type": paper.source_type,
            "url": paper.url,
            "abstract": paper.abstract,
            "full_text": paper.full_text,
            "embeddings": embeddings,
        }

        table = db_manager.db.open_table("sources")
        table.merge_insert("id") \
            .when_matched_update_all() \
            .when_not_matched_insert_all() \
            .execute([record])

        return {
            "status": "success",
            "paper_id": paper.id,
            "embeddings": len(embeddings),
            "regenerated": include_fulltext,
        }

    except HTTPException:
        raise
    except Exception as e:
        logger.exception("Ingest failed")
        raise HTTPException(status_code=500, detail=str(e))
    

@router.get("/{paper_id}")
def get_paper(paper_id: str):
    paper = _get_existing_paper(paper_id)
    if paper:
        return {
        "id": paper["id"],
        "title": paper["title"],
        "abstract": paper["abstract"],
        "source_type": paper["source_type"],
        "url": paper["url"],
        "embeddings": len(paper["embeddings"])
        }

@router.get("/{paper_id}/fulldata")
def get_paper_fulldata(paper_id: str):
    """
    Fetch a paper by ID from LanceDB. Include all fields.
    """
    return _get_existing_paper(paper_id)

@router.post("/ingest_batch")
def ingest_batch(papers: list[PaperIngest], include_fulltext: bool = False):
    """
    Batch ingest multiple papers with automatic deduplication.
    
    Deduplicates by paper ID, keeping first occurrence.
    Returns summary of succeeded/failed ingestions.
    """
    try:
        # Deduplicate by ID, keeping first occurrence
        seen_ids = set()
        unique_papers = []
        duplicates = 0
        
        for paper in papers:
            if paper.id not in seen_ids:
                seen_ids.add(paper.id)
                unique_papers.append(paper)
            else:
                duplicates += 1
        
        if duplicates > 0:
            logger.info(f"Removed {duplicates} duplicate paper IDs from batch")
        
        logger.info(f"Starting batch ingestion of {len(unique_papers)} unique papers")
        
        # Collect all texts to embed
        texts_to_embed = []
        paper_indices = []  # Track which paper each text belongs to
        
        for i, paper in enumerate(unique_papers):
            if paper.abstract:
                texts_to_embed.append(paper.abstract)
                paper_indices.append(i)
        
        # Batch embed all abstracts
        logger.info(f"Batch embedding {len(texts_to_embed)} abstracts")

        embeddings = []
        for i in range(0, len(texts_to_embed), BATCH_LIMIT):
            subbatch = embedder.embed_batch(texts_to_embed[i:i + BATCH_LIMIT])
            embeddings.extend(subbatch)
        
        # Build records with embeddings
        records = []
        results = []
        
        for i, paper in enumerate(unique_papers):
            try:
                # Find this paper's embedding(s)
                paper_embeddings = []
                for j, idx in enumerate(paper_indices):
                    if idx == i:
                        paper_embeddings.append(embeddings[j])
                
                if not paper_embeddings:
                    results.append({
                        "paper_id": paper.id,
                        "status": "failed",
                        "error": "No embeddable content"
                    })
                    continue
                
                record = {
                    "id": paper.id,
                    "elfeed_feed_id": paper.elfeed_feed_id,
                    "elfeed_entry_id": paper.elfeed_entry_id,
                    "title": paper.title,
                    "source_type": paper.source_type,
                    "url": paper.url,
                    "abstract": paper.abstract,
                    "full_text": paper.full_text,
                    "embeddings": paper_embeddings
                }
                
                records.append(record)
                results.append({
                    "paper_id": paper.id,
                    "status": "success"
                })
                
            except Exception as e:
                logger.error(f"Error preparing paper {paper.id}: {str(e)}")
                results.append({
                    "paper_id": paper.id,
                    "status": "failed",
                    "error": str(e)
                })
        
        # Batch insert into LanceDB
        if records:
            logger.info(f"Inserting {len(records)} records into database")
            table = db_manager.db.open_table("sources")
            table.merge_insert("id") \
                .when_matched_update_all() \
                .when_not_matched_insert_all() \
                .execute(records)
        
        succeeded = sum(1 for r in results if r["status"] == "success")
        failed = len(results) - succeeded
        
        logger.info(f"Batch ingestion complete: {succeeded} succeeded, {failed} failed")
        
        return {
            "total_submitted": len(papers),
            "duplicates_removed": duplicates,
            "succeeded": succeeded,
            "failed": failed,
            "results": results
        }
    
    except Exception as e:
        logger.error(f"Batch ingestion failed catastrophically: {str(e)}")
        raise HTTPException(status_code=500, detail=f"Batch ingestion failed: {str(e)}")
