from fastapi import APIRouter, HTTPException
from src.db.connection import db_manager
from src.api.models import PaperIngest
from src.core.embeddings import embedder
import logging

logger = logging.getLogger(__name__)
router = APIRouter(prefix="/papers", tags=["papers"])
embedder = embedder

@router.post("/ingest-test")
def vectorize_paper(paper: PaperIngest):
    abstract_vector = embedder.embed(paper.abstract)
    return abstract_vector

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

@router.post("/ingest")
def ingest_paper(paper: PaperIngest):
    """
    Ingest a paper: embed abstract and/or full text, store paper + vectors in LanceDB.
    Requires at least one of abstract or full_text to be present and embeddable.
    """
    try:
        logger.info("Attempting embed.")
        abstract_vector = None
        full_text_vectors = None
        
        # Try to embed abstract
        if paper.abstract:
            try:
                logger.info(f"Embedding abstract for paper: {paper.id}")
                abstract_vector = embedder.embed(paper.abstract)
            except Exception as e:
                logger.warning(f"Failed to embed abstract for {paper.id}: {str(e)}")
        
        # TODO: Try to embed full text
       
        # At least one embedding must succeed
        if abstract_vector is None and full_text_vectors is None:
            raise HTTPException(
                status_code=400,
                detail="Paper must have either abstract or full_text, and at least one must be embeddable"
            )
        
        # Prepare the record for LanceDB
        record = {
            "id": paper.id,
            "elfeed_feed_id": paper.elfeed_feed_id,
            "elfeed_entry_id": paper.elfeed_entry_id,
            "title": paper.title,
            "source_type": paper.source_type,
            "url": paper.url,
            "abstract": paper.abstract,
            "abstract_vector": abstract_vector,
            "full_text": paper.full_text,
            # "full_text_vectors": full_text_vectors,
        }
        logger.info(f"Successfully prepared a record for paper {paper.id}")
        
        # Insert into the "sources" table
        table = db_manager.db.open_table("sources")
        table.merge_insert("id") \
            .when_matched_update_all() \
            .when_not_matched_insert_all() \
            .execute([record])
        
        logger.info(f"Successfully ingested paper: {paper.id}")
        return {
            "status": "success",
            "paper_id": paper.id,
            "title": paper.title,
            "message": f"Paper '{paper.title}' ingested and embedded."
        }
    
    except Exception as e:
        logger.error(f"Error ingesting paper {paper.id}: {str(e)}")
        raise HTTPException(status_code=500, detail=f"Ingestion failed: {str(e)}")


@router.get("/{paper_id}")
def get_paper(paper_id: str):
    """
    Fetch a paper by ID from LanceDB.
    """
    try:
        table = db_manager.db.open_table("sources")
        results = table.search().where(f"id = '{paper_id}'").limit(1).to_list()
        
        if not results:
            raise HTTPException(status_code=404, detail=f"Paper {paper_id} not found")
        
        paper = results[0]
        return {
            "id": paper["id"],
            "title": paper["title"],
            "abstract": paper["abstract"],
            "source_type": paper["source_type"],
            "url": paper["url"],
        }
    
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error fetching paper {paper_id}: {str(e)}")
        raise HTTPException(status_code=500, detail=f"Fetch failed: {str(e)}")
