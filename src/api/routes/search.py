from fastapi import APIRouter, HTTPException, Query
from src.core.embeddings import embedder
from src.db.connection import db_manager
from src.api.models import SearchResult
import logging

logger = logging.getLogger(__name__)
router = APIRouter(prefix="/search", tags=["search"])
embedder = embedder

@router.get("/semantic")
def semantic_search(query: str, top_k: int = Query(default=10, ge=1, le=100)):
    """Search papers by semantic similarity to query string."""
    try:
        # Embed the query
        query_vector = embedder.embed(query)
        
        # Search LanceDB - specify which vector column to use
        table = db_manager.db.open_table("sources")
        results = table.search(query_vector, vector_column_name="embeddings") \
                      .limit(top_k) \
                      .to_list()
        
        return [{
            "id": r["id"],
            "elfeed_feed_id": r.get("elfeed_feed_id"),
            "elfeed_entry_id": r.get("elfeed_entry_id"),
            "title": r["title"],
            "similarity_score": r["_distance"]
        } for r in results]
        
    except Exception as e:
        logger.error(f"Error in semantic search: {str(e)}")
        raise HTTPException(status_code=500, detail=f"Search failed: {str(e)}")
