from fastapi import APIRouter
from src.core.embeddings import embedder
from src.db.connection import db_manager
from src.api.models import SearchResult

router = APIRouter(prefix="/search", tags=["search"])

def _search_by_vector(vector: list[float], top_k: int = 10) -> list[dict]:
    """Internal vector search against LanceDB."""
    table = db_manager.db.open_table("sources")
    results = table.search(vector).limit(top_k).to_list()
    return results

@router.get("/semantic")
def semantic_search(query: str, top_k: int = 10) -> list[SearchResult]:
    """Search papers by semantic similarity to query string."""
    vector = embedder.embed(query)
    results = _search_by_vector(vector, top_k)
    return [
        SearchResult(
            id=r["id"],
            title=r["title"],
            similarity_score=r["_distance"]
        )
        for r in results
    ]
