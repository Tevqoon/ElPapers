from fastapi import APIRouter
from src.db.connection import db_manager
from src.api.models import PaperIngest

router = APIRouter(prefix="/papers", tags=["papers"])

@router.post("/ingest")
def ingest_paper(paper: PaperIngest):
    # Handle paper ingestion with embeddings
    pass

@router.get("/{paper_id}")
def get_paper(paper_id: str):
    # Fetch paper from DB
    pass
