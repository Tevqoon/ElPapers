from pydantic import BaseModel
from typing import Optional

class PaperIngest(BaseModel):
    id: str
    title: str
    abstract: str
    full_text: Optional[str] = None
    source_type: str
    url: Optional[str] = None

class SearchResult(BaseModel):
    id: str
    title: str
    similarity_score: float
