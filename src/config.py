from pathlib import Path
import os

DB_PATH = Path("/app/data/papers.lancedb")
OPENAI_API_KEY = os.getenv("OPENAI_API_KEY")
EMBEDDING_MODEL = "text-embedding-3-small"
EMBEDDING_DIM = 1536
