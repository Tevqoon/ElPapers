import lancedb
from src.config import DB_PATH
from src.db.schema import sources_schema

class DatabaseManager:
    def __init__(self):
        self.db = None
    
    def connect(self):
        self.db = lancedb.connect(str(DB_PATH))
        self._ensure_tables()
    
    def _ensure_tables(self):
        if "sources" not in self.db.table_names():
            self.db.create_table("sources", schema=sources_schema, mode="overwrite")
    
    def close(self):
        if self.db:
            self.db.close()

db_manager = DatabaseManager()
