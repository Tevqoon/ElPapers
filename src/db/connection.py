import lancedb
from src.config import DB_PATH
from src.db.schema import sources_schema
import logging

logger = logging.getLogger(__name__)

class DatabaseManager:
    def __init__(self):
        self.db = None
    
    def connect(self):
        """Connect to LanceDB and ensure tables exist."""
        try:
            logger.info(f"Connecting to database at {DB_PATH}")
            self.db = lancedb.connect(str(DB_PATH))
            self._ensure_tables()
            logger.info("Database connection successful")
        except Exception as e:
            logger.error(f"Failed to connect to database: {e}")
            raise
    
    def _ensure_tables(self):
        """Create tables if they don't exist."""
        try:
            existing_tables = self.db.table_names()
            logger.info(f"Existing tables: {existing_tables}")
            
            if "sources" not in existing_tables:
                logger.info("Creating 'sources' table with schema")
                self.db.create_table("sources", schema=sources_schema, mode="create")
                logger.info("'sources' table created successfully")
            else:
                logger.info("'sources' table already exists")
                
        except Exception as e:
            logger.error(f"Error ensuring tables: {e}")
            raise
    
    def close(self):
        """Close database connection."""
        if self.db:
            self.db.close()
            logger.info("Database connection closed")

db_manager = DatabaseManager()

