from fastapi import FastAPI
from src.db.connection import db_manager
from src.api.routes import health, papers, search

app = FastAPI()

@app.on_event("startup")
def startup():
    db_manager.connect()
    print("Database connected!")

# Register routers
app.include_router(health.router)
app.include_router(papers.router)
app.include_router(search.router)
