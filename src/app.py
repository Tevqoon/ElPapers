from fastapi import FastAPI
import lancedb
import pyarrow as pa

app = FastAPI()
db = lancedb.connect("/app/data/papers.lancedb")

@app.on_event("startup")
def startup():
    print("MESSAGE: I'm singing in the rain!")

@app.get("/")
def root():
    return {"status": "ok"}

@app.get("/health")
def health():
    return {"status": "yummy noodles"}

@app.get("/tables")
def list_tables():
    return {"tables": db.table_names()}


