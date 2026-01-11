from fastapi import FastAPI
import lancedb

app = FastAPI()

@app.get("/")
def root():
    return {"status": "ok"}

@app.get("/health")
def health():
    return {"status": "yummy noodles"}
