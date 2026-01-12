from fastapi import APIRouter

router = APIRouter(tags=["health"])

@router.get("/")
def root():
    return {"status": "ok"}

@router.get("/health")
def health():
    return {"status": "yummy noodles"}
