from abc import ABC, abstractmethod
import openai
from src.config import OPENAI_API_KEY, EMBEDDING_MODEL

class EmbeddingProvider(ABC):
    @abstractmethod
    def embed(self, text: str) -> list[float]:
        pass

class OpenAIProvider(EmbeddingProvider):
    def __init__(self):
        openai.api_key = OPENAI_API_KEY
    
    def embed(self, text: str) -> list[float]:
        response = openai.Embedding.create(
            input=text,
            model=EMBEDDING_MODEL
        )
        return response["data"][0]["embedding"]

class OllamaProvider(EmbeddingProvider):
    # Placeholder for Ollama migration path
    pass

# Default provider
embedder = OpenAIProvider()
