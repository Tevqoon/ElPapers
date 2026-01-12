from abc import ABC, abstractmethod
from openai import OpenAI
from src.config import OPENAI_API_KEY, EMBEDDING_MODEL
import logging

logger = logging.getLogger(__name__)


class EmbeddingProvider(ABC):
    @abstractmethod
    def embed(self, text: str) -> list[float]:
        pass


class OpenAIProvider(EmbeddingProvider):
    def __init__(self):
        self.client = OpenAI(api_key=OPENAI_API_KEY)
    
    def embed(self, text: str) -> list[float]:
        """
        Embed text using OpenAI API.
        Returns a list of floats representing the embedding vector.
        """
        try:
            response = self.client.embeddings.create(
                input=text,
                model=EMBEDDING_MODEL
            )
            return response.data[0].embedding
        except Exception as e:
            logger.error(f"Error embedding text with OpenAI: {str(e)}")
            raise


class OllamaProvider(EmbeddingProvider):
    # Placeholder for Ollama migration path
    def embed(self, text: str) -> list[float]:
        raise NotImplementedError("Ollama provider not yet implemented")

# Default provider
embedder = OpenAIProvider()
