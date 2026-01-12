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

    def embed_batch(self, texts: list[str]) -> list[list[float]]:
        """
        Embed multiple texts in a single API call.
        Returns a list of embedding vectors in the same order as input texts.
        
        OpenAI's API supports up to 2048 inputs per request for text-embedding-3-small.
        For larger batches, consider chunking the request.
        """
        if not texts:
            return []
        
        try:
            logger.info(f"Batch embedding {len(texts)} texts")
            response = self.client.embeddings.create(
                input=texts,
                model=EMBEDDING_MODEL
            )
            
            # Response data is ordered by index field
            sorted_data = sorted(response.data, key=lambda x: x.index)
            embeddings = [item.embedding for item in sorted_data]
            
            logger.info(f"Successfully embedded {len(embeddings)} texts")
            return embeddings
            
        except Exception as e:
            logger.error(f"Error batch embedding texts with OpenAI: {str(e)}")
            raise
        


class OllamaProvider(EmbeddingProvider):
    # Placeholder for Ollama migration path
    def embed(self, text: str) -> list[float]:
        raise NotImplementedError("Ollama provider not yet implemented")

# Default provider
embedder = OpenAIProvider()
