import httpx
from bs4 import BeautifulSoup
from typing import Optional, List
import logging

logger = logging.getLogger(__name__)


def fetch_arxiv_html(paper_id: str, timeout: float = 30.0) -> Optional[str]:
    """
    Fetch HTML version of paper from arXiv using paper ID.
    
    Args:
        paper_id: Paper ID (e.g., '2601.04814' or '2601.04814v1')
    
    Returns:
        HTML content or None if not available
    """
    base_id = paper_id.split('v')[0]
    html_url = f"https://arxiv.org/html/{base_id}"
    
    try:
        with httpx.Client(timeout=timeout, follow_redirects=True) as client:
            response = client.get(html_url)
            
            if response.status_code == 404:
                logger.info(f"HTML not available for {paper_id}")
                return None
            
            response.raise_for_status()
            return response.text
    
    except Exception as e:
        logger.error(f"Error fetching HTML for {paper_id}: {e}")
        return None


def chunk_text(text: str, max_tokens: int = 400) -> List[str]:
    """
    Chunk text using rolling window on paragraph boundaries.
    
    Args:
        text: Text to chunk
        max_tokens: Target max tokens per chunk (~4 chars per token)
    
    Returns:
        List of text chunks
    """
    max_chars = max_tokens * 4
    paragraphs = [p.strip() for p in text.split('\n\n') if p.strip()]
    
    if not paragraphs:
        return []
    
    chunks = []
    current = ""
    
    for para in paragraphs:
        if len(current) + len(para) + 2 > max_chars:
            if current:
                chunks.append(current.strip())
            current = para + '\n\n'
        else:
            current += para + '\n\n'
    
    if current.strip():
        chunks.append(current.strip())
    
    return chunks


def chunk_by_paper_id(paper_id: str, max_tokens: int = 400) -> Optional[List[str]]:
    """
    Fetch arXiv paper HTML and chunk into pieces for embedding.
    
    Args:
        paper_id: Paper ID like '2601.04814'
        max_tokens: Target max tokens per chunk
    
    Returns:
        List of text chunks or None if HTML unavailable
    """
    html = fetch_arxiv_html(paper_id)
    if not html:
        return None
    
    # Extract text from HTML
    soup = BeautifulSoup(html, 'html.parser')
    document = soup.find('article', class_='ltx_document')
    
    if not document:
        logger.warning(f"Could not find document for {paper_id}")
        return None
    
    text = document.get_text(separator='\n\n', strip=True)
    
    if not text:
        logger.warning(f"No text extracted for {paper_id}")
        return None
    
    chunks = chunk_text(text, max_tokens=max_tokens)
    logger.info(f"Created {len(chunks)} chunks for {paper_id}")
    
    return chunks if chunks else None
