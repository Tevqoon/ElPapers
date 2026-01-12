#!/usr/bin/env python3
"""
ElPapers MCP Server
Provides semantic search interface to ElPapers vector database via MCP protocol.
This server runs on the host machine and proxies requests to the FastAPI service.
"""

import sys
import httpx
from mcp.server.fastmcp import FastMCP

# Debug logging to stderr (safe for STDIO transport)
print("Starting ElPapers MCP server...", file=sys.stderr)

# Configuration
API_BASE = "http://localhost:8000"
API_TIMEOUT = 30.0

# Create FastMCP server
mcp = FastMCP("elpapers")

print("FastMCP server initialized", file=sys.stderr)


@mcp.tool()
def search_papers(query: str, top_k: int = 10) -> str:
    """
    Search academic papers by semantic similarity.
    
    Returns papers most relevant to the query based on abstract embeddings.
    Use this to find papers related to specific topics, methods, or research questions.
    
    Args:
        query: Natural language search query describing the topic or concept
        top_k: Number of results to return (1-50, default 10)
    
    Returns:
        Formatted list of matching papers with titles, IDs, and similarity scores
    """
    try:
        print(f"Searching for: {query} (top_k={top_k})", file=sys.stderr)
        
        with httpx.Client(timeout=API_TIMEOUT) as client:
            response = client.get(
                f"{API_BASE}/search/semantic",
                params={"query": query, "top_k": min(max(top_k, 1), 50)}
            )
            response.raise_for_status()
            results = response.json()
        
        if not results:
            return f"No papers found matching '{query}'"
        
        output = [f"Found {len(results)} papers matching '{query}':\n"]
        for i, paper in enumerate(results, 1):
            output.append(f"{i}. {paper['title']}")
            output.append(f"   ID: {paper['id']}")
            output.append(f"   Similarity: {paper['similarity_score']:.4f}")
            if paper.get('elfeed_id'):
                output.append(f"   Elfeed ID: {paper['elfeed_id']}")
            output.append("")
        
        return "\n".join(output)
    
    except httpx.HTTPError as e:
        error_msg = f"API request failed: {str(e)}"
        if hasattr(e, 'response') and e.response is not None:
            error_msg += f"\nStatus: {e.response.status_code}"
        print(error_msg, file=sys.stderr)
        return f"Error: {error_msg}"
    
    except Exception as e:
        print(f"Unexpected error: {str(e)}", file=sys.stderr)
        return f"Error: {str(e)}"


@mcp.tool()
def get_paper(paper_id: str) -> str:
    """
    Get detailed information about a specific paper by its ID.
    
    Use this after search_papers to get full details including abstract and URL.
    
    Args:
        paper_id: The paper ID (e.g., arXiv ID like '2301.12345')
    
    Returns:
        Full paper details including title, abstract, URL, and source type
    """
    try:
        print(f"Fetching paper: {paper_id}", file=sys.stderr)
        
        with httpx.Client(timeout=API_TIMEOUT) as client:
            response = client.get(f"{API_BASE}/papers/{paper_id}")
            response.raise_for_status()
            paper = response.json()
        
        output = [
            f"{paper['title']}\n",
            f"ID: {paper['id']}",
            f"Source: {paper.get('source_type', 'unknown')}",
        ]
        
        if paper.get('url'):
            output.append(f"URL: {paper['url']}")
        
        if paper.get('abstract'):
            output.append(f"\nAbstract:\n{paper['abstract']}")
        
        return "\n".join(output)
    
    except httpx.HTTPError as e:
        error_msg = f"API request failed: {str(e)}"
        if hasattr(e, 'response') and e.response is not None:
            error_msg += f"\nStatus: {e.response.status_code}"
        print(error_msg, file=sys.stderr)
        return f"Error: {error_msg}"
    
    except Exception as e:
        print(f"Unexpected error: {str(e)}", file=sys.stderr)
        return f"Error: {str(e)}"


@mcp.tool()
def papers_stats() -> str:
    """
    Get statistics about the papers database.
    
    Returns:
        Summary of database contents including total paper count
    """
    try:
        print("Fetching stats", file=sys.stderr)
        
        with httpx.Client(timeout=API_TIMEOUT) as client:
            response = client.get(f"{API_BASE}/papers/stats")
            response.raise_for_status()
            stats = response.json()
        
        return f"Papers database contains {stats['total_papers']} papers"
    
    except httpx.HTTPError as e:
        error_msg = f"API request failed: {str(e)}"
        if hasattr(e, 'response') and e.response is not None:
            error_msg += f"\nStatus: {e.response.status_code}"
        print(error_msg, file=sys.stderr)
        return f"Error: {error_msg}"
    
    except Exception as e:
        print(f"Unexpected error: {str(e)}", file=sys.stderr)
        return f"Error: {str(e)}"


if __name__ == "__main__":
    print("Running MCP server on stdio...", file=sys.stderr)
    # Run with stdio transport (for Claude Desktop)
    mcp.run(transport="stdio")
