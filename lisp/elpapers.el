;;; elpapers.el --- Elfeed-first vectorization and RAG system for discovery -*- lexical-binding: t; -*-

;; Author: Jure Smolar
;; URL: https://github.com/Tevqoon/ElPapers
;; Version: 0.1
;; Package-Requires: ((emacs "30.1") (elfeed "3.4") (request "0.3.0"))
;; Keywords: elfeed, vector-database, rag, academic-papers

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; ElPapers integrates Elfeed with LanceDB vector storage and RAG capabilities
;; for semantic search of academic papers. Papers tagged with "+papers" in
;; Elfeed are ingested into a vector database running as a FastAPI service.
;;
;; Features:
;; - Semantic search across arXiv papers
;; - RAG-powered context assembly for LLM interactions
;; - MCP integration for chat-based paper discovery
;;
;; Setup:
;; 1. Start the Docker service: docker-compose up
;; 2. Configure your Elfeed feeds
;; 3. Tag papers with "+papers" to ingest them

;;; Code:
