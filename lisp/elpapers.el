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

(require 'elfeed)
(require 'elpapers-api)

;;; Customization

(defgroup elpapers nil
  "Elfeed integration with vector database."
  :group 'elfeed
  :prefix "elpapers-")

(defcustom elpapers-auto-ingest-tags '(papers)
  "List of tags that trigger automatic ingestion of new entries."
  :group 'elpapers
  :type '(repeat symbol))

;;; Helper functions

(defun elpapers-extract-paper-id (url)
  "Extract canonical paper ID from URL.
For arXiv URLs, returns the arXiv ID (e.g., '2301.12345v2')."
  (cond
   ((string-match "arxiv\\.org/\\(?:abs\\|pdf\\)/\\([0-9]+\\.[0-9]+\\(?:v[0-9]+\\)?\\)" url)
    (match-string 1 url))
   (t url)))

(defun elpapers-ingest-entry (entry callback)
  "Ingest ENTRY into the vector database.
Calls CALLBACK with (success result) when complete."
  (let* ((url (elfeed-entry-link entry))
         (title (elfeed-entry-title entry))
         (elfeed-id (elfeed-entry-id entry))
         (paper-id (elpapers-extract-paper-id url))
         (content (elfeed-entry-content entry))
         (abstract (if (elfeed-ref-p content)
                      (elfeed-deref content)
                    content)))

    (if (and url title)
        (let ((data `((id . ,(format "%s" paper-id))
                     (elfeed_id . ,(format "%s" elfeed-id))
                     (title . ,title)
                     (abstract . ,(or abstract ""))
                     (source_type . "arxiv_elfeed")
                     (url . ,url))))
          (elpapers-api-ingest-paper data callback))
      (funcall callback nil "No URL or title"))))

;;; Interactive functions

(defun elpapers-ingest-entries (&optional entries)
  "Ingest selected elfeed entries into vector database.
If ENTRIES is provided, use those instead of the selected entries.
In show mode: ingests current entry.
In search mode: ingests all selected entries.
Tags successfully ingested entries with '+vectorized'."
  (interactive)
  (let ((entries
         (cond
          (entries entries)
          ((derived-mode-p 'elfeed-show-mode)
           (list elfeed-show-entry))
          ((derived-mode-p 'elfeed-search-mode)
           (elfeed-search-selected))
          (t (user-error "Not in an Elfeed buffer")))))

    (message "Ingesting %d entries..." (length entries))
    (dolist (entry entries)
      (elpapers-ingest-entry
       entry
       (lambda (success _result)
         (when success
           (message "✓ Vectorized: %s" (elfeed-entry-title entry))
           (elfeed-tag entry 'vectorized)))))
    (elfeed-db-save)))

;;; Automatic ingestion hook

(defun elpapers-auto-ingest-hook (entry)
  "Automatically ingest ENTRY if tagged with configured tags.
Add to `elfeed-new-entry-hook' for automatic operation."
  (let ((tags (elfeed-entry-tags entry)))
    (when (and (seq-some (lambda (tag) (memq tag tags))
                        elpapers-auto-ingest-tags)
              (not (memq 'vectorized tags)))
      (elpapers-ingest-entry
       entry
       (lambda (success _result)
         (when success
           (elfeed-tag entry 'vectorized)
           (elfeed-db-save)))))))

(defun elpapers-enable-auto-ingest ()
  "Enable automatic ingestion of new entries."
  (interactive)
  (add-hook 'elfeed-new-entry-hook #'elpapers-auto-ingest-hook)
  (message "ElPapers auto-ingest enabled for tags: %s"
           elpapers-auto-ingest-tags))

(defun elpapers-disable-auto-ingest ()
  "Disable automatic ingestion of new entries."
  (interactive)
  (remove-hook 'elfeed-new-entry-hook #'elpapers-auto-ingest-hook)
  (message "ElPapers auto-ingest disabled"))

(provide 'elpapers)

;;; elpapers.el ends here
