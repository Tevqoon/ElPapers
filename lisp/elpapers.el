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

(defun elpapers-ingest-entry (entry callback &optional include-fulltext)
  "Ingest ENTRY into the vector database.
Calls CALLBACK with (success result) when complete.
Passes INCLUDE-FULLTEXT along."
  (let* ((url (elfeed-entry-link entry))
         (title (elfeed-entry-title entry))
         (elfeed-id (elfeed-entry-id entry))
         (feed-id (car elfeed-id))
         (entry-id (cdr elfeed-id))
         (paper-id (elpapers-extract-paper-id url))
         (content (elfeed-entry-content entry))
         (abstract (if (elfeed-ref-p content)
                      (elfeed-deref content)
                    content)))

    (if (and url title)
        (let ((data `((id . ,(format "%s" paper-id))
                     (elfeed_feed_id . ,feed-id)
                     (elfeed_entry_id . ,entry-id)
                     (title . ,title)
                     (abstract . ,(or abstract ""))
                     (source_type . "arxiv")
                     (url . ,url))))
          (elpapers-api-ingest-paper data callback include-fulltext))
      (funcall callback nil "No URL or title"))))

;;; Interactive functions

(defun elpapers-ingest-entries (&optional entries include-fulltext)
  "Ingest selected elfeed ENTRIES into vector database.

With prefix argument (C-u), force full-text vectorization even if
'+full-vectorized' is already present.

When INCLUDE-FULLTEXT is true, also signal fulltext ingestion."
  (interactive
   (list nil current-prefix-arg))

  (let* ((entries
          (cond
           (entries entries)
           ((derived-mode-p 'elfeed-show-mode)
            (list elfeed-show-entry))
           ((derived-mode-p 'elfeed-search-mode)
            (elfeed-search-selected))
           (t (user-error "Not in an Elfeed buffer"))))

         ;; Force overrides everything
         (force-fulltext include-fulltext))

    (message "Ingesting %d entries..." (length entries))

    (dolist (entry entries)
      ;; Snapshot tags *once*
      (let* ((tags (elfeed-entry-tags entry))
             (has-papers (memq 'papers tags))
             (already-full (memq 'full-vectorized tags))
             ;; Decide intent BEFORE async call
             (do-fulltext (and has-papers
                               (or force-fulltext
                                   (not already-full)))))

        (elpapers-ingest-entry
         entry
         (lambda (success _result)
           (when success
             ;; Always mark vectorized
             (elfeed-tag entry 'vectorized)

             (when do-fulltext
               (elfeed-tag entry 'full-vectorized)
               (message "✓ Full-text vectorized: %s"
                        (elfeed-entry-title entry)))

             (unless do-fulltext
               (message "✓ Vectorized: %s"
                        (elfeed-entry-title entry)))))
         do-fulltext)))

    (elfeed-db-save)))


(defun elpapers-ingest-full ()
  "Wrapper for elpaper-ingest-entries to automatically"
  (interactive)
  (elpapers-ingest-entries nil t))

(defun elpapers-ingest-batch ()
  "Batch ingest entries into vector database (abstracts only).
Sends all selected entries in a single API call."
  (interactive)
  (let ((entries (cond
                  ((derived-mode-p 'elfeed-show-mode) (list elfeed-show-entry))
                  ((derived-mode-p 'elfeed-search-mode) (elfeed-search-selected))
                  (t (user-error "Not in an Elfeed buffer")))))
    
    (when (null entries)
      (user-error "No entries selected"))
    
    (message "Preparing %d entries for batch ingestion..." (length entries))
    
    ;; Convert to API format
    (let ((papers-data
           (mapcar
            (lambda (entry)
              (let* ((url (elfeed-entry-link entry))
                     (elfeed-id (elfeed-entry-id entry))
                     (paper-id (elpapers-extract-paper-id url))
                     (content (elfeed-entry-content entry))
                     (abstract (if (elfeed-ref-p content)
                                   (elfeed-deref content)
                                 content)))
                `((id . ,paper-id)
                  (elfeed_feed_id . ,(car elfeed-id))
                  (elfeed_entry_id . ,(cdr elfeed-id))
                  (title . ,(elfeed-entry-title entry))
                  (abstract . ,(or abstract ""))
                  (source_type . "arxiv_elfeed")
                  (url . ,url))))
            entries)))
      
      ;; Single API call for all papers
      (message "Sending batch request for %d papers..." (length papers-data))
      (elpapers-api-ingest-batch
       papers-data
       (lambda (success result)
         (if success
             (let ((succeeded (alist-get 'succeeded result))
                   (failed (alist-get 'failed result)))
               (message "✓ Batch ingestion complete: %d succeeded, %d failed"
                        succeeded failed))
           (message "✗ Batch ingestion failed: %s" result)))))))

;;; Searching

(defun elpapers-semantic-search (query &optional top-k)
  "Search for papers semantically similar to QUERY and display in elfeed buffer.
TOP-K defaults to 20."
  (interactive "sSearch query: ")
  (let ((top-k (or top-k 20)))
    (message "Searching for: %s" query)
    (elpapers-api-semantic-search
     query
     top-k
     (lambda (success results)
       (if success
           (elpapers--display-search-results results query)
         (message "Search failed: %s" results))))))

(defun elpapers--display-search-results (results query)
  "Display search RESULTS in elfeed buffer with QUERY as filter description."
  (let* ((results-list (append results nil))
         (entries
          (delq nil
                (mapcar (lambda (r)
                          (let ((feed-id (cdr (assq 'elfeed_feed_id r)))
                                (entry-id (cdr (assq 'elfeed_entry_id r))))
                            (when (and feed-id entry-id)
                              (let* ((elfeed-id (cons feed-id entry-id))
                                     (entry (elfeed-db-get-entry elfeed-id)))
                                entry))))
                        results-list))))
    
    (if entries
        (with-current-buffer (elfeed-search-buffer)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (setq elfeed-search-entries entries)
            (setq elfeed-search-filter (format "semantic: %s" query))
            (dolist (entry entries)
              (funcall elfeed-search-print-entry-function entry)
              (insert "\n")))
          (goto-char (point-min))
          (switch-to-buffer (current-buffer))
          (message "Found %d results for: %s" (length entries) query))
      (message "No elfeed entries found for these papers"))))

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
