;;; elpapers-api.el --- API client for ElPapers service -*- lexical-binding: t; -*-

;; Author: Jure Smolar
;; URL: https://github.com/Tevqoon/ElPapers
;; Version: 0.1
;; Package-Requires: ((emacs "30.1") (request "0.3.0"))
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

;; This module provides Emacs Lisp wrappers around the ElPapers FastAPI
;; service endpoints. It handles HTTP communication, JSON serialization,
;; and error handling for all API operations.

;;; Code:

(require 'request)
(require 'json)

;;; Customization

(defgroup elpapers-api nil
  "API client for ElPapers service."
  :group 'elpapers
  :prefix "elpapers-api-")

(defcustom elpapers-api-base-url "http://localhost:8000"
  "Base URL for the ElPapers API service."
  :type 'string
  :group 'elpapers-api)

(defcustom elpapers-api-timeout 30
  "Timeout in seconds for API requests."
  :type 'integer
  :group 'elpapers-api)

;;; Internal helpers

(defun elpapers-api--url (path)
  "Construct full API URL from PATH."
  (concat elpapers-api-base-url path))

(defun elpapers-api--handle-response (response success-fn error-fn)
  "Handle API RESPONSE, calling SUCCESS-FN or ERROR-FN as appropriate."
  (let ((status (request-response-status-code response))
        (data (request-response-data response)))
    (cond
     ((and (>= status 200) (< status 300))
      (when success-fn
        (funcall success-fn data)))
     (t
      (when error-fn
        (funcall error-fn status data))))))

;;; Health check endpoints

(defun elpapers-api-health-check (callback)
  "Check API health status. Call CALLBACK with result."
  (request
    (elpapers-api--url "/health")
    :type "GET"
    :parser 'json-read
    :timeout elpapers-api-timeout
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (funcall callback t data)))
    :error (cl-function
            (lambda (&key error-thrown &allow-other-keys)
              (funcall callback nil error-thrown)))))

(defun elpapers-api-list-tables (callback)
  "List available database tables. Call CALLBACK with result."
  (request
    (elpapers-api--url "/tables")
    :type "GET"
    :parser 'json-read
    :timeout elpapers-api-timeout
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (funcall callback t data)))
    :error (cl-function
            (lambda (&key error-thrown &allow-other-keys)
              (funcall callback nil error-thrown)))))

;;; Paper ingestion endpoints

(defun elpapers-api-ingest-paper (paper-data callback)
  "Ingest PAPER-DATA into vector database. Call CALLBACK with result.

PAPER-DATA should be an alist with keys:
  - id: unique paper identifier
  - title: paper title
  - abstract: paper abstract
  - source_type: source type (e.g. \"arxiv\")
  - url: paper URL (optional)
  - full_text: full paper text (optional)"
  (request
    (elpapers-api--url "/papers/ingest")
    :type "POST"
    :headers '(("Content-Type" . "application/json"))
    :data (json-encode paper-data)
    :parser 'json-read
    :timeout elpapers-api-timeout
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (funcall callback t data)))
    :error (cl-function
            (lambda (&key error-thrown &allow-other-keys)
              (funcall callback nil error-thrown)))))

(defun elpapers-api-get-paper (paper-id callback)
  "Retrieve paper by PAPER-ID. Call CALLBACK with result."
  (request
    (elpapers-api--url (format "/papers/%s" paper-id))
    :type "GET"
    :parser 'json-read
    :timeout elpapers-api-timeout
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (funcall callback t data)))
    :error (cl-function
            (lambda (&key error-thrown &allow-other-keys)
              (funcall callback nil error-thrown)))))

;;; Search endpoints

(defun elpapers-api-semantic-search (query top-k callback)
  "Perform semantic search with QUERY, returning TOP-K results.
Call CALLBACK with results."
  (request
    (elpapers-api--url "/search/semantic")
    :type "GET"
    :params `(("query" . ,query)
              ("top_k" . ,(number-to-string top-k)))
    :parser 'json-read
    :timeout elpapers-api-timeout
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (funcall callback t data)))
    :error (cl-function
            (lambda (&key error-thrown &allow-other-keys)
              (funcall callback nil error-thrown)))))

;;; Interactive test functions

(defun elpapers-api-test-connection ()
  "Test connection to ElPapers API service."
  (interactive)
  (message "Testing ElPapers API connection...")
  (elpapers-api-health-check

   (lambda (success data)
     (if success
         (message "✓ API is healthy: %s" data)
       (message "✗ API connection failed: %s" data)))))

(defun elpapers-show-stats ()
  "Show statistics about the papers database."
  (interactive)
  (request
    (elpapers-api--url "/papers/stats")
    :type "GET"
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (message "Papers DB: %d total"
                         (alist-get 'total_papers data))))
    :error (cl-function
            (lambda (&key error-thrown &allow-other-keys)
              (message "Failed to get stats: %s" error-thrown)))))

(provide 'elpapers-api)

;;; elpapers-api.el ends here
