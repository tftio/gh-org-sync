;;; gh-org-docs.el --- Sync org-mode documents with Google Docs -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: James Felix Black
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (org "9.7"))
;; Keywords: outlines, docs, collaboration
;; URL: https://github.com/tftio/gh-org-sync
;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Sync org-mode documents with Google Docs for collaboration.
;;
;; Google Docs is used as a collaboration surface for comments only.
;; Org-mode remains the single source of truth.

;;; Code:

(require 'org)
(require 'org-element)
(require 'subr-x)

(defgroup gh-org-docs nil
  "Sync org-mode with Google Docs."
  :group 'org
  :prefix "gh/org-docs-")

(defcustom gh/org-docs-credentials-path
  (expand-file-name "~/.config/org2gdocs/credentials.json")
  "Path to Google API credentials file.

Note: org2gdocs-api primarily uses its own XDG TOML config; this variable
exists for compatibility with the original plan."
  :type 'file
  :group 'gh-org-docs)

(defcustom gh/org-docs-token-path
  (expand-file-name "~/.local/share/org2gdocs/token.json")
  "Path to OAuth token file.

Note: org2gdocs-api primarily uses its own XDG TOML config; this variable
exists for compatibility with the original plan."
  :type 'file
  :group 'gh-org-docs)

(defcustom gh/org-docs-api-command "org2gdocs-api"
  "Command to invoke the Google Docs API tool."
  :type 'string
  :group 'gh-org-docs)

(defcustom gh/org-docs-debug-mode nil
  "Enable debug logging when non-nil."
  :type 'boolean
  :group 'gh-org-docs)

(defcustom gh/org-docs-auto-enable 'linked
  "When to auto-enable `gh/org-docs-mode' in org buffers.
- `linked': Only for files with GDOC_ID (already linked to Google Doc)
- `always': For all org-mode files
- `nil': Never auto-enable (manual activation only)"
  :type '(choice (const :tag "Only linked files" linked)
                 (const :tag "All org files" always)
                 (const :tag "Never" nil))
  :group 'gh-org-docs)

(defvar gh/org-docs--debug-buffer "*gh-org-docs-debug*"
  "Buffer name for debug output.")

(defun gh/org-docs--debug (format-string &rest args)
  "Log debug message if `gh/org-docs-debug-mode' is enabled."
  (when gh/org-docs-debug-mode
    (with-current-buffer (get-buffer-create gh/org-docs--debug-buffer)
      (goto-char (point-max))
      (insert (format "[%s] " (format-time-string "%H:%M:%S")))
      (insert (apply #'format format-string args))
      (insert "\n"))))

(defun gh/org-docs-toggle-debug ()
  "Toggle debug mode."
  (interactive)
  (setq gh/org-docs-debug-mode (not gh/org-docs-debug-mode))
  (message "gh-org-docs debug mode: %s"
           (if gh/org-docs-debug-mode "enabled" "disabled"))
  (when gh/org-docs-debug-mode
    (display-buffer (get-buffer-create gh/org-docs--debug-buffer))))

(defun gh/org-docs-clear-debug-log ()
  "Clear the debug log buffer."
  (interactive)
  (when (get-buffer gh/org-docs--debug-buffer)
    (with-current-buffer gh/org-docs--debug-buffer
      (erase-buffer))
    (message "Debug log cleared")))

;;;###autoload
(define-minor-mode gh/org-docs-mode
  "Minor mode for syncing org documents with Google Docs."
  :lighter " GDoc"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c g s") #'gh/org-docs-dwim)
            (define-key map (kbd "C-c g p") #'gh/org-docs-push)
            (define-key map (kbd "C-c g f") #'gh/org-docs-pull)
            (define-key map (kbd "C-c g c") #'gh/org-docs-clean)
            (define-key map (kbd "C-c g o") #'gh/org-docs-open-in-browser)
            (define-key map (kbd "C-c g d") #'gh/org-docs-toggle-debug)
            map))

;; ============================================================================
;; S-expression encoding/decoding
;; ============================================================================

(defun gh/org-docs--sexp-encode (obj)
  "Encode OBJ as an S-expression string."
  (cond
   ((null obj) "nil")
   ((eq obj t) "t")
   ((symbolp obj)
    (symbol-name obj))
   ((stringp obj)
    (gh/org-docs--encode-string obj))
   ((numberp obj)
    (number-to-string obj))
   ((and (consp obj) (not (listp (cdr obj))))
    ;; Dotted pair (cons cell)
    (format "(%s . %s)"
            (gh/org-docs--sexp-encode (car obj))
            (gh/org-docs--sexp-encode (cdr obj))))
   ((listp obj)
    (format "(%s)"
            (mapconcat #'gh/org-docs--sexp-encode obj " ")))
   (t (error "Cannot encode object type: %S" (type-of obj)))))

(defun gh/org-docs--encode-string (s)
  "Encode string S with proper escaping for S-expression."
  (with-temp-buffer
    (insert "\"")
    (let ((i 0))
      (while (< i (length s))
        (let ((char (aref s i)))
          (pcase char
            (?\" (insert "\\\""))
            (?\\ (insert "\\\\"))
            (?\n (insert "\\n"))
            (?\t (insert "\\t"))
            (?\r (insert "\\r"))
            (_ (insert char))))
        (setq i (1+ i))))
    (insert "\"")
    (buffer-string)))

(defun gh/org-docs--sexp-decode (str)
  "Decode S-expression string STR to Emacs Lisp object."
  (car (read-from-string str)))

;; ============================================================================
;; File metadata helpers
;; ============================================================================

(defun gh/org-docs--get-file-property (key)
  "Return file keyword KEY value, or nil.

KEY is provided without the leading #+ prefix (e.g., \"GDOC_ID\")."
  (save-excursion
    (goto-char (point-min))
    (let ((re (format "^#\\+%s: \\(.+\\)$" (regexp-quote key))))
      (when (re-search-forward re nil t)
        (string-trim (match-string 1))))))

(defun gh/org-docs--set-file-property (key value)
  "Set file keyword KEY to VALUE.

If KEY exists, update it. Otherwise insert it after #+TITLE if present,
else at the start of the buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((re (format "^#\\+%s: " (regexp-quote key))))
      (if (re-search-forward re nil t)
          (progn
            (delete-region (point) (line-end-position))
            (insert value))
        (goto-char (point-min))
        (if (re-search-forward "^#\\+TITLE: " nil t)
            (progn
              (end-of-line)
              (insert (format "\n#+%s: %s" key value)))
          (insert (format "#+%s: %s\n" key value)))))))

(defun gh/org-docs--get-doc-id ()
  "Get GDOC_ID from file properties."
  (gh/org-docs--debug "Getting GDOC_ID from buffer")
  (let ((id (gh/org-docs--get-file-property "GDOC_ID")))
    (when id
      (gh/org-docs--debug "Found GDOC_ID: %s" id))
    id))

(defun gh/org-docs--set-doc-id (doc-id)
  "Set GDOC_ID in file properties."
  (gh/org-docs--debug "Setting GDOC_ID to: %s" doc-id)
  (gh/org-docs--set-file-property "GDOC_ID" doc-id))

(defun gh/org-docs--get-doc-url ()
  "Get GDOC_URL from file properties."
  (gh/org-docs--get-file-property "GDOC_URL"))

(defun gh/org-docs--set-doc-url (url)
  "Set GDOC_URL in file properties."
  (gh/org-docs--debug "Setting GDOC_URL to: %s" url)
  (gh/org-docs--set-file-property "GDOC_URL" url))

(defun gh/org-docs--get-last-push ()
  "Get last push timestamp."
  (gh/org-docs--get-file-property "GDOC_LAST_PUSH"))

(defun gh/org-docs--set-last-push (timestamp)
  "Set last push timestamp."
  (gh/org-docs--set-file-property "GDOC_LAST_PUSH" timestamp))

(defun gh/org-docs--get-last-pull ()
  "Get last pull timestamp."
  (gh/org-docs--get-file-property "GDOC_LAST_PULL"))

(defun gh/org-docs--set-last-pull (timestamp)
  "Set last pull timestamp."
  (gh/org-docs--set-file-property "GDOC_LAST_PULL" timestamp))

(defun gh/org-docs--get-title ()
  "Get document title from #+TITLE or buffer name."
  (or (gh/org-docs--get-file-property "TITLE")
      (when (buffer-file-name)
        (file-name-base (buffer-file-name)))
      "Untitled"))

;; ============================================================================
;; CUSTOM_ID generation
;; ============================================================================

(defun gh/org-docs--generate-slug (text)
  "Generate URL-safe slug from TEXT."
  (let* ((lower (downcase text))
         (no-special (replace-regexp-in-string "[^a-z0-9 -]" "" lower))
         (dashed (replace-regexp-in-string " +" "-" no-special))
         (trimmed (replace-regexp-in-string "^-+\\|-+$" "" dashed)))
    (if (string-empty-p trimmed)
        "section"
      (substring trimmed 0 (min 50 (length trimmed))))))

(defun gh/org-docs--ensure-custom-id (headline)
  "Ensure HEADLINE has a CUSTOM_ID property.
If missing, generate one and add it. Returns the CUSTOM_ID."
  (let ((existing (org-element-property :CUSTOM_ID headline)))
    (if existing
        existing
      (let* ((title (org-element-property :raw-value headline))
             (slug (gh/org-docs--generate-slug title))
             (new-id (format "sec-%s" slug)))
        (save-excursion
          (goto-char (org-element-property :begin headline))
          (org-set-property "CUSTOM_ID" new-id))
        new-id))))

(defun gh/org-docs--make-element-id (parent-id type counter)
  "Create hierarchical element ID.
PARENT-ID is the parent section's CUSTOM_ID.
TYPE is the element type (para, list, table, image).
COUNTER is the occurrence number."
  (format "%s/%s-%d" parent-id type counter))

;; ============================================================================
;; Org element to S-expression conversion
;; ============================================================================

(defvar gh/org-docs--comment-marker-re
  "\\[\\[id:comment-[^]]+\\]\\[.\\]\\]"
  "Regexp matching comment markers inserted by gh-org-docs.")

(defun gh/org-docs--strip-comment-markers (text)
  "Remove comment markers from TEXT."
  (replace-regexp-in-string gh/org-docs--comment-marker-re "" text))

(defun gh/org-docs--extract-plain-text (element)
  "Extract plain text from ELEMENT, handling nested objects.
Uses iterative approach to avoid deep recursion."
  (let ((result ""))
    (gh/org-docs--extract-text-iter element
                                    (lambda (s) (setq result (concat result s))))
    ;; Strip text properties and comment markers
    (substring-no-properties
     (gh/org-docs--strip-comment-markers (string-trim result)))))

(defun gh/org-docs--extract-text-iter (element callback)
  "Extract text from ELEMENT, calling CALLBACK for each text fragment.
Iterates without nesting org-element-map calls.
Respects :post-blank property to preserve spacing after formatted text."
  (let ((contents (org-element-contents element)))
    (dolist (child contents)
      (cond
       ;; Plain text string
       ((stringp child)
        (funcall callback child))
       ;; Formatting elements - descend into contents, then add post-blank spaces
       ((memq (org-element-type child)
              '(bold italic underline strike-through))
        (gh/org-docs--extract-text-iter child callback)
        ;; Add trailing spaces based on :post-blank property
        (let ((post-blank (org-element-property :post-blank child)))
          (when (and post-blank (> post-blank 0))
            (funcall callback (make-string post-blank ?\s)))))
       ;; Code/verbatim elements store text in :value property, not contents
       ((memq (org-element-type child) '(code verbatim))
        (let ((value (org-element-property :value child)))
          (when value
            (funcall callback value)))
        (let ((post-blank (org-element-property :post-blank child)))
          (when (and post-blank (> post-blank 0))
            (funcall callback (make-string post-blank ?\s)))))
       ;; Links - use description or raw-link, then add post-blank
       ((eq (org-element-type child) 'link)
        (let ((desc (org-element-contents child)))
          (if desc
              (gh/org-docs--extract-text-iter child callback)
            (funcall callback (org-element-property :raw-link child))))
        (let ((post-blank (org-element-property :post-blank child)))
          (when (and post-blank (> post-blank 0))
            (funcall callback (make-string post-blank ?\s)))))
       ;; Nested elements (paragraphs, items, etc.) - descend
       ((and (listp child) (org-element-type child))
        (gh/org-docs--extract-text-iter child callback))))))

(defun gh/org-docs--extract-formatting (element base-text)
  "Extract formatting ranges from ELEMENT.
BASE-TEXT is the plain text for calculating offsets."
  (let ((ranges nil)
        (pos 0))
    (org-element-map element '(bold italic underline strike-through code link)
      (lambda (obj)
        (let* ((type (org-element-type obj))
               (content (gh/org-docs--extract-plain-text obj))
               (start (string-match (regexp-quote content) base-text pos)))
          (when start
            (let ((end (+ start (length content))))
              (setq pos end)
              (push `(,(pcase type
                         ('bold 'bold)
                         ('italic 'italic)
                         ('underline 'underline)
                         ('strike-through 'strikethrough)
                         ('code 'code)
                         ('link 'link))
                      :start ,start
                      :end ,end
                      ,@(when (eq type 'link)
                          (list :url (org-element-property :raw-link obj))))
                    ranges)))))
      nil nil t)
    (nreverse ranges)))

(defun gh/org-docs--paragraph-to-sexp (paragraph parent-id counter)
  "Convert PARAGRAPH element to S-expression.
PARENT-ID is the parent section ID.
COUNTER is the paragraph occurrence number."
  (let* ((text (gh/org-docs--extract-plain-text paragraph))
         (elem-id (gh/org-docs--make-element-id parent-id "para" counter))
         (formatting (gh/org-docs--extract-formatting paragraph text)))
    (list 'paragraph
          :text text
          :custom-id elem-id
          :formatting (or formatting 'nil))))

(defun gh/org-docs--list-to-sexp (plain-list parent-id counter)
  "Convert PLAIN-LIST element to S-expression.
PARENT-ID is the parent section ID.
COUNTER is the list occurrence number."
  (let* ((list-type (if (eq (org-element-property :type plain-list) 'ordered)
                        "ordered"
                      "unordered"))
         (elem-id (gh/org-docs--make-element-id parent-id "list" counter))
         (items nil))
    (org-element-map plain-list 'item
      (lambda (item)
        (let ((text (gh/org-docs--extract-plain-text item)))
          (push text items)))
      nil nil 'first-match)
    (list 'list
          :type list-type
          :custom-id elem-id
          :items (nreverse items))))

(defun gh/org-docs--table-to-sexp (table parent-id counter)
  "Convert TABLE element to S-expression.
PARENT-ID is the parent section ID.
COUNTER is the table occurrence number."
  (let* ((elem-id (gh/org-docs--make-element-id parent-id "table" counter))
         (rows nil))
    (org-element-map table 'table-row
      (lambda (row)
        (when (eq (org-element-property :type row) 'standard)
          (let ((cells nil))
            (org-element-map row 'table-cell
              (lambda (cell)
                (push (gh/org-docs--extract-plain-text cell) cells))
              nil nil t)
            (push (nreverse cells) rows))))
      nil nil t)
    (list 'table
          :custom-id elem-id
          :rows (nreverse rows))))

(defun gh/org-docs--heading-to-sexp (headline)
  "Convert HEADLINE element to S-expression."
  (let* ((level (org-element-property :level headline))
         (title (org-element-property :raw-value headline))
         (custom-id (gh/org-docs--ensure-custom-id headline)))
    (list 'heading
          :level level
          :text (gh/org-docs--strip-comment-markers title)
          :custom-id custom-id)))

(defun gh/org-docs--paragraph-image-link (paragraph)
  "If PARAGRAPH contains only an image file link, return the path. Otherwise nil."
  (let ((image-links nil)
        (has-other-content nil))
    ;; Find image file links
    (org-element-map paragraph 'link
      (lambda (link)
        (let ((type (org-element-property :type link))
              (path (org-element-property :path link)))
          (when (and (member type '("file" "attachment"))
                     path
                     (string-match-p "\\.\\(png\\|jpg\\|jpeg\\|gif\\|svg\\|webp\\)$" path))
            (push path image-links))))
      nil nil t)
    ;; Check for non-link, non-whitespace content
    (let ((contents (org-element-contents paragraph)))
      (dolist (child contents)
        (cond
         ;; Plain text that isn't just whitespace
         ((stringp child)
          (when (string-match-p "[^[:space:]]" (substring-no-properties child))
            (setq has-other-content t)))
         ;; Non-link elements count as other content
         ((and (listp child)
               (not (eq (org-element-type child) 'link)))
          (setq has-other-content t)))))
    ;; Return the image path if it's the only meaningful content
    (when (and image-links (= (length image-links) 1) (not has-other-content))
      (car image-links))))

(defun gh/org-docs--image-to-sexp (image-path parent-id counter)
  "Convert IMAGE-PATH to S-expression.
PARENT-ID is the parent section ID.
COUNTER is the image occurrence number."
  (let* ((elem-id (gh/org-docs--make-element-id parent-id "image" counter))
         ;; Resolve path relative to current buffer's directory
         (abs-path (if (file-name-absolute-p image-path)
                       image-path
                     (expand-file-name image-path
                                       (file-name-directory (or buffer-file-name default-directory))))))
    (list 'image
          :custom-id elem-id
          :path abs-path
          :alt-text (file-name-base image-path))))

(defun gh/org-docs--src-block-generates-image-p (src-block)
  "Return non-nil if SRC-BLOCK generates an image file.
Checks for :file parameter in header args."
  (let ((params (org-element-property :parameters src-block)))
    (and params (string-match-p ":file" params))))

(defun gh/org-docs--src-block-to-sexp (src-block parent-id counter)
  "Convert SRC-BLOCK element to S-expression (as monospace paragraph).
PARENT-ID is the parent section ID.
COUNTER is the code block occurrence number.
Returns nil for image-generating blocks (their output is captured separately)."
  ;; Skip blocks that generate images - the image will be captured from #+RESULTS:
  (unless (gh/org-docs--src-block-generates-image-p src-block)
    (let* ((elem-id (gh/org-docs--make-element-id parent-id "code" counter))
           (lang (org-element-property :language src-block))
           (value (org-element-property :value src-block))
           ;; Clean up the value (remove trailing newline if present)
           (text (string-trim-right value)))
      ;; Format the entire block as code (monospace)
      (list 'paragraph
            :text text
            :custom-id elem-id
            :formatting (list (list 'code :start 0 :end (length text)))))))

(defun gh/org-docs--buffer-to-content ()
  "Convert current buffer content to list of content S-expressions."
  (gh/org-docs--debug "Converting buffer to content S-expressions")
  (let ((content nil)
        (current-section-id "doc")
        (counters (make-hash-table :test 'equal)))
    (org-element-map (org-element-parse-buffer) '(headline paragraph plain-list table src-block)
      (lambda (element)
        (let ((type (org-element-type element)))
          ;; Skip elements inside GDOC_METADATA section
          (unless (gh/org-docs--in-metadata-section-p element)
            (pcase type
              ('headline
               (let* ((sexp (gh/org-docs--heading-to-sexp element))
                      (custom-id (plist-get (cdr sexp) :custom-id)))
                 (setq current-section-id custom-id)
                 ;; Reset counters for new section
                 (clrhash counters)
                 (push sexp content)))
              ('paragraph
               ;; Skip paragraphs inside list items (they're handled by list processing)
               (unless (gh/org-docs--inside-list-p element)
                 ;; Check if this paragraph is just an image link
                 (let ((image-path (gh/org-docs--paragraph-image-link element)))
                   (if image-path
                       ;; It's an image
                       (let* ((key (format "%s-image" current-section-id))
                              (count (gethash key counters 0)))
                         (puthash key (1+ count) counters)
                         (push (gh/org-docs--image-to-sexp image-path current-section-id count)
                               content))
                     ;; Regular paragraph
                     (let* ((key (format "%s-para" current-section-id))
                            (count (gethash key counters 0)))
                       (puthash key (1+ count) counters)
                       (push (gh/org-docs--paragraph-to-sexp element current-section-id count)
                             content))))))
              ('plain-list
               (let* ((key (format "%s-list" current-section-id))
                      (count (gethash key counters 0)))
                 (puthash key (1+ count) counters)
                 (push (gh/org-docs--list-to-sexp element current-section-id count)
                       content)))
              ('table
               (let* ((key (format "%s-table" current-section-id))
                      (count (gethash key counters 0)))
                 (puthash key (1+ count) counters)
                 (push (gh/org-docs--table-to-sexp element current-section-id count)
                       content)))
              ('src-block
               ;; Only include non-image-generating source blocks
               (let ((sexp (gh/org-docs--src-block-to-sexp element current-section-id
                            (gethash (format "%s-code" current-section-id) counters 0))))
                 (when sexp
                   (let* ((key (format "%s-code" current-section-id))
                          (count (gethash key counters 0)))
                     (puthash key (1+ count) counters)
                     (push sexp content)))))))))
      nil nil 'first-match)
    (nreverse content)))

(defun gh/org-docs--excluded-heading-p (heading-text)
  "Return non-nil if HEADING-TEXT should be excluded from push."
  (and heading-text
       (or (string-match-p "GDOC_METADATA" heading-text)
           (string-match-p "^=+$" heading-text)  ; Separator lines
           (string-match-p "Local Variables" heading-text))))

(defun gh/org-docs--in-metadata-section-p (element)
  "Return non-nil if ELEMENT should be excluded from push.
Excludes GDOC_METADATA section, separator headings, Local Variables, and
any section with :noexport: tag."
  (or
   ;; For headlines, check if it's an excluded heading or has :noexport:
   (when (eq (org-element-type element) 'headline)
     (or (member "noexport" (org-element-property :tags element))
         (gh/org-docs--excluded-heading-p
          (org-element-property :raw-value element))))
   ;; For all elements, check if inside an excluded section
   (let ((begin (org-element-property :begin element)))
     (save-excursion
       (goto-char begin)
       (let ((found-excluded nil))
         (while (and (not found-excluded)
                     (org-up-heading-safe))
           (when (or (member "noexport" (org-get-tags))
                     (gh/org-docs--excluded-heading-p (org-get-heading t t t t)))
             (setq found-excluded t)))
         found-excluded)))))

(defun gh/org-docs--inside-list-p (element)
  "Return non-nil if ELEMENT is inside a list item."
  (let ((parent (org-element-property :parent element)))
    (while (and parent (not (memq (org-element-type parent) '(item plain-list))))
      (setq parent (org-element-property :parent parent)))
    (and parent (memq (org-element-type parent) '(item plain-list)))))

;; ============================================================================
;; API communication
;; ============================================================================

(defun gh/org-docs--call-api (operation data)
  "Call the Go API tool with OPERATION and DATA.
Returns the parsed response."
  (gh/org-docs--debug "Calling API: %s" operation)
  (let* ((request (list 'operation operation :data data))
         (request-str (gh/org-docs--sexp-encode request))
         (cmd (if gh/org-docs-debug-mode
                  (format "%s --debug" gh/org-docs-api-command)
                gh/org-docs-api-command)))
    (gh/org-docs--debug "Request:\n%s" request-str)
    (with-temp-buffer
      (let ((exit-code (call-process-region
                        request-str nil
                        shell-file-name nil t nil
                        shell-command-switch
                        cmd)))
        (gh/org-docs--debug "Exit code: %d" exit-code)
        (gh/org-docs--debug "Response:\n%s" (buffer-string))
        (if (= exit-code 0)
            (let ((response (gh/org-docs--sexp-decode (buffer-string))))
              (gh/org-docs--parse-api-response response))
          (error "API call failed with exit code %d: %s"
                 exit-code (buffer-string)))))))

(defun gh/org-docs--parse-api-response (response)
  "Parse API RESPONSE and handle errors."
  (unless (and (listp response) (>= (length response) 2))
    (error "Invalid API response format"))
  (let ((result-type (cadr response)))
    (if (eq result-type 'error)
        (let ((code (plist-get (cddr response) :code))
              (message (plist-get (cddr response) :message))
              (details (plist-get (cddr response) :details)))
          (error "API error [%s]: %s%s"
                 code message
                 (if details (format "\nDetails: %s" details) "")))
      ;; Success - return the plist
      (cddr response))))

;; ============================================================================
;; Position map management
;; ============================================================================

(defun gh/org-docs--get-position-map ()
  "Get position map from GDOC_METADATA section."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^\\*\\* Position Map" nil t)
        (progn
          (forward-line 1)
          (when (re-search-forward "#\\+begin_src emacs-lisp" nil t)
            (forward-line 1)
            (let ((start (point)))
              (when (re-search-forward "#\\+end_src" nil t)
                (forward-line -1)
                (let ((sexp-str (buffer-substring-no-properties start (line-end-position))))
                  (condition-case nil
                      (eval (read sexp-str))
                    (error nil)))))))
      nil)))

(defun gh/org-docs--save-position-map (pos-map)
  "Save POS-MAP to GDOC_METADATA section."
  (gh/org-docs--ensure-metadata-section)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^\\*\\* Position Map" nil t)
      (forward-line 1)
      (when (re-search-forward "#\\+begin_src emacs-lisp" nil t)
        (forward-line 1)
        (let ((start (point)))
          (when (re-search-forward "#\\+end_src" nil t)
            (forward-line -1)
            (delete-region start (line-end-position))
            (goto-char start)
            (insert (format "'%S" pos-map))))))))

;; ============================================================================
;; Comment management
;; ============================================================================

(defun gh/org-docs--get-known-comment-ids ()
  "Get list of comment IDs already known from GDOC_METADATA."
  (let ((ids nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^:COMMENT_ID: \\(.+\\)$" nil t)
        (push (match-string 1) ids)))
    ids))

(defun gh/org-docs--get-resolved-comment-ids ()
  "Get list of comment IDs marked as DONE."
  (let ((ids nil))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^\\*\\* Active Comments" nil t)
        (let ((end (save-excursion
                     (or (re-search-forward "^\\*\\* " nil t) (point-max)))))
          (while (re-search-forward "^\\*\\*\\* DONE " end t)
            (save-excursion
              (when (re-search-forward "^:COMMENT_ID: \\(.+\\)$"
                                       (save-excursion (outline-next-heading) (point))
                                       t)
                (push (match-string 1) ids)))))))
    ids))

(defun gh/org-docs--get-collaborator-cache ()
  "Get collaborator cache from GDOC_METADATA section."
  (let ((cache nil))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^\\*\\* Collaborator Cache" nil t)
        (forward-line 2)  ; Skip header row
        (while (looking-at "^| \\([^|]+\\) +| \\([^|]+\\) +|")
          (let ((email (string-trim (match-string 1)))
                (name (string-trim (match-string 2))))
            (unless (or (string= email "Email") (string= email "---"))
              (push (cons email name) cache)))
          (forward-line 1))))
    cache))

(defun gh/org-docs--update-collaborator-cache (new-collabs)
  "Add NEW-COLLABS to the collaborator cache table."
  (when new-collabs
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^\\*\\* Collaborator Cache" nil t)
        (re-search-forward "^|---" nil t)
        (end-of-line)
        (dolist (collab new-collabs)
          (insert (format "\n| %s | %s |" (car collab) (cdr collab))))))))

;; ============================================================================
;; GDOC_METADATA section management
;; ============================================================================

(defun gh/org-docs--ensure-metadata-section ()
  "Ensure GDOC_METADATA section exists with proper warnings."
  (save-excursion
    (goto-char (point-min))
    (unless (re-search-forward "^\\* GDOC_METADATA" nil t)
      (goto-char (point-max))
      (insert "\n")
      (insert "* ======================================================================= :noexport:\n")
      (insert "# WARNING: This section is managed by gh-org-docs. Do not edit manually!\n")
      (insert "# Any manual changes will be overwritten on the next pull/push operation.\n")
      (insert "#\n")
      (insert "# To resolve comments: Change TODO -> DONE, then run C-c g s\n")
      (insert "# To remove resolved comments: Run C-c g c\n")
      (insert "\n")
      (insert "* GDOC_METADATA :noexport:\n")
      (insert "\n")
      (insert "** Active Comments\n")
      (insert "\n")
      (insert "** Position Map\n")
      (insert "#+begin_src emacs-lisp :exports none :results silent\n")
      (insert "'()\n")
      (insert "#+end_src\n")
      (insert "\n")
      (insert "** Collaborator Cache\n")
      (insert "| Email | Display Name |\n")
      (insert "|-------+--------------|\n"))))

(defun gh/org-docs--insert-comment-marker (comment)
  "Insert comment marker for COMMENT at appropriate location."
  (let* ((anchor (plist-get comment :anchor))
         (custom-id (plist-get anchor :custom-id))
         (comment-id (plist-get comment :id)))
    (when custom-id
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward
               (format ":CUSTOM_ID: %s" (regexp-quote custom-id))
               nil t)
          (org-back-to-heading)
          (org-end-of-subtree)
          ;; Insert marker at end of section
          (skip-chars-backward " \t\n")
          (insert (format "[[id:comment-%s][*]]" comment-id)))))))

(defun gh/org-docs--create-comment-todo (comment)
  "Create TODO entry for COMMENT in Active Comments section."
  (let ((comment-id (plist-get comment :id))
        (author-name (plist-get comment :author-name))
        (author-email (plist-get comment :author-email))
        (text (plist-get comment :text))
        (created (plist-get comment :created))
        (anchor (plist-get comment :anchor)))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^\\*\\* Active Comments" nil t)
        (org-end-of-subtree)
        (insert "\n\n")
        (insert (format "*** TODO %s - %s\n"
                        (truncate-string-to-width text 50 nil nil "...")
                        author-name))
        (insert ":PROPERTIES:\n")
        (insert (format ":COMMENT_ID: %s\n" comment-id))
        (insert (format ":COMMENT_AUTHOR: %s\n" author-name))
        (insert (format ":COMMENT_EMAIL: %s\n" author-email))
        (insert (format ":COMMENT_DATE: %s\n" created))
        (insert (format ":COMMENT_SECTION: %s\n" (or (plist-get anchor :custom-id) "")))
        (insert ":END:\n")
        (insert "\n")
        (insert "#+begin_quote\n")
        (insert text)
        (insert "\n#+end_quote\n")
        (insert "\n")
        (insert (format "[[id:comment-%s][Jump to comment location]]\n" comment-id))))))

;; ============================================================================
;; Pre-push processing
;; ============================================================================

(defun gh/org-docs--execute-image-blocks ()
  "Execute source blocks that generate images (mermaid, ditaa, plantuml, etc.)."
  (gh/org-docs--debug "Executing image-generating source blocks")
  (let ((executed 0))
    (org-element-map (org-element-parse-buffer) 'src-block
      (lambda (block)
        (let ((lang (org-element-property :language block)))
          (when (member lang '("mermaid" "ditaa" "plantuml" "dot" "graphviz"))
            (save-excursion
              (goto-char (org-element-property :begin block))
              (condition-case err
                  (progn
                    (org-babel-execute-src-block)
                    (setq executed (1+ executed)))
                (error
                 (gh/org-docs--debug "Failed to execute %s block: %s" lang err)))))))
      nil nil t)
    (when (> executed 0)
      (gh/org-docs--debug "Executed %d image-generating block(s)" executed)
      (message "Generated %d image(s) from source blocks" executed))))

(defun gh/org-docs--check-missing-images (content)
  "Check CONTENT for image items with missing files. Return list of missing paths."
  (let ((missing nil))
    (dolist (item content)
      (when (eq (car item) 'image)
        (let ((path (plist-get (cdr item) :path)))
          (unless (file-exists-p path)
            (push path missing)))))
    (nreverse missing)))

;; ============================================================================
;; Main commands
;; ============================================================================

(defun gh/org-docs-push ()
  "Push current buffer to Google Docs."
  (interactive)
  (unless gh/org-docs-mode
    (user-error "Enable gh/org-docs-mode first"))
  (unless (derived-mode-p 'org-mode)
    (user-error "This command only works in org-mode buffers"))

  ;; Execute image-generating blocks first
  (gh/org-docs--execute-image-blocks)
  (save-buffer)

  ;; Convert buffer to content
  (let* ((content (gh/org-docs--buffer-to-content))
         (missing-images (gh/org-docs--check-missing-images content))
         (doc-id (gh/org-docs--get-doc-id))
         (title (gh/org-docs--get-title))
         (comments-to-resolve (gh/org-docs--get-resolved-comment-ids)))

    ;; Warn about missing images
    (when missing-images
      (unless (y-or-n-p (format "%d image(s) not found: %s. Continue anyway? "
                                (length missing-images)
                                (mapconcat #'file-name-nondirectory missing-images ", ")))
        (user-error "Push cancelled - generate missing images first"))
      ;; Filter out missing images from content
      (setq content (cl-remove-if
                     (lambda (item)
                       (and (eq (car item) 'image)
                            (member (plist-get (cdr item) :path) missing-images)))
                     content)))

    (let ((data (list :document-id (or doc-id 'nil)
                       :title title
                       :content content
                       :comments-to-resolve (or comments-to-resolve 'nil))))

      (message "Pushing to Google Docs...")
      (let ((result (gh/org-docs--call-api 'push data)))
        ;; Update metadata
        (gh/org-docs--set-doc-id (plist-get result :document-id))
        (gh/org-docs--set-doc-url (plist-get result :document-url))
        (gh/org-docs--save-position-map (plist-get result :position-map))
        (gh/org-docs--set-last-push (format-time-string "[%Y-%m-%d %a %H:%M]"))

        (save-buffer)
        (message "Pushed to Google Docs: %s" (plist-get result :document-url))))))

(defun gh/org-docs-pull ()
  "Pull comments from Google Docs."
  (interactive)
  (unless gh/org-docs-mode
    (user-error "Enable gh/org-docs-mode first"))
  (unless (derived-mode-p 'org-mode)
    (user-error "This command only works in org-mode buffers"))

  (let ((doc-id (gh/org-docs--get-doc-id)))
    (unless doc-id
      (user-error "No Google Doc linked. Push first to create one"))

    (let* ((pos-map (gh/org-docs--get-position-map))
           (known-ids (gh/org-docs--get-known-comment-ids))
           (collab-cache (gh/org-docs--get-collaborator-cache))
           (data (list :document-id doc-id
                       :position-map (or pos-map 'nil)
                       :known-comment-ids (or known-ids 'nil)
                       :collaborator-cache (or collab-cache 'nil))))

      (message "Pulling comments from Google Docs...")
      (let ((result (gh/org-docs--call-api 'pull data)))
        (let ((comments (plist-get result :comments))
              (new-collabs (plist-get result :new-collaborators)))

          ;; Ensure metadata section exists
          (gh/org-docs--ensure-metadata-section)

          ;; Process each new comment
          (dolist (comment comments)
            (gh/org-docs--insert-comment-marker comment)
            (gh/org-docs--create-comment-todo comment))

          ;; Update collaborator cache
          (gh/org-docs--update-collaborator-cache new-collabs)

          ;; Update timestamp
          (gh/org-docs--set-last-pull (format-time-string "[%Y-%m-%d %a %H:%M]"))

          (save-buffer)
          (message "Pulled %d new comment(s) from Google Docs" (length comments)))))))

(defun gh/org-docs-dwim ()
  "Do What I Mean for Google Docs sync."
  (interactive)
  (unless gh/org-docs-mode
    (user-error "Enable gh/org-docs-mode first"))

  (cond
   ;; No doc linked - create one
   ((not (gh/org-docs--get-doc-id))
    (when (y-or-n-p "No Google Doc linked. Create new document? ")
      (gh/org-docs-push)))

   ;; Has resolved comments - push to sync
   ((gh/org-docs--get-resolved-comment-ids)
    (message "You have resolved comments. Pushing to sync...")
    (gh/org-docs-push))

   ;; Buffer modified - offer to push
   ((buffer-modified-p)
    (when (y-or-n-p "Buffer modified. Push to Google Docs? ")
      (gh/org-docs-push)))

   ;; Otherwise pull
   (t
    (message "Pulling comments from Google Docs...")
    (gh/org-docs-pull))))

(defun gh/org-docs-clean ()
  "Archive resolved comments and remove their markers."
  (interactive)
  (unless gh/org-docs-mode
    (user-error "Enable gh/org-docs-mode first"))

  (let ((count 0))
    (save-excursion
      ;; Remove DONE comment entries
      (goto-char (point-min))
      (when (re-search-forward "^\\*\\* Active Comments" nil t)
        (let ((section-end (save-excursion
                             (or (re-search-forward "^\\*\\* " nil t)
                                 (point-max)))))
          (while (re-search-forward "^\\*\\*\\* DONE " section-end t)
            (org-back-to-heading)
            (let ((elem (org-element-at-point)))
              (delete-region (org-element-property :begin elem)
                             (org-element-property :end elem))
              (setq count (1+ count)))))))

    ;; Remove orphaned comment markers
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\[\\[id:comment-[^]]+\\]\\[.\\]\\]" nil t)
        (let* ((marker (match-string 0))
               (comment-id (when (string-match "comment-\\([^]]+\\)" marker)
                             (match-string 1 marker))))
          ;; Check if this comment still exists in Active Comments
          (unless (save-excursion
                    (goto-char (point-min))
                    (re-search-forward
                     (format "^:COMMENT_ID: %s$" (regexp-quote comment-id))
                     nil t))
            (replace-match "")))))

    (save-buffer)
    (message "Cleaned up %d resolved comment(s)" count)))

(defun gh/org-docs-open-in-browser ()
  "Open the linked Google Doc in a web browser."
  (interactive)
  (let ((url (gh/org-docs--get-doc-url))
        (doc-id (gh/org-docs--get-doc-id)))
    (cond
     (url (browse-url url))
     (doc-id (browse-url (format "https://docs.google.com/document/d/%s/edit" doc-id)))
     (t (user-error "No Google Doc linked to this file")))))

;; ============================================================================
;; Auto-enable hook
;; ============================================================================

(defun gh/org-docs--maybe-enable ()
  "Maybe enable `gh/org-docs-mode' based on `gh/org-docs-auto-enable' setting."
  (when (and (derived-mode-p 'org-mode)
             (buffer-file-name))
    (pcase gh/org-docs-auto-enable
      ('always (gh/org-docs-mode 1))
      ('linked (when (gh/org-docs--get-doc-id)
                 (gh/org-docs-mode 1))))))

;;;###autoload
(defun gh/org-docs-setup-auto-enable ()
  "Add `gh/org-docs-mode' auto-enable to `org-mode-hook'.
Call this in your init file to enable automatic activation."
  (add-hook 'org-mode-hook #'gh/org-docs--maybe-enable))

;; Auto-setup when package is loaded (can be disabled by setting
;; gh/org-docs-auto-enable to nil before loading)
(with-eval-after-load 'org
  (add-hook 'org-mode-hook #'gh/org-docs--maybe-enable))

(provide 'gh-org-docs)
;;; gh-org-docs.el ends here
