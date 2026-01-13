;;; gh-org-docs.el --- Sync org-mode documents with Google Docs -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: James Felix Black
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (org "9.7"))
;; Keywords: org-mode, google-docs, collaboration

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

;; ---- Commands (stubs; implemented in later tasks) ----

(defun gh/org-docs-dwim ()
  "Do What I Mean for Google Docs sync."
  (interactive)
  (user-error "gh/org-docs-dwim not implemented yet"))

(defun gh/org-docs-push ()
  "Push current buffer to Google Docs."
  (interactive)
  (user-error "gh/org-docs-push not implemented yet"))

(defun gh/org-docs-pull ()
  "Pull comments from Google Docs."
  (interactive)
  (user-error "gh/org-docs-pull not implemented yet"))

(defun gh/org-docs-clean ()
  "Clean up resolved comments."
  (interactive)
  (user-error "gh/org-docs-clean not implemented yet"))

(defun gh/org-docs-open-in-browser ()
  "Open the associated Google Doc in a browser."
  (interactive)
  (user-error "gh/org-docs-open-in-browser not implemented yet"))

;; ---- File metadata helpers ----

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

(provide 'gh-org-docs)
;;; gh-org-docs.el ends here

