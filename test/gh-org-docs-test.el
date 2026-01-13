;;; gh-org-docs-test.el --- Tests for gh-org-docs -*- lexical-binding: t; -*-

(require 'ert)

(load-file
 (expand-file-name "../gh-org-docs.el" (file-name-directory load-file-name)))

(ert-deftest gh-org-docs-file-metadata-get-set ()
  (with-temp-buffer
    (insert "#+TITLE: Test Doc\n\n* Heading\nBody\n")

    (gh/org-docs--set-doc-id "doc-123")
    (should (string= (gh/org-docs--get-doc-id) "doc-123"))

    (gh/org-docs--set-last-push "[2026-01-13 Mon 14:30]")
    (should (string= (gh/org-docs--get-last-push) "[2026-01-13 Mon 14:30]"))

    (gh/org-docs--set-last-pull "[2026-01-13 Mon 16:45]")
    (should (string= (gh/org-docs--get-last-pull) "[2026-01-13 Mon 16:45]"))

    ;; Updating existing value should overwrite the existing line.
    (gh/org-docs--set-doc-id "doc-456")
    (should (string= (gh/org-docs--get-doc-id) "doc-456"))

    (goto-char (point-min))
    (should (re-search-forward "^#\\+GDOC_ID: doc-456$" nil t))))

(provide 'gh-org-docs-test)
;;; gh-org-docs-test.el ends here

