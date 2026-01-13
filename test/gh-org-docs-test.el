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

(ert-deftest gh-org-docs-sexp-encode-basic ()
  "Test basic S-expression encoding."
  ;; Nil
  (should (string= (gh/org-docs--sexp-encode nil) "nil"))
  ;; T
  (should (string= (gh/org-docs--sexp-encode t) "t"))
  ;; Numbers
  (should (string= (gh/org-docs--sexp-encode 42) "42"))
  (should (string= (gh/org-docs--sexp-encode -17) "-17"))
  ;; Symbols
  (should (string= (gh/org-docs--sexp-encode 'foo) "foo"))
  (should (string= (gh/org-docs--sexp-encode :keyword) ":keyword"))
  ;; Strings
  (should (string= (gh/org-docs--sexp-encode "hello") "\"hello\""))
  (should (string= (gh/org-docs--sexp-encode "with \"quotes\"") "\"with \\\"quotes\\\"\"")))

(ert-deftest gh-org-docs-sexp-encode-lists ()
  "Test S-expression encoding of lists."
  ;; Empty list
  (should (string= (gh/org-docs--sexp-encode '()) "nil"))
  ;; Simple list
  (should (string= (gh/org-docs--sexp-encode '(a b c)) "(a b c)"))
  ;; Mixed list
  (should (string= (gh/org-docs--sexp-encode '(foo 42 "bar"))
                   "(foo 42 \"bar\")"))
  ;; Nested list
  (should (string= (gh/org-docs--sexp-encode '(a (b c) d))
                   "(a (b c) d)")))

(ert-deftest gh-org-docs-sexp-encode-plist ()
  "Test S-expression encoding of property lists."
  (should (string= (gh/org-docs--sexp-encode '(:key "value"))
                   "(:key \"value\")"))
  (should (string= (gh/org-docs--sexp-encode '(:a 1 :b 2))
                   "(:a 1 :b 2)")))

(ert-deftest gh-org-docs-sexp-decode-basic ()
  "Test basic S-expression decoding."
  (should (eq (gh/org-docs--sexp-decode "nil") nil))
  (should (eq (gh/org-docs--sexp-decode "t") t))
  (should (= (gh/org-docs--sexp-decode "42") 42))
  (should (eq (gh/org-docs--sexp-decode "foo") 'foo))
  (should (string= (gh/org-docs--sexp-decode "\"hello\"") "hello")))

(ert-deftest gh-org-docs-sexp-decode-lists ()
  "Test S-expression decoding of lists."
  (should (equal (gh/org-docs--sexp-decode "(a b c)") '(a b c)))
  (should (equal (gh/org-docs--sexp-decode "(:key \"value\")") '(:key "value"))))

(ert-deftest gh-org-docs-generate-slug ()
  "Test slug generation from text."
  (should (string= (gh/org-docs--generate-slug "Hello World") "hello-world"))
  (should (string= (gh/org-docs--generate-slug "Introduction") "introduction"))
  (should (string= (gh/org-docs--generate-slug "Section 1.2: Setup") "section-12-setup"))
  (should (string= (gh/org-docs--generate-slug "   Spaces   ") "spaces"))
  (should (string= (gh/org-docs--generate-slug "") "section")))

(ert-deftest gh-org-docs-strip-comment-markers ()
  "Test removal of comment markers from text."
  (should (string= (gh/org-docs--strip-comment-markers "plain text") "plain text"))
  (should (string= (gh/org-docs--strip-comment-markers "text[[id:comment-abc][*]]more")
                   "textmore"))
  (should (string= (gh/org-docs--strip-comment-markers "[[id:comment-123][x]]start")
                   "start")))

(ert-deftest gh-org-docs-ensure-metadata-section ()
  "Test metadata section creation."
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: Test\n\n* Content\nSome text\n")
    (gh/org-docs--ensure-metadata-section)
    (goto-char (point-min))
    (should (re-search-forward "^\\* GDOC_METADATA" nil t))
    (should (re-search-forward "^\\*\\* Active Comments" nil t))
    (should (re-search-forward "^\\*\\* Position Map" nil t))
    (should (re-search-forward "^\\*\\* Collaborator Cache" nil t))))

(ert-deftest gh-org-docs-get-title ()
  "Test title extraction."
  (with-temp-buffer
    (insert "#+TITLE: My Document\n\n* Heading\n")
    (should (string= (gh/org-docs--get-title) "My Document")))
  (with-temp-buffer
    ;; No title - should fall back
    (insert "* Heading\n")
    (should (stringp (gh/org-docs--get-title)))))

(provide 'gh-org-docs-test)
;;; gh-org-docs-test.el ends here
