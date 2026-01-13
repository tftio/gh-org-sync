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

;; ============================================================================
;; Content extraction tests
;; ============================================================================

(ert-deftest gh-org-docs-extract-plain-text ()
  "Test plain text extraction from org elements."
  (with-temp-buffer
    (org-mode)
    (insert "Simple paragraph text.\n")
    (let* ((tree (org-element-parse-buffer))
           (para (org-element-map tree 'paragraph #'identity nil t)))
      (should (string= (gh/org-docs--extract-plain-text para)
                       "Simple paragraph text."))))
  ;; Text with formatting (spaces around formatting may be collapsed)
  (with-temp-buffer
    (org-mode)
    (insert "Text with *bold* and /italic/ words.\n")
    (let* ((tree (org-element-parse-buffer))
           (para (org-element-map tree 'paragraph #'identity nil t))
           (text (gh/org-docs--extract-plain-text para)))
      ;; Should contain the words without formatting markers
      (should (string-match-p "bold" text))
      (should (string-match-p "italic" text)))))

(ert-deftest gh-org-docs-table-parsing ()
  "Test table parsing extracts cells correctly without text properties."
  (with-temp-buffer
    (org-mode)
    (insert "| A | B | C |\n|---+---+---|\n| 1 | 2 | 3 |\n| x | y | z |\n")
    (let* ((tree (org-element-parse-buffer))
           (table (org-element-map tree 'table #'identity nil t))
           (result (gh/org-docs--table-to-sexp table "test" 0))
           (rows (plist-get (cdr result) :rows)))
      ;; Should have 3 rows (header + 2 data rows, separator is skipped)
      (should (= (length rows) 3))
      ;; First row (header)
      (should (equal (car rows) '("A" "B" "C")))
      ;; Second row
      (should (equal (cadr rows) '("1" "2" "3")))
      ;; Third row
      (should (equal (caddr rows) '("x" "y" "z")))
      ;; Verify strings have no text properties
      (should (not (text-properties-at 0 (caar rows)))))))

(ert-deftest gh-org-docs-image-link-detection ()
  "Test detection of image-only paragraphs."
  ;; Paragraph with only an image link
  (with-temp-buffer
    (org-mode)
    (insert "[[file:test.png]]\n")
    (setq buffer-file-name "/tmp/test.org")
    (let* ((tree (org-element-parse-buffer))
           (para (org-element-map tree 'paragraph #'identity nil t)))
      (should (string= (gh/org-docs--paragraph-image-link para) "test.png"))))
  ;; Paragraph with text and image - should NOT be detected as image-only
  (with-temp-buffer
    (org-mode)
    (insert "Some text [[file:test.png]] more text\n")
    (setq buffer-file-name "/tmp/test.org")
    (let* ((tree (org-element-parse-buffer))
           (para (org-element-map tree 'paragraph #'identity nil t)))
      (should (null (gh/org-docs--paragraph-image-link para)))))
  ;; Non-image link
  (with-temp-buffer
    (org-mode)
    (insert "[[file:document.pdf]]\n")
    (setq buffer-file-name "/tmp/test.org")
    (let* ((tree (org-element-parse-buffer))
           (para (org-element-map tree 'paragraph #'identity nil t)))
      (should (null (gh/org-docs--paragraph-image-link para))))))

(ert-deftest gh-org-docs-src-block-parsing ()
  "Test source block parsing."
  ;; Regular source block (no :file)
  (with-temp-buffer
    (org-mode)
    (insert "#+begin_src python\nprint('hello')\n#+end_src\n")
    (let* ((tree (org-element-parse-buffer))
           (block (org-element-map tree 'src-block #'identity nil t))
           (result (gh/org-docs--src-block-to-sexp block "test" 0)))
      (should result)
      (should (eq (car result) 'paragraph))
      (should (string= (plist-get (cdr result) :text) "print('hello')"))))
  ;; Image-generating source block (with :file) should return nil
  (with-temp-buffer
    (org-mode)
    (insert "#+begin_src mermaid :file diagram.png\ngraph TD\n#+end_src\n")
    (let* ((tree (org-element-parse-buffer))
           (block (org-element-map tree 'src-block #'identity nil t))
           (result (gh/org-docs--src-block-to-sexp block "test" 0)))
      (should (null result)))))

(ert-deftest gh-org-docs-src-block-image-detection ()
  "Test detection of image-generating source blocks."
  ;; With :file parameter
  (with-temp-buffer
    (org-mode)
    (insert "#+begin_src mermaid :file output.png\ncode\n#+end_src\n")
    (let* ((tree (org-element-parse-buffer))
           (block (org-element-map tree 'src-block #'identity nil t)))
      (should (gh/org-docs--src-block-generates-image-p block))))
  ;; Without :file parameter
  (with-temp-buffer
    (org-mode)
    (insert "#+begin_src python\ncode\n#+end_src\n")
    (let* ((tree (org-element-parse-buffer))
           (block (org-element-map tree 'src-block #'identity nil t)))
      (should (not (gh/org-docs--src-block-generates-image-p block))))))

(ert-deftest gh-org-docs-paragraph-to-sexp ()
  "Test paragraph to S-expression conversion."
  (with-temp-buffer
    (org-mode)
    (insert "A simple paragraph.\n")
    (let* ((tree (org-element-parse-buffer))
           (para (org-element-map tree 'paragraph #'identity nil t))
           (result (gh/org-docs--paragraph-to-sexp para "sec-test" 0)))
      (should (eq (car result) 'paragraph))
      (should (string= (plist-get (cdr result) :text) "A simple paragraph."))
      (should (string= (plist-get (cdr result) :custom-id) "sec-test/para-0")))))

(ert-deftest gh-org-docs-list-to-sexp ()
  "Test list to S-expression conversion."
  ;; Unordered list
  (with-temp-buffer
    (org-mode)
    (insert "- Item one\n- Item two\n- Item three\n")
    (let* ((tree (org-element-parse-buffer))
           (lst (org-element-map tree 'plain-list #'identity nil t))
           (result (gh/org-docs--list-to-sexp lst "sec-test" 0)))
      (should (eq (car result) 'list))
      (should (string= (plist-get (cdr result) :type) "unordered"))
      (should (= (length (plist-get (cdr result) :items)) 3))))
  ;; Ordered list
  (with-temp-buffer
    (org-mode)
    (insert "1. First\n2. Second\n")
    (let* ((tree (org-element-parse-buffer))
           (lst (org-element-map tree 'plain-list #'identity nil t))
           (result (gh/org-docs--list-to-sexp lst "sec-test" 0)))
      (should (string= (plist-get (cdr result) :type) "ordered")))))

(ert-deftest gh-org-docs-heading-to-sexp ()
  "Test heading to S-expression conversion."
  (with-temp-buffer
    (org-mode)
    (insert "* Top Level Heading\n:PROPERTIES:\n:CUSTOM_ID: sec-top\n:END:\nContent\n")
    (let* ((tree (org-element-parse-buffer))
           (headline (org-element-map tree 'headline #'identity nil t))
           (result (gh/org-docs--heading-to-sexp headline)))
      (should (eq (car result) 'heading))
      (should (= (plist-get (cdr result) :level) 1))
      (should (string= (plist-get (cdr result) :text) "Top Level Heading"))
      (should (string= (plist-get (cdr result) :custom-id) "sec-top")))))

(ert-deftest gh-org-docs-image-to-sexp ()
  "Test image S-expression generation."
  (with-temp-buffer
    (setq buffer-file-name "/path/to/document.org")
    (let ((result (gh/org-docs--image-to-sexp "images/diagram.png" "sec-test" 0)))
      (should (eq (car result) 'image))
      (should (string= (plist-get (cdr result) :custom-id) "sec-test/image-0"))
      (should (string= (plist-get (cdr result) :path) "/path/to/images/diagram.png"))
      (should (string= (plist-get (cdr result) :alt-text) "diagram")))))

(provide 'gh-org-docs-test)
;;; gh-org-docs-test.el ends here
