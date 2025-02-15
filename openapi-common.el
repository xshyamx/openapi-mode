;;; openapi-common.el --- Common functions  -*- lexical-binding: t; -*-

;; Author:   S. Shyam Sundar (xshyamx@users.noreply.github.com)
;; Version:  0.1

;; Additional stuff
;; Keywords: yaml, openapi, xref

;;; Commentary:

;; Common utility functions

;;; Code:

(rx bol (+ space)
    (? "-") (+ space)
    (group (? (or "'" "\""))) "$ref" (backref 1)
    (* space) ":" (* space)
    (group (? (or "'" "\""))) (group (+? nonl)) (backref 2))
"Regexp to extract '$ref' reference")

(defconst openapi-block-regexp
  (rx bol (group (* space))
      (? "-" (+ space))
      (group (? (or "'" "\""))) (* word) (backref 2)
      (* space) ":" (* nonl)
      eol)
  "Regexp to extract block reference")

(defconst openapi-components-regexp
  (rx (seq bol "components" (* (any space)) ":"))
  "Regexp to match the top-level 'components' key")

(defconst openapi-subsection-key-regexp
  (rx (seq
       bol
       (group (or (repeat 2 " ") (repeat 4 " ")))
       (group (+ (any word "_"))) ":"))
  "Regexp to match subsection & key under 'components' top-level key")

(defconst openapi-json-value-regexp
  (rx (seq
       bol (* space)
       (? "-" (+ space))
       (group (? (any "'" "\""))) (* (any word "_-")) (backref 1)
       (* space) ":" (+ space)
       (group (any " {[")) ))
  "Regexp to match the beginning of a JSON array or object as a key value")

(defconst openapi-path-operation-regexp
  (rx (seq
       bol
       (group (or (repeat 2 " ") (repeat 4 " ")))
       (group (+ (any word "_/{}-"))) ":"))
  "Regexp to match path operations under the `paths' top-level key ")

(defconst openapi-yaml-ref-regexp
  (rx (seq (group (? (any "'\""))) "$ref" (backref 1) ":" (+ space) (group (? (any "'\"")))))
  "Regexp to match '$ref'")

(defun openapi-forward-block ()
  "Move to end of yaml block"
  (interactive)
  (when-let ((b (openapi-block-bounds)))
    (setq-local openapi--block-mark (car b))
    (goto-char (cadr b))))

(defun openapi-backward-block ()
  "Move back to beginning of yaml block"
  (interactive)
  (push-mark)
  (when openapi--block-mark
    (goto-char openapi--block-mark)
    (forward-to-word 1)))

(defun openapi-indent-length ()
  "Find the length of the indent by counting the spaces from
beginning of line to the first non-space character"
  (beginning-of-line)
  (let ((indent-length 0) (el (line-end-position)))
    (while (and (eq ? (char-after))
		(< indent-length eol))
      (setq indent-length (1+ indent-length))
      (forward-char))
    indent-length))


(defun openapi-block-bounds ()
  "Find the bounds for the yaml block under point"
  (save-excursion
    (let ((start) (end) (found)
	  (indent-length))
      (setq start (line-beginning-position))
      (goto-char start)
      (setq indent-length (current-indentation))
      ;; (message "start : %d, indent: %d" start indent-length)
      (while (and (not found)
		  (< (line-end-position) (point-max)))
	;; (message "Looking at % < %d ? '%s'" (line-end-position) (point-max) (thing-at-point 'line nil))
	(when (re-search-forward openapi-json-value-regexp (line-end-position) t)
	  ;; (message "object beginning '%s'" (match-string-no-properties 0))
	  ;; JSON object
	  (goto-char (match-beginning 2)) (forward-list) (forward-line)
	  )
	(forward-line)
	(setq found
	      (and (not (= (current-indentation) (- (line-end-position) (line-beginning-position))))
		   (<= (current-indentation) indent-length))))
      (when found
	(forward-line -1))
      (setq end (if found (line-end-position) (point-max)))
      (list start end))))


(defun openapi-narrow-to-block ()
  "Narrows to yaml block"
  (interactive)
  (let (bounds)
    (save-excursion
      (setq bounds (openapi-block-bounds)))
    (if bounds
	(apply #'narrow-to-region bounds)
      (user-error "Not in a block" ))))


(defun openapi--display-item (item)
  "Returns a display label for path operation"
  (cons
   (format "%7s %s" (upcase (plist-get item :method)) (plist-get item :url))
   item))
(defun openapi-list-paths ()
  "Returns list of all path operations defined in OpenAPI
specification yaml"
  (let ((refs) (subsection))
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(when (re-search-forward "^paths:" nil t)
	  (setq sb (openapi-block-bounds))
	  (apply #'narrow-to-region sb)
	  (goto-char (point-min))
	  (while (re-search-forward openapi-path-operation-regexp nil t)
	    (pcase (/ (length (match-string-no-properties 1)) 2)
	      (1
	       (setq subsection (match-string-no-properties 2)))
	      (2
	       (push
		(list :url subsection :method (match-string-no-properties 2) :buffer-location (match-beginning 2))
		refs)))))))
    (reverse refs)))

(defvar openapi-list-paths-backend
  nil
  "Function to get list of path operations")

(defun openapi-jump-to-operation ()
  "Jumps to path operation selected by the user"
  (interactive)
  (let ((options (funcall openapi-list-paths-backend)))
    (let ((display-list (mapcar #'openapi--display-item options)))
      (let ((selection (minibuffer-with-setup-hook #'minibuffer-complete
			 (completing-read "Goto operation: " display-list nil t))))
	(when-let ((operation (cdr (assoc selection display-list))))
	  (push-mark)
	  (goto-char (plist-get operation :buffer-location)))))))

(defun openapi-copy-block ()
  "Copy the current yaml block at point"
  (interactive)
  (when-let ((b (openapi-block-bounds)))
    (kill-ring-save (car b) (cadr b))))

(defun openapi-kill-block ()
  "Cut the current yaml block at point"
  (interactive)
  (when-let ((b (openapi-block-bounds)))
    (kill-region (car b) (cadr b))))

(defun openapi-select-block ()
  "Select the current yaml block at point"
  (interactive)
  (when-let ((b (openapi-block-bounds)))
    (goto-char (car b))
    (push-mark (point) nil t)
    (goto-char (cadr b))
    (setq mark-active t)
    ))

(defvar openapi--yaml-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "C-M-f") #'openapi-forward-block)
    (define-key keymap (kbd "C-M-b") #'openapi-backward-block)
    (define-key keymap (kbd "C-x n b") #'openapi-narrow-to-block)
    (define-key keymap (kbd "C-c C-w") #'openapi-copy-block)
    (define-key keymap (kbd "C-c C-k") #'openapi-kill-block)
    (define-key keymap (kbd "C-c C-SPC") #'openapi-select-block)
    keymap)
  "Common keybings to navigate & narrow to yaml blocks")

(defvar-local openapi--block-mark
    nil
  "Stores the current block beginning position")

(defun openapi--add-font-lock-extras ()
  "Additional font-lock rules specific to OpenAPI"
  (let ((mime-types '("application/json"
		      "application/xml"
		      "application/x-www-form-urlencoded"
		      "multipart/form-data"
		      "text/plain"
		      "text/html"
		      "application/pdf"
		      "image/png"))
	(rules))
    ;; mime-types
    (push (list
	   (rx-to-string
	    `(seq bol (+ whitespace)
		  (group (? (any "\"'")))
		  (group (or ,@mime-types))
		  (? (backref 1))
		  (* whitespace) ":"))
	   1 ''font-lock-keyword-face)
	  rules)
    ;; important keys
    (push (list
	   (rx  bol (+ whitespace)
		(group (or "parameters" "requestBody" "responses"))
		(* whitespace) ":")
	   1 ''font-lock-keyword-face)
	  rules)
    ;; operationId
    (push (list
	   (rx  bol (+ whitespace) (group (? (any "\"'")))
		(group "operationId") (? (backref 1)) (* whitespace)
		":" (+ whitespace) (group (+ any)))
	   '(2 'font-lock-function-name-face)
	   '(3 'font-lock-constant-face))
	  rules)
    ;; http-methods
    (push (list
	   (rx  bol (+ whitespace) bow (group (or "get" "put" "post" "patch" "head" "delete")) eow (* whitespace) ":")
	   1 ''font-lock-function-name-face)
	  rules)
    ;; response-codes
    (push (list
	   (rx  bol (+ whitespace) bow (group (any "12345") (repeat 2 digit)) eow (* whitespace) ":")
	   1 ''font-lock-constant-face)
	  rules)
    ;; path-parameter
    (push (list
	   (rx  "/" (group "{" (*? word) "}"))
	   1 ''font-lock-keyword-face t)
	  rules)
    ;; path
    (push (list
	   (rx bol (+ whitespace) (group "/" (*? (not (any ":")))) ":")
	   1 ''font-lock-builtin-face)
	  rules)
    (font-lock-add-keywords 'yaml-mode rules)))

(provide 'openapi-common)
;;; openapi-common.el ends here
