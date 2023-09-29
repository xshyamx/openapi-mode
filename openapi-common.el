(defconst openapi-ref-regexp
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
  (rx (seq (group (? (any "'\""))) "$ref" (backref 1) ": " (any "'\"")))
  "Regexp to match '$ref'")

(defun openapi-forward-block ()
  "Move to end of yaml block"
  (interactive)
  (push-mark)
  (when-let ((b (openapi-block-bounds)))
    (goto-char (cadr b))))

(defun openapi-backward-block ()
  "Move back to beginning of yaml block"
  (interactive)
  (push-mark)
  (when-let ((b (openapi-block-bounds)))
    (goto-char (car b))))

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

(defvar openapi--yaml-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "C-M-f") #'openapi-forward-block)
    (define-key keymap (kbd "C-M-b") #'openapi-backward-block)
    (define-key keymap (kbd "C-x n b") #'openapi-narrow-to-block)
    keymap)
  "Common keybings to navigate & narrow to yaml blocks")
(provide 'openapi-common)
