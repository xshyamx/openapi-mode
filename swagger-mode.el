;;; swagger-mode.el --- xref backend for swagger files

;; Author:   S. Shyam Sundar (xshyamx@users.noreply.github.com)
;; Version:  0.1

;; Additional stuff
;; Keywords: yaml, swagger, xref

;;; Commentary (S. Shyam Sundar):

;; Added `xref-backend' to support jumping to schema references within
;; the swagger yaml file.


(require 'cl-lib)
(require 'xref)
(require 'openapi-common)

(defun swagger-xref-backend ()
  "xref backend identifier for swagger"
  'swagger)

;; define the indentifier
(cl-defmethod xref-backend-identifier-at-point ((backend (eql swagger)))
  (let ((filename (thing-at-point 'filename t)))
    (when filename
      (let ((tokens (split-string filename "/")))
	(when (and (eql 3 (length tokens)) (string-equal "#" (car tokens)))
	  (string-join (cdr tokens) "/"))))))

;; xref-backend
(cl-defmethod xref-backend-identifier-completion-table ((backend (eql swagger)))
  "Return list of terms for completion from the current buffer"
  (swagger--list-definitions))
(cl-defmethod xref-backend-definitions ((backend (eql swagger)) symbol)
  "Find xref definitions for SYMBOL"
  (swagger--find-definitions symbol))
(cl-defmethod xref-backend-references ((backend (eql swagger)) symbol)
  "Find all references matching SYMBOL"
  (swagger--find-references symbol))

;; helpers
(defun is-swagger-file ()
  "Function returns t if current buffer is a swagger file by
checking if the top-level key `swagger' is present"
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (re-search-forward "^swagger:" nil t)
	t))))

(defun swagger--list-definitions ()
  "Find all definitions in the swagger file by narrowing to the
top-level key `definitions'"
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((refs) (start) (end))
	(when (re-search-forward "^definitions:" nil t)
	  ;; found section
	  (setq start (match-beginning 0))
	  ;; search for next top-level key or set to max
	  (setq end
		(if (re-search-forward "^[[:word]]+:" nil t)
		    (match-beginning 0)
		  (point-max)))
	  ;; narrow and search definitions
	  (narrow-to-region start end)
	  (goto-char (point-min))
	  (while (re-search-forward "^  \\([[:word:]_-]+\\)" nil t)
	    (push
	     (format "definitions/%s" (match-string-no-properties 1))
	     refs))
	  (seq-uniq refs))))))

(defun swagger--find-references (symbol)
  "Find schema references within the buffer of the form
$ref: '#/definitions/key'"
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let (refs)
	(while (re-search-forward (format "#/%s" symbol) nil t)
	  (push
	   (xref-make
	    (format
	     "%s:%s"
	     (propertize (format "%d" (line-number-at-pos (line-beginning-position))) 'face 'shadow)
	     (buffer-substring (line-beginning-position) (line-end-position))
	     )
	    (xref-make-buffer-location (current-buffer) (match-beginning 0)))
	   refs))
	(reverse refs)))))

(defun swagger--find-definitions (symbol)
  "Return the xref objects for the definitionsf found for SYMBOL
within the buffer"
  (let* ((tokens (split-string symbol "/"))
	 (section (car tokens))
	 (key (cadr tokens)))
    (when (and section key)
      (save-excursion
	(save-restriction
	  (widen)
	  (goto-char (point-min))
	  (when (re-search-forward (format "^%s:" section) nil t)
	    ;; found section
	    (when (re-search-forward (format "^  %s:" key) nil t)
	      ;; found key
	      (list
	       (xref-make
		(buffer-substring-no-properties (line-beginning-position) (line-end-position))
		(xref-make-buffer-location (current-buffer) (match-beginning 0)))))))))))

(defun swagger-insert-definition-ref ()
  "Inserts a component reference at point"
  (interactive)
  (when-let
      ((selection
	(completing-read "Select definition: " (swagger--list-definitions)
			 nil t)))
    (insert (format
	     (if (looking-back openapi-yaml-ref-regexp (line-beginning-position))
		 "#/%s"
	       "$ref: '#/%s'")
	     selection))))
(defvar swagger-mode-map
  (let ((keymap (make-sparse-keymap)))
    (set-keymap-parent keymap openapi--yaml-mode-map)
    (define-key keymap (kbd "C-c C-p") #'openapi-jump-to-operation)
    (define-key keymap (kbd "C-c C-c") #'swagger-insert-definition-ref)
    keymap)
  "Swagger keymap for block & path-operation navigation")

(define-minor-mode swagger-mode
  "Minor mode to provide xref navigation within swagger yaml files"
  :lighter " Swagger"
  (setq-local openapi-list-paths-backend #'openapi-list-paths)
  (add-hook 'xref-backend-functions #'swagger-xref-backend nil t))

(defun enable-swagger-mode ()
  "Conditionally enable swagger mode"
  (when (is-swagger-file)
    (swagger-mode)))

;;;###autoload
(add-hook 'yaml-mode-hook #'enable-swagger-mode)

(provide 'swagger-mode)
