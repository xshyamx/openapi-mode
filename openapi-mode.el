(require 'cl-lib)
(require 'xref)
(require 'openapi-common)
;; regexp constants
(defun openapi-xref-backend ()
  "ref backend identifier for openapi"
  'openapi)

;; define the indentifier
(cl-defmethod xref-backend-identifier-at-point ((backend (eql openapi)))
  (openapi--identifier-at-point))

;; xref-backend
(cl-defmethod xref-backend-identifier-completion-table ((backend (eql openapi)))
  "Return list of terms for completion from the current buffer"
  (openapi--list-definitions))

(cl-defmethod xref-backend-definitions ((backend (eql openapi)) symbol)
  "Find ref definitions for SYMBOL"
  (openapi--find-definitions symbol))

(cl-defmethod xref-backend-references ((backend (eql openapi)) symbol)
  "Find all references matching SYMBOL"
  (openapi--find-references symbol))

;; helpers
(defun is-openapi-file ()
  "Function returns t if current buffer is a openapi file by
checking if the top-level key 'openapi' is present"
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (re-search-forward "^openapi:" nil t)
	t))))

(defun openapi--identifier-at-point ()
  "Looks at current line to find if a component reference is present"
  (save-excursion
    (goto-char (line-beginning-position))
    (when (re-search-forward openapi-ref-regexp (line-end-position) t)
      (when-let ((ref (match-string-no-properties 3)))
	;; (message "identifier : %s" ref)
	(let ((tokens (split-string ref "/")))
	  (when (and (eql 4 (length tokens)) (string-equal "#" (car tokens)))
	    (let ((id (string-join (cdr tokens) "/")))
	      ;; (message "id: %s" id)
	      id)))))))

(defun openapi--list-definitions ()
  "Returns list of all available component definitions"
  (let ((refs) (subsection))
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(when (re-search-forward openapi-components-regexp nil t)
	  (setq sb (openapi-block-bounds))
	  (apply #'narrow-to-region sb)
	  (goto-char (point-min))
	  (while (re-search-forward openapi-subsection-key-regexp nil t)
	    (pcase (/ (length (match-string-no-properties 1)) 2)
	      (1
	       (setq subsection (match-string-no-properties 2)))
	      (2
	       (push (format "components/%s/%s" subsection (match-string-no-properties 2)) refs)))))))
    (reverse refs)))

(defun openapi--list-sections ()
  "Returns the lit of sections inside the 'components' key"
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (re-search-forward openapi-components-regexp nil t)
	(message "found tld")
	(let ((b (openapi-block-bounds)))
	  (apply #'narrow-to-region (openapi-block-bounds))
	  (goto-char (point-min))
	  (let (sections)
	    (while (re-search-forward (format "^%s\\([[:word:]]+\\):" (make-string 2 ? )) nil t)
	      (push
	       (cons (format "components/%s" (match-string-no-properties 1)) (match-beginning 1))
	       sections))
	    (reverse sections)))))))

(defun openapi--find-references (symbol)
  "Find schema references within the buffer of the
form $ref: '#/components/subsection/key' for the SYMBOL"
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
	     (propertize (format "%s" (line-number-at-pos (line-beginning-position))) 'face 'shadow)
	     (buffer-substring (line-beginning-position) (line-end-position))
	     )
	    (xref-make-buffer-location (current-buffer) (match-beginning 0)))
	   refs))
	(append (reverse refs) (openapi--find-definitions symbol))))))

(defun openapi--find-definitions (symbol)
  "Return the ref objects for the definitions found for SYMBOL
within the buffer"
  (let* ((tokens (split-string symbol "/"))
	 (section (car tokens))
	 (subsection (cadr tokens))
	 (key (caddr tokens)))
    (when (and section subsection key)
      (save-excursion
	(save-restriction
	  (widen)
	  (goto-char (point-min))
	  (when (re-search-forward (format "^%s:" section) nil t)
	    (setq sb (openapi-block-bounds))
	    (apply #'narrow-to-region sb)
	    (goto-char (point-min))
	    ;; (message "looking at section : '%s'" (buffer-substring-no-properties (point-min) (point-max)))
	    (when (re-search-forward (format "^%s%s:" (make-string 2 ? ) subsection) (cadr sb) t)
	      (setq ssb (openapi-block-bounds))
	      (apply #'narrow-to-region ssb)
	      (goto-char (point-min))
	      ;; (message "looking at subsection: '%s'" (buffer-substring-no-properties (point-min) (point-max)))
	      (when (re-search-forward (format "^%s%s:" (make-string 4 ? ) key) nil t)
		;; (message "match : '%s' %d / %" (match-string-no-properties 0) (match-beginning 0) (line-beginning-position))
		(list
		 (xref-make
		  (buffer-substring-no-properties (line-beginning-position) (line-end-position))
		  (xref-make-buffer-location (current-buffer) (match-beginning 0))))))))))))

(defun openapi-jump-to-section ()
  "Jumps to selected component section"
  (interactive)
  (let ((options (openapi--list-sections)))
    (let ((selection (completing-read "Select section: " options nil t "components/")))
      (when-let ((section (assoc selection options)))
	(push-mark)
	(print section)
	(goto-char (cdr section))))))

(defun openapi-insert-component-ref ()
  "Inserts a component reference at point"
  (interactive)
  (when-let
      ((selection
	(completing-read "Select component: " (openapi--list-definitions)
			 nil t "components/")))
    (insert
     (format
      (if (looking-back openapi-yaml-ref-regexp (line-beginning-position))
	  (if (> (length (match-string-no-properties 2)) 0)
	      "#/%s"
	    "'#/%s'"
	    )
	"$ref: '#/%s'"
	)
      selection))))

(defvar openapi-mode-map
  (let ((keymap (make-sparse-keymap)))
    (set-keymap-parent keymap openapi--yaml-mode-map)
    (define-key keymap (kbd "C-c C-p") #'openapi-jump-to-operation)
    (define-key keymap (kbd "C-c C-s") #'openapi-jump-to-section)
    (define-key keymap (kbd "C-c C-c") #'openapi-insert-component-ref)
    keymap)
  "OpenAPI keymap for block & path-operation navigation"
  )

(define-minor-mode openapi-mode
  "Minor mode to provide xref navigation within OpenAPI yaml files"
  :lighter " OpenAPI"
  (setq-local openapi-list-paths-backend #'openapi-list-paths)
  (add-hook 'xref-backend-functions #'openapi-xref-backend nil t))

(defun enable-openapi-mode ()
  "Conditionally enable openapi mode"
  (when (is-openapi-file)
    (openapi-mode)))

;;;###autoload
(add-hook 'yaml-mode-hook #'enable-openapi-mode)

(provide 'openapi-mode)
