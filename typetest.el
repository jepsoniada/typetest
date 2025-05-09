;; (define-derived-mode result-mode special-mode "Result")

(require 'dash)

(defvar typetest--buffer nil
  "currently used buffer")

(defvar typet--text ""
  "text prepared for typetesting")

(defvar typetest--time 0
  "epoch time when test started")

(defvar typetest--overlay nil
  "overlay for typetest--buffer")

(defcustom typetest-finish-on-full-completeness t
  "boolean to determine style of typetest finishing condition"
  :type 'boolean)

(defcustom typetest-source 'quotes-english
  "choose quote pool for `typetest-quote'
it must be one of the entries from `typetest-source-bindings'"
  :type 'symbol)

(defcustom typetest-source-bindings
  '((quotes-english . ((url . "https://raw.githubusercontent.com/monkeytypegame/monkeytype/master/frontend/static/quotes/english.json")
		       (filename . "quotes-english.json")
		       (root-getter . (lambda (json) (gethash "quotes" json)))
		       (sample-getter . (lambda (json) (gethash "text" json))))))
  "Alist of source name to source data bindings.
The source data is alist of following shape:
	url - url where json of samples is stored
	filename - where to save downloaded json
	root-getter - function to get the root of your samples
	sample-getter - function to get list of samples
")

(defface typetest-text '((t ( :inherit font-lock-comment-face)))
  "text to write")

(defface typetest-error '((t ( :foreground "#cc6666"
			       :underline ( :color foreground-color
					    :style wave))))
  "on error")

;; BUG (?) - atribute ":inherit default" doesn't apply theme colors
;; current implementation needs to restart at every change of theme to take effect
(defface typetest-correct `((t ( :foreground ,(face-attribute 'default :foreground)
				 :underline t)))
  "on success")

(defun typetest-fetch-sources (&rest sources)
  (ignore-errors (make-directory (concat user-emacs-directory
					 "typetest")))
  (cl-loop for source in sources
	   for source = (alist-get source
				   typetest-source-bindings)
	   when source
	   for filename = (alist-get 'filename
				     source)
	   when source
	   for body = (let* ((buffer (url-retrieve-synchronously (alist-get 'url
									    source)))
			     (http-string (with-current-buffer buffer
					    (buffer-string)))
			     (http-code (string-to-number (nth 1
							       (string-split (substring http-string
											0
											(string-search "\n"
												       http-string))))))
			     (http-body (substring http-string
						   (+ 1
						      (string-search "\n\n"
								     http-string
								     (string-search "\n\n"
										    http-string))))))
			(when (= http-code 200)
			  http-body))
	   when (and body source)
	   do (with-temp-buffer
		(insert body)
		(set-buffer-file-coding-system 'raw-text)
		(write-region nil nil (concat user-emacs-directory
					      "typetest/"
					      filename)))))

(defun typetest--finish ()
  "thinks to run on test finish"
  (read-only-mode 1)

  (let* ((time-elapsed-in-seconds (- (float-time) typetest--time))
	 (words (/ (length typetest--text) 5)))
    (message (format "your wpm is: %f"
		     (/ words (/ time-elapsed-in-seconds 60))))
    )
  )

(defun typetest--text-processing (overlay is-after-edit begin end &optional length)
  "processing faces and after-string of overlay. Compatible with modification-hooks"
  (if is-after-edit
      (progn
	(overlay-put overlay 'after-string
		     (condition-case nil
			 (substring typetest--text (1- (overlay-end overlay)))
		       (args-out-of-range (lambda () ""))))
	(mapcar (lambda (x)
		  (set-text-properties (+ 1 (nth 0 x)) (+ 1 (nth 1 x)) '(face 'typetest-error)))
		(string-diff (buffer-substring-no-properties (point-min) (point-max))
			     typetest--text))
	(when (if typetest-finish-on-full-completeness
		  (and (= (length typetest--text) (length (buffer-substring-no-properties (point-min) (point-max))))
		       (string-equal typetest--text (buffer-substring-no-properties (point-min) (point-max))))
		(<= (length typetest--text) (length (buffer-substring-no-properties (point-min) (point-max)))))
	  (typetest--finish)))
    (progn
      (set-text-properties (point-min) (point-max) '(face 'typetest-correct)))))

(defun chunk-by (by list) "splits LIST into BY sized chunks"
       (mapcar (lambda (y) (mapcar (lambda (z) (nth z list)) y))
	       (mapcar (lambda (x)
			 (number-sequence (- (* by x) by) (1- (* by x))))
		       (number-sequence 1 (/ (length list) by)))
	       )
       )

(defun string-diff (a b) "string diff"
       (if (= 0 (length a))
	   '((0 0))
	 (let* ((diff-by-char (mapcar (lambda (x) (if (= (car x) (cdr x)) 0 1))
				      (-zip (append a nil) (append b nil))))
		(diff-change-points (mapcar (lambda (x) (if (eq (car x) (cdr x)) 0 1))
					    (-zip (cdr diff-by-char) diff-by-char)))
		(diff-change-unfiltered-indexes
		 (-zip-with #'*
			    diff-change-points
			    (number-sequence 1 (length diff-by-char))))
		(diff-change-indexes (-filter (lambda (x) (> x 0))
					      diff-change-unfiltered-indexes))
		(prepended (if (> (car diff-by-char) 0)
			       `(0 . ,diff-change-indexes)
			     diff-change-indexes))
		(appended (if (= 1 (% (length prepended) 2))
			      (append prepended `(,(length a)))
			    prepended))
		)
	   (chunk-by 2 appended)
	   ))
       )

(defun typetest-buffer ()
  "test-type text in buffer"
  (interactive)
  (set 'typetest--buffer (get-buffer-create "*typetest*"))
  (set 'typetest--text (buffer-substring-no-properties (point-min) (point-max)))
  (set-buffer typetest--buffer)
  (read-only-mode -1)
  (visual-line-mode 1)
  (erase-buffer)
  (when typetest--overlay
    (delete-overlay typetest--overlay))
  (make-local-variable 'first-change-hook)
  (add-hook 'first-change-hook (lambda () (setq typetest--time (float-time))))
  (set 'typetest--overlay (make-overlay 0 0 typetest--buffer nil t))
  (overlay-put typetest--overlay 'after-string typetest--text)
  (overlay-put typetest--overlay 'modification-hooks '(typetest--text-processing))
  (overlay-put typetest--overlay 'insert-behind-hooks '(typetest--text-processing))
  (buffer-face-mode t)
  (buffer-face-set 'typetest-text)
  (switch-to-buffer typetest--buffer)
  (not-modified)
  )

(defun typetest-quote ()
  "gets random quote to open typetest buffer with it"
  (interactive)
  (let* ((filename (alist-get 'filename
			      (alist-get typetest-source
					 typetest-source-bindings)))
	 (json-string (with-temp-buffer
			(insert-file-contents (concat user-emacs-directory
						      "typetest/"
						      filename))
			(buffer-string)))
	 (json (json-parse-string json-string)))
    (let ((root-getter (eval-expression (alist-get 'root-getter
						   (alist-get typetest-source
							      typetest-source-bindings))))
	  (sample-getter (eval-expression (alist-get 'sample-getter
						     (alist-get typetest-source
								typetest-source-bindings)))))
      (with-temp-buffer
	(insert (funcall sample-getter
			 (elt (funcall root-getter json)
			      (random (length (funcall root-getter json))))))
	(typetest-buffer)))))


(provide 'typetest)
