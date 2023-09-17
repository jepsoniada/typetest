;; (define-derived-mode result-mode special-mode "Result")

(defvar typetest--buffer nil
  "currently used buffer")

(defvar typet--text ""
  "text prepared for typetesting")

(defvar typetest--time 0
  "epoch time when test started")

(defface typetest-error '((t ( :foreground "#cc6666"
			    :underline ( :color foreground-color
					 :style wave))))
  "on error")

(defface typetest-correct '((t ( :foreground "#ffffff"
				 :underline t)))
  "on success")

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
			 (substring typetest--text (1- end))
		       (args-out-of-range (lambda () ""))))
	(mapcar (lambda (x)
		  (set-text-properties (+ 1 (nth 0 x)) (+ 1 (nth 1 x)) '(face 'typetest-error))
		  )
		(string-diff (buffer-substring-no-properties (point-min) (point-max))
			     typetest--text))
	(unless (> (length typetest--text) (length (buffer-substring-no-properties (point-min) (point-max))))
	  (typetest--finish))
	)
    (progn
      (set-text-properties (point-min) (point-max) '(face 'typetest-correct))
      ))
  )

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
  (erase-buffer)
  (make-local-variable 'first-change-hook)
  (add-hook 'first-change-hook (lambda () (setq typetest--time (float-time))))
  (defvar-local overlay (make-overlay 0 0 typetest--buffer nil t))
  (overlay-put overlay 'after-string typetest--text)
  (overlay-put overlay 'modification-hooks '(typetest--text-processing))
  (overlay-put overlay 'insert-behind-hooks '(typetest--text-processing))
  (buffer-face-mode t)
  (buffer-face-set 'font-lock-comment-face)
  (switch-to-buffer typetest--buffer)
  (not-modified)
  )
