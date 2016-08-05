
(defconst brain-const-max-height 7)

(defconst brain-const-readonly-mode "readonly")
(defconst brain-const-readwrite-mode "readwrite")
(defconst brain-const-search-mode "search")

(defconst brain-const-color-by-sharability "sharability")
(defconst brain-const-color-by-class-inference "inference")

(defconst brain-const-forward-style "forward")
(defconst brain-const-backward-style "backward")

(defconst brain-const-sharability-private 0.25)
(defconst brain-const-sharability-personal 0.5)
(defconst brain-const-sharability-universal 1.0)
(defconst brain-const-weight-none 0.0)
(defconst brain-const-weight-default 0.5)
(defconst brain-const-weight-all 1.0)

(defconst brain-const-date-format "%Y-%m-%d")
(defconst brain-const-time-format "%H:%M")
(defconst brain-const-time-with-seconds-format "%H:%M:%S")

(defun brain-env-context (&optional context)
  (if context context brain-bufferlocal-context))

(defun brain-env-context-for-request (&optional context)
  (let ((ctx (if context context (brain-env-context))))
    (set-view nil ctx)
    (set-atoms-by-id nil ctx)
    ctx))

(defun brain-env-set-context (context)
  (setq brain-bufferlocal-context context)
  (make-local-variable 'brain-bufferlocal-context))

(defun refresh-context (&optional context)
  (set-line (line-number-at-pos) context))

;; change the default sharability in the new view after a user visits a link or target
;; The default will never be greater than 0.75 unless explicitly set by the user.
(defun adjust-default-sharability (sharability)
  (if sharability
      (if (<= sharability 0.75) sharability 0.75)
    0.5))

(defun brain-env-clone-context (&optional context)
  (refresh-context context)
  (let (
    (context (copy-alist (brain-env-context context)))
    (sharability (brain-data-target-sharability)))
      (set-default-sharability (adjust-default-sharability sharability) context)
      context))

(defun get-value (key json)
  (cdr (assoc key json)))

(defun get-bufferlocal (context key)
  (get-value key (brain-env-context context)))

(defun set-bufferlocal (context key value)
  (setcdr (assoc key (brain-env-context context)) value))

(defun get-action (&optional context) (get-bufferlocal context 'action))
(defun get-atoms-by-id (&optional context) (get-bufferlocal context 'atoms-by-id))
(defun get-default-sharability (&optional context) (get-bufferlocal context 'default-sharability))
(defun get-default-weight (&optional context) (get-bufferlocal context 'default-weight))
(defun get-file (&optional context) (get-bufferlocal context 'file))
(defun get-format (&optional context) (get-bufferlocal context 'format))
(defun get-height (&optional context) (get-bufferlocal context 'height))
(defun get-line (&optional context) (get-bufferlocal context 'line))
(defun get-max-sharability (&optional context) (get-bufferlocal context 'max-sharability))
(defun get-max-weight (&optional context) (get-bufferlocal context 'max-weight))
(defun get-min-sharability (&optional context) (get-bufferlocal context 'min-sharability))
(defun get-min-weight (&optional context) (get-bufferlocal context 'min-weight))
(defun get-minimize-verbatim-blocks (&optional context) (get-bufferlocal context 'minimize-verbatim-blocks))
(defun get-mode (&optional context) (get-bufferlocal context 'mode))
(defun get-query (&optional context) (get-bufferlocal context 'query))
(defun get-query-type (&optional context) (get-bufferlocal context 'query-type))
(defun get-root-id (&optional context) (get-bufferlocal context 'root-id))
(defun get-style (&optional context) (get-bufferlocal context 'style))
(defun get-title (&optional context) (get-bufferlocal context 'title))
(defun get-value-length-cutoff (&optional context) (get-bufferlocal context 'value-length-cutoff))
(defun get-view (&optional context) (get-bufferlocal context 'view))
(defun get-view-properties (&optional context) (get-bufferlocal context 'view-properties))
(defun get-view-style (&optional context) (get-bufferlocal context 'view-style))

(defun set-action (action &optional context) (set-bufferlocal context 'action action))
(defun set-atoms-by-id (atoms-by-id &optional context) (set-bufferlocal context 'atoms-by-id atoms-by-id))
(defun set-file (file &optional context) (set-bufferlocal context 'file file))
(defun set-format (format &optional context) (set-bufferlocal context 'format format))
(defun set-default-sharability (sharability &optional context) (set-bufferlocal context 'default-sharability sharability))
(defun set-default-weight (weight &optional context) (set-bufferlocal context 'default-weight weight))
(defun set-height (height &optional context) (set-bufferlocal context 'height height))
(defun set-line (line &optional context) (set-bufferlocal context 'line line))
(defun set-max-sharability (sharability &optional context) (set-bufferlocal context 'max-sharability sharability))
(defun set-max-weight (weight &optional context) (set-bufferlocal context 'max-weight weight))
(defun set-min-sharability (sharability &optional context) (set-bufferlocal context 'min-sharability sharability))
(defun set-min-weight (weight &optional context) (set-bufferlocal context 'min-weight weight))
(defun set-minimize-verbatim-blocks (flag &optional context) (set-bufferlocal context 'minimize-verbatim-blocks flag))
(defun set-mode (mode &optional context) (set-bufferlocal context 'mode mode))
(defun set-query (query &optional context) (set-bufferlocal context 'query query))
(defun set-query-type (type &optional context) (set-bufferlocal context 'query-type type))
(defun set-root-id (root-id &optional context) (set-bufferlocal context 'root-id root-id))
(defun set-style (style &optional context) (set-bufferlocal context 'style style))
(defun set-title (title &optional context) (set-bufferlocal context 'title title))
(defun set-value-length-cutoff (cutoff &optional context) (set-bufferlocal context 'value-length-cutoff cutoff))
(defun set-view (view &optional context) (set-bufferlocal context 'view view))
(defun set-view-properties (view-properties &optional context) (set-bufferlocal context 'view-properties view-properties))
(defun set-view-style (view-style &optional context) (set-bufferlocal context 'view-style view-style))

(defun default-context () (list
  (cons 'action 'nil)
  (cons 'atoms-by-id 'nil)
  (cons 'default-sharability brain-const-sharability-personal)
  (cons 'default-weight brain-const-weight-default)
  (cons 'file 'nil)
  (cons 'format 'nil)
  (cons 'height 2)
  (cons 'line 1)
  (cons 'max-sharability brain-const-sharability-universal)
  (cons 'max-weight brain-const-weight-all)
  (cons 'min-sharability brain-const-sharability-private)
  (cons 'min-weight brain-const-weight-none)
  (cons 'minimize-verbatim-blocks 'nil)
  (cons 'mode brain-const-readonly-mode)
  (cons 'query 'nil)
  (cons 'query-type 'nil)
  (cons 'root-id 'nil)
  (cons 'style brain-const-forward-style)
  (cons 'title 'nil)
  (cons 'value-length-cutoff 100)
  (cons 'view 'nil)
  (cons 'view-properties 'nil)
  (cons 'view-style brain-const-color-by-sharability)))

(defun brain-env-define-buffer-local-variables ()
  (defvar brain-bufferlocal-context (default-context)))

(defun brain-env-numeric-value (json prop default)
  (let ((v (assoc prop json)))
    (if v (string-to-number (cdr v)) default)))

(defun brain-env-parse-context (json context)
  (set-min-sharability (brain-env-numeric-value json 'minSharability (get-min-sharability context)))
  (set-max-sharability (brain-env-numeric-value json 'maxSharability (get-max-sharability context)))
  (set-default-sharability (brain-env-numeric-value json 'defaultSharability (get-default-sharability context)))
  (set-min-weight (brain-env-numeric-value json 'minWeight (get-min-weight context)))
  (set-max-weight (brain-env-numeric-value json 'maxWeight (get-max-weight)))
  (set-default-weight (brain-env-numeric-value json 'defaultWeight (get-default-weight)))
  (set-root-id (get-value 'root json))
  (set-height
    ;; Always leave a search view with height 1, rather than that of the last view.
    ;; The user experience is a little unpredictable otherwise.
    (if (equal (get-mode) brain-const-search-mode)
      1
      (brain-env-numeric-value json 'height (get-height context))))
  (let ((style (get-value 'style json)))
    (if style (set-style style)))
  (set-title (get-value 'title json))
  (set-atoms-by-id (make-hash-table :test 'equal)))

(defun brain-env-debug-message (msg)
  (message "%s" (concat "Debug: " msg)))

(defun brain-env-info-message (msg)
  (message "%s" (concat "Info: " msg)))

(defun brain-env-error-message (msg)
  (message "%s" (concat "Error: " msg)))

(defun brain-env-error-no-target ()
  (brain-env-error-message "there is no target associated with this line"))

(defun brain-env-create-search-context ()
  (let ((context (brain-env-clone-context)))
    (set-mode brain-const-search-mode context)
    (set-height 1 context)
    (set-line 1 context)
    context))

(defun brain-env-using-inference ()
  (equal (get-view-style) brain-const-color-by-class-inference))

(defun mode-for-visit ()
  (if (or (equal (get-mode) brain-const-readwrite-mode) (equal (get-mode) brain-const-readonly-mode))
      (get-mode)
    brain-const-readonly-mode))

(defun brain-env-succeed () t)

(defun brain-env-fail (message)
  (and (brain-env-error-message message) nil))

(defun brain-env-in-readonly-mode ()
  (equal (get-mode) brain-const-readonly-mode))

(defun brain-env-in-readwrite-mode ()
  (equal (get-mode) brain-const-readwrite-mode))

(defun brain-env-in-search-mode ()
  (equal (get-mode) brain-const-search-mode))

(defun brain-env-set-readonly (&optional context)
   (set-mode brain-const-readonly-mode context))

(defun brain-env-set-readwrite (&optional context)
   (set-mode brain-const-readwrite-mode context))

(defun brain-env-set-forward-style (&optional context)
   (set-style brain-const-forward-style context))

(defun brain-env-set-backward-style (&optional context)
   (set-style brain-const-backward-style context))

(defun brain-env-in-view-mode ()
  (let ((mode (get-mode)))
    (if (or
        (equal mode brain-const-readonly-mode)
        (equal mode brain-const-readwrite-mode))
      t
    (brain-env-fail (concat "cannot create tree view in mode '" mode "'")) nil)))

(defun brain-env-in-setproperties-mode ()
  "determines whether atom properties can be set in the current buffer"
  (if (or
       (equal (get-mode) brain-const-search-mode)
       (equal (get-mode) brain-const-readonly-mode)
       (equal (get-mode) brain-const-readwrite-mode))
      t
    (brain-env-fail "cannot set properties in current mode") nil))

(defun brain-env-is-readwrite-context (&optional context)
  (let ((mode (get-mode context)))
    (and mode
      (equal mode brain-const-readwrite-mode))))

(defun brain-env-assert-readwrite-context ()
  (if (brain-env-is-readwrite-context)
    (brain-env-succeed)
    (brain-env-fail (concat "cannot update view in current mode: " (get-mode))) nil))

(defun brain-env-assert-height-in-bounds (height)
  "Asserts that a view height is positive and not greater than the maximum allowed height"
  (if (< height 1)
    (brain-env-fail (concat "height of " (number-to-string height) " is too low (must be >= 1)"))
    (if (> height brain-const-max-height)
      (brain-env-fail (concat "height of " (number-to-string height) " is too high (must be <= "
                                 (number-to-string brain-const-max-height) ")"))
      (brain-env-succeed))))

(defun brain-env-toggle-inference-viewstyle (&optional context)
  (set-view-style
    (if (equal (get-view-style) brain-const-color-by-sharability)
      brain-const-color-by-class-inference
      brain-const-color-by-sharability) context))

(defun brain-env-format-date (time)
  (format-time-string brain-const-date-format time))

(defun brain-env-format-time (time &optional with-seconds)
  (format-time-string
    (if with-seconds
        brain-const-time-with-seconds-format
        brain-const-time-format)
    time))


(provide 'brain-env)
