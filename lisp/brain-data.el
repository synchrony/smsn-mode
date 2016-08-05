(require 'brain-env)


(defun current-line ()
  (interactive)
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun brain-data-atom-id-at-point ()
  (let ((line (current-line)))
    (if (string-match "^[0-9A-Za-z@&]*: " line)
        (let ((i3 (string-match ": " line)))
          (let ((s2 (substring line 0 i3)))
            (if (< 0 (length s2)) s2 nil)))
      (get-text-property (line-beginning-position) 'target-id))))

(defun brain-data-atom-id (atom)
  (get-value 'id atom))

(defun brain-data-atom-created (atom)
  (get-value 'created atom))

(defun brain-data-atom-value (atom)
  (get-value 'value atom))

(defun brain-data-atom-priority (atom)
  (let ((v (assoc 'priority atom)))
    (if v (cdr v) nil)))

(defun brain-data-atom-sharability (atom)
  (let ((v (assoc 'sharability atom)))
    (if v (cdr v) (get-default-sharability))))

(defun brain-data-atom-weight (atom)
  (let ((v (assoc 'weight atom)))
    (if v (cdr v) (get-default-weight))))

(defun brain-data-atom-alias (atom)
  (let ((x (assoc 'alias atom)))
    (if x (cdr x) nil)))

(defun brain-data-atom-shortcut (atom)
  (let ((x (assoc 'shortcut atom)))
    (if x (cdr x) nil)))

(defun brain-data-atom-meta (atom)
  (let ((x (assoc 'meta atom)))
    (if x (cdr x) nil)))

(defun brain-data-atom (id)
  (if id
      (let ((atoms (get-atoms-by-id)))
        (if atoms (gethash id atoms) nil))
    nil))

(defun brain-data-target ()
  (brain-data-atom (brain-data-atom-id-at-point)))

(defun brain-data-target-value ()
  (let ((g (brain-data-target)))
    (if g (brain-data-atom-value g))))

(defun brain-data-target-alias ()
  (let ((g (brain-data-target)))
    (if g (brain-data-atom-alias g))))

(defun brain-data-target-sharability ()
  (let ((g (brain-data-target)))
    (if g (brain-data-atom-sharability g))))

(defun brain-data-show (atom)
  (let (
        (created (brain-data-atom-created atom))
        (value (brain-data-atom-value atom))
        (weight (brain-data-atom-weight atom))
        (sharability (brain-data-atom-sharability atom))
        (priority (brain-data-atom-priority atom))
        (alias (brain-data-atom-alias atom))
        (meta (brain-data-atom-meta atom)))
    ;;(type (get-atom-type atom)))
    (brain-env-info-message (concat
              ;;(if type (concat "type: " type ", "))
              (if meta (concat "[meta], "))
              "weight: " (number-to-string weight)
              ", sharability: " (number-to-string sharability)
              (if priority (concat ", priority: " (number-to-string priority)) "")
              ", created: " (format-time-string "%Y-%m-%dT%H:%M:%S%z" (seconds-to-time (/ created 1000.0)))
              ", value: " value
              (if alias (concat ", alias: " alias) "")))))


(provide 'brain-data)
