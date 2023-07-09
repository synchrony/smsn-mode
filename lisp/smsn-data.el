;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; smsn-data.el -- Data model and data accessors
;;
;; Part of the SmSn-mode package for Emacs:
;;   https://github.com/synchrony/smsn-mode
;;
;; Copyright (C) 2011-2017 Joshua Shinavier and collaborators
;;
;; You should have received a copy of the GNU General Public License
;; along with this software.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'smsn-env)


(defun current-line ()
  (interactive)
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun smsn-data-payload-view (payload)
  (smsn-env-json-get 'view payload))

(defun smsn-data-root-id ()
  (smsn-env-context-get 'root-id))

(defun smsn-data-page-title ()
  (smsn-env-context-get 'title))

(defun smsn-data-note-id-at-point ()
  (let ((line (current-line)))
    (if (string-match "^[0-9A-Za-z@&]*: " line)
        (let ((i3 (string-match ": " line)))
          (let ((s2 (substring line 0 i3)))
            (if (< 0 (length s2)) s2 nil)))
      (get-text-property (line-beginning-position) 'id))))

(defun smsn-data-note-id (note)
  (smsn-env-json-get 'id note))

(defun smsn-data-note-created (note)
  (smsn-env-json-get 'created note))

(defun smsn-data-note-title (note)
  (smsn-env-json-get 'title note))

(defun smsn-data-note-text (note)
  (smsn-env-json-get 'text note))

(defun smsn-data-note-priority (note)
  (smsn-env-json-get 'priority note))

(defun smsn-data-note-source (note)
  (smsn-env-json-get 'source note (smsn-env-context-get 'default-source)))

(defun smsn-data-note-weight (note)
  (smsn-env-json-get 'weight note (smsn-env-context-get 'default-weight)))

(defun smsn-data-note-alias (note)
  (smsn-env-json-get 'alias note))

(defun smsn-data-note-shortcut (note)
  (smsn-env-json-get 'shortcut note))

(defun smsn-data-note-meta (note)
  (smsn-env-json-get 'meta note))

(defun smsn-data-note (id)
  (if id
      (let ((notes (smsn-env-context-get 'notes-by-id)))
        (if notes (gethash id notes) nil))
    nil))

(defun smsn-data-focus ()
  (smsn-data-note (smsn-data-note-id-at-point)))

(defun smsn-data-focus-title ()
  (let ((g (smsn-data-focus)))
    (if g (smsn-data-note-title g))))

(defun smsn-data-focus-alias ()
  (let ((g (smsn-data-focus)))
    (if g (smsn-data-note-alias g))))

(defun smsn-data-focus-source ()
  (let ((g (smsn-data-focus)))
    (if g (smsn-data-note-source g))))

(defun smsn-data-show (note)
  (let (
        (created (smsn-data-note-created note))
        (title (smsn-data-note-title note))
        (weight (smsn-data-note-weight note))
        (source (smsn-data-note-source note))
        (priority (smsn-data-note-priority note))
        (alias (smsn-data-note-alias note))
        (meta (smsn-data-note-meta note)))
    ;;(type (smsn-env-context-get 'note-type note)))
    (message "%s" (concat
              ;;(if type (concat "type: " type ", "))
              (if meta (concat "[meta], "))
              "weight: " (number-to-string weight)
              ", source: " source
              (if priority (concat ", priority: " (number-to-string priority)) "")
              ", created: " (format-time-string "%Y-%m-%dT%H:%M:%S%z" (seconds-to-time (/ created 1000.0)))
              ", title: " title
              (if alias (concat ", alias: " alias) "")))))


(provide 'smsn-data)
