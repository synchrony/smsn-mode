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

(defun smsn-data-atom-id-at-point ()
  (let ((line (current-line)))
    (if (string-match "^[0-9A-Za-z@&]*: " line)
        (let ((i3 (string-match ": " line)))
          (let ((s2 (substring line 0 i3)))
            (if (< 0 (length s2)) s2 nil)))
      (get-text-property (line-beginning-position) 'id))))

(defun smsn-data-atom-id (atom)
  (smsn-env-json-get 'id atom))

(defun smsn-data-atom-created (atom)
  (smsn-env-json-get 'created atom))

(defun smsn-data-atom-title (atom)
  (smsn-env-json-get 'title atom))

(defun smsn-data-atom-text (atom)
  (smsn-env-json-get 'text atom))

(defun smsn-data-atom-priority (atom)
  (smsn-env-json-get 'priority atom))

(defun smsn-data-atom-source (atom)
  (smsn-env-json-get 'source atom (smsn-env-context-get 'default-source)))

(defun smsn-data-atom-weight (atom)
  (smsn-env-json-get 'weight atom (smsn-env-context-get 'default-weight)))

(defun smsn-data-atom-alias (atom)
  (smsn-env-json-get 'alias atom))

(defun smsn-data-atom-shortcut (atom)
  (smsn-env-json-get 'shortcut atom))

(defun smsn-data-atom-meta (atom)
  (smsn-env-json-get 'meta atom))

(defun smsn-data-atom (id)
  (if id
      (let ((atoms (smsn-env-context-get 'atoms-by-id)))
        (if atoms (gethash id atoms) nil))
    nil))

(defun smsn-data-focus ()
  (smsn-data-atom (smsn-data-atom-id-at-point)))

(defun smsn-data-focus-title ()
  (let ((g (smsn-data-focus)))
    (if g (smsn-data-atom-title g))))

(defun smsn-data-focus-alias ()
  (let ((g (smsn-data-focus)))
    (if g (smsn-data-atom-alias g))))

(defun smsn-data-focus-source ()
  (let ((g (smsn-data-focus)))
    (if g (smsn-data-atom-source g))))

(defun smsn-data-show (atom)
  (let (
        (created (smsn-data-atom-created atom))
        (title (smsn-data-atom-title atom))
        (weight (smsn-data-atom-weight atom))
        (source (smsn-data-atom-source atom))
        (priority (smsn-data-atom-priority atom))
        (alias (smsn-data-atom-alias atom))
        (meta (smsn-data-atom-meta atom)))
    ;;(type (smsn-env-context-get 'atom-type atom)))
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
