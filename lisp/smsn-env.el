;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; smsn-env.el -- Constants, state, and buffer-local variables
;;
;; Part of the SmSn-mode package for Emacs:
;;   https://github.com/synchrony/smsn-mode
;;
;; Copyright (C) 2011-2017 Joshua Shinavier and collaborators
;;
;; You should have received a copy of the GNU General Public License
;; along with this software.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defconst smsn-const-max-height 7
  "The maximum allowed height of a view")

(defconst smsn-const-treeview-mode "treeview-mode"
  "A state for viewing and editing tree views of graph data")
(defconst smsn-const-wikiview-mode "wikiview-mode"
  "A state for viewing and editing the page of an atom")
(defconst smsn-const-search-mode "search-mode"
  "A state for immutable search results")

(defconst smsn-const-color-by-sharability "sharability"
  "A color scheme based on atom weight and sharability")
(defconst smsn-const-color-by-class-inference "inference"
  "A color scheme based on type inference")

(defconst smsn-const-forward-style "forward"
  "A view style in which atom children are arranged below parents in a tree")
(defconst smsn-const-backward-style "backward"
  "A view style in which atom parents are arranged below children in a tree")

(defconst smsn-const-sharability-private 0.25
  "A sharability level for the most sensitive information")
(defconst smsn-const-sharability-personal 0.5
  "A sharability level for personal information which may be shared in certain contexts")
(defconst smsn-const-sharability-public 0.75
  "A sharability level for information which can be freely shared")
(defconst smsn-const-sharability-universal 1.0
  "A sharability level for extra-personal information known to everyone")
(defconst smsn-const-weight-none 0.0
  "A lower limit of atom weight")
(defconst smsn-const-weight-weak 0.25
  "A weight for less important atoms")
(defconst smsn-const-weight-default 0.5
  "A weight for typical atoms")
(defconst smsn-const-weight-strong 0.75
  "A weight for atoms of special importance")
(defconst smsn-const-weight-full 1.0
  "A weight for the most important atoms")

(defun smsn-env-get-context (&optional context)
  "Retrieves smsn-mode's buffer-local context"
  (if context context smsn-bufferlocal-context))

(defun smsn-env-set-context (context)
  "Sets smsn-mode's buffer-local context"
  (setq smsn-bufferlocal-context context)
  (make-local-variable 'smsn-bufferlocal-context))

(defun smsn-env-define-buffer-local-variables ()
  "Populates a first definition of buffer-local context with default values"
  (defvar smsn-bufferlocal-context (default-context)))

(defun smsn-env-error-no-focus ()
  "Informs the user that a focus atom was not found"
  (error "there is no atom associated with this line"))

(defun smsn-env-error-no-root ()
  "Informs the user that a root atom was not found"
  (error "there is no root atom associated with this view"))

(defun smsn-env-using-inference ()
  "Determines whether the current buffer is an inference-enabled view"
  (equal (smsn-env-context-get 'view-style) smsn-const-color-by-class-inference))

(defun smsn-env-succeed ()
  "Confirms an assertion"
  t)

(defun smsn-env-fail (message)
  "Fails an assertion with the given message"
  (and (error message) nil))

(defun smsn-env-is-readonly ()
  "Determines whether the current buffer is a read-only tree view"
  (not (equal (smsn-env-context-get 'readonly) 'nil)))

(defun smsn-env-in-treeview-mode ()
  "Determines whether the current buffer is a tree view"
  (equal (smsn-env-context-get 'mode) smsn-const-treeview-mode))

(defun smsn-env-in-wikiview-mode ()
  "Determines whether the current buffer is a wiki view"
  (equal (smsn-env-context-get 'mode) smsn-const-wikiview-mode))

(defun smsn-env-in-search-mode ()
  "Determines whether the current buffer is a page of search results"
  (equal (smsn-env-context-get 'mode) smsn-const-search-mode))

(defun smsn-env-to-treeview-mode (&optional context)
  (smsn-env-context-set 'mode smsn-const-treeview-mode context))

(defun smsn-env-to-wikiview-mode (&optional context)
  (smsn-env-context-set 'mode smsn-const-wikiview-mode context))

(defun smsn-env-to-search-mode (&optional context)
  (smsn-env-context-set 'mode smsn-const-search-mode context))

(defun smsn-env-set-readonly (readonly &optional context)
  "Switches to a read-only tree view"
  (smsn-env-context-set 'readonly (if readonly t 'nil) context))

(defun smsn-env-context-set-forward-style (&optional context)
  "Switches to the default, parent-to-child style of tree view"
  (smsn-env-context-set 'style smsn-const-forward-style context))

(defun smsn-env-context-set-backward-style (&optional context)
  "Switches to the inverse (child-to-parent) style of tree view"
  (smsn-env-context-set 'style smsn-const-backward-style context))

(defun smsn-env-in-setproperties-mode ()
  "Determines whether atom properties can be set in the current buffer"
  (or
     (smsn-env-in-search-mode)
     (smsn-env-in-treeview-mode)))

(defun smsn-env-assert-height-in-bounds (height)
  "Asserts that a view height is positive and not greater than the maximum allowed height"
  (if (< height 1)
    (smsn-env-fail (concat "height of " (number-to-string height) " is too low (must be >= 1)"))
    (if (> height smsn-const-max-height)
      (smsn-env-fail (concat "height of " (number-to-string height) " is too high (must be <= "
                                 (number-to-string smsn-const-max-height) ")"))
      (smsn-env-succeed))))

(defun smsn-env-toggle-inference-viewstyle (&optional context)
  "Switches from the 'weight/sharability' view style to the 'type inference' style, or vice versa"
  (smsn-env-context-set 'view-style
    (if (equal (smsn-env-context-get 'view-style) smsn-const-color-by-sharability)
      smsn-const-color-by-class-inference
      smsn-const-color-by-sharability) context))

(defun smsn-env-format-date (time)
  "Formats the current date for use in a buffer of notes"
  (format-time-string const-date-format time))

(defun smsn-env-format-time (time &optional with-seconds)
  "Formats the current time for use in a buffer of notes"
  (format-time-string
    (if with-seconds
        const-time-with-seconds-format
        const-time-format)
    time))

(defun smsn-env-set-timestamp ()
  "Sets a context-local time stamp to the current time"
  (smsn-env-context-set 'timestamp (current-time)))

(defun smsn-env-response-time ()
  "Finds the difference, in milliseconds, between the current time and the last recorded time stamp"
  (* 1000.0 (float-time (time-subtract
    (current-time)
    (smsn-env-context-get 'timestamp)))))

(defun smsn-env-context-get (key &optional context)
  (smsn-env-json-get key (smsn-env-get-context context)))

(defun smsn-env-context-set (key value &optional context)
  (setcdr (assoc key (smsn-env-get-context context))
          (if value value 'nil)))

(defun smsn-env-json-get (key json)
  (if json
    (cdr (assoc key json))
    nil))

(defconst const-date-format "%Y-%m-%d")
(defconst const-time-format "%H:%M")
(defconst const-time-with-seconds-format "%H:%M:%S")

(defun default-context () (list
  (cons 'action 'nil)
  (cons 'atoms-by-id 'nil)
  (cons 'default-sharability smsn-const-sharability-personal)
  (cons 'default-weight smsn-const-weight-default)
  (cons 'file 'nil)
  (cons 'format 'nil)
  (cons 'height 2)
  (cons 'line 1)
  (cons 'min-sharability smsn-const-sharability-personal)
  (cons 'min-weight smsn-const-weight-none)
  (cons 'minimize-verbatim-blocks 'nil)
  (cons 'mode smsn-const-search-mode)
  (cons 'query 'nil)
  (cons 'query-type 'nil)
  (cons 'root-id 'nil)
  (cons 'readonly 'nil)
  (cons 'style smsn-const-forward-style)
  (cons 'timestamp 'nil)
  (cons 'title 'nil)
  (cons 'page 'nil)
  (cons 'truncate-long-lines 'nil)
  (cons 'title-length-cutoff 100)
  (cons 'view 'nil)
  (cons 'view-properties 'nil)
  (cons 'view-style smsn-const-color-by-sharability)))

;; change the default sharability in the new view after a user visits a link or atom
;; The default will never be greater than 0.75 unless explicitly set by the user.
(defun adjust-default-sharability (sharability)
  (if sharability
      (if (<= sharability 0.75) sharability 0.75)
    0.5))


(provide 'smsn-env)
