;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; brain-env.el -- Constants, state, and buffer-local variables
;;
;; Part of the Brain-mode package for Emacs:
;;   https://github.com/synchrony/brain-mode
;;
;; Copyright (C) 2011-2017 Joshua Shinavier and collaborators
;;
;; You should have received a copy of the GNU General Public License
;; along with this software.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defconst brain-const-max-height 7
  "The maximum allowed height of a view")

(defconst brain-const-treeview-mode "treeview-mode"
  "A state for viewing and editing tree views of graph data")
(defconst brain-const-wikiview-mode "wikiview-mode"
  "A state for viewing and editing the page of an atom")
(defconst brain-const-search-mode "search-mode"
  "A state for immutable search results")

(defconst brain-const-color-by-sharability "sharability"
  "A color scheme based on atom weight and sharability")
(defconst brain-const-color-by-class-inference "inference"
  "A color scheme based on type inference")

(defconst brain-const-forward-style "forward"
  "A view style in which atom children are arranged below parents in a tree")
(defconst brain-const-backward-style "backward"
  "A view style in which atom parents are arranged below children in a tree")

(defconst brain-const-sharability-private 0.25
  "A sharability level for the most sensitive information")
(defconst brain-const-sharability-personal 0.5
  "A sharability level for personal information which may be shared in certain contexts")
(defconst brain-const-sharability-public 0.75
  "A sharability level for information which can be freely shared")
(defconst brain-const-sharability-universal 1.0
  "A sharability level for extra-personal information known to everyone")
(defconst brain-const-weight-none 0.0
  "A weight for atoms of no importance, used as a lower limit")
(defconst brain-const-weight-deemphasized 0.25
  "A weight for less important atoms")
(defconst brain-const-weight-default 0.5
  "A weight for typical atoms")
(defconst brain-const-weight-emphasized 0.75
  "A weight for atoms of special importance")
(defconst brain-const-weight-all 1.0
  "A weight for the most important atoms")

(defun brain-env-get-context (&optional context)
  "Retrieves Brain-mode's buffer-local context"
  (if context context brain-bufferlocal-context))

(defun brain-env-set-context (context)
  "Sets Brain-mode's buffer-local context"
  (setq brain-bufferlocal-context context)
  (make-local-variable 'brain-bufferlocal-context))

(defun brain-env-define-buffer-local-variables ()
  "Populates a first definition of buffer-local context with default values"
  (defvar brain-bufferlocal-context (default-context)))

(defun brain-env-error-no-focus ()
  "Informs the user that a focus atom was not found"
  (error "there is no atom associated with this line"))

(defun brain-env-error-no-root ()
  "Informs the user that a root atom was not found"
  (error "there is no root atom associated with this view"))

(defun brain-env-using-inference ()
  "Determines whether the current buffer is an inference-enabled view"
  (equal (brain-env-context-get 'view-style) brain-const-color-by-class-inference))

(defun brain-env-succeed ()
  "Confirms an assertion"
  t)

(defun brain-env-fail (message)
  "Fails an assertion with the given message"
  (and (error message) nil))

(defun brain-env-is-readonly ()
  "Determines whether the current buffer is a read-only tree view"
  (not (equal (brain-env-context-get 'readonly) 'nil)))

(defun brain-env-in-treeview-mode ()
  "Determines whether the current buffer is a tree view"
  (equal (brain-env-context-get 'mode) brain-const-treeview-mode))

(defun brain-env-in-wikiview-mode ()
  "Determines whether the current buffer is a wiki view"
  (equal (brain-env-context-get 'mode) brain-const-wikiview-mode))

(defun brain-env-in-search-mode ()
  "Determines whether the current buffer is a page of search results"
  (equal (brain-env-context-get 'mode) brain-const-search-mode))

(defun brain-env-to-treeview-mode (&optional context)
  (brain-env-context-set 'mode brain-const-treeview-mode context))

(defun brain-env-to-wikiview-mode (&optional context)
  (brain-env-context-set 'mode brain-const-wikiview-mode context))

(defun brain-env-to-search-mode (&optional context)
  (brain-env-context-set 'mode brain-const-search-mode context))

(defun brain-env-set-readonly (readonly &optional context)
  "Switches to a read-only tree view"
  (brain-env-context-set 'readonly (if readonly t 'nil) context))

(defun brain-env-context-set-forward-style (&optional context)
  "Switches to the default, parent-to-child style of tree view"
  (brain-env-context-set 'style brain-const-forward-style context))

(defun brain-env-context-set-backward-style (&optional context)
  "Switches to the inverse (child-to-parent) style of tree view"
  (brain-env-context-set 'style brain-const-backward-style context))

(defun brain-env-in-setproperties-mode ()
  "Determines whether atom properties can be set in the current buffer"
  (or
     (brain-env-in-search-mode)
     (brain-env-in-treeview-mode)))

(defun brain-env-assert-height-in-bounds (height)
  "Asserts that a view height is positive and not greater than the maximum allowed height"
  (if (< height 1)
    (brain-env-fail (concat "height of " (number-to-string height) " is too low (must be >= 1)"))
    (if (> height brain-const-max-height)
      (brain-env-fail (concat "height of " (number-to-string height) " is too high (must be <= "
                                 (number-to-string brain-const-max-height) ")"))
      (brain-env-succeed))))

(defun brain-env-toggle-inference-viewstyle (&optional context)
  "Switches from the 'weight/sharability' view style to the 'type inference' style, or vice versa"
  (brain-env-context-set 'view-style
    (if (equal (brain-env-context-get 'view-style) brain-const-color-by-sharability)
      brain-const-color-by-class-inference
      brain-const-color-by-sharability) context))

(defun brain-env-format-date (time)
  "Formats the current date for use in a buffer of notes"
  (format-time-string const-date-format time))

(defun brain-env-format-time (time &optional with-seconds)
  "Formats the current time for use in a buffer of notes"
  (format-time-string
    (if with-seconds
        const-time-with-seconds-format
        const-time-format)
    time))

(defun brain-env-set-timestamp ()
  "Sets a context-local time stamp to the current time"
  (brain-env-context-set 'timestamp (current-time)))

(defun brain-env-response-time ()
  "Finds the difference, in milliseconds, between the current time and the last recorded time stamp"
  (* 1000.0 (float-time (time-subtract
    (current-time)
    (brain-env-context-get 'timestamp)))))

(defun brain-env-context-get (key &optional context)
  (brain-env-json-get key (brain-env-get-context context)))

(defun brain-env-context-set (key value &optional context)
  (setcdr (assoc key (brain-env-get-context context))
          (if value value 'nil)))

(defun brain-env-json-get (key json)
  (if json
    (cdr (assoc key json))
    nil))

(defconst const-date-format "%Y-%m-%d")
(defconst const-time-format "%H:%M")
(defconst const-time-with-seconds-format "%H:%M:%S")

(defun default-context () (list
  (cons 'action 'nil)
  (cons 'atoms-by-id 'nil)
  (cons 'default-sharability brain-const-sharability-personal)
  (cons 'default-weight brain-const-weight-default)
  (cons 'file 'nil)
  (cons 'format 'nil)
  (cons 'height 2)
  (cons 'line 1)
  (cons 'min-sharability brain-const-sharability-private)
  (cons 'min-weight brain-const-weight-none)
  (cons 'minimize-verbatim-blocks 'nil)
  (cons 'mode brain-const-search-mode)
  (cons 'query 'nil)
  (cons 'query-type 'nil)
  (cons 'root-id 'nil)
  (cons 'readonly 'nil)
  (cons 'style brain-const-forward-style)
  (cons 'timestamp 'nil)
  (cons 'title 'nil)
  (cons 'page 'nil)
  (cons 'truncate-long-lines 'nil)
  (cons 'title-length-cutoff 100)
  (cons 'view 'nil)
  (cons 'view-properties 'nil)
  (cons 'view-style brain-const-color-by-sharability)))

;; change the default sharability in the new view after a user visits a link or atom
;; The default will never be greater than 0.75 unless explicitly set by the user.
(defun adjust-default-sharability (sharability)
  (if sharability
      (if (<= sharability 0.75) sharability 0.75)
    0.5))


(provide 'brain-env)
