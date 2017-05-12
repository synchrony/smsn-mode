;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; smsn-view.el -- Tree views and buffers
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


(defun get-display-color (source-name)
  (let ((source (smsn-env-get-source-by-name source-name)))
    (let ((color (smsn-env-json-get 'displayColor source)))
      (if color (to-color-triple color) (error (concat "no display color for source '" source-name "'"))))))

(defun find-normal-color (source weight is-link)
  (let ((base (get-display-color source)))
    (let ((link-or-text-color
      (if is-link base (darken base 0.4))))
      (fade-color-triple link-or-text-color weight))))

(defun to-color-triple (numeric-color)
  (let ((blue (% numeric-color 256))
        (green (% (truncate (/ numeric-color 256)) 256))
        (red (truncate (/ numeric-color 65536))))
    (list red green blue)))

(defvar smsn-view-full-colors-supported (> (length (defined-colors)) 8))

(defun color-part-red (color)
  (string-to-number (substring color 1 3) 16))

(defun color-part-green (color)
  (string-to-number (substring color 3 5) 16))

(defun color-part-blue (color)
  (string-to-number (substring color 5 7) 16))

(defun color-from-string (str)
  (list (color-part-red str) (color-part-green str) (color-part-blue str)))

(defun color-triple-to-string (color-triple)
  (let ((red (car color-triple))
        (green (car (cdr color-triple)))
	    (blue (car (cdr (cdr color-triple)))))
    (concat "#" (format "%02X" red) (format "%02X" green) (format "%02X" blue))))

(defun darken (color-triple factor)
  (mapcar (lambda (n)
    (truncate (* factor n))) color-triple))

(defun weighted-average (a b weight)
  (+ (* a (- 1 weight)) (* b weight)))

(defun fade (color weight)
  (let ((low (weighted-average color 255 0.9375))
        (high color))
    (weighted-average low high weight)))

(defun fade-color-triple (color-triple weight)
  (mapcar (lambda (n)
    (fade n weight)) color-triple))

(defun atom-color (weight source is-link)
  (color-triple-to-string (find-normal-color source weight is-link)))

(defun colorize (text weight source priority-bg priority-fg bright has-meta)
  (let ((color (if smsn-view-full-colors-supported
                   (atom-color weight source bright)
                 "black")))
    (setq l (list
             :foreground color
             :underline (if (and priority-fg (> priority-fg 0))
                            (list :color (atom-color priority-fg source bright)) nil)
             :box (if priority-bg
               (list :color (atom-color priority-bg source bright)) nil)))
    (propertize text 'face l)))

(defun light-gray ()
  (if smsn-view-full-colors-supported
    (list :foreground "gray" :background "white")
    (list :foreground "black")))

(defun dark-gray ()
  (if smsn-view-full-colors-supported
    (list :foreground "dim gray" :background "white")
    (list :foreground "black")))

(defun orange-background ()
  (if smsn-view-full-colors-supported
    (list :background "orange")
    (list :background "gray")))

(defun purple-background ()
  (if smsn-view-full-colors-supported
    (list :background "purple")
    (list :background "gray")))

(defun yellow-background ()
  (if smsn-view-full-colors-supported
    (list :background "yellow")
    (list :background "gray")))

(defun red-background ()
  (if smsn-view-full-colors-supported
    (list :background "red")
    (list :background "gray")))

(defun make-background-orange (text)
  (propertize text 'face (orange-background)))

(defun make-background-purple (text)
  (propertize text 'face (purple-background)))

(defun make-background-yellow (text)
  (propertize text 'face (yellow-background)))

(defun make-background-red (text)
  (propertize text 'face (red-background)))

(defun make-light-gray (text)
  (propertize text 'face (light-gray)))

(defun make-dark-gray (text)
  (propertize text 'face (dark-gray)))

(defun delimit-value (value)
  "Encloses multi-line values in triple brackets, leaving single-line values unchanged"
  (let ((s (string-match "\n" value)))
    (if s (let ((content (concat "\n" value "\n")))
            (concat "{{{"
                    (if (smsn-env-context-get 'minimize-verbatim-blocks) (propertize content 'invisible t) content)
                    "}}}")) value)))

(defun choose-bullet (n-children)
  (if (> n-children 0) "+" "\u00b7"))

(defun pad-to-length-2 (n)
  (if (> n 99) "++"
    (let ((s (number-to-string n)))
      (if (> (length s) 1) s (concat " " s)))))

(defun add-meta-columns (text n-children n-parents has-page)
  ;; yellow for parents > 1. blank for parents|children = 0. purple child field for markup, unless there are children too, in which case red.
  (let ((parent-text (if (> n-parents 0) (pad-to-length-2 n-parents) "  "))
        (child-text (if (> n-children 0) (pad-to-length-2 n-children) "  ")))
    (let ((parent-string (if (> n-parents 1)
			     (make-background-yellow parent-text) parent-text))
	  (child-string (if has-page
			    (make-background-purple child-text)
			    child-text)))
      (let ((meta (concat parent-string " "
			  (if (and (> n-children 0) has-page)
			      (make-background-red child-string)
			      child-string))))
	(propertize text 'display `((margin right-margin),meta))))))

(defun write-treeview (children tree-indent)
  (loop for json across children do
        (let (
              (link (smsn-env-json-get 'link json))
              (children (smsn-env-json-get 'children json)))
          (let ((focus-id (smsn-data-atom-id json))
                (focus-created (smsn-env-json-get 'created json))
                (focus-title (let ((v (smsn-data-atom-title json))) (if v v "")))
                (focus-has-page (smsn-env-json-get 'page json))
		        (focus-weight (smsn-data-atom-weight json))
		        (focus-source (smsn-data-atom-source json))
		        (focus-priority (smsn-data-atom-priority json))
                (focus-has-children (not (equal json-false (smsn-env-json-get 'hasChildren json))))
                (focus-n-children (smsn-env-json-get 'numberOfChildren json))
                (focus-n-parents (smsn-env-json-get 'numberOfParents json))
		        (focus-alias (smsn-data-atom-alias json))
		        (focus-shortcut (smsn-data-atom-shortcut json))
		        (focus-meta (smsn-data-atom-meta json)))
            (if focus-id
              (puthash focus-id json (smsn-env-context-get 'atoms-by-id))
              (error "missing focus id"))
            (setq space "")
            (loop for i from 1 to tree-indent do (setq space (concat space " ")))
            (let ((line "") (id-infix
                (add-meta-columns (smsn-view-create-id-infix focus-id)
                  focus-n-children focus-n-parents focus-has-page)))
              (setq line (concat line space))
              (let ((bullet (choose-bullet focus-n-children)))
                (setq line (concat line
                   (make-read-only (concat
                      (colorize bullet
                        focus-weight focus-source focus-priority nil focus-alias focus-meta)
                      id-infix
                      " "))
                   (colorize (delimit-value focus-title)
                             focus-weight focus-source nil focus-priority focus-alias focus-meta)
                   "\n")))
              (insert (propertize line 'id focus-id)))
            (if (smsn-env-using-inference)
                (loop for a across focus-meta do (insert (make-light-gray (concat space "    @{" a "}\n")))))
            (if (smsn-env-context-get 'view-properties)
              (let ()
                (make-read-only
                  (insert-line-for-property "@created" (number-to-string focus-created)))
                (insert-line-for-property "@source" focus-source)
                (insert-line-for-property "@weight" (number-to-string focus-weight))
                (if focus-priority
                  (insert-line-for-property "@priority" (number-to-string focus-priority)))
                (if focus-shortcut
                  (insert-line-for-property "@shortcut" focus-shortcut))
                (if focus-alias
                  (insert-line-for-property "@alias" focus-alias))))
            (write-treeview children (+ tree-indent 4))))))

(defun make-read-only (text)
  text ;; TODO: any way to make the text read-only unless you delete the entire line?
  ;;(propertize text 'read-only t)
  )

(defun insert-line-for-property (name value)
  (insert (make-light-gray
    (concat space "    " name " " value "\n"))))

(defun write-wikiview (json)
  (let ((page (let ((v (smsn-data-atom-page json))) (if v v "")))
    (weight (smsn-data-atom-weight json))
    (source (smsn-data-atom-source json)))
      (insert (colorize page weight source nil 0.0 nil nil))))

(defun num-or-nil-to-string (n)
  (if n (number-to-string n) "nil"))

(defun view-info ()
  (concat
   "(root: " (smsn-env-context-get 'root-id)
   " :height " (num-or-nil-to-string (smsn-env-context-get 'height))
   " :style " (smsn-env-context-get 'style)
   " :sharability
             " (num-or-nil-to-string (smsn-env-context-get 'min-sharability))
   " :weight
             [" (num-or-nil-to-string (smsn-env-context-get 'min-weight))
   ", " (num-or-nil-to-string (smsn-env-context-get 'default-weight)) "]"
   " :title \"" (smsn-env-context-get 'title) "\")")) ;; TODO: actually escape the title string

(defun shorten-title (str maxlen)
  (if (> (length str) maxlen)
    (concat (substring str 0 maxlen) "...")
    str))

(defun name-for-view-buffer (root-id payload is-treeview)
  (let ((title (smsn-env-json-get 'title payload)))
    (if root-id
      (concat (shorten-title title 20) " [" root-id "]" (if is-treeview " - tree" ""))
      title)))
      
(defun prepare-right-margin ()
  (set-window-margins (frame-selected-window) 0 5))

(defun switch-to-buffer-with-context (name context)
  "activate smsn-mode in a new view buffer created by smsn-mode"
  (switch-to-buffer name)
  (setq buffer-read-only nil)
  (smsn-mode)
  (prepare-right-margin)
  (smsn-env-set-context context))

(defun read-string-value (context-variable payload-variable payload)
  (let ((value (smsn-env-json-get payload-variable payload)))
    (if value (smsn-env-context-set context-variable value))))

(defun read-numeric-value (context-variable payload-variable payload)
  (let ((value (smsn-env-json-get payload-variable payload)))
    (if value (smsn-env-context-set context-variable (string-to-number value)))))

(defun configure-context (payload)
  "Sets variables of the buffer-local context according to a service response"
  (let ((view (smsn-data-payload-view payload)))
    (read-string-value 'root-id 'root payload)
    (read-string-value 'style 'style payload)
    (read-string-value 'title 'title payload)
    (read-numeric-value 'height 'height payload)
    (read-numeric-value 'min-sharability 'minSharability payload)
    (read-numeric-value 'min-weight 'minWeight payload)
    (read-numeric-value 'default-weight 'defaultWeight payload)))

(defun smsn-view-set-context-line (&optional line)
  (smsn-env-context-set 'line
    (if line line (line-number-at-pos))))

;; Try to move to the corresponding line in the previous view.
;; This is not always possible and not always helpful, but it is often both.
(defun move-to-context-line ()
  (let ((line (smsn-env-context-get 'line)))
    (if line
      (beginning-of-line line)
      (error "no line number"))))

(defun create-atom-hashtable ()
  (smsn-env-context-set 'atoms-by-id (make-hash-table :test 'equal)))

;; always include line numbers in views
(defun show-line-numbers ()
  (linum-mode t))

(defun is-readonly ()
  (or (smsn-env-is-readonly)
    (smsn-env-in-search-mode)))

(defun configure-buffer ()
  (if (not (smsn-env-context-get 'truncate-long-lines)) (toggle-truncate-lines))
  (beginning-of-buffer)
  (setq visible-cursor t)
  (move-to-context-line)
  (setq buffer-read-only (is-readonly))
  (show-line-numbers))

(defun write-treeview-to-buffer (payload)
  (write-treeview (smsn-env-json-get 'children (smsn-data-payload-view payload)) 0))

(defun write-wikiview-to-buffer (payload)
  (write-wikiview (smsn-data-payload-view payload)))

(defun smsn-treeview-open (payload context)
  "Callback to receive and display the data of a view"
  (switch-to-buffer-with-context
     (name-for-view-buffer (smsn-data-atom-id (smsn-data-payload-view payload)) payload t) context)
  (configure-context payload)
  (create-atom-hashtable)
  (erase-buffer)
  (write-treeview-to-buffer payload)
  (configure-buffer)
  (message "view updated in %.0f ms" (smsn-env-response-time)))

(defun smsn-wikiview-open (payload context)
  "Callback to receive and display the page of an atom"
  (switch-to-buffer-with-context
     (name-for-view-buffer (smsn-data-atom-id (smsn-data-payload-view payload)) payload nil) context)
  (configure-context payload)
  (erase-buffer)
  (write-wikiview-to-buffer payload)
  (configure-buffer)
  (message "page updated in %.0f ms" (smsn-env-response-time)))

(defun smsn-view-create-id-infix (id)
  "Creates a string of the form :0000000:, where 000000 is the id of an atom"
  (propertize (concat " :" id ":") 'invisible t))


(provide 'smsn-view)
