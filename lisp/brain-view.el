;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; brain-view.el -- Tree views and buffers
;;
;; Part of the Brain-mode package for Emacs:
;;   https://github.com/synchrony/brain-mode
;;
;; Copyright (C) 2011-2017 Joshua Shinavier and collaborators
;;
;; You should have received a copy of the GNU General Public License
;; along with this software.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'brain-env)


;; unused colors: black/gray, orange
(defconst sharability-base-colors  '("#660000" "#604000" "#005000" "#000066"))
(defconst sharability-bright-colors  '("#D00000" "#D0B000" "#00B000" "#0000D0"))
(defconst sharability-reduced-colors '("red" "red" "blue" "blue"))
(defconst inference-base-colors '("#660066" "#006666"))
(defconst inference-bright-colors '("#FF00FF" "#00FFFF"))

(defvar brain-view-full-colors-supported (> (length (defined-colors)) 8))

(defun color-part-red (color)
  (string-to-number (substring color 1 3) 16))

(defun color-part-green (color)
  (string-to-number (substring color 3 5) 16))

(defun color-part-blue (color)
  (string-to-number (substring color 5 7) 16))

(defun color-string (red green blue)
  (concat "#" (format "%02X" red) (format "%02X" green) (format "%02X" blue)))

(defun weighted-average (a b weight)
  (+ (* a (- 1 weight)) (* b weight)))

(defun fade-color (color weight)
  (let ((low (weighted-average color 255 0.9375))
        (high color))
    (weighted-average low high weight)))

(defun atom-color (weight sharability bright has-meta)
  (let ((s
         (if (brain-env-using-inference)
             (elt (if bright inference-bright-colors inference-base-colors) (if has-meta 0 1))
           (elt (if bright sharability-bright-colors sharability-base-colors) (- (ceiling (* sharability 4)) 1)))))
    (color-string
     (fade-color (color-part-red s) weight)
     (fade-color (color-part-green s) weight)
     (fade-color (color-part-blue s) weight))))

(defun colorize (text weight sharability priority-bg priority-fg bright has-meta)
  (let ((color (if brain-view-full-colors-supported
                   (atom-color weight sharability bright has-meta)
                 (elt sharability-reduced-colors (- (ceiling (* sharability 4)) 1)))))
    (setq l (list
             :foreground color
             ;;:weight 'bold
             :underline (if (and priority-fg (> priority-fg 0))
                            (list :color (atom-color priority-fg sharability bright has-meta)) nil)
             :box (if priority-bg (list
                                   :color (atom-color priority-bg sharability bright has-meta)) nil)))
    (propertize text 'face l)))

(defun light-gray ()
  (if brain-view-full-colors-supported
    (list :foreground "gray" :background "white")
    (list :foreground "black")))

(defun dark-gray ()
  (if brain-view-full-colors-supported
    (list :foreground "dim gray" :background "white")
    (list :foreground "black")))

(defun orange-background ()
  (if brain-view-full-colors-supported
    (list :background "orange")
    (list :background "gray")))

(defun purple-background ()
  (if brain-view-full-colors-supported
    (list :background "purple")
    (list :background "gray")))

(defun yellow-background ()
  (if brain-view-full-colors-supported
    (list :background "yellow")
    (list :background "gray")))

(defun red-background ()
  (if brain-view-full-colors-supported
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
                    (if (brain-env-context-get 'minimize-verbatim-blocks) (propertize content 'invisible t) content)
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
              (link (brain-env-json-get 'link json))
              (children (brain-env-json-get 'children json)))
          (let ((focus-id (brain-data-atom-id json))
                (focus-title (let ((v (brain-data-atom-title json))) (if v v "")))
                (focus-has-page (brain-env-json-get 'page json))
		        (focus-weight (brain-data-atom-weight json))
		        (focus-sharability (brain-data-atom-sharability json))
		        (focus-priority (brain-data-atom-priority json))
                (focus-has-children (not (equal json-false (brain-env-json-get 'hasChildren json))))
                (focus-n-children (brain-env-json-get 'numberOfChildren json))
                (focus-n-parents (brain-env-json-get 'numberOfParents json))
		        (focus-alias (brain-data-atom-alias json))
		        (focus-shortcut (brain-data-atom-shortcut json))
		        (focus-meta (brain-data-atom-meta json)))
            (if focus-id
              (puthash focus-id json (brain-env-context-get 'atoms-by-id))
              (error "missing focus id"))
            (setq space "")
            (loop for i from 1 to tree-indent do (setq space (concat space " ")))
            (let ((line "") (id-infix
                (add-meta-columns (brain-view-create-id-infix focus-id) focus-n-children focus-n-parents focus-has-page)))
              (setq line (concat line space))
              (let ((bullet (choose-bullet focus-n-children)))
                (setq line (concat line
                                   (colorize bullet
                                             focus-weight focus-sharability focus-priority nil focus-alias focus-meta)
                                   id-infix
                                   " "
                                   (colorize (delimit-value focus-title)
                                             focus-weight focus-sharability nil focus-priority focus-alias focus-meta)
                                   "\n")))
              (insert (propertize line 'id focus-id)))
            (if (brain-env-using-inference)
                (loop for a across focus-meta do (insert (make-light-gray (concat space "    @{" a "}\n")))))
            (if (brain-env-context-get 'view-properties)
              (let ()
                (insert (make-light-gray
                         (concat space "    @sharability " (number-to-string focus-sharability) "\n")))
                (insert (make-light-gray
                         (concat space "    @weight      " (number-to-string focus-weight) "\n")))
                (if focus-priority
                    (insert (make-light-gray (concat space "    @priority    " (number-to-string focus-priority) "\n"))))
                (if focus-shortcut
                    (insert (make-light-gray (concat space "    @shortcut    " focus-shortcut "\n"))))
                (if focus-alias
                    (insert (make-light-gray (concat space "    @alias       " focus-alias "\n"))))))
            (write-treeview children (+ tree-indent 4))))))

(defun write-wikiview (json)
  (let ((page (let ((v (brain-data-atom-page json))) (if v v "")))
    (weight (brain-data-atom-weight json))
    (sharability (brain-data-atom-sharability json)))
      (insert (colorize page weight sharability nil 0.0 nil nil))))

(defun num-or-nil-to-string (n)
  (if n (number-to-string n) "nil"))

(defun view-info ()
  (concat
   "(root: " (brain-env-context-get 'root-id)
   " :height " (num-or-nil-to-string (brain-env-context-get 'height))
   " :style " (brain-env-context-get 'style)
   " :sharability
             [" (num-or-nil-to-string (brain-env-context-get 'min-sharability))
   ", " (num-or-nil-to-string (brain-env-context-get 'default-sharability)) "]"
   " :weight
             [" (num-or-nil-to-string (brain-env-context-get 'min-weight))
   ", " (num-or-nil-to-string (brain-env-context-get 'default-weight)) "]"
   " :title \"" (brain-env-context-get 'title) "\")")) ;; TODO: actually escape the title string

(defun shorten-title (str maxlen)
  (if (> (length str) maxlen)
    (concat (substring str 0 maxlen) "...")
    str))

(defun name-for-view-buffer (root-id payload is-treeview)
  (let ((title (brain-env-json-get 'title payload)))
    (if root-id
      (concat (shorten-title title 20) " [" root-id "]" (if is-treeview " - tree" ""))
      title)))
      
(defun prepare-right-margin ()
  (set-window-margins (frame-selected-window) 0 5))

(defun switch-to-buffer-with-context (name context)
  "activate Brain-mode in a new view buffer created by Brain-mode"
  (switch-to-buffer name)
  (setq buffer-read-only nil)
  (brain-mode)
  (prepare-right-margin)
  (brain-env-set-context context))

(defun read-string-value (context-variable payload-variable payload)
  (let ((value (brain-env-json-get payload-variable payload)))
    (if value (brain-env-context-set context-variable value))))

(defun read-numeric-value (context-variable payload-variable payload)
  (let ((value (brain-env-json-get payload-variable payload)))
    (if value (brain-env-context-set context-variable (string-to-number value)))))

(defun find-default-sharability (root-sharability)
  (min brain-const-sharability-public root-sharability))

(defun configure-context (payload)
  "Sets variables of the buffer-local context according to a service response"
  (let ((view (brain-data-payload-view payload)))
    (read-string-value 'root-id 'root payload)
    (read-string-value 'style 'style payload)
    (read-string-value 'title 'title payload)
    (read-numeric-value 'height 'height payload)
    (read-numeric-value 'min-sharability 'minSharability payload)
    (brain-env-context-set 'default-sharability (find-default-sharability (brain-data-atom-sharability view)))
    ;;(read-numeric-value 'default-sharability 'defaultSharability payload)
    (read-numeric-value 'min-weight 'minWeight payload)
    (read-numeric-value 'default-weight 'defaultWeight payload)))

(defun brain-view-set-context-line (&optional line)
  (brain-env-context-set 'line
    (if line line (line-number-at-pos))))

;; Try to move to the corresponding line in the previous view.
;; This is not always possible and not always helpful, but it is often both.
(defun move-to-context-line ()
  (let ((line (brain-env-context-get 'line)))
    (if line
      (beginning-of-line line)
      (error "no line number"))))

(defun create-atom-hashtable ()
  (brain-env-context-set 'atoms-by-id (make-hash-table :test 'equal)))

;; always include line numbers in views
(defun show-line-numbers ()
  (linum-mode t))

(defun is-readonly ()
  (or (brain-env-is-readonly)
    (brain-env-in-search-mode)))

(defun configure-buffer ()
  (if (not (brain-env-context-get 'truncate-long-lines)) (toggle-truncate-lines))
  (beginning-of-buffer)
  (setq visible-cursor t)
  (move-to-context-line)
  (setq buffer-read-only (is-readonly))
  (show-line-numbers))

(defun write-treeview-to-buffer (payload)
  (write-treeview (brain-env-json-get 'children (brain-data-payload-view payload)) 0))

(defun write-wikiview-to-buffer (payload)
  (write-wikiview (brain-data-payload-view payload)))

(defun brain-treeview-open (payload context)
  "Callback to receive and display the data of a view"
  (switch-to-buffer-with-context
     (name-for-view-buffer (brain-data-atom-id (brain-data-payload-view payload)) payload t) context)
  (configure-context payload)
  (create-atom-hashtable)
  (erase-buffer)
  (write-treeview-to-buffer payload)
  (configure-buffer)
  (message "view updated in %.0f ms" (brain-env-response-time)))

(defun brain-wikiview-open (payload context)
  "Callback to receive and display the page of an atom"
  (switch-to-buffer-with-context
     (name-for-view-buffer (brain-data-atom-id (brain-data-payload-view payload)) payload nil) context)
  (configure-context payload)
  (erase-buffer)
  (write-wikiview-to-buffer payload)
  (configure-buffer)
  (message "page updated in %.0f ms" (brain-env-response-time)))

(defun brain-view-create-id-infix (id)
  "Creates a string of the form :0000000:, where 000000 is the id of an atom"
  (propertize (concat " :" id ":") 'invisible t))


(provide 'brain-view)
