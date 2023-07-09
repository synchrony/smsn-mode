;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; smsn-commands.el -- Top-level commands and key bindings
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
(require 'smsn-data)
(require 'smsn-view)
(require 'smsn-client)


(defconst fast-numbers
  '(
    (?0 0) (?1 1) (?2 2) (?3 3) (?4 4) (?5 5) (?6 6) (?7 7) (?8 8) (?9 9)
    (?z 0) (?a 1) (?s 2) (?d 3) (?f 4) (?g 5) (?h 6) (?j 7) (?k 8) (?l 9) (?\; 10)))

(defconst line-addr-keypairs
 (list
  '(?0 ?0) '(?1 ?1) '(?2 ?2) '(?3 ?3) '(?4 ?4) '(?5 ?5) '(?6 ?6) '(?7 ?7) '(?8 ?8) '(?9 ?9)
  '(?\; ?0) '(?a ?1) '(?s ?2) '(?d ?3) '(?f ?4) '(?g ?5) '(?h ?6) '(?j ?7) '(?k ?8) '(?l ?9)
  '(?u ?1) '(?i ?2) '(?o ?3) '(?p ?4)))

(defvar line-addr-keymap (make-hash-table))

(dolist (pair line-addr-keypairs)
  (puthash (car pair) (car (cdr pair)) line-addr-keymap))

(defun assert-readwrite-context ()
  "Asserts that the current smsn-mode buffer is in a writable state"
  (if (smsn-env-is-readonly)
    (smsn-env-fail (concat "cannot update view in current mode: " (smsn-env-context-get 'mode))) nil)
    (smsn-env-succeed))

;; from Emacs-w3m w3m-url-encode-string
(defun url-encode (str &optional coding)
  (apply (function concat)
         (mapcar (lambda (ch)
                   (cond
                    ((string-match "[-a-zA-Z0-9_:/]" (char-to-string ch)) ; xxx?
                     (char-to-string ch)) ; printable
                    (t
                     (format "%%%02X" ch)))) ; escape
                 ;; Coerce a string to a list of chars.
                 (append (encode-coding-string str (or coding 'utf-8))
                         nil))))

(defun smsn-note-info (selector)
  "display, in the minibuffer, information about an note produced by SELECTOR"
  (lexical-let ((as selector))
    (lambda () (interactive)
      (let ((note (funcall as)))
        (if note
            (smsn-data-show note)
          (smsn-env-error-no-focus))))))

;; note: the id doesn't stay invisible when you paste it, although it stays light gray
(defun smsn-copy-focus-reference-to-clipboard ()
  "copy a reference to the note at point to the system clipboard"
  (interactive)
  (let ((id (smsn-data-note-id-at-point)))
    (if id
        (copy-to-clipboard (concat "*" (smsn-view-create-id-infix id)))
      (smsn-env-error-no-focus))))

(defun smsn-copy-focus-title-to-clipboard ()
  "copy the title of the note at point to the system clipboard"
  (interactive)
  (let ((title (smsn-data-focus-title)))
    (if title
      (copy-to-clipboard title)
      (smsn-env-error-no-focus))))

(defun smsn-copy-root-reference-to-clipboard ()
  "copy a reference to the note at point to the system clipboard"
  (interactive)
  (let ((id (smsn-data-root-id)))
    (if id
        (copy-to-clipboard (concat "*" (smsn-view-create-id-infix id)))
      (smsn-env-error-no-root))))

(defun smsn-debug ()
  "executes a debug action (by default a no-op)"
  (interactive)
  (message (concat "current mode: " (smsn-env-context-get 'mode))))

(defun smsn-duplicates ()
  "retrieve a list of notes with duplicate titles"
  (interactive)
  (smsn-client-fetch-duplicates))

(defun smsn-enter-readwrite-view ()
  "enter edit (read/write) mode in the current view"
  (interactive)
  (if (and (smsn-env-in-treeview-mode) (smsn-env-is-readonly)) (progn
    (smsn-env-set-readonly nil)
    (smsn-client-refresh-treeview))))

(defun smsn-enter-readonly-view ()
  "enter read-only mode in the current view"
  (interactive)
  (if (and (smsn-env-in-treeview-mode) (not (smsn-env-is-readonly))) (progn
    (smsn-env-set-readonly t)
    (smsn-client-refresh-treeview))))

(defun smsn-events ()
  "retrieve the Semantic Synchrony event stack (e.g. notifications of gestural events), ordered by decreasing time stamp"
  (interactive)
  (smsn-client-fetch-events 2))

(defun smsn-write-vcs ()
  "export graph as version-controlled directory"
  (interactive)
  (message "%s" (concat "exporting VCS dump to configured locations"))
  (smsn-client-write-graph "VCS"))

(defun smsn-write-yaml ()
  "export graph as in the YAML format"
  (interactive)
  (message "%s" (concat "exporting YAML dump to configured locations"))
  (smsn-client-write-graph "YAML"))

(defun smsn-export-edges (file)
  "export tab-separated dump of Semantic Synchrony parent-child edges to the file system"
  (interactive)
  (message "%s" (concat "exporting edges to " file))
  (smsn-client-write-graph "Edges" file))

(defun smsn-export-graphml (file)
  "export a GraphML dump of the knowledge base to the file system"
  (interactive)
  (message "%s" (concat "exporting GraphML to " file))
  (smsn-client-write-graph "GraphML" file))

(defun smsn-export-latex (file)
  "export a LaTeX-formatted view of a subtree of the knowledge base to the file system"
  (interactive)
  (message "%s" (concat "exporting LaTeX to " file))
  (smsn-client-write-graph "LaTeX" file))

(defun smsn-export-pagerank (file)
  "export a tab-separated PageRank ranking of Semantic Synchrony notes to the file system"
  (interactive)
  (message "%s" (concat "computing and exporting PageRank to " file))
  (smsn-client-write-graph "PageRank" file))

(defun smsn-export-rdf (file)
  "export an RDF dump of the knowledge base to the file system"
  (interactive)
  (message "%s" (concat "exporting private N-Triples dump to " file))
  (smsn-client-write-graph "N-Triples" file))

(defun smsn-export-vertices (file)
  "export tab-separated dump of Semantic Synchrony vertices (notes) to the file system"
  (interactive)
  (message "%s" (concat "exporting vertices to " file))
  (smsn-client-write-graph "Vertices" file))

(defun smsn-read-vcs ()
  "import a graph from configured data sources into the knowledge graph"
  (interactive)
  (message "importing graph from configured sources")
  (smsn-client-read-graph "VCS"))

(defun smsn-read-yaml ()
  "import a graph from configured data sources into the knowledge graph"
  (interactive)
  (message "importing graph from configured sources")
  (smsn-client-read-graph "YAML"))

(defun smsn-view-log (file)
  "create a view of Git history"
  (interactive)
  (message "%s" (concat "reading logs from " file))
  (smsn-client-view-log file))

(defun smsn-import-freeplane (file)
  "import one or more Freeplane files into the knowledge base"
  (interactive)
  (message "%s" (concat "importing Freeplane nodes from " file))
  (smsn-client-read-graph "Freeplane" file))

(defun smsn-import-graphml (file)
  "import a GraphML dump from the file system into the knowledge base"
  (interactive)
  (message "%s" (concat "importing GraphML from " file))
  (smsn-client-read-graph "GraphML" file))

(defun smsn-find-isolated-notes ()
  "retrieve a list of isolated notes (i.e. notes with neither parents nor children) in the knowledge base"
  (interactive)
  (smsn-client-fetch-find-isolated-notes))

(defun smsn-remove-isolated-notes ()
  "remove all isolated notes (i.e. notes with neither parents nor children) from the knowledge base"
  (interactive)
  (smsn-client-fetch-remove-isolated-notes))

(defun smsn-find-roots ()
  "retrieve a list of roots (i.e. notes with no parents) in the knowledge base"
  (interactive)
  (smsn-client-find-roots))

(defun smsn-history ()
  "retrieve a list of the most recently viewed or updated notes, in decreasing order of recency"
  (interactive)
  (smsn-client-fetch-history))

(defun smsn-infer-types ()
  "perform type inference on the Semantic Synchrony knowledge base, adding type annotations"
  (interactive)
  (message "performing type inference")
  (smsn-client-infer-types))

(defun smsn-insert-attr-priority (expr)
  "insert a line to set the priority of a note to the value given by EXPR"
  (interactive)
  (let ((n (number-shorthand-to-number expr)))
    (if n (insert (concat "\n                @priority " (number-to-string (/ n 4.0)) "\n")))))

(defun smsn-insert-attr-source (char)
  "insert a line to set the source of a note to the value given by CODE"
  (interactive)
  (if (smsn-env-in-treeview-mode)
    (let ((code (char-to-string char)))
      (insert (concat "\n                @source " (smsn-env-json-get 'name (smsn-env-get-source-by-code code)))))))

(defun smsn-insert-attr-weight (expr)
  "insert a line to set the weight of a note to the value given by EXPR"
  (interactive)
  (let ((n (number-shorthand-to-number expr)))
    (if n (insert (concat "\n                @weight " (number-to-string (/ n 4.0)) "\n")))))

(defun smsn-insert-current-date ()
  "insert the current date, in the format yyyy-mm-dd, into the current buffer"
  (interactive)
  (insert (smsn-env-format-date (current-time))))

(defun smsn-insert-current-time ()
  "insert the current time, in the format hh:mm, into the current buffer"
  (interactive)
  (insert (smsn-env-format-time (current-time) nil)))

(defun smsn-insert-current-time-with-seconds ()
  "insert the current time with seconds, in the format hh:mm:ss, into the current buffer"
  (interactive)
  (insert (smsn-env-format-time (current-time) t)))

(defun smsn-kill-other-buffers ()
  "Kill all other smsn-mode buffers."
  (interactive)
  (mapc 'kill-buffer
	(my-filter
	 (lambda (bname)
	   (eq (buffer-local-value 'major-mode (get-buffer bname))
	       'smsn-mode))
	 (delq (current-buffer) (buffer-list))
	 )))

(defun smsn-action-dujour ()
  "calls an action which may vary over time; used in development and migration"
  (interactive)
  (smsn-client-action-dujour))

(defun smsn-ping-server ()
  "finds the response time of the server connection to a simple request"
  (interactive)
  (smsn-client-ping-server))

(defun smsn-preview-focus-latex-math ()
  "create a graphical preview of the title of the note at point, which must be a LaTeX mathematical expression"
  (interactive)
  (end-of-line)
  (backward-word)
  (latex-math-preview-expression))

(defun smsn-priorities ()
  "retrieve a list of notes with nonzero priority values, ordered by decreasing priority"
  (interactive)
  (smsn-client-fetch-priorities))

(defun smsn-push-view ()
  "push an up-to-date view into the knowledge base"
  (interactive)
  (if (assert-readwrite-context)
    (if (smsn-env-in-wikiview-mode) (smsn-client-push-wikiview)
      (if (smsn-env-in-treeview-mode) (smsn-client-push-treeview)))))

(defun smsn-ripple-query (query)
  "evaluate Ripple expression QUERY"
  (interactive)
  (if (> (length query) 0)
      (smsn-client-fetch-ripple-results query)))

(defun do-query-by-partial-title ()
  (if (boundp 'smsn-const-query-by-partial-title) smsn-const-query-by-partial-title nil))

(defun to-partial-title-query (query)
  (mapconcat 'identity
    (mapcar
      (lambda (s) (concat "*" s "*"))
      (split-string query)) " "))

(defun adjust-title-query (query)
  (if (do-query-by-partial-title)
    (to-partial-title-query query)
    query))

(defun smsn-title-query (query)
  "evaluate full-text query for QUERY, yielding a ranked list of query results in a new buffer"
  (interactive)
  (if (> (length query) 0)
      (smsn-client-fetch-query (adjust-title-query query) "FullText")))

(defun smsn-acronym-query (query)
  "evaluate acronym (abbreviated title) query for QUERY, yielding a ranked list of query results in a new buffer"
  (interactive)
  (if (> (length query) 0)
    (smsn-client-fetch-query query "Acronym")))

(defun smsn-shortcut-query (query)
  "evaluate shortcut query for QUERY, yielding query results (normally zero or one) in a new buffer"
  (interactive)
  (if (> (length query) 0)
    (smsn-client-fetch-query query "Shortcut")))

(defun smsn-query-on-focus-title ()
  "evaluate a full-text query for the title of the note at point"
  (interactive)
  (let ((title (smsn-data-focus-title)))
    (if title (smsn-title-query title))))

(defun smsn-set-min-source (char)
  "set the minimum source (for notes visible in the current view) to the data source represented by CHAR"
  (interactive)
  (if (smsn-env-in-treeview-mode)
    (let ((source (smsn-env-get-source-by-code (char-to-string char))))
      (if source (smsn-client-set-min-source (smsn-env-json-get 'name source))))))

(defun smsn-set-min-weight (expr)
  "set the minimum @weight (for notes visible in the current view) to the number represented by EXPR"
  (interactive)
  (if (smsn-env-in-treeview-mode)
    (let ((n (number-shorthand-to-number expr)))
      (if n (smsn-client-set-min-weight (/ n 4.0))))))

(defun smsn-set-focus-priority (expr)
  "set the @priority of the note at point to the number represented by EXPR"
  (interactive)
  (if (smsn-env-in-treeview-mode)
    (let ((n (number-shorthand-to-number expr)))
      (if n (smsn-client-set-focus-priority (/ n 4.0))))))

(defun smsn-set-focus-source (char)
  "set the @source of the note at point to the number represented by CHAR"
  (interactive)
  (if (smsn-env-in-treeview-mode)
    (let ((code (char-to-string char)))
      (smsn-client-set-focus-source
         (smsn-env-json-get 'name (smsn-env-get-source-by-code code))))))

(defun smsn-set-focus-weight (expr)
  "set the @weight of the note at point to the number represented by EXPR"
  (interactive)
  (if (smsn-env-in-treeview-mode)
    (let ((n (number-shorthand-to-number expr)))
      (if n (smsn-client-set-focus-weight (/ n 4.0))))))

(defun smsn-set-title-truncation-length (length-str)
  "set the title truncation length to the number represented by LENGTH-STR.
Longer title are truncated, for efficiency and readability, when they appear in views.
A value of -1 indicates that titles should not be truncated."
  (interactive)
  (let ((n (string-to-number length-str)))
    (smsn-env-context-set 'title-length-cutoff n)))

(defun smsn-set-view-height (expr)
  "set the height of the current view to the number of levels represented by EXPR"
  (interactive)
  (if (smsn-env-in-treeview-mode)
    (let ((height (number-shorthand-to-number expr)))
      (if (smsn-env-assert-height-in-bounds  height)
        (progn
          (smsn-env-context-set 'height height)
          (smsn-client-refresh-treeview))))))

(defun smsn-toggle-emacspeak ()
  "turn Emacspeak on or off"
  (interactive)
  (dtk-toggle-quiet))

(defun smsn-toggle-inference-viewstyle ()
  "toggle between the source view style and the type inference view style.
In the source view style, colors are assigned to notes based on the data source of each note
(for example, private notes are red, while public notes are green).
However, in the type inference view style, a note is either cyan or magenta depending on whether
a type has been assigned to it by the inference engine."
  (interactive)
  (if (smsn-env-in-treeview-mode) (progn
    (smsn-env-toggle-inference-viewstyle)
    (smsn-update-view)
    (message (concat "switched to " (smsn-env-context-get 'view-style) " view style")))))

(defun smsn-toggle-properties-view ()
  "enable or disable the explicit display of note properties as extra lines within views"
  (interactive)
  (if (smsn-env-in-treeview-mode) (progn
    (smsn-env-context-set 'view-properties (not (smsn-env-context-get 'view-properties)))
    (smsn-update-view)
    (message (concat (if (smsn-env-context-get 'view-properties) "enabled" "disabled") " property view")))))

(defun smsn-toggle-truncate-lines ()
  "toggle line wrap mode"
  (interactive)
  (smsn-env-context-set 'truncate-long-lines (not (smsn-env-context-get 'truncate-long-lines)))
  (toggle-truncate-lines))

(defun smsn-update-to-backward-view ()
  "switch to a 'backward' view, i.e. a view in which a note's parents appear as list items beneath it"
  (interactive)
  (if (smsn-env-in-treeview-mode) (progn
      (smsn-env-context-set-backward-style)
      (smsn-client-refresh-treeview))))

(defun smsn-update-to-forward-view ()
  "switch to a 'forward' view (the default), i.e. a view in which a note's children appear as list items beneath it"
  (interactive)
  (if (smsn-env-in-treeview-mode) (progn
      (smsn-env-context-set-forward-style)
      (smsn-client-refresh-treeview))))

(defun smsn-update-view ()
  "refresh the current view from the data store"
  (interactive)
  (if (smsn-env-in-wikiview-mode) (smsn-client-refresh-wikiview)
    (if (smsn-env-in-treeview-mode) (smsn-client-refresh-treeview))))

(defun smsn-web-search (selector service-name base-url)
  "search a particular service for the value generated by VALUE-SELECTOR and view the results in a browser"
  (visit-focus-title selector
    (lexical-let ((base-url base-url))
      (lambda (value)
        (concat base-url (url-encode value))))))

(defun smsn-web-search-on-focus-title (service-name base-url)
  (smsn-web-search 'smsn-data-focus-title service-name base-url))

(defun smsn-web-search-on-page-title (service-name base-url)
  (smsn-web-search 'smsn-data-page-title service-name base-url))

(defun smsn-open-new-note ()
  "create a new note and opening a new (empty) view of the note"
  (interactive)
  (smsn-client-open-note "create-new-note"))

(defun smsn-open-focus-note ()
  "opening a view of the note at point"
  (interactive)
  (let ((id (smsn-data-note-id-at-point)))
    (if id
      (smsn-client-open-note id)
      (smsn-env-error-no-focus))))

(defun smsn-focus-wikiview ()
  "open a note's text for editing in a separate buffer"
  (interactive)
  (let ((id (smsn-data-note-id-at-point)))
    (if id
      (smsn-client-wikiview id)
      (smsn-env-error-no-focus))))

(defun smsn-navigate-to-focus-alias ()
  "visit the @alias of the note at point (normally a URL) in a browser"
  (interactive)
  (let ((alias (smsn-data-focus-alias)))
    (if alias
        (browse-url alias)
      (smsn-env-error-no-focus))))

(defun smsn-visit-as-url (value-selector)
  "visit the URL generated by VALUE-SELECTOR in a browser"
  (interactive)
  (visit-focus-title value-selector (lambda (value) value)))

(defun smsn-visit-url-at-point ()
  "visit the URL at point in a browser"
  (interactive)
  (goto-address-at-point))

(defun smsn-use-move-submode ()
  (dolist (m smsn-move-submode-map)
    (let ((key (car m)) (symbol (cdr m)))
     (mode-define-key key symbol))))

(defun smsn-use-edit-submode ()
  (dolist (m smsn-move-submode-map)
    (let ((key (car m)) (symbol (cdr m)))
     (mode-define-key key 'nil))))

(defvar smsn-move-submode nil)
(defun smsn-toggle-move-or-edit-submode ()
  (interactive)
  (if smsn-move-submode
    (progn (setq smsn-move-submode 'nil)
           (smsn-use-edit-submode)
	   (message "submode: edit")
	   (set-cursor-color "black")
	   )
    (progn (setq smsn-move-submode t)
           (smsn-use-move-submode)
	   (message "submode: move")
	   (set-cursor-color "green")
	   )
    )
  )

(defun smsn-insert-attr-priority-prompt ()
  (interactive)
  (prompt-for-char 'smsn-insert-attr-priority "priority = ?"))

(defun smsn-insert-attr-source-prompt ()
  (interactive)
  (prompt-for-char 'smsn-insert-attr-source "source = ?"))

(defun smsn-insert-attr-weight-prompt ()
  (interactive)
  (prompt-for-char 'smsn-insert-attr-weight "weight = ?"))

(defun smsn-set-view-height-prompt ()
  (interactive)
  (prompt-for-char 'smsn-set-view-height "height = ?"))

(defun smsn-write-vcs-prompt ()
  (interactive)
  (prompt-for-confirmation 'smsn-write-vcs "export graph to VCS"))

(defun smsn-write-yaml-prompt ()
  (interactive)
  (prompt-for-confirmation 'smsn-write-yaml "export graph to YAML"))

(defun get-default (var &optional default)
  (if (boundp var) (symbol-value var) default))

(defun smsn-export-edges-prompt ()
  (interactive)
  (prompt-for-string 'smsn-export-edges "export edges to file: "
    (get-default 'smsn-default-edges-file)))

(defun smsn-export-graphml-prompt ()
  (interactive)
  (prompt-for-string 'smsn-export-graphml "export GraphML to file: "
    (get-default 'smsn-default-graphml-file)))

(defun smsn-export-latex-prompt ()
  (interactive)
  (prompt-for-string 'smsn-export-latex "export LaTeX to file: "
    (get-default 'smsn-default-latex-file)))

(defun smsn-export-pagerank-prompt ()
  (interactive)
  (prompt-for-string 'smsn-export-pagerank "export PageRank results to file: "
    (get-default 'smsn-default-pagerank-file)))

(defun smsn-export-rdf-prompt ()
  (interactive)
  (prompt-for-string 'smsn-export-rdf "export N-Triples dump to file: "
    (get-default 'smsn-default-rdf-file)))

(defun smsn-export-vertices-prompt ()
  (interactive)
  (prompt-for-string 'smsn-export-vertices "export vertices to file: "
    (get-default 'smsn-default-vertices-file)))

(defun smsn-read-vcs-prompt ()
  (interactive)
  (prompt-for-confirmation 'smsn-read-vcs "import graph from VCS"))

(defun smsn-read-yaml-prompt ()
  (interactive)
  (prompt-for-confirmation 'smsn-read-yaml "import graph from YAML"))

(defun smsn-view-log-prompt ()
  (interactive)
  (prompt-for-string
    'smsn-view-log
    "VCS directory: " (get-default 'smsn-default-vcs-file "~/")))

(defun smsn-import-freeplane-prompt ()
  (interactive)
  (prompt-for-string 'smsn-import-freeplane "import Freeplane data from file/directory: "
    (get-default 'smsn-default-freeplane-file)))

(defun smsn-import-graphml-prompt ()
  (interactive)
  (prompt-for-string 'smsn-import-graphml "import GraphML from file: "
    (get-default 'smsn-default-graphml-file)))

(defun smsn-set-min-source-prompt ()
  (interactive)
  (prompt-for-char 'smsn-set-min-source "minimum source = ?"))

(defun smsn-set-focus-priority-prompt ()
  (interactive)
  (prompt-for-char 'smsn-set-focus-priority "new priority = ?"))

(defun smsn-set-focus-source-prompt ()
  (interactive)
  (prompt-for-char 'smsn-set-focus-source "new source = ?"))

(defun smsn-set-focus-weight-prompt ()
  (interactive)
  (prompt-for-char 'smsn-set-focus-weight "new weight = ?"))

(defun smsn-set-title-truncation-length-prompt ()
  (interactive)
  (prompt-for-string 'smsn-set-title-truncation-length "title truncation length: "))

(defun smsn-set-min-weight-prompt ()
  (interactive)
  (prompt-for-char 'smsn-set-min-weight "minimum weight = ?"))

(defun smsn-acronym-query-prompt ()
  (interactive)
  (prompt-for-string 'smsn-acronym-query "acronym search for: "))

(defun smsn-shortcut-query-prompt ()
  (interactive)
  (prompt-for-string 'smsn-shortcut-query "shortcut search for: "))

(defun smsn-ripple-query-prompt ()
  (interactive)
  (prompt-for-string 'smsn-ripple-query "ripple query: "))

(defun smsn-title-query-prompt ()
  (interactive)
  (prompt-for-string 'smsn-title-query "title search for: "))

(defun smsn-open-note-prompt ()
  (interactive)
  (prompt-for-string 'smsn-client-open-note "open view for note with id: "))

(defun smsn-push-view-prompt ()
  (interactive)
  (if (eq (read-char "really push view? (press 'z' to confirm)") 122)
      (smsn-push-view)
      nil))

(defun smsn-update-view-prompt ()
  (interactive)
  (if (eq (read-char "really update view? (press 'm' to confirm)") 109)
      (smsn-update-view)
      nil))

(defun visit-focus-title (value-selector value-to-url)
  (lexical-let ((vs value-selector) (vu value-to-url))
    (lambda () (interactive)
      (let ((value (funcall vs)))
        (if value
            (browse-url (funcall vu value))
          (smsn-env-error-no-focus))))))

(defun number-shorthand-to-number (c)
  (interactive)
  (let ((l (assoc c fast-numbers)))
    (if l
       (car (cdr l))
       (error (concat "no number associated with character " (char-to-string c))))))

;; note: works in Aquamacs and MacPorts Emacs, but apparently not in the terminal Emacs 24 on Mac OS X
 (defun copy-to-clipboard (g)
  (let ((buffer (get-buffer-create "*temp*")))
    (with-current-buffer buffer
      (unwind-protect
          (insert g)
        (let ((beg 1) (end (+ (length g) 1)))
          (clipboard-kill-ring-save beg end))
        (kill-buffer buffer)))))

(defun mapkey (c)
  (gethash c line-addr-keymap))

(defun address-to-lineno (address)
  (if (string-match "[0-9asdfghjkl;]+" address)
      (string-to-number (coerce (mapcar 'mapkey (coerce address 'list)) 'string))
    nil))

(defun handle-changewindow (address)
  (let ((c (car (coerce address 'list))))
    (if (string-match "[uiop]" (string c))
        (let ((n (string-to-number (string (gethash c line-addr-keymap)))))
          (other-window n)
          (coerce (cdr (coerce address 'list)) 'string))
      address)))

(defun color-at-min-source ()
  "Returns the color for at note at the minimum visible source"
  "dim gray")

(defun color-prompt-by-min-source (callback)
  (let ((newcol (color-at-min-source))
        (oldcol (face-foreground 'minibuffer-prompt)))
    (set-face-foreground 'minibuffer-prompt newcol)
    (funcall callback)
    (set-face-foreground 'minibuffer-prompt oldcol)))

(defun prompt-for-string (function prompt &optional initial)
  (color-prompt-by-min-source (lambda ()
    ;; note: use of the INITIAL argument is discouraged, but here it makes sense
    (let ((arg (read-from-minibuffer prompt initial)))
      (if arg (funcall function arg))))))

(defun prompt-for-char (function prompt)
  (color-prompt-by-min-source (lambda ()
    (let ((c (read-char prompt)))
      (if c (funcall function c))))))

(defun prompt-for-confirmation (function prompt)
  (prompt-for-string
    (lexical-let ((function function))
      (lambda (arg)
        (if (and (> (length arg) 0) (equal "y" (downcase (substring arg 0 1))))
          (funcall function))))
    (concat prompt " (y/n)? ")))

;; "edit"(default) and "move" submodes
    ;; some editing (esp. cut|paste and of properties) is still
    ;; possible in move-mode, but some keys do not print to screen

(if (boundp 'smsn-move-submode-map) ()
  (defconst smsn-move-submode-map '(
    (";" . smsn-toggle-truncate-lines)
    ("b" . smsn-update-to-backward-view)
    ("B" . smsn-bury-line)
    ("c" . kill-ring-save)
    ("d" . smsn-insert-delete-instruction)
    ("f" . smsn-update-to-forward-view)
    ("F" . smsn-float-line)
    ("g" . smsn-update-view-prompt) ;; keyboard shortcut is effectively "g m"
    ("h" . smsn-set-view-height-prompt) ;; "h 3" unfolds the tree to depth 3
    ("i" . previous-line)  ;; up
    ("I" . scroll-down-command)
    ("j" . backward-char)  ;; left
    ("J" . move-beginning-of-line)
    ("k" . next-line)  ;; down
    ("K" . scroll-up-command)
    ("l" . forward-char)  ;; right
    ("L" . move-end-of-line)
    ("m" . smsn-set-minimal-font)
    ("n" . smsn-open-focus-note-and-kill-buffer)
    ("o" . smsn-open-focus-note-in-other-window)
    ("O" . other-window)
    ("p" . smsn-set-priority-and-drop-cursor) ;; !! first use one-liner-view
    ("q" . kill-buffer)
    ("s" . smsn-set-source-and-drop-cursor) ;; !! first use one-liner-view
    ("t" . smsn-open-focus-note)
    ("u" . undo)
    ("U" . smsn-navigate-to-focus-alias) ;; u as in url
    ("v" . yank)
    ("w" . smsn-set-weight-and-drop-cursor) ;; !! first use one-liner-view
    ("W" . smsn-focus-wikiview)
    ("y" . smsn-push-view-prompt) ;; shortcut is effectively "y z"
    ("z" . set-mark-command)
)))

(defun smsn-insert-delete-instruction ()
  "This marks a node for deletion, by prefixing its title with [delete - x], where `x` can be chosen from a menu of four common boilerplate values. Escape the choice with `C-g` to write a custom description)."
  (interactive)
  (move-beginning-of-line 1)
  (forward-char 21)
  (insert "[delete - ] ")
  (backward-char 2)
  (let* ((choices '("done" "merged" "redundant"
		    "premature" "stale" "useless"))
	 (choice (ido-completing-read "Why?" choices )))
    (insert choice)
    ))

(defun smsn-bury-line ()
  "Drop line to bottom of buffer. Dangerous if the line is at depth > 1."
  (interactive)
  (move-beginning-of-line nil)
  (let ((id (smsn-data-note-id-at-point))
        (line (count-lines 1 (point)))) ;; 1, 2, 10 have same effect, but 0 errs
    (kill-line)
    (end-of-buffer)
    (open-line 1) ;; insert one newline after point
    (yank)
    (goto-line (+ 1 line)) ;; why we have to add 1, I don't know
    (message "note %s buried; change not saved" id))
  )

(defun smsn-float-line ()
  "For comments, see float-line."
  (interactive)
  (move-beginning-of-line nil)
  (let ((id (smsn-data-note-id-at-point))
        (line (count-lines 1 (point))))
    (kill-line)
    (beginning-of-buffer)
    (open-line 1)
    (yank)
    (goto-line (+ line 2))
    (message "note %s floated; change not saved" id))
  )

(defun smsn-open-focus-note-in-other-window ()
  "in another window, open a view of the note at point"
  (interactive)
  (let ((id (smsn-data-note-id-at-point)))
    (progn
      (if (eq (count-windows) 1)
	  (split-window-below)
        nil)
      (other-window 1)
      (if id
	  (smsn-client-open-note id)
	(smsn-env-error-no-focus))
      )))

(defun smsn-set-minimal-font ()
  (interactive)
  (progn
    (move-end-of-line 1)
    (insert "\n  @priority 0\n  @weight 0.25\n")
    (kill-line)
    (previous-line)
    (previous-line)
    (previous-line) ;; return cursor to the start of that node, because presumably the user is about to delete it
    ))

(defun smsn-set-priority-and-drop-cursor ()
  (interactive)
  (progn
    (move-end-of-line 1)
    (smsn-insert-attr-priority-prompt)
    (kill-line)
    (backward-char)
    ))

(defun smsn-set-source-and-drop-cursor ()
  (interactive)
  (progn
    (move-end-of-line 1)
    (smsn-insert-attr-source-prompt)
    (kill-line)
    (open-line 1)
    ))

(defun smsn-set-weight-and-drop-cursor ()
  (interactive)
  (progn
    (move-end-of-line 1)
    (smsn-insert-attr-weight-prompt)
    (kill-line)
    (backward-char)
    ))

(defun smsn-open-focus-note-and-kill-buffer ()
  (interactive)
  (let ((name (buffer-name)))
    (progn (smsn-open-focus-note)
	   (kill-buffer name))))

(defun my-filter (condp lst)
  ;; https://www.emacswiki.org/emacs/ElispCookbook
  (delq nil
	(mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun mode-define-key (key symbol)
   (define-key smsn-mode-map (kbd key) symbol))

(defun define-search-keys ()
  (if (boundp 'smsn-search-services)
    (dolist (l smsn-search-services)
      (let ((key (nth 0 l)) (service-name (nth 1 l)) (base-url (nth 2 l)))
        (define-key smsn-mode-map (kbd (concat "C-c C-t C-b " key))
          (smsn-web-search-on-focus-title service-name base-url))
        (define-key smsn-mode-map (kbd (concat "C-c C-r C-b " key))
          (smsn-web-search-on-page-title service-name base-url))
          ))))

(defvar smsn-mode-map nil)
(defun smsn-commands-define-keymap ()
  (if smsn-mode-map () (progn
    (setq smsn-mode-map (make-sparse-keymap))
    (define-key smsn-mode-map (kbd "C-c C-a C-p")     'smsn-insert-attr-priority-prompt)
    (define-key smsn-mode-map (kbd "C-c C-a C-s")     'smsn-insert-attr-source-prompt)
    (define-key smsn-mode-map (kbd "C-c C-a C-w")     'smsn-insert-attr-weight-prompt)
    (define-key smsn-mode-map (kbd "C-c C-a d")       'smsn-insert-current-date)
    (define-key smsn-mode-map (kbd "C-c C-a s")       'smsn-insert-current-time-with-seconds)
    (define-key smsn-mode-map (kbd "C-c C-a t")       'smsn-insert-current-time)
    (define-key smsn-mode-map (kbd "C-c C-b")         'smsn-visit-url-at-point)
    (define-key smsn-mode-map (kbd "C-c C-d")         'smsn-set-view-height-prompt)
    (define-key smsn-mode-map (kbd "C-c C-f")         'smsn-find-roots)
    (define-key smsn-mode-map (kbd "C-c C-i f")       'smsn-find-isolated-notes)
    (define-key smsn-mode-map (kbd "C-c C-i r")       'smsn-remove-isolated-notes)
    (define-key smsn-mode-map (kbd "C-c C-r c")       'smsn-read-vcs-prompt)
    (define-key smsn-mode-map (kbd "C-c C-r f")       'smsn-import-freeplane-prompt)
    (define-key smsn-mode-map (kbd "C-c C-r g")       'smsn-import-graphml-prompt)
    (define-key smsn-mode-map (kbd "C-c C-r y")       'smsn-read-yaml-prompt)
    (define-key smsn-mode-map (kbd "C-c C-s C-m")     'smsn-set-min-source-prompt)
    (define-key smsn-mode-map (kbd "C-c C-t C-a b")   'smsn-navigate-to-focus-alias)
    (define-key smsn-mode-map (kbd "C-c C-t C-p")     'smsn-set-focus-priority-prompt)
    (define-key smsn-mode-map (kbd "C-c C-t C-s")     'smsn-set-focus-source-prompt)
    (define-key smsn-mode-map (kbd "C-c C-t C-w")     'smsn-set-focus-weight-prompt)
    (define-key smsn-mode-map (kbd "C-c C-t a")       (smsn-visit-as-url 'smsn-data-focus-title))
    (define-key smsn-mode-map (kbd "C-c C-t i")       (smsn-note-info 'smsn-data-focus))
    (define-key smsn-mode-map (kbd "C-c C-t l")       'smsn-preview-focus-latex-math)
    (define-key smsn-mode-map (kbd "C-c C-t s")       'smsn-query-on-focus-title)
    (define-key smsn-mode-map (kbd "C-c C-v ;")       'smsn-toggle-truncate-lines)
    (define-key smsn-mode-map (kbd "C-c C-v e")       'smsn-enter-readwrite-view)
    (define-key smsn-mode-map (kbd "C-c C-v i")       'smsn-toggle-inference-viewstyle)
    (define-key smsn-mode-map (kbd "C-c C-v p")       'smsn-toggle-properties-view)
    (define-key smsn-mode-map (kbd "C-c C-v r")       'smsn-enter-readonly-view)
    (define-key smsn-mode-map (kbd "C-c C-v s")       'smsn-toggle-emacspeak)
    (define-key smsn-mode-map (kbd "C-c C-v t")       'smsn-set-title-truncation-length-prompt)
    (define-key smsn-mode-map (kbd "C-c C-v v")       'smsn-toggle-minimize-verbatim-blocks)
    (define-key smsn-mode-map (kbd "C-c C-w C-m")     'smsn-set-min-weight-prompt)
    (define-key smsn-mode-map (kbd "C-c C-w c")       'smsn-write-vcs-prompt)
    (define-key smsn-mode-map (kbd "C-c C-w e")       'smsn-export-edges-prompt)
    (define-key smsn-mode-map (kbd "C-c C-w g")       'smsn-export-graphml-prompt)
    (define-key smsn-mode-map (kbd "C-c C-w l")       'smsn-export-latex-prompt)
    (define-key smsn-mode-map (kbd "C-c C-w p")       'smsn-export-pagerank-prompt)
    (define-key smsn-mode-map (kbd "C-c C-w r")       'smsn-export-rdf-prompt)
    (define-key smsn-mode-map (kbd "C-c C-w v")       'smsn-export-vertices-prompt)
    (define-key smsn-mode-map (kbd "C-c C-w y")       'smsn-write-yaml-prompt)
    (define-key smsn-mode-map (kbd "C-c P")           'smsn-priorities)
    (define-key smsn-mode-map (kbd "C-c a")           'smsn-acronym-query-prompt)
    (define-key smsn-mode-map (kbd "C-c b")           'smsn-update-to-backward-view)
    (define-key smsn-mode-map (kbd "C-c d")           'smsn-duplicates)
    (define-key smsn-mode-map (kbd "C-c e")           'smsn-events)
    (define-key smsn-mode-map (kbd "C-c f")           'smsn-update-to-forward-view)
    (define-key smsn-mode-map (kbd "C-c g")           'smsn-ping-server)
    (define-key smsn-mode-map (kbd "C-c h")           'smsn-history)
    (define-key smsn-mode-map (kbd "C-c i")           'smsn-infer-types)
    (define-key smsn-mode-map (kbd "C-c j")           'smsn-action-dujour)
    (define-key smsn-mode-map (kbd "C-c l")           'smsn-view-log-prompt)
    (define-key smsn-mode-map (kbd "C-c m")           'smsn-toggle-move-or-edit-submode)
    (define-key smsn-mode-map (kbd "C-c n")           'smsn-open-new-note)
    (define-key smsn-mode-map (kbd "C-c o")           'smsn-open-note-prompt)
    (define-key smsn-mode-map (kbd "C-c p")           'smsn-push-view)
    (define-key smsn-mode-map (kbd "C-c q")           'smsn-ripple-query-prompt)
    (define-key smsn-mode-map (kbd "C-c r")           'smsn-copy-focus-reference-to-clipboard)
    (define-key smsn-mode-map (kbd "C-c s")           'smsn-title-query-prompt)
    (define-key smsn-mode-map (kbd "C-c t")           'smsn-open-focus-note)
    (define-key smsn-mode-map (kbd "C-c u")           'smsn-update-view)
    (define-key smsn-mode-map (kbd "C-c v")           'smsn-copy-focus-title-to-clipboard)
    (define-key smsn-mode-map (kbd "C-c w")           'smsn-focus-wikiview)
    (define-key smsn-mode-map (kbd "C-c x")           'smsn-shortcut-query-prompt)
    (define-key smsn-mode-map (kbd "C-c .")           'smsn-copy-root-reference-to-clipboard)
    (define-key smsn-mode-map (kbd "C-c C-c")         'smsn-debug)
    (define-key smsn-mode-map (kbd "C-x C-k o")       'smsn-kill-other-buffers)
    (define-search-keys)
)))

;; special mappings reserved for use through emacsclient
;; C-c c  --  smsn-data-note-id-at-point

(provide 'smsn-commands)
