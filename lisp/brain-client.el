;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; brain-client.el -- Client/server interaction
;;
;; Part of the Brain-mode package for Emacs:
;;   https://github.com/joshsh/brain-mode
;;
;; Copyright (C) 2011-2016 Joshua Shinavier and collaborators
;;
;; You should have received a copy of the GNU General Public License
;; along with this software.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'brain-env)
(require 'brain-data)


(defun create-request (action-name)
  (let ((action-class (concat "net.fortytwo.smsn.server.actions." action-name)))
    (list :action action-class)))

(defvar broadcast-rdf-request (create-request "BroadcastRDF"))
(defvar find-duplicates-request (create-request "FindDuplicates"))
(defvar find-isolated-atoms-request (create-request "FindIsolatedAtoms"))
(defvar find-roots-request (create-request "FindRoots"))
(defvar get-events-request (create-request "GetEvents"))
(defvar get-history-request (create-request "GetHistory"))
(defvar get-priorities-request (create-request "GetPriorities"))
(defvar get-view-request (create-request "GetView"))
(defvar infer-types-request (create-request "InferTypes"))
(defvar ping-request (create-request "Ping"))
(defvar push-event-request (create-request "PushEvent"))
(defvar read-graph-request (create-request "ReadGraph"))
(defvar remove-isolated-atoms-request (create-request "RemoveIsolatedAtoms"))
(defvar search-request (create-request "Search"))
(defvar set-properties-request (create-request "SetProperties"))
(defvar update-view-request (create-request "UpdateView"))
(defvar write-graph-request (create-request "WriteGraph"))

(defun issue-request (request &optional callback)
  (http-post-and-receive (find-server-url) request callback))

(defun add-to-request (request additional-params)
  (append request additional-params))

(defun to-filter-request (base-request)
  (add-to-request base-request (list
      :filter (to-filter))))

(defun to-query-request (base-request)
  (add-to-request (to-filter-request base-request) (list
      :valueCutoff (brain-env-context-get 'value-length-cutoff)
      :style brain-const-forward-style)))

(defun create-search-request (query-type query)
  (add-to-request (to-query-request search-request) (list
    :queryType query-type
    :query query
    :height 1)))

(defun create-view-request (root-id)
  (add-to-request (to-filter-request get-view-request) (list
    :root root-id
    :height (brain-env-context-get 'height)
    :style (brain-env-context-get 'style))))

(defun do-search (query-type query)
  (let ((request (create-search-request query-type query))
        (callback
          (lambda (payload context)
            (brain-env-context-set 'root-id nil context)
            (brain-env-context-set 'line 1 context)
            (funcall 'brain-view-open payload context))))
    (issue-request request callback)))

(defun do-write-graph (format file)
  (let ((request (add-to-request write-graph-request (list
        :format format
        :file file))))
    (issue-request request 'receive-export-response)))

(defun do-read-graph (format file)
  (let ((request (add-to-request read-graph-request (list
        :format format
        :file file))))
    (issue-request request 'receive-import-response)))

(defun do-set-property (id name value)
  (let ((request (add-to-request set-properties-request (list
        :id id
        :name name
        :value value))))
    (issue-request request 'receive-set-properties-response)))

(defun do-push-view ()
  (let ((request (add-to-request update-view-request (list
        :view (buffer-string)
        :viewFormat "wiki"
        :filter (to-filter)
        :root (brain-env-context-get 'root-id)
        :height (brain-env-context-get 'height)
        :style (brain-env-context-get 'style)))))
    (issue-request request)))

;; from Emacs-w3m w3m-url-encode-string
(defun brain-client-url-encode (str &optional coding)
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

(defun format-request-data (params)
  (json-encode (list
    (cons 'language "smsn")
    (cons 'gremlin (json-encode params)))))

(defun http-post (url params callback)
  "Issue an HTTP POST request to URL with PARAMS"
  (let ((url-request-method "POST")
        (url-request-extra-headers '(("Content-Type" . "application/json;charset=UTF-8")))
        (url-request-data
          (format-request-data params)))
    (url-retrieve url callback)))

(defun http-get (url callback)
  "Issue an HTTP GET request to URL"
  (url-retrieve url callback))

(defun strip-http-headers (entity)
  (let ((i (string-match "\n\n" entity)))
    (if i
        (decode-coding-string (substring entity (+ i 2)) 'utf-8)
        "{}")))

(defun get-buffer-json ()
  (json-read-from-string (strip-http-headers (buffer-string))))

(defun get-response-error (json)
  (brain-env-json-get 'error json))

(defun get-response-message (json)
  (brain-env-json-get 'message
    (brain-env-json-get 'status json)))

(defun get-response-payload (json)
  (car
    (brain-env-json-get 'data
      (brain-env-json-get 'result json))))

(defun acknowledge-success (success-message)
    (brain-env-info-message success-message))

(defun receive-export-response (payload context)
  (acknowledge-success "exported successfully"))

(defun receive-import-response (payload context)
  (acknowledge-success "imported successfully"))

(defun receive-inference-response (payload context)
  (acknowledge-success "type inference completed successfully"))

(defun receive-remove-isolated-atoms-response (payload context)
  (acknowledge-success "removed isolated atoms"))

(defun receive-set-properties-response (payload context)
  (brain-client-request))

(defun to-filter (&optional context)
  (list
      :minSharability (brain-env-context-get 'min-sharability context)
      :maxSharability (brain-env-context-get 'max-sharability context)
      :defaultSharability (brain-env-context-get 'default-sharability context)
      :minWeight (brain-env-context-get 'min-weight context)
      :maxWeight (brain-env-context-get 'max-weight context)
      :defaultWeight (brain-env-context-get 'default-weight context)))

(defun to-query-list (&optional context)
  (list
    :action (brain-env-context-get 'action context)
    :root (brain-env-context-get 'root-id context)
    :height (brain-env-context-get 'height context)
    :style (brain-env-context-get 'style context)
    :includeTypes (if (brain-env-using-inference) "true" "false")
    :file (brain-env-context-get 'file context)
    :format (brain-env-context-get 'format context)
    :query (brain-env-context-get 'query context)
    :queryType (brain-env-context-get 'query-type context)
    :valueCutoff (brain-env-context-get 'value-length-cutoff)
    :view (brain-env-context-get 'view context)
    :filter (to-filter context)))

(defun http-post-and-receive (url params &optional callback)
  ;;(brain-env-debug-message (concat "context: " (json-encode context)))
  (http-post url params
    (http-callback callback)))

(defun http-callback (&optional callback)
  (lexical-let
    ((context (brain-env-get-context))
     (callback (if callback callback 'brain-view-open)))
    (lambda (status)
      (handle-response status context callback))))

(defun handle-response (http-status context callback)
  (if http-status
    (brain-env-error-message (concat "HTTP request failed: " (json-encode http-status)))
    (let ((json (get-buffer-json)))
      (let ((message (brain-env-json-get 'message (brain-env-json-get 'status json)))
            (payload (get-payload json)))
        (if (and message (> (length message) 0))
          (brain-env-error-message (concat "request failed: " message))
          (if payload
            (funcall callback payload context)
            (brain-env-error-message "no response data")))))))

(defun get-payload (json)
  (if json
    (let ((data-array (brain-env-json-get 'data (brain-env-json-get 'result json))))
      (if data-array
        (if (= 1 (length data-array))
          (json-read-from-string (aref data-array 0))
          (brain-env-error-message "unexpected data array length"))
        nil))
    nil))

(defun find-server-url ()
  (if (boundp 'brain-server-url) brain-server-url "http://127.0.0.1:8182"))

(defun brain-client-navigate-to-atom (atom-id)
  (issue-request (create-view-request atom-id)))

(defun brain-client-request ()
  (brain-client-navigate-to-atom (brain-env-context-get 'root-id)))

(defun brain-client-fetch-history ()
  (issue-request (to-filter-request get-history-request)))

(defun brain-client-fetch-events (height)
  (issue-request (to-query-request get-events-request)))

(defun brain-client-fetch-duplicates ()
  (issue-request (to-filter-request find-duplicates-request)))

(defun brain-client-fetch-query (query query-type)
  (do-search query-type query))

(defun brain-client-fetch-priorities ()
  (issue-request (to-query-request get-priorities-request)))

(defun brain-client-fetch-find-isolated-atoms ()
  (issue-request (to-filter-request find-isolated-atoms-request)))

(defun brain-client-fetch-find-roots ()
  (issue-request (to-query-request find-roots-request)))

(defun brain-client-fetch-remove-isolated-atoms ()
  (issue-request (to-filter-request remove-isolated-atoms-request) 'receive-remove-isolated-atoms-response))

(defun brain-client-fetch-ripple-response (query)
  (do-search "Ripple" query))

(defun brain-client-export (format file)
  (do-write-graph format file))

(defun brain-client-import (format file)
  (do-read-graph format file))

(defun brain-client-set-property (id name value)
  (if (brain-env-in-setproperties-mode)
    (do-set-property id name value)))

(defun brain-client-push-view ()
  (do-push-view))

(defun brain-client-infer-types ()
  (issue-request infer-types-request 'receive-inference-response))

(defun brain-client-set-min-weight (s)
  (if (and (brain-env-in-setproperties-mode) (>= s 0) (<= s 1))
    (let ()
      (brain-env-context-set 'min-weight s)
      (brain-client-request))
    (brain-env-error-message
     (concat "min weight " (number-to-string s) " is outside of range [0, 1]"))))

(defun brain-client-set-min-sharability (s)
  (if (and (brain-env-in-setproperties-mode) (>= s 0) (<= s 1))
    (let ()
      (brain-env-context-set 'min-sharability s)
      (brain-client-request))
    (brain-env-error-message
     (concat "min sharability " (number-to-string s) " is outside of range [0, 1]"))))

(defun brain-client-set-focus-priority (v)
  (if (and (>= v 0) (<= v 1))
      (let ((focus (brain-data-focus)))
        (if focus
            (let (
                  (id (brain-data-atom-id focus)))
              (brain-client-set-property id "priority" v))
          (brain-env-error-no-focus)))
    (brain-env-error-message
     (concat "priority " (number-to-string v) " is outside of range [0, 1]"))))

(defun brain-client-set-focus-sharability (v)
  (if (and (> v 0) (<= v 1))
      (let ((focus (brain-data-focus)))
        (if focus
            (let ((id (brain-data-atom-id focus)))
              (brain-client-set-property id "sharability" v))
          (brain-env-error-no-focus)))
    (brain-env-error-message
     (concat "sharability " (number-to-string v) " is outside of range (0, 1]"))))

(defun brain-client-set-focus-weight (v)
  (if (and (> v 0) (<= v 1))
      (let ((focus (brain-data-focus)))
        (if focus
            (let (
                  (id (brain-data-atom-id focus)))
              (brain-client-set-property id "weight" v))
          (brain-env-error-no-focus)))
    (brain-env-error-message
     (concat "weight " (number-to-string v) " is outside of range (0, 1]"))))


(provide 'brain-client)
