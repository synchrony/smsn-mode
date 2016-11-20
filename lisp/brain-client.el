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
;;(require 'brain-view)


(defun create-search-context ()
  (let ((context (brain-env-clone-context)))
    (brain-env-context-set 'mode brain-const-search-mode context)
    (brain-env-context-set 'height 1 context)
    (brain-env-context-set 'line 1 context)
    context))

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

(defun receive-export-results (json context)
  (acknowledge-success "exported successfully"))

(defun receive-import-results (json context)
  (acknowledge-success "imported successfully"))

(defun receive-inference-results (json context)
  (acknowledge-success "type inference completed successfully"))

(defun receive-remove-isolated-atoms-response (json context)
  (acknowledge-success "removed isolated atoms"))

(defun receive-set-properties-response (json context)
  (brain-client-request))

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
    :filter (list
      :minSharability (brain-env-context-get 'min-sharability context)
      :maxSharability (brain-env-context-get 'max-sharability context)
      :defaultSharability (brain-env-context-get 'default-sharability context)
      :minWeight (brain-env-context-get 'min-weight context)
      :maxWeight (brain-env-context-get 'max-weight context)
      :defaultWeight (brain-env-context-get 'default-weight context))))

(defun http-post-and-receive (url params &optional context handler)
  ;;(brain-env-debug-message (concat "context: " (json-encode context)))
  (http-post url params
    (http-callback context handler)))

(defun http-callback (&optional context handler)
  (lexical-let
    ((context (context-for-response context))
     (handler (if handler handler 'brain-view-open)))
    (lambda (status)
      (handle-response status context handler))))

(defun handle-response (http-status context handler)
  (if http-status
    (brain-env-error-message (concat "HTTP request failed: " (json-encode http-status)))
    (let ((json (get-buffer-json)))
      (let ((message (brain-env-json-get 'message (brain-env-json-get 'status json)))
            (payload (get-payload json)))
        (if (and message (> (length message) 0))
          (brain-env-error-message (concat "request failed: " message))
          (if payload
            (funcall handler payload context)
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

(defun context-for-response (&optional context)
  (let ((ctx (if context context (brain-env-context-get-context))))
;;    (brain-env-context-set 'view nil ctx)
;;    (brain-env-context-set 'atoms-by-id nil ctx)
    ctx))

(defun to-params (context params)
  (if params params
    (to-query-list (if context context (brain-env-context-get-context)))))

(defun execute-request (action context params &optional handler)
  (brain-env-context-set 'action action context)
  (http-post-and-receive brain-server-url
    (to-params context params) context handler))

(defun brain-client-request (&optional context)
  (execute-request "view" context nil))

(defun brain-client-fetch-history ()
  (let ((context (create-search-context)))
    (execute-request "history" context nil)))

(defun brain-client-fetch-events (height)
  (let ((context (create-search-context)))
    (execute-request "get-events" context nil)))

(defun brain-client-fetch-duplicates ()
  (let ((context (create-search-context)))
    (execute-request "duplicates" context nil)))

(defun brain-client-fetch-query (query query-type)
  (let ((context (create-search-context)))
    (brain-env-context-set 'query query context)
    (brain-env-context-set 'query-type query-type context)
    (execute-request "search" context nil)))

(defun brain-client-fetch-priorities ()
  (let ((context (create-search-context)))
    (execute-request "priorities" context nil)))

(defun brain-client-fetch-find-isolated-atoms ()
  (let ((context (create-search-context)))
    (execute-request "find-isolated-atoms" context nil)))

(defun brain-client-fetch-find-roots ()
  (let ((context (create-search-context)))
    (execute-request "find-roots" context nil)))

(defun brain-client-fetch-remove-isolated-atoms ()
  (execute-request "remove-isolated-atoms" nil nil 'receive-remove-isolated-atoms-response))

(defun brain-client-fetch-ripple-results (query)
  (let ((context (create-search-context)))
    (brain-env-context-set 'query query context)
    (execute-request "ripple" context nil)))

(defun brain-client-export (format file)
  (let ((context (brain-env-clone-context)))
    (brain-env-context-set 'format format context)
    (brain-env-context-set 'file file context)
    (execute-request "export" context nil 'receive-export-results)))

(defun brain-client-import (format file)
  (let ((context (brain-env-clone-context)))
    (brain-env-context-set 'format format context)
    (brain-env-context-set 'file file context)
    (execute-request "import" context nil 'receive-import-results)))

(defun brain-client-set-property (id name value)
  (if (brain-env-in-setproperties-mode)
    ;; TODO: this is redundant
    (let ((params (list :action "set" :id id :name name :value value)))
       (execute-request "set" nil params 'receive-set-properties-response))))

(defun brain-client-push-view ()
  (let ((context (brain-env-clone-context)) (entity (buffer-string)))
    (brain-env-context-set 'view entity context)
    (brain-env-context-set-readwrite context)
    (execute-request "update" context nil)))

(defun brain-client-infer-types ()
  (execute-request "infer-types" context nil 'receive-inference-results))

(defun brain-client-set-min-weight (s)
  (if (and (brain-env-in-setproperties-mode) (>= s 0) (<= s 1))
    (let ((context (brain-env-clone-context)))
      (brain-env-context-set 'min-weight s context)
      (brain-client-request context))
    (brain-env-error-message
     (concat "min weight " (number-to-string s) " is outside of range [0, 1]"))))

(defun brain-client-set-min-sharability (s)
  (if (and (brain-env-in-setproperties-mode) (>= s 0) (<= s 1))
    (let ((context (brain-env-clone-context)))
      (brain-env-context-set 'min-sharability s context)
      (brain-client-request context))
    (brain-env-error-message
     (concat "min sharability " (number-to-string s) " is outside of range [0, 1]"))))

(defun brain-client-set-target-priority (v)
  (if (and (>= v 0) (<= v 1))
      (let ((target (brain-data-target)))
        (if target
            (let (
                  (id (brain-data-atom-id target)))
              (brain-client-set-property id "priority" v))
          (brain-env-error-no-target)))
    (brain-env-error-message
     (concat "priority " (number-to-string v) " is outside of range [0, 1]"))))

(defun brain-client-set-target-sharability (v)
  (if (and (> v 0) (<= v 1))
      (let ((target (brain-data-target)))
        (if target
            (let ((id (brain-data-atom-id target)))
              (brain-client-set-property id "sharability" v))
          (brain-env-error-no-target)))
    (brain-env-error-message
     (concat "sharability " (number-to-string v) " is outside of range (0, 1]"))))

(defun brain-client-set-target-weight (v)
  (if (and (> v 0) (<= v 1))
      (let ((target (brain-data-target)))
        (if target
            (let (
                  (id (brain-data-atom-id target)))
              (brain-client-set-property id "weight" v))
          (brain-env-error-no-target)))
    (brain-env-error-message
     (concat "weight " (number-to-string v) " is outside of range (0, 1]"))))


(provide 'brain-client)
