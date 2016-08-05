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
  (mapconcat
    (lambda (arg)
      (concat
        (brain-client-url-encode (car arg))
        "="
        (brain-client-url-encode (car (last arg)))))
    params
    "&"))

(defun http-post (url params callback)
  "Issue an HTTP POST request to URL with PARAMS"
  (let ((url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "application/x-www-form-urlencoded;charset=UTF-8")))
        (url-request-data
          (format-request-data (list (list "request" (json-encode params))))))
    (url-retrieve url callback)))

(defun http-get (url callback)
  "Issue an HTTP GET request to URL"
  (url-retrieve url callback))

(defun strip-http-headers (entity)
  (let ((i (string-match "\n\n" entity)))
    (if i
        (decode-coding-string (substring entity (+ i 2)) 'utf-8)
        "{}")))

(defun brain-client-buffer-json ()
  (json-read-from-string (strip-http-headers (buffer-string))))

(defun base-url ()
  (concat brain-rexster-url "/graphs/" brain-rexster-graph "/smsn/"))

(defun brain-client-show-http-response-status (status json)
  (let ((msg (get-value 'message json))
        (error (get-value 'error json)))
    (if error
        (brain-env-error-message error)
        (if msg
            (brain-env-error-message msg)
          (brain-env-error-message (concat "request failed: "
                                 (json-encode status)))))))

(defun acknowledge-http-response (status success-message)
  (if status
      (let ((json (brain-client-buffer-json)))
        (brain-client-show-http-response-status status json))
    (brain-env-info-message success-message)))

(defun receive-export-results (status)
  (acknowledge-http-response status "exported successfully"))

(defun receive-import-results (status)
  (acknowledge-http-response status "imported successfully"))

(defun receive-inference-results (status)
  (acknowledge-http-response status "type inference completed successfully"))

(defun receive-remove-isolated-atoms-response (status)
  (acknowledge-http-response status "removed isolated atoms"))

(defun receive-set-properties-response (status)
  (if status
    (brain-client-show-http-response-status status json)
    (brain-client-request)))

(defun to-query-list (&optional context)
  (list
    :action (get-action context)
    :root (get-root-id context)
    :height (get-height context)
    :style (get-style context)
    :includeTypes (if (brain-env-using-inference) "true" "false")
    :file (get-file context)
    :format (get-format context)
    :query (get-query context)
    :queryType (get-query-type context)
    :valueCutoff (get-value-length-cutoff)
    :view (get-view context)
    :filter (list
      :minSharability (get-min-sharability context)
      :maxSharability (get-max-sharability context)
      :defaultSharability (get-default-sharability context)
      :minWeight (get-min-weight context)
      :maxWeight (get-max-weight context)
      :defaultWeight (get-default-weight context))))

(defun entity-for-request (params)
  (brain-client-url-encode (json-encode params)))

(defun url-for-request (path &optional params)
  (concat
    (base-url)
    path
    (if params
      (entity-for-request params)
      nil)))

(defun http-post-and-receive (url params &optional context handler)
  ;;(brain-env-debug-message (concat "context: " (json-encode context)))
  (http-post url params
    (if handler handler (brain-view-open context))))

(defun to-params (context params)
  (if params params
    (to-query-list (if context context (brain-env-context)))))

(defun execute-request (action context params &optional handler)
  (set-action action context)
  (http-post-and-receive (url-for-request "brain")
    (to-params context params) context handler))

(defun brain-client-request (&optional context)
  (execute-request "view" context nil))

(defun brain-client-fetch-history ()
  (let ((context (brain-env-create-search-context)))
    (execute-request "history" context nil)))

(defun brain-client-fetch-events (height)
  (let ((context (brain-env-create-search-context)))
    (execute-request "get-events" context nil)))

(defun brain-client-fetch-duplicates ()
  (let ((context (brain-env-create-search-context)))
    (execute-request "duplicates" context nil)))

(defun brain-client-fetch-query (query query-type)
  (let ((context (brain-env-create-search-context)))
    (set-query query context)
    (set-query-type query-type context)
    (execute-request "search" context nil)))

(defun brain-client-fetch-priorities ()
  (let ((context (brain-env-create-search-context)))
    (execute-request "priorities" context nil)))

(defun brain-client-fetch-find-isolated-atoms ()
  (let ((context (brain-env-create-search-context)))
    (execute-request "find-isolated-atoms" context nil)))

(defun brain-client-fetch-find-roots ()
  (let ((context (brain-env-create-search-context)))
    (execute-request "find-roots" context nil)))

(defun brain-client-fetch-remove-isolated-atoms ()
  (execute-request "remove-isolated-atoms" nil nil 'receive-remove-isolated-atoms-response))

(defun brain-client-fetch-ripple-results (query)
  (let ((context (brain-env-create-search-context)))
    (set-query query context)
    (execute-request "ripple" context nil)))

(defun brain-client-export (format file)
  (let ((context (brain-env-clone-context)))
    (set-format format context)
    (set-file file context)
    (execute-request "export" context nil 'receive-export-results)))

(defun brain-client-import (format file)
  (let ((context (brain-env-clone-context)))
    (set-format format context)
    (set-file file context)
    (execute-request "import" context nil 'receive-import-results)))

(defun brain-client-set-property (id name value)
  (if (brain-env-in-setproperties-mode)
    ;; TODO: this is redundant
    (let ((params (list :action "set" :id id :name name :value value)))
       (execute-request "set" nil params 'receive-set-properties-response))))

(defun brain-client-push-view ()
  (let ((context (brain-env-clone-context)) (entity (buffer-string)))
    (set-view entity context)
    (brain-env-set-readwrite context)
    (execute-request "update" context nil)))

(defun brain-client-infer-types ()
  (execute-request "infer-types" context nil 'receive-inference-results))

(defun brain-client-set-min-weight (s)
  (if (and (brain-env-in-setproperties-mode) (>= s 0) (<= s 1))
    (let ((context (brain-env-clone-context)))
      (set-min-weight s context)
      (brain-client-request context))
    (brain-env-error-message
     (concat "min weight " (number-to-string s) " is outside of range [0, 1]"))))

(defun brain-client-set-min-sharability (s)
  (if (and (brain-env-in-setproperties-mode) (>= s 0) (<= s 1))
    (let ((context (brain-env-clone-context)))
      (set-min-sharability s context)
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
