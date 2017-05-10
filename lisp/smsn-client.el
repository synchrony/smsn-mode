;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; smsn-client.el -- Client/server interaction
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
(require 'smsn-http)
(require 'smsn-websocket)

(defun create-request (action-name)
  (let ((action-class (concat "net.fortytwo.smsn.server.actions." action-name)))
    (list :action action-class)))

(defun add-to-request (request additional-params)
  (append request additional-params))

(defvar broadcast-rdf-request (create-request "BroadcastRDF"))
(defvar action-dujour-request (create-request "ActionDuJour"))
(defvar find-duplicates-request (create-request "FindDuplicates"))
(defvar find-isolated-atoms-request (create-request "FindIsolatedAtoms"))
(defvar find-roots-request (add-to-request (create-request "FindRoots") (list :height 1)))
(defvar get-configuration-request (create-request "GetConfiguration"))
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
(defvar view-log-request (create-request "ViewLog"))
(defvar write-graph-request (create-request "WriteGraph"))

(defun to-filter-request (base-request)
  (add-to-request base-request (list
      :filter (to-filter))))

(defun to-query-request (base-request)
  (add-to-request (to-filter-request base-request) (list
      :titleCutoff (smsn-env-context-get 'title-length-cutoff)
      :style smsn-const-forward-style)))

(defun create-search-request (query-type query)
  (add-to-request (to-query-request search-request) (list
    :queryType query-type
    :query query
    :height 1)))

(defun create-treeview-request (root-id)
  (add-to-request (to-filter-request get-view-request) (list
    :root root-id
    :height (smsn-env-context-get 'height)
    :style (smsn-env-context-get 'style))))

(defun create-new-treeview-request (root-id)
  "Like create-treeview-request, but with max inherited height of 3."
  (add-to-request (to-filter-request get-view-request) (list
    :root root-id
    :height (min (smsn-env-context-get 'height) 3)
    :style (smsn-env-context-get 'style))))

(defun create-wikiview-request (root-id)
  (add-to-request (to-filter-request get-view-request) (list
    :root root-id
    :height 1)))

(defun do-search (query-type query)
  (issue-request (create-search-request query-type query) 'search-view-callback))

(defun do-write-graph (format file)
  (let ((request (add-to-request write-graph-request (list
        :filter (to-filter)
        :format format
        :file file))))
    (issue-request request 'export-callback)))

(defun do-read-graph (format file)
  (let ((request (add-to-request read-graph-request (list
        :format format
        :file file))))
    (issue-request request 'import-callback)))

(defun do-view-log (file)
  (let ((request (add-to-request (to-filter-request view-log-request) (list
      :file file
      :height 3))))
    (issue-request request 'smsn-treeview-open)))

(defun set-property (id name value callback)
  (smsn-view-set-context-line)
  (let ((request (add-to-request set-properties-request
      (list
        :id id
        :name name
        :value value))))
    (issue-request request callback)))

(defun set-property-in-treeview (id name value)
  (set-property id name value
    (lambda (payload context)
      (issue-request (create-treeview-request (smsn-env-context-get 'root-id context)) 'smsn-treeview-open))))

(defun set-property-in-wikiview (name value)
  (let ((id (smsn-env-context-get 'root-id)))
    (set-property id name value (lambda (payload context)
    (issue-request (create-wikiview-request (smsn-env-context-get 'root-id context)) 'smsn-wikiview-open)))))

(defun push-treeview ()
  (let ((request (add-to-request update-view-request
      (list
        :view (buffer-string)
        :viewFormat "wiki"
        :filter (to-filter)
        :root (smsn-env-context-get 'root-id)
        :height (smsn-env-context-get 'height)
        :style (smsn-env-context-get 'style)))))
    (issue-request request 'smsn-treeview-open)))

(defun push-wikiview ()
   (set-property-in-wikiview "page" (buffer-string)))

(defun to-filter (&optional context)
  (list
      :minSharability (smsn-env-context-get 'min-sharability context)
      :defaultSharability (smsn-env-context-get 'default-sharability context)
      :minWeight (smsn-env-context-get 'min-weight context)
      :defaultWeight (smsn-env-context-get 'default-weight context)))

(defun export-callback (payload context)
  (message "exported successfully in %.0f ms" (smsn-env-response-time)))

(defun import-callback (payload context)
  (message "imported successfully in %.0f ms" (smsn-env-response-time)))

(defun inference-callback (payload context)
  (message "type inference completed successfully in %.0f ms" (smsn-env-response-time)))
      
(defun ping-callback (payload context)
  (message "completed in %.0f ms" (smsn-env-response-time)))

(defun remove-isolated-atoms-callback (payload context)
  (message "removed isolated atoms in %.0f ms" (smsn-env-response-time)))

(defun treeview-callback (payload context)
  (smsn-env-to-treeview-mode context)
  (smsn-treeview-open payload context))

(defun wikiview-callback (payload context)
  (smsn-env-to-wikiview-mode context)
  (smsn-wikiview-open payload context))

(defun search-view-callback (payload context)
  (smsn-view-set-context-line 1)
  (smsn-env-to-search-mode context)
  (smsn-treeview-open payload context))

(defun create-source-map (conf)
  (let ((sources (smsn-env-json-get 'sources conf)))
    (if sources
      (let ((ht (make-hash-table :test 'equal)) (source-list (mapcar (lambda (x) x) sources)))
        (dolist (source source-list)
          (let ((name (smsn-env-json-get 'name source)))
            (puthash name source ht)))
        ht)
      (error "no sources"))))

(defun update-configuration-callback (payload context)
  (let ((conf (json-read-from-string (smsn-env-json-get 'configuration payload))))
    (smsn-env-context-set 'configuration conf)
    (smsn-env-context-set 'sources (create-source-map conf))))

(defun issue-request (request callback)
  (smsn-env-set-timestamp)
  (let (
      (protocol (find-server-protocol))
      (host (find-server-host))
      (port (find-server-port)))
    (if (equal "http" protocol)
      (smsn-http-send-and-receive host port request callback)
      (if (equal "websocket" protocol)
        (smsn-websocket-send-and-receive host port request callback)
        (error (concat "unsupported protocol: " protocol))))))

(defun find-server-protocol ()
  (downcase
    (if (boundp 'smsn-server-protocol) smsn-server-protocol "http")))

(defun find-server-host ()
  (if (boundp 'smsn-server-host) smsn-server-host "127.0.0.1"))

(defun find-server-port ()
  (if (boundp 'smsn-server-port) smsn-server-port 8182))

(defun smsn-client-export (format file)
  (do-write-graph format file))

(defun smsn-client-fetch-duplicates ()
  (issue-request (to-filter-request find-duplicates-request) 'search-view-callback))

(defun smsn-client-fetch-configuration ()
  (issue-request (to-filter-request get-configuration-request) 'update-configuration-callback))

(defun smsn-client-fetch-events (height)
  (issue-request (to-query-request get-events-request) 'search-view-callback))

(defun smsn-client-fetch-history ()
  (issue-request (to-filter-request get-history-request) 'search-view-callback))

(defun smsn-client-fetch-find-isolated-atoms ()
  (issue-request (to-filter-request find-isolated-atoms-request) 'search-view-callback))

(defun smsn-client-fetch-priorities ()
  (issue-request (to-query-request get-priorities-request) 'search-view-callback))

(defun smsn-client-fetch-query (query query-type)
  (do-search query-type query))

(defun smsn-client-fetch-remove-isolated-atoms ()
  (issue-request (to-filter-request remove-isolated-atoms-request) 'remove-isolated-atoms-callback))

(defun smsn-client-fetch-ripple-response (query)
  (do-search "Ripple" query))

(defun smsn-client-find-roots ()
  (issue-request (to-query-request find-roots-request) 'search-view-callback))

(defun smsn-client-import (format file)
  (do-read-graph format file))

(defun smsn-client-view-log (file)
  (do-view-log file))

(defun smsn-client-infer-types ()
  (issue-request infer-types-request 'inference-callback))

(defun smsn-client-open-atom (atom-id)
  (smsn-view-set-context-line 1)
  (issue-request (create-new-treeview-request atom-id) 'treeview-callback))

(defun smsn-client-wikiview (atom-id)
  (smsn-view-set-context-line 1)
  (issue-request (create-wikiview-request atom-id) 'wikiview-callback))

(defun smsn-client-action-dujour ()
  (issue-request action-dujour-request 'ping-callback))

(defun smsn-client-ping-server ()
  (issue-request ping-request 'ping-callback))

(defun smsn-client-push-treeview ()
  (smsn-view-set-context-line)
  (push-treeview))

(defun smsn-client-push-wikiview () (push-wikiview))

(defun smsn-client-refresh-treeview ()
  (smsn-view-set-context-line)
  (issue-request (create-treeview-request (smsn-env-context-get 'root-id)) 'treeview-callback))

(defun smsn-client-refresh-wikiview ()
  (issue-request (create-wikiview-request (smsn-env-context-get 'root-id)) 'wikiview-callback))

(defun smsn-client-set-focus-priority (v)
  (if (and (>= v 0) (<= v 1))
      (let ((focus (smsn-data-focus)))
        (if focus
            (let (
                  (id (smsn-data-atom-id focus)))
              (smsn-client-set-property id "priority" v))
          (smsn-env-error-no-focus)))
    (error 
     (concat "priority " (number-to-string v) " is outside of range [0, 1]"))))

(defun smsn-client-set-focus-sharability (v)
  (if (and (> v 0) (<= v 1))
      (let ((focus (smsn-data-focus)))
        (if focus
            (let ((id (smsn-data-atom-id focus)))
              (smsn-client-set-property id "sharability" v))
          (smsn-env-error-no-focus)))
    (error 
     (concat "sharability " (number-to-string v) " is outside of range (0, 1]"))))

(defun smsn-client-set-focus-weight (v)
  (if (and (> v 0) (<= v 1))
      (let ((focus (smsn-data-focus)))
        (if focus
            (let (
                  (id (smsn-data-atom-id focus)))
              (smsn-client-set-property id "weight" v))
          (smsn-env-error-no-focus)))
    (error 
     (concat "weight " (number-to-string v) " is outside of range (0, 1]"))))

(defun smsn-client-set-min-sharability (s)
  (if (and (smsn-env-in-setproperties-mode) (>= s 0) (<= s 1))
    (let ()
      (smsn-env-context-set 'min-sharability s)
      (smsn-client-refresh-treeview))
    (error 
     (concat "min sharability " (number-to-string s) " is outside of range [0, 1]"))))

(defun smsn-client-set-min-weight (s)
  (if (and (smsn-env-in-setproperties-mode) (>= s 0) (<= s 1))
    (let ()
      (smsn-env-context-set 'min-weight s)
      (smsn-client-refresh-treeview))
    (error 
     (concat "min weight " (number-to-string s) " is outside of range [0, 1]"))))

(defun smsn-client-set-property (id name value)
  (if (smsn-env-in-setproperties-mode)
    (set-property-in-treeview id name value)))


(provide 'smsn-client)
