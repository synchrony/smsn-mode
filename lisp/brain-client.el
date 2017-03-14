;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; brain-client.el -- Client/server interaction
;;
;; Part of the Brain-mode package for Emacs:
;;   https://github.com/joshsh/brain-mode
;;
;; Copyright (C) 2011-2017 Joshua Shinavier and collaborators
;;
;; You should have received a copy of the GNU General Public License
;; along with this software.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'brain-env)
(require 'brain-data)
(require 'brain-client-http)
(require 'brain-client-websocket)


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

(defun to-filter-request (base-request)
  (add-to-request base-request (list
      :filter (to-filter))))

(defun to-query-request (base-request)
  (add-to-request (to-filter-request base-request) (list
      :titleCutoff (brain-env-context-get 'title-length-cutoff)
      :style brain-const-forward-style)))

(defun create-search-request (query-type query)
  (add-to-request (to-query-request search-request) (list
    :queryType query-type
    :query query
    :height 1)))

(defun create-treeview-request (root-id)
  (add-to-request (to-filter-request get-view-request) (list
    :root root-id
    :height (brain-env-context-get 'height)
    :style (brain-env-context-get 'style))))

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

(defun set-property (id name value callback)
  (brain-view-set-context-line)
  (let ((request (add-to-request set-properties-request
      (list
        :id id
        :name name
        :value value))))
    (issue-request request callback)))

(defun set-property-in-treeview (id name value)
  (set-property id name value
    (lambda (payload context)
      (issue-request (create-treeview-request (brain-env-context-get 'root-id context)) 'brain-treeview-open))))

(defun set-property-in-wikiview (name value)
  (let ((id (brain-env-context-get 'root-id)))
    (set-property id name value (lambda (payload context)
    (issue-request (create-wikiview-request (brain-env-context-get 'root-id context)) 'brain-wikiview-open)))))

(defun push-treeview ()
  (let ((request (add-to-request update-view-request
      (list
        :view (buffer-string)
        :viewFormat "wiki"
        :filter (to-filter)
        :root (brain-env-context-get 'root-id)
        :height (brain-env-context-get 'height)
        :style (brain-env-context-get 'style)))))
    (issue-request request 'brain-treeview-open)))

(defun push-wikiview ()
   (set-property-in-wikiview "page" (buffer-string)))

(defun to-filter (&optional context)
  (list
      :minSharability (brain-env-context-get 'min-sharability context)
      :maxSharability (brain-env-context-get 'max-sharability context)
      :defaultSharability (brain-env-context-get 'default-sharability context)
      :minWeight (brain-env-context-get 'min-weight context)
      :maxWeight (brain-env-context-get 'max-weight context)
      :defaultWeight (brain-env-context-get 'default-weight context)))

(defun export-callback (payload context)
  (message "exported successfully in %.0f ms" (brain-env-response-time)))

(defun import-callback (payload context)
  (message "imported successfully in %.0f ms" (brain-env-response-time)))

(defun inference-callback (payload context)
  (message "type inference completed successfully in %.0f ms" (brain-env-response-time)))
      
(defun ping-callback (payload context)
  (message "completed in %.0f ms" (brain-env-response-time)))

(defun remove-isolated-atoms-callback (payload context)
  (message "removed isolated atoms in %.0f ms" (brain-env-response-time)))

(defun treeview-callback (payload context)
  (brain-env-to-treeview-mode context)
  (brain-treeview-open payload context))

(defun wikiview-callback (payload context)
  (brain-env-to-wikiview-mode context)
  (brain-wikiview-open payload context))

(defun search-view-callback (payload context)
  (brain-view-set-context-line 1)
  (brain-env-to-search-mode context)
  (brain-treeview-open payload context))

(defun issue-request (request callback)
  (brain-env-set-timestamp)
  (let (
      (protocol (find-server-protocol))
      (host (find-server-host))
      (port (find-server-port)))
    (if (equal "http" protocol)
      (brain-client-http-send-and-receive host port request callback)
      (if (equal "websocket" protocol)
        (brain-client-websocket-send-and-receive host port request callback)
        (error (concat "unsupported protocol: " protocol))))))

(defun find-server-protocol ()
  (downcase
    (if (boundp 'brain-server-protocol) brain-server-protocol "http")))

(defun find-server-host ()
  (if (boundp 'brain-server-host) brain-server-host "127.0.0.1"))

(defun find-server-port ()
  (if (boundp 'brain-server-port) brain-server-port 8182))

(defun brain-client-export (format file)
  (do-write-graph format file))

(defun brain-client-fetch-duplicates ()
  (issue-request (to-filter-request find-duplicates-request) 'search-view-callback))

(defun brain-client-fetch-events (height)
  (issue-request (to-query-request get-events-request) 'search-view-callback))

(defun brain-client-fetch-history ()
  (issue-request (to-filter-request get-history-request) 'search-view-callback))

(defun brain-client-fetch-find-isolated-atoms ()
  (issue-request (to-filter-request find-isolated-atoms-request) 'search-view-callback))

(defun brain-client-fetch-priorities ()
  (issue-request (to-query-request get-priorities-request) 'search-view-callback))

(defun brain-client-fetch-query (query query-type)
  (do-search query-type query))

(defun brain-client-fetch-remove-isolated-atoms ()
  (issue-request (to-filter-request remove-isolated-atoms-request) 'remove-isolated-atoms-callback))

(defun brain-client-fetch-ripple-response (query)
  (do-search "Ripple" query))

(defun brain-client-find-roots ()
  (issue-request (to-query-request find-roots-request) 'search-view-callback))

(defun brain-client-import (format file)
  (do-read-graph format file))

(defun brain-client-infer-types ()
  (issue-request infer-types-request 'inference-callback))

(defun brain-client-navigate-to-atom (atom-id)
  (brain-view-set-context-line 1)
  (issue-request (create-treeview-request atom-id) 'treeview-callback))

(defun brain-client-wikiview (atom-id)
  (brain-view-set-context-line 1)
  (issue-request (create-wikiview-request atom-id) 'wikiview-callback))

(defun brain-client-action-dujour ()
  (issue-request action-dujour-request 'ping-callback))

(defun brain-client-ping-server ()
  (issue-request ping-request 'ping-callback))

(defun brain-client-push-treeview ()
  (brain-view-set-context-line)
  (push-treeview))

(defun brain-client-push-wikiview () (push-wikiview))

(defun brain-client-refresh-treeview ()
  (brain-view-set-context-line)
  (issue-request (create-treeview-request (brain-env-context-get 'root-id)) 'treeview-callback))

(defun brain-client-refresh-wikiview ()
  (issue-request (create-wikiview-request (brain-env-context-get 'root-id)) 'wikiview-callback))

(defun brain-client-set-focus-priority (v)
  (if (and (>= v 0) (<= v 1))
      (let ((focus (brain-data-focus)))
        (if focus
            (let (
                  (id (brain-data-atom-id focus)))
              (brain-client-set-property id "priority" v))
          (brain-env-error-no-focus)))
    (error 
     (concat "priority " (number-to-string v) " is outside of range [0, 1]"))))

(defun brain-client-set-focus-sharability (v)
  (if (and (> v 0) (<= v 1))
      (let ((focus (brain-data-focus)))
        (if focus
            (let ((id (brain-data-atom-id focus)))
              (brain-client-set-property id "sharability" v))
          (brain-env-error-no-focus)))
    (error 
     (concat "sharability " (number-to-string v) " is outside of range (0, 1]"))))

(defun brain-client-set-focus-weight (v)
  (if (and (> v 0) (<= v 1))
      (let ((focus (brain-data-focus)))
        (if focus
            (let (
                  (id (brain-data-atom-id focus)))
              (brain-client-set-property id "weight" v))
          (brain-env-error-no-focus)))
    (error 
     (concat "weight " (number-to-string v) " is outside of range (0, 1]"))))

(defun brain-client-set-min-sharability (s)
  (if (and (brain-env-in-setproperties-mode) (>= s 0) (<= s 1))
    (let ()
      (brain-env-context-set 'min-sharability s)
      (brain-client-refresh-treeview))
    (error 
     (concat "min sharability " (number-to-string s) " is outside of range [0, 1]"))))

(defun brain-client-set-min-weight (s)
  (if (and (brain-env-in-setproperties-mode) (>= s 0) (<= s 1))
    (let ()
      (brain-env-context-set 'min-weight s)
      (brain-client-refresh-treeview))
    (error 
     (concat "min weight " (number-to-string s) " is outside of range [0, 1]"))))

(defun brain-client-set-property (id name value)
  (if (brain-env-in-setproperties-mode)
    (set-property-in-treeview id name value)))


(provide 'brain-client)
