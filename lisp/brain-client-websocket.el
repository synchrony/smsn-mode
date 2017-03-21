;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; brain-client-websocket.el -- WebSocket client for Semantic Synchrony
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
(require 'brain-serde)


;;(setq websocket-debug t)

(setq websocket-connection nil)
(setq websocket-response-handler (lambda (response)
  (message (concat "server response: " response))))

(defun websocket-connection-is-open ()
  (and websocket-connection (websocket-openp websocket-connection)))

(defun get-websocket-connection (host port)
  (if (not (websocket-connection-is-open))
    (progn
      (setq websocket-connection (create-websocket-connection host port))
      (sleep-for 0.1)))
  websocket-connection)

(defun create-websocket-connection (host port)
  (message "opening connection")
  (websocket-open
    (websocket-connection-url host port)
    :on-message
      (lambda (_websocket frame)
        (let ((response (websocket-frame-text frame)))
          (funcall websocket-response-handler response)))
    :on-close
      (lambda (_websocket) (message "WebSocket connection closed"))))

(defun websocket-connection-url (host port)
  (concat "ws://" host ":" (number-to-string port) "/gremlin"))

(defun brain-client-websocket-send-and-receive (host port request callback)
  ;;(message  (concat "context: " (json-encode context)))
  ;;(setq websocket-response-handler ...)
  (let ((connection (get-websocket-connection host port)))
    (if (websocket-connection-is-open)
      (let ((payload (brain-serde-format-request request)))
        (setq websocket-response-handler
          (lexical-let (
            (callback callback)
            (context (copy-alist (brain-env-get-context))))
              (lambda (response)
                (brain-serde-handle-response response callback context))))
        (websocket-send-text connection payload))
      (error "WebSocket connection could not be opened"))))


(provide 'brain-client-websocket)
