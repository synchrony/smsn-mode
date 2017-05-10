;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; smsn-mode.el -- Semantic Synchrony client and user interface
;; This major mode allows you to view, edit, search, and process a Semantic Synchrony personal knowledge graph.
;;
;; Part of the SmSn-mode package for Emacs:
;;   https://github.com/synchrony/smsn-mode
;;
;; Dependencies:
;;
;;     aes, indent-guide, json, latex-math-preview, linum, and websocket
;;
;; Optional global variables:
;;
;;     smsn-server-host: IP address of Gremlin Server (defaults to "127.0.0.1")
;;     smsn-server-port: listening port of Gremlin Server (defaults to 8182)
;;     smsn-server-protocol: "http" or "websocket" (defaults to "http")
;;
;; See smsn-mode-init-example.el for additional variables with example values.
;;
;; Copyright (C) 2011-2017 Joshua Shinavier and collaborators
;;
;; You should have received a copy of the GNU General Public License
;; along with this software.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; for JSON-formatted messages to and from Semantic Synchrony services (see json-read-from-string, json-encode)
(require 'json)

;; for line number annotations in buffers (see linum-mode)
(require 'linum)

;; for visiting URLs in a browser (see goto-address-at-point)
(require 'goto-addr)

;; for encryption of sensitive values
(require 'aes)

;; for LaTeX views (nice-to-have, but not essential)
(require 'latex-math-preview)

;; a visual aid to consistent indentation
(require 'indent-guide)

;; WebSocket support
(require 'tls)   ;; tests a WebSocket-related bug on emacs 23
(require 'websocket)
(eval-when-compile (require 'cl))

;; for smsn-mode development
(require 'edebug)


(require 'smsn-client)
(require 'smsn-commands)
(require 'smsn-data)
(require 'smsn-env)
(require 'smsn-view)
(require 'smsn-wrapper)

(defvar smsn-mode-syntax-table nil
  "Syntax table used while in smsn-mode.")
(if smsn-mode-syntax-table ()
  (setq smsn-mode-syntax-table (make-syntax-table)))

(defvar smsn-mode-abbrev-table nil
  "Abbrev table used while in smsn-mode.")
(define-abbrev-table 'smsn-mode-abbrev-table ())

(defun set-indent-guide-mode ()
  (indent-guide-mode)
  (setq indent-guide-recursive t)
  (set-face-foreground 'indent-guide-face "gray"))

(defun smsn-mode ()
  "Major mode for interacting with a Semantic Synchrony personal knowledge base"
  (interactive)
  (kill-all-local-variables)
  (use-local-map smsn-mode-map)
  (smsn-env-define-buffer-local-variables)
  (setq local-abbrev-table smsn-mode-abbrev-table)
  (set-syntax-table smsn-mode-syntax-table)
  ;; fetch configuration as early as possible, prior to user input
  (smsn-client-fetch-configuration)
  ;; note: not customizing indent style with indent-line-function
  (setq mode-name "smsn-mode")
  (setq major-mode 'smsn-mode)
  (set-indent-guide-mode)
  (run-hooks 'smsn-hook))
(defun smsn ()
  "Major mode for interacting with a Semantic Synchrony personal knowledge base"
  (interactive)
  (smsn-mode))

(provide 'smsn-mode)
