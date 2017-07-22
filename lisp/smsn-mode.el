;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; smsn-mode.el -- Semantic Synchrony client and user interface
;; This major mode allows you to view, edit, search, and process a Semantic Synchrony personal knowledge graph.
;;
;; Part of the SmSn-mode package for Emacs:
;;   https://github.com/synchrony/smsn-mode
;;
;; Dependencies:
;;
;;     edebug, goto-addr, indent-guide, json, latex-math-preview, linum, and websocket
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

;; for smsn-mode development
(require 'edebug)

;; for visiting URLs in a browser
(require 'goto-addr)

;; a visual aid to consistent indentation
(require 'indent-guide)

;; for JSON-formatted messages to and from Semantic Synchrony services (see json-read-from-string, json-encode)
(require 'json)

;; for LaTeX views (nice-to-have, but not essential)
(require 'latex-math-preview)

;; for line number annotations in buffers (see linum-mode)
(require 'linum)

;; WebSocket support
(require 'websocket)
(eval-when-compile (require 'cl))


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
  (smsn-commands-define-keymap)
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
  "Convenience function to start SmSn-mode"
  (interactive)
  (smsn-mode))

(provide 'smsn-mode)
