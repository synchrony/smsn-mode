;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; smsn-wrapper.el -- Additional functions for use through emacsclient
;;
;; Part of the SmSn-mode package for Emacs:
;;   https://github.com/synchrony/smsn-mode
;;
;; Copyright (C) 2011-2017 Joshua Shinavier and collaborators
;;
;; You should have received a copy of the GNU General Public License
;; along with this software.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun smsn-emacsclient-eval (function)
  "evaluate FUNCTION from emacsclient as if a user had typed it into the current buffer"
  (set-buffer (window-buffer (selected-window)))
  (funcall function))

(defun smsn-previous-line ()
  (interactive)
  (previous-line)
  (emacspeak-speak-line))

(defun smsn-next-line ()
  (interactive)
  (next-line)
  (emacspeak-speak-line))

(defun smsn-backward-char ()
  (interactive)
  (backward-char)
  (emacspeak-speak-display-char t)) ;; PREFIX arg disables phonetic pronunciation

(defun smsn-forward-char ()
  (interactive)
  (forward-char)
  (emacspeak-speak-display-char t)) ;; PREFIX arg disables phonetic pronunciation


(provide 'smsn-wrapper)
