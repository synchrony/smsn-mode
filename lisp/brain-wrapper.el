;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; brain-wrapper.el -- Additional functions for use through emacsclient
;;
;; Part of the Brain-mode package for Emacs:
;;   https://github.com/joshsh/brain-mode
;;
;; Copyright (C) 2011-2016 Joshua Shinavier and collaborators
;;
;; You should have received a copy of the GNU General Public License
;; along with this software.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun brain-emacsclient-eval (function)
  "evaluate FUNCTION from emacsclient as if a user had typed it into the current buffer"
  (set-buffer (window-buffer (selected-window)))
  (funcall function))

(defun brain-previous-line ()
  (interactive)
  (previous-line)
  (emacspeak-speak-line))

(defun brain-next-line ()
  (interactive)
  (next-line)
  (emacspeak-speak-line))

(defun brain-backward-char ()
  (interactive)
  (backward-char)
  (emacspeak-speak-display-char t)) ;; PREFIX arg disables phonetic pronunciation

(defun brain-forward-char ()
  (interactive)
  (forward-char)
  (emacspeak-speak-display-char t)) ;; PREFIX arg disables phonetic pronunciation


(provide 'brain-wrapper)
