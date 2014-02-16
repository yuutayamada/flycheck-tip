;;; flycheck-tip.el --- show flycheck's error by popup-tip

;; Copyright (C) 2013 by Yuta Yamada

;; Author: Yuta Yamada <cokesboy"at"gmail.com>
;; URL: https://github.com/yuutayamada/flycheck-tip
;; Version: 0.0.1
;; Package-Requires: ((flycheck "0.13") (dash "1.2") (emacs "24.1") (popup "0.5.0"))
;; Keywords: flycheck

;;; License:
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; see readme.md

;;; Code:

(eval-when-compile (require 'cl))
(require 'flycheck)
(require 'error-tip)
(require 'popup)

(defvaralias 'flycheck-tip-timer-delay 'error-tip-timer-delay
  "Alias of `error-tip-timer-delay'.")

(defcustom flycheck-tip-avoid-show-func t
  "Avoid `flycheck-show-error-at-point' function's behavior.
This variable is true by default."
  :group 'flycheck-tip
  :type 'boolean)

;; Error status memo
;; 0 : err name?
;; 1 : buffer
;; 2 : gofmt
;; 3 : file
;; 4 : line
;; 5 : line?
;; 6 : message
;; 7 : err type?

;;;###autoload
(defun flycheck-tip-cycle (&optional reverse)
  "Move to next error if it's exists.
If it wasn't exists then move to previous error.
Move to previous error if REVERSE is non-nil."
  (interactive)
  (error-tip-cycle
   (error-tip-collect-current-file-errors flycheck-current-errors) reverse))

;;;###autoload
(defun flycheck-tip-cycle-reverse ()
  "Do `flycheck-tip-cycle by reverse order."
  (interactive)
  (flycheck-tip-cycle t))

(defadvice flycheck-display-error-at-point
  (around flycheck-tip-avoid-function activate)
  "Avoid flycheck's displaying feature on echo ares if you set non-nil to `flycheck-tip-avoid-show-func'."
  (if flycheck-tip-avoid-show-func
      nil
    ad-do-it))

(defun flycheck-tip-display-current-line-error-message (errors)
  "Show current line's ERRORS by popup."
  (interactive)
  (lexical-let
      ((current-line-errors (-keep #'flycheck-error-message errors)))
    (when current-line-errors
      (popup-tip (format "*%s" (s-join "\n*" current-line-errors))))))

(provide 'flycheck-tip)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; flycheck-tip.el ends here
