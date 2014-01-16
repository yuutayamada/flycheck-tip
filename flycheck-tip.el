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
(require 'popup)

(defcustom flycheck-tip-avoid-show-func t
  "Avoid `flycheck-show-error-at-point' function's behavior.
This variable is true by default."
  :group 'flycheck-tip
  :type 'boolean)

(defvar flycheck-tip-popup-object nil)

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
  (flycheck-tip-delete-popup)
  (when flycheck-current-errors
    (lexical-let*
        ((errors   (flycheck-tip-collect-current-file-errors))
         (next     (assoc-default :next         errors))
         (previous (assoc-default :previous     errors))
         (cur-line (assoc-default :current-line errors))
         (jump (lambda (errs)
                 (goto-char (point-min))
                 (forward-line (1- (elt (car errs) 4)))
                 (flycheck-tip-popup-error-message errs)))
         (target (if (not reverse)
                     (or next previous cur-line)
                   (reverse (or previous next cur-line)))))
      (funcall jump target))))

;;;###autoload
(defun flycheck-tip-cycle-reverse ()
  "Do `flycheck-tip-cycle by reverse order."
  (interactive)
  (flycheck-tip-cycle t))

(when flycheck-tip-avoid-show-func
  (defadvice flycheck-show-error-at-point
    (around flycheck-tip-avoid-function activate)
    nil))

(defun flycheck-tip-collect-current-file-errors ()
  "Collect errors from `flycheck-current-errors'."
  (loop with errors = flycheck-current-errors
        with next and previous and current-line
        for err in errors
        for err-line = (elt err 4)
        for c-line   = (line-number-at-pos (point))
        if (and buffer-file-truename ; whether file or buffer
                (not (equal (expand-file-name buffer-file-truename)
                            (elt err 3))))
        do '() ; skip
        else if (< c-line err-line)
        collect err into next
        else if (> c-line err-line)
        collect err into previous
        else if (= c-line err-line)
        collect err into current-line
        finally return (list (cons :next         next)
                             (cons :previous     previous)
                             (cons :current-line current-line))))

(defun flycheck-tip-popup-error-message (errors)
  "Popup error message(s) from ERRORS.
If there are multiple errors on current line, all current line's errors are
appered."
  (lexical-let
      ((line-errors (loop with result and fallback
                          with current-line = (line-number-at-pos (point))
                          for error in errors
                          for e-line = (elt error 4)
                          if (equal current-line e-line)
                          collect (elt error 6) into result
                          else if (and (< (- 1 current-line) e-line)
                                       (> (+ 1 current-line) e-line))
                          collect (elt error 6) into fallback
                          finally return (or result fallback))))
    (setq flycheck-tip-popup-object
          (popup-tip (format "*%s" (mapconcat 'identity line-errors "\n*"))
                     :nowait t))
    (add-hook 'pre-command-hook 'flycheck-tip-delete-popup)))

(defun flycheck-tip-delete-popup ()
  (condition-case err
      (when (popup-live-p flycheck-tip-popup-object)
        (popup-delete flycheck-tip-popup-object))
    (error err))
  (remove-hook 'pre-command-hook 'flycheck-tip-delete-popup))

(provide 'flycheck-tip)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; flycheck-tip.el ends here
