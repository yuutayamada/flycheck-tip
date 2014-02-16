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

(defvar flycheck-tip-timer-delay 0.3
  "Whether how much delay showing error popup.
If you set nil to this variable, then do not use delay timer.")

;; Error status memo
;; 0 : err name?
;; 1 : buffer
;; 2 : gofmt
;; 3 : file
;; 4 : line
;; 5 : line?
;; 6 : message
;; 7 : err type?

;; TODO: move error-tip-* functions to error-tip.el
;; INTERNAL VARIABLE
(defvar error-tip-popup-object nil)
(defvar error-tip-timer-object nil)
(defvar error-tip-current-errors nil)

(defun error-tip-cycle (errors &optional reverse)
  (error-tip-delete-popup)
  (when errors
    (lexical-let*
        ((next     (assoc-default :next         errors))
         (previous (assoc-default :previous     errors))
         (cur-line (assoc-default :current-line errors))
         (jump (lambda (errs)
                 (goto-char (point-min))
                 (forward-line (1- (elt (car errs) 4)))
                 (setq error-tip-current-errors errs)
                 (if (null flycheck-tip-timer-delay)
                     (error-tip-popup-error-message (error-tip-get-errors))
                   (flycheck-tip-cancel-timer)
                   (flycheck-tip-register-timer))))
         (target (if (not reverse)
                     (or next previous cur-line)
                   (reverse (or previous next cur-line)))))
      (funcall jump target))))

(defun error-tip-get (err element)
  (cond
   ((bound-and-true-p flycheck-mode)
    (case element
      (line    (elt err 4))
      (file    (elt err 3))
      (message (elt err 6))))
   ((bound-and-true-p eclim-mode)
    (case element
      (line    (assoc-default 'line     err))
      (file    (assoc-default 'filename err))
      (message (assoc-default 'message  err))))))

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

(defun error-tip-collect-current-file-errors (errors)
  "Collect errors from ERRORS."
  (loop with c-line = (line-number-at-pos (point))
        with next and previous and current-line
        for err in errors
        for err-line = (error-tip-get err 'line)
        if (and buffer-file-truename ; whether file or buffer
                (not (equal (expand-file-name buffer-file-truename)
                            (error-tip-get err 'file))))
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

(defun error-tip-popup-error-message (errors)
  "Popup error message(s) from ERRORS.
If there are multiple errors on current line, all current line's errors are
appeared."
  (setq error-tip-popup-object
        (popup-tip
         (format "*%s" (mapconcat 'identity errors "\n*")) :nowait t))
  (add-hook 'pre-command-hook 'error-tip-delete-popup))

(defun error-tip-get-errors ()
  "Get errors."
  (loop with result and fallback
        with current-line = (line-number-at-pos (point))
        for error in error-tip-current-errors
        for e-line = (error-tip-get error 'line)
        for e-str  = (error-tip-get error 'message)
        if (or (equal current-line e-line)
               (and (equal 1 current-line)
                    (equal 0 e-line)))
        collect e-str into result
        else if (and (< (- 1 current-line) e-line)
                     (> (+ 1 current-line) e-line))
        collect e-str into fallback
        finally return (or result fallback)))

(defun flycheck-tip-register-timer ()
  "Register timer that show error message."
  (setq error-tip-timer-object
        (run-with-timer flycheck-tip-timer-delay nil
                        (lambda ()
                          (error-tip-popup-error-message (error-tip-get-errors))))))

(defun flycheck-tip-cancel-timer ()
  "Cancel `error-tip-timer-object'."
  (when (timerp error-tip-timer-object)
    (cancel-timer error-tip-timer-object)))

(defun error-tip-delete-popup ()
  "Delete popup object."
  (condition-case err
      (when (popup-live-p error-tip-popup-object)
        (popup-delete error-tip-popup-object))
    (error err))
  (remove-hook 'pre-command-hook 'error-tip-delete-popup))

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
