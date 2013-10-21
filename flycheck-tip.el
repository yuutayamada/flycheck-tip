;;; flycheck-tip.el --- show flycheck's error by popup-tip

;; Copyright (C) 2013 by Yuta Yamada

;; Author: Yuta Yamada <cokesboy"at"gmail.com>
;; URL: https://github.com/yuutayamada/flycheck-tip
;; Version: 0.0.1
;; Package-Requires: ((flycheck "20131020) (popup "20130708"))
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

(eval-when-compile (require 'cl))
(require 'flycheck)
(require 'popup)

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
(defun flycheck-tip-cycle ()
  (interactive)
  (lexical-let*
      ((next     (flycheck-tip-collect-current-file-errors :next))
       (previous (unless next
                   (flycheck-tip-collect-current-file-errors :previous)))
       (jump (lambda (direction errors)
               (case direction
                 (:next     (flycheck-next-error))
                 (:previous (flycheck-previous-error)))
               (flycheck-tip-popup-error-message errors))))
    (if next
        (funcall jump :next next)
      (if previous
          (funcall jump :previous previous)))))

(when flycheck-tip-avoid-show-func
  (defadvice flycheck-show-error-at-point
    (around flycheck-tip-avoid-function activate)
    nil))

(defun flycheck-tip-collect-current-file-errors (&optional direction)
  (loop with errors   = flycheck-current-errors
        with next     = '()
        with previous = '()
        for err in errors
        for line = (elt err 4)
        if (and
            (equal (expand-file-name buffer-file-truename)
                   (elt err 3))
            (case direction
              (:previous (> (line-number-at-pos (point)) line))
              (:next     (< (line-number-at-pos (point)) line))
              (t         t)))
        collect err))

(defun flycheck-tip-popup-error-message (errors)
  (loop for error in errors
        for line = (elt error 4)
        if (and
            (equal (line-number-at-pos (point)) line)
            (equal (expand-file-name buffer-file-truename) (elt error 3)))
        do (popup-tip (elt error 6))))

(provide 'flycheck-tip)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; flycheck-tip.el ends here
