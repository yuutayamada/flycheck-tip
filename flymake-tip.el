;;; flymake-tip.el --- show flymake's error by popup-tip -*- lexical-binding: t; -*-

;; Copyright (C) 2014 by Yuta Yamada

;; Author: Yuta Yamada <cokesboy"at"gmail.com>
;; URL: https://github.com/yuutayamada/flycheck-tip
;; Version: 0.5.0
;; Package-Requires: ((emacs "24.1"))
;; Keywords: flymake

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

;;; Code:

(require 'error-tip)
(require 'flymake)
(require 'cl-lib)
(require 'flymake)

(cond
 ((version<= "26" (number-to-string emacs-major-version))
  (defun flymake-tip-get-diag-text-on-line ()
    "Return list of string of error/warning info on the current line."
    (cl-loop for ov in (flymake--overlays :beg (point-at-bol) :end (point-at-eol))
             collect (flymake--diag-text (overlay-get ov 'flymake--diagnostic)))))
 ;; Old implementation for emacs-major-version < 26
 ((fboundp 'flymake-find-err-info)
  (defun flymake-tip-get-error-info-on-line-old ()
    (cl-loop with line-err-info = (flymake-find-err-info
                                   (bound-and-true-p flyamke-err-info) (line-number-at-pos))
             for err in (car line-err-info)
             if (vectorp err)
             collect (elt err 4)))))

(defvar flymake-tip--err-info-function
  (if (version<= "26" (number-to-string emacs-major-version))
      'flymake-tip-get-diag-text-on-line
    'flymake-tip-get-error-info-on-line-old))

;;;###autoload
(defun flymake-tip-cycle (reverse)
  (interactive)
  (if reverse
      (flymake-goto-prev-error)
    (flymake-goto-next-error))
  (error-tip-popup-error-message
   (funcall flymake-tip--err-info-function)))

;;;###autoload
(defun flymake-tip-cycle-reverse ()
  (interactive)
  (flymake-tip-cycle t))

(provide 'flymake-tip)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; flymake-tip.el ends here
