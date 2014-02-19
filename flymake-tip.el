;;; flymake-tip.el ---

;; Copyright (C) 2014 by Yuta Yamada

;; Author: Yuta Yamada <cokesboy"at"gmail.com>
;; URL: https://github.com/yuutayamada/
;; Version: 0.0.1
;; Package-Requires: ((package "version-number"))
;; Keywords: keyword

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

(eval-when-compile (require 'cl))
(require 'error-tip)
(require 'flymake)

(defun flymake-tip-collect-current-line-errors ()
  (interactive)
  (lexical-let*
      ((current-line (flymake-current-line-no))
       (line-err-info-list
        (nth 0 (flymake-find-err-info flymake-err-info current-line)))
       (menu-data (flymake-make-err-menu-data current-line line-err-info-list)))
    (loop for (err . b) in (cadr menu-data) collect err)))

(defun flymake-tip-cycle (reverse)
  (interactive)
  (if reverse
      (flymake-goto-prev-error)
    (flymake-goto-next-error))
  (error-tip-popup-error-message
   (flymake-tip-collect-current-line-errors)))

(defun flymake-tip-cycle-reverse ()
  (interactive)
  (flymake-tip-cycle t))

(provide 'flymake-tip)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; flymake-tip.el ends here
