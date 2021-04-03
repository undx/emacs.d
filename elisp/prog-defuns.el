;;; prog-defuns.el --- prog-mode defuns -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2021 Emmanuel GALLOIS
;;
;; Author: Emmanuel GALLOIS <emmanuel.gallois@gmail.com>
;; Maintainer: Emmanuel GALLOIS <emmanuel.gallois@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;;
;; Created: 29 Mar 2021
;;
;; URL: https://github.com/undx/
;;
;; License: GPLv3
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <http://www.gnu.org/licenses/>.
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

;;; java
;; https://snarfed.org/java-stack-traces-in-emacs-compilation-mode
(defvar java-stack-trace-dir "src/")
(defun java-stack-trace-regexp-to-filename ()
  "Generate a relative filename from java-stack-trace regexp match data."
  (concat java-stack-trace-dir
          (replace-regexp-in-string "\\." "/" (match-string 1))
          (match-string 2)))

(add-to-list 'compilation-error-regexp-alist 'java-stack-trace)
(add-to-list 'compilation-error-regexp-alist-alist
             '(java-stack-trace .
                                ("^[[:space:]]*at \\(\\(?:[[:lower:]]+\\.\\)+\\)[^(]+(\\([[:alnum:]]+\\.java\\):\\([[:digit:]]+\\))"
                                 java-stack-trace-regexp-to-filename 3)))

;; comment to check
;; (add-to-list
;;  ‘compilation-error-regexp-alist-alist
;;  ‘(java-stack-trace .
;;                     (“at \\(\\(?:[[:alnum:]]+\\.\\)+\\)+[[:alnum:]]+\\.[[:alnum:]]+(\\([[:alnum:]]+\\.java\\):\\([[:digit:]]+\\))$”
;;                          java-stack-trace-regexp-to-filename 3)))


(provide 'prog-defuns)
;;; prog-defuns.el ends here
