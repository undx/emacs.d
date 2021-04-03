;;; misc-defuns.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2021 Emmanuel GALLOIS
;;
;; Author: Emmanuel GALLOIS <emmanuel.gallois@gmail.com>
;; Maintainer: Emmanuel GALLOIS <emmanuel.gallois@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;;
;; Created: 22 Mar 2021
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


;;;; dwim BOL
(defun undx/move-beginning-of-line ()
  "Move to indentation, or beginning of the line."
  (interactive)
  (if (bolp)
      (back-to-indentation)
    (move-beginning-of-line 1)))

;; This bit of code adds a bitmap to the fringe that marks the current mark. Handy when popping off the ring.
;; overlay an arrow where the mark is
(defvar mp-overlay-arrow-position)
(make-variable-buffer-local 'mp-overlay-arrow-position)
(add-to-list 'overlay-arrow-variable-list  'mp-overlay-arrow-position)
(defun mp-mark-hook ()
  ;; (make-local-variable 'mp-overlay-arrow-position)
  (unless (or (minibufferp (current-buffer)) (not (mark)))
    (set
     'mp-overlay-arrow-position
     (save-excursion
       (goto-char (mark))
       (forward-line 0)
       (point-marker)))))
(add-hook 'post-command-hook 'mp-mark-hook)
;;If you want to change the bitmap, defaults to the left arrow, change the attribute for mp-overlay-arrow-position:
(put 'mp-overlay-arrow-position 'overlay-arrow-bitmap 'right-triangle)
;; or you can make a custom one, here’s mine:

;; make the mark fringe bitmap look cool dude
(define-fringe-bitmap 'mp-hollow-right-arrow [128 192 96 48 24 48 96 192 128] 9 8 'center)
(put 'mp-overlay-arrow-position 'overlay-arrow-bitmap 'mp-hollow-right-arrow)



(defun wc-buffer ()
  "Count words in buffer."
  (count-words (point-min) (point-max)))
(defun wc-region (BEGIN END)
  "Count words in region ('BEGIN' to 'END')."
  (count-words-region BEGIN END))
(defun lc-buffer()
  "Count lines in buffer."
  (count-lines (point-min) (point-max)))
(defun lc-region(BEGIN END)
  "Count lines in region ('BEGIN' to 'END')."
  (count-lines BEGIN END))

(defun undx/delete-to-point (POINT)
  "Delete from current point '(point)' to POINT."
  (interactive)
  (delete-region (point) POINT))

(defun undx/delete-to-beginning-of-buffer ()
  "Delete from point to beginning of buffer."
  (interactive)
  (undx/delete-to-point (point-min)))

(defun undx/delete-to-end-of-buffer ()
  "Delete from point to end of buffer."
  (interactive)
  (undx/delete-to-point (point-max)))

(provide 'misc-defuns)
;;; misc-defuns.el ends here
