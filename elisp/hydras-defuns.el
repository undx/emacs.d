;;; hydras-defuns.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2021 Emmanuel GALLOIS
;;
;; Author: Emmanuel GALLOIS <emmanuel.gallois@gmail.com>
;; Maintainer: Emmanuel GALLOIS <emmanuel.gallois@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;;
;; Created: 18 Mar 2021
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
;;  Inspirated by https://gist.github.com/dfeich/1df4e174d45f05fb5798ca514d28c68a
;;; Code:

;;; ** Hydras
;;; *** context launcher for hydras
(defun dfeich/context-hydra-launcher ()
  "A launcher for hydras based on the current context."
  (interactive)
  (message "Launching hydra for major mode: %s" major-mode)
  (cl-case major-mode
    ('org-mode (let* ((elem (org-element-context))
		      (etype (car elem))
		      (type (org-element-property :type elem)))
		 (cl-case etype
		   (src-block (hydra-babel-helper/body))
		   (link (hydra-org-link-helper/body))
		   ((table-row table-cell) (hydra-org-table-helper/body) )
		   (t (message "No specific hydra for %s/%s" etype type)
		      (hydra-org-default/body))))
	       )
    ('bibtex-mode (org-ref-bibtex-hydra/body))
    ('ibuffer-mode (hydra-ibuffer-main/body))
    (t (message "No hydra for this major mode: %s" major-mode))))

(global-set-key (kbd "<f9> <f9>") 'dfeich/context-hydra-launcher)
(global-set-key (kbd "C-<") 'dfeich/context-hydra-launcher)

;;; *** org mode hydras
(defhydra hydra-org-default (:color pink :hint nil)
  "
Org default hydra

_l_ insert template from last src block
_s_ insert src block ref with helm

_q_ quit
"
  ("l" dfeich/copy-last-src-block-head :color blue)
  ("s" helm-lib-babel-insert :color blue)
  ("q" nil :color blue))


(defhydra hydra-org-link-helper (:color pink :hint nil)
  "
org link helper
_i_ backward slurp     _o_ forward slurp    _n_ next link
_j_ backward barf      _k_ forward barf     _p_ previous link
_t_ terminal at path

_q_ quit
"
  ("i" org-link-edit-backward-slurp)
  ("o" org-link-edit-forward-slurp)
  ("j" org-link-edit-backward-barf)
  ("k" org-link-edit-forward-barf)
  ("n" org-next-link)
  ("p" org-previous-link)
  ("t" dfeich/gnome-terminal-at-link :color blue)
  ("q" nil :color blue))

(defhydra hydra-org-table-helper (:color pink :hint nil)
  "
org table helper
_r_ recalculate     _w_ wrap region      _c_ toggle coordinates
_i_ iterate table   _t_ transpose        _D_ toggle debugger
_B_ iterate buffer  _E_ export table     _n_ remove number separators
_e_ eval formula    _s_ sort lines       _d_ edit field

_q_ quit
"
  ("E" org-table-export :color blue)
  ("s" org-table-sort-lines)
  ("d" org-table-edit-field)
  ("e" org-table-eval-formula)
  ("r" org-table-recalculate)
  ("i" org-table-iterate)
  ("B" org-table-iterate-buffer-tables)
  ("w" org-table-wrap-region)
  ("D" org-table-toggle-formula-debugger)
  ("t" org-table-transpose-table-at-point)
  ("n" dfeich/org-table-remove-num-sep :color blue)
  ("c" org-table-toggle-coordinate-overlays :color blue)
  ("q" nil :color blue))

(defhydra hydra-babel-helper (:color pink :hint nil)
  "
org babel src block helper functions
_n_ next       _i_ info           _I_ insert header
_p_ prev       _c_ check
_h_ goto head  _E_ expand
^ ^            _s_ split
_q_ quit       _r_ remove result  _e_ examplify region

"
  ("i" org-babel-view-src-block-info)
  ("I" org-babel-insert-header-arg)
  ("c" org-babel-check-src-block :color blue)
  ("s" org-babel-demarcate-block :color blue)
  ("n" org-babel-next-src-block)
  ("p" org-babel-previous-src-block)
  ("E" org-babel-expand-src-block :color blue)
  ("e" org-babel-examplify-region :color blue)
  ("r" org-babel-remove-result :color blue)
  ("h" org-babel-goto-src-block-head)
  ("q" nil :color blue))
(global-set-key (kbd "<f9> b") 'hydra-babel-helper/body)


;;; *** window management hydra
(defun shrink-frame-horizontally (&optional increment)
  (interactive "p")
  (let ((frame (window-frame)))
    (set-frame-width frame (- (frame-width frame) increment))))
(defun enlarge-frame-horizontally (&optional increment)
  (interactive "p")
  (let ((frame (window-frame)))
    (set-frame-width frame (+ (frame-width frame) increment))))
(defun shrink-frame-vertically (&optional increment)
  (interactive "p")
  (let ((frame (window-frame)))
    (set-frame-height frame (- (frame-height frame) increment))))
(defun enlarge-frame-vertically (&optional increment)
  (interactive "p")
  (let ((frame (window-frame)))
    (set-frame-height frame (+ (frame-height frame) increment))))

(defun shift-frame-right (&optional increment)
  (interactive "P")
  (unless increment (setq increment 20))
  (set-frame-parameter nil 'left (+ (frame-parameter nil 'left) increment)))
(defun shift-frame-left (&optional increment)
  (interactive "P")
  (unless increment (setq increment 20))
  (message "user-pos:" (frame-parameter nil 'user-position))
  (set-frame-parameter nil 'left (- (frame-parameter nil 'left) increment)))
(defun shift-frame-up (&optional increment)
  (interactive "P")
  (unless increment (setq increment 20))
  (set-frame-parameter nil 'top (- (frame-parameter nil 'top) increment)))
(defun shift-frame-down (&optional increment)
  (interactive "P")
  (unless increment (setq increment 20))
  (set-frame-parameter nil 'top (+ (frame-parameter nil 'top) increment)))

(defhydra hydra-window-mngm (:color pink :hint nil)
  "
frame    ^ ^ _m_ ^ ^    frame   ^ ^ _z_ ^ ^    window   ^ ^ _w_
sizing:  _j_ ^ ^ _k_    moving: _g_ ^ ^ _h_    sizing:  _a_ ^ ^ _s_
         ^ ^ _i_ ^ ^            ^ ^ _b_ ^ ^             ^ ^ _y_
"
  ("a" shrink-window-horizontally)
  ("s" enlarge-window-horizontally)
  ("y" enlarge-window)
  ("w" shrink-window)
  ("j" shrink-frame-horizontally)
  ("k" enlarge-frame-horizontally)
  ("i" shrink-frame-vertically)
  ("m" enlarge-frame-vertically)
  ("g" shift-frame-left)
  ("h" shift-frame-right)
  ("z" shift-frame-up)
  ("b" shift-frame-down)
  ("t" dfeich/toggle-window-split "toggle windows splitting")
  ("n" dfeich/open-buffer-in-new-frame "open buffer in new frame" :color blue)
  ("q" nil "quit" :color blue))

(global-set-key (kbd "C-c w") 'hydra-window-mngm/body)

;;; *** ibuffer hydra
;; from https://github.com/abo-abo/hydra/wiki/Ibuffer
(defhydra hydra-ibuffer-main (:color pink :hint nil)
  "
 ^Navigation^ | ^Mark^        | ^Actions^        | ^View^
-^----------^-+-^----^--------+-^-------^--------+-^----^-------
  _k_:    ʌ   | _m_: mark     | _D_: delete      | _g_: refresh
 _RET_: visit | _u_: unmark   | _S_: save        | _s_: sort
  _j_:    v   | _*_: specific | _a_: all actions | _/_: filter
-^----------^-+-^----^--------+-^-------^--------+-^----^-------
"
  ("j" ibuffer-forward-line)
  ("RET" ibuffer-visit-buffer :color blue)
  ("k" ibuffer-backward-line)

  ("m" ibuffer-mark-forward)
  ("u" ibuffer-unmark-forward)
  ("*" hydra-ibuffer-mark/body :color blue)

  ("D" ibuffer-do-delete)
  ("S" ibuffer-do-save)
  ("a" hydra-ibuffer-action/body :color blue)

  ("g" ibuffer-update)
  ("s" hydra-ibuffer-sort/body :color blue)
  ("/" hydra-ibuffer-filter/body :color blue)

  ("o" ibuffer-visit-buffer-other-window "other window" :color blue)
  ("q" ibuffer-quit "quit ibuffer" :color blue)
  ("." nil "toggle hydra" :color blue))

(defhydra hydra-ibuffer-mark (:color teal :columns 5
				     :after-exit (hydra-ibuffer-main/body))
  "Mark"
  ("*" ibuffer-unmark-all "unmark all")
  ("M" ibuffer-mark-by-mode "mode")
  ("m" ibuffer-mark-modified-buffers "modified")
  ("u" ibuffer-mark-unsaved-buffers "unsaved")
  ("s" ibuffer-mark-special-buffers "special")
  ("r" ibuffer-mark-read-only-buffers "read-only")
  ("/" ibuffer-mark-dired-buffers "dired")
  ("e" ibuffer-mark-dissociated-buffers "dissociated")
  ("h" ibuffer-mark-help-buffers "help")
  ("z" ibuffer-mark-compressed-file-buffers "compressed")
  ("b" hydra-ibuffer-main/body "back" :color blue))

(defhydra hydra-ibuffer-action (:color teal :columns 4
				       :after-exit
				       (if (eq major-mode 'ibuffer-mode)
					   (hydra-ibuffer-main/body)))
  "Action"
  ("A" ibuffer-do-view "view")
  ("E" ibuffer-do-eval "eval")
  ("F" ibuffer-do-shell-command-file "shell-command-file")
  ("I" ibuffer-do-query-replace-regexp "query-replace-regexp")
  ("H" ibuffer-do-view-other-frame "view-other-frame")
  ("N" ibuffer-do-shell-command-pipe-replace "shell-cmd-pipe-replace")
  ("M" ibuffer-do-toggle-modified "toggle-modified")
  ("O" ibuffer-do-occur "occur")
  ("P" ibuffer-do-print "print")
  ("Q" ibuffer-do-query-replace "query-replace")
  ("R" ibuffer-do-rename-uniquely "rename-uniquely")
  ("T" ibuffer-do-toggle-read-only "toggle-read-only")
  ("U" ibuffer-do-replace-regexp "replace-regexp")
  ("V" ibuffer-do-revert "revert")
  ("W" ibuffer-do-view-and-eval "view-and-eval")
  ("X" ibuffer-do-shell-command-pipe "shell-command-pipe")
  ("b" nil "back"))

(defhydra hydra-ibuffer-sort (:color amaranth :columns 3)
  "Sort"
  ("i" ibuffer-invert-sorting "invert")
  ("a" ibuffer-do-sort-by-alphabetic "alphabetic")
  ("v" ibuffer-do-sort-by-recency "recently used")
  ("s" ibuffer-do-sort-by-size "size")
  ("f" ibuffer-do-sort-by-filename/process "filename")
  ("m" ibuffer-do-sort-by-major-mode "mode")
  ("b" hydra-ibuffer-main/body "back" :color blue))

(defhydra hydra-ibuffer-filter (:color amaranth :columns 4)
  "Filter"
  ("m" ibuffer-filter-by-used-mode "mode")
  ("M" ibuffer-filter-by-derived-mode "derived mode")
  ("n" ibuffer-filter-by-name "name")
  ("c" ibuffer-filter-by-content "content")
  ("e" ibuffer-filter-by-predicate "predicate")
  ("f" ibuffer-filter-by-filename "filename")
  (">" ibuffer-filter-by-size-gt "size")
  ("<" ibuffer-filter-by-size-lt "size")
  ("/" ibuffer-filter-disable "disable")
  ("b" hydra-ibuffer-main/body "back" :color blue))

(define-key ibuffer-mode-map "." 'hydra-ibuffer-main/body)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; *** misc hydras
;; an analog of the earlier toggle map I used

;; from a Gist by Michael Fogleman
;; https://gist.github.com/mwfogleman/95cc60c87a9323876c6c
;; even though I am pretty comfortable with the C-x n n / C-x n w
;; default keys settings
(defun narrow-or-widen-dwim (p)
  "If the buffer is narrowed, it widens. Otherwise, it narrows intelligently.
Intelligently means: region, subtree, or defun, whichever applies
first.

With prefix P, don't widen, just narrow even if buffer is already
narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning) (region-end)))
        ((derived-mode-p 'org-mode) (org-narrow-to-subtree))
        (t (narrow-to-defun))))
;; (define-key my-toggle-map "n" 'narrow-or-widen-dwim)

(autoload 'dired-toggle-read-only "dired" nil t)

(defhydra hydra-toggle-map (:color blue)
  "toggle map"
  ("c" column-number-mode "colnum")
  ("d" toggle-debug-on-error "debug-on-error")
  ("f" auto-fill-mode "auto fill")
  ("l" toggle-truncate-lines "truncate-lines")
  ("q" toggle-debug-on-quit "debug on quit")
  ("r" dired-toggle-read-only "read-only")
  ("n" narrow-or-widen-dwim "narrow/widen")
  )
(global-set-key (kbd "C-x t") 'hydra-toggle-map/body)


(provide 'hydras-defuns)
;;; hydras-defuns.el ends here
