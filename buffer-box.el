;;; buffer-box.el --- Borders around buffers     -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Nicolas P. Rougier (inria)
;; URL: https://github.com/rougier/buffer-box
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, mode-line, header-line

;; Author: Nicolas P. Rougier (inria) <nicolas.rougier@inria.fr>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package allows to surround a buffer with a one character border
;; on all sides.  This works on both graphic or terminal modes and takes
;; advantage of margins, header-line, tab-line, mode-line, line-prefix
;; and wrap-prefix.

;;;; Limitations

;;  If you display the same buffer in two different windows, the side
;;  borders will be either active or inactive in both windows (while
;;  the tab-line, header-line and mode-line should be ok).  The reason
;;  is that line-prefix and wrap-prefix are attached to a buffer, not
;;  a window.  I don't see any way to fix it.

;;  If you have some text that has a display property spanning several
;;  lines, you'll see blanks on the sides.  The reason is that the
;;  displayed property does not include the proper line-prefix /
;;  wrap-prefix from the main buffer.  One way to fix it is to include
;;  a dynamic line-prefix / wrap-prefix inside your displayed text
;;  with the proper active/inactive glyph.

;;;; Example usage

;; Immediate border toggling (using default header)
;; (buffer-box)

;; Bound a key to toggle it
;; (bind-key "C-c b" #'buffer-box)
;; (bind-key "C-=" #'buffer-box)

;;; Code:

(defgroup buffer-box nil
  "Buffer-box customization group."
  :group 'convenience)

(defconst buffer-box--border-styles
  '((default . ((TL . "┌") (TR . "┐") (BL . "└")
                (BR . "┘") (V  . "│") (H  . "─")))
    (dash    . ((TL . "┌") (TR . "┐") (BL . "└")
                (BR . "┘") (V  . "╎") (H  . "╴")))
    (bold    . ((TL . "┏") (TR . "┓") (BL . "┗")
                (BR . "┛") (V  . "┃") (H  . "━")))
    (double  . ((TL . "╔") (TR . "╗") (BL . "╚")
                 (BR . "╝") (V  . "║") (H  . "═")))
    (corner-single  . ((TL . "┏") (TR . "┓") (BL . "┗")
                       (BR . "┛") (V  . "│") (H  . "─")))
    (corner-double  . ((TL . "╔") (TR . "╗") (BL . "╚")
                       (BR . "╝") (V  . "│") (H  . "─"))))
  "Available styles for the boxes.")

(defcustom buffer-box-style-active 'default
  "Styles for active buffer."
  :type
  (let ((choices (mapcar (lambda (s) `(const ,s))
                         (mapcar #'car buffer-box--border-styles))))
    `(choice  ,@choices)))

(defcustom buffer-box-style-inactive 'default
  "Styles for inactive buffers."
  :type
  (let ((choices (mapcar (lambda (s) `(const ,s))
                         (mapcar #'car buffer-box--border-styles))))
    `(choice  ,@choices)))

(defface buffer-box-face-active
  `((t ( :family     ,(face-attribute 'default :family)
         :foreground ,(face-foreground 'default nil 'default)
         :background ,(face-background 'default nil 'default)
         
         :inherit default)))
  "Face for active buffer.")

(defface buffer-box-face-inactive
  `((t ( :family     ,(face-attribute 'default :family)
         :foreground ,(face-foreground 'font-lock-comment-face nil 'default)
         :background ,(face-background 'font-lock-comment-face nil 'default)
         :inherit default)))
  "Face for inactive buffers.")

(defun buffer-box--border (location &optional active)
  "Return a propertized glyph for the given LOCATION.
Face depends on the ACTIVE status."
  (let ((face (if active
                  'buffer-box-face-active
                'buffer-box-face-inactive)))
    (propertize
     (alist-get location
                (alist-get
                 (if active
                     buffer-box-style-active
                   buffer-box-style-inactive)
                 buffer-box--border-styles))
     'face face)))

(defun buffer-box--overlay ()
  "Return the buffer box overlay (if any) on the current buffer."
  (let ((existing nil))
    (dolist (overlay (overlays-in (point-min) (point-max)))
      (when (overlay-get overlay 'buffer-box)
        (setq existing overlay)))
    existing))

(defvar buffer-box--last-selected (make-hash-table :test 'eq)
  "Hash table mapping frames to their last selected window.")

(defvar buffer-box--windows (window-list)
  "List of windows currently known.")

(defun buffer-box--track-windows (&rest _args)
  "Detect newly created windows."
  (let ((current-windows (window-list)))
    (dolist (window current-windows)
      (unless (memq window buffer-box--windows)
        (if (eq window (selected-window))
            (buffer-box--side-border (window-buffer window) t)
          (buffer-box--side-border (window-buffer window) nil))))
    (setq buffer-box--windows current-windows)))

(defun buffer-box--selection-change (&optional frame)
  "Track which window gained or lost focus in FRAME."
  (let* ((frame (or frame (selected-frame)))
         (previous (gethash frame buffer-box--last-selected))
         (current (selected-window)))
    (unless (or (eq current previous)
                (eq current (minibuffer-window)))
      (when previous
        (buffer-box--side-border (window-buffer previous)))
      (when current
        (buffer-box--side-border (window-buffer current) t))
      (puthash frame current buffer-box--last-selected))))

(defun buffer-box-header (&optional prefix name mode info prefix-color)
  "The default header line that overlaps the left border (on purpose).

It composed of a PREFIX, NAME and MODE on the left and INFO on the
right.  The prefix can be colored using the provided PREFIX-COLOR (as
background color)."

  (let* ((active (mode-line-window-selected-p))
         (prefix (or prefix (cond (buffer-read-only    " RO ")
                                  ((buffer-modified-p) " ** ")
                                  (t                   " RW "))))
         (name (or name (format-mode-line " %b ")))
         (mode (or mode (format "(%s mode)" (format-mode-line mode-name))))
         (info (or info (format-mode-line " %c:%l ")))
         (dedicated  (if (window-dedicated-p) "● " ""))
         (face-read-only (list :foreground (face-background 'default nil 'default)
                               :background (face-foreground 'default nil 'default)
                               :inherit 'bold))
         (face-read-write (list :foreground (face-background 'link nil 'default)
                                :background (face-foreground 'link nil 'default)
                                :inherit 'bold))
         (face-modified  (list :foreground (face-background 'warning nil 'default)
                               :background (face-foreground 'warning nil 'default)
                               :inherit 'bold))         
         (face-inactive-i (list :foreground (face-background 'shadow nil 'default)
                                :background (face-foreground 'shadow nil 'default)))
         (face-active-default (list :foreground (face-foreground 'default nil 'default)
                                    :background (face-background 'default nil 'default)))
         (face-inactive-default (list :foreground (face-foreground 'shadow nil 'default)
                                      :background (face-background 'shadow nil 'default)))
         (face-active-bold (list :foreground (face-foreground 'link nil 'default)
                                 :background (face-background 'link nil 'default)
                                 :inherit 'bold))
         (face-inactive-bold (list :foreground (face-foreground 'shadow nil 'default)
                                   :background (face-background 'shadow nil 'default)
                                   :inherit 'bold))
         (face-active-shadow (list :foreground (face-foreground 'shadow nil 'default)
                                   :background (face-background 'shadow nil 'default)))
         (face-inactive-shadow (list :foreground (face-foreground 'shadow nil 'default)
                                     :background (face-background 'shadow nil 'default)))
         (face-prefix (cond ((not active) face-inactive-i)
                            (prefix-color (list :foreground (face-background 'default)
                                                :background prefix-color
                                                :inherit 'bold))
                            (buffer-read-only    face-read-only)
                            ((buffer-modified-p) face-modified)
                            (t                   face-read-write)))
         (face-name (if active face-active-bold face-inactive-bold))
         (face-mode (if active face-active-default face-inactive-default))
         (face-info (if active face-active-shadow face-inactive-shadow))
         (prefix (propertize prefix 'face face-prefix))
         (name (propertize name 'face face-name))
         (mode (propertize mode 'face face-mode))
         (info (concat (propertize info 'face face-info)
                       (propertize dedicated 'face face-info)))
         (border (buffer-box--border 'V active))
         (spacing (propertize " "
                              'display `(space :align-to (- scroll-bar
                                                            ,(1+ (length info))))
                              'face 'default)))
    (list prefix name mode spacing info border)))

(defun buffer-box--top-border ()
  "A regular top border string."
  (let* ((active (mode-line-window-selected-p))
         (margins (window-margins))
         (width (+ (window-width)
                   -2
                   (or (car margins) 0)
                   (or (cdr margins) 0)))
         (face (if active
                   'buffer-box-face-active
                 'buffer-box-face-inactive)))
    (propertize
     (concat (buffer-box--border 'TL active)
             (make-string width (aref (buffer-box--border 'H active) 0))
             (buffer-box--border 'TR active)
             " ")
     'face face)))

(defun buffer-box--bottom-border ()
  "A regular bottom border string."
  (let* ((active (mode-line-window-selected-p))
         (margins (window-margins))
         (width (+ (window-width)
                   -2
                   (or (car margins) 0)
                   (or (cdr margins) 0)))
         (face (if active
                   'buffer-box-face-active
                 'buffer-box-face-inactive)))
    (propertize
     (concat (buffer-box--border 'BL active)
             (make-string width (aref (buffer-box--border 'H active) 0))
             (buffer-box--border 'BR active)
             " ")
     'face face)))

(defun buffer-box--side-border (buffer &optional active)
  "A regular side border for the provided BUFFER.

Border style depends on the ACTIVE status."
  (when buffer
    (with-current-buffer buffer
      (when-let* ((border (buffer-box--border 'V active))
                  (window (get-buffer-window buffer t))
                  (margins (window-margins window))
                  (margin-left (make-string (1- (or (car margins) 1)) ? ))
                  (border-left (concat border margin-left))
                  (margin-right (make-string (1- (or (cdr margins) 1)) ? ))
                  (border-right (concat margin-right border))
                  (face (if active
                            'buffer-box-face-active
                          'buffer-box-face-inactive))
                  (margin-left (propertize  " "
                                'display `((margin left-margin) ,border-left)))
                  (margin-right (propertize  " "
                                 'display `((margin right-margin) ,border-right)))
                  (overlay (buffer-box--overlay)))
        (when overlay
          (setq-local line-prefix (concat margin-right margin-left))
          (setq-local wrap-prefix (concat margin-right margin-left))
          (overlay-put overlay 'line-prefix (concat margin-right margin-left)))))))

(defun buffer-box-on (&optional header-line &rest args)
  "Install borders around current buffer.

The provided HEADER-LINE function with associated ARGS is evaluated to
generate the actual header line."
  (interactive)
  (setq-local buffer-box--data `((tab-line  . ,tab-line-format)
                                 (mode-line . ,mode-line-format)
                                 (header-line . ,header-line-format)
                                 (wrap-prefix . ,wrap-prefix)
                                 (line-prefix . ,line-prefix)
                                 (fringes-outside-margins  . ,fringes-outside-margins )
                                 (right-margin . ,right-margin-width)
                                 (left-margin . ,left-margin-width)))
  (let ((header-line (or header-line #'buffer-box-header)))
    (unless (buffer-box--overlay)
      (let ((overlay (make-overlay (point-min) (point-max))))
        (overlay-put overlay 'buffer-box t)))
    (set-window-margins (selected-window) 2 2)
    (setq-local fringes-outside-margins nil
                left-margin-width 2
                right-margin-width 2
                tab-line-format '(:eval (buffer-box--top-border))
                header-line-format `(:eval ,(cons header-line args))                
                mode-line-format '(:eval (buffer-box--bottom-border)))
    (buffer-box--side-border (current-buffer) t))

  ;; This hook is responsible for side borders that cannot be changed
  ;; from within the dynamic mode-line or header-line.
  (add-hook 'window-selection-change-functions #'buffer-box--selection-change)
  
  ;; This hook is responsible for newly created windows
  (add-hook 'window-buffer-change-functions #'buffer-box--track-windows))

(defun buffer-box-off ()
  "Remove borders around current buffer."
  
  (interactive)
  (when-let ((overlay (buffer-box--overlay)))
    (delete-overlay overlay))
  (when (boundp 'buffer-box--data)
    (set-window-margins (selected-window)
                        (alist-get 'right-margin buffer-box--data)
                        (alist-get 'left-margin buffer-box--data))
    (setq-local left-margin-width (alist-get 'left-margin buffer-box--data)
                fringes-outside-margins
                (alist-get 'fringes-outside-margins  buffer-box--data)
                right-margin-width (alist-get 'right-margin buffer-box--data)
                tab-line-format (alist-get 'tab-line buffer-box--data)
                header-line-format (alist-get 'header-line buffer-box--data)
                mode-line-format (alist-get 'mode-line buffer-box--data)
                line-prefix (alist-get 'line-prefix buffer-box--data)
                wrap-prefix (alist-get 'wrap-prefix buffer-box--data))))

(defun buffer-box (&optional header-line &rest args)
  "Toggle borders around current buffer.

An optional HEADER-LINE function (that will be called with ARGS) can be
provided."

  (interactive)
  (if (buffer-box--overlay)
      (buffer-box-off)
    (if header-line
        (buffer-box-on header-line args)
      (buffer-box-on))))

(provide 'buffer-box)

;;; buffer-box.el ends here
