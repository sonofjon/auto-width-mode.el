;;; auto-width-mode.el --- Automatically resize the width of focused windows -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Andreas Jonsson
;;
;; Author: Andreas Jonsson <ajdev8@gmail.com>
;; Maintainer: Andreas Jonsson <ajdev8@gmail.com>
;; URL: https://github.com/sonofjon/auto-width-mode.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: convenience, frames
;;
;;; Commentary:
;;
;; auto-width-mode is a global minor mode that automatically sets the width
;; of windows when they gain focus.
;;
;; This is particularly useful when working with multiple windows on smaller
;; screens, ensuring the active window maintains a readable width (e.g., 80
;; columns) without manual resizing each time you switch focus.
;;
;; Features:
;; - Automatically resizes focused window to target width (default 80
;; - columns)
;; - Excludes windows by major mode or buffer name pattern
;; - Respects window constraints (minibuffer, dedicated windows)
;; - Graceful handling when resizing is impossible
;;
;; Usage:
;;
;;   (require 'auto-width-mode)
;;   (auto-width-mode 1)
;;
;; Configuration:
;;
;;   ;; Set target width (default 80)
;;   (setq auto-width-target-width 100)
;;
;;   ;; Exclude specific major modes
;;   (setq auto-width-exclude-modes '(dired-mode magit-status-mode))
;;
;;   ;; Exclude buffers matching regexp patterns
;;   (setq auto-width-exclude-buffer-regexp '("^\\*Messages\\*" "^\\*scratch\\*"))
;;
;;; Code:

(defgroup auto-width nil
  "Automatically resize windows to a target width."
  :group 'convenience
  :prefix "auto-width-")

(defcustom auto-width-target-width 80
  "Target width, in columns, for the selected window."
  :type 'integer
  :group 'auto-width)

(defcustom auto-width-exclude-modes nil
  "A list of symbols or strings naming major modes.
Switching to a buffer whose major mode is a member of this list will not
cause the window to be resized."
  :type '(repeat (choice symbol string))
  :group 'auto-width)

(defcustom auto-width-exclude-buffer-regexp nil
  "A list of regexp's used to match buffer names.
Switching to a buffer whose name matches one of these regexps will
prevent the window from being resized."
  :type '(repeat string)
  :group 'auto-width)

(defvar auto-width-mode nil
  "Non-nil when auto-width-mode is enabled.")

(defvar auto-width--inhibit nil
  "Non-nil while `auto-width-mode' is resizing a window.")

(defun auto-width--exclude-major-mode-p ()
  "Return non-nil if current `major-mode' should not be resized."
  (or (memq major-mode auto-width-exclude-modes)
      (member (symbol-name major-mode) auto-width-exclude-modes)))

(defun auto-width--exclude-buffer-name-p ()
  "Return non-nil if current buffer name should not be resized."
  (and auto-width-exclude-buffer-regexp
       (seq-some (lambda (regexp)
                   (string-match regexp (buffer-name)))
                 auto-width-exclude-buffer-regexp)))

(defun auto-width--eligible-window-p (window)
  "Return non-nil when WINDOW should be auto-resized."
  (and (window-live-p window)
       (not (window-minibuffer-p window))
       ;; Exclude strongly dedicated windows (value t)
       ;; - Note that side windows are often "side-dedicated" (value 'side).
       (not (eq (window-dedicated-p window) t))
       ;; Exclude by major mode
       (with-current-buffer (window-buffer window)
         (not (auto-width--exclude-major-mode-p)))
       ;; Exclude by buffer name regexp
       (with-current-buffer (window-buffer window)
         (not (auto-width--exclude-buffer-name-p)))))

(defun auto-width--apply (window)
  "Resize WINDOW to `auto-width-target-width' columns.

Does nothing when resizing is impossible (e.g. due to frame
constraints)."
  (when (and auto-width-mode
             (not auto-width--inhibit))
    (let* ((target (max 1 auto-width-target-width))
           (delta (- target (window-body-width window))))
      (when (and (auto-width--eligible-window-p window)
                 (not (zerop delta)))
        (let ((auto-width--inhibit t))
          (condition-case _err
              (window-resize window delta t)
            (error nil)))))))

(defun auto-width--on-selection-change (&rest _args)
  "Apply the configured width when a new window becomes selected."
  (auto-width--apply (selected-window)))

;;;###autoload
(define-minor-mode auto-width-mode
  "Auto-apply `auto-width-target-width' when selecting a window.

This global minor mode reacts to window selection changes and resizes
the newly selected window once.  It does not continuously maintain the
width, so manual resizing is not overridden unless you switch away and
back again."
  :global t
  :lighter " aw"
  :group 'auto-width
  (if auto-width-mode
      (progn
        (add-hook 'window-selection-change-functions
                  #'auto-width--on-selection-change)
        (auto-width--apply (selected-window)))
    (remove-hook 'window-selection-change-functions
                 #'auto-width--on-selection-change)))

(provide 'auto-width-mode)

;;; auto-width-mode.el ends here
