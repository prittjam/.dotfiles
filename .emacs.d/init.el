(add-to-list 'load-path "~/.emacs.d/elisp/")
(let ((default-directory  "~/.emacs.d/elisp/"))
  (normal-top-level-add-subdirs-to-load-path))

(setq column-number-mode t) 

(require 'sr-speedbar)

(setq 
    sr-speedbar-width 30
    speedbar-use-images t
    speedbar-show-unknown-files t
    sr-speedbar-right-side t
    )

(add-hook 'emacs-startup-hook (lambda ()
  (sr-speedbar-open)
  (with-current-buffer sr-speedbar-buffer-name
    (setq window-size-fixed 'width))
  ))

(if (>= emacs-major-version 24)
    (progn;
      (add-to-list 'custom-theme-load-path "~/.emacs.d/elisp/emacs-color-theme-solarized")
      (load-theme 'solarized t)))

;; --- MATLAB MODE ---
(load-library "~/.emacs.d/elisp/matlab-emacs/matlab-load.el")

(when (fboundp 'winner-mode)
  (winner-mode 1))
(put 'erase-buffer 'disabled nil)

(setq tags-table-list
      '("~/src"))

(setq tags-revert-without-query 1)
(defalias 'yes-or-no-p 'y-or-n-p)

(require 'ctags-update)
(ctags-update-minor-mode 1)

;;;---------------------------------------------------------------------
;;; display-buffer

;; The default behaviour of `display-buffer' is to always create a new
;; window. As I normally use a large display sporting a number of
;; side-by-side windows, this is a bit obnoxious.
;;
;; The code below will make Emacs reuse existing windows, with the
;; exception that if have a single window open in a large display, it
;; will be split horisontally.

(setq pop-up-windows nil)

(defun my-display-buffer-function (buf not-this-window)
  (if (and (not pop-up-frames)
           (one-window-p)
           (or not-this-window
               (not (eq (window-buffer (selected-window)) buf)))
           (> (frame-width) 162))
      (split-window-horizontally))
  ;; Note: Some modules sets `pop-up-windows' to t before calling
  ;; `display-buffer' -- Why, oh, why!
  (let ((display-buffer-function nil)
        (pop-up-windows nil))
    (display-buffer buf not-this-window)))

(setq display-buffer-function 'my-display-buffer-function)
(global-set-key "\C-x\C-b" 'buffer-menu)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(defun transpose-buffers (arg)
  "Transpose the buffers shown in two windows."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
            (next-win (window-buffer (funcall selector))))
        (set-window-buffer (selected-window) next-win)
        (set-window-buffer (funcall selector) this-win)
        (select-window (funcall selector)))
      (setq arg (if (plusp arg) (1- arg) (1+ arg))))))

(global-set-key [f9] 'recompile)

(setq path-to-ctags "/usr/bin/ctags") ;; <- your ctags path here
  
(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "ctags -f %s -e -R %s" path-to-ctags (directory-file-name dir-name)))
  )

;;7(setq load-path (cons "/home/you" load-path)) ;; Adjust path here
;; Ampl mode
(setq auto-mode-alist
      (cons '("\\.mod$" . ampl-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.dat$" . ampl-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.ampl$" . ampl-mode) auto-mode-alist))
(setq interpreter-mode-alist
      (cons '("ampl" . ampl-mode)
            interpreter-mode-alist))

(autoload 'ampl-mode "ampl-mode" "Ampl editing mode." t)
