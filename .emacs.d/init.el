;; setup windmove
(windmove-default-keybindings)

(global-set-key (kbd "S-<right>") 'windmove-right)
(global-set-key (kbd "S-<left>") 'windmove-left)
(global-set-key (kbd "S-<up>") 'windmove-up)
(global-set-key (kbd "S-<down>") 'windmove-down)

(global-set-key "\C-x\C-b" 'buffer-menu)

;; setup MELPA
(when 
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))


(when (>= emacs-major-version 24) 
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24) 
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

(defvar myPackages
  '(better-defaults
    ein ;; add the ein package (Emacs ipython notebook)
    elpy
    flycheck
    material-theme
    py-autopep8))

(mapc #'(lambda (package)
	  (unless (package-installed-p package)
	    (package-install package)))
      myPackages)

;; hide the startup message
(setq inhibit-startup-message t) 

;; enable line numbers globally
;;(global-linum-mode t) 

;; setup elpy
;;(elpy-enable)
;;(elpy-use-ipython)
;;(setq elpy-rpc-python-command "/usr/bin/python3")
;;(setq python-shell-interpreter "/usr/bin/python3")
;;;; use flycheck not flymake with elpy
;;(when (require 'flycheck nil t)
;;  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;  (add-hook 'elpy-mode-hook 'flycheck-mode))
;;
;; enable autopep8 formatting on save
;;(require 'py-autopep8)
;;(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
;;

(when (< emacs-major-version 24)
  (add-to-list 'load-path "~/.emacs.d/elisp/")
  (let ((default-directory  "~/.emacs.d/elisp/"))
    (normal-top-level-add-subdirs-to-load-path))
  
  (require 'color-theme)
  (require 'color-theme-solarized) 
  (color-theme-solarized)
  
  ;;  
  ;; --- MATLAB MODE ---
  (load-library "~/.emacs.d/elisp/matlab-emacs/matlab-load.el"))


;; swap buffers with C-c p
(setq swapping-buffer nil)
(setq swapping-window nil)
(defun swap-buffers-in-windows ()
  "Swap buffers between two windows"
  (interactive)
  (if (and swapping-window
	   swapping-buffer)
      (let ((this-buffer (current-buffer))
	    (this-window (selected-window)))
	(if (and (window-live-p swapping-window)
		 (buffer-live-p swapping-buffer))
	    (progn (switch-to-buffer swapping-buffer)
		   (select-window swapping-window)
		   (switch-to-buffer this-buffer)
		   (select-window this-window)
		   (message "Swapped buffers."))
	  (message "Old buffer/window killed.  Aborting."))
	(setq swapping-buffer nil)
	(setq swapping-window nil))
    (progn
      (setq swapping-buffer (current-buffer))
      (setq swapping-window (selected-window))
      (message "Buffer and window marked for swapping."))))

(global-set-key (kbd "C-c p") 'swap-buffers-in-windows)

;; setup macros, preferences
(setq column-number-mode t) 
(global-set-key [f9] 'recompile)

(when (>= emacs-major-version 24)
  (load-theme 'solarized t))

;;setup speedbar
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


;; uniquify.el is a helper routine to help give buffer names a better unique name.
(when (load "uniquify" 'NOERROR)
  (require 'uniquify)
  (setq uniquify-buffer-name-style 'forward)
					;(setq uniquify-buffer-name-style 'post-forward)
  )

;; setup MATLAB 
(custom-set-variables
 '(matlab-shell-command "matlab9")
 '(matlab-shell-command-switches (quote ("-nodesktop -nosplash"))))


;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))


;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))
