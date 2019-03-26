;; hide the startup message
(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
'(tab-stop-list (quote (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120)))

(require 'package)
(add-to-list 'package-archives
       '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

(defvar myPackages
  '(better-defaults
    ein
    elpy
    flycheck
    material-theme
    py-autopep8
    yasnippet
    magit)) ;; add the autopep8 package

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPackages)

(load-theme 'solarized-light t)

(pyvenv-activate "~/virtualenvs/py36")

(require 'jupyter)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (jupyter . t)))

(setq org-confirm-babel-evaluate nil)   ;don't prompt me to confirm everytime I want to evaluate a block
(setq org-src-tab-acts-natively t)

(require 'yasnippet)
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-global-mode 1)

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

;; setup speedbar
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
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(matlab-shell-command "matlab")
 '(matlab-shell-command-switches (quote ("-nodesktop -nosplash")))
 '(package-selected-packages
   (quote
    (realgud exec-path-from-shell jupyter ob-ipython window-purpose rope-read-mode ein python-mode matlab-mode color-theme-sanityinc-solarized sr-speedbar solarized-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(jupyter-repl-traceback ((t (:background "moccasin"))))
 '(region ((t (:background "goldenrod" :foreground "#fdf6e3")))))

;; setup windmove
(windmove-default-keybindings)

(global-set-key (kbd "S-<right>") 'windmove-right)
(global-set-key (kbd "S-<left>") 'windmove-left)
(global-set-key (kbd "S-<up>") 'windmove-up)
(global-set-key (kbd "S-<down>") 'windmove-down)

(setq org-support-shift-select 'always)
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

(global-set-key "\C-x\C-b" 'buffer-menu)

(elpy-enable)
(add-hook 'elpy-mode-hook
      (lambda ()
        (setq indent-tabs-mode nil)
        (setq tab-width 4)
        (setq python-indent-offset 4)))

(setq elpy-company-add-completion-from-shell nil)
(define-key elpy-mode-map (kbd "C-c C-c") nil)  

(setq split-height-threshold 1200)
(setq split-width-threshold 2000)

(setq org-src-window-setup 'current-window)

(setq org-display-inline-images t)
(setq org-redisplay-inline-images t)
(setq org-startup-with-inline-images "inlineimages")

;;(require 'window-purpose)
;;(purpose-mode)
;;(add-to-list 'purpose-user-mode-purposes '(elpy . py))
;;(add-to-list 'purpose-user-mode-purposes '(ipython . *IPython*))
;;(purpose-compile-user-configuration)
