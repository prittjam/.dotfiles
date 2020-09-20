(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(add-to-list 'package-archives
	     '("gnu" . "http://elpa.gnu.org/packages/"))
(require 'package)
(require 'cl)
(setq package-check-signature nil)
(package-refresh-contents)

(setq pop-up-windows nil)

;;list the packages you want
(setq package-list
      '(color-theme-sanityinc-tomorrow
	window-purpose buffer-move jedi pyvenv ivy swiper yasnippet))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(require 'color-theme-sanityinc-tomorrow)
(require 'window-purpose)

(pyvenv-mode 1)
(pyvenv-activate "/home/jbpritts/.conda/envs")

(setq package-enable-at-startup nil)

(require 'use-package)

(add-to-list 'load-path "~/Downloads/old_matlab/")
(load-library "matlab-load")

(add-hook 'python-mode-hook
    (lambda ()
        (setq indent-tabs-mode nil)
        (infer-indentation-style)))

(use-package lsp-mode
  :config
  (require 'lsp-clients)
  (add-hook 'python-mode-hook 'lsp))

(defvar lsp-language-id-configuration
  '(...
    (python-mode . "python")
    ...))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection "pyls")
                  :major-modes '(python-mode)
                  :server-id 'pyls))

(use-package company-lsp)
(use-package lsp-ui)
(use-package dap-mode
  :bind (:map dap-mode-map
	      ("<f10>" . dap-debug-last)
	      ("<f11>" . my/debug-directly)
	      ("C-x C-a C-b" . dap-breakpoint-add)
	      ("C-x C-a C-d" . dap-breakpoint-delete)
	      ("C-c C-n" . dap-next)
	      ("C-c C-r" . dap-continue)
	      ("C-c C-s" . dap-step-in)
	      ("C-c C-c" . dap-eval-region)
	      ("C-c C-p" . dap-eval-thing-at-point))
  :hook ((dap-mode . dap-ui-mode)
	 (python-mode . (lambda () (require 'dap-python)
			  (dap-mode)))))

(use-package neotree
  :ensure t
  :bind ("<f8>" . 'neotree-toggle)
  :init
  ;; slow rendering
;;  (setq inhibit-compacting-font-caches t)

  ;; set icons theme
;;  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))

  ;; Every time when the neotree window is opened, let it find current file and jump to node
;;  (setq neo-smart-open t)

  ;; When running ‘projectile-switch-project’ (C-c p p), ‘neotree’ will change root automatically
;;  (setq projectile-switch-project-action 'neotree-projectile-action)

;; show hidden files
(setq-default neo-show-hidden-files nil)
(setq neo-window-position 'right))

(defun my/debug-directly ()
  (interactive)
  (dap-debug (list :type "python"
		   :args ""
		   :cwd nil
		   :target-module nil
		   :request "launch"
		   :name "Python :: Run Configuration")))

;; delete trailing whitespace
(defun my-prog-nuke-trailing-whitespace ()
  (when (derived-mode-p 'prog-mode)
    (delete-trailing-whitespace)))
(add-hook 'before-save-hook 'my-prog-nuke-trailing-whitespace)

(load-theme 'sanityinc-tomorrow-eighties t)

(purpose-mode)
(add-to-list 'purpose-user-mode-purposes '(tex-mode . code))
(add-to-list 'purpose-user-mode-purposes '(python-mode . code))
(add-to-list 'purpose-user-mode-purposes '(matlab-mode . code))
(add-to-list 'purpose-user-mode-purposes '(emacs-lisp-mode . code))
(add-to-list 'purpose-user-mode-purposes '(javascript-mode . code))
(add-to-list 'purpose-user-mode-purposes '(t-mode . code))
(add-to-list 'purpose-user-mode-purposes '(dap-ui-repl-mode . repl))
(add-to-list 'purpose-user-mode-purposes '(matlab-shell-mode . repl))
(add-to-list 'purpose-user-mode-purposes '(dap-server-log-mode . info))
(add-to-list 'purpose-user-mode-purposes '(compilation-mode . info))
(add-to-list 'purpose-user-mode-purposes '(eshell-mode . shell))
(add-to-list 'purpose-user-mode-purposes '(treemacs-mode . treemacs))
(add-to-list 'purpose-user-mode-purposes '(treemacs-mode . treemacs))
(add-to-list 'purpose-user-mode-purposes '(fundamental-mode . info))
(purpose-compile-user-configuration)

;; setup macros, preferences
(setq column-number-mode t)
(global-set-key [f9] 'recompile)
(global-set-key [f5] 'my/debug-directly)
(global-set-key [remap list-buffers] 'ibuffer)
;;

(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
;;
;; setup windmove
(windmove-default-keybindings)
(global-set-key (kbd "S-<right>") 'windmove-right)
(global-set-key (kbd "S-<left>") 'windmove-left)
(global-set-key (kbd "S-<up>") 'windmove-up)
(global-set-key (kbd "S-<down>") 'windmove-down)
;;
(use-package buffer-move)
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)
;;
;; setup MATLAB
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 (autoload 'matlab-mode "matlab" "Matlab Editing Mode" t)
 (add-to-list
  'auto-mode-alist
  '("\\.m$" . matlab-mode))
 (setq matlab-indent-function t)
 (setq matlab-shell-command "matlab")

(add-hook 'python-mode-hook
      (lambda ()
        (setq indent-tabs-mode nil)
        (setq python-indent 4)
        (setq tab-width 4))
      (untabify (point-min) (point-max)))

 ;;
;;(defun toggle-window-dedicated ()
;;  "Control whether or not Emacs is allowed to display another
;;buffer in current window."
;;  (interactive)
;;  (message
;;   (if (let (window (get-buffer-window (current-buffer)))
;;         (set-window-dedicated-p window (not (window-dedicated-p window))))
;;       "%s: Can't touch this!"
;;     "%s is up for grabs.")
;;   (current-buffer)))
;;
;;(global-set-key (kbd "C-c t") 'toggle-window-dedicated)
;;

;;
;;
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
;; enable this if you want `swiper' to use it
;; (setq search-default-mode #'char-fold-to-regexp)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-x C-b") 'ivy-switch-buffer)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
;;(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)

(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
(define-key ivy-minibuffer-map (kbd "<ESC>") 'minibuffer-keyboard-quit)
(define-key swiper-map (kbd "<ESC>") 'minibuffer-keyboard-quit)

;;(defun dap-python--populate-start-file-args (conf)
;;  "Populate CONF with the required arguments."
;;  (let* ((host "localhost")
;;         (debug-port (dap--find-available-port host (incf dap-python-default-debug-port)))
;;         (python-executable (dap-python--pyenv-executable-find dap-python-executable))
;;         (python-args (or (plist-get conf :args) ""))
;;         (program (or (plist-get conf :target-module)
;;                      (plist-get conf :program)
;;                      (buffer-file-name)))
;;         (module (plist-get conf :module)))
;;
;;    (dap--put-if-absent conf :program-to-start
;;                        (format "%s%s -m ptvsd --wait --host %s --port %s %s %s %s"
;;                                (or dap-python-terminal "")
;;                                (shell-quote-argument python-executable)
;;                                host
;;                                debug-port
;;                                (if module (concat "-m " (shell-quote-argument module)) "")
;;                                (shell-quote-argument program)
;;                                python-args))
;;    (plist-put conf :program program)
;;    (plist-put conf :debugServer debug-port)
;;    (plist-put conf :port debug-port)
;;    (plist-put conf :wait-for-port t)
;;    (plist-put conf :hostName host)
;;    (plist-put conf :host host)
;;    conf))
;;
;;(eval-defun)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (yasnippet window-purpose use-package tree-mode swiper pyvenv neotree matlab-mode lsp-ui lsp-python-ms jedi dap-mode company-lsp color-theme-sanityinc-tomorrow buffer-move))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
