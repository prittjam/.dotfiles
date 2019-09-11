(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq lsp-prefer-flymake nil)

(eval-when-compile
  (require 'use-package))

(require 'lsp-mode)
(require 'color-theme-sanityinc-tomorrow)

(use-package lsp-mode
  :hook (python-mode . lsp)
  :commands lsp)

;;(use-package lsp-python)

(use-package lsp-python-ms
  :demand t
  :ensure nil
  :hook (python-mode . lsp)
  :config

  ;; for dev build of language server
  (setq lsp-python-ms-dir
        (expand-file-name "/opt/python-language-server/bin"))
  ;; for executable of language server, if it's not symlinked on your PATH
  (setq lsp-python-ms-executable
        "/opt/python-language-server/bin/Microsoft.Python.LanguageServer.LanguageServer"))

(use-package lsp-ui :commands lsp-ui-mode
  :ensure t
  :init (add-hook 'lsp-mode-hook 'lsp-ui-mode))
(use-package company-lsp :commands company-lsp)
;;(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
;;(use-package helm-lsp :commands helm-lsp-workspace-symbol)
(use-package dap-mode)

(use-package dap-python)
;;(use-package lsp-python :demand)

;; setup dap mode
;;(dap-mode 1)
;;(dap-ui-mode 1)
;;(define-key dap-mode-map (kbd "C-x C-a C-b") 'dap-breakpoint-add)
;;(define-key dap-mode-map (kbd "C-x C-a C-d") 'dap-breakpoint-delete)
;;(define-key dap-mode-map (kbd "C-c C-n") 'dap-next)
;;(define-key dap-mode-map (kbd "C-c C-r") `dap-continue)
;;(define-key dap-mode-map (kbd "C-c C-s") `dap-step-in)
;;(define-key dap-mode-map (kbd "C-c C-c") `dap-eval-region)
;;(define-key dap-mode-map (kbd "C-c C-p") `dap-eval-thing-at-point)

(defun my/debug-directly ()
  (interactive)
  (dap-debug    (list :type "python"
                      :args ""
                      :cwd nil
                      :target-module nil
                      :request "launch"
                      :name "Python :: Run Configuration")))

(define-key dap-mode-map (kbd "<f9>") `my/debug-directly)

;; delete trailing whitespace
(defun my-prog-nuke-trailing-whitespace ()
  (when (derived-mode-p 'prog-mode)
    (delete-trailing-whitespace)))
(add-hook 'before-save-hook 'my-prog-nuke-trailing-whitespace)

;; setup solarized light
;;(load-theme 'solarized-light t)
(load-theme 'sanityinc-tomorrow-eighties t)


(require 'window-purpose)
(purpose-mode)
(add-to-list 'purpose-user-mode-purposes '(python-mode . code))
(add-to-list 'purpose-user-mode-purposes '(matlab-mode . code))
(add-to-list 'purpose-user-mode-purposes '(emacs-lisp-mode . code))
(add-to-list 'purpose-user-mode-purposes '(javascript-mode . code))
(add-to-list 'purpose-user-mode-purposes '(t-mode . code))
(add-to-list 'purpose-user-mode-purposes '(dap-ui-repl-mode . repl))
(add-to-list 'purpose-user-mode-purposes '(matlab-shell-mode . repl))
(add-to-list 'purpose-user-mode-purposes '(compilation-mode . info))
(add-to-list 'purpose-user-mode-purposes '(eshell-mode . info))
(add-to-list 'purpose-user-mode-purposes '(treemacs-mode . treemacs))
(purpose-compile-user-configuration)


;; setup macros, preferences
(setq column-number-mode t)
(global-set-key [f9] 'recompile)
(global-set-key [f5] 'my/debug-directly)
(global-set-key [remap list-buffers] 'ibuffer)

;; setup yasnippets
;;(require 'yasnippet)
;;(yas-global-mode 1)

(global-set-key (kbd "C-x b") 'ivy-switch-buffer)

;; setup windmove
(windmove-default-keybindings)
(global-set-key (kbd "S-<right>") 'windmove-right)
(global-set-key (kbd "S-<left>") 'windmove-left)
(global-set-key (kbd "S-<up>") 'windmove-up)
(global-set-key (kbd "S-<down>") 'windmove-down)

(use-package buffer-move)
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

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
    (0blayout counsel treemacs-magit treemacs-icons-dired treemacs-projectile treemacs-evil buffer-move ms-python window-purpose yasnippet-snippets yasnippet-classic-snippets use-package sr-speedbar solarized-theme pyenv-mode py-autopep8 projectile matlab-mode material-theme magit lsp-ui lsp-treemacs lsp-python-ms lsp-python lsp-java jupyter helm-lsp flycheck elpy dap-mode company-lsp company-jedi company-box))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(defun toggle-window-dedicated ()
  "Control whether or not Emacs is allowed to display another
buffer in current window."
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window (not (window-dedicated-p window))))
       "%s: Can't touch this!"
     "%s is up for grabs.")
   (current-buffer)))

(global-set-key (kbd "C-c t") 'toggle-window-dedicated)

;; default to python3.6
(pyvenv-activate "~/.venv/py36")

(add-hook 'python-mode-hook
      (lambda ()
        (setq indent-tabs-mode nil)
        (setq python-indent 4)
        (setq tab-width 4))
      (untabify (point-min) (point-max)))


(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if (executable-find "python3") 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-follow-delay             0.2
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-position                      'right
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-desc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-width                         35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)



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
