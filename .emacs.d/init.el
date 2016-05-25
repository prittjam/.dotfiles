;; add packages not in melpa for emacs23 for lcraid
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

;; setup MELPA
(require 'package)

(add-to-list 
 'package-archives
 '("melpa" . "https://melpa.org/packages/") 
 '("gnu" . "http://elpa.gnu.org/packages/"))

(package-initialize)

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

;; setup MATLAB 
(custom-set-variables
 '(matlab-shell-command "matlab9")
 '(matlab-shell-command-switches (quote ("-nodesktop -nosplash"))))
