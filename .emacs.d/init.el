;; add elisp directory to path
;;(add-to-list 'load-path "~/.emacs.d/elisp/")

;; setup MELPA
(require 'package)

(add-to-list 
 'package-archives
 '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

;; add packages not in melpa for emacs23 for lcraid
(when (< emacs-major-version 24)
  (let ((default-directory  "~/.emacs.d/elisp/"))
    (normal-top-level-add-subdirs-to-load-path))

  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
  (progn;
    (add-to-list 'custom-theme-load-path "~/.emacs.d/elisp/emacs-color-theme-solarized"))
  
  ;; --- MATLAB MODE ---
  (load-library "~/.emacs.d/elisp/matlab-emacs/matlab-load.el"))

;; setup macros, preferences
(setq column-number-mode t) 
(global-set-key [f9] 'recompile)
(load-theme 'solarized t)

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

;; setup MATLAB 
(custom-set-variables
 '(matlab-shell-command "matlab9")
 '(matlab-shell-command-switches '("-nodesktop -nosplash")))
