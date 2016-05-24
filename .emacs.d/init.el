;; add elisp directory to path
(add-to-list 'load-path "~/.emacs.d/elisp/")
(let ((default-directory  "~/.emacs.d/elisp/"))
  (normal-top-level-add-subdirs-to-load-path))

;; setup MELPA
(require 'package)
(add-to-list 
 'package-archives
 '("melpa" . "https://melpa.org/packages/"))

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

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


(if (>= emacs-major-version 24)
    (progn;
      (add-to-list 'custom-theme-load-path "~/.emacs.d/elisp/emacs-color-theme-solarized")
      (load-theme 'solarized t)))

;; --- MATLAB MODE ---
(load-library "~/.emacs.d/elisp/matlab-emacs/matlab-load.el")

(custom-set-variables
 '(matlab-shell-command "matlab81")
 '(matlab-shell-command-switches '("-nodesktop -nosplash")))