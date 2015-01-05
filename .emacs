(add-to-list 'load-path "~/elisp")
(add-to-list 'load-path "~/elisp/matlab-emacs")

;; --- MATLAB MODE ---
(load-library "~/elisp/matlab-emacs/matlab-load.el")

;(autoload 'octave-mode "octave-mod" nil t)
;(setq auto-mode-alist
;      (cons '("\\.m$" . octave-mode) auto-mode-alist))                           
;

(when (fboundp 'winner-mode)
  (winner-mode 1))
(put 'erase-buffer 'disabled nil)

(setq tags-revert-without-query 1)
(defalias 'yes-or-no-p 'y-or-n-p)

(setq tags-table-list
      '("/home.stud/prittjam/src/cvtk2" "/home.stud/prittjam/src/cvdb" "/home.stud/prittjam/src/wbs" "/home.stud/prittjam/src/repeat" "/home.stud/prittjam/src/repeat" "/home.stud/prittjam/src/courses/w2010/ae4m33tdv" "/home.stud/prittjam/src/Fa"))

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