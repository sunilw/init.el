;; prevent annoying "really really close buffer?" bullshit from emacs
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

(add-to-list 'load-path "~/.emacs.d/lisp")

;;;;;;;;;;;;;;;
;; color theme
;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/lisp/color-theme-6.6.0")
(require 'color-theme)
(load-file 
 "~/.emacs.d/lisp/color-theme-6.6.0/themes/color-theme-wombat/color-theme-wombat.el")

;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;

(require 'popup)

;; global auto revert
(global-auto-revert-mode 1)

;; helm!
(add-to-list 'load-path "~/.emacs.d/lisp/helm")
(require 'helm-config)
(global-set-key (kbd "C-<tab>") 'helm-mini)


;;;;;;;;;;;;;;;;;;;;;;;
;;  auto-complete
;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/lisp/auto-complete")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/lisp/auto-complete/dict")
(ac-config-default)

(add-to-list 'ac-modes 'coffee-mode)
(add-to-list 'ac-modes 'js-mode)
(add-to-list 'ac-modes 'python-mode)
(add-to-list 'ac-modes 'shell-mode)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; django/python stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/lisp/python-django.el")
(require 'python-django)

(add-to-list 'load-path "~/.emacs.d/lisp/pony-mode/src")
(require 'pony-mode)


(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
 "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
 "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
 "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

(autoload 'python-shell-switch-to-shell "python" nil t)


;; connect to pyflakes
(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "pyflakes" (list local-file))))

  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))

(add-hook 'find-file-hook 'flymake-find-file-hook)

;; get tab completions?
(eval-after-load "python"
  '(define-key inferior-python-mode-map "\t" 'python-shell-completion-complete-or-indent))


(add-to-list 'load-path "~/.emacs.d/lisp/emacs-helm-pydoc")
(require 'helm-pydoc)
(eval-after-load "python"
  '(progn
     (define-key python-mode-map (kbd "C-c C-d") 'helm-pydoc)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; jedi
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/lisp/emacs-ctable")
(require 'ctable)

(add-to-list 'load-path "~/.emacs.d/lisp/emacs-deferred")
(require 'deferred)

(add-to-list 'load-path "~/.emacs.d/lisp/emacs-epc")
(require 'epc)

(add-to-list 'load-path "~/.emacs.d/lisp/emacs-jedi")
(add-hook 'python-mode-hook 'jedi:setup)
(require 'jedi)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://www.emacswiki.org/emacs/BackupDirectory
;; move autosave files out of the present working directory
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.saves"))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)       ; use versioned backups


(message "Deleting old backup files...")
(let ((week (* 60 60 24 7))
      (current (float-time (current-time))))
  (dolist (file (directory-files temporary-file-directory t))
    (when (and (backup-file-name-p file)
               (> (- current (float-time (fifth (file-attributes file))))
                  week))
      (message "%s" file)
      (delete-file file))))

;; paredit fix!
;; prevents extra space being thrown in
(defun paredit-space-for-delimiter-p (endp delimiter)
  (and (not (if endp (eobp) (bobp)))
       (memq (char-syntax (if endp (char-after) (char-before)))
             (list ?\"  ;; REMOVED ?w ?_
                   (let ((matching (matching-paren delimiter)))
                     (and matching (char-syntax matching)))))))

;; workgroups
(add-to-list 'load-path "~/.emacs.d/lisp/workgroups.el")
(require 'workgroups)
(setq wg-prefix-key (kbd "C-c w"))

;; rainbows!
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; websocket
(add-to-list 'load-path "~/.emacs.d/lisp/emacs-websocket")
(require 'websocket)

;; jsSlime
(add-to-list 'load-path "~/.emacs.d/lisp/jss")
(require 'jss)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yasnippet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/lisp/yasnippet")
(require 'yasnippet)
(yas/load-directory "~/.emacs.d/lisp/yasnippet/snippets")
;;(yas--initialize)
(yas-reload-all)

;;fix broken yas behaviour
(defun yas/advise-indent-function (function-symbol)
  (eval `(defadvice ,function-symbol (around yas/try-expand-first activate)
           ,(format
             "Try to expand a snippet before point, then call `%s' as usual"
             function-symbol)
           (let ((yas/fallback-behavior nil))
             (unless (and (interactive-p)
                          (yas/expand))
               ad-do-it)))))

;; (yas/advise-indent-function 'css-mode-indent-line)
(global-set-key (kbd "C-c C-r") 'sgml-tag)

;; get rid of annoying tilde files
(setq make-backup-files nil)

;; set up yas in modes
(add-hook 'shell-mode-hook 'yas-minor-mode)
(add-hook 'python-mode-hook 'yas-minor-mode)




;; http://emacsblog.org/2007/01/17/indent-whole-buffer/
(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))


(savehist-mode 1)
;;(require 'recentf)
(recentf-mode 1)

;; zen coding
(add-to-list 'load-path "~/.emacs.d/lisp/zencoding")
(require 'zencoding-mode)
(global-set-key (kbd "<C-return>") 'zencoding-expand-line)


(add-to-list 'load-path
             "~/.emacs.d/lisp/haml-mode")
(add-to-list 'load-path
             "~/.emacs.d/lisp/sass-mode")

;;sass mode
(require 'sass-mode)
(add-hook 'sass-mode-hook 'linum-mode)
(add-hook 'sass-mode-hook 'global-auto-revert-mode)
(add-hook 'sass-mode-hook 'auto-complete-mode 1)

(eval-when-compile (require 'cl))
(defun toggle-transparency ()
  (interactive)
  (if (/=
       (cadr (frame-parameter nil 'alpha))
       100)
      (set-frame-parameter nil 'alpha '(100 100))
    (set-frame-parameter nil 'alpha '(95 50))))
(global-set-key (kbd "C-c t") 'toggle-transparency)

(tool-bar-mode -1)

(add-to-list 'load-path
             "~/.emacs.d/lisp/php-mode-1.5.0")
(require 'php-mode)
(add-hook 'php-mode-hook 'imenu-add-menubar-index)
(add-hook 'php-mode-hook 'auto-indent-mode)

;; imenu+
(require 'hide-comnt)
(require 'imenu+)

(add-to-list 'load-path
             "~/.emacs.d/lisp/phpplus-mode")
(require 'php+-mode)

;; php docs
(require 'php-doc nil t)
(setq php-doc-directory "~/.docs/php-chunked-xhtml")
(add-hook 'php-mode-hook
          (lambda ()
            (local-set-key "\t" 'php-doc-complete-function)
            (local-set-key (kbd "\C-c h") 'php-doc)
            (set (make-local-variable 'eldoc-documentation-function)
                 'php-doc-eldoc-function)
            (eldoc-mode 1)))


(add-hook 'nxhtml-mode-hook
          (lambda ()
            (local-set-key "\t" 'php-doc-complete-function)
            (local-set-key (kbd "\C-c h") 'php-doc)
            (set (make-local-variable 'eldoc-documentation-function)
                 'php-doc-eldoc-function)
            (eldoc-mode 1)))

;; php-mode indentation is less then ideal
;; lets see if we can unbreak it
;; http://www.emacswiki.org/emacs/PhpMode#toc16
(add-hook 'php-mode-hook (lambda ()
                           (defun ywb-php-lineup-arglist-intro (langelem)
                             (save-excursion
                               (goto-char (cdr langelem))
                               (vector (+ (current-column) c-basic-offset))))
                           (defun ywb-php-lineup-arglist-close (langelem)
                             (save-excursion
                               (goto-char (cdr langelem))
                               (vector (current-column))))
                           (c-set-offset 'arglist-intro 'ywb-php-lineup-arglist-intro)
                           (c-set-offset 'arglist-close 'ywb-php-lineup-arglist-close)))

;; fix  broken css indentation
(setq cssm-indent-level 4)
(setq cssm-newline-before-closing-bracket t)
(setq cssm-indent-function #'cssm-c-style-indenter)
(setq cssm-mirror-mode nil)
;; while we are here...
(add-hook 'css-mode-hook 'smartparens-mode)
(add-hook 'css-mode-hook 'yas-minor-mode)



;; increase text size
(global-set-key
 (kbd "C--")
 (lambda ()
   (interactive)
   (set-face-attribute 'default nil :height 80)))
(global-set-key
 (kbd "C-0")
 (lambda ()
   (interactive)
   (set-face-attribute 'default nil :height 140)))
(global-set-key
 (kbd "C-=")
 (lambda ()
   (interactive)
   (set-face-attribute 'default nil :height 240)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; js2 mode!
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '(".bowerrc" . js2-mode))
(add-hook 'js2-mode-hook 'yas-minor-mode)

;; js2 enhancements

(require 'flymake-cursor)
;; syntax checking
(add-to-list 'load-path "~/.emacs.d/lisp/lintnode")
(require 'flymake-jslint)
;; Make sure we can find the lintnode executable
(setq lintnode-location "~/.emacs.d/lisp/lintnode")
;; JSLint can be... opinionated
(setq lintnode-jslint-excludes (list 'nomen 'undef 'plusplus 'onevar 'white))
;; Start the server when we first open a js file and start checking
(add-hook 'js-mode-hook
          (lambda ()
            (lintnode-hook)))

;; unset unneccessary errors for code within docready
;;   /*jslint browser: true */
;;   /*global $,console */

;; js repl
(require 'js-comint)
;; Use node as our repl
(setq inferior-js-program-command "node")
(setq inferior-js-mode-hook
      (lambda ()
        ;; We like nice colors
        (ansi-color-for-comint-mode-on)
        ;; Deal with some prompt nonsense
        (add-to-list 'comint-preoutput-filter-functions
                     (lambda (output)
                       (replace-regexp-in-string ".*1G\.\.\..*5G" "..."
                                                 (replace-regexp-in-string ".*1G.*3G" "&gt;" output))))))


;; jquery stuff
(add-to-list 'load-path "~/.emacs.d/lisp/jquery-doc.el")
(require 'jquery-doc)
(add-hook 'js2-mode-hook 'jquery-doc-setup)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; skewer dep: simple-httpd
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(add-to-list 'load-path "~/.emacs.d/lisp/emacs-web-server")
(require 'simple-httpd)
(setq httpd-root "/var/www")
(httpd-start)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; skewer
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/lisp/skewer-mode")
(require 'skewer-mode)

(add-hook 'web-mode-hook 'skewer-mode)
(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'sass-mode-hook 'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; auto-complete
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/lisp/auto-complete")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/lisp/auto-complete/dict")
(ac-config-default)

;; unbreak auto-complete
(defun complete-or-newline (arg)
  (if (and
       (or (bobp) (= ?w (char-syntax (char-before))))
       (or (eobp) (not (= ?w (char-syntax (char-after))))))
      (auto-complete)
    (newline-and-indent)))

;; Rinari
;; (add-to-list 'load-path "~/.emacs.d/lisp/rinari")
;; (require 'rinari)

(add-hook 'html-mode-hook 'yas-minor-mode)
(add-hook 'html-mode-hook 'linum-mode)

;; mustache-mode
(require 'mustache-mode)

;; emacs nav, by google
(add-to-list 'load-path "~/.emacs.d/lisp/emacs-nav-49")
(require 'nav)
(nav-disable-overeager-window-splitting)
;; Optional: set up a quick key to toggle nav
;; (global-set-key [f8] 'nav-toggle)

;; restclient
(add-to-list 'load-path "~/.emacs.d/lisp/restclient.el")
(require 'restclient)

(add-to-list 'load-path "~/.emacs.d/lisp/coffee-mode")
(require 'coffee-mode)


;; web mode
(add-to-list 'load-path "~/.emacs.d/lisp/web-mode")
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-hook 'web-mode-hook 'linum-mode 1)
(add-hook 'web-mode-hook 'autopair-mode)
(add-hook 'web-mode-hook 'wrap-region-mode)
(add-hook 'web-mode-hook 'linum-mode)
(add-hook 'web-mode-hook 'yas-minor-mode)
(add-hook 'web-mode-hook 'electric-pair-mode)

(defun web-mode-init ()
  "web-mode config."
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-indent-style 2)
  (setq web-mode-prefer-server-commenting t)
  (comment-auto-fill)
  (auto-complete-init)
  )

(add-to-list 'ac-modes 'web-mode)

;; make php-mode indentation vaguely sane
(add-hook 'php-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)
            (setq tab-width 4)
            (c-set-offset 'substatement-open 0)))

(add-hook 'php-mode-hook 'auto-complete-mode 1)


;; scss mode
(add-to-list 'load-path "~/.emacs.d/lisp/scss-mode")
(autoload 'scss-mode "scss-mode")
(setq scss-compile-at-save nil)
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(add-hook 'scss-mode-hook 'auto-complete-mode 1)
(add-hook 'scss-mode-hook 'smartparens-mode)

;; erc notify
(add-to-list 'load-path "~/.emacs.d/lisp/erc-nick-notify.el")
(autoload 'erc-nick-notify-mode "erc-nick-notify"
  "Minor mode that calls `erc-nick-notify-cmd' when his nick gets
mentioned in an erc channel" t)
(eval-after-load 'erc '(erc-nick-notify-mode t))

;; mozilla connection
(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)
(add-hook 'javascript-mode-hook 'javascript-custom-setup)
(defun javascript-custom-setup ()
  (moz-minor-mode 1))

(defun auto-reload-firefox-on-after-save-hook ()
  (add-hook 'after-save-hook
            '(lambda ()
               (interactive)
               (comint-send-string (inferior-moz-process)
                                   "setTimeout(BrowserReload(), \"1000\");"))
            'append 'local)) ; buffer-local


;; php-boris
(add-to-list 'load-path "~/.emacs.d/lisp/php-boris")
(require 'php-boris)

;; boris minor mode
(add-to-list 'load-path "~/.emacs.d/lisp/php-boris-minor-mode")
(require 'php-boris-minor-mode)


;; https://gist.github.com/nonsequitur/666092
(define-minor-mode moz-reload-on-save-mode
  "Moz Reload On Save Minor Mode"
  nil " Reload" nil
  (if moz-reload-on-save-mode
      ;; Edit hook buffer-locally.
      (add-hook 'after-save-hook 'moz-firefox-reload nil t)
    (remove-hook 'after-save-hook 'moz-firefox-reload t)))

(defun moz-firefox-reload ()
  (comint-send-string (inferior-moz-process) "BrowserReload();"))


;; twittering mode
(add-to-list 'load-path "~/.emacs.d/lisp/twittering-mode")
(require 'twittering-mode)

;; marmalade
(require 'package)
;; Add the original Emacs Lisp Package Archive
(add-to-list 'package-archives
             '("elpa" . "http://tromey.com/elpa/"))


;; Add the user-contributed repository
(add-to-list
 'package-archives
 '("marmalade" . "http://marmalade-repo.org/packages/"))

(add-hook 'php-mode-hook 'my-php-mode-hook)
(defun my-php-mode-hook ()
  "My PHP mode configuration."
  (setq indent-tabs-mode nil
        tab-width 4
        c-basic-offset 4))


;; auto indent mode
(add-to-list 'load-path "~/.emacs.d/lisp/auto-indent-mode.el")
(setq auto-indent-on-visit-file t)
(require 'auto-indent-mode)
(require 'php-boris-minor-mode)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  color theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-cursor-color "white")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  color theme config ends
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; customize mode-line
(set-face-foreground 'mode-line "white")
(set-face-background 'mode-line "purple")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-revert-check-vc-info t)
 '(jabber-account-list (quote (("") ("sunilw@chat.facebook.com" (:password . "maddogmad00")))))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:stipple nil :background "black" :foreground "#7eff00" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 123 :width normal :foundry "unknown" :family "Droid Sans Mono"))))
 '(cursor ((t (:background "white"))))
 '(font-lock-comment-face ((t (:foreground "gray49" :box nil :slant italic))))
 '(jabber-activity-face ((t (:background "light gray" :foreground "red" :weight bold))))
 '(mode-line-inactive ((t (:background "blue" :foreground "white"))))
 '(nil ((t (:foreground "Green"))) t)
 '(web-mode-html-attr-value-face ((t (:foreground "CornflowerBlue"))))
 '(web-mode-html-tag-face ((t (:foreground "maroon1"))))
 '(web-mode-keyword-face ((t (:inherit font-lock-keyword-face :foreground "Green"))))
 '(web-mode-preprocessor-face ((t (:inherit font-lock-preprocessor-face :foreground "DeepSkyBlue1")))))


(set-cursor-color "white")
