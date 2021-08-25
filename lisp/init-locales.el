;;; init-locales.el --- Configure default locale -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun sanityinc/locale-var-encoding (v)
  "Return the encoding portion of the locale string V, or nil if missing."
  (when v
    (save-match-data
      (let ((case-fold-search t))
        (when (string-match "\\.\\([^.]*\\)\\'" v)
          (intern (downcase (match-string 1 v))))))))

(dolist (varname '("LC_ALL" "LANG" "LC_CTYPE"))
  (let ((encoding (sanityinc/locale-var-encoding (getenv varname))))
    (unless (memq encoding '(nil utf8 utf-8))
      (message "Warning: non-UTF8 encoding in environment variable %s may cause interop problems with this Emacs configuration." varname))))

(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(unless (eq system-type 'windows-nt)
  (set-selection-coding-system 'utf-8))

;; 中文对齐
(require-package 'cnfonts)
;; 我喜欢undotree
(require-package 'undo-tree)
(global-undo-tree-mode)

;; 分享快捷键
(require-package 'keycast)

;; 使用clj-kondo来做clj/cljc/cljs的语法检查, 需要安装clj-kondo
(require-package 'flycheck-clj-kondo)
(require 'flycheck-clj-kondo)
(dolist (checker '(clj-kondo-clj clj-kondo-cljs clj-kondo-cljc clj-kondo-edn))
  (setq flycheck-checkers (cons checker (delq checker flycheck-checkers))))


;; 开启换页线
(global-page-break-lines-mode)

(add-hook 'clojure-mode-hook
          (lambda ()
            (page-break-lines-mode)))
(add-hook 'clojurescript-mode-hook
          (lambda ()
            (page-break-lines-mode)))

;; 80个字符处放置竖线
(setq-default fill-column 80)

;; 使用cua做矩形区域编辑
(cua-mode 1)
(setq cua-enable-cua-keys nil)
(global-set-key
 (kbd "<C-return>")
 'cua-set-rectangle-mark)

;; org文件生成reveal.js PPT
(require-package 'ox-reveal)
(load-library "ox-reveal")


;; 需要expand-region
(require-package 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; 映射全角字符到半角字符
(let (
      ($replacePairs
       [
        ["，" ","]
        ["。" "."]
        ["；" ";"]
        ["：" ":"]
        ["【" "["]
        ["】" "]"]
        ["（" "("]
        ["）" ")"]
        ["！" "!"]
        ["、" "\\"]
        ["／" "/"]
        ["《" "<"]
        ["》" ">"]
        ["‘" "'"]
        ["’" "'"]
        ["“" "\""]
        ["”" "\""]
        ]
       ))
  (mapcar (lambda(x) (define-key key-translation-map
                       (kbd (elt x 0)) (kbd (elt x 1)))) $replacePairs))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (awk . t)
   (dot . t)
   (plantuml . t)
   (clojure . t)
   (latex . t)
   (python . t)
   (perl . t)
   (js . t)
   (shell . t)
   (sql . t)
   (org . t)
   (ditaa . t)
   (emacs-lisp . t)
   (lisp . t)    ;; slime - lisp interaction mode
   (gnuplot . t)))
(setq org-plantuml-jar-path "~/.emacs.d/plantuml.jar")

(setq org-confirm-babel-evaluate t)

(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src elisp"))


(setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))




;; 使用org-bullets
(require-package 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(setq org-ellipsis " ▾")

(setq org-agenda-files '("~/sandbox/rc/learn-clojure/todo.org"))
(setq org-latex-create-formula-image-program 'imagemagick)


;; 使用pandoc把org文件转为md, 需要安装pandoc
(require-package 'ox-pandoc)
(load-library "ox-pandoc")

(require-package 'smex)
(require 'smex) ; Not needed if you use package.el
(smex-initialize) ; Can be omitted. This might cause a (minimal) delay


;; counsel & ivy
(require-package 'counsel)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "<f2> j") 'counsel-set-variable)
;;(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
(global-set-key (kbd "C-c v") 'ivy-push-view)
(global-set-key (kbd "C-c V") 'ivy-pop-view)

(global-set-key (kbd "C-c c") 'counsel-compile)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c L") 'counsel-git-log)
(global-set-key (kbd "C-c k") 'counsel-rg)
(global-set-key (kbd "C-c m") 'counsel-linux-app)
(global-set-key (kbd "C-c n") 'counsel-fzf)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-c J") 'counsel-file-jump)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(global-set-key (kbd "C-c w") 'counsel-wmctrl)

(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "C-c b") 'counsel-bookmark)
(global-set-key (kbd "C-c d") 'counsel-descbinds)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c o") 'counsel-outline)
(global-set-key (kbd "C-c t") 'counsel-load-theme)
(global-set-key (kbd "C-c F") 'counsel-org-file)

;; eval ditaa block in org file
(defun my-org-confirm-babel-evaluate (lang body)
  (not (string= lang "ditaa")))  ;don't ask for ditaa
(setq org-confirm-babel-evaluate #'my-org-confirm-babel-evaluate)


(defun try-convert (out)
  (shell-command-on-region
   (region-beginning) (region-end)
   (format "convert.clj %s " out)
   nil "REPLACE" nil t))
(defun convert-to-edn  () (interactive) (try-convert "edn"))
(defun convert-to-json () (interactive) (try-convert "json"))
(defun convert-to-yaml () (interactive) (try-convert "yaml"))


(require-package 'org-present)
(eval-after-load "org-present"
  '(progn
     (add-hook 'org-present-mode-hook
               (lambda ()
                 (org-present-big)
                 (org-display-inline-images)
                 (org-present-hide-cursor)
                 (org-present-read-only)))
     (add-hook 'org-present-mode-quit-hook
               (lambda ()
                 (org-present-small)
                 (org-remove-inline-images)
                 (org-present-show-cursor)
                 (org-present-read-write)))))

(setq magit-log-margin-show-committer-date t)

(require-package 'virtualenvwrapper)
(require 'virtualenvwrapper)
(venv-initialize-interactive-shells)
(venv-initialize-eshell)

(setq org-babel-clojure-backend 'cider)


;; Region lines and then `M-x osx-say' to make OSX speak.

;; Adjust speak speed
(setq osx-say-speed 180)

;; Change voice
;; Kathy, Vicki, Victoria, Alex, Bruce, Fred
(setq osx-say-voice "Alex")
(setq osx-say-buffer "*osx say*")

(defun osx-say-stop ()
  (interactive)
  (when (get-buffer osx-say-buffer)
    (kill-buffer osx-say-buffer)))

(defun osx-say (&optional $word $speed)
  "Utilize `say' command that Mac OSX has."
  (interactive)
  (unless (executable-find "say")
    (error (message "`say' command not found")))
  (osx-say-stop)
  (cond ($word $word)
        (mark-active
         (setq $word (buffer-substring-no-properties
                      (region-beginning) (region-end))))
        ((setq $word (thing-at-point 'word)))
        (t (setq $word (read-string "word: "))))
  (mapc (lambda ($r)
          (setq $word (replace-regexp-in-string (car $r) (cdr $r) $word)))
        (list ;;'("'"   . "\\\\'")
         '("\""  . "\\\\\"")
         '("?"   . "\\\\?")
         '("\n"  . " ")
         '("\("  . "\\\\(")
         '("\)"  . "\\\\)")
         '("\\[" . "\\\\[")
         '("\\]" . "\\\\]")
         '("\;"  . "\\\\;")
         '("\&"  . "\\\\&")
         '("\|"  . "\\\\|")))
  (save-window-excursion
    (start-process "OSX Say" osx-say-buffer
                   "say" "-v" osx-say-voice "-r"
                   (number-to-string (or $speed osx-say-speed)) $word)))

(show-paren-mode 1)

(global-company-mode)

(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)

(require 'yasnippet)
(yas-global-mode 1)


(provide 'init-locales)
;;; init-locales.el ends here
