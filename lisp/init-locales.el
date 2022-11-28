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
(require-package 'cnfonts);; 我喜欢undotree
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
        ["·" "`"]
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
        ["¥" "$"]
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
   (C . t)
   (awk . t)
   (dot . t)
   (plantuml . t)
   (clojure . t)
   (latex . t)
   (python . t)
   (csharp . t)
   (java . t)
   (powershell . t)
   (rust .  t)
   (perl . t)
   (js . t)
   (shell . t)
   (sql . t)
   (org . t)
   (ditaa . t)
   (emacs-lisp . t)
   (lisp . t) ;; slime - lisp interaction mode
   (gnuplot . t)))

(setq org-plantuml-jar-path "~/.emacs.d/plantuml.jar")

(setq org-confirm-babel-evaluate t)

(require 'org-tempo)

(setq org-structure-template-alist
      '(("a" . "export ascii\n")
        ("c" . "center\n")
        ("C" . "comment\n")
        ("e" . "example\n")
        ("E" . "export")
        ("h" . "export html\n")
        ("l" . "export latex\n")
        ("q" . "quote\n")
        ("s" . "src\n")
        ("sdt" . "src ditaa :file \n")
        ("sjs" . "src js :results output :exports both\n")
        ("sel" . "src elisp\n")
        ("scl" . "src clojure\n")
        ("ssh" . "src shell :results pp :exports both\n")
        ("scsx" . "src csharp :results pp :exports both\n")
        ("v" . "verse\n")))

(setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))




;; 使用org-bullets
(require-package 'org-bullets)
(add-hook 'org-mode-hook (lambda ()
                           (org-bullets-mode 1)
                           (org-indent-mode 1)))

;; ➽ ➼ ⬊
;; ⇘ ↘ ⟱ ⤋ ↡ ↧ ⇩ ⬇ ▾  ⇲ ➘ ➴ ᐁ ᐯ ᗐ ￫ ⬥ ᗒ ᐉ ➥ ➻
(setq org-ellipsis " ➘")


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

;; Set correct Python interpreter
(setq venv-postactivate-hook
      (list (lambda ()
              (setq python-shell-interpreter (concat python-shell-virtualenv-path "bin/python3")))))
(setq venv-postdeactivate-hook
      (list (lambda ()
              (setq python-shell-interpreter "python3"))))

(require-package 'pyenv-mode)
(pyenv-mode)


(setq org-babel-clojure-backend 'cider)


;; Region lines and then `M-x osx-say' to make OSX speak.

;; Adjust speak speed
(setq osx-say-speed
      (if (eq system-type 'windows-nt)
          70
        250))

;; Change voice
;; Kathy, Vicki, Victoria, Alex, Bruce, Fred
(setq osx-say-voice
      (if (eq system-type 'windows-nt)
          "2"
        "Samantha"))

(setq osx-say-buffer "*osx say*")

(defun osx-say-stop ()
  (interactive)
  (when (get-buffer osx-say-buffer)
    (kill-buffer osx-say-buffer)))

(setq osx-say-cmd
      (if (eq system-type 'windows-nt)
          "wsay"
        "say"))

(setq osx-say-speed-param
      (if (eq system-type 'windows-nt)
          "-s"
        "-r"))

(defun osx-say (&optional $word $speed)
  "Utilize `say' command that Mac OSX has/ or wsay on windows."
  (interactive)
  (unless (or
           (executable-find "wsay")
           (executable-find "say"))
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
                   osx-say-cmd "-v" osx-say-voice
                   osx-say-speed-param
                   (number-to-string (or $speed osx-say-speed)) $word)))

(show-paren-mode 1)

(global-company-mode)

;;(add-hook 'cider-repl-mode-hook #'company-mode)
;;(add-hook 'cider-mode-hook #'company-mode)

(require 'yasnippet)
(yas-global-mode 1)

(require-package 'ob-mermaid)
(require-package 'mermaid-mode)


(with-eval-after-load 'ox-latex
  ;; http://orgmode.org/worg/org-faq.html#using-xelatex-for-pdf-export
  ;; latexmk runs pdflatex/xelatex (whatever is specified) multiple times
  ;; automatically to resolve the cross-references.
  (setq org-latex-pdf-process '("latexmk -xelatex -quiet -shell-escape -f %f"))
  (add-to-list 'org-latex-classes
               '("elegantpaper"
                 "\\documentclass[lang=cn]{elegantpaper}
                 [NO-DEFAULT-PACKAGES]
                 [PACKAGES]
                 [EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (setq org-latex-listings 'minted)
  (add-to-list 'org-latex-packages-alist '("" "minted")))
(setq org-confirm-babel-evaluate nil)
(setq org-babel-clojure-backend 'cider)

(setq mac-command-key-is-meta t)
(require-package 'eldoc)
(load-library "eldoc")
(require-package 'diff-hl)
(load-library "diff-hl")

(which-key-mode)
;; epub reading
(require-package 'nov)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
(add-to-list 'auto-mode-alist '("\\.csx\\'" . csharp-mode))
(add-to-list 'auto-mode-alist '("\\.csproj\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.xaml\\'" . xml-mode))

(setq org-image-actual-width nil)

(define-key global-map (kbd "C-0") 'iterm-here)

(defun iterm-here ()
  (interactive)
  (dired-smart-shell-command "open -a iTerm $PWD" nil nil))


;; load java & C#

;; (defun my-csharp-repl ()
;;   "Switch to the CSharpRepl buffer, creating it if necessary."
;;   (interactive)
;;   (if-let ((buf (get-buffer "*csi*")))
;;       (pop-to-buffer buf)
;;     (progn (call-interactively 'csi)
;;            (when-let ((buf (get-buffer "*csi*")))
;;              (switch-to-buffer-other-window buf)))))

;; (defun my-csharp-repl ()
;;   "Switch to the CSharpRepl buffer, creating it if necessary."
;;   (interactive)
;;   (
;;    save-window-excursion
;;    (call-interactively 'csi)
;;    (if-let ((buf (get-buffer "*CSharpRepl*")))
;;        (pop-to-buffer buf)
;;      (when-let ((b (make-comint "CSharpRepl" "csharp")))
;;        (switch-to-buffer-other-window b)))))
;; (define-key csharp-mode-map (kbd "C-c C-z") 'my-csharp-repl)

;; (defun my-send (beg end)
;;   (interactive "r")
;;   (process-send-string (get-process "csi")
;;                        (concat (buffer-substring-no-properties beg end) "\n"))
;;   )


(setq system-time-locale "C")

;;(define-key js-mode-map [remap eval-last-sexp] #'js-comint-send-last-sexp)
;;(define-key js-mode-map (kbd "C-c b") 'js-send-buffer)


(eval-after-load
    'company
  '(add-to-list 'company-backends #'company-omnisharp))

(defun my-csharp-mode-setup ()
  (omnisharp-mode)
  (company-mode)
  (flycheck-mode)

  (setq indent-tabs-mode nil)
  (setq c-syntactic-indentation t)
  (c-set-style "ellemtel")
  (setq c-basic-offset 4)
  (setq truncate-lines t)
  (setq tab-width 4)
  (setq evil-shift-width 4)

  ;;csharp-mode README.md recommends this too
  ;;(electric-pair-mode 1)       ;; Emacs 24
  ;;(electric-pair-local-mode 1) ;; Emacs 25

  (local-set-key (kbd "C-c r r") 'omnisharp-run-code-action-refactoring)
  (local-set-key (kbd "M-.") 'omnisharp-go-to-definition)
  (local-set-key (kbd "C-c C-t") 'omnisharp-unit-test-buffer)
  (local-set-key (kbd "C-c C-p") 'omnisharp-unit-test-at-point)
  (local-set-key (kbd "C-c C-c") 'recompile))

;; (add-hook 'csharp-mode-hook
;;           (lambda ()
;;             (page-break-lines-mode)
;;             (setq-local compile-command
;;                         (concat "dotnet run "))))

(defun find-cs-project-name ()
  (let* ((project-name (file-name-directory (buffer-file-name)))
         (paths (file-name-directory (buffer-file-name)))
         (last-path (string-replace "/" "" (car (last (eshell-split-path paths))))))
    last-path))


(add-hook 'csharp-mode-hook 'my-csharp-mode-setup t)

;; (projectile-register-project-type 'dotnet '("Program.cs")
;;                                   :project-file "Program.cs"
;;                                   :compile "dotnet build"
;;                                   :test "dotnet test"
;;                                   :run (concat "dotnet run" (find-cs-project-name))
;;                                   :test-suffix "Tests")

(provide 'init-locales)
;;; init-locales.el ends here
