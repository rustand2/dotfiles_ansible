(in-package #:nyxt-user)

;;; Reset ASDF registries to allow loading Lisp systems from
;;; everywhere.
(reset-asdf-registries)

;;; Load quicklisp. Not sure it works.
#-quicklisp
(let ((quicklisp-init
       (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(start-slynk)


(define-configuration web-buffer
  ((default-modes
    (pushnew 'nyxt/mode/vi:vi-normal-mode %slot-value%))))

(define-configuration :document-mode
  "Add basic keybindings."
  ((keyscheme-map
    (keymaps:define-keyscheme-map
     "custom" (list :import %slot-value%)
     ;; If you want to have VI bindings overriden, just use
     ;; `scheme:vi-normal' or `scheme:vi-insert' instead of
     ;; `scheme:emacs'.
     nyxt/keyscheme:vi-normal
     (list "J"     'switch-buffer-next
           "K"     'switch-buffer-previous
           "r"     'reload-buffer
           "C-x b" 'switch-buffer
           "M-x"   'execute-command
           "g i"   'nyxt/mode/document:focus-first-input-field
           "p p"   'url
           "C-h k" 'describe-key
           "C-h a" 'describe-any
           "C-h m" 'describe-mode
           "C-h s" 'describe-slot
           "C-h o" 'describe-object
           "C-h c" 'describe-command
           "C-h f" 'describe-function
           "C-h v" 'describe-variable
           )))))

(define-configuration nyxt/mode/hint:hint-mode
  ((nyxt/mode/hint:hinting-type :vi))
  ((nyxt/mode/hint:hints-alphabet "asdfghjkl")))

(define-configuration browser
  ;; Enable --remote --eval code evaluation.
  ((remote-execution-p t)
   (external-editor-program
    (list "emacsclient" "-nw" "-a" ""))))

;; Enable adblocker
;;(nyxt/mode/blocker:blocker-mode)

