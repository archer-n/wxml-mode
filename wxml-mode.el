;;; wxml-mode.el --- WXML eidting mode -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'nxml-mode)
(require 'company)

;; keymap
(defvar wxml-mode-map
  (let ((map (make-keymap)))	;`sparse' doesn't allow binding to charsets.
    (define-key map ">" 'nxml-balanced-close-start-tag-inline)
    (define-key map (kbd "=") (lambda  () (interactive)
                                (if (wxml-capf-grab-attribute)
                                    (progn
                                      (insert-char ?=)
                                      (insert-char ?\" 2)
                                      (backward-char))
                                  (insert-char ?=))))
    map)
  "Keymap for WXML mode. ")

(easy-menu-define wxml-mode-menu wxml-mode-map
  "Menu for WXML mode."
  '("WXML"
    ["Format" wxml-format-buffer]))

(define-derived-mode wxml-mode nxml-mode "WXML"
   "Major mode for editing wxml."
   (setq-local nxml-slash-auto-complete-flag t)
   (setq-local completion-at-point-functions '(wxml-completion-at-point))
   (setq-local company-minimum-prefix-length 0)
   (setq-local company-idle-delay 0)
   (setq-local electric-pair-inhibit-predicate #'wxml-electric-pair-conservative-inhibit))

(add-to-list 'auto-mode-alist '("\\.wxml\\'" . wxml-mode))


;; electric
(defun wxml-electric-pair-conservative-inhibit (char)
  (or
   ;; I find it more often preferable not to pair when the
   ;; same char is next.
   (eq char (char-after))
   ;; Don't pair up when we insert the <.
   (eq char ?<)
   ;; I also find it often preferable not to pair next to a word.
   (eq (char-syntax (following-char)) ?w)))

;; completion
(defvar wxml-data-file (expand-file-name "wxml-data.json" (file-name-directory load-file-name)))
(defconst wxml-data (let ((json-object-type 'alist))
                      (json-read-file wxml-data-file)))

(defun wxml-completion-at-point ()
  (let ((tag (wxml-capf-grab-tag))
        (attribute (wxml-capf-grab-attribute)))
    (cond ((stringp tag) (wxml-tag-completion-at-point tag))
          ((stringp attribute) (wxml-attr-completion-at-point attribute)))))


(defun wxml-capf--get-annotation (str)
   "Generate annotation for STR.
STR is a candidate in a capf session.  See the implementation of
`wxml-completion-at-point'."
   (wxml-get-property 'annotation str))

(defun wxml-cap--get-docsig (str)
     "Generate docsig for STR.
STR is a candidate in a capf session.  See the implementation of
`wxml-completion-at-point'."
  (wxml-get-property 'signature str))

;; tag

(defun wxml-capf-grab-tag ()
  (let ((symbol (company-grab-line "<[[:space:]]*\\([[:alnum:]_-]\\)*")))
    (when (stringp symbol)
      (substring symbol 1))))

(defun wxml-completion-tag-name ()
  (wxml-completion-tags (cdr (assq 'tags wxml-data))))

(defun wxml-completion-tags (wxml-tags)
  (if (vectorp wxml-tags)
      (let ((list (append wxml-tags nil)))
        (mapcar (lambda (wxml-tag)
                  (let ((tag (cdr (assq 'name wxml-tag)))
                        (signature (cdr (assq 'description wxml-tag))))
                    (wxml-put-property tag 'signature signature)))
                list))))

(defun wxml-tag-completion-at-point (tag)
  (let ((beg (- (point) (length tag))) (end (point)))
                              (list beg
                                    end
                                    (completion-table-dynamic
                                     (lambda (_)
                                       (wxml-completion-tag-name)))
                                    :annotation-function #'wxml-capf--get-annotation
                                    :company-docsig #'wxml-cap--get-docsig)))

;; attr

(defun wxml-capf-grab-attribute ()
  (let ((symbol (company-grab-line "\\(?:<[[:alnum:]]+\\([[:space:]]+[[:alnum:]]+=\".*\"\\)*[[:space:]]+[[:alnum:]]+\\)")))
    (when (stringp symbol)
      (let* ((list (split-string symbol))
             (tag-name (substring (car list) 1))
             (attr-name (car (last list))))
        (wxml-put-property attr-name 'tag tag-name)))))


(defun wxml-completion-global-attr ()
  (let ((global-attrs (cdr (assq 'globalAttributes wxml-data))))
    (when global-attrs
      (let ((list (append global-attrs nil)))
        (mapcar (lambda (attr)
                  (let ((name (cdr (assq 'name attr)))
                        (annotation (cdr (assq 'attrType attr)))
                        (signature (cdr (assq 'description attr))))
                    (wxml-put-property name 'annotation annotation 'signature signature)))
                list)))))

(defun wxml-attr-completion-at-point (attr)
  (let* ((end (point))
         (beg (- end (length attr))))
                              (list beg
                                    end
                                    (completion-table-dynamic
                                     (lambda (_)
                                       (wxml-completion-global-attr)))
                                    :annotation-function #'wxml-capf--get-annotation
                                    :company-docsig #'wxml-cap--get-docsig)))
;; attr-value
(defconst wxml-get-attribute-value-re
  (concat "<[[:space:]]*\\([[:alnum:]_-]+\\)[^>]*"
          "[^[:alnum:]>_-]\\([[:alnum:]_-]+\\)=\"\\([[:alnum:]_-]*\\)")
  "Regexp of wxml attribute")

(defun wxml-grab-attribute-value ()
  (let ((sybmol (company-grab-line wxml-get-attribute-value-re)))
    (when sybmol
        (let* ((list (split-string sybmol))
               (tag-name (substring (car list) 1))
               (attribute (split-string (car (last list)) "=\""))
               (attr-name (car attribute))
               (attr-value (cadr attribute)))
          (wxml-put-property attr-name 'tag tag-name 'attr-value attr-value)
          attr-name))))

;;;; Text property

(defun wxml-get-property (field str)
  "Get the text property corresponding to FIELD in STR.
STR should be propertized by `wxml-put-property'.

What it actually does is prefix FIELD by `wxml-', and get that
text property."
  (get-text-property 0 (intern (concat "wxml-" (symbol-name field))) str))

(defun wxml-put-property (str &rest properties)
  "Set the text property of STR.
STR is the string to be modified.  PROPERTIES form a sequence of
PROPERTY VALUE pairs for test properties to add.  Each PROPERTY
is prefixed by \"wxml-\".  Propertized STR is returned."
  (let ((i 0)
        (len (length properties)))
    (while (< (1+ (* 2 i)) len)
      (let ((prop (nth (* 2 i) properties))
            (val (nth (1+ (* 2 i)) properties)))
        (put-text-property 0 (length str)
                           (intern (concat "wxml-" (symbol-name prop)))
                           val str))
      (cl-incf i)))
  str)


;; helper
(defun wxml-format-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(provide 'wxml-mode)
;;; wxml-mode.el ends here
