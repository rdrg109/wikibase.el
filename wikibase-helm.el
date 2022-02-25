(require 'helm)
(require 'request)
(require 'wikibase-wbsearchentities)

(defcustom wikibase-helm-suggest-item-actions
  '(("Insert QID at point" . wikibase-helm-insert-id-at-point)
    ("Visit item in browser" . wikibase-helm-visit-entity-in-browser)
    ("Kill QID" . wikibase-helm-kill-id))
  "List of actions for `wikibase-helm-suggest-item-source'."
  :group 'wikibase-helm
  :type '(alist :key-type string :value-type function))

(defcustom wikibase-helm-suggest-property-actions
  '(("Insert PID at point" . wikibase-helm-insert-id-at-point)
    ("Visit property in browser" . wikibase-helm-visit-entity-in-browser)
    ("Kill PID" . wikibase-helm-kill-id))
  "List of actions for `wikibase-helm-suggest-property-source'."
  :group 'wikibase-helm
  :type '(alist :key-type string :value-type function))

(defcustom wikibase-helm-suggest-lexeme-actions
  '(("Insert LID at point" . wikibase-helm-insert-id-at-point)
    ("Visit lexeme in browser" . wikibase-helm-visit-entity-in-browser)
    ("Kill LID" . wikibase-helm-kill-id))
  "List of actions for `wikibase-helm-suggest-lexeme-source'."
  :group 'wikibase-helm
  :type '(alist :key-type string :value-type function))

(defcustom wikibase-helm-suggest-form-actions
  '(("Insert FID at point" . wikibase-helm-insert-id-at-point)
    ("Visit form in browser" . wikibase-helm-visit-entity-in-browser)
    ("Kill FID" . wikibase-helm-kill-id))
  "List of actions for `wikibase-helm-suggest-form-source'."
  :group 'wikibase-helm
  :type '(alist :key-type string :value-type function))

(defun wikibase-helm-suggest-set-candidates (type)
  "Fetch and format candidates with type TYPE that matches pattern.

Allowed types are the ones defined in a Wikibase instance. Some
values that are accepted in some instances are: \"item\",
\"property\", \"lexeme\" and \"form\"."
  (let ((suggestions
         (unless (equal helm-pattern "")
           (wikibase-helm-suggest-fetch type helm-pattern))))
    (mapcar (lambda (suggestion)
              (cons
               ;; The strings are decoded so that Unicode coding character
               ;; (e.g. \345\247\223\346\260\217) are not shown
               (concat
                "QID: "
                (decode-coding-string (plist-get suggestion 'id) 'utf-8)
                ;; Check that suggestion has a "label" or
                ;; "description". If it doesn't the label is not
                ;; shown. It also avoids getting (wrong-type-argument
                ;; stringp nil) when calling decode-coding-string with
                ;; nil.
                (when (plist-get suggestion 'label)
                  (concat
                   "\nLabel: "
                   (decode-coding-string (plist-get suggestion 'label) 'utf-8)))
                (when (plist-get suggestion 'description)
                  (concat
                   "\nDescription: "
                   (decode-coding-string (plist-get suggestion 'description) 'utf-8))))
               (plist-get suggestion 'id)))
            suggestions)))

(defun wikibase-helm-suggest-fetch (type input)
  "Fetch suggestions for INPUT from XML buffer."
  (when (and input (not (equal input "")))
    (request-response-data
     (request wikibase-url-api
       :sync t
       :params `(("action" . "wbsearchentities")
                 ("search" . ,input)
                 ("format" . "xml")
                 ("errorformat" . "plaintext")
                 ("type" . ,type)
                 ,@wikibase-helm-wbsearchentities-custom-params)
       :parser (lambda ()
                 (cl-loop
                  with result-alist = (xml-get-children
                                       (assq 'search
                                             (cdar
                                              (xml-parse-region (point-min) (point-max))))
                                       'entity)
                  for i in result-alist collect
                  (list
                   'id
                   (alist-get 'id (cadr i))
                   'label
                   (alist-get 'label (cadr i))
                   'description
                   (alist-get 'description (cadr i)))))))))

(defvar wikibase-helm-suggest-item-source
  (helm-build-sync-source "Wikibase suggest"
    :candidates (lambda ()
                  (wikibase-helm-suggest-set-candidates "item"))
    :action 'wikibase-helm-suggest-item-actions
    :match-dynamic t
    :keymap helm-map
    :multiline t))

(defvar wikibase-helm-suggest-property-source
  (helm-build-sync-source "Wikibase suggest"
    :candidates (lambda ()
                  (wikibase-helm-suggest-set-candidates "property"))
    :action 'wikibase-helm-suggest-property-actions
    :match-dynamic t
    :keymap helm-map
    :multiline t))

(defvar wikibase-helm-suggest-lexeme-source
  (helm-build-sync-source "Wikibase suggest"
    :candidates (lambda ()
                  (wikibase-helm-suggest-set-candidates "lexeme"))
    :action 'wikibase-helm-suggest-lexeme-actions
    :match-dynamic t
    :keymap helm-map
    :multiline t))

(defvar wikibase-helm-suggest-form-source
  (helm-build-sync-source "Wikibase suggest"
    :candidates (lambda ()
                  (wikibase-helm-suggest-set-candidates "form"))
    :action 'wikibase-helm-suggest-form-actions
    :match-dynamic t
    :keymap helm-map
    :multiline t))

(defun wikibase-helm-visit-entity-in-browser (candidate)
  (let ((url (format wikibase-helm-suggest-search-url
                     (url-hexify-string candidate))))
    (browse-url url)))

(defun wikibase-helm-insert-id-at-point (candidate)
  (insert candidate))

(defun wikibase-helm-kill-id (candidate)
  (kill-new candidate))

(defun wikibase-helm-suggest-item ()
  (interactive)
  (helm-other-buffer 'wikibase-helm-suggest-item-source wikibase-helm-buffer-name))

(defun wikibase-helm-suggest-property ()
  (interactive)
  (helm-other-buffer 'wikibase-helm-suggest-property-source wikibase-helm-buffer-name))

(defun wikibase-helm-suggest-lexeme ()
  (interactive)
  (helm-other-buffer 'wikibase-helm-suggest-lexeme-source wikibase-helm-buffer-name))

(defun wikibase-helm-suggest-form ()
  (interactive)
  (helm-other-buffer 'wikibase-helm-suggest-form-source wikibase-helm-buffer-name))

(provide 'wikibase-helm)
