(require 'csv-mode)
(require 'request)

(defcustom wikidata-show-string-func
  (lambda (string &optional point)
    (message string))
  "Store a lambda function for showing a string

The first parameter is the string to show. The second parameter
is optional and it's the point in which the message is intended
to be shown.

Some users might want to set this to a function that shows a
message below the point instead of showing it in the
minibuffer.")

(defcustom wikidata-properties-fields
  '(myProperty
    myPropertyType
    propertyLabel
    propertyDescription
    propertyAltLabel)
  "Fields that are retrieved when querying the data using
  `wikidata-sparql-get-properties'.")

(defcustom wikidata-properties-completion-list-format
  '((propertyLabel)
    (myProperty (prefix " (" postfix ")")))
  "a")

(defcustom wikidata-sparql-get-properties "
SELECT
  ?propertyId
  ?propertyType
  ?propertyUriLabel
  ?propertyUriDescription
  ?propertyUriAltLabel
{
  ?propertyUri wikibase:propertyType ?wikibasePropertyType.

  # It is necessary to explicitly mention a language. Otherwise, the
  # API returns nothing in those fields that intend to use a label.

  SERVICE wikibase:label { bd:serviceParam wikibase:language \"en\". }

  # This avoids that the entire URL to the items are retrieved. That
  # is, only the necessary information is sent by the server.

  BIND(STRAFTER(STR(?propertyUri), '/entity/') AS ?propertyId)
  BIND(STRAFTER(STR(?wikibasePropertyType), '/ontology#') AS ?propertyType)
}"
  "SPARQL query for getting all existing properties")

(defvar wikidata-id-entity-re "\\(Q\\|L\\|P\\)[0-9]+"
  "Regular expression that matches an identifier of a Wikidata
entity at point")

(defvar wikidata-id-phabricator-re "T[0-9]+"
  "Regular expression that matches an identifier of a ticket at
Wikimedia Phabricator")

(defvar wikidata-cache-properties nil
  "Cache for Wikidata properties (those whose ID start with P)")

(defvar wikidata-cache-items nil
  "Cache for Wikidata items (those whose ID start with Q)")

(defvar wikidata-cache-lexemes nil
  "Cache for Wikidata lexemes (those whose ID start with L)")

(defvar wikidata-notify-function
  (lambda (&rest r)
    (message "wikidata.el: %s" (plist-get r :body)))
  "Function for showing notifications of this package")

(defun wikidata-read-property (&optional field)
  "Prompt for a property and return it

FIELD is the property that the function need to return."
  ;; FIXME: Return the corresponding property
  (unless wikidata-cache-properties
    (error "wikidata-cache-properties is nil"))
  (unless field
    (setq field 'myProperty))
  (let (value)
    (setq value
          (completing-read
           "Prompt: "
           (mapcar
            (lambda (prop)
              (let (str _)
                (dolist (item wikidata-properties-completion-list-format)
                  (setq _ (alist-get (car item) prop))
                  ;; A conditional so that we don't show those fields that
                  ;; doesn't exist for a given item.
                  (when (and _ (not (equal _ "")))
                    (setq str (concat
                               str
                               (plist-get (cadr item) 'prefix)
                               (alist-get (car item) prop)
                               (plist-get (cadr item) 'postfix)))))
                (setq str (propertize str 'wikidata (alist-get field prop)))
                str))
            wikidata-cache-properties)))
    (get-text-property 0 'wikidata value)))

(defun wikidata-cache-properties-store (data)
  "Store data of properties in `wikidata-cache-pid'"
  ;; We clear the previous values
  (setq wikidata-cache-properties nil)
  ;; Iterate through the returned lines
  (with-temp-buffer
    (insert data)
    (beginning-of-buffer)
    (let (row)
      (while (not (eobp))
        (setq row (csv--collect-fields (point-at-eol)))
        ;; By default, data is returned in CSV format since that's
        ;; the most lightweight alternative for retrieving data from
        ;; the API. The data is stored in association
        ;; lists. Therefore, getting the name of the fields is
        ;; necessary. This loop sets the label for each item in a row.
        (dotimes (i (length row))
          (setf (nth i row) (cons (nth i wikidata-properties-fields)
                                  (nth i row))))
        (push row wikidata-cache-properties)
        (forward-line 1)))))

(defun wikidata-cache-properties-get ()
  "Get data from all existing properties"
  (request
    "https://query.wikidata.org/sparql"
    :type "GET"
    :headers '(("Accept" . "text/csv"))
    :params `(("query" . ,wikidata-sparql-get-properties))
    :success
    (lambda (&rest r)
      ;; FIXME: Omit table header. It contains the labels of the rows.
      ;; FIXME: property must equal the identifier of the property
      ;; FIXME: propertyType must not be a URL
      (wikidata-cache-properties-store (plist-get r :data))
      (funcall wikidata-notify-function
               :body "Properties were retrieved"))))

(defun wikidata-visit-entity-at-point ()
  "Open the Q, L, P or T entity at point

With a prefix argument, insert the identifier to the kill ring."
  ;; FIXME: Avoid using org-in-regexp since this implies that the user
  ;; need to have Org Mode installed. org-in-regexp is used to get the
  ;; boundaries of the string matching a given regular expression
  (interactive)
  (catch 'done
    (let (tap url)
      (setq url
            (cond ((setq tap (org-in-regexp wikidata-id-entity-re))
                   (concat "https://www.wikidata.org/entity/"
                           (buffer-substring-no-properties (car tap) (cdr tap))))
                  ((setq tap (org-in-regexp wikidata-id-phabricator-re))
                   (concat "https://phabricator.wikimedia.org/"
                           (buffer-substring-no-properties (car tap) (cdr tap))))
                  (t
                   (throw 'done "The thing at point is not an identifier"))))
      (browse-url url))))

(defun wikidata-visit-entity (&optional entity)
  (interactive (list (or (org-in-regexp wikidata-id-entity-re)
                         (org-in-regexp wikidata-id-phabricator-re))))
  (let (url)
    (when (eq (type-of entity) 'cons)
      (setq entity (buffer-substring-no-properties (car entity) (cdr entity))))
    (setq url (cond ((string-match wikidata-id-entity-re entity)
                     (concat "https://www.wikidata.org/entity/" entity))
                    ((string-match wikidata-id-phabricator-re entity)
                     (concat "https://phabricator.wikimedia.org/" entity))))
    (browse-url url)))

(defun wikidata-show-label-entity-id-at-point ()
  "Show the label of the entity at point.

The function doesn't support LIDs yet.

It calls `wikidata-show-string-func' to show the message."
  (interactive)
  (catch 'done
    (let ((id (org-in-regexp wikidata-id-entity-re))
          point)
      (unless id
        (throw 'done "The point is not on a ID"))
      (setq point (point))
      (setq id (buffer-substring-no-properties (car id) (cdr id)))
      (request
        "https://www.wikidata.org/w/api.php"
        :type "GET"
        :params `(("action" . "wbgetentities")
                  ("props" . "labels")
                  ("ids" . ,id)
                  ("languages" . "en")
                  ("format" . "json"))
        :success `(lambda (&rest r)
                    (let* ((json (plist-get r :data))
                           (data (json-read-from-string json))
                           ;; FIXME: This doesn't work for LIDs.
                           (label (alist-get 'value
                                             (cdar
                                              (alist-get 'labels (cdadar data))))))
                      (funcall wikidata-show-string-func label ,point)))))))

(defun wikidata-insert-property ()
  (interactive)
  (insert (wikidata-read-property 'myProperty)))

(defun wikidata-visit-property ()
  (interactive)
  (wikidata-visit-entity
   (wikidata-read-property 'myProperty)))

(defun wikidata-show-query-details ()
  "Show the details of the query in the current buffer

Information on query explanations can be found in
https://www.mediawiki.org/wiki/Wikidata_Query_Service/User_Manual#Explain_Query"
  (interactive)
  (let* ((string (buffer-substring-no-properties (point-min) (point-max)))
         (query (url-hexify-string string))
         (url (concat
               "https://query.wikidata.org/sparql?query="
               query
               "&explain=details")))
    (w3m url)))

(provide 'wikidata)
