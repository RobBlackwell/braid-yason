;;;; braid-yason.lisp

(in-package #:braid-yason)

;;;

(defun string-to-keyword (string)
  "Converts STRING to a keyword."
  (intern (string-upcase string) :keyword))

(defun encode-json (object)
  "Encodes OBJECT as a JSON string."
  (with-output-to-string (stream)
	(yason:encode object stream)))

(defun parse-json (string)
  "Takes a STRING containing a JSON description and returns a
corresponding lisp data structure."
  (yason:parse string
			   ;; Use options that favour readability.
			   :object-as :plist :object-key-fn #'string-to-keyword))

(defun encode-json-message-body (http-message)
  "Converts a response body to JSON and sets the content-type header
if not already present."
  (setf (braid:http-message-body http-message) (encode-json (braid:http-message-body http-message)))
  (unless (braid:http-message-header http-message :content-type)
    (setf (braid:http-message-header http-message :content-type) "application/json; charset=utf-8")))

(defun parse-json-message-body (http-message)
  "Parses MESSAGE body as a JSON string replacing body with the
corresponding Lisp data structure."
  (setf (braid:http-message-body http-message)
	(parse-json (braid:http-message-body http-message))))

(defun wrap-encode-json-response-body (request-handler)
  "Braid middleware that converts a response body to JSON and sets the
content-type header if not already present."
  (lambda (http-request)
    (let ((response (funcall request-handler http-request))
	  (encode-json-message-body response)
	  response))))

(defun wrap-parse-json-request-body (request-handler)
  "Braid middleware that..."
  (lambda (http-request)
    (parse-json-message-body http-request)
    (funcall request-handler http-request)))

;;; End
