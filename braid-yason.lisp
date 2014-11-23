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

;;;

(defun encode-json-body (message)
	"Converts a Message body to JSON and sets the content-type header
if not already present."
	(setf (braid:body message) (encode-json (braid:body message)))
	(unless (braid:header message :content-type)
		(setf (braid:header message :content-type) "application/json; charset=utf-8"))
	message)

(defun wrap-encode-json-body (request-handler)
	"Braid middleware that converts a response body to JSON and sets the
content-type header if not already present."
	(lambda (request)
		(encode-json-body (funcall request-handler request))))

;;;

(defun parse-json-body (message)
	"Parses MESSAGE body as a JSON string replacing body with the
corresponding Lisp data structure."
	(setf (braid:body message)
				(parse-json (braid:body message)))
	message)
	
(defun wrap-parse-json-body (response-handler)
	"Braid middleware that parses a JSON string message body replacing
it with a corresponding Lisp data structure."
	(lambda (response)
		(parse-json-body (funcall response-handler response))))

;;; End
