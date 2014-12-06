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

;;; TODO REVIEW THIS ..

(defun encode-json-body (http-response)
  "Converts a response body to JSON and sets the content-type header
if not already present."
  (setf (braid:http-response-body http-response) (encode-json (braid:http-response-body http-response)))
  (unless (braid:http-response-header http-response :content-type)
	(setf (braid:http-response-header http-response :content-type) "application/json; charset=utf-8"))
  http-response)

(defun wrap-encode-json-body (request-handler)
  "Braid middleware that converts a response body to JSON and sets the
content-type header if not already present."
  (lambda (http-request)
	(encode-json-body (funcall request-handler http-request))))

;;;

(defun parse-json-body (http-response)
  "Parses MESSAGE body as a JSON string replacing body with the
corresponding Lisp data structure."
  (setf (braid:http-response-body http-response)
		(parse-json (braid:http-response-body http-response)))
  http-response)
	
;; (defun wrap-parse-json-body (response-handler)
;; 	"Braid middleware that parses a JSON string message body replacing
;; it with a corresponding Lisp data structure."
;; 	(lambda (response)
;; 		(parse-json-body (funcall response-handler response))))

;;; End
