;;;; braid-yason.lisp

(in-package #:braid-yason)

(defun encode-json (x)
	"Encodes x in JSON return the JSON as a string."
	(with-output-to-string (s)
		(yason:encode x s)))

(defun decode-json (json)
	""
	(yason:parse json))

(defun wrap-json-response (handler)
	"Braid middleware that converts response body to JSON and sets the
content-type header if not already present."
	(lambda (request)
		(let* ((response (funcall handler request))
					 (json (encode-json (braid:message-body response))))
			(setf (braid:message-body response) json)
			(unless (braid:message-header response :content-type)
				(setf (braid:message-header response :content-type) "application/json; charset=utf-8"))
			response)))

;; wrap-json-params

(defun wrap-json-body (handler)
	"Braid middleware that parses a JSON body setting message-body to
the corresponding data structure."
	(lambda (request)
		(let ((response (funcall handler request)))
			(setf (braid:message-body response)
						(json-decode (braid:message-body response)))
			response)))






