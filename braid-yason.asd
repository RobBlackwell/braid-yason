;;;; braid-yason.asd

(asdf:defsystem #:braid-yason
		:version "0.0.1"
		:author "Rob Blackwell"
		:description ""
		:serial t
		:depends-on (#:alexandria
								 #:yason
								 #:braid)
		:components ((:file "package")
								 (:file "braid-yason")))
