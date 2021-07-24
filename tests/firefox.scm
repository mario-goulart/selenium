(import test
	selenium
	(chicken pathname)
	(chicken process-context)
	(chicken irregex))

(define caps '((javascriptEnabled . #t)))

(with-firefox-webdriver
 (lambda ()
   (test 0 (alist-ref "status" (set-url! (string-append "file://" (make-pathname (current-directory) "test.html"))) equal?))
   (test "test" (page-title))

   (define foo-elt (get-element-by-id "foo"))
   (test #t (element? foo-elt))
   (test "foo-id" (element-text foo-elt))

   (define foo-input-elt (get-element-by-xpath "//*[@id='foo-input']"))
   (test #t (element? foo-input-elt))
   (test "foo-input-value" (element-attribute-value foo-input-elt 'value))

   (define foo1-elt (get-element-by-id "foo-1"))
   (test "input" (element-tag-name foo-input-elt))
   (test #t (element-displayed? foo-elt))
   (test "a-link" (element-text (get-element-by-link-text "a-link")))

   (define foo-checkbox (get-element-by-id "a-checkbox"))
   (test (void) (click-element! foo-checkbox))
   (test #t (element-selected? foo-checkbox))

   (test #t ((list-of? element?) (get-elements-by-class-name "foo")))
   (test #t (element? (active-element)))

   (test 0 (alist-ref "status" (focus-frame! 0) equal?))

   (set-url! "http://example.com/")
   (test "http://example.com/" (current-url))

   ;;; Cookies
   (set-cookie! "foo" "bar")
   (let* ((cookies (get-cookies))
          (cookie (car cookies)))
     (test 1 (length cookies))
     (test "foo" (cookie-name cookie))
     (test "bar" (cookie-value cookie))
     (test "example.com" (cookie-domain cookie))
     (test "/" (cookie-path cookie))
     (test #f (cookie-secure? cookie)))

   (let* ((cookies (get-cookies-by-name "foo"))
          (cookie (car cookies)))
     (test 1 (length cookies))
     (test "foo" (cookie-name cookie))
     (test "bar" (cookie-value cookie))
     (test "example.com" (cookie-domain cookie))
     (test "/" (cookie-path cookie))
     (test #f (cookie-secure? cookie)))

   (let* ((cookies (get-cookies-by-value "bar"))
          (cookie (car cookies)))
     (test 1 (length cookies))
     (test "foo" (cookie-name cookie))
     (test "bar" (cookie-value cookie))
     (test "example.com" (cookie-domain cookie))
     (test "/" (cookie-path cookie))
     (test #f (cookie-secure? cookie)))

   (let* ((cookies (get-cookies-by-name (irregex "f.*")))
          (cookie (car cookies)))
     (test 1 (length cookies))
     (test "foo" (cookie-name cookie))
     (test "bar" (cookie-value cookie))
     (test "example.com" (cookie-domain cookie))
     (test "/" (cookie-path cookie))
     (test #f (cookie-secure? cookie)))

   (test 0 (alist-ref "status" (navigate-backward!) equal?))
   (test 0 (alist-ref "status" (navigate-forward!) equal?))

   ;; The firefox webdriver aparently doesn't set the cookie path...
   ;; (set-cookie! "foo" "bar" path: "/bar")
   ;; (let* ((cookies (get-cookies-by-path "/bar"))
   ;;        (cookie (car cookies)))
   ;;   (test 1 (length cookies))
   ;;   (test "foo" (cookie-name cookie))
   ;;   (test "/bar" (cookie-value cookie))
   ;;   (test "" (cookie-domain cookie))
   ;;   (test "/bar" (cookie-path cookie))
   ;;   (test #f (cookie-secure? cookie)))

   (test 0 (alist-ref "status" (set-implicit-wait-time! 0) equal?))

   (test (void) (execute-javascript-async "var callback = arguments[1]; callback(console.log('Hello, ' + arguments[0]))" '("Jack")))
   (test (void) (execute-javascript "console.log('Hello, ' + arguments[0])" '("John")))

   (execute-javascript "confirm('example?')" '())
   (test "example?" (javascript-dialog-text))
   (test 0 (alist-ref "status" (dismiss-javascript-dialog!) equal?))
   (execute-javascript "confirm('example?')" '())
   (test 0 (alist-ref "status" (accept-javascript-dialog!) equal?))

   (test #t ((list-of? string?) (window-handles)))

   (define win0 (window-handle))
   (test #t (string? win0))
   (test 0 (alist-ref "status" (focus-window! win0) equal?))
   (test 0 (alist-ref "status" (close-window!) equal?)))
 capabilities: caps)
