(import test
	selenium
	(chicken pathname)
	(chicken process-context)
	(chicken irregex))

(define caps '((javascriptEnabled . #t)))

(with-firefox-webdriver
 (lambda ()
   (set-url! (string-append "file://" (make-pathname (current-directory) "test.html")))
   (test '("foo-1" "foo-2") (map (lambda (elt)
                                   (element-attribute-value elt 'id))
                                 (get-elements-by-class-name "foo")))
   (test "test" (page-title))
   (test "foo-id" (element-text (get-element-by-id "foo")))
   (test "foo-input-value" (element-attribute-value (get-element-by-id "foo-input") 'value))
   (test "foo-1" (element-attribute-value (get-element-by-id "foo-1") 'id))
   (test "input" (element-tag-name (get-element-by-id "foo-input")))
   (test "div" (element-tag-name (get-element-by-id "foo-1")))
   (test "foo-class-1" (element-text (get-element-by-id "foo-1")))
   (test #t (element-displayed? (get-element-by-id "foo")))
   (test "a-link" (element-text (get-element-by-link-text "a-link")))
   (test #t (begin
              (click-element! (get-element-by-id "a-checkbox"))
              (element-selected? (get-element-by-id "a-checkbox"))))

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

   (define win0 (window-handle))

   (test #t (string? win0))
   (test 0 (alist-ref "status" (focus-window! win0) equal?))

   (close-window! win0))
 capabilities: caps)
