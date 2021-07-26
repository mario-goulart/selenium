(import test
	selenium
	(chicken pathname)
	(chicken process-context)
	(chicken irregex)
	(chicken string))

(define caps '((javascriptEnabled . #t)))

(with-firefox-webdriver
 "selenium-server-standalone.jar"
 (lambda ()
   (test (void) (set-url! (string-append "file://" (make-pathname (current-directory) "test.html"))))
   (test (void) (refresh-page!))
   (test #t (substring=? "<html>" (page-source)))
   (test "test" (page-title))

   (test #t (substring=? "iVB" (screenshot)))

   (test #t (alist-ref "ready" (status) equal?))
   (test "id" (car (vector-ref (car (sessions)) 0)))

   (define foo-elt (get-element-by-id "foo"))
   (test #t (element? foo-elt))
   (test "foo-id" (element-text foo-elt))
   (test #t (pair? (element-location foo-elt)))
   (test #t (pair? (element-location-in-view foo-elt)))
   (test "rgb(0, 0, 0)" (element-css-property-value foo-elt 'color))

   (define foo-input-elt (get-element-by-name "foo-input"))
   (test #t (element? foo-input-elt))
   (test "input" (element-tag-name foo-input-elt))
   (test "foo-input-value" (element-attribute-value foo-input-elt 'value))
   (test (void) (clear-element! foo-input-elt))
   (test (void) (set-element-value! foo-input-elt "new value"))

   (define foo1-elt (get-element-by-css-selector "#foo-1"))
   (test #t (element? foo1-elt))
   (test #t (element-enabled? foo1-elt))
   (test #t (element-displayed? foo1-elt))
   (test #t (pair? (element-size foo1-elt)))
   (test (void) (move-mouse-cursor-to! 1 1 foo1-elt))

   (test #t (element? (get-element-by-link-text "a-link")))

   (define foo-checkbox (get-element-by-xpath "//*[@id='a-checkbox']"))
   (test #t (element? foo-checkbox))
   (test (void) (click-element! foo-checkbox))
   (test #t (element-selected? foo-checkbox))

   (test #t (element? (get-element-by-partial-link-text "a-")))
   (test #t (element? (get-element-by-tag-name "iframe")))

   (test #t ((list-of? element?) (get-elements-by-class-name "foo")))
   (test #t ((list-of? element?) (get-elements-by-id "foo")))
   (test #t ((list-of? element?) (get-elements-by-tag-name "iframe")))
   (test #t ((list-of? element?) (get-elements-by-xpath "//*[@id='a-checkbox']")))
   (test #t ((list-of? element?) (get-elements-by-css-selector "#foo-1")))
   (test #t ((list-of? element?) (get-elements-by-link-text "a-link")))
   (test #t ((list-of? element?) (get-elements-by-partial-link-text "a-")))
   (test #t ((list-of? element?) (get-elements-by-name "foo-input")))
   
   (test #t (element? (active-element)))

   (test (void) (focus-frame! 0))

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

   (test (void) (navigate-backward!))
   (test (void) (navigate-forward!))

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

   (test (void) (set-implicit-timeout! 0))
   (test (void) (set-script-timeout! 30000))

   (test (void) (execute-javascript-async "var callback = arguments[1]; callback(console.log('Hello, ' + arguments[0]))" '("Jack")))
   (test (void) (execute-javascript "console.log('Hello, ' + arguments[0])" '("John")))

   (execute-javascript "confirm('example?')" '())
   (test "example?" (javascript-dialog-text))
   (test (void) (dismiss-javascript-dialog!))
   (execute-javascript "confirm('example?')" '())
   (test (void) (accept-javascript-dialog!))

   (test (void) (click-mouse-button! "left"))
   (test (void) (mouse-button-down!))
   (test (void) (mouse-button-up!))
   (test (void) (double-click-mouse-button!))
   
   (test #t ((list-of? string?) (window-handles)))

   (define win0 (window-handle))
   (test #t (string? win0))
   (test (void) (focus-window! win0))
   (test '() (close-window! win0)))
 capabilities: caps)
