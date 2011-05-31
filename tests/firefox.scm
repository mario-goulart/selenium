(use html-tags test selenium)

(define test-page-source
  (<html>
   (<head> (<title> "test"))
   (<body>
    (<div> id: "foo" "foo-id")
    (<div> class: "foo" id: "foo-1" "foo-class-1")
    (<div> class: "foo" id: "foo-2" "foo-class-2")
    (<input> name: "foo-input" id: "foo-input" value: "foo-input-value")
    (<a> href: "a-location" "a-link")
    (<input> type: "checkbox" name: "a-checkbox" id: "a-checkbox")
    )))

(with-output-to-file "test.html"
  (cut display test-page-source))

(with-firefox-webdriver
 (make-pathname (current-directory) "profile")
 (lambda ()
   (set-url! (make-pathname (list "file://" (current-directory)) "test.html"))
   (test '("foo-1" "foo-2") (map (lambda (elt)
                                   (element-attribute-value elt 'id))
                                 (get-elements-by-class-name "foo")))
   (test "test" (page-title))
   (test "foo-id" (element-text (get-element-by-id "foo")))
   (test "foo-input-value" (element-value (get-element-by-id "foo-input")))
   (test "foo-1" (element-attribute-value (get-element-by-id "foo-1") 'id))
   (test "input" (element-tag-name (get-element-by-id "foo-input")))
   (test "div" (element-tag-name (get-element-by-id "foo-1")))
   (test "foo-class-1" (element-text (get-element-by-id "foo-1")))
   (test #t (element-displayed? (get-element-by-id "foo")))
   (test "a-link" (element-text (get-element-by-link-text "a-link")))
   (test #t (begin
              (click-element! (get-element-by-id "a-checkbox"))
              (element-selected? (get-element-by-id "a-checkbox"))))
   (quit!)
   (close-window! (window-handle))
   ))
