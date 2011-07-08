(module selenium
  (;; Session
   quit! capabilities set-implicit-wait-time!

   ;; Javascript
   execute-javascript execute-javascript-async javascript-dialog-text
   set-javascript-dialog-text! dismiss-javascript-dialog!
   accept-javascript-dialog!

   ;; URLs
   set-url! current-url

   ;; Navigation
   navigate-forward! navigate-backward!

   ;; IME: Input Method Editor
   ime-available-engines ime-active-engine ime-activated? ime-deactivate!
   ime-activate!

   ;; Frames & windows
   focus-frame! focus-window! close-window! window-handle window-handles

   ;; User input
   user-input-speed set-user-input-speed!

   ;; Elements
   active-element get-element-by-id get-element-by-name get-element-by-class-name
   get-element-by-css-selector get-element-by-link-text
   get-element-by-partial-link-text get-element-by-tag-name get-element-by-xpath
   element-value element-tag-name element-text select-element! toggle-element!
   click-element! clear-element! hover-element! drag-element! element-enabled?
   element-selected? element-displayed? element-location element-location-in-view
   element-size element-css-property-value set-element-value!
   active-element-send-modifier! element-attribute-value same-element?
   get-elements-by-class-name get-elements-by-css-selector get-elements-by-id
   get-elements-by-name get-elements-by-link-text
   get-elements-by-partial-link-text get-elements-by-tag-name
   get-elements-by-xpath

   ;; Cookies
   get-cookies set-cookie! get-cookies-by-name get-cookies-by-value
   get-cookies-by-domain get-cookies-by-path
   cookie-name cookie-value cookie-path cookie-domain cookie-secure? cookie-expiry


   ;; Pages
   page-source page-title refresh-page!

   ;; Screen
   screen-orientation screenshot

   ;; Mouse actions
   move-mouse-cursor-to! click-mouse-button! mouse-button-down! mouse-button-up!
   double-click-mouse-button!

   ;; Firefox webdriver
   with-firefox-webdriver

   ;; "Remote" webdriver
   with-remote-webdriver
   )

(import chicken scheme)
(use json http-client intarweb uri-common srfi-13 srfi-1 regex
     data-structures extras files ports tcp srfi-18 posix)

(include "keys.scm")

(tcp-buffer-size 2048)

;;; Parameters to be set by webdrivers
(define session-identifier (make-parameter #f))
(define desired-capabilities (make-parameter #f))
(define command-executor-scheme (make-parameter #f))
(define command-executor-host (make-parameter #f))
(define command-executor-port (make-parameter #f))
(define command-executor-path (make-parameter #f))


(define (command-executor)
  (conc (command-executor-scheme) "://" (command-executor-host) ":"
        (command-executor-port) (command-executor-path)))


(define (selenium-request method path #!optional (data ""))

  (define (request-error type message)
    (error 'selenium-request type message))

  (let* ((req-headers
          (lambda (data)
            (headers
             `((content-type application/json) ;charset=UTF-8")
               (accept "application/json")
               (connection Keep-Alive)
               (content-length ,(string-length data))))))
           (response
          (parameterize ((max-redirect-depth 0)) ;; hack to workaround the 303 bug
            (handle-exceptions exn
              (cond (((condition-predicate 'server-error) exn) ;; handle 500
                     (with-input-from-string
                         (get-condition-property exn 'server-error 'body)
                       json-read))
                    (((condition-predicate 'redirect-depth-exceeded) exn)
                     (handle-exceptions exn ;; Probably a redirection code (302)
                       (raise exn)
                       (call-with-input-request
                        (make-request
                         method: 'GET
                         uri: (get-condition-property exn 'redirect-depth-exceeded 'uri)
                         headers: (req-headers ""))
                        ""
                        json-read)))
                    (else (raise exn)))
              (call-with-input-request
               (make-request
                method: method
                uri: (uri-reference (make-pathname (command-executor) path))
                headers: (req-headers data))
               data
               json-read))))
         (response-code (alist-ref "status" (vector->list response) equal?)))
    (case response-code
      ((0) (vector->list response))
      ((7) (request-error
            'NoSuchElement
            (string-append
             "An element could not be located on the page using the given "
             "search parameters.")))
      ((8) (request-error
            'NoSuchFrame
            (string-append
             "A request to switch to a frame could not be satisfied because "
             "the frame could not be found.")))
      ((9) (request-error
            'UnknownCommand
            (string-append
             "The requested resource could not be found, or a request was "
             "received using an HTTP method that is not supported by the "
             "mapped resource.")))
      ((10) (request-error
             'StaleElementReference
             (string-append
              "An element command failed because the referenced element is "
              "no longer attached to the DOM.")))
      ((11) (request-error
             'ElementNotVisible
             (string-append
              "An element command could not be completed because the element "
              "is not visible on the page.")))
      ((12) (request-error
             'InvalidElementState
             (string-append
              "An element command could not be completed because the element is "
              "in an invalid state (e.g. attempting to click a disabled element).")))
      ((13) (request-error
             'UnknownError
             "An unknown server-side error occurred while processing the command."))
      ((15) (request-error
             'ElementIsNotSelectable
             "An attempt was made to select an element that cannot be selected."))
      ((17) (request-error
             'JavaScriptError
             "An error occurred while executing user supplied JavaScript."))
      ((19) (request-error
             'XPathLookupError
             "An error occurred while searching for an element by XPath."))
      ((23) (request-error
             'NoSuchWindow
             (string-append
              "A request to switch to a different window could not be "
              "satisfied because the window could not be found.")))
      ((24) (request-error
             'InvalidCookieDomain
             (string-append
              "An illegal attempt was made to set a cookie under a different "
              "domain than the current page.")))
      ((25) (request-error
             'UnableToSetCookie
             "A request to set a cookie's value could not be satisfied."))
      ((28) (request-error
             'Timeout
             "A command did not complete before its timeout expired."))
      (else (request-error 'UnknownError (conc "Unexpected response status code: "
                                               response-code)))
      )))


(define (remote-execute method path #!key (json-args '()) (url-args '()))
  (let ((data (with-output-to-string
                (lambda ()
                  (json-write (list->vector json-args))))))
    (when (session-identifier)
      (set! path (apply sprintf (append (list path (session-identifier))
                                        url-args))))
    (selenium-request method path data)))

(define (response-value response)
  (alist-ref "value" response equal?))

;;;
;;; Commands
;;;

;;; Session
(define (start-session)
  (let ((response
         (remote-execute 'POST "/session"
                         json-args: `((desiredCapabilities
                                       . ,(list->vector
                                           (desired-capabilities)))))))
    (or (alist-ref "sessionId" response equal?)
        (error 'start-session "Could not get a session identifier."))))


(define (quit!)
  (remote-execute 'DELETE "/session/~A"))


(define (capabilities)
  (response-value (remote-execute 'GET "/session/~A")))


(define (set-implicit-wait-time! time-ms)
  (remote-execute 'POST "/session/~A/timeouts/implicit_wait"
                  json-args: `((ms . ,time-ms))))


;;; Javascript
(define (execute-javascript script args)
  (response-value
   (remote-execute 'POST "/session/~A/execute"
                   json-args: `((script . ,script)
                                (args . ,args)))))


(define (execute-javascript-async script args)
  (response-value
   (remote-execute 'POST "/session/~A/execute_async"
                   json-args: `((script . ,script)
                                (args . ,args)))))


(define (javascript-dialog-text)
  (response-value (remote-execute 'GET "/session/~A/alert_text")))


(define (set-javascript-dialog-text! text)
  (remote-execute 'POST "/session/~A/alert_text"
                  json-args: `((keysToSend . ,text))))


(define (dismiss-javascript-dialog!)
  (remote-execute 'POST "/session/~A/dismiss_alert"))


(define (accept-javascript-dialog!)
  (remote-execute 'POST "/session/~A/accept_alert"))



;;; URLs
(define (set-url! url)
  (remote-execute 'POST "/session/~A/url"
                  json-args: `((url . ,url))))


(define (current-url)
  (response-value (remote-execute 'GET "/session/~A/url")))



;;; Navigation
(define (navigate-forward!)
  (remote-execute 'POST "/session/~A/forward"))


(define (navigate-backward!)
  (remote-execute 'POST "/session/~A/back"))



;;; IME: Input Method Editor
(define (ime-available-engines)
  (response-value (remote-execute 'GET "/session/~A/ime/available_engines")))


(define (ime-active-engine)
  (response-value (remote-execute 'GET "/session/~A/ime/active_engine")))


(define (ime-activated?)
  (response-value (remote-execute 'GET "/session/~A/ime/activated")))


(define (ime-deactivate!)
  (remote-execute 'POST "/session/~A/ime/deactivate"))


(define (ime-activate! engine)
  (remote-execute 'POST "/session/~A/ime/activate"
                  json-args: `((engine . ,engine))))



;;; Frames & windows

(define (focus-frame! id)
  (remote-execute 'POST "/session/~A/frame"
                  json-args: `((id . ,id))))


(define (focus-window! id)
  (remote-execute 'POST "/session/~A/window"
                  json-args: `((id . ,id))))


(define (close-window! id)
  (remote-execute 'DELETE "/session/~A/window"
                  json-args: `((id . ,id))))


(define (window-handle)
  (response-value (remote-execute 'GET "/session/~A/window_handle")))


(define (window-handles)
  (response-value (remote-execute 'GET "/session/~A/window_handles")))



;;; User input

(define (user-input-speed)
  (response-value (remote-execute 'GET "/session/~A/speed")))


(define (set-user-input-speed! speed)
  (remote-execute 'POST "/session/~A/speed"
                  json-args: `((speed . ,speed))))


;;; Elements

(define-record element id)


(define (active-element)
  (make-element
   (response-value
    (remote-execute 'POST "/session/~A/element/active"))))


(define (get-element value #!key using)
  (let ((response
         (response-value
          (remote-execute 'POST "/session/~A/element"
                          json-args: `((using . ,(->string using))
                                       (value . ,(->string value)))))))
    (make-element
     (uri-encode-string (alist-ref "ELEMENT" (vector->list response) equal?)))))


(define (element-property property #!key (method 'GET))
  (lambda (elt #!key (using 'id))
    (response-value
     (remote-execute method
                     (conc "/session/~A/element/~A/" property)
                     url-args: (list (element-id elt))))))

(define (get-element-by-id id) (get-element id using: "id"))

(define (get-element-by-name name) (get-element name using: "name"))

(define (get-element-by-class-name name) (get-element name using: "class name"))

(define (get-element-by-css-selector selector)
  (get-element selector using: "css selector"))

(define (get-element-by-link-text text)
  (get-element text using: "link text"))

(define (get-element-by-partial-link-text text)
  (get-element text using: "partial link text"))

(define (get-element-by-tag-name name)
  (get-element name using: "tag name"))

(define (get-element-by-xpath xpath)
  (get-element xpath using: "xpath"))

(define element-value (element-property 'value))
(define element-tag-name (element-property 'name))
(define element-text (element-property 'text))

(define select-element! (element-property 'selected method: 'POST))
(define toggle-element! (element-property 'toggle method: 'POST))
(define click-element! (element-property 'click method: 'POST))
(define clear-element! (element-property 'clear method: 'POST))
(define hover-element! (element-property 'hover method: 'POST))

(define (drag-element! elt x y)
  (remote-execute 'POST
                  (conc "/session/~A/element/~A/drag")
                  url-args: (list (element-id elt))
                  json-args: `((x . ,x)
                               (y . ,y))))


(define element-enabled? (element-property 'enabled))
(define element-selected? (element-property 'selected))
(define element-displayed? (element-property 'displayed))

(define (element-location elt)
  (let ((res ;; #(("x" . <coord-x>) ("y" . <coord-y>))
         (response-value
          (remote-execute 'GET
                          (conc "/session/~A/element/~A/location")
                          url-args: (list (element-id elt))))))
    (cons (cdr (vector-ref res 0))
          (cdr (vector-ref res 1)))))

(define (element-location-in-view elt)
  (let ((res ;; #(("x" . <coord-x>) ("y" . <coord-y>))
         (response-value
          (remote-execute 'GET
                          (conc "/session/~A/element/~A/location_in_view")
                          url-args: (list (element-id elt))))))
    (cons (cdr (vector-ref res 0))
          (cdr (vector-ref res 1)))))

(define (element-size elt)
  (let ((res ;; #(("width" . <coord-x>) ("height" . <coord-y>))
         (response-value
          (remote-execute 'GET
                          (conc "/session/~A/element/~A/size")
                          url-args: (list (element-id elt))))))
    (cons (cdr (vector-ref res 0))
          (cdr (vector-ref res 1)))))

(define (element-css-property-value elt property)
  (response-value (remote-execute 'GET "/session/~A/element/~A/css/~A"
                                  url-args: (list (element-id elt) property))))


(define (set-element-value! elt value)
  (let ((value (if (string? value)
                   (map ->string (string->list value))
                   value)))
    (remote-execute 'POST "/session/~A/element/~A/value"
                    url-args: (list (element-id elt))
                    json-args: `((value . ,value)))))


(define (active-element-send-modifier! key down?)
  (remote-execute 'POST "/session/~A/modifier"
                  json-args: `((value . ,key)
                               (isdown . ,down?))))


(define (element-attribute-value elt attrib)
  (response-value
   (remote-execute 'GET
                   "/session/~A/element/~A/attribute/~A"
                   url-args: (list (element-id elt) (->string attrib)))))


(define (same-element? elt1 elt2)
  (response-value
   (remote-execute 'GET "/session/~A/element/~A/equals/~A"
                   url-args: (list (element-id elt1)
                                   (element-id elt2)))))


(define (get-elements value #!optional (using "class name"))
  (let ((res (response-value
              (remote-execute 'POST "/session/~A/elements"
                              json-args: `((using . ,using)
                                           (value . ,value))))))
    (map (lambda (result)
           (make-element
            (uri-encode-string
             (alist-ref "ELEMENT" (vector->list result) equal?))))
         res)))

(define get-elements-by-class-name get-elements)

(define (get-elements-by-css-selector selector)
  (get-elements selector using: "css selector"))

(define (get-elements-by-id id)
  (get-elements id using: "id"))

(define (get-elements-by-name name)
  (get-elements name using: "name"))

(define (get-elements-by-link-text link-text)
  (get-elements link-text using: "link text"))

(define (get-elements-by-partial-link-text partial-link-text)
  (get-elements partial-link-text using: "partial link text"))

(define (get-elements-by-tag-name tag-name)
  (get-elements tag-name using: "tag name"))

(define (get-elements-by-xpath xpath)
  (get-elements xpath using: "xpath"))


;;; Cookies

(define-record cookie name value path domain secure? expiry)

(define (get-cookies)
  (let ((cookies (response-value (remote-execute 'GET "/session/~A/cookie"))))
    (map (lambda (c)
           (apply make-cookie (map cdr (vector->list c))))
         cookies)))

(define (name/regex-equal obj)
  (if (string? obj)
      equal?
      string-match))

(define (get-cookies-by field str/regex)
  (let ((cookies (get-cookies))
        (compare (name/regex-equal str/regex)))
    (filter-map (lambda (cookie)
                  (and (compare str/regex
                                (case field
                                  ((name) (cookie-name cookie))
                                  ((value) (cookie-value cookie))
                                  ((domain) (cookie-domain cookie))
                                  ((path) (cookie-path cookie))
                                  (else (error 'get-cookies-by "Invalid field"
                                               field))))
                       cookie))
                cookies)))

(define (get-cookies-by-name name/regex) (get-cookies-by 'name name/regex))
(define (get-cookies-by-value value/regex) (get-cookies-by 'value value/regex))
(define (get-cookies-by-domain domain/regex) (get-cookies-by 'domain domain/regex))
(define (get-cookies-by-path path/regex) (get-cookies-by 'path path/regex))


(define (set-cookie! name value #!key path domain secure? expiry)
  (let ((cookie-data
         (append `((name . ,name)
                   (value . ,value))
                 (if path `((path . ,path)) '())
                 (if domain `((domain . ,domain)) '())
                 (if secure? `((secure . ,secure?)) '())
                 (if expiry `((expiry . ,expiry)) '()))))
    (remote-execute 'POST "/session/~A/cookie"
                    json-args: `((cookie . ,(list->vector cookie-data))))))


;;; Pages

(define (page-source)
  (response-value (remote-execute 'GET "/session/~A/source")))


(define (page-title)
  (response-value (remote-execute 'GET "/session/~A/title")))


(define (refresh-page!)
  (remote-execute 'POST "/session/~A/refresh"))



;;; Screen

(define (screen-orientation elt)
  (let ((res (response-value (remote-execute 'GET "/session/~A/orientation"))))
    (string->symbol (string-downcase res))))


(define (screenshot)
  (response-value (remote-execute 'GET "/session/~A/screenshot")))


;;; Mouse actions

(define (move-mouse-cursor-to! #!optional x y elt)
  (remote-execute 'POST "/session/~A/moveto"
                  json-args: (append
                              (if elt
                                  `((element . ,(element-id elt)))
                                  '())
                              (if x
                                  `((xoffset . ,x))
                                  '())
                              (if y
                                  `((yoffset . ,y))
                                  '()))))


(define (click-mouse-button! #!optional button)
  (remote-execute 'POST "/session/~A/click"
                  json-args: (and button
                                  `((button . ,button)))))


(define (mouse-button-down!)
  (remote-execute 'POST "/session/~A/buttondown"))


(define (mouse-button-up!)
  (remote-execute 'POST "/session/~A/buttonup"))


(define (double-click-mouse-button!)
  (remote-execute 'POST "/session/~A/doubleclick"))

(include "firefox-webdriver.scm")
(include "remote-webdriver.scm")

) ;; end module
