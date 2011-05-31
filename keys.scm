(define keys
  '((null         . "e000")
    (cancel       . "e001") ;; ^break
    (help         . "e002")
    (backspace   . "e003")
    (tab          . "e004")
    (clear        . "e005")
    (return       . "e006")
    (enter        . "e007")
    (shift        . "e008")
    (left-shift   . "e008") ;; alias
    (control      . "e009")
    (left-control . "e009") ;; alias
    (alt          . "e00a")
    (left-alt     . "e00a") ;; alias
    (pause        . "e00b")
    (escape       . "e00c")
    (space        . "e00d")
    (page-up      . "e00e")
    (page-down    . "e00f")
    (end          . "e010")
    (home         . "e011")
    (left         . "e012")
    (arrow-left   . "e012") ;; alias
    (up           . "e013")
    (arrow-up     . "e013") ;; alias
    (right        . "e014")
    (arrow-right  . "e014") ;; alias
    (down         . "e015")
    (arrow-down   . "e015") ;; alias
    (insert       . "e016")
    (delete       . "e017")
    (semicolon    . "e018")
    (equals       . "e019")

    (numpad0      . "e01a") ;; number pad keys
    (numpad1      . "e01b")
    (numpad2      . "e01c")
    (numpad3      . "e01d")
    (numpad4      . "e01e")
    (numpad5      . "e01f")
    (numpad6      . "e020")
    (numpad7      . "e021")
    (numpad8      . "e022")
    (numpad9      . "e023")
    (*            . "e024")
    (+            . "e025")
    (separator    . "e026")
    (-            . "e027")
    (decimal      . "e028")
    (divide       . "e029")

    (f1           . "e031") ;; function keys
    (f2           . "e032")
    (f3           . "e033")
    (f4           . "e034")
    (f5           . "e035")
    (f6           . "e036")
    (f7           . "e037")
    (f8           . "e038")
    (f9           . "e039")
    (f10          . "e03a")
    (f11          . "e03b")
    (f12          . "e03c")

    (meta         . "e03d")
    (command      . "e03d")))

(define (key sym)
  (alist-ref sym keys))


(define mouse-button
  (let ((buttons
         '((left . 0)
           (middle . 1)
           (right . 2))))
    (lambda (button)
      (or (alist-ref button buttons)
          (error 'mouse-button "Invalid button" button)))))
