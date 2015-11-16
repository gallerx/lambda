(defnode 
  #:state  S4-1
  #:parent S4
  #:entry (λ _
            (menu! MENU:new-edit)
            (with-menu-screen (format "~A Order Menu" (fcdr order-type))))
  #:init do-nothing
  #:exit do-nothing
  #:handler (with-parts (case verb
                          [(new)  (trans this S4-2)] ;back will take you to logout
                          [(esc)  (trans this S2)]
                          [(edit) (trans this S4-2-7)]
                          [else  #f]))
  #:parser px-CG-menu
  #:bindings menu)
