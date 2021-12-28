;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: FKPOS; Base: 10 -*-

(in-package :fkpos)

(setq cl-who:*attribute-quote-char* #\")

(defun html-header (s)
  (with-html-output (s)
    (:meta :charset "UTF-8")
    (:title "Fjærkroa Kooking")
    (:meta :name "viewport" :content "width=device-width, initial-scale=1, shrink-to-fit=no")
    (:link :rel "stylesheet" :href "https://cdn.jsdelivr.net/npm/bootstrap@5.0.1/dist/css/bootstrap.min.css")
    (:link :rel "stylesheet" :href "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.7.2/font/bootstrap-icons.css")
    (:link :rel "stylesheet" :href "fkpos/fkpos.css")
    (:script :src "js/vue.js")
    (:script :src "js/jquery-3.5.1.min.js")
    (:script :src "https://cdn.jsdelivr.net/npm/@popperjs/core@2.9.2/dist/umd/popper.min.js" :integrity "sha384-IQsoLXl5PILFhosVNubq5LC7Qb9DXgDA9i+tQ8Zj3iwWAwPtgFTxbJ8NT4GN1R8p" :crossorigin "anonymous")
    (:script :src "https://cdn.jsdelivr.net/npm/bootstrap@5.0.1/dist/js/bootstrap.min.js" :integrity "sha384-Atwg2Pkwv9vp0ygtn1JAojH0nYbwNJLPhwyoVbhoPwBhjQPR5VtM2+xf0Uwh9KtT" :crossorigin "anonymous")
    (:script :src "fkpos/fkpos.js")))

(defun html-test-button (s)
  (with-html-output (s)
    (:button :type "button"
             :id "group"
             :class "container-fluid btn btn-primary p-3"
             (:div :class "row"
                   (:div :class "col-sm-2 h4" :style "text-align: left;" :title "purchase.ts" "ts")
                   (:div :class "col-sm h1" :style "text-align: left;"
		                     (:span (:div "purchase.name")
		                            (:div :class "h4" :style "font-size: 20px; font-style: italic;" "purchase.description")
		                            (:div :style "font-size: 20px; fant-style: bold;" "purchase.variant")
		                            (:div :class "h4" :style "font-size: 20px;" "purchase.comment")))
                   (:div :class "col-sm-2 h5" :style "text-align: right;"
		                     (:div :title "purchase.done" "getTS(purchase.done)"))))))

(defun default-page ()
  (with-html-output-to-string (s nil :prologue t)
    (:html (:head (html-header s))
           (:body (:div :id "app"
                        (:div :class "container-fluid"
                              (:div :class "row flex-nowrap"
                                    (:div :class "col-auto col-md-3 col-xl-2 px-sm-2 px-0 bg-dark"
                                          (:div :class "d-flex flex-column align-items-center align-items-sm-start px-3 pt-2 text-white min-vh-100"
                                                (:a :href "/" :class "d-flex align-items-center pb-3 mb-md-0 me-md-auto text-white text-decoration-none"
                                                    (:span :class "fs-5 d-none d-sm-inline" "Menu"))
                                                (:ul :class "nav nav-pills flex-column mb-sm-auto mb-0 align-items-center align-items-sm-start" :id "menu"
                                                     (:li :class "nav-item"
                                                          (:a :href "#" :class "nav-link align-middle px-0"
                                                              (:i :class "fs-4 bi-house")
                                                              (:span :class "ms-1 d-none d-sm-inline" "Home")))
                                                     (:li (:a :href "#submenu1" :data-bs-toggle "collapse" :class "nav-link px-0 align-middle"
                                                              (:i :class "fs-4 bi-speedometer2")
                                                              (:span :class "ms-1 d-none d-sm-inline" "Dashboard"))
                                                          (:ul :class "collapse show nav flex-column ms-1" :id "submenu1" :data-bs-parent "#menu"
                                                               (:li :class "w-100"
                                                                    (:a :href "#" :class "nav-link px-0"
                                                                        (:span :class "d-none d-sm-inline" "Item")
                                                                        "1"))
                                                               (:li (:a :href "#" :class "nav-link px-0"
                                                                        (:span :class "d-none d-sm-inline" "Item")
                                                                        "2")))))
                    ;<li>
                    ;    <a href="#" class="nav-link px-0 align-middle">
                    ;        <i class="fs-4 bi-table"></i> <span class="ms-1 d-none d-sm-inline">Orders</span></a>
                    ;</li>
                    ;<li>
                    ;    <a href="#submenu2" data-bs-toggle="collapse" class="nav-link px-0 align-middle ">
                    ;        <i class="fs-4 bi-bootstrap"></i> <span class="ms-1 d-none d-sm-inline">Bootstrap</span></a>
                    ;    <ul class="collapse nav flex-column ms-1" id="submenu2" data-bs-parent="#menu">
                    ;        <li class="w-100">
                    ;            <a href="#" class="nav-link px-0"> <span class="d-none d-sm-inline">Item</span> 1</a>
                    ;        </li>
                    ;        <li>
                    ;            <a href="#" class="nav-link px-0"> <span class="d-none d-sm-inline">Item</span> 2</a>
                    ;        </li>
                    ;    </ul>
                    ;</li>
                    ;<li>
                    ;    <a href="#submenu3" data-bs-toggle="collapse" class="nav-link px-0 align-middle">
                    ;        <i class="fs-4 bi-grid"></i> <span class="ms-1 d-none d-sm-inline">Products</span> </a>
                    ;        <ul class="collapse nav flex-column ms-1" id="submenu3" data-bs-parent="#menu">
                    ;        <li class="w-100">
                    ;            <a href="#" class="nav-link px-0"> <span class="d-none d-sm-inline">Product</span> 1</a>
                    ;        </li>
                    ;        <li>
                    ;            <a href="#" class="nav-link px-0"> <span class="d-none d-sm-inline">Product</span> 2</a>
                    ;        </li>
                    ;        <li>
                    ;            <a href="#" class="nav-link px-0"> <span class="d-none d-sm-inline">Product</span> 3</a>
                    ;        </li>
                    ;        <li>
                    ;            <a href="#" class="nav-link px-0"> <span class="d-none d-sm-inline">Product</span> 4</a>
                    ;        </li>
                    ;    </ul>
                    ;</li>
                    ;<li>
                    ;    <a href="#" class="nav-link px-0 align-middle">
                    ;        <i class="fs-4 bi-people"></i> <span class="ms-1 d-none d-sm-inline">Customers</span> </a>
                    ;</li>
                                                ;(:button :type "button" :class "btn btn-secondary container-fluid m-1 " "test")
                                                ;(:button :type "button" :class "btn btn-secondary container-fluid m-1 col-12" "test")
                                                ;(:button :type "button" :class "btn btn-secondary container-fluid m-1 col-12" "test")
                                                     ))
                                    (:div :class "col py-3"
                                          (:div :class "row container-fluid"
                                                (:button :type "button" :class "btn btn-info container-fluid m-1 col-1" "test")
                                                (:button :type "button" :class "btn btn-info container-fluid m-1 col-1" "test")
                                                (:button :type "button" :class "btn btn-info container-fluid m-1 col-1" "test"))
                                          (:div :class "row container-fluid"
                                                (:button :type "button" :class "btn btn-info container-fluid m-1 col-12" "test"))))))))))

(defun default-js ()
  (ps
    (defun test-adder () (+ 2 2))))

(defun default-css ()
  (css
    (("body")
     (:background "gray"))
    ((".leftNav")
     (:position "fixed"
      :height "100%"
      :width "160px"
      ;:z-index "1"
      :margin "0px"
      ;:overflow-x "hidden"
      ))
    (("#app")
     (:width "100%"))
    (("#group")
     (:background-color "#00cd7b"))))
