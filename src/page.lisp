(in-package :fkpos)

(defun default-page ()
  (with-html-output-to-string (*standard-output* nil :prologue t :indent t)
    (:html (:head (:meta :charset "UTF-8")
                  (:title "Fjærkroa Kooking")
                  (:meta :name "viewport" :content "width=device-width, initial-scale=1, shrink-to-fit=no")
                  (:link :rel "stylesheet" :href "css/bootstrap.min.css" :integrity "sha384-JcKb8q3iqJ61gNV9KGb8thSsNjpSL0n8PARn9HuZOnIxN0hoP+VmmDGMN5t9UJ0Z" :crossorigin "anonymous")
                  (:script :src "js/vue.js")
                  (:script :src "js/jquery-3.5.1.min.js")
                  (:script :src "js/popper.min.js" :integrity "sha384-9/reFTGAW83EW2RDu2S0VKaIzap3H66lZH81PoYlFhbGU+6BZp6G7niu735Sk7lN" :crossorigin "anonymous")
                  (:script :src "js/bootstrap.min.js" :integrity "sha384-B4gt1jrGC7Jh4AgTPSdUtOBvfO8shuf57BaghqFfPlYxofvL8/KUEfYiJOMMV+rV" :crossorigin "anonymous"))
           (:body :style "background: black;"
                  (:div :id "app" :style "with: 100%;"
                        (:div :class "p-1"
                              (:button :type "button" :class "container-fluid btn"
                                       (:div :class "row" "test"))))))))
