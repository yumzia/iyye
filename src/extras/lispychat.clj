

(ns extras.lispychat
  (:use lamina.core
        aleph.http
        compojure.core
    ; This sets the correct file type for js includes used by hiccup
        (ring.middleware resource file-info)
        (hiccup core page))
  (:require [compojure.route :as route]
            [iyye.bios.ioframes :as ioframes]
            [clojure.tools.logging :as log]
            [iyye.bios.process-lisp :as lisp]))

(defn page []
  "HTML page rendered using Hiccup. Includes the css and js for websockets."
  (html5
    [:head
     (include-css "/static/stylesheets/master.css")]
    [:body
     [:div.container
      [:div.row
       [:div.columns.twelve
        [:p [:h1#headline "Iyye control chat"]]
        [:form
         [:input#message {:type "text"}]
         [:input.nice.large.blue.button {:type "submit"}]]
        [:div#messages]]]]
     (include-js "http://ajax.googleapis.com/ajax/libs/jquery/1.7.2/jquery.min.js")
     (include-js "/static/javascripts/web_socket.js")
     (include-js "/static/javascripts/app.js")]))

(defn sync-app [request]
  "Rendered response of the chat page"
  {:status 200
   :headers {"content-type" "text/html"}
   :body (page)})

(def wrapped-sync-app
  "Wraps the response with static files"
  (-> sync-app
      (wrap-resource "public")
      (wrap-file-info)))

(def chat-channel (ref ()))
(defn send-msg [IO msg]
  (enqueue @chat-channel (str "iyye> " msg)))

(defn chat-init [ch])

(defn chat-handler [ch room]
  "Relays messages into a chat room. If it doesn't
  exist create a new channel"
  (let [chat (named-channel room chat-init)]
    (siphon chat ch)
    (siphon ch chat)))

(defn chat [ch request]
  "View handler that handles a chat room. If it's not
  a websocket request then return a rendered html response."
  ; (println "ch " ch " req " request)
  (if (:websocket request)
    (dosync
      (ref-set chat-channel ch)
      (chat-handler ch "iyye"))
    (enqueue ch (wrapped-sync-app request))))

(defroutes app-routes
           "Routes requests to their handler function. Captures dynamic variables."
           (GET ["/chat/iyye"] {}
             (wrap-aleph-handler chat))
           (GET ["/"] {} "To access Iyye, go to /chat/iyye <a href=\"http://localhost:8080/chat/iyye\">iyye chat</a>")
           ;;Route our public resources like css and js to the static url
           (route/resources "/static")
           ;;Any url without a route handler will be served this response
           (route/not-found "Page not found, go to /chat/iyye <a href=\"http://localhost:8080/chat/iyye\">iyye chat</a>"))

(defn start-lispy-chat-server [& args]
  "Main thread for the server which starts an async server with
  all the routes we specified and is websocket ready."
  (log/info "start http server")
  (start-http-server (wrap-ring-handler app-routes)
                     {:host "localhost" :port 8080 :websocket true}))

(def lispy-chat-IO (ioframes/create-IO "lispy-chat" start-lispy-chat-server (ioframes/create-InputSources false [lisp/process-input]) (ioframes/->OutputActuators false [send-msg])))

(def lispy-chat-process-input (partial ioframes/process-input lispy-chat-IO))

(defn chat-init [ch]
  "register callback for receive msg"
  (receive-all ch lispy-chat-process-input))

