(ns databinder.core
  (:use
   compojure.core)
  (:require
   [clj-http.client :as client]
   [compojure.handler :as handler]
   [compojure.route :as route]
   [hiccup.core :as hic]
   [databinder.query :as q]))




(defn startup []
  (do
    (print "starting up server!")
    ;;(logger/init)
    ))

(defroutes main-routes
  (GET "/" []
       (q/query "http://localhost:8080/sparql/" q/exquery
                ))
  (route/resources "/")
  (route/not-found "Page not found"))

(def app
  (handler/site main-routes))
