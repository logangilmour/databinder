(ns databinder.core
  (:use
   compojure.core
   lamina.core
   aleph.http
   aleph.formats
   databinder.rdf
   [databinder.interpreter :only (interpreter)]
   [databinder.preprocessor :only (preprocess)]
   [databinder.synchronize :only (synchronize edit)]
   [databinder.template :only (template-env)])
  (:require
   [compojure.route :as route]
   [hiccup.core :as hic]
   [databinder.model :as m]))

(def weaver (m/to-model
             "
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix : <http://logangilmour.com/data-binder#> .

:subject-binding rdfs:subClassOf :binding.

:object-binding rdfs:subClassOf :binding.

:binds-subject rdfs:subPropertyOf :binds.

:binds-object rdfs:subPropertyOf :binds.

:binds rdfs:domain :binding.

:binds-subject rdfs:domain :subject-binding.

:binds-object rdfs:domain :object-binding.

:has-property rdfs:domain :binding;
              rdfs:range :property-binding.

:has-list rdfs:domain :binding;
          rdfs:range :list-binding.

:property-binding rdfs:subClassOf :binding.

:list-binding rdfs:subClassOf :binding.

:root rdfs:subClassOf :list-binding.

"))

(def example-view (m/to-model
                   "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix : <http://logangilmour.com/data-binder#> .
@prefix ex: <http://logangilmour.com/example-view#> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .


:application a ex:foaf-manager .

ex:myview a :root;
  :name \"people\";
  :binds-object rdf:type;
  #:key \"\"\"(fn [obj] (if (= (:type obj) \"http://xmlns.com/foaf/0.1/\") (:first obj))) \"\"\";
  :has-property [:name \"type\"; :binds-subject rdf:type];
  :has-property [:name \"first\"; :binds-subject foaf:givenName].
"))



(def expanded-example nil)
(def broadcast-channel (permanent-channel))
(def data nil)


(defn page [body]
  (hic/html
   [:html
    [:head
     [:meta {:name "viewport" :content "width=device-width, initial-scale=1.0"}]

     [:link {:href "/css/bootstrap.min.css" :rel "stylesheet" :media "screen"}]
     [:link {:rel "stylesheet" :href "http://cdn.leafletjs.com/leaflet-0.5/leaflet.css"}]
     [:link {:href "/css/datepicker.css" :rel "stylesheet" :media "screen"}]
     [:link {:href "/css/graph.css" :rel "stylesheet" :media "screen"}]
     [:title "FIXME"]
     [:script {:type "text/javascript" :src "/js/jquery.min.js"}]]
    [:body body
     [:script {:type "text/javascript" :src "/js/bootstrap.min.js"}]
     [:script {:type "text/javascript" :src "/js/bootstrap-datepicker.js"}]
     [:script {:type "text/javascript" :src "/js/underscore-min.js"}]
     [:script {:type "text/javascript" :src "http://cdn.leafletjs.com/leaflet-0.5/leaflet.js"}]
     [:script {:type "text/javascript" :src "http://d3js.org/d3.v3.min.js"}]
     [:script {:type "text/javascript" :src "/js/update.js"}]]

    ]
   ))

(defn startup []
  (do
    (print "starting up server!")))


(def channels (atom {}))

(defn register [ch url]
  (siphon (map* (partial edit expanded-example data) (map* decode-json ch)) broadcast-channel)
  (swap! channels (fn [old]

                    (let [all (reduce (fn [accum key]
                                        (if (closed? (get accum key))
                                          (do
                                            (println "Getting rid of old connection for " key)
                                            (dissoc accum key))
                                          accum)) old (keys old))
                          url-ch (or (get all url)
                                     (let [new-ch (channel)]
                                       (println "creating a channel for url " url)
                                       (siphon (map*
                                                encode-json->string
                                                (map*
                                                 (fn [message]
                                                   (synchronize expanded-example data message url))
                                                 broadcast-channel))
                                               new-ch)
                                       new-ch))]
                      (siphon url-ch ch) ;; listen on your url channel
                      (doall (map (partial println "\nConnection: ") (keys all)))
                      (assoc all url url-ch)))))

(defn chat-handler [ch handshake]
  (receive ch
           (fn [input]
             (let [url (str (clojure.string/replace (:uri handshake) #"^/async/" "/view/")
                            "?" (:query-string handshake))]
               (println "registering a connection to url " url)
               ;;(let jc [(map* decode-json ch)])
               (register ch url))
             )))


(defroutes main-routes
  (GET "/async/*" [] (wrap-aleph-handler chat-handler))
  (GET "/view/*" [* :as request]
       (let [uri ;;(clojure.string/replace (:uri request) #"^/view/" "")
             (str (:uri request) "?" (:query-string request))
             ]
         (println "URL!!!!!!! " uri)
         (page (interpreter expanded-example data uri))))
  (route/resources "/")
  (route/not-found "Page not found"))


(comment (defn -main
    [& args]
    (def expanded-example (preprocess example-view weaver))
    (def data (m/union (m/default-model) (:model expanded-example)))
    (template-env)
    (start-http-server (wrap-ring-handler main-routes)
                       {:port 8080 :websocket true})))



;;(do (use 'databinder.core :reload-all) (def stop! (-main)) (defn reload [] (stop!) (use 'databinder.core :reload-all) (def stop! (-main))))
