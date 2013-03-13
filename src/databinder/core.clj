(ns databinder.core
  (:use
   compojure.core
   lamina.core
   aleph.http
   aleph.formats
   [clojure.tools.logging :only (spy)])
  (:require
   [clj-http.client :as client]
   [compojure.handler :as handler]
   [compojure.route :as route]
   [hiccup.core :as hic]
   [databinder.query :as q]
   )
  (:import (com.hp.hpl.jena.rdf.model ModelFactory ResourceFactory Model SimpleSelector)
           (java.io ByteArrayInputStream)
           (com.hp.hpl.jena.query QuerySolutionMap ParameterizedSparqlString)
           (com.hp.hpl.jena.update GraphStoreFactory UpdateAction)))



(defmacro dbg [string fun & args]
  `(let [expr# (~fun ~@args)] (println (str ~string " : " expr#)) expr#))

;; For now, we'll just be building an rdf model out of turtle-data, so this thing is simple and dumb. Later it should go to a url and get arbitrary RDF.

(defn to-model [string-data]
  (let [model (. ModelFactory createDefaultModel)]
    (.read model (new ByteArrayInputStream (.getBytes string-data "UTF-8")) "http://localhost:3000" "TURTLE")
    model))

;; To serialize relational data based on relational data, we first make a serializing function out of some relational data. For now, we'll cheat and pass in the function bindings as part of the serializer. We'll also ditch ordering for now. Also, the view-data is assumed to just be a string of turtle for now.

(defn uris [base & resources]
  (apply assoc {} (flatten (map (fn [resource] [resource (str base (name resource))]) resources))))



(defn prop [uri]
  (. ResourceFactory createProperty uri))

(defn res [uri]
  (. ResourceFactory createResource uri))

(defn plit [val]
  (. ResourceFactory createPlainLiteral val))

(def db (uris "http://logangilmour.com/data-binder#" :root :binds :bindo :haschild :projects :list :paragraph :text-field))


(def functions
  {(db :list)
   {:bind (fn [uri binding predicate meta vals]
            (hic/html [:p (:title meta)]
                      [:ol {:data-uri uri
                            :data-binding binding
                            :data-description (:description meta)}
                       (map (fn [val] [:li val]) vals)
                       [:a.list-add-binding
                        {:href "#"
                         :data-uri uri
                         :data-binding binding} "New!"]]))

    :update (fn [reverse]
              )}

   (db :paragraph)
   {:bind (fn [uri binding predicate meta vals]
            (hic/html [:p {:data-uri uri
                           :data-binding binding
                           :data-description (:description meta)}
                       (:title meta) ": " vals]))
    :update nil}

   (db :text-field)
   {:bind (fn [uri binding predicate meta vals]
            (hic/html [:label (str (:title meta) " ")
                       [:input {:type "text"
                                :class "text-field-binding"
                                :data-uri uri
                                :data-predicate predicate
                                :data-binding binding
                                :data-description (:description meta)
                                :value (apply str vals)}]]))
    :update (fn [uri pred data]
              {:type "update" :uri uri :predicate pred :value data})}})


(defn relator [statement model resource]
  (let [binder (.getURI (.getPredicate statement))]
    (if (= binder (db :binds))
      (iterator-seq (.listResourcesWithProperty model (prop (.getURI (.asResource (.getObject statement)))) resource ))
      (iterator-seq (.listObjectsOfProperty model resource (prop (.getURI (.asResource (.getObject statement)))))))
    ))

;;TODO projectors url should bind itself some stuff so it knows its unique values and its name

(defn bound-func [binding model]
  (get functions
       (.getURI
        (.getObject
         (.getProperty model binding (prop "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))))))

(defn literalize [literal]
  (.getString (.asLiteral literal)))

(def dc (uris "http://purl.org/dc/elements/1.1/" :title :description))

(defn get-meta-data [resource model]
  (apply assoc {} (flatten (map (fn [local]
                                  (let [p (.getProperty model resource (prop (dc local)))]
                                    (if p
                                      [local (.asLiteral (.getObject p))])))
                                (keys dc)))))

(defn get-bound [binding model]
  (or  (.getProperty model binding (prop (:binds db)))
       (.getProperty model binding (prop (:bindo db)))))

(defn s-rec [binding model]

  (let [children (map (fn [val] (.asResource val))
                      (iterator-seq (.listObjectsOfProperty
                                     model
                                     (.asResource binding)
                                     (prop (:haschild db)))))

        statement (get-bound binding model)

        func (:bind (bound-func binding model))

        relation-getter (fn [data resource] (relator statement data resource))

        sub-calls (map (fn [child] (s-rec child model)) children)
        meta-data (get-meta-data binding model)]

    (fn [data resource]

      (let [related (relation-getter data resource)]

        (func (.getURI resource)
              (.getURI binding)
              (.getURI (.getObject statement))
              meta-data
              (if (empty? sub-calls)
                (map literalize related)
                (map seq (apply map vector (map (fn [sub-call]
                                           (map
                                            (partial sub-call data)
                                            related))
                                                sub-calls)))))))))

(defn select [model subject predicate object]
  (iterator-seq (.listStatements (new SimpleSelector subject predicate object))))

(defn simple-update [query s p o]
  (let [q-map (new QuerySolutionMap)]
    (.add q-map "s" s)
    (.add q-map "p" p)
    (.add q-map "o" o)
    (.asUpdate (new ParameterizedSparqlString query q-map))
    ))

(defn query-builder [command reversed]

  (cond (= (:type command) "update")
        (if reversed
          (simple-update "
DELETE { ?old ?p ?o }
INSERT { ?s ?p ?o }
WHERE { ?old ?p ?o }"
                         (plit (:value command))
                         (prop (:predicate command))
                         (res (:uri command)))
          (simple-update "
DELETE { ?s ?p ?old }
INSERT { ?s ?p ?o }
WHERE { ?s ?p ?old }"
                         (res (:uri command))
                         (prop (:predicate command))
                         (plit (:value command))))))

(comment (if reversed
                   "INSERT DATA { ?s ?p ?o }"
                   ))

(defn editor [inf]
  (fn [model uri binding data]
    (let [statement (get-bound binding inf)
          func (:update (bound-func binding inf))
          binder (.getURI (.getPredicate statement))
          pred (.getURI (.asResource (.getObject statement)))
          resource (res uri)
          new-resource (res (str "urn:uuid:" (java.util.UUID/randomUUID)))
          reverse (= binder (db :binds))

          graph-store (. GraphStoreFactory create model)
          ]

      (let [command (func uri pred data)
            update (query-builder command reverse)]
        (println "!!!!!!!! " update)
        (. UpdateAction execute update graph-store)
        command))))

(defn serializer
  ([view-data]

      (let [inf (. ModelFactory createRDFSModel view-data)
            root (.asResource (.getObject (.getProperty inf
                                                        (res (:root db))
                                                        (prop (:haschild db)))))]

        (s-rec root inf)
        ))
  ([view-data binding]

      (let [inf (. ModelFactory createRDFSModel view-data)]

        (s-rec binding inf)
        )))




(def example-view
  "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix data: <http://logangilmour.com/data-binder#> .
@prefix ex: <http://logangilmour.com/example-view#> .
@prefix ont: <http://logangilmour.com/example-ontology#> .
@prefix dc: <http://purl.org/dc/elements/1.1/> .

data:root data:haschild ex:person-list .

ex:person-list rdf:type data:list ;
               data:binds rdf:type ;
               dc:title \"List of People\" ;
               dc:description \"A list of people contained within the application\" .

ex:person-list data:haschild ex:person-name , ex:person-age , ex:pets-list .

ex:person-name rdf:type data:text-field ;
               data:bindo ont:name ;
               dc:title \"Name\" ;
               dc:description \"The name of a person\" .

ex:person-age rdf:type data:text-field ;
              data:bindo ont:age ;
              dc:title \"Age\" ;
              dc:description \"The age of a person\".

ex:pets-list rdf:type data:list ;
             data:bindo ont:haspet ;
             dc:title \"Pets\" ;
             dc:description \"A list of the pets a person has\" .

")

(def mini-view
  "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix data: <http://logangilmour.com/data-binder#> .
@prefix ex: <http://logangilmour.com/example-view#> .
@prefix ont: <http://logangilmour.com/example-ontology#> .

data:root data:haschild ex:person-name .

ex:person-name rdf:type data:paragraph .

ex:person-name data:bindo ont:name .

")

(def data-binder "
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix data: <http://logangilmour.com/data-binder#> .

data:root rdf:type rdfs:property ;
          rdfs:range data:serializer ;
          rdfs:domain data:view .

data:serializer rdf:type rdfs:class .

data:view rdf:type rdfs:class .

data:binds rdf:type rdfs:property .

data:binds rdfs:domain data:serializer .

data:label rdf:type


")

(def ontology "
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix ont: <http://logangilmour.com/example-ontology#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

ont:person rdf:type rdfs:class .

ont:name rdf:type rdfs:property .
ont:name rdfs:domain ont:person .
ont:name rdfs:range xsd:string .
ont:age rdfs:domain ont:person .
ont:age rdfs:range xsd:integer .

")

(def data "
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix ont: <http://logangilmour.com/example-ontology#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix test: <http://logangilmour.com/test#> .

test:logan ont:name \"Logan Gilmour\" ;
           rdf:type ont:person ;
           ont:age 23 .

test:logan ont:haspet \"Dog\" ;
           ont:haspet \"Tomato\" .

test:dave ont:name \"Dave Davidson\" ;
          rdf:type ont:person ;
          ont:age 25 .

test:dave ont:haspet \"Monster\" ;
          ont:haspet \"Truck\" ;
          ont:haspet \"Salamander\" .
")


(defn page [body]
  (hic/html
   [:html
    [:head
     [:title "FIXME"]]
    [:body body
     [:script {:type "text/javascript" :src "/js/jquery.min.js"}]
     [:script {:type "text/javascript" :src "/js/underscore-min.js"}]
     [:script {:type "text/javascript" :src "/js/update.js"}]]

    ]
   ))

(defn startup []
  (do
    (print "starting up server!")
    ;;(logger/init)
    ))

(def data-model (to-model data))

(def broadcast-channel (channel))
(def edit (editor (to-model example-view)))

(defn store [message]
  (edit data-model (:uri message) (res (:binding message)) (:data message)))

(defn chat-handler [ch handshake]
  (receive ch
           (fn [input]
             ;;(let jc [(map* decode-json ch)])
             (siphon (map* encode-json->string (map* store (map* decode-json ch))) broadcast-channel)
             (siphon broadcast-channel ch)
             )))


(defroutes main-routes
  (GET "/async/" [] (wrap-aleph-handler chat-handler))
  (GET "/" []
       (page ((serializer (to-model example-view)) data-model (res "http://logangilmour.com/example-ontology#person"))))
  (route/resources "/")
  (route/not-found "Page not found"))

(defn -main
  [& args]
  (start-http-server (wrap-ring-handler main-routes)
                     {:port 8080 :websocket true}))
