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
   [clojure.zip :as z]
   [clojure.walk :as w]
   )
  (:import (com.hp.hpl.jena.rdf.model ModelFactory ResourceFactory Model SimpleSelector)
           (java.io ByteArrayInputStream)
           (com.hp.hpl.jena.query QuerySolutionMap ParameterizedSparqlString QueryExecutionFactory)
           (com.hp.hpl.jena.update GraphStoreFactory UpdateExecutionFactory)))



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

(defn uuid [] (str "urn:uuid:" (java.util.UUID/randomUUID)))

(def db (uris "http://logangilmour.com/data-binder#" :rank :root :binds :bindo  :haschild :container :relator :contains :projects :path))

(def funcs
  {:projector
   (fn [uri binding active url meta vals]
     (let [s (apply str vals)]
       (hic/html [:a.projector-binding {
                                        :href url
                                        :data-uri uri
                                        :data-binding binding
                                        } (if (re-matches #"^\s*$" s) "untitled" s)])))
   :list-item
   (fn [uri binding active url meta vals]
     (hic/html [:li  {:class (if active "active" "")

                      :data-uri uri}
                vals]))
   :column8
   (fn [uri binding active url meta vals]
     (hic/html [:div.span8 vals]))

   :column4
   (fn [uri binding active url meta vals]
     (hic/html [:div.span4 vals]))
   :row
   (fn [uri binding active url meta vals]
     (hic/html [:div.row vals]))

   :list
   (fn [uri binding active url meta vals]
     (hic/html
      [:ul {:class "list-binding nav nav-list"
            :data-uri uri
            :data-binding binding
            :data-description (:description meta)}
       [:li.nav-header (:title meta)]
       vals]
      [:a
       {:href "#"
        :class "list-add-binding btn"
        :data-uri uri
        :data-binding binding} "Create"]))

   :deleter
   (fn [uri binding active url meta vals]
     (hic/html [:a.remove-binding
                {:href "#"
                 :data-uri uri
                 :data-binding binding
                 :data-value (apply str vals)} (:title meta)]))

   :paragraph
   (fn [uri binding active url meta vals]
     (hic/html [:p {:data-uri uri
                    :data-binding binding
                    :data-description (:description meta)}
                (:title meta) ": " vals]))

   :text-field
   (fn [uri binding active url meta vals]
     (hic/html [:label (str (:title meta) " ")
                [:input {:type "text"
                         :class "text-field-binding"
                         :data-uri uri
                         :data-binding binding
                         :data-description (:description meta)
                         :value (apply str vals)}]]))

   :h1
   (fn [uri binding active url meta vals]
     (hic/html [:h1 vals]))})

(def functions (apply assoc {} (flatten (map (fn [key] [(str "http://logangilmour.com/bootstrap-widgets#" (name key)) (get funcs key)]) (keys funcs)))))


(defn relator [statement model resource]

  (let [binder (.getURI (.getPredicate statement))]
    (if (= binder (db :binds))
      (iterator-seq (.listResourcesWithProperty model (prop (.getURI (.asResource (.getObject statement)))) resource ))
      (iterator-seq
       (.listObjectsOfProperty model resource
                               (prop
                                (.getURI
                                 (.asResource
                                  (.getObject statement)))))))
    ))

;;TODO projectors url should bind itself some stuff so it knows its unique values and its name

(defn literalize [res]
  (if (.isLiteral res)
    (.getString (.asLiteral res))
    (.toString res)))

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


;; take a thing and get its function
;; are there sub-things? if so, get them and recurse
;; if not, return the result of running the function on the thing and the empty list
;;
;; build a thing as we return up the tree.
;; order things after we build them.

;; necessary: bind:subject, bind:object, bind:order1 ... orderN, bind:haschild.

;; bind:subject takes a function1 and a predicate and makes a function that takes an object and returns the result of calling the function1 on the right side of the predicate, filtered by object.
;; bind:object ...
;; bind:order takes a function and a

;; take an object and a predicate and get the binding for that predicate and apply it to the object.

;; a relation-function takes a resource and turns it into a function that takes a resource and turns it into a function... until strings.
;; haschild: take a bunch of things and call tree-builder on them.




;; The output point is when a function makes a function that returns strings.

;; bind subject, object, and child are all bindings.

;; haschild bind:object haschild

;; Tree-builder algorithm:
;; Takes an initial resource.
;; Gets children by getting all properties of a type, then getting the bound functions for them, then , then
;; recurses on children.
;; returns aggregation of children run through the function for this resource.
;; functions needed: make a sequence of children into a single thing, get the necessary children.
;; haschild makes a function that returns the related values, while contains makes a function returns with the same values.

;; tree builder takes a resource and a property.
;; tree builder then gets all sub-resources pointed at by the property.
;; tree builder orders those sub-resources.
;; tree builder then runs the tree-ify function bound to the predicate on the resource and the children and returns the result.
;; A binding function takes an object and a list of trees and makes a new tree with the object at the top.
;; Each predicate that we follow in a tree has a binding function.
;; A more complex tree of bindings


;; tree-build starting from university of alberta, employee
;; get a bunch of employees, including me
;; tree-build on each of us for each predicate that matters. Is this by type? This is where recursive assembly comes in - building the right tree of tree-builders.
;; sort the returned values by the bundled keys.
;; apply the function for binding the employee relationship to the result of this map and the university.
;; return

;; parts definitely common to both: Ordering, getting all relations and then mapping a list of functions to them,

;; Container: doesn't relate, just passes its values on to the next function.
;; Projector: relates and recurses, but with a function that is somewhere else in the tree (no rendered children)
;; Child: recurses and relates






(defn relate-right [model predicate resource]
  (iterator-seq (.listObjectsOfProperty model resource predicate)))

(defn relate-left [model predicate resource]
  (iterator-seq (.listResourcesWithProperty model predicate resource)))


(defn parse-uri [uri]
  (let [parts (filter (comp not (partial re-matches #"^\s*$")) (clojure.string/split uri #"/"))]

    (if (empty? parts)
     {}
     (apply assoc {} parts))))

(defn s-rec [binding model url-parts]

  (let [children (sort-by (fn [resource]
                            (let [p (.getProperty model resource (prop (:rank db)))]
                              (if p
                                (.getString (.asLiteral (.getObject p)))
                                "")))
                          (concat
                           (relate-right model (prop (:contains db)) binding)
                           (relate-right model (prop (:haschild db)) binding)))

        projector (first (relate-left model (prop (:projects db)) binding)) ;;TODO only works for container pointed at by projector for now.
        path (if projector (first (relate-right model (prop (:path db)) projector)))

        bindable (first (relate-right model (prop "http://www.w3.org/1999/02/22-rdf-syntax-ns#type") binding))

        types (set (map #(.toString %) (relate-right model (prop "http://www.w3.org/1999/02/22-rdf-syntax-ns#type") binding)))

        func (reduce (fn [accum val] (or accum (get functions val))) nil types)

        new-url-parts (if path
                  (conj url-parts (.getString (.asLiteral path)))
                  url-parts)

        sub-calls (map (fn [child] (s-rec child model url-parts)) children) ;; make the next part of the tree
        meta-data (get-meta-data binding model)
        myp (first (relate-right model (prop (:path db)) binding))
        mypath (if myp (.getString (.asLiteral myp)))]

    (cond
     (contains? types (db :relator))

     (let [statement (get-bound binding model) ;; fuckery for binding
           relation-getter (fn [data resource] (relator statement data resource))]
       (fn [data child resource url built-url]

         (let [related (if child   ;; get sub-resources (a single one if partial rendering)
                         (seq [child])
                         (relation-getter data resource))
               current (if mypath (get (parse-uri url) mypath))
               active (if mypath (= current (.getURI resource)))
               built-url (or (if mypath (str built-url mypath "/" (.getURI resource) "/")) built-url)
               vals (if (empty? sub-calls)
                   (map literalize related)
                   (map seq (apply map vector (map (fn [sub-call]
                                                     (map
                                                      (fn [val] (sub-call data nil val url built-url))
                                                      related))
                                                   sub-calls))))]
           (if child
             (hic/html vals) ;;TODO extract this business... maybe?
             (func (.getURI resource) ;; apply the bound function to the thing, the binding that points at the funtion, meta-data, and the finished children (should be ordered by now).
                   (.getURI binding)
                   active
                   built-url
                   meta-data
                   vals)))))
     (contains? types (db :container)) ;;TODO make it so we can project from a container.
          (fn [data child resource url built-url]
            (let [ppath (if path (.getString (.asLiteral path)))
                  p (get (parse-uri url) ppath)
                  projected (if (and p (res p))
                              (if (.containsResource data (res p))
                                (res p)))

                  current (if mypath (get (parse-uri url) mypath))

                  active (if mypath (= current (.getURI resource)))
                  built-url (or (if mypath (str built-url mypath "/" (.getURI resource) "/")) built-url)]

              (if ppath
                (if projected
                  (func
                   (.getURI resource)
                   (.getURI binding)
                   active
                   built-url
                   meta-data
                   (map (fn [sub-call] (sub-call data nil projected url (str built-url ppath "/" (.getURI resource) "/"))) sub-calls))
                  "")
                (func
                 (.getURI resource)
                 (.getURI binding)
                 active
                 built-url
                 meta-data
                 (map (fn [sub-call] (sub-call data nil resource url built-url)) sub-calls))
                ))))))

(defn select [model subject predicate object]
  (iterator-seq (.listStatements (new SimpleSelector subject predicate object))))

(defn simple-update [query s p o]
  (let [q-map (new QuerySolutionMap)]
    (.add q-map "s" s)
    (.add q-map "p" p)
    (.add q-map "o" o)
    (.toString (new ParameterizedSparqlString query q-map))
    ))


(defn flatten-seq [coll]
  (reduce (fn [accum val]
            (cond (seq? val)
                  (vec (concat accum (flatten-seq val)))

                  (vector? val)
                  (conj accum (flatten-seq val))

                  :default
                  (conj accum val))) [] coll))

(defn qb [binding model uri] ;; TODO come up with a nice URI aliasing thing
  (let [children (concat
                  (relate-right model (prop (:haschild db)) binding)
                  (relate-right model (prop (:projects db)) binding))

        projector (first (relate-left model (prop (:projects db)) binding))
        ppath (if projector (first (relate-left model (prop (:path db)) projector)))

        types (set (map #(.toString %) (relate-right model (prop "http://www.w3.org/1999/02/22-rdf-syntax-ns#type") binding)))

        path (first (relate-right model (prop (:path db)) binding))
        onpath (if path (get uri (.getString (.asLiteral path))))

        subs
        (if path
         (if onpath
           (map (fn [child] (qb child model uri)) children)
           '())
         (map (fn [child] (qb child model uri)) children))]

    (cond (contains? types (db :relator))
          (let [statement (get-bound binding model)
                binder (.getURI (.getPredicate statement))
                predicate (.asResource (.getObject statement)) ;;TODO handle type thing with path thing
                reverse (= binder (db :binds))]

            [{:pred (.getURI predicate) :rev reverse :path onpath} subs])

          (contains? types (db :container))

          subs
          )))





(defn qbuild [inf uri]
  (let [root (.asResource (.getObject (.getProperty inf
                                                        (res (:root db))
                                                        (prop (:haschild db)))))
        init (qb root inf uri)

        numbered (loop [loc (z/vector-zip (flatten-seq init))
                        uid 0]

      (cond (z/end? loc)
            (z/root loc)

            (map? (z/node loc))
            (recur (-> loc (z/replace (assoc (z/node loc) :id (str uid))) z/next)
                   (+ uid 1))
            :else
            (recur (z/next loc) uid)))]
    (vec (concat [{:id "root"}] numbered))


    ))


(defn q-map [[node & children]]
  (let [results (apply merge {} (map q-map children))]
    (if (:path node)
      (assoc
          results
        (str "p" (:id node)) (:path node))
      results)))

(defn render-query [[node & children] opt]
  (apply str (map (fn [child]
                    (str
                     (if (:rev (first child))
                       (str (if opt "OPTIONAL {?" "?")
                            (:id (first child)) " "
                            "<"(:pred (first child)) "> ?"
                            (if (:path node) "p" "") (:id node) " .\n" )
                       (str (if opt "OPTIONAL {?" "?")
                            (if (:path node) "p" "") (:id node) " "
                            "<"(:pred (first child)) "> ?"
                            (:id (first child)) " .\n" ))
                     (render-query child true) (if opt " }\n" "\n")))
                  children)))
(defn render-graph [[node & children]]
  (apply str (map (fn [child]
                    (str
                     (if (:rev (first child))
                       (str "?"
                            (:id (first child)) " "
                            "<"(:pred (first child)) "> ?"
                            (if (:path node) "p" "") (:id node) " .\n" )
                       (str "?"
                            (if (:path node) "p" "") (:id node) " "
                            "<"(:pred (first child)) "> ?"
                            (:id (first child)) " .\n" ))
                     (render-graph child) "\n"))
                  children)))

(defn query-map [query]
  (let [vals (q-map query)
        qm (new QuerySolutionMap)]
    (doall (map (fn [key] (.add qm (str key) (res (get vals key)))) (keys vals)))
    qm))


(defn build-query [view-data widget-data uri]
  (let [inf (. ModelFactory createRDFSModel (.union view-data widget-data))
        query  (qbuild inf (parse-uri uri))
        qm (query-map query)
        s (str "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX ont: <http://logangilmour.com/example-ontology#>

CONSTRUCT {"
             (render-graph query)
             "} WHERE {"
             (render-query query false)
             "}"
             )]
    (.add qm "root" (res "http://logangilmour.com/example-ontology#person"))  ;; TODO DIRTY DIRTY HACKS

    (let [done (.toString (new ParameterizedSparqlString s qm))]

      (.execConstruct
       (. QueryExecutionFactory sparqlService
          "http://localhost:8000/sparql/"
          done
          )))))

(defn query-builder [command reversed]
  (cond (= (:type command) "update")
        (if reversed
          [(simple-update "
DELETE { ?old ?p ?o }
WHERE { ?old ?p ?o }"
                           (plit (:value command))
                           (prop (:predicate command))
                           (res (:uri command)))
           (simple-update "
INSERT DATA {?s ?p ?o }"
                         (plit (:value command))
                         (prop (:predicate command))
                         (res (:uri command)))]
          [(simple-update "
DELETE { ?s ?p ?old }
WHERE { ?s ?p ?old }"
                           (res (:uri command))
                           (prop (:predicate command))
                           (plit (:value command)))
           (simple-update "
INSERT DATA {?s ?p ?o }"
                           (res (:uri command))
                           (prop (:predicate command))
                           (plit (:value command)))])
        (= (:type command) "create")
        (if reversed
          [(simple-update "INSERT DATA { ?s ?p ?o }"
                           (res (:value command))
                           (prop (:predicate command))
                           (res (:uri command)))]
          [(simple-update "INSERT DATA { ?s ?p ?o }"
                           (res (:uri command))
                           (prop (:predicate command))
                           (res (:value command)))])

        (= (:type command) "delete")
        (if reversed
          [(simple-update "DELETE DATA { ?s ?p ?o }"
                           (res (:value command))
                           (prop (:predicate command))
                           (res (:uri command)))]
          [(simple-update "DELETE DATA { ?s ?p ?o }"
                           (res (:uri command))
                           (prop (:predicate command))
                           (res (:value command)))])

       ))

(comment (if reversed

                   ))

(defn get-all-bindings [inf predicate]
  (concat (iterator-seq (.listStatements inf nil (prop (:binds db)) predicate))
          (iterator-seq (.listStatements inf nil (prop (:bindo db)) predicate))))


(defn serializer ;;TODO make this build sub-serializers that work for sub-projections
  ([view-data widget-data]

     (let [inf (. ModelFactory createRDFSModel (.union view-data widget-data))
            root (.asResource (.getObject (.getProperty inf
                                                        (res (:root db))
                                                        (prop (:haschild db)))))]


        (fn [data resource uri]
          ((s-rec root inf []) data nil resource uri "/view/"))
        ))
  ([view-data widget-data binding]

     (let [inf (. ModelFactory createRDFSModel (.union view-data widget-data))]
        (fn [data child resource uri] ((s-rec binding inf []) data child resource uri "/view/"))
        )))

(defn syncer [inf widget-data]
  (fn [command url]

    (let [predicate (res (:predicate command))
          binders (get-all-bindings inf predicate)
          bindings (map #(.getSubject %) binders)
          views (map (partial serializer inf widget-data) bindings)]

      (map (fn [binder]

             (let [binding (.getSubject binder)
                   reverse (= (.getURI (.getPredicate binder)) (db :binds))]

              (dissoc
               (cond
                (or (= (:type command) "update") (= (:type command) "create"))
                (assoc command
                  :value
                  ((serializer inf widget-data binding)
                   (build-query inf widget-data url)
                   (if (= (:type command) "update")
                     (plit (:value command))
                     (res (:value command)))
                   (res (:uri command))
                   url)
                  :binding
                  (.toString binding))

                (= (:type command) "delete")
                (assoc
                    (if reverse
                      (assoc command :uri (:value command) :value (:uri command))
                      command)
                  :binding (.toString binding))) :predicate)))
           binders)


      )))

(defn commander [uri pred data type]
  (cond (= type "update")
        {:uri uri :predicate pred :value data :type type}
        (= type "create")
        {:uri uri :predicate pred :value (uuid) :type type}
        (= type "delete")
        {:uri uri :predicate pred :value data :type type}))

(defn editor [inf]
  (fn [message]
    (let [uri (:uri message)
          binding (res (:binding message))
          data (:data message)
          type (:type message)
          statement (get-bound binding inf)
          binder (.getURI (.getPredicate statement))
          predicate (.asResource (.getObject statement))
          pred (.getURI predicate)
          resource (res uri)
          reverse (= binder (db :binds))
          command (commander uri pred data type)
          update (query-builder command reverse)]
      (doall (map (fn [up]
                    (client/post "http://localhost:8000/update/" {:form-params {:update up}})) update))
      command)))



(def example-view
  "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix data: <http://logangilmour.com/data-binder#> .
@prefix ex: <http://logangilmour.com/example-view#> .
@prefix ont: <http://logangilmour.com/example-ontology#> .
@prefix dc: <http://purl.org/dc/elements/1.1/> .
@prefix w: <http://logangilmour.com/bootstrap-widgets#> .

data:root data:haschild ex:person-manager .

ex:person-manager
  rdf:type w:row ;
  dc:title \"The Application\" ;
  dc:description \"Here we are\" ;
  data:haschild ex:person-list-container ;
  data:contains ex:person-container .

ex:person-list-container
  data:haschild ex:person-list ;
  rdf:type w:column4 ;
  data:rank 1 .

ex:person-list
  rdf:type w:list ;
  data:binds rdf:type ;
  dc:title \"List of People\" ;
  dc:description \"A list of people contained within the application\" ;
  data:haschild ex:person-item .

ex:person-item
  rdf:type w:list-item ;
  data:path \"test\" ;
  data:haschild ex:person-projector ;
  data:projects ex:person-container .

ex:person-projector
  rdf:type w:projector ;
  data:bindo ont:name ;
  dc:title \"select\" ;
  dc:description \"choose a person\" .

ex:person-container
  rdf:type w:column8 ;
  dc:title \"Person\" ;
  dc:description \"A person\" ;
  data:rank 2 .

ex:person-container data:haschild ex:person-name-container , ex:person-age ,  ex:person-deleter .

ex:person-deleter rdf:type w:deleter ;
                  data:bindo rdf:type ;
                  dc:title \"Remove\" ;
                  data:rank 3 ;
                  dc:description \"Permanently delete a person from the database\" .

ex:person-name-container
  data:haschild ex:person-name ;
  dc:title \"thing\" ;
  dc:description \"stuff\" ;
  rdf:type w:h1 .

ex:person-name rdf:type w:text-field ;
               data:rank 1 ;
               data:bindo ont:name ;
               dc:title \"Name\" ;
               dc:description \"The name of a person\" .

ex:person-age rdf:type w:text-field ;
              data:rank 2 ;
              data:bindo ont:age ;
              dc:title \"Age\" ;
              dc:description \"The age of a person\".

")

(def widgets
  "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix data: <http://logangilmour.com/data-binder#> .
@prefix w: <http://logangilmour.com/bootstrap-widgets#> .

w:projector rdfs:subClassOf data:relator .

w:column8 rdfs:subClassOf data:container .

w:column4 rdfs:subClassOf data:container .

w:row rdfs:subClassOf data:container .

w:list rdfs:subClassOf data:relator .

w:list-item rdfs:subClassOf data:container .

w:deleter rdfs:subClassOf data:relator .

w:paragraph rdfs:subClassOf data:relator .

w:text-field rdfs:subClassOf data:relator .

w:h1 rdfs:subClassOf data:container .
")


(defn page [body]
  (hic/html
   [:html
    [:head
     [:meta {:name "viewport" :content "width=device-width, initial-scale=1.0"}]

     [:link {:href "/css/bootstrap.min.css" :rel "stylesheet" :media "screen"}]
     [:title "FIXME"]]
    [:body body
     [:script {:type "text/javascript" :src "/js/jquery.min.js"}]
     [:script {:type "text/javascript" :src "/js/bootstrap.min.js"}]
     [:script {:type "text/javascript" :src "/js/underscore-min.js"}]
     [:script {:type "text/javascript" :src "/js/update.js"}]]

    ]
   ))

(defn startup []
  (do
    (print "starting up server!")
    ;;(logger/init)
    ))

;;(def data-model (to-model data))

(def broadcast-channel (permanent-channel))
(def edit (editor (to-model example-view)))
(def syn (syncer (to-model example-view) (to-model widgets)))


(def channels (atom {}))

(defn register [ch url]
  (siphon (map* edit (map* decode-json ch)) broadcast-channel)
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
                                       (siphon (map* encode-json->string (map* (fn [message] (syn message url)) broadcast-channel)) new-ch)
                                       new-ch))]
                      (siphon url-ch ch) ;; listen on your url channel
                      (doall (map (partial println "\nConnection: ") (keys all)))
                      (assoc all url url-ch)
                      ))))

(defn chat-handler [ch handshake]
  (receive ch
           (fn [input]
             (let [url (:* (:params handshake))]
               (println "registering a connection to url " url)
               ;;(let jc [(map* decode-json ch)])
               (register ch url))
             )))


(defroutes main-routes
  (GET "/async/*" [] (wrap-aleph-handler chat-handler))
  (GET "/view/*" [*]
       (page ((serializer (to-model example-view) (to-model widgets)) (build-query (to-model example-view) (to-model widgets) *) (res "http://logangilmour.com/example-ontology#person") *)))
  (route/resources "/")
  (route/not-found "Page not found"))

(defn -main
  [& args]
  (start-http-server (wrap-ring-handler main-routes)
                     {:port 8080 :websocket true}))
