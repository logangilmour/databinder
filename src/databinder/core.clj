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
   [clojure.java.io :as io]
   )
  (:import (com.hp.hpl.jena.rdf.model ModelFactory ResourceFactory Model SimpleSelector)
           (java.io ByteArrayInputStream)
           (com.hp.hpl.jena.query QuerySolutionMap ParameterizedSparqlString QueryExecutionFactory)
           (com.hp.hpl.jena.update GraphStoreFactory UpdateExecutionFactory)
           [org.mozilla.javascript Context ScriptableObject Function NativeArray]))



(defmacro dbg [string fun & args]
  `(let [expr# (~fun ~@args)] (println (str ~string " : " expr#)) expr#))

;; For now, we'll just be building an rdf model out of turtle-data, so this thing is simple and dumb. Later it should go to a url and get arbitrary RDF.

(defn prop [uri]
  (. ResourceFactory createProperty uri))

(defn res [uri]
  (. ResourceFactory createResource uri))

(defn plit [val]
  (. ResourceFactory createPlainLiteral val))

(defn uuid [] (str "urn:uuid:" (java.util.UUID/randomUUID)))

(defn skolemize [model bnode]

  (let [id (res (uuid))]
    (doseq [statement (doall (iterator-seq (.listStatements model nil nil bnode)))] ;;TODO replace map with doseq in other places
      (let [s (.getSubject statement)
            p (.getPredicate statement)]
        (.remove model statement)
        (.add model (.createStatement model s p id))))
    (doseq [statement (doall (iterator-seq (.listStatements model bnode nil nil)))]
      (let [p (.getPredicate statement)
            o (.getObject statement)]
        (.remove model statement)
        (.add model (.createStatement model id p o))))))

(defn skolemize-model [model]
  (let [statements (doall (iterator-seq (.listStatements model)))]
    (doseq [statement statements]
      (let [s (.getSubject statement)
            o (.getObject statement)]
        (if (and (.isResource s) (not (.getURI s)))
          (skolemize model s))
        (if (and (.isResource o) (not (.getURI o)))
          (skolemize model o))))
    ))

(def js-env (atom nil))

(defn template-env []
  (let [cx (Context/enter)
        scope (.initStandardObjects cx)]
    (.evaluateReader cx scope (io/reader (io/resource "mustache.js")) "mustache.js" 1 nil)
    (.evaluateReader cx scope (io/reader (io/resource "templates.js")) "templates.js" 1 nil)
    (swap! js-env (fn [old] {:context cx :scope scope}))))

(template-env)

(defn mapper [obj vals]
  (doseq [key (keys vals)]
    (ScriptableObject/putProperty obj key (get vals key))))

(defn template [js]
  (fn [resource bindings active url params vals]
    (let [cx (Context/enter)
          shared-scope (:scope @js-env)
          scope (.newObject cx shared-scope)
          a (.setPrototype scope shared-scope)
          b (.setParentScope scope nil)]

      (mapper scope params)

      (ScriptableObject/putProperty scope "uri" resource)
      (ScriptableObject/putProperty scope "binding" bindings)
      (ScriptableObject/putProperty scope "url" url)
      (ScriptableObject/putProperty scope "active" active)
      (ScriptableObject/putProperty scope "vals" (hic/html vals))
      (ScriptableObject/putProperty scope "values" (new NativeArray (to-array (flatten vals))))
      (ScriptableObject/putProperty scope "first" (first (flatten vals)))
      (ScriptableObject/putProperty scope "second" (second (flatten vals)))

      (.evaluateString cx scope "var ret=\"\"; function emit(val){ret=val;};" "<cmd>" 1 nil)
      (.evaluateString cx scope js "<cmd>" 1 nil) ;;TODO sort out line numbers
      ;;(.evaluateString cx scope (str "var ret = stuff.resource") "<cmd>" 1 nil)
      (Context/exit)
      (.get scope "ret" scope))))


(defn to-model [string-data]
  (let [model (. ModelFactory createDefaultModel)]
    (.read model (new ByteArrayInputStream (.getBytes string-data "UTF-8")) "http://localhost:3000" "TURTLE")
    (skolemize-model model)
    model))

;; To serialize relational data based on relational data, we first make a serializing function out of some relational data. For now, we'll cheat and pass in the function bindings as part of the serializer. We'll also ditch ordering for now. Also, the view-data is assumed to just be a string of turtle for now.

(defn uris [base & resources]
  (apply assoc {} (flatten (map (fn [resource] [resource (str base (name resource))]) resources))))


(def dc (uris "http://purl.org/dc/elements/1.1/" :title :description))


(def bind (uris "http://logangilmour.com/data-binder#" :js :parameter :rank :root :application :subject :object :child :container :projection :path))

(defn relator [statement model resource]

  (let [binder (.getURI (.getPredicate statement))]
    (if (= binder (bind :object))
      (iterator-seq (.listResourcesWithProperty model (prop (.getURI (.asResource (.getObject statement)))) resource ))
      (iterator-seq
       (.listObjectsOfProperty model resource
                               (prop
                                (.getURI
                                 (.asResource
                                  (.getObject statement)))))))
    ))

(defn literalize [res]
  (if (.isLiteral res)
    (.getString (.asLiteral res))
    (.toString res)))

(defn get-bound [binding model]
  (or  (.getProperty model binding (prop (bind :object)))
       (.getProperty model binding (prop (bind :subject)))))

(defn relate-right [model predicate resource]
  (iterator-seq (.listObjectsOfProperty model resource predicate)))

(defn relate-left [model predicate resource]
  (iterator-seq (.listResourcesWithProperty model predicate resource)))


(def widgets (to-model

             "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix bind: <http://logangilmour.com/data-binder#> .
@prefix w: <http://logangilmour.com/bootstrap-widgets#> .
@prefix dc: <http://purl.org/dc/elements/1.1/> .

dc:title bind:parameter \"title\".

w:before bind:parameter \"before\".

w:after bind:parameter \"after\".

w:projector
  rdfs:subClassOf bind:container ;
  bind:js \"emit(projector(this));\" .


w:column8
  rdfs:subClassOf bind:container ;
  bind:js \"emit(column8(this));\" .

w:column4
  rdfs:subClassOf bind:container ;
  bind:js \"emit(column4(this));\" .

w:row
  rdfs:subClassOf bind:container ;
  bind:js \"emit(row(this));\" .

w:list
  rdfs:subClassOf bind:container ;
  bind:js \"emit(list(this));\" .

w:list-item
  rdfs:subClassOf bind:container ;
  bind:js \"emit(listItem(this));\" .

w:deleter
  rdfs:subClassOf bind:container ;
  bind:js \"emit(deleter(this));\" .

w:span
  rdfs:subClassOf bind:container ;
  bind:js \"emit(span(this));\" .

w:plain
  rdfs:subClassOf bind:container ;
  bind:js \"emit(this.vals);\" .

w:checkbox
  rdfs:subClassOf bind:container ;
  bind:js \"emit(checkbox(this));\" .

w:check
  rdfs:subClassOf bind:container ;
  bind:js \"emit(check(this));\" .

w:value
  rdfs:subClassOf bind:container ;
  bind:js \"emit(value(this));\" .

w:creator
  rdfs:subClassOf bind:container ;
  bind:js \"emit(creator(this));\" .

w:paragraph
  rdfs:subClassOf bind:container ;
  bind:js \"emit(paragraph(this));\" .

w:text-field
  rdfs:subClassOf bind:container ;
  bind:js \"emit(textField(this));\" .

w:popup
  rdfs:subClassOf bind:container ;
  bind:js \"emit(popup(this));\".

w:datepicker
  rdfs:subClassOf bind:container ;
  bind:js \"emit(datepicker(this));\".
"

              ))


(def params
  (apply assoc {} (flatten (map (fn [statement]
                                  [(.getURI (.getSubject statement))
                                   (.getString (.getObject statement))])
                                (doall (iterator-seq
                                        (.listStatements widgets
                                                         nil
                                                         (prop (bind :parameter))
                                                         nil)))))))

(defn get-params [resource model]
  (apply assoc {}
         (flatten
          (map (fn [pr]
                 (let [p (prop pr)]
                   (let [param (first (relate-right model p resource))]
                     (if param
                       [(get params pr) (.getString param)]
                       [(get params pr) nil]))))
               (keys params)
               ))))

(defn parse-uri [uri]
  (let [parts (filter (comp not (partial re-matches #"^\s*$")) (clojure.string/split uri #"/"))]

    (if (empty? parts)
     {}
     (apply assoc {} parts))))


(defn types [model binding] (set (map #(.toString %) (relate-right model (prop "http://www.w3.org/1999/02/22-rdf-syntax-ns#type") binding))))

(defn get-subs [model binding] ;;TODO make this deep.
  (filter #(not (contains? (types model %) (bind :container))) (relate-right model (prop (bind :child)) binding)))


(defn rdf->str [resource]
  (cond
   (.isResource resource)
   (.getURI resource)
   (.isLiteral resource)
   (.getString resource)
   :default
   (.toString resource)))

(defn s-rec [binding model url-parts]

  (let [children (sort-by (fn [resource]
                            (let [p (.getProperty model resource (prop (bind :rank)))]
                              (if p
                                (.getString (.asLiteral (.getObject p)))
                                "")))
                          (relate-right model (prop (bind :child)) binding))
        bindings (clojure.string/join ", "
                                      (map #(.getURI %)
                                           (get-subs model binding)))

        projector (first (relate-left model (prop (bind :projection)) binding)) ;;TODO only works for container pointed at by projector for now.
        path (if projector (first (relate-right model (prop (bind :path)) projector)))

        root (first (relate-right model (prop (bind :root)) binding))

        types (types model binding)

        new-url-parts (if path
                  (conj url-parts (.getString (.asLiteral path)))
                  url-parts)

        sub-calls (map (fn [child] (s-rec child model url-parts)) children) ;; make the next part of the tree
        params (get-params binding model)

        myp (first (relate-right model (prop (bind :path)) binding))
        mypath (if myp (.getString (.asLiteral myp)))]

    (cond
     (contains? types (bind :container))
     (let [func (template (.getString
                           (reduce (fn [accum val]
                                     (or accum
                                         (first (relate-right model
                                                              (prop (bind :js))
                                                              (res val) ))))
                                   nil types)))
           ]
         (fn [data child resource url built-url]
           (let [ppath (if path (.getString (.asLiteral path)))

                 p (get (parse-uri url) ppath)
                 projected (if (and p (res p))
                             (if (.containsResource data (res p))
                               (res p)))

                 current (if mypath (get (parse-uri url) mypath))

                 active (if mypath (= current (.getURI resource)))
                 built-url (or (if mypath (str built-url mypath "/" (.getURI resource) "/")) built-url)]

             (cond
              root
              (func
               (.getURI root)
                bindings ;;(.getURI binding) TODO make it so we actually support multiple bindings in the js
                active
                built-url
                params
                (map (fn [sub-call] (sub-call data nil root url built-url)) sub-calls))
              ppath
              (if projected
                (func
                  (.getURI projected)
                  bindings ;;(.getURI binding)
                  active
                  built-url
                  params
                  (map (fn [sub-call] (sub-call data nil projected url (str built-url ppath "/" (.getURI resource) "/"))) sub-calls))
                "") ;;TODO what is this exactly?
              :default

               (func
                (.getURI resource)
                bindings ;;(.getURI binding)
                active
                built-url
                params
                (map (fn [sub-call] (sub-call data nil resource url built-url)) sub-calls))
               ))))

     :default
     (let [statement (get-bound binding model) ;; fuckery for binding

           relation-getter (fn [data resource] (filter #(not (and (.isResource %) (not (.getURI %)))) (relator statement data resource)))]
       (fn [data child resource url built-url]

         (let [ppath (if path (.getString (.asLiteral path)))

               p (get (parse-uri url) ppath)
               projected (if (and p (res p))
                           (if (.containsResource data (res p))
                             (res p)))

               rel (if child   ;; get sub-resources (a single one if partial rendering)
                         (seq [child])
                         (relation-getter data resource))

               related (if projected
                         (seq (if (contains? (set rel) projected)
                                [projected]
                                []
                                ))
                         rel)


               current (if mypath (get (parse-uri url) mypath))
               active (if mypath (= current (.getURI resource)))
               built-url (or (if mypath (str built-url mypath "/" (.getURI resource) "/")) built-url)
               vals (if (empty? sub-calls)
                   (map literalize related)
                   (map (fn [sub-call]
                          (map
                           (fn [val]
                             (sub-call data nil val url built-url))
                           related))
                        sub-calls))]
           vals)))

          )))

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
                  (filter (fn [b] (empty? (relate-left model (prop (bind :projection)) b))) (relate-right model (prop (bind :child)) binding))
                  (relate-right model (prop (bind :projection)) binding))

        projector (first (relate-left model (prop (bind :projection)) binding))
        ppath (if projector (first (relate-left model (prop (bind :path)) projector)))

        types (set (map #(.toString %) (relate-right model (prop "http://www.w3.org/1999/02/22-rdf-syntax-ns#type") binding)))

        path (first (relate-right model (prop (bind :path)) binding))
        onpath (if path (get uri (.getString (.asLiteral path))))

        subs
        (if path
         (if onpath
           (map (fn [child] (qb child model uri)) children)
           '())
         (map (fn [child] (qb child model uri)) children))]

    (cond
     (contains? types (bind :container))
     subs
     :default
          (let [statement (get-bound binding model)
                binder (.getURI (.getPredicate statement))
                predicate (.asResource (.getObject statement)) ;;TODO handle type thing with path thing
                reverse (= binder (bind :object))]

            [{:pred (.getURI predicate) :rev reverse :path onpath} subs]))));;TODO this is almost certainly bad. Paths are not handled right I think.



(defn qbuild [inf uri]
  (let [root (.asResource (.getObject (.getProperty inf
                                                        (res (bind :application))
                                                        (prop (bind :child)))))
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
             "?s ?p ?o";;(render-graph query)
             "} WHERE {"
             "?s ?p ?o";;(render-query query false)
             "}"
             )]

    (.add qm "root" (res "http://xmlns.com/foaf/0.1/Person"))  ;; TODO DIRTY DIRTY HACKS

    (let [done (.toString (new ParameterizedSparqlString s qm))

          rval (.execConstruct
       (. QueryExecutionFactory sparqlService
          "http://localhost:8000/sparql/"
          done
          ))]

      rval

      )))

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
        (or (= (:type command) "create") (= (:type command) "assoc"))
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
  (concat (iterator-seq (.listStatements inf nil (prop (bind :object)) predicate))
          (iterator-seq (.listStatements inf nil (prop (bind :subject)) predicate))))


(defn serializer ;;TODO make this build sub-serializers that work for sub-projections
  ([view-data widget-data]

     (let [inf (. ModelFactory createRDFSModel (.union view-data widget-data))
            root (.asResource (.getObject (.getProperty inf
                                                        (res (bind :application))
                                                        (prop (bind :child)))))]


        (fn [data resource uri]
          ((s-rec root inf []) data nil resource uri "/view/"))
        ))
  ([view-data widget-data binding]

     (let [inf (. ModelFactory createRDFSModel (.union view-data widget-data))]
       (fn [data child resource uri] (hic/html ((s-rec binding inf []) data child resource uri "/view/")))
        )))

(defn syncer [inf widget-data]
  (fn [command url]

    (let [predicate (res (:predicate command))
          binders (get-all-bindings inf predicate)
          bindings (map #(.getSubject %) binders)
          views (map (partial serializer inf widget-data) bindings)]

      (map (fn [binder]

             (let [binding (.getSubject binder)
                   reverse (= (.getURI (.getPredicate binder)) (bind :object))]

               (dissoc
                (cond
                 (or (= (:type command) "update") (= (:type command) "create"))

                 (if reverse
                   (assoc command
                     :uri (:value command)
                     :value
                     ((serializer inf widget-data binding)
                      (build-query inf widget-data url)
                      (res (:uri command))
                      (if (= (:type command) "update")
                        (plit (:value command))
                        (res (:value command)))
                      url)
                     :binding
                     (.toString binding))
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
                     (.toString binding)))

                 (or (= (:type command) "delete") (= (:type command) "assoc"))
                 (assoc
                     (if reverse
                      (assoc command :uri (:value command) :value (:uri command))
                      command)
                   :binding (.toString binding))) :predicate)))
           binders)


      )))
(comment (if reverse
                         (assoc command :uri (:value command) :value (:uri command))
                         command))
(defn commander [uri pred data type]

  (cond
   (= type "assoc")
   {:uri uri :predicate pred :value data :type type}
   (= type "update")
   {:uri uri :predicate pred :value data :type type}
   (= type "create")
   {:uri uri :predicate pred :value (if (= data "") (uuid) data) :type type}
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
          reverse (= binder (bind :object))
          command (commander uri pred data type)

          update (query-builder command reverse)]
      (doall (map (fn [up]
                    (client/post "http://localhost:8000/update/" {:form-params {:update up}})) update))
      (if reverse
                      (assoc command :uri (:value command) :value (:uri command))
                      command))))



(def example-view (to-model
                   "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix bind: <http://logangilmour.com/data-binder#> .
@prefix ex: <http://logangilmour.com/example-view#> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix dc: <http://purl.org/dc/elements/1.1/> .
@prefix w: <http://logangilmour.com/bootstrap-widgets#> .

bind:application bind:child ex:person-manager .

ex:person-manager
  rdf:type w:row ;
  bind:root foaf:Person ;
  bind:child
    [bind:child ex:person-list ,
      [rdf:type w:creator ;
       bind:rank 2;
       bind:child [bind:object rdf:type]] ;
     rdf:type w:column4 ;
     bind:rank 1] ,
    ex:person-container .


ex:person-list
  dc:title \"List of People\" ;
  rdf:type w:list ;
  bind:child
    [bind:object rdf:type ;
     bind:child
       [rdf:type w:list-item ;
        bind:path \"test\" ;
        bind:projection ex:person-container, ex:person-relator, ex:pr;
        bind:child
          [rdf:type w:projector ;
           bind:child ex:fullName]]] .

ex:fullName #bind:subject foaf:givenName .
  rdf:type w:span ;
  bind:child
    [rdf:type w:span ;
     w:after \" \";
     bind:child [bind:subject foaf:givenName] ;
     bind:rank 1],
    [rdf:type w:span ;
     bind:child [bind:subject foaf:familyName] ;
     bind:rank 2] .

ex:known
  dc:title \"Knows\" ;
  rdf:type w:list ;
  bind:rank 6 ;
  bind:child ex:knower .

ex:knower bind:subject foaf:knows;
     bind:child
      [rdf:type w:list-item;
       bind:child ex:fullName] .

ex:mini-list
  dc:title \"All\" ;
  rdf:type w:list ;
  bind:rank 0 ;
  bind:root foaf:Person ;
  bind:child
    [bind:object rdf:type ;
     bind:child
       [rdf:type w:list-item ;
        bind:child
          ex:fullName,
          [rdf:type w:checkbox ;
           bind:child
            ex:pr,
            ex:person-relator]]] .

ex:pr
  bind:object foaf:knows;
  bind:rank 2 ;
  bind:child
    [rdf:type w:value] .

ex:person-relator rdf:type w:value .

ex:person-container
  dc:title \"Person\" ;
  rdf:type w:column4 ;
  bind:rank 2 ;
  bind:child
    [dc:title \"Given Name\" ;
     rdf:type w:text-field ;
     bind:rank 1 ;
     bind:child [bind:subject foaf:givenName]] ,
    [dc:title \"Family Name\" ;
     rdf:type w:text-field ;
     bind:rank 2 ;
     bind:child [bind:subject foaf:familyName]] ,

    [dc:title \"Birthday\" ;
     rdf:type w:datepicker ;
     bind:rank 3 ;
     bind:child [bind:subject foaf:birthday]] ,

    [dc:title \"Status\" ;
     rdf:type w:text-field ;
     bind:rank 4 ;
     bind:child [bind:subject foaf:status]] ,

    [dc:title \"Remove\" ;
     rdf:type w:deleter ;
     bind:rank 0 ;
     bind:child [bind:subject rdf:type]] ,
     ex:known ,
       [rdf:type w:popup;
        bind:rank 7 ;
        dc:title \"Edit Known...\" ;
        bind:child ex:mini-list ] .

"))


(defn page [body]
  (hic/html
   [:html
    [:head
     [:meta {:name "viewport" :content "width=device-width, initial-scale=1.0"}]

     [:link {:href "/css/bootstrap.min.css" :rel "stylesheet" :media "screen"}]
     [:link {:href "/css/datepicker.css" :rel "stylesheet" :media "screen"}]
     [:title "FIXME"]]
    [:body body
     [:script {:type "text/javascript" :src "/js/jquery.min.js"}]
     [:script {:type "text/javascript" :src "/js/bootstrap.min.js"}]
     [:script {:type "text/javascript" :src "/js/bootstrap-datepicker.js"}]
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
(def edit (editor example-view))
(def syn (syncer example-view widgets))


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
                                       (siphon (map* encode-json->string (map* (fn [message]
                                                                                 (syn message url)
                                                                                 ) broadcast-channel)) new-ch)
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
       (page ((serializer example-view widgets) (build-query example-view widgets *) (res "http://logangilmour.com/example-ontology#person") *)))
  (route/resources "/")
  (route/not-found "Page not found"))


(defn -main
  [& args]
  (start-http-server (wrap-ring-handler main-routes)
                     {:port 8080 :websocket true}))
