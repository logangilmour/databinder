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
   [clojure.string :as s]
   )
  (:import (com.hp.hpl.jena.rdf.model ModelFactory ResourceFactory Model SimpleSelector)
           (java.io ByteArrayInputStream)
           (com.hp.hpl.jena.query QuerySolutionMap ParameterizedSparqlString QueryExecutionFactory)
           (com.hp.hpl.jena.update GraphStoreFactory UpdateExecutionFactory)
           [org.mozilla.javascript Context ScriptableObject Function NativeArray]))



(defmacro dbg [string fun & args]
  `(let [expr# (~fun ~@args)] (println (str ~string " : " expr#)) expr#))

;; For now, we'll just be building an rdf model out of turtle-data, so this thing is simple and dumb. Later it should go to a url and get arbitrary RDF.


(defn make-map [f1 f2 coll]
  (apply hash-map (flatten (map (fn [val] [(f1 val) (f2 val)]) coll))))

(defn prop [uri]
  (if uri
   (. ResourceFactory createProperty uri)))

(defn res [uri]
  (if uri
    (. ResourceFactory createResource uri)))

(defn plit [val]
  (. ResourceFactory createPlainLiteral val))

(defn uris [base & resources]
  (apply assoc {} (flatten (map (fn [resource] [resource (str base (name resource))]) resources))))

(defn literalize [res]
  (if res
    (if (.isLiteral res)
      (.getString (.asLiteral res))
      (.toString res))))

(defn relate-right
  [model predicate resource]
  (iterator-seq (.listObjectsOfProperty model resource predicate)))

(defn relate-left [model predicate resource]
  (iterator-seq (.listResourcesWithProperty model predicate resource)))

(defn uuid [] (str "urn:uuid:" (java.util.UUID/randomUUID)))

(def rdfs (uris "http://www.w3.org/1999/02/22-rdf-syntax-ns#" :first :rest :nil :List))

(defn rdf-seq [mod top]
  (seq (loop [model mod
              head top
              list []]
         (if (or (not head) (= head (res (rdfs :nil))))
           list
           (recur model
                  (first (relate-right model (prop (rdfs :rest)) head))
                  (conj list
                        (first (relate-right model (prop (rdfs :first)) head))))))))

(defn make-seq [model coll]
  (if (empty? coll)
    (res (rdfs :nil))
    (let [current (res (uuid))]
      (.add model (.createStatement model current (prop (rdfs :first)) (first coll)))
      (.add model (.createStatement model current (prop (rdfs :rest)) (make-seq model (rest coll))))
      current)))

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
    (.evaluateReader cx scope (io/reader (io/resource "underscore.js")) "mustache.js" 1 nil)
    (.evaluateReader cx scope (io/reader (io/resource "templates.js")) "templates.js" 1 nil)
    (swap! js-env (fn [old] {:context cx :scope scope}))))

(template-env)

(defn mapper [obj vals]
  (doseq [key (keys vals)]
    (ScriptableObject/putProperty obj key (get vals key))))

(defn template [js]
  (fn [resource bindings url params vals]
    (let [cx (Context/enter)
          shared-scope (:scope @js-env)
          scope (.newObject cx shared-scope)
          a (.setPrototype scope shared-scope)
          b (.setParentScope scope nil)]

      (mapper scope params)

      (ScriptableObject/putProperty scope "uri" resource)
      (ScriptableObject/putProperty scope "binding" bindings)
      (ScriptableObject/putProperty scope "url" url)
      (ScriptableObject/putProperty scope "vals" (hic/html vals))
      (ScriptableObject/putProperty scope "values" (new NativeArray (to-array (flatten vals))))
      (ScriptableObject/putProperty scope "first" (first (flatten vals)))
      (ScriptableObject/putProperty scope "second" (second (flatten vals))) ;;TODO these won't work if we have n objects

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


(def dc (uris "http://purl.org/dc/elements/1.1/" :title :description))


(def bind (uris "http://logangilmour.com/data-binder#" :js :debug :index :rank :root :application :subject :object :child :container :from :path :view :base :param :end :name :binding :compiled))



(defn relator [statement model resource]
;;(println "WTF " statement " and " resource)
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


(defn get-bound [binding model]
  (or  (.getProperty model binding (prop (bind :object)))
       (.getProperty model binding (prop (bind :subject)))))


(def widgets (to-model

             "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix : <http://logangilmour.com/data-binder#> .
@prefix w: <http://logangilmour.com/bootstrap-widgets#> .
@prefix dc: <http://purl.org/dc/elements/1.1/> .
@prefix cb: <http://logangilmour.com/bootstrap-widgets/compare-box#> .
@prefix pl: <http://logangilmour.com/bootstrap-widgets/plain-list#> .
@prefix tm: <http://logangilmour.com/bootstrap-widgets/type-manager#> .

:component :param dc:title .
dc:title :name \"title\".


:component :param :debug .
:debug :name \"debug\" .

:component :param dc:description .
dc:description :name \"description\" .

:container :param w:id .
w:id :name \"id\".

:component :param :root .
:component :param :path .
:component :param :from .
:component :param :rank .
:component :param :index .

:index :name \"index\" .


:object rdfs:domain :binding .
:subject rdfs:domain :binding .

:child rdfs:domain :component .
:child rdfs:range :component .

:binding :param :subject .
:binding :param :object .

:binding rdfs:subClassOf :component .

:container rdfs:subClassOf :component .

:view rdfs:subClassOf :component .

:application rdfs:subClassOf :view .




w:projector
  rdfs:subClassOf :container ;
  :js \"emit(projector(this));\" .


w:column8
  rdfs:subClassOf :container ;
  :js \"emit(column8(this));\" .

w:column4
  rdfs:subClassOf :container ;
  :js \"emit(column4(this));\" .

w:row
  rdfs:subClassOf :container ;
  :js \"emit(row(this));\" .

w:list
  rdfs:subClassOf :container ;
  :js \"emit(list(this));\" .

pl:list rdfs:subClassOf :view ;
  :param pl:binding, dc:title, :root ;
  :end pl:ending ;
  :base [a w:list ;
         :root :root ;
         :debug \"thing\";
         dc:title dc:title;
         :child pl:binding] .

pl:binding :child pl:ending.

pl:ending a w:li .

w:li rdfs:subClassOf :container ;
  :js \"emit(li(this));\" .


w:list-item-
  rdfs:subClassOf :container ;
  :js \"emit(listItem(this));\" .

w:list-item rdfs:subClassOf :view ;
  :param w:item-url ;
  :end w:item-end ;
  :base [a w:list-item- ;
         :path w:item-url ;
         :child [a w:active ;
                 w:check-path w:item-url ;
                 :rank 1 ],
                w:item-end] .

w:item-end a w:projector ;
  :rank 2 .

w:link-list rdfs:subClassOf :view ;
  :param w:list-path, w:list-binding, w:list-title;
  :end w:list-end ;
  :base
    [a w:list ;
     dc:title w:list-title ;
     :child w:list-binding].

w:list-binding :child w:list-end .

w:list-end a w:list-item;
  w:item-url w:list-path.



tm:type-manager rdfs:subClassOf :view ;
:param tm:type, tm:title, tm:item , tm:path;
:end tm:ending;
:base
  [a w:row ;
  :root tm:type ;
  :child
    [a w:column4 ;
     :rank 1 ;
     :child
      [a w:link-list ;
       :rank 1 ;
       w:list-title tm:title ;
       w:list-binding [:object rdf:type ] ;
       :child tm:item ;
       w:list-path tm:path] ,
      [a w:creator ;
       :rank 2;
       :child [:object rdf:type]]] ,
      tm:ending].

tm:ending a w:column4;
:from tm:path ;
:rank 2.





cb:relator rdfs:subClassOf :view ;
  :param cb:binding , cb:path ;
  :base [a w:checkbox ;
         :child cb:binding ,
                [a w:value ; :from cb:path ; :rank 2 ]] .

cb:binding
  :from cb:path ;
  :rank 1 ;

  :child [a w:value] .



w:item :rank 2 .


w:activating-list-item rdfs:subClassOf :view .

w:deleter
  rdfs:subClassOf :container ;
  :js \"emit(deleter(this));\" .

w:text
  rdfs:subClassOf :container ;
  :js \"emit(span(this));\" ;
  :param w:before , w:after.

w:join-text rdfs:subClassOf :container ;
  :js \"emit(join(this));\" ;
  :param w:join-with .

w:join-with :name \"joinWith\" .

w:after :name \"after\".
w:before :name \"before\".

w:plain
  rdfs:subClassOf :container ;
  :js \"emit(this.vals);\" .

w:checkbox
  rdfs:subClassOf :container ;
  :js \"emit(checkbox(this));\" .

w:check
  rdfs:subClassOf :container ;
  :js \"emit(check(this));\" .

w:value
  rdfs:subClassOf :container ;
  :js \"emit(value(this));\" .

w:creator
  rdfs:subClassOf :container ;
  :js \"emit(creator(this));\" .

w:paragraph
  rdfs:subClassOf :container ;
  :js \"emit(paragraph(this));\" .

w:text-field
  rdfs:subClassOf :container ;
  :js \"emit(textField(this));\" .

w:popup
  rdfs:subClassOf :container ;
  :js \"emit(popup(this));\".

w:string
  rdfs:subClassOf :container ;
  :js \"emit(string(this));\".

w:datepicker
  rdfs:subClassOf :container ;
  :js \"emit(datepicker(this));\".

w:active-test
  rdfs:subClassOf :container ;
  :js \"if(this.vals==this.uri){emit('active')}else{emit('')};\" .

w:active rdfs:subClassOf :view ;
  :param w:check-path ;
  :base [a w:active-test ;
         :child [a w:value ; :from w:check-path] ] .
"

              ))


(comment (def params
    (apply assoc {} (flatten (map (fn [statement]
                                    [(.getURI (.getSubject statement))
                                     (.getString (.getObject statement))])
                                  (doall (iterator-seq
                                          (.listStatements widgets
                                                           nil
                                                           (prop (bind :parameter))
                                                           nil))))))))

(comment (defn get-params [resource model]
    (apply assoc {}
           (flatten
            (map (fn [pr]
                   (let [p (prop pr)]
                     (let [param (first (relate-right model p resource))]
                       (if param
                         [(get params pr) (.getString param)]
                         [(get params pr) nil]))))
                 (keys params)
                 )))))

(defn parse-uri [uri]
  (vec (filter (comp not (partial re-matches #"^\s*$")) (clojure.string/split uri #"/"))))


(defn types [model binding]
  (println "Trying to deal with " binding)
  (if binding
    (set (map #(.toString %) (relate-right model (prop "http://www.w3.org/1999/02/22-rdf-syntax-ns#type") binding)))
    (set [])))

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

(defn get-property [model resource property]
  (filter identity (map #(if % (.getURI %))
         (flatten
          (map
           (fn [type]
             (relate-right model property type))
           (relate-right model (prop "http://www.w3.org/1999/02/22-rdf-syntax-ns#type") resource))))))


(defn named-params [model context]
  (into {} (keep #(if (first %) %) (make-map #(literalize (first (relate-right model (prop (bind :name)) (res %))))
                           #(literalize ( get context %))
                           (keys context)))))


(defn relate-lookup [model context predicate resource]
  (map #(or (get context (literalize %)) %) (iterator-seq (.listObjectsOfProperty model resource predicate))))

(defn build-context [model parent-context binding]
  (let [params (get-property model binding (prop (bind :param)))

        context (make-map identity
                          #(first (relate-lookup model parent-context (prop %) binding)) params)
        ] ;;TODO these could be parameters themselves
    context))

(defn copy-context [model local-context node]
  (doseq [param (keys local-context)]
    (if (get local-context param)
      (.add model (.createStatement model node (prop param) (get local-context param))))))

(defn merge-context [parent-context context]
  (let [tits (merge parent-context
                    (into {} (keep (fn [[key val :as pair]] (if (not (get parent-context key)) pair))
                                   context)))]

    tits))

(defn debug [message context]
  (if (get context (bind :debug)) (println "\n" message ", " (get context (bind :debug)) ", " context)))


(defn url-index [model node index] ;;end end-children
  (if (contains? (types model node) (bind :compiled))
    (let [context (build-context model {} node)

          children (doall
                    (filter (fn [child] (empty? (relate-right model (prop (bind :from)) child)))
                     (relate-right model (prop (bind :child)) node)))

          path (get context (bind :path))

          onlookers (if path (doall (relate-left model (prop (bind :from)) path)) [])

          observer (get context (bind :from))]
      (do
        (.add model
              (.createStatement model node (prop (bind :index)) (plit (str index))))
        (doseq [child children]
          (url-index model child index))
        (doseq [onlooker onlookers] ;;end end-children
          (url-index model onlooker (+ index 1)))))))

(defn generics [model node parent-context parent-children local-part] ;;end end-children
  (let [local-context (build-context model parent-context node)

        local-context (if parent-children (merge-context local-part local-context) local-context)

        context (merge parent-context local-context)

        thing (println "\n\n1: " parent-context "\n\n2: " local-context "\n\n3: " context)

        ;; children (doall (relate-right model (prop (bind :child)) node))
        children
        (map #(generics model % parent-context nil nil)
             (rdf-seq model (get parent-context (bind :children)))) ;;TODO holy shit I'm here

        children (if parent-children (concat parent-children children) children)

        base (res (first (get-property model node (prop (bind :base)))))

        types (conj (types model node) (bind :compiled))

        clone (res (uuid))

        subbed (get parent-context (literalize node))
        ;;fuck (debug (str "Hmm: " child-map "\n\n" new-end "\n\n") local-context)
        ]
    (cond
     subbed
     (generics model subbed context children local-context) ;;end end-children
     (contains? types (bind :view))
     (generics model base context nil local-context) ;;(conj end new-end) (conj end-children children)
     :default
     (do

       (doseq [type types]
           (.add model
                 (.createStatement model clone (prop "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
                                   (res type))))
       (copy-context model local-context clone)

         clone))))


(defn s-rec [binding model]

  (let [context (build-context model {} binding)

        params (named-params model context)
        children (sort-by (fn [resource]
                            (let [p (.getProperty model resource (prop (bind :rank)))]
                              (if p
                                (.getString (.asLiteral (.getObject p)))
                                "")))
                          (relate-right model (prop (bind :child)) binding))
        bindings (clojure.string/join ", "
                                      (map #(.getURI %)
                                           (get-subs model binding)))

        from (get context (bind :from))

        root (get context (bind :root))

        index (get context (bind :index))

        index (read-string (first (re-seq #"\d+" (.getString index))))

        types (types model binding)

        sub-calls (map (fn [child] (s-rec child model)) children) ;; make the next part of the tree

        path (get context (bind :path));;(first (relate-right model (prop (bind :path)) binding))
        path (if path (.getString path))
        ]

    (cond
     (contains? types (bind :container))
     (let [func (template (.getString
                           (reduce (fn [accum val]
                                     (or accum
                                         (first (relate-right model
                                                              (prop (bind :js))
                                                              (res val)))))
                                   nil types)))]
       (fn [data child resource url built-url]

         (let [url-parts (parse-uri url)
               my-part (get url-parts (- index 1))
               fuck (println "Mine: " my-part " at index " index " of " url)
               my-res (res my-part)

               my-res (if my-res
                           (if (.containsResource data my-res)
                             my-res))

               built-url (if my-res (str (s/join "/" (subvec url-parts 0 index)) "/") built-url)

               built-url (or (if path (str built-url (.getURI resource) "/")) built-url)]

           (cond
            root
            (func
             (.getURI root)
             bindings ;;(.getURI binding) TODO make it so we actually support multiple bindings in the js
             built-url
             params
             (map (fn [sub-call] (sub-call data nil root url built-url)) sub-calls))
            from
            (if my-res
              (func
               (.getURI my-res)
               bindings ;;(.getURI binding)
               built-url
               params
               (map (fn [sub-call] (sub-call data nil my-res url built-url)) sub-calls))
              "")
            :default

            (func
             (.getURI resource)
             bindings ;;(.getURI binding)
             built-url
             params
             (map (fn [sub-call] (sub-call data nil resource url built-url)) sub-calls))))))

     (contains? types (bind :binding))
     (let [statement (get-bound binding model) ;; fuckery for binding

           relation-getter (fn [data resource]
                             (filter #(not (and (.isResource %) (not (.getURI %))))
                                     (relator statement data resource)))]

       (fn [data child resource url built-url]

         (let [url-parts (parse-uri url)
               my-part (get url-parts (- index 1))
               my-res (res my-part)

               my-res (if my-res
                        (if (.containsResource data my-res)
                          my-res))

               built-url (if my-res (str (s/join "/" (subvec url-parts 0 index)) "/") built-url)

               built-url (or (if path (str built-url (.getURI resource) "/")) built-url)




               rel (if child   ;; get sub-resources (a single one if partial rendering)
                         (seq [child])
                         (relation-getter data resource))

               related (if (and from my-res)
                         (seq (if (contains? (set rel) my-res) [my-res] []))
                         rel)


               vals (if (empty? sub-calls)
                   (map literalize related)
                   (map (fn [sub-call]
                          (map
                           (fn [val]
                             (sub-call data nil val url built-url))
                           related))
                        sub-calls))]
           vals)))
     :default
     (println "\nFound a node with unknown type:\n\n\n" context "\n\nTypes: " types))))

(defn prepare [view widgets]
  (let [model (. ModelFactory createRDFSModel (.union view widgets))
        root (generics model (res (bind :application)) {} nil nil {})]
    (url-index model root 0)
    {:root root :model model}))

(defn serializer ;;TODO make this build sub-serializers that work for sub-projections
  ([expanded-view]
     (let [app-root (:root expanded-view)]
      (fn [data resource uri]
        ((s-rec app-root (:model expanded-view)) data nil resource uri "/view/"))))
  ([expanded-view binding]
     (let [app-root (:root expanded-view)]
       (fn [data child resource uri] (hic/html ((s-rec binding (:model expanded-view)) data child resource uri "/view/"))))
     ))

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
                  (relate-right model (prop (bind :projection)) binding)) ;;TODO fucked now

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



(defn qbuild [view uri]
  (let [root (:root view)
        inf (:model view)
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


(defn build-query [view uri]
  (let [
        ;;query  (qbuild inf (parse-uri uri))
        ;;qm (query-map query)
        s (str "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX ont: <http://logangilmour.com/example-ontology#>

CONSTRUCT {"
             "?s ?p ?o";;(render-graph query)
             "} WHERE {"
             "?s ?p ?o";;(render-query query false)
             "}"
             )]

    ;;(.add qm "root" (res "http://xmlns.com/foaf/0.1/Person"))  ;; TODO DIRTY DIRTY HACKS

    (let [;;done (.toString (new ParameterizedSparqlString s qm))
          ;;dumb (println "Query: " done)
          rval (.execConstruct
       (. QueryExecutionFactory sparqlService
          "http://localhost:8000/sparql/"
          s ;;done
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
  (filter (fn [root] (contains? (types inf (.getSubject root)) (bind :compiled)))
   (concat (iterator-seq (.listStatements inf nil (prop (bind :object)) predicate))
                  (iterator-seq (.listStatements inf nil (prop (bind :subject)) predicate)))))

(defn syncer [expanded-view]
  (fn [command url]

    (let [predicate (res (:predicate command))
          binders (get-all-bindings (:model expanded-view) predicate)
          bindings (map #(.getSubject %) binders)
          views (map (partial serializer expanded-view) bindings)]

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
                     ((serializer expanded-view binding)
                      (build-query expanded-view url)
                      (res (:uri command))
                      (if (= (:type command) "update")
                        (plit (:value command))
                        (res (:value command)))
                      url)
                     :binding
                     (.toString binding))
                   (assoc command
                     :value
                     ((serializer expanded-view binding)
                      (build-query expanded-view url)
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
    (println "message: " message)
    (let [uri (:uri message)
          binding (res (:binding message))
          data (:data message)
          type (:type message)
          statement (get-bound binding (:model inf))
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
@prefix : <http://logangilmour.com/data-binder#> .
@prefix ex: <http://logangilmour.com/example-view#> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix dc: <http://purl.org/dc/elements/1.1/> .
@prefix w: <http://logangilmour.com/bootstrap-widgets#> .
@prefix pl: <http://logangilmour.com/bootstrap-widgets/plain-list#> .
@prefix tm: <http://logangilmour.com/bootstrap-widgets/type-manager#> .


:application a ex:foaf-manager .

ex:foaf-manager rdfs:subClassOf :view ;
:base
 [a pl:list;
    :root foaf:Person;
    dc:title \"Butt\";
    :debug \"the list\";
    pl:binding [:object rdf:type];
    :child [:subject foaf:givenName]].


 #[a tm:type-manager ;
 # tm:type foaf:Person ;
 # tm:title \"People\" ;
 # tm:item ex:fullName ;
 # tm:path \"person\" ;
 # :child
 #  [dc:title \"Remove\" ;                     :rank 0 ;
 #    a w:deleter ;
 #    :child [:subject rdf:type]],
#
#   [dc:title \"Given Name\" ;                 :rank 1 ;
#     a w:text-field ;
#     :child [:subject foaf:givenName]] ,
#
#    [dc:title \"Family Name\" ;               :rank 2 ;
#     a w:text-field ;
#     :child [:subject foaf:familyName]]]



"))

(def example-view-large (to-model
                   "
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>.
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>.
@prefix : <http://logangilmour.com/data-binder#>.
@prefix ex: <http://logangilmour.com/example-view#>.
@prefix foaf: <http://xmlns.com/foaf/0.1/>.
@prefix dc: <http://purl.org/dc/elements/1.1/>.
@prefix w: <http://logangilmour.com/bootstrap-widgets#>.
@prefix cb: <http://logangilmour.com/bootstrap-widgets/compare-box#>.
@prefix pl: <http://logangilmour.com/bootstrap-widgets/plain-list#>.
@prefix tm: <http://logangilmour.com/bootstrap-widgets/type-manager#>.

:application a ex:foaf-manager.

ex:foaf-manager rdfs:subClassOf :view ;
:base
 [a tm:type-manager;
  tm:type foaf:Person;
  tm:title \"People\";
  tm:item ex:fullName;
  tm:path \"person\";
  :child
   [dc:title \"Remove\";                     :rank 0;
     a w:deleter;
     :child [:subject rdf:type]],

   [dc:title \"Given Name\";                 :rank 1;
     a w:text-field;
     :child [:subject foaf:givenName]],

    [dc:title \"Family Name\";               :rank 2;
     a w:text-field;
     :child [:subject foaf:familyName]],

    [dc:title \"Birthday\";                  :rank 3;
     a w:datepicker;
     :child [:subject foaf:birthday]],

    [dc:title \"Status\";                    :rank 4;
     a w:text-field;
     :child [:subject foaf:status]],

    [a pl:list;                              :rank 5;
     dc:title \"Knows\";
     pl:binding [:subject foaf:knows];
     :child ex:fullName],

    [a w:popup;                               :rank 6;
     dc:title \"Edit Known...\";
     :child
       [a pl:list;
        dc:title \"All\";
        :root foaf:Person;
        pl:binding [:object rdf:type];
        :child
          ex:fullName,
          [a cb:relator;
           cb:binding [:object foaf:knows];
           cb:path \"person\"]]]].

ex:fullName a w:join-text;
  w:join-with \" \";
  :child
    [a w:text;                               :rank 1;
     :child [:subject foaf:givenName]],
    [a w:text;                               :rank 2;
     :child [:subject foaf:familyName]].

"))

(def expanded-example (atom nil))
(def broadcast-channel (permanent-channel))
(def edit (atom nil))
(def syn (atom nil))
(def renderer (atom nil))


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


(def channels (atom {}))

(defn register [ch url]
  (siphon (map* @edit (map* decode-json ch)) broadcast-channel)
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
                                                                                 (@syn message url)
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
       (page (@renderer (build-query @expanded-example *) (res "http://logangilmour.com/example-ontology#person") *)))
  (route/resources "/")
  (route/not-found "Page not found"))


(defn -main
  [& args]
  (swap! expanded-example (fn [val] (prepare example-view widgets)))
  (swap! edit (fn [val] (editor @expanded-example)))
  (swap! syn (fn [val] (syncer @expanded-example)))
  (swap! renderer (fn [val] (serializer @expanded-example)))
  (start-http-server (wrap-ring-handler main-routes)
                     {:port 8080 :websocket true}))



;;(do (use 'databinder.core) (def stop! (-main)) (defn reload [] (stop!) (use 'databinder.core :reload) (def stop! (-main))))
