(ns databinder.sparql.query
  (:use
   databinder.rdf)
  (:require
   [databinder.model :as m]
   [clojure.zip :as z])
  (:import
   (com.hp.hpl.jena.query QuerySolutionMap ParameterizedSparqlString QueryExecutionFactory)))
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
                  (filter (fn [b] (empty? (m/relate-left model (m/prop (bind :projection)) b))) (m/relate-right model (m/prop (bind :child)) binding))
                  (m/relate-right model (m/prop (bind :projection)) binding)) ;;TODO fucked now

        projector (first (m/relate-left model (m/prop (bind :projection)) binding))
        ppath (if projector (first (m/relate-left model (m/prop (bind :path)) projector)))

        types (set (map #(.toString %) (m/relate-right model (m/prop "http://www.w3.org/1999/02/22-rdf-syntax-ns#type") binding)))

        path (first (m/relate-right model (m/prop (bind :path)) binding))
        onpath (if path (get uri (m/stringify path)))

        subs
        (if path
         (if onpath
           (map (fn [child] (qb child model uri)) children)
           '())
         (map (fn [child] (qb child model uri)) children))]

    (cond
     (contains? types (bind :template))
     subs
     :default
          (let [statement "ham";;(get-bound binding model)
                binder (m/stringify (.getPredicate statement))
                predicate (.asResource (.getObject statement)) ;;TODO handle type thing with path thing
                reverse (= binder (bind :object))]

            [{:pred (.getURI predicate) :rev reverse :path onpath} subs]))))

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
    (doall (map (fn [key] (.add qm (str key) (m/res (get vals key)))) (keys vals)))
    qm))

(defn build-query [view uri]
  (let [
        ;;query  (qbuild inf (u/parse-url uri))
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

    ;;(.add qm "root" (m/res "http://xmlns.com/foaf/0.1/Person"))  ;; TODO DIRTY DIRTY HACKS

    (let [;;done (.toString (new ParameterizedSparqlString s qm))
          ;;dumb (println "Query: " done)
          rval (.execConstruct
       (. QueryExecutionFactory sparqlService
          "http://localhost:8000/sparql/"
          s ;;done
          ))]

      rval

      )))
