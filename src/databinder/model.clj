(ns databinder.model
  (:use
   databinder.rdf)
  (:require
   [clojure.java.io :as io]
   [clojure.stacktrace :as st])
  (:import
   (com.hp.hpl.jena.rdf.model ModelFactory ResourceFactory Model SimpleSelector)
   (java.io ByteArrayInputStream)
   (com.hp.hpl.jena.tdb TDBFactory)
   (com.hp.hpl.jena.query ReadWrite)))

(def ds nil)

(defn prop [uri]
  (if uri
    (. ResourceFactory createProperty (str uri))))

(defn res [uri]
  (if uri
    (. ResourceFactory createResource uri)))

(defn plit [val]
  (. ResourceFactory createPlainLiteral val))

(defn write-out [model]
  (with-open [out (io/output-stream "/Users/logangilmour/test.rdf")]
    (.write model out "TURTLE"))

  nil)

(defn rdfs-model [model]
  (. ModelFactory createRDFSModel model))

(defn present? [model resource]
  (.containsResource model resource))

(defn present-tx? [model resource]
  (try
    (.begin ds (. ReadWrite READ))
    (.containsResource model resource)
    (catch Exception e (.printStackTrace e))
    (finally (.end ds))))

(defn stringify [res]
  (if res
    (if (.isLiteral res)
      (.getString (.asLiteral res))
      (.toString res))))

(defn relate-right
  [model predicate resource]
  (doall (iterator-seq (.listObjectsOfProperty model resource predicate))))



(defn relate-left [model predicate resource]
  (doall (iterator-seq (.listResourcesWithProperty model predicate resource))))



(defn uuid [] (str "urn:uuid:" (java.util.UUID/randomUUID)))

(defn add
  ([model subject predicate object]
     (println "\n\nAHHH:" subject " " predicate " " object "\n\n")
     (.begin ds (. ReadWrite WRITE))
     (try
       (.add model
             (.createStatement model subject predicate object))
       (.commit ds)
       (catch Exception e (println "&&&&&&&&&&&\n\nBad create: " e)  )
       (finally (.end ds)))
      )
  ([model triple]
     (println "\n\nWOAH:" triple)
     (.begin ds (. ReadWrite WRITE))
     (try
       (.add model
             (.createStatement model (res (:s triple)) (prop (:p triple)) (res (:o triple))))
       (.commit ds)
       (catch Exception e (println "&&&&&&&&&&&\n\nBad create: " e) )
       (finally (.end ds)))
     ))

(defn delete
  ([model subject predicate object]
     (println "\n\nAHHH:" subject " " predicate " " object "\n\n")
     (.begin ds (. ReadWrite WRITE))
     (try
       (.remove model
                (.createStatement model subject predicate object))
       (.commit ds)
          (catch Exception e (println "&&&&&&&&&&&\n\nBad delete: " e) )
          (finally (.end ds))))
  ([model triple]
     (println "\n\nWOAH:" triple)
     (.begin ds (. ReadWrite WRITE))
     (try (.remove model
                   (.createStatement model (res (:s triple)) (prop (:p triple)) (res (:o triple))))
          (.commit ds)
          (catch Exception e
                 (println "&&&&&&&&&&&\n\nBad delete: " e) )
          (finally (.end ds)))))

(defn update-subject
  ([model subject predicate object]

     (let [old (first (relate-left model predicate object))]
        (if old (delete model old predicate object))
        (add model subject predicate object))
     )
  ([model triple]

     (let [subject (plit (:s triple))
           predicate (prop (:p triple))
           object (res (:o triple))
           old (first (relate-left model predicate object))]
       (if old (delete model old predicate object))
       (add model subject predicate object))
     ))

(defn update-object
  ([model subject predicate object]

     (let [old (first (relate-right model predicate subject))]
        (if old (delete model subject predicate old))
        (add model subject predicate object))
     )
  ([model triple]

     (let [subject (res (:s triple))
           predicate (prop (:p triple))
           object (plit (:o triple))
           old (first (relate-right model predicate subject))]
       (if old (delete model subject predicate old))
       (add model subject predicate object))
     ))

(defn rdf->seq [mod top]
  (seq (loop [model mod
              head top
              list []]
         (if (or (not head) (= head (res (rdfs :nil))))
           list
           (recur model
                  (first (relate-right model (prop (rdfs :rest)) head))
                  (conj list
                        (first (relate-right model (prop (rdfs :first)) head))))))))

(defn seq->rdf [model coll]
  (if (empty? coll)
    (res (rdfs :nil))
    (let [current (res (uuid))]
      (.add model (.createStatement model current (prop (rdfs :first)) (first coll)))
      (.add model (.createStatement model current (prop (rdfs :rest)) (seq->rdf model (rest coll))))
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

(defn to-model [string-data]
  (let [model (. ModelFactory createDefaultModel)]
    (.read model (new ByteArrayInputStream (.getBytes string-data "UTF-8")) "http://localhost:3000" "TURTLE")
    (skolemize-model model)
    model))

(defn default-model []
  (if ds
    (.close ds))
  (. TDBFactory reset)
  (let [dataset (. TDBFactory createDataset "defaultdatabase")]
    (def ds dataset)
    (.getDefaultModel ds)))

(defn types [model binding]
  (if (and binding (.isResource binding))
    (set (map #(.toString %)
              (relate-right model (prop "http://www.w3.org/1999/02/22-rdf-syntax-ns#type") binding)))
    (set [])))

(defn union [model-a model-b]
  (. ModelFactory createUnion model-a model-b))

(defn blank? [node]
  (and (.isResource node) (not (.getURI node))))
