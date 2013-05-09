(ns databinder.model
  (:use
   databinder.rdf)
  (:require
   [clojure.java.io :as io])
  (:import
   (com.hp.hpl.jena.rdf.model ModelFactory ResourceFactory Model SimpleSelector)
   (java.io ByteArrayInputStream)))

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

(defn union [a b]
  (. ModelFactory createRDFSModel (.union a b)))

(defn present? [model resource]
  (.containsResource model resource))

(defn stringify [res]
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

(defn types [model binding]
  (if (and binding (.isResource binding))
    (set (map #(.toString %)
              (relate-right model (prop "http://www.w3.org/1999/02/22-rdf-syntax-ns#type") binding)))
    (set [])))

(defn blank? [node]
  (and (.isResource node) (not (.getURI node))))
