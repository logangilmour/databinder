(ns databinder.query
  (:import (com.hp.hpl.jena.query
            QueryExecution
            QueryExecutionFactory
            ResultSet
            ResultSetFormatter)
           (com.hp.hpl.jena.rdf.model ModelFactory)))

(def exquery "http://localhost:8080/sparql/" "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

CONSTRUCT {?s ?p ?o} WHERE {
?s ?p ?o
}")

(defn query [endpoint query]
  (map str (iterator-seq (.execConstructTriples (. QueryExecutionFactory sparqlService endpoint query)))))
