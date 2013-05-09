(ns databinder.sparql.update
  (:require
   [databinder.model :as m]
   [clj-http.client :as client])
  (:import
   (com.hp.hpl.jena.query QuerySolutionMap ParameterizedSparqlString QueryExecutionFactory)))

(defn simple-update [query s p o]
  (let [q-map (new QuerySolutionMap)]
    (.add q-map "s" s)
    (.add q-map "p" p)
    (.add q-map "o" o)
    (.toString (new ParameterizedSparqlString query q-map))
    ))

(defn build-update [command reversed]
  (cond (= (:type command) "update")
        (if reversed
          [(simple-update "
DELETE { ?old ?p ?o }
WHERE { ?old ?p ?o }"
                           (m/plit (:value command))
                           (m/prop (:predicate command))
                           (m/res (:uri command)))
           (simple-update "
INSERT DATA {?s ?p ?o }"
                         (m/plit (:value command))
                         (m/prop (:predicate command))
                         (m/res (:uri command)))]
          [(simple-update "
DELETE { ?s ?p ?old }
WHERE { ?s ?p ?old }"
                           (m/res (:uri command))
                           (m/prop (:predicate command))
                           (m/plit (:value command)))
           (simple-update "
INSERT DATA {?s ?p ?o }"
                           (m/res (:uri command))
                           (m/prop (:predicate command))
                           (m/plit (:value command)))])
        (or (= (:type command) "create") (= (:type command) "assoc"))
        (if reversed
          [(simple-update "INSERT DATA { ?s ?p ?o }"
                           (m/res (:value command))
                           (m/prop (:predicate command))
                           (m/res (:uri command)))]
          [(simple-update "INSERT DATA { ?s ?p ?o }"
                           (m/res (:uri command))
                           (m/prop (:predicate command))
                           (m/res (:value command)))])

        (= (:type command) "delete")
        (if reversed
          [(simple-update "DELETE DATA { ?s ?p ?o }"
                           (m/res (:value command))
                           (m/prop (:predicate command))
                           (m/res (:uri command)))]
          [(simple-update "DELETE DATA { ?s ?p ?o }"
                           (m/res (:uri command))
                           (m/prop (:predicate command))
                           (m/res (:value command)))])))

(defn update [url command reverse]
  (doall (map (fn [up]
           (client/post url
                        {:form-params {:update up}})) (build-update command reverse))))
