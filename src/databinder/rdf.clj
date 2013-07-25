(ns databinder.rdf)

(defn uris [base & resources]
  (apply assoc {} (flatten (map (fn [resource] [resource (str base (name resource))]) resources))))

(def dc (uris "http://purl.org/dc/elements/1.1/" :title :description))

(defn rdfs [k]  (str "http://www.w3.org/2000/01/rdf-schema#" (name k)))

(defn rdf [k]  (str "http://www.w3.org/1999/02/22-rdf-syntax-ns#" (name k)))

(defn foaf [k]  (str "http://xmlns.com/foaf/0.1/" (name k)))

(defn bind [k]
  (str "http://logangilmour.com/data-binder#" (name k)))
