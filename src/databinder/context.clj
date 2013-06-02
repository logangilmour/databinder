(ns databinder.context
  (:use
   databinder.rdf)
  (:require
   [databinder.model :as m]
   [databinder.utils :as u])
  (:import
   [java.net URLEncoder URLDecoder]))

(defn get-property [model resource property]
  (filter identity (map #(if % (.getURI %))
         (flatten
          (map
           (fn [type]
             (m/relate-right model property type))
           (m/relate-right model (m/prop "http://www.w3.org/1999/02/22-rdf-syntax-ns#type") resource))))))

(defn named-params [model context]
  (into {} (keep #(if (first %) %)
                 (u/make-map #(m/stringify (first (m/relate-right model (m/prop (bind :name)) (m/res %))))
                           #(let [property (get context %)]
                              (if (seq? property)
                                (doall (map (fn [p] (m/stringify p)) property))
                                (m/stringify property)))
                           (keys context)))))

(defn resolve-property [model context resource]
  (if (contains? (set (keys context)) (m/stringify resource))
    (get context (m/stringify resource))
    resource))

(defn resolve-list [model property]
  (if (contains? (m/types model property) (rdfs :List))
                (doall (m/rdf->seq model property))
                property))

(defn build-context [model binding]
  (let [params (get-property model binding (m/prop (bind :param)))]
    (u/make-map identity
              #(resolve-list model (first (m/relate-right model (m/prop %) binding)))
              params)))

(defn resolve-context [model parent-context context]
  (u/make-map identity
            (fn [key]
              (let [property (get context key)]
                (if (seq? property)
                  (map (partial resolve-property model parent-context) property)
                  (resolve-property model parent-context property))))
            (keys context)))

(defn resolve-param [model property url]
  (let [query (:query-params (u/parse-url url))
        val (get query (m/stringify (first (m/relate-right model (m/prop (bind :key)) property))))]
    (if val (m/plit (URLDecoder/decode val)))))

(defn resolve-path [model property url]
  (let [path (:path-vec (u/parse-url url))
        index (m/stringify (first (m/relate-right model (m/prop (bind :index)) property)))
        val (get path
                 (try (Integer/parseInt index)
                      (catch NumberFormatException e 0)))]
    (if val (m/res (URLDecoder/decode val)))))

(defn resolve-url [model context url]
  (u/make-map identity
              (fn [key]
                (let [property (get context key)]
                  (if (seq? property)
                    property
                    (cond (contains? (m/types model property) (bind :query))
                          (resolve-param model property url)

                          (contains? (m/types model property) (bind :path))
                          (resolve-path model property url)

                          :default
                          property))))
              (keys context)))

(defn copy-context [model local-context node]
  (doseq [param (keys local-context)]
    (let [property (get local-context param)]
      (if property
        (.add model (.createStatement model node (m/prop param)
                                      (if (seq? property)
                                        (m/seq->rdf model property)
                                        property)))))))

(defn merge-context [parent-context context]
  (merge-with
   (fn [parent child]
     (if (and (seq? parent) (seq? child))
       (doall (concat parent child))
       child))
   parent-context
   (into {} (keep (fn [[key val :as pair]] (if (not (get parent-context key)) pair))
                  context))))

(defn debug [message context]
  (if (get context (bind :debug)) (println "\n" message ", " (get context (bind :debug)) ", " context)))
