(ns databinder.utils
  (:use [clojure.string :only (split)] ))

(defn make-map [k-fn v-fn coll]
  (apply hash-map (apply concat (map (fn [val] [(k-fn val) (v-fn val)]) coll))))

(defn parse-url [url]
  (let [[uri query] (split url #"\?")]
    {:path uri
     :path-vec
     (vec (filter (comp not (partial re-matches #"^\s*$")) (split uri #"/")))
     :query-string query
     :query-params
     (make-map first second
               (map (fn [pair] (if pair (split pair #"="))) (if query (split query #"&"))))}))
