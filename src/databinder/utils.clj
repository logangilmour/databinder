(ns databinder.utils)

(defn make-map [k-fn v-fn coll]
  (apply hash-map (apply concat (map (fn [val] [(k-fn val) (v-fn val)]) coll))))

(defn parse-url [uri]
  (vec (filter (comp not (partial re-matches #"^\s*$")) (clojure.string/split uri #"/"))))
