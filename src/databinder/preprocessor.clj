(ns databinder.preprocessor
  (:use
   databinder.rdf)
  (:require
   [databinder.model :as m]
   [databinder.context :as c]
   ))

(defn url-index [model node index] ;;end end-children
  (if (contains? (m/types model node) (bind :compiled))
    (let [context (c/build-context model node)

          children (doall
                    (filter (fn [child] (empty? (m/relate-right model (m/prop (bind :from)) child)))
                     (get context (bind :children))))

          path (get context (bind :path))

          onlookers (if path (doall (m/relate-left model (m/prop (bind :from)) path)) [])

          observer (get context (bind :from))]
      (do
        (.add model
              (.createStatement model node (m/prop (bind :index)) (m/plit (str index))))
        (doseq [child children]
          (url-index model child index))
        (doseq [onlooker onlookers] ;;end end-children
          (url-index model onlooker (+ index 1)))))))

(defn generics [model node parent-context local-part] ;;end end-children
  (let [local-context (c/build-context model node)

        ;;thing (println "\n\n1: " parent-context "\n\n2: " local-context)

        ;; children (doall (m/relate-right model (m/prop (bind :child)) node))
        children (get local-context (bind :children))

        local-context
        (if (seq? children)
          (assoc local-context (bind :children) (doall (map #(generics model % parent-context nil) children)))
          local-context
          )

        local-context (c/resolve-context model parent-context local-context)

        local-context (if local-part (c/merge-context local-part local-context) local-context)

        types (conj (m/types model node) (bind :compiled))

        base (m/res (first (c/get-property model node (m/prop (bind :base)))))

        clone (m/res (m/uuid))

        subbed (get parent-context (m/stringify node))]


    (cond
     subbed
     (generics model subbed parent-context local-context) ;;end end-children
     (contains? types (bind :view))
     (generics model base (merge parent-context local-context) nil) ;;(conj end new-end) (conj end-children children)
     :default
     (do

       (doseq [type types]
         (.add model
               (.createStatement model clone (m/prop "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
                                 (m/res type))))
       (c/copy-context model local-context clone)

       clone))))

(defn preprocess [view widgets]
  (let [model (m/union view widgets)
        root (generics model (m/res (bind :application)) {} nil)]
    (url-index model root 0)
    {:root root :model model}))
