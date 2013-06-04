(ns databinder.preprocessor
  (:use
   databinder.rdf)
  (:require
   [databinder.model :as m]
   [databinder.context :as c]
   [databinder.utils :as u]
   ))

(defn expand [model expander resource]
  (if (seq? resource)
    (doall (map (partial expand model expander) resource))
    (if resource
      (let [types (m/types model resource)]
        (if (contains? types (bind :component))
          (expander resource)
          resource)))))

(defn generics [model node parent-context local-part] ;;end end-children
  (let [local-context (c/build-context model node)

        local-context
        (u/make-map first
                    (comp (partial expand model #(generics model % parent-context nil)) second)
                    local-context)

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
  (let [model (m/rdfs-model (m/union view widgets))
        root (generics model (m/res (bind :application)) {} nil)]
    {:root root :model model}))
