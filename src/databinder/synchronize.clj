(ns databinder.synchronize
  (:use
   databinder.rdf
   [databinder.sparql.update :only (update)]
   [databinder.interpreter :only (interpreter)]
   [databinder.sparql.query :only (build-query)])
  (:require
   [databinder.model :as m]))

(defn commander [uri pred data type]

  (cond
   (= type "update")
   {:uri uri :predicate pred :value data :type type}
   (= type "create")
   {:uri uri :predicate pred :value (if (= data "") (m/uuid) data) :type type}
   (= type "delete")
   {:uri uri :predicate pred :value data :type type}))

(defn edit [inf message]

  (let [binding (m/res (:binding message))
        predicate (first (m/relate-right (:model inf) (m/prop (bind :bind)) binding))
        reverse (contains? (m/types (:model inf) binding) (bind :object-binding))
        command (commander (:uri message)
                           (m/stringify predicate)
                           (:data message)
                           (:type message))]

    (update "http://localhost:8000/update/" command reverse)

    (if reverse
      (assoc command :uri (:value command) :value (:uri command))
      command)))


(defn get-all-bindings [inf predicate]
  (filter (fn [binding] (contains? (m/types inf binding) (bind :compiled)))
   (m/relate-left inf (m/prop (bind :bind)) predicate)))

(defn synchronize [expanded-view command url]

  (let [predicate (m/res (:predicate command))
        bindings (get-all-bindings (:model expanded-view) predicate)
        views (map (partial interpreter expanded-view) bindings)]

    (map (fn [binding]

           (let [reverse (contains? (m/types (:model expanded-view) binding) (bind :object-binding))]

             (dissoc
              (cond
               (or (= (:type command) "update") (= (:type command) "create"))

               (if reverse
                 (assoc command
                   :uri (:value command)
                   :value
                   ((interpreter expanded-view binding)
                    (build-query expanded-view url)
                    (m/res (:uri command))
                    (if (= (:type command) "update")
                      (m/plit (:value command))
                      (m/res (:value command)))
                    url)
                   :binding
                   (.toString binding))
                 (assoc command
                   :value
                   ((interpreter expanded-view binding)
                    (build-query expanded-view url)
                    (if (= (:type command) "update")
                      (m/plit (:value command))
                      (m/res (:value command)))
                    (m/res (:uri command))
                    url)
                   :binding
                   (.toString binding)))

               (= (:type command) "delete")
               (assoc
                   (if reverse
                     (assoc command :uri (:value command) :value (:uri command))
                     command)
                 :binding (.toString binding))) :predicate)))
         bindings)))
