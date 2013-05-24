(ns databinder.synchronize
  (:use
   databinder.rdf
   [databinder.interpreter :only (interpreter)])
  (:require
   [databinder.model :as m]))

(defn commander [uri pred data type reverse]

  (let [command (cond
          (= type "update")
          {:uri uri :predicate pred :value data :type type}
          (= type "create")
          {:uri uri :predicate pred :value (if (= data "") (m/uuid) data) :type type}
          (= type "delete")
          {:uri uri :predicate pred :value data :type type})]
    (if reverse
                  (assoc command :uri (:value command) :value (:uri command))
                  command)))

(defn to-triple [command]
  {:s (:uri command) :p (:predicate command) :o (:value command)})

(defn edit [inf data message]

  (let [binding (m/res (:binding message))
        predicate (first (m/relate-right (:model inf) (m/prop (bind :bind)) binding))
        reverse (contains? (m/types (:model inf) binding) (bind :object-binding))
        command (commander (:uri message)
                           (m/stringify predicate)
                           (:data message)
                           (:type message)
                           reverse)
        triple (to-triple command)]

    (cond
     (= (:type command) "update")
     (if reverse
       (m/update-subject data triple)
       (m/update-object data triple))

     (= (:type command) "create")
     (m/add data triple)

     (= (:type command) "delete")
     (m/delete data triple))

    command))


(defn get-all-bindings [inf predicate]
  (filter (fn [binding] (contains? (m/types inf binding) (bind :compiled)))
   (m/relate-left inf (m/prop (bind :bind)) predicate)))

(defn synchronize [expanded-view data command url]

  (let [predicate (m/res (:predicate command))
        bindings (get-all-bindings (:model expanded-view) predicate)]

    (map (fn [binding]

           (let [reverse (contains? (m/types (:model expanded-view) binding) (bind :object-binding))]

             (dissoc
              (cond
               (or (= (:type command) "update") (= (:type command) "create"))

               (if reverse
                 (assoc command
                   :uri (:value command)
                   :value
                   (interpreter expanded-view binding
                    data
                    (m/res (:uri command))
                    (if (= (:type command) "update")
                      (m/plit (:value command))
                      (m/res (:value command)))
                    url)
                   :binding
                   (.toString binding))
                 (assoc command
                   :value
                   (interpreter expanded-view binding
                    data
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
