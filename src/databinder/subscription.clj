(ns databinder.subscription
  (:use
   databinder.rdf)
  (:require
   [databinder.model :as m] ))

(def test-schema
  (m/to-statements (m/to-model "
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix : <http://logangilmour.com/data-binder#> .
@prefix ex: <http://logangilmour.com/example-view#> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .



ex:test
  :name \"test\";
  :type :list;
  :property ex:prop;
  :keyfn \"(fn [x] (str x x))\";
  :predicate ex:stupid.

ex:prop
  :name \"prop1\";
  :predicate ex:rep;
  :type :literal.

")))

(def test-statements
  (m/to-statements (m/to-model "
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix : <http://logangilmour.com/data-binder#> .
@prefix ex: <http://logangilmour.com/example-view#> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

ex:one ex:stupid [ex:rep \"dumb\"].

")))

(def base
  {"namebinding" {:down {"predicatebinding" "predicate"
                         "typebinding" "type"
                         "namebinding" "name"
                         "propertybinding" "properties"
                         "parentbinding" "parent"
                         "reversebinding" "reverse"
                         "keyfnbinding" "keyfn"
                         }}
   "typebinding" {:down {"predicatebinding" (bind :literal)
                         "namebinding" (bind :literal)
                         "typebinding" (bind :literal)
                         "reversebinding" (bind :literal)
                         "propertybinding" (bind :list)
                         "parentbinding" (bind :list)
                         "keyfnbinding" (bind :function)}}
   "predicatebinding" {:down {"reversebinding" (bind :reverse)
                              "typebinding" (bind :type)
                              "frombinding" (bind :from)
                              "tobinding" (bind :to)
                              "namebinding" (bind :name)
                              "predicatebinding" (bind :predicate)
                              "propertybinding" (bind :property)
                              "parentbinding" (bind :property)
                              "keyfnbinding" (bind :keyfn)}
                       :up {(bind :reverse) #{"reversebinding"}
                            (bind :type) #{"typebinding"}
                            (bind :from) #{"frombinding"}
                            (bind :to) #{"tobinding"}
                            (bind :name) #{"namebinding"}
                            (bind :predicate) #{"predicatebinding"}
                            (bind :property) #{"propertybinding"}
                            (bind :keyfn) #{"keyfnbinding"}
                            }}
   "reversebinding" {:down {"parentbinding" "true"}
                     :up {"true" #{"parentbinding"}}}
   "propertybinding" {:down {"propertybinding" (sorted-map "reversebinding" "reversebinding"
                                                           "namebinding" "namebinding"
                                                           "typebinding" "typebinding"
                                                           "predicatebinding" "predicatebinding"
                                                           "propertybinding" "propertybinding"
                                                           "keyfnbinding" "keyfnbinding"
                                                           "parentbinding" "parentbinding"
                                                           )}
                      :up {"namebinding" #{"propertybinding"}
                           "typebinding" #{"propertybinding"}
                           "reversebinding" #{"propertybinding"}
                           "predicatebinding" #{"propertybinding"}
                           "keyfnbinding" #{"propertybinding"}
                           "parentbinding" #{"propertybinding"}
                           "propertybinding" #{"propertybinding"}
                           }}})




(def bootstrap
  ((fn f []
    {:predicate (bind :property)
     :$id "propertybinding"
     :type (bind :list)
     :name "properties"
     :properties (lazy-seq (list
                            {:$id "predicatebinding" :predicate (bind :predicate) :type (bind :literal) :name "predicate" :$parents (lazy-seq (list (f)))}
                            {:$id "reversebinding" :predicate (bind :reverse) :type (bind :literal) :name "reverse" :$parents (lazy-seq (list (f)))}
                            {:$id "typebinding" :predicate (bind :type) :type (bind :literal) :name "type" :$parents (lazy-seq (list (f)))}
                            {:$id "frombinding" :predicate (bind :from) :type (bind :literal) :name "from" :$parents (lazy-seq (list (f)))}
                            {:$id "tobinding" :predicate (bind :to) :type (bind :literal) :name "to" :$parents (lazy-seq (list (f))) }
                            {:$id "namebinding" :predicate (bind :name) :type (bind :literal) :name "name" :$parents (lazy-seq (list (f)))}
                            {:$id "keyfnbinding" :predicate (bind :keyfn) :type (bind :function) :name "keyfn" :$parents (lazy-seq (list (f)))}
                            (f)))
     :$parents (lazy-seq (list (f)))})))

(declare construct render)
;; This will be the normalized edition - we are expecting a proper subject-predicate-object thing
;; with reversals taken care of. Lazy evaluation should ensure we don't make big things, so
;; calling render in here will be reasonable.


(defn insert [sub statement]
  (let [bindings (get-in sub ["predicatebinding" :up (:predicate statement)])]
    (reduce (fn [tree binding]
              (let [binding (construct sub binding bootstrap {})
                    subject (if (:reverse binding) (:object statement) (:subject statement))
                    object (if (:reverse binding) (:subject statement) (:object statement))]
                (-> tree
                    (update-in [(:$id binding) :down subject] #(if (= (:type binding) (bind :list))
                                                                 (assoc (or % (sorted-map)) nil object)
                                                                 object))
                    (update-in [(:$id binding) :up object] #(conj (or % #{}) subject))))) sub bindings)))

(defn batch [sub statements]
  (reduce (fn [tree statement]
            (insert tree statement))
          sub
          statements))

(defn ss [] (batch (batch base test-schema) test-statements))


(defn construct [sub node binding url-params]

  (cond
   (= (:type binding) (bind :literal))
   node
   (= (:type binding) (bind :function))
   (eval (read-string node))
   :default
   (assoc
       (into {} (map (fn [sub-binding]
                       [(keyword (:name sub-binding)) (render sub node sub-binding url-params)])
                     (:properties binding)))
     :$id node
     :$binding (:$id binding)
     :$parents (map (fn [parent] (construct sub parent (first (:$parents binding)) url-params)) (get-in sub [(:$id (first (:$parents binding))) :up node]))
     )))

(defn render [sub node binding url-params]
  (let
      [prop (get-in sub [(:$id binding) :down node])]
    (if prop
      (if (= (:type binding) (bind :list))
        (map (fn [sub-node] (construct sub sub-node binding url-params))
             (let [from (if (:from binding) (get url-params (:from binding)))
                   to (if (:to binding) (get url-params (:to binding)))]
               (cond
                (and from to) (map second (subseq prop >= from <= to))
                from (map second (subseq prop >= from))
                to (map second (subseq prop <= to))
                :default (vals prop))))
        (construct sub prop binding url-params)))))
