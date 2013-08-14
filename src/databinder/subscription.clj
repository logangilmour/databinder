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


ex:base
  :name \"base\";
  :type :list;
  :property ex:test.

ex:test
  :name \"test\";
  :type :list;
  :property ex:prop;
  :from \"f\";
  :to \"t\";
  :keyfn \"(fn [x] (str (:prop1 x)))\";
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

ex:other ex:rep \"wat\".
ex:thing ex:rep \"dumb\".

")))

(def earlytest
  (m/to-statements (m/to-model "
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix : <http://logangilmour.com/data-binder#> .
@prefix ex: <http://logangilmour.com/example-view#> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

ex:one ex:stupid ex:thing.
ex:one ex:stupid ex:other.



")))

(def base
  {"namebinding" {:down {"predicatebinding" "predicate"
                         "typebinding" "type"
                         "namebinding" "name"
                         "propertybinding" "properties"
                         "reversebinding" "reverse"
                         "keyfnbinding" "keyfn"
                         "frombinding" "from"
                         "tobinding" "to"
                         }}
   "typebinding" {:down {"predicatebinding" (bind :literal)
                         "namebinding" (bind :literal)
                         "typebinding" (bind :literal)
                         "reversebinding" (bind :literal)
                         "propertybinding" (bind :list)
                         "keyfnbinding" (bind :function)
                         "frombinding" (bind :literal)
                         "tobinding" (bind :literal)}}
   "predicatebinding" {:down {"reversebinding" (bind :reverse)
                              "typebinding" (bind :type)
                              "frombinding" (bind :from)
                              "tobinding" (bind :to)
                              "namebinding" (bind :name)
                              "predicatebinding" (bind :predicate)
                              "propertybinding" (bind :property)
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
   "propertybinding" {:down {"propertybinding" (sorted-map "reversebinding" #{"reversebinding"}
                                                           "namebinding" #{"namebinding"}
                                                           "typebinding" #{"typebinding"}
                                                           "predicatebinding" #{"predicatebinding"}
                                                           "propertybinding" #{"propertybinding"}
                                                           "keyfnbinding" #{"keyfnbinding"}
                                                           "frombinding" #{"frombinding"}
                                                           "tobinding" #{"tobinding"}
                                                           )}
                      :up {"namebinding" #{"propertybinding"}
                           "typebinding" #{"propertybinding"}
                           "reversebinding" #{"propertybinding"}
                           "predicatebinding" #{"propertybinding"}
                           "keyfnbinding" #{"propertybinding"}
                           "propertybinding" #{"propertybinding"}
                           "frombinding" #{"propertybinding"}
                           "tobinding" #{"propertybinding"}
                           }}})

(defn unparent [view]
  (cond (map? view)
        (into {} (map (fn [[k v]] [k (unparent v)]) (dissoc view :$parents)))
        (seq? view)
        (map unparent view)
        :default
        view))


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


(defn keychanges [sub view binding]
  (if (= (:type binding) (bind :list))
    (let [k ((or (:keyfn binding) (fn [val] (:$id val))) (unparent view))]

      (-> sub
          (update-in [(:$id binding) :down (:$id (first (:$parents view)))]
                     (fn [val] (let [keyed (update-in val [k] #(conj (or % #{}) (:$id view)))
                                    old (get-in sub [(:$id binding) :ks (:$id view)])
                                    removed (update-in keyed [old]
                                                       #(disj % (:$id view)))]
                                (if (empty? (get removed old))
                                  (dissoc removed old)
                                  removed))))
          (assoc-in [(:$id binding) :ks (:$id view)] k)))
    (keychanges sub (first (:$parents view)) (first (:$parents binding)))))

(defn insert [sub statement]
  (let [bindings (get-in sub ["predicatebinding" :up (:predicate statement)])]
    (reduce (fn [tree binding]
              (let [binding (construct sub binding bootstrap {})
                    subject (if (:reverse binding) (:object statement) (:subject statement))
                    object (if (:reverse binding) (:subject statement) (:object statement))]
                (let [updated
                      (-> tree
                          (update-in [(:$id binding) :down subject]
                                     (fn [val] (if (= (:type binding) (bind :list))
                                           (update-in (or val (sorted-map)) [nil] #(conj
                                                                                  (or % #{})
                                                                                  object))
                                           object)))
                          (update-in [(:$id binding) :up object] #(conj (or % #{}) subject)))]
                  (if (= (:type binding) (bind :list))
                    (keychanges updated
                                (construct updated object binding {})
                                binding)
                    (keychanges updated
                                (let [out (construct updated subject (first (:$parents binding)) {})]
                                  (println "AHAHAHAHA  " out "\n\n ==== \n" binding "\n")
                                  out)
                                (first (:$parents binding))))))) sub bindings)))

(defn batch [sub statements]
  (reduce (fn [tree statement]
            (insert tree statement))
          sub
          statements))

(defn ss [] (batch (batch (batch base test-schema) earlytest) test-statements))


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
     :$parents (map (fn [parent] (construct sub parent (first (:$parents binding)) url-params)) (get-in sub [(:$id binding) :up node])) ;;TODO fix that we only allow one parent in the binding.
     )))

(defn render [sub node binding url-params]
  (let
      [prop (get-in sub [(:$id binding) :down node])]
    (if prop
      (if (= (:type binding) (bind :list))
        (map (fn [sub-node] (construct sub sub-node binding url-params))
             (let [from (if (:from binding) (get url-params (:from binding)))
                   to (if (:to binding) (get url-params (:to binding)))
                   flat (comp flatten (partial map (comp seq second)))]
               (cond
                (and from to) (flat (subseq prop >= from <= to))
                from (flat (subseq prop >= from))
                to (flat (subseq prop <= to))
                :default (flat prop))))
        (construct sub prop binding url-params)))))
