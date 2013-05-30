(ns databinder.rdf)

(defn uris [base & resources]
  (apply assoc {} (flatten (map (fn [resource] [resource (str base (name resource))]) resources))))

(def rdfs (uris "http://www.w3.org/1999/02/22-rdf-syntax-ns#" :first :rest :nil :List))

(def dc (uris "http://purl.org/dc/elements/1.1/" :title :description))

(def bind (uris "http://logangilmour.com/data-binder#" :js :debug :index :root :application :subject :object :children :template :from :filter :query :key :path :view :base :param :name :binding :bind :order-by :object-binding :subject-binding :compiled))
