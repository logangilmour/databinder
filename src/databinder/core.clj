(ns databinder.core
  (:use
   compojure.core
   lamina.core
   aleph.http
   aleph.formats
   databinder.rdf
   [databinder.interpreter :only (interpreter)]
   [databinder.preprocessor :only (preprocess)]
   [databinder.synchronize :only (synchronize edit)]
   [databinder.sparql.query :only (build-query )])
  (:require
   [compojure.handler :as handler]
   [compojure.route :as route]
   [hiccup.core :as hic]
   [databinder.query :as q]
   [databinder.model :as m]))


(def widgets (m/to-model

             "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix : <http://logangilmour.com/data-binder#> .
@prefix w: <http://logangilmour.com/bootstrap-widgets#> .
@prefix dc: <http://purl.org/dc/elements/1.1/> .
@prefix cb: <http://logangilmour.com/bootstrap-widgets/compare-box#> .
@prefix pl: <http://logangilmour.com/bootstrap-widgets/plain-list#> .
@prefix tm: <http://logangilmour.com/bootstrap-widgets/type-manager#> .


:component :param :debug .
:debug :name \"debug\" .

:component :param :root .
:component :param :path .
:component :param :from .
:component :param :index .
:component :param :children .

:index :name \"index\" .


:bind rdfs:domain :binding .
:object rdfs:domain :object-binding .
:subject rdfs:domain :subject-binding .

:object rdfs:subPropertyOf :bind .
:subject rdfs:subPropertyOf :bind .

:object-binding rdfs:subClassOf :binding .
:subject-binding rdfs:subClassOf :binding .

:children rdfs:domain :component .
:children rdfs:range rdfs:List .

:binding :param :bind .

:binding rdfs:subClassOf :component .

:template rdfs:subClassOf :component .

:view rdfs:subClassOf :component .



:component :param w:label .
w:label :name \"label\".


:template :param w:id .
w:id :name \"id\".


w:projector
  rdfs:subClassOf :template ;
  :js \"emit(projector(this));\" .


w:column8
  rdfs:subClassOf :template ;
  :js \"emit(column8(this));\" .

w:column4
  rdfs:subClassOf :template ;
  :js \"emit(column4(this));\" .

w:row
  rdfs:subClassOf :template ;
  :js \"emit(row(this));\" .

w:list
  rdfs:subClassOf :template ;
  :js \"emit(list(this));\" .

pl:list rdfs:subClassOf :view ;
  :param pl:binding, w:label, :root, :children;
  :base [a w:list ;
         :root :root ;
         w:label w:label;
         :children (pl:binding)] .

pl:binding :children ([a w:li;
                       :children :children]).

w:li rdfs:subClassOf :template ;
  :js \"emit(li(this));\" .


w:list-item-
  rdfs:subClassOf :template ;
  :js \"emit(listItem(this));\" .

w:list-item rdfs:subClassOf :view ;
  :param :path, :children ;
  :base [a w:list-item- ;
         :path :path;
         :children (
                 [a w:active ;
                  :from :path]
                 [a w:projector;
                  :children :children])] .

w:link-list rdfs:subClassOf :view ;
  :param :path, w:list-binding, w:label, :children;
  :base
    [a w:list ;
     w:label w:label ;
     :children (w:list-binding)].

w:list-binding :children (
  [a w:list-item;
   :path :path;
   :children :children]) .


tm:type-manager rdfs:subClassOf :view ;
:param tm:type, w:label, tm:item , :path, :children;
:base
  [a w:row ;
  :root tm:type ;
  :children (
    [a w:column4 ;
     :children (
      [a w:link-list ;
       w:label w:label ;
       w:list-binding [:object rdf:type] ;
       :children (tm:item) ;
       :path :path]
      [a w:creator ;
       :children ([:object rdf:type])])]
    [a w:column4;
     :from :path;
     :children :children])].


cb:relator rdfs:subClassOf :view ;
  :param cb:binding , cb:path ;
  :base [a w:checkbox ;
         :children (cb:binding
                [a w:value ; :from cb:path])] .

cb:binding :from cb:path ;
  :children ([a w:value]) .

w:deleter
  rdfs:subClassOf :template ;
  :js \"emit(deleter(this));\" .

w:text
  rdfs:subClassOf :template ;
  :js \"emit(span(this));\" ;
  :param w:before , w:after.

w:join-text rdfs:subClassOf :template ;
  :js \"emit(join(this));\" ;
  :param w:join-with .

w:join-with :name \"joinWith\" .

w:after :name \"after\".
w:before :name \"before\".

w:plain
  rdfs:subClassOf :template ;
  :js \"emit(this.vals);\" .

w:checkbox
  rdfs:subClassOf :template ;
  :js \"emit(checkbox(this));\" .

w:check
  rdfs:subClassOf :template ;
  :js \"emit(check(this));\" .

w:value
  rdfs:subClassOf :template ;
  :js \"emit(value(this));\" .

w:creator
  rdfs:subClassOf :template ;
  :js \"emit(creator(this));\" .

w:paragraph
  rdfs:subClassOf :template ;
  :js \"emit(paragraph(this));\" .

w:text-field
  rdfs:subClassOf :template ;
  :js \"emit(textField(this));\" .

w:popup
  rdfs:subClassOf :template ;
  :js \"emit(popup(this));\".

w:string
  rdfs:subClassOf :template ;
  :js \"emit(string(this));\".

w:datepicker
  rdfs:subClassOf :template ;
  :js \"emit(datepicker(this));\".

w:active-test
  rdfs:subClassOf :template ;
  :js \"if(this.children[0]==this.uri){emit('active')}else{emit('')};\" .

w:active rdfs:subClassOf :view ;
  :param :from ;
  :base [a w:active-test ;
         :children ([a w:value ; :from :from]) ] .
"

             ))

(def example-view-small (m/to-model
                   "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix : <http://logangilmour.com/data-binder#> .
@prefix ex: <http://logangilmour.com/example-view#> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix dc: <http://purl.org/dc/elements/1.1/> .
@prefix w: <http://logangilmour.com/bootstrap-widgets#> .
@prefix pl: <http://logangilmour.com/bootstrap-widgets/plain-list#> .
@prefix tm: <http://logangilmour.com/bootstrap-widgets/type-manager#> .


:application a ex:foaf-manager .

ex:foaf-manager rdfs:subClassOf :view ;
:base
 [a pl:list;
    :root foaf:Person;
    w:label \"Butt\";
    pl:binding [:object rdf:type];
    :children ([:subject foaf:givenName])].
"))


(def example-view (m/to-model
                   "
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>.
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>.
@prefix : <http://logangilmour.com/data-binder#>.
@prefix ex: <http://logangilmour.com/example-view#>.
@prefix foaf: <http://xmlns.com/foaf/0.1/>.
@prefix dc: <http://purl.org/dc/elements/1.1/>.
@prefix w: <http://logangilmour.com/bootstrap-widgets#>.
@prefix cb: <http://logangilmour.com/bootstrap-widgets/compare-box#>.
@prefix pl: <http://logangilmour.com/bootstrap-widgets/plain-list#>.
@prefix tm: <http://logangilmour.com/bootstrap-widgets/type-manager#>.

:application a ex:foaf-manager.

ex:foaf-manager rdfs:subClassOf :view ;
:base
 [a tm:type-manager;
  tm:type foaf:Person;
  w:label \"People\";
  tm:item ex:fullName;
  :path \"person\";
  :children (
   [w:label \"Remove\";
     a w:deleter;
     :children ([:subject rdf:type])]

   [w:label \"Given Name\";
     a w:text-field;
     :children ([:subject foaf:givenName])]

    [w:label \"Family Name\";
     a w:text-field;
     :children ([:subject foaf:familyName])]

    [w:label \"Birthday\";
     a w:datepicker;
     :children ([:subject foaf:birthday])]

    [w:label \"Status\";
     a w:text-field;
     :children ([:subject foaf:status])]

    [a pl:list;
     w:label \"Knows\";
     pl:binding [:subject foaf:knows];
     :children (ex:fullName)]

    [a w:popup;
     w:label \"Edit Known...\";
     :children
       ([a pl:list;
        w:label \"All\";
        :root foaf:Person;
        pl:binding [:object rdf:type];
        :children (
          ex:fullName
          [a cb:relator;
           cb:binding [:object foaf:knows];
           cb:path \"person\"] ) ] ) ] ) ].

ex:fullName a w:join-text;
  w:join-with \" \";
  :children (
    [a w:text;
     :children ([:subject foaf:givenName])]
    [a w:text;
     :children ([:subject foaf:familyName])]).

"))


(def expanded-example (atom nil))
(def broadcast-channel (permanent-channel))
(def renderer (atom nil))


(defn page [body]
  (hic/html
   [:html
    [:head
     [:meta {:name "viewport" :content "width=device-width, initial-scale=1.0"}]

     [:link {:href "/css/bootstrap.min.css" :rel "stylesheet" :media "screen"}]
     [:link {:href "/css/datepicker.css" :rel "stylesheet" :media "screen"}]
     [:title "FIXME"]]
    [:body body
     [:script {:type "text/javascript" :src "/js/jquery.min.js"}]
     [:script {:type "text/javascript" :src "/js/bootstrap.min.js"}]
     [:script {:type "text/javascript" :src "/js/bootstrap-datepicker.js"}]
     [:script {:type "text/javascript" :src "/js/underscore-min.js"}]
     [:script {:type "text/javascript" :src "/js/update.js"}]]

    ]
   ))

(defn startup []
  (do
    (print "starting up server!")
    ;;(logger/init)
    ))

;;(def data-model (m/to-model data))


(def channels (atom {}))

(defn register [ch url]
  (siphon (map* (partial edit @expanded-example) (map* decode-json ch)) broadcast-channel)
  (swap! channels (fn [old]

                    (let [all (reduce (fn [accum key]
                                        (if (closed? (get accum key))
                                          (do
                                            (println "Getting rid of old connection for " key)
                                            (dissoc accum key))
                                          accum)) old (keys old))
                          url-ch (or (get all url)
                                     (let [new-ch (channel)]
                                       (println "creating a channel for url " url)
                                       (siphon (map*
                                                encode-json->string
                                                (map*
                                                 (fn [message]
                                                   (synchronize @expanded-example message url))
                                                 broadcast-channel))
                                               new-ch)
                                       new-ch))]
                      (siphon url-ch ch) ;; listen on your url channel
                      (doall (map (partial println "\nConnection: ") (keys all)))
                      (assoc all url url-ch)))))

(defn chat-handler [ch handshake]
  (receive ch
           (fn [input]
             (let [url (:* (:params handshake))]
               (println "registering a connection to url " url)
               ;;(let jc [(map* decode-json ch)])
               (register ch url))
             )))


(defroutes main-routes
  (GET "/async/*" [] (wrap-aleph-handler chat-handler))
  (GET "/view/*" [*]
       (page (@renderer (build-query @expanded-example *) (m/res "http://logangilmour.com/example-ontology#person") *))) ;;TODO WTF!!!! GET THIS OUT OF HERE!!!
  (route/resources "/")
  (route/not-found "Page not found"))


(defn -main
  [& args]
  (swap! expanded-example (fn [val] (preprocess example-view widgets)))
  (swap! renderer (fn [val] (interpreter @expanded-example)))
  (start-http-server (wrap-ring-handler main-routes)
                     {:port 8080 :websocket true}))



;;(do (use 'databinder.core :reload-all) (def stop! (-main)) (defn reload [] (stop!) (use 'databinder.core :reload-all) (def stop! (-main))))
