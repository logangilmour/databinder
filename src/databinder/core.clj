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
   [databinder.template :only (template-env)])
  (:require
   [compojure.route :as route]
   [hiccup.core :as hic]
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
@prefix n: <http://logangilmour.com/bootstrap-widgets/nav-bar#> .
@prefix f: <http://logangilmour.com/filter#> .
@prefix u: <http://logangilmour.com/uri#> .
@prefix m: <http://logangilmour.com/map#> .
@prefix geo: <http://www.w3.org/2003/01/geo/wgs84_pos#> .
@prefix g: <http://logangilmour.com/graph#> .


:root :name \"root\" .

:component :param :debug .
:debug :name \"debug\" .

:component :param :root, :children, :types .
:template :param :uri, :filter .

:bind rdfs:domain :binding .
:object rdfs:domain :object-binding .
:subject rdfs:domain :subject-binding .

:object rdfs:subPropertyOf :bind .
:subject rdfs:subPropertyOf :bind .

:object-binding rdfs:subClassOf :binding .
:subject-binding rdfs:subClassOf :binding .

:children rdfs:domain :component .
:children rdfs:range rdfs:List .

:children :name \"children\" .

:binding :param :bind, :order-by .

:binding rdfs:subClassOf :component .

:template rdfs:subClassOf :component .

:reader rdfs:subClassOf :template .

:writer rdfs:subClassOf :template .

u:reader rdfs:subClassOf :reader;
  :js \"emit(decodeURIComponent(parseURL(url).path[parseInt(this.children[0].index)+1]));\" .

u:writer rdfs:subClassOf :writer;
  :js \"emit(writeIndex(this));\".

u:writer rdfs:subClassOf :writer .

u:index rdfs:domain u:data .

u:index :name \"index\" .

u:data rdfs:subClassOf :json ;
  :param u:index .

:json rdfs:subClassOf :template ;
  :js \"emit(this)\" .

:view rdfs:subClassOf :component .

:key rdfs:domain :query .

:index rdfs:domain :path .

:component :param w:label .
w:label :name \"label\".


:template :param w:id .
w:id :name \"id\".

:template :param w:classes .

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
  :param w:classes ;
  rdfs:subClassOf :template ;
  :js \"emit(list(this));\" .

w:classes :name \"classes\" .

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


w:nav-item-
  rdfs:subClassOf :template ;
  :js \"emit(navItem(this));\" .

w:nav-item rdfs:subClassOf :view ;
  :param w:url, :children ;
  :base [a w:nav-item- ;
         :children (
                 [a w:active ;
                  w:url w:url]
                 [a w:projector;

                  :uri [a u:writer; :children (w:url)];
                  :children :children])] .

w:link-list rdfs:subClassOf :view ;
  :param w:url, w:list-binding, w:label, :children ;
  :base
    [a w:nav-list;
     w:url w:url;
     w:list-binding w:list-binding;
     w:label w:label;
     w:classes \"nav nav-list\" ;
     :children :children].

w:nav-list rdfs:subClassOf :view ;
  :param w:url, w:list-binding, w:label, :children, w:classes;
  :base
    [a w:list ;
     w:label w:label ;
     w:classes w:classes;
     :children (w:list-binding)].

w:list-binding :children (
  [a w:nav-item;
   w:url w:url;
   :children :children]) .


w:div rdfs:subClassOf :template ;
  :js \"emit(div(this));\" .

#n:tab-panel rdfs:subClassOf :view ;
#  :param n:tab-set, n:tab , n:base;
#  :base
#  [a w:plain;
#     :root n:base;
#     :children (
#        [a n:nav-bar; n:tab-binding n:has-tab; n:tab n:tab; w:url w:url]
#        [a w:div; w:classes])].


n:nav-bar rdfs:subClassOf :view ;
  :param n:tab-binding, n:tab, w:url;
  :base
    [a w:div;
     w:classes \"navbar\" ;
     :children
     ([a w:div;
       w:classes \"navbar-inner\" ;
       :children
        ([a w:nav-list;
          w:url w:url;
          w:list-binding n:tab-binding ;
          :children (n:tab) ;
          w:classes \"nav\"])])].

n:pane rdfs:subClassOf :view ;
  :param n:resource, w:url, :children;
  :base
    [a w:div;
    :root [a u:reader; :children (w:url)];
     :filter [a f:equals; f:val n:resource];
     w:classes \"container\";
     :children :children].

tm:type-manager rdfs:subClassOf :view ;
:param :root, w:label, tm:item , w:url, :children, :order-by;
:base
  [a w:row ;
  :root :root ;
  :children (
    [a w:column4 ;
     :children (
      [a w:link-list ;
       w:label w:label ;
       w:list-binding [:object rdf:type; :order-by :order-by] ;
       :children (tm:item) ;
       w:url w:url]
      [a w:creator ;
       :children ([:object rdf:type])])]
    [a w:column4;
     :root [a u:reader; :children (w:url)];
     :children :children])].


cb:relator rdfs:subClassOf :view ;
  :param cb:binding , w:url ;
  :base [a w:checkbox ;
         :children (cb:binding
                [a w:value ; :root [a u:reader; :children (w:url)]])] .

cb:binding :root [a u:reader; :children (w:url)] ;
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
  :js \"emit(joinString(this.children,''));\" .

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
  :param w:url ;
  :base [a w:active-test ;
         :children ([a w:value ; :root [a u:reader; :children (w:url)]]) ] .


f:val :name \"val\".

f:equals rdfs:subClassOf :template;
  :param f:val ;
  :js \"if(this.children[0]==this.val){emit('true')}else{emit('')};\" .

u:updater rdfs:subClassOf :template;
  :param u:update ;
  :js \"emit(this.update);\" .

u:update :name \"update\" .

u:update rdfs:domain u:updater .

w:s-brackets rdfs:subClassOf :template;
  :js \"emit(stringalize({list:'$children$','uri':uri,'binding':binding},{'children':'['+children[0].substring(0,children[0].length-1)+']'}));\".

w:commas rdfs:subClassOf :template;
  :js \"emit(children[0]+',')\".


w:j-list rdfs:subClassOf :view;
  :param w:j-binding;
  :base
    [a w:s-brackets;
     :children (w:j-binding)].

w:j-binding :children ([a w:commas; :children :children]).

w:f-list rdfs:subClassOf :template;
  :js \"emit(stringalize({list:['$children$'],'uri':uri,'binding':binding},{'children':children[0]}));\".

w:tester rdfs:subClassOf :template;
  :js \"emit(JSON.stringify(this));\".

m:map rdfs:subClassOf :template;
  :js \"emit(leafMap(this))\".

m:marker- rdfs:subClassOf :template;
  :js \"emit(stringalize({lat: '$children0$', lon: '$children1$', content: children[2], 'uri': uri, 'binding': binding}, {'children0':children[0],'children1':children[1]}));\".

m:num rdfs:subClassOf :template;
  :js \"emit(JSON.stringify({val: children[0].length>0?JSON.parse(children[0]):[], 'uri': uri, 'binding': binding}));\".

m:marker rdfs:subClassOf :view;
  :param m:content;
  :base
    [a m:marker-;
     :children ([a m:num; :children ([:subject geo:lat])]
                [a m:num; :children ([:subject geo:long])]
                m:content)].

g:graph rdfs:subClassOf :template;
  :js \"emit(labelGraph(this));\".

g:node rdfs:subClassOf :view;
  :param g:edge, g:label;
  :base
    [a g:node-;
     :children ([a w:j-list; w:j-binding g:edge; :children ([a g:node--])]
                g:label)].

g:node- rdfs:subClassOf :template;
  :js \"emit(stringalize({'children':'$children0$', 'uri':uri,'binding':binding, 'label':children[1]},{'children0':children[0]}));\".
g:node-- rdfs:subClassOf :template;
  :js \"emit(JSON.stringify({'uri':uri,'binding':binding}));\".

"

             ))

(def example-viewS (m/to-model
                   "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix : <http://logangilmour.com/data-binder#> .
@prefix ex: <http://logangilmour.com/example-view#> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix dc: <http://purl.org/dc/elements/1.1/> .
@prefix w: <http://logangilmour.com/bootstrap-widgets#> .
@prefix pl: <http://logangilmour.com/bootstrap-widgets/plain-list#> .
@prefix tm: <http://logangilmour.com/bootstrap-widgets/type-manager#> .
@prefix u: <http://logangilmour.com/uri#> .


:application a ex:foaf-manager .

ex:foaf-manager rdfs:subClassOf :view ;
:base
 [a w:tester;
    :root ex:thing;
    :children ([:object rdf:type])].

ex:thing a u:reader; :children ([u:index 1]).
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
@prefix n: <http://logangilmour.com/bootstrap-widgets/nav-bar#> .
@prefix f: <http://logangilmour.com/filter#> .
@prefix u: <http://logangilmour.com/uri#> .
@prefix geo: <http://www.w3.org/2003/01/geo/wgs84_pos#> .
@prefix m: <http://logangilmour.com/map#> .
@prefix g: <http://logangilmour.com/graph#> .

:application a ex:foaf-manager.

ex:base ex:hastab ex:Person-tab .

ex:Person-tab ex:label \"People\" .

ex:base ex:hastab ex:Map-tab .

ex:Map-tab ex:label \"Map\" .

ex:base ex:hastab ex:Graph-tab .

ex:Graph-tab ex:label \"Graph\" .



ex:foaf-manager rdfs:subClassOf :view ;
:base [a w:plain;
       :root ex:base;
       :children
      ([a n:nav-bar;
         n:tab-binding [:subject ex:hastab] ;
         n:tab [:subject ex:label] ;
         w:url [u:index 1]]

         [a n:pane;
          n:resource ex:Person-tab ;
          w:url [u:index 1];
          :children (ex:types)]
         [a n:pane;
          n:resource ex:Map-tab ;
          w:url [u:index 1];
          :children (ex:map)]
         [a n:pane;
          n:resource ex:Graph-tab ;
          w:url [u:index 1];
          :children (ex:graph)])].

ex:graph a w:column8 ;
  :root foaf:Person ;
  :children (
    [a g:graph;
     :children (
          [a w:j-list; w:j-binding [:object rdf:type];
            :children
              ([a g:node; g:edge [:subject foaf:knows]; g:label [:subject foaf:givenName]])])]).

ex:map a w:column8 ;
  :root foaf:Person ;
  :children (
    [a m:map;
      :children
            ([a w:j-list;
              w:j-binding [:object rdf:type];
              :children
                ([a m:marker; m:content ex:fullName])])]).


ex:types a tm:type-manager;
  :root foaf:Person;
  w:label \"People\";
  tm:item ex:fullName;
  w:url [u:index 2];
  :order-by ([:subject foaf:familyName] [:subject foaf:givenName]);
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

    [w:label \"Lat\" ;
     a w:text-field;
     :children ([:subject geo:lat])]

    [w:label \"Long\" ;
     a w:text-field;
     :children ([:subject geo:long])]

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
           w:url [u:index 2]] ) ] ) ]
     [a m:map;
      :children
       ([a w:f-list;
         :children
            ([a m:marker; m:content ex:fullName])])]).

ex:fullName a w:join-text;
  w:join-with \" \";
  :children (
    [a w:text;
     :children ([:subject foaf:givenName])]
    [a w:text;
     :children ([:subject foaf:familyName])]).

"))


(def expanded-example nil)
(def broadcast-channel (permanent-channel))
(def data nil)


(defn page [body]
  (hic/html
   [:html
    [:head
     [:meta {:name "viewport" :content "width=device-width, initial-scale=1.0"}]

     [:link {:href "/css/bootstrap.min.css" :rel "stylesheet" :media "screen"}]
     [:link {:rel "stylesheet" :href "http://cdn.leafletjs.com/leaflet-0.5/leaflet.css"}]
     [:link {:href "/css/datepicker.css" :rel "stylesheet" :media "screen"}]
     [:link {:href "/css/graph.css" :rel "stylesheet" :media "screen"}]
     [:title "FIXME"]
     [:script {:type "text/javascript" :src "/js/jquery.min.js"}]]
    [:body body
     [:script {:type "text/javascript" :src "/js/bootstrap.min.js"}]
     [:script {:type "text/javascript" :src "/js/bootstrap-datepicker.js"}]
     [:script {:type "text/javascript" :src "/js/underscore-min.js"}]
     [:script {:type "text/javascript" :src "http://cdn.leafletjs.com/leaflet-0.5/leaflet.js"}]
     [:script {:type "text/javascript" :src "http://d3js.org/d3.v3.min.js"}]
     [:script {:type "text/javascript" :src "/js/update.js"}]]

    ]
   ))

(defn startup []
  (do
    (print "starting up server!")))


(def channels (atom {}))

(defn register [ch url]
  (siphon (map* (partial edit expanded-example data) (map* decode-json ch)) broadcast-channel)
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
                                                   (synchronize expanded-example data message url))
                                                 broadcast-channel))
                                               new-ch)
                                       new-ch))]
                      (siphon url-ch ch) ;; listen on your url channel
                      (doall (map (partial println "\nConnection: ") (keys all)))
                      (assoc all url url-ch)))))

(defn chat-handler [ch handshake]
  (receive ch
           (fn [input]
             (let [url (str (clojure.string/replace (:uri handshake) #"^/async/" "/view/")
                            "?" (:query-string handshake))]
               (println "registering a connection to url " url)
               ;;(let jc [(map* decode-json ch)])
               (register ch url))
             )))


(defroutes main-routes
  (GET "/async/*" [] (wrap-aleph-handler chat-handler))
  (GET "/view/*" [* :as request]
       (let [uri ;;(clojure.string/replace (:uri request) #"^/view/" "")
             (str (:uri request) "?" (:query-string request))
             ]
         (println "URL!!!!!!! " uri)
         (page (interpreter expanded-example data uri))))
  (route/resources "/")
  (route/not-found "Page not found"))


(defn -main
  [& args]
  (def expanded-example (preprocess example-view widgets))
  (def data (m/union (m/default-model) (:model expanded-example)))
  (template-env)
  (start-http-server (wrap-ring-handler main-routes)
                     {:port 8080 :websocket true}))



;;(do (use 'databinder.core :reload-all) (def stop! (-main)) (defn reload [] (stop!) (use 'databinder.core :reload-all) (def stop! (-main))))
