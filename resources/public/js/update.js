markers = [];


binders = {
    "input.text-field-binding, input.date-binding": function(){
        var el = this;
        addBound($(this).data("binding"),
                 $(this).data("uri"),
                 {"update": function(value){$(el).val(value);}});
    },
    "input.checkbox-binding": function(){
        var el = this;
        addBound($(this).data("binding"),
                 $(this).data("uri"),
                 {"create": function(value){ $(el).prop("checked",true);},
                  "delete": function(value){$(el).prop("checked",false);}});
    },
    ".bound": function(){
        var el = this;
        addBound($(this).data("binding"),
                 $(this).data("uri"),
                 {"update":function(value){$(el).html(value);}});
    },
    "a.projector-binding": function(){
        var el = this;
        addBound($(this).data("binding"),
                 $(this).data("uri"),
                 {"update":function(value){$(el).text(value);}});
    },
    ".list-binding": function(){
        var el = this;
        addBound($(this).data("binding"),
                 $(this).data("uri"),
                 {"create":function(value){
                     var child = $(value);
                     $(el).append(child);
                     _.map(binders,function(val,key){
                         child.find(key).each(val);});
                 },
                 "delete":function(value){$(el).children("li[data-uri='"+value+"']").remove();}});
    }};


    //_.map(markers,function(marker){
    //    if(marker.lat.uri==uri && marker.lat.binding==binding){
    //        var latlng = marker.marker.getLatLng();
    //        var lng = latlng.lng;
    //        marker.marker.setLatLng([value,lng]);
    //    }
    //});
    //$("[data-binding='"+binding+"']").each(function(i){
    // TODO figure this out    if($(this).data("uri")==


window.bindings = {};
function addBound(binding, resource, fmap){
    var resources = bindings[binding];
    if (!resources) {
        resources = {};
        bindings[binding]=resources;
    }

    resources[resource] = fmap;
}

function getBound(binding, resource, type){
    try{
    return bindings[binding][resource][type];
    }catch(e){return null;}
}
function deleteBound(binding, resource){
    try{
        bindings[binding][resource]=null;
    }catch(e){
    }
}
function setup(){
     var path = window.location.pathname.replace(/^\/view\//,"");
    _.map(binders, function(val, key){
        $(key).each(val);
        });

     window.ws = new WebSocket("ws://localhost:8080/async/"+path+window.location.search);
        ws.onmessage = function(evt) {
            console.log(evt.data);
            _.map(JSON.parse(evt.data),
                  function(message){
                      //process(message.type,message.uri,message.binding,message.value);
                      bound = getBound(message.binding, message.uri, message.type);
                      if (bound){
                          bound(message.value);
                      }

                  });
        };

        ws.onclose = function() { console.log("socket closed");};
        ws.onopen = function() {
          console.log("Connected...");
          ws.send("hello server");
        };

    $("body").on("focusout", ".text-field-binding",
                 function(evt){
                     var el = $(evt.target);
                     var oldval = el.data("old-value");
                     var newval = el.val();
                     if(oldval != newval){
                         ws.send(
                             JSON.stringify(
                                 {uri: el.data("uri"),
                                  binding: el.data("binding"),
                                  data:newval,
                                  type:"update"}));
                         $(evt.target).data("old-value",newval);
                     }
                 });
    $("body").on("focusout", ".date-binding",
                 function(evt){
                     var el = $(evt.target);

                         ws.send(
                             JSON.stringify(
                                 {uri: el.data("uri"),
                                  binding: el.data("binding"),
                                  data:el.val(),
                                  type:"update"}));
                 });
    $("body").on("changeDate", ".date-binding",
                 function(evt){
                     var el = $(evt.target);

                         ws.send(
                             JSON.stringify(
                                 {uri: el.data("uri"),
                                  binding: el.data("binding"),
                                  data:el.val(),
                                  type:"update"}));
                 });
    $("body").on("click", ".list-add-binding",
                 function(evt){
                     var el = $(evt.target);

                     ws.send(
                             JSON.stringify(
                                 {uri: el.data("uri"),
                                  binding: el.data("binding"),
                                  data:"",
                                  type:"create"}));
                 });
    $("body").on("click", "input.checkbox-binding",
                 function(evt){
                     var el = $(evt.target);
                     var type = "create";
                     if(!el.prop("checked")){
                         type="delete";
                     }
                     var mes = JSON.stringify({uri: el.data("uri"),
                                 binding: el.data("binding"),
                                 data:el.val(),
                                 type:type});

                     //alert(mes);
                     ws.send(
                            mes
                                );
                 });
    $("body").on("click", ".remove-binding",
                 function(evt){
                     var el = $(evt.target);

                     ws.send(
                             JSON.stringify(
                                 {uri: el.data("uri"),
                                  binding: el.data("binding"),
                                  data:el.data("value"),
                                  type:"delete"}));
                 });
    leafMap();
    graph();

}

function leafMap(){
    $(".leaf-map").each(function(i,el){
        var map = L.map(el, {
            center: [51.505, -0.09],
            zoom: 13
        });
        L.tileLayer('http://{s}.tile.cloudmade.com/0003702debcd4b348f8129a8ac9c163f/997/256/{z}/{x}/{y}.png', {
            attribution: 'Map data &copy; <a href="http://openstreetmap.org">OpenStreetMap</a> contributors, <a href="http://creativecommons.org/licenses/by-sa/2.0/">CC-BY-SA</a>, Imagery © <a href="http://cloudmade.com">CloudMade</a>',
            maxZoom: 18
        }).addTo(map);

        var markers = $(el).data("markers").list;
        //for (child in children) {
        _.map(markers, function(child){
        try{
        var marker = L.marker([child.lat.val, child.lon.val],{draggable: true});

        popup = marker.bindPopup(child.content);

        marker.on('click', function(){marker.openPopup();});

        marker.on('dragend',function(){
            var loc = marker.getLatLng();
            ws.send(
                             JSON.stringify(
                                 {uri: child.uri,
                                  binding: child.lat.binding,
                                  data: loc.lat,
                                  type:"update"}));
            ws.send(
                             JSON.stringify(
                                 {uri: child.uri,
                                  binding: child.lon.binding,
                                  data: loc.lng,
                                  type:"update"}));
            });


        addBound(child.lon.binding, child.lon.uri,
                 {"update": function(val){
                     var lat = marker.getLatLng().lat;
                     marker.setLatLng([lat,val]);}});

        addBound(child.lat.binding, child.lat.uri,
                 {"update": function(val){
                     var lng = marker.getLatLng().lng;
                     marker.setLatLng([val,lng]);}});


        marker.addTo(map);

        }catch(e){}
        });

        //}
    });

}

setup();

function graph(){
    d3.selectAll(".labelGraph").each(function(d, i){
    var width = 960,
    height = 500;

var color = d3.scale.category10();

var nodes = [],
    links = [];

var force = d3.layout.force()
    .nodes(nodes)
    .links(links)
    .charge(-400)
    .linkDistance(120)
    .size([width, height])
    .on("tick", tick);

var svg = d3.select("div.labelGraph").append("svg")
    .attr("width", width)
    .attr("height", height);


        var node = svg.selectAll(".node"),
            link = svg.selectAll(".link");


        var nodeset = JSON.parse(this.dataset.nodes);
        var nodemap = {};




// 1. Add three nodes and three links.
setTimeout(function() {


        _.map(nodeset.list,function(node){
            if(node.uri){
                var built = {id: node.uri, label:node.label};
                nodemap[node.uri] = built;
                nodes.push(built);
                console.log("Binding: " + node.uri+ ", " +node.binding);
                addBound(node.binding,node.uri,
                         {"update": function(value){
                             built.label=value;
                             start();}});
                addBound(node.children.binding,node.uri,
                         {"create": function(value){

                             var val = JSON.parse(value.substring(0,value.length-1));
                             links.push({source: nodemap[node.uri], target: nodemap[val.uri]});
                             start();},
                          "delete": function(val){
                              _.map(links,function(v,i){
                                  console.log(v);
                                  if(v.target==nodemap[val] && v.source==nodemap[node.uri]){
                                      links.splice(i,i+1);
                                      start();
                                      return;
                                  }
                              });
                          }});
            }
        });

    _.map(nodeset.list,function(node){

        if(node.uri){
            var n1 = nodemap[node.uri];
            _.map(node.children.list,function(child){
                if(nodemap[child.uri]){

                    var n2 = nodemap[child.uri];

                    links.push({source: n1, target: n2});

                }
            });
        }
    });

    addBound(nodeset.binding,nodeset.uri,
             {"create": function(value){
                 var node = JSON.parse(value.substring(0,value.length-1));
                 var built = {id: node.uri, label:node.label};
                 nodes.push(built);
                 nodemap[node.uri]= built;
                 addBound(node.binding,node.uri,
                         {"update": function(value){
                             built.label=value;
                             start();}});
                 addBound(node.children.binding, node.uri,
                          {"create": function(value){

                              var val = JSON.parse(value.substring(0,value.length-1));
                              links.push({source: nodemap[node.uri], target: nodemap[val.uri]});

                              start();},
                           "delete": function(val){
                               _.map(links,function(v,i){
                                   if(v.target==nodemap[val] && v.source==nodemap[node.uri]){
                                       links.splice(i,i+1);
                                       start();
                                       return;
                                   }
                               });
                           }});
                 start();},
              "delete": function(val){
                  _.map(nodes,function(v,i){
                      if(nodemap[val]==v){
                          nodes.splice(i,i+1);
                          var j=0;
                          while(j<links.length){
                              if(links[j].source==nodemap[val]
                                 || links[j].target==nodemap[val]){
                                  links.splice(j,j+1);

                              }else{
                                  j+=1;
                              }
                          }

                          delete nodemap[val];
                          start();
                          return;
                      }
                  });
              }});

    start();
}, 0);

function start() {
  link = link.data(force.links(), function(d) { return d.source.id + "-" + d.target.id; });
  link.enter().insert("line", ".node").attr("class", "link");
  link.exit().remove();

  node = node.data(force.nodes(), function(d) { return d.id;});
  node.text(function(d, i) {
                                return d.label;
                        });
  //node.enter().append("circle").attr("class", function(d) { return "node " + d.id; }).attr("r", 8);
  node.enter().append("svg:text").text(function(d, i) {
                                return d.label;
                        }).style("fill", "#555").style("font-family", "Arial").style("font-size", 12);
  node.exit().remove();

  force.start();
}

function tick() {
  node.attr("x", function(d) { return d.x; })
      .attr("y", function(d) { return d.y; });

  link.attr("x1", function(d) { return d.source.x; })
      .attr("y1", function(d) { return d.source.y; })
      .attr("x2", function(d) { return d.target.x; })
      .attr("y2", function(d) { return d.target.y; });
}
    });
}
