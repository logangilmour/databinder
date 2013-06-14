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

        var markers = $(el).data("markers");
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
