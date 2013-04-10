
function process(type,uri,binding,value){
    $("input.text-field-binding").each(function(i){
        if($(this).data("uri")==uri &&
           $(this).data("binding")==binding){
            $(this).val(value);
        }
    });
    $("a.projector-binding").each(function(i){
        if($(this).data("uri")==uri &&
           $(this).data("binding")==binding){
            $(this).text(value);
        }
    });
    $(".list-binding").each(function(i){
        if($(this).data("uri")==uri &&
           $(this).data("binding")==binding){
            if(type=="delete"){
                $(this).children("li[data-uri='"+value+"']").remove(); // make this global, then go back to minimal functions for everything!
            } else if (type=="create"){
                $(this).append($(value));
            }
        }
    });
    //$("[data-binding='"+binding+"']").each(function(i){
    // TODO figure this out    if($(this).data("uri")==
}

function setup(){
     var path = window.location.pathname.replace(/^\/view\//,"");
     var ws = new WebSocket("ws://localhost:8080/async/"+path);
        ws.onmessage = function(evt) {
            console.log(evt.data);
            _.map(JSON.parse(evt.data),
                  function(message){
                      process(message.type,message.uri,message.binding,message.value);
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

}

setup();