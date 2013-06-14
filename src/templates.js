
//Mustache.compilePartial('title', "{{#param}}http://purl.org/dc/elements/1.1/title{{/param}}");

var untitled =
function(){
    return function(val, render){
        var rend = render(val);
        if(/^\s*$/.test(rend)){
            return "untitled";
        }else{
            return rend;
        }
    };
};

var first =
function(){
    return function(val, render){
         return render(val);
    };
};

function joinString(array, joinStr){
    return _.reduce(array, function(accum,val){ return accum+joinStr+val; }, "");
}

function parseURL(url){
    var regex = /^(([^:\/?#]+):)?(\/\/([^\/?#]*))?([^?#]*)(\?([^#]*))?(#(.*))?/;
    var match = regex.exec(url);
    var path;
    if (match[5]){
        path = match[5].split("/");
    }
    return {scheme : match[1], authority : match[3], path : path , query : match[6], fragment : match[8]};
};

function stringURL(url){
   var path = "";
   if(url.path){
       path=url.path.join("/");
   }   return (url.scheme || "")+(url.authority || "")+path+(url.query||"")+(url.fragment||"");
}

function writeIndex(context){
    var u = parseURL(context.url);
    var i = parseInt(context.children[0].index)+1;
    u.path[i]=encodeURIComponent(context.uri);
    return stringURL(u);
}

function arrayEqual(a, b) {
        var i = Math.max(a.length, b.length, 1);
        while(i-- >= 0 && a[i] === b[i]);
        return (i === -2);
}

function flatten(arr) {
        var r = [];

        while (!arrayEqual(r, arr)) {
                r = arr;
                arr = [].concat.apply([], arr);
        }
        return joinString(arr,"");
}

function flat(fn){
    return function(context){
        //context.first = context.children[0];
        //context.second = context.children[1];
        //context.third = context.children[2];
        //context.fourth = context.children[3];
        //context.fifth = context.children[4];
        //context.sixth = context.children[5];
        context.children = flatten(context.children);
            return fn(context);
    };
};


//Mustache.compilePartial('untitled', "{{#untitled}}{{title}}{{/untitled}}");

var projector = flat(Mustache.compile(
"<a class='projector-binding' href='{{url}}' "
+ "data-uri='{{{uri}}}'  "
+ "data-binding='{{{binding}}}'>{{#untitled}}{{{children}}}{{/untitled}}</a>" ));

var leafMap = flat(Mustache.compile(
"<div class='leaf-map' data-uri='{{uri}}' data-binding='{{binding}}' style='height:200px' data-children='{{children}}'/>"));



var div = flat(Mustache.compile(
"<div data-binding='{{{binding}}} data-uri='{{{uri}}}' class='{{{classes}}}'>{{{children}}}</div>"));

var navItem = Mustache.compile(
"<li class='{{{children.0}}}' data-uri='{{{uri}}}' data-binding='{{{binding}}}'>"
+ "{{{children.1}}}</li>");

var li = flat(Mustache.compile(
"<li data-uri='{{{uri}}}' data-binding='{{{binding}}}'>"
+ "{{{children}}}</li>"));

var column8 = flat(Mustache.compile(
"<div class='span8' data-uri='{{{uri}}}' data-binding='{{{binding}}}'>{{{children}}}</div>"));

var column4 = flat(Mustache.compile(
"<div class='span6' data-uri='{{{uri}}}' data-binding='{{{binding}}}'>{{{children}}}</div>"));

var row = flat(Mustache.compile(
"<div class='row'>{{{children}}}</div>"));

var list = flat(Mustache.compile(
"<ul class='list-binding {{classes}}' data-uri='{{{uri}}}' data-binding='{{{binding}}}'>"
+ "{{{children}}}</ul>"));

var span = flat(Mustache.compile(
"{{before}}<span class='bound' data-uri='{{{uri}}}' data-binding='{{{binding}}}'>{{#untitled}}{{{children}}}{{/untitled}}</span>{{after}}"));

var join = function(context){
    return joinString(context.children,context.joinWith);
};

var deleter = flat(Mustache.compile(
"<a class='remove-binding' href='#' data-uri='{{{uri}}}' data-binding='{{{binding}}}'"
+ "data-value='{{{children}}}'>{{label}}</a>"));

var creator = flat(Mustache.compile(
"<a href='#' class='list-add-binding btn' data-uri='{{{uri}}}' "
        + "data-binding='{{{binding}}}'>Create</a>"));

var checkbox = Mustache.compile(
"<input type='checkbox' class='checkbox-binding' data-uri='{{{uri}}}' data-binding='{{{binding}}}' value='{{{children.1}}}' {{#children.0}}checked='checked'{{/children.0}}>");

var check = flat(Mustache.compile(
"checked='checked'"));

var value = flat(Mustache.compile(
"{{{uri}}}"));

var paragraph = flat(Mustache.compile(
"<p data-uri='{{{uri}}}' data-binding='{{{binding}}}'>{{label}}: {{{children}}}</p>"));

var textField = flat(Mustache.compile(
"<label>{{{label}}} <input type='text' class='text-field-binding' data-uri='{{{uri}}}'"
+ "data-binding='{{{binding}}}' value='{{children}}'></label>"));

var popup = flat(Mustache.compile(
"<a href='#modal{{{id}}}' data-target='#modal{{{id}}}' role='button' data-toggle='modal'>{{label}}</a>"
+ "<div id='modal{{{id}}}' class='modal hide fade' tabindex='-1' role='dialog' aria-labelledby='myModalLabel' aria-hidden='true'>"
+"  <div class='modal-header'>"
+"    <button type='button' class='close' data-dismiss='modal' aria-hidden='true'>×</button>"
+"    <h3 id='myModalLabel'>{{label}}</h3>"
+"  </div>"
+"  <div class='modal-body'>"
+"{{{children}}}"
+"  </div>"
+"  <div class='modal-footer'>"
+"    <button class='btn' data-dismiss='modal' aria-hidden='true'>Done</button>"
+"  </div>"
+"</div>"));


var string = Mustache.compile(
"{{{label}}}");

var datepicker = flat(Mustache.compile(
"<label>{{label}} <input class='date-binding' data-provide='datepicker' data-date-format='dd-mm-yyyy' data-binding='{{{binding}}}' data-uri='{{{uri}}}' size='12' type='text' value='{{children}}'></label>"));
