
//Mustache.compilePartial('title', "{{#param}}http://purl.org/dc/elements/1.1/title{{/param}}");


/*
    cycle.js
    2013-02-19

    Public Domain.

    NO WARRANTY EXPRESSED OR IMPLIED. USE AT YOUR OWN RISK.

    This code should be minified before deployment.
    See http://javascript.crockford.com/jsmin.html

    USE YOUR OWN COPY. IT IS EXTREMELY UNWISE TO LOAD CODE FROM SERVERS YOU DO
    NOT CONTROL.
*/

/*jslint evil: true, regexp: true */

/*members $ref, apply, call, decycle, hasOwnProperty, length, prototype, push,
    retrocycle, stringify, test, toString
*/

if (typeof JSON.decycle !== 'function') {
    JSON.decycle = function decycle(object) {
        'use strict';

// Make a deep copy of an object or array, assuring that there is at most
// one instance of each object or array in the resulting structure. The
// duplicate references (which might be forming cycles) are replaced with
// an object of the form
//      {$ref: PATH}
// where the PATH is a JSONPath string that locates the first occurance.
// So,
//      var a = [];
//      a[0] = a;
//      return JSON.stringify(JSON.decycle(a));
// produces the string '[{"$ref":"$"}]'.

// JSONPath is used to locate the unique object. $ indicates the top level of
// the object or array. [NUMBER] or [STRING] indicates a child member or
// property.

        var objects = [],   // Keep a reference to each unique object or array
            paths = [];     // Keep the path to each unique object or array

        return (function derez(value, path) {

// The derez recurses through the object, producing the deep copy.

            var i,          // The loop counter
                name,       // Property name
                nu;         // The new object or array

// typeof null === 'object', so go on if this value is really an object but not
// one of the weird builtin objects.

            if (typeof value === 'object' && value !== null &&
                    !(value instanceof Boolean) &&
                    !(value instanceof Date)    &&
                    !(value instanceof Number)  &&
                    !(value instanceof RegExp)  &&
                    !(value instanceof String)) {

// If the value is an object or array, look to see if we have already
// encountered it. If so, return a $ref/path object. This is a hard way,
// linear search that will get slower as the number of unique objects grows.

                for (i = 0; i < objects.length; i += 1) {
                    if (objects[i] === value) {
                        return {$ref: paths[i]};
                    }
                }

// Otherwise, accumulate the unique value and its path.

                objects.push(value);
                paths.push(path);

// If it is an array, replicate the array.

                if (Object.prototype.toString.apply(value) === '[object Array]') {
                    nu = [];
                    for (i = 0; i < value.length; i += 1) {
                        nu[i] = derez(value[i], path + '[' + i + ']');
                    }
                } else {

// If it is an object, replicate the object.

                    nu = {};
                    for (name in value) {
                        if (Object.prototype.hasOwnProperty.call(value, name)) {
                            nu[name] = derez(value[name],
                                path + '[' + JSON.stringify(name) + ']');
                        }
                    }
                }
                return nu;
            }
            return value;
        }(object, '$'));
    };
}


if (typeof JSON.retrocycle !== 'function') {
    JSON.retrocycle = function retrocycle($) {
        'use strict';

// Restore an object that was reduced by decycle. Members whose values are
// objects of the form
//      {$ref: PATH}
// are replaced with references to the value found by the PATH. This will
// restore cycles. The object will be mutated.

// The eval function is used to locate the values described by a PATH. The
// root object is kept in a $ variable. A regular expression is used to
// assure that the PATH is extremely well formed. The regexp contains nested
// * quantifiers. That has been known to have extremely bad performance
// problems on some browsers for very long strings. A PATH is expected to be
// reasonably short. A PATH is allowed to belong to a very restricted subset of
// Goessner's JSONPath.

// So,
//      var s = '[{"$ref":"$"}]';
//      return JSON.retrocycle(JSON.parse(s));
// produces an array containing a single element which is the array itself.

        var px =
            /^\$(?:\[(?:\d+|\"(?:[^\\\"\u0000-\u001f]|\\([\\\"\/bfnrt]|u[0-9a-zA-Z]{4}))*\")\])*$/;

        (function rez(value) {

// The rez function walks recursively through the object looking for $ref
// properties. When it finds one that has a value that is a path, then it
// replaces the $ref object with a reference to the value that is found by
// the path.

            var i, item, name, path;

            if (value && typeof value === 'object') {
                if (Object.prototype.toString.apply(value) === '[object Array]') {
                    for (i = 0; i < value.length; i += 1) {
                        item = value[i];
                        if (item && typeof item === 'object') {
                            path = item.$ref;
                            if (typeof path === 'string' && px.test(path)) {
                                value[i] = eval(path);
                            } else {
                                rez(item);
                            }
                        }
                    }
                } else {
                    for (name in value) {
                        if (typeof value[name] === 'object') {
                            item = value[name];
                            if (item) {
                                path = item.$ref;
                                if (typeof path === 'string' && px.test(path)) {
                                    value[name] = eval(path);
                                } else {
                                    rez(item);
                                }
                            }
                        }
                    }
                }
            }
        }($));
        return $;
    };
}




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

function mstr(obj, depth){
    if (depth == 0){
        return "done";
    }

    var properties = [];
for(var key in obj) {
    try{
    if(obj.hasOwnProperty(key) && typeof obj[key] !== 'function') {
        properties.push({'key': key, 'val': mstr(obj[key],depth-1)});
    }
}catch(e){
    for(i=0;i<obj.length;i++){
        properties.push({'key': key, 'val':mstr(obj[i],depth-1)});
    }

}}
return JSON.stringify(properties);
}

function mst(val){

    var cache = [];
    JSON.stringify(val, function(key, value) {
        if (typeof value === 'object' && value !== null) {
            if (cache.indexOf(value) !== -1) {
                // Circular reference found, discard key
                return null;
            }
            // Store value in our collection
            cache.push(value);
        }
        return value;
    });
    cache = null;
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

var leafMap = Mustache.compile(
"<div class='leaf-map' data-uri='{{uri}}' data-binding='{{binding}}' style='height:200px' data-markers='{{children.0}}'/>");

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
"<div class='span4' data-uri='{{{uri}}}' data-binding='{{{binding}}}'>{{{children}}}</div>"));

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
