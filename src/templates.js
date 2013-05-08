
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

function flat(fn){
    return function(context){
        //context.first = context.children[0];
        //context.second = context.children[1];
        //context.third = context.children[2];
        //context.fourth = context.children[3];
        //context.fifth = context.children[4];
        //context.sixth = context.children[5];
        context.children = joinString(context.children, "");
        return fn(context);
    };
};


//Mustache.compilePartial('untitled', "{{#untitled}}{{title}}{{/untitled}}");

var projector = flat(Mustache.compile(
"<a class='projector-binding' href='{{url}}' "
+ "data-uri='{{{uri}}}'  "
+ "data-binding='{{{binding}}}'>{{#untitled}}{{{children}}}{{/untitled}}</a>" ));


var listItem = Mustache.compile(
"<li class='{{{children.0}}}' data-uri='{{{uri}}}' data-binding='{{{binding}}}'>"
+ "{{{children.1}}}</li>");

var li = flat(Mustache.compile(
"<li data-uri='{{{uri}}}' data-binding='{{{binding}}}'>"
+ "{{{children}}}</li>"));

var column8 = flat(Mustache.compile(
"<div class='span8'>{{{children}}}</div>"));

var column4 = flat(Mustache.compile(
"<div class='span4'>{{{children}}}</div>"));

var row = flat(Mustache.compile(
"<div class='row'>{{{children}}}</div>"));

var list = flat(Mustache.compile(
"{{label}}:<ul class='list-binding nav nav-list {{classes}}' data-uri='{{{uri}}}' data-binding='{{{binding}}}'>"
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
"<input type='checkbox' class='checkbox-binding' data-uri='{{{uri}}}' data-binding='{{{binding}}}' value='{{{children.0}}}' {{#children.1}}checked='checked'{{/children.1}}>");

var check = flat(Mustache.compile(
"checked='checked'"));

var value = flat(Mustache.compile(
"{{{uri}}}"));

var paragraph = flat(Mustache.compile(
"<p data-uri='{{{uri}}}' data-binding='{{{binding}}}'>{{label}}: {{{children}}}</p>"));

var textField = flat(Mustache.compile(
"<label>{{{label}}} <input type='text' class='text-field-binding' data-uri='{{{uri}}}'"
+ "data-binding='{{{binding}}}' value='{{{children}}}'></label>"));

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
"<label>{{label}} <input class='date-binding' data-provide='datepicker' data-date-format='dd-mm-yyyy' data-binding='{{{binding}}}' data-uri='{{{uri}}}' size='12' type='text' value='{{{children}}}'></label>"));
