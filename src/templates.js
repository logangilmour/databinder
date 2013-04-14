
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

//Mustache.compilePartial('untitled', "{{#untitled}}{{title}}{{/untitled}}");

var projector = Mustache.compile(
"<a class='projector-binding' href='{{url}}' "
+ "data-uri='{{{uri}}}'  "
+ "data-binding='{{{binding}}}'>{{#untitled}}{{{vals}}}{{/untitled}}</a>" );


var listItem = Mustache.compile(
"<li class='{{#active}}active{{/active}}' data-uri='{{{uri}}}' data-binding='{{{binding}}}'>"
+ "{{{vals}}}</li>");

var column8 = Mustache.compile(
"<div class='span8'>{{{vals}}}</div>");

var column4 = Mustache.compile(
"<div class='span4'>{{{vals}}}</div>");

var row = Mustache.compile(
"<div class='row'>{{{vals}}}</div>");

var list = Mustache.compile(
"<ul class='list-binding nav nav-list' data-uri='{{{uri}}}' data-binding='{{{binding}}}'>"
+ "<li class='nav-header'>{{title}}</li>{{{vals}}}</ul>"
        + "<a href='#' class='list-add-binding btn' data-uri='{{{uri}}}' "
        + "data-binding='{{{binding}}}'>Create</a>");


var deleter = Mustache.compile(
"<a class='remove-binding' href='#' data-uri='{{{uri}}}' data-binding='{{{binding}}}'"
+ "data-value='{{{vals}}}'>{{title}}</a>");

var paragraph = Mustache.compile(
"<p data-uri='{{{uri}}}' data-binding='{{{binding}}}'>{{title}}: {{{vals}}}</p>");

var textField = Mustache.compile(
"<label>{{{title}}} <input type='text' class='text-field-binding' data-uri='{{{uri}}}'"
+ "data-binding='{{{binding}}}' value='{{{vals}}}'></label>");
