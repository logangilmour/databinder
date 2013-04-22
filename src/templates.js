
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
+ "<li class='nav-header'>{{title}}</li>{{{vals}}}</ul>");

var span = Mustache.compile(
"{{before}}<span class='bound' data-uri='{{{uri}}}' data-binding='{{{binding}}}'>{{#untitled}}{{{vals}}}{{/untitled}}</span>{{after}}");

var deleter = Mustache.compile(
"<a class='remove-binding' href='#' data-uri='{{{uri}}}' data-binding='{{{binding}}}'"
+ "data-value='{{{vals}}}'>{{title}}</a>");

var creator = Mustache.compile(
"<a href='#' class='list-add-binding btn' data-uri='{{{uri}}}' "
        + "data-binding='{{{binding}}}'>Create</a>");

var checkbox = Mustache.compile(
"<input type='checkbox' class='checkbox-binding' data-uri='{{{uri}}}' data-binding='{{{binding}}}' value='{{{first}}}' {{#second}}checked='checked'{{/second}}>");

var check = Mustache.compile(
"checked='checked'");

var value = Mustache.compile(
"{{{uri}}}");

var paragraph = Mustache.compile(
"<p data-uri='{{{uri}}}' data-binding='{{{binding}}}'>{{title}}: {{{vals}}}</p>");

var textField = Mustache.compile(
"<label>{{{title}}} <input type='text' class='text-field-binding' data-uri='{{{uri}}}'"
+ "data-binding='{{{binding}}}' value='{{{vals}}}'></label>");

var popup = Mustache.compile(
"<a href='#myModal' data-target='#myModal' role='button' data-toggle='modal'>{{title}}</a>"
+ "<div id='myModal' class='modal hide fade' tabindex='-1' role='dialog' aria-labelledby='myModalLabel' aria-hidden='true'>"
+"  <div class='modal-header'>"
+"    <button type='button' class='close' data-dismiss='modal' aria-hidden='true'>×</button>"
+"    <h3 id='myModalLabel'>{{title}}</h3>"
+"  </div>"
+"  <div class='modal-body'>"
+"{{{vals}}}"
+"  </div>"
+"  <div class='modal-footer'>"
+"    <button class='btn' data-dismiss='modal' aria-hidden='true'>Done</button>"
+"  </div>"
+"</div>");


var datepicke = Mustache.compile(
"<div class='input-append date datepicker' data-provide='datepicker' data-date='12-02-2012' data-date-format='dd-mm-yyyy'>"
 +"<input data-provied='datepicker' data-binding='{{{binding}}}' data-uri='{{{uri}}}' size='16' type='text' value='{{{vals}}}' readonly></div>");

var datepicker = Mustache.compile(
"<label>{{title}} <input class='date-binding' data-provide='datepicker' data-date-format='dd-mm-yyyy' data-binding='{{{binding}}}' data-uri='{{{uri}}}' size='12' type='text' value='{{{vals}}}'></label>");
