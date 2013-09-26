// This file is part of prosoda.  prosoda is free software: you can
// redistribute it and/or modify it under the terms of the GNU General Public
// License as published by the Free Software Foundation, version 2.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
// Copyright 2013 by Siemens AG, Albert Eckert <albert.eckert@siemens.com>
// All Rights Reserved.

$(function(){ //document ready

 // Initialize any divs with class gridster
  $(".gridster > ul").each(function() {
    $el = $(this);

    var marginx = Number(this.getAttribute('data-marginx')) || 10;
    var marginy = Number(this.getAttribute('data-marginy')) || 10;
    var width   = Number(this.getAttribute('data-width'))   || 140;
    var height  = Number(this.getAttribute('data-height'))  || 140;

    $(this).gridster({
      widget_margins: [marginx, marginy],
      widget_base_dimensions: [width, height],
      widget_selector: "> li.qawidget",
      serialize_params: 
        function($w, wgd) { 
          return {  col: wgd.col, row: wgd.row, 
                    size_x: wgd.size_x, size_y: wgd.size_y, 
                    id: $w.children("div['data-qaid']").attr("data-qaid"),
                    cls: $w.children("div['data-qaclass']").attr("data-qaclass")} }
    });
  });

  Shiny.addCustomMessageHandler("GridsterMessage",
    function(message) {	
			switch(message.msgname) {
			case 'addWidget': 
				var gridster = $(".gridster > ul").gridster().data('gridster');
				Shiny.unbindAll();
				gridster.add_widget( message.html, message. size_x, message.size_y, message.col, message.row );

        $(".gridster div#"+message.qaid).popover(message.help);
        
        $(".icon-remove-sign").click(function(){
          var gridster = $(".gridster > ul").gridster().data('gridster');
          var el = $(this).parent();
          var qaid = $(this).parent().children("div['data-qaid']").attr("data-qaid");
          $(".gridster div#"+ qaid).popover('destroy'); 
          Shiny.unbindAll();
          gridster.remove_widget(el);
          Shiny.bindAll();
        })
				Shiny.bindAll();
        var toel = $(".gridsterButton");
        toel.attr("gridster-action","updateconfig");
        toel.trigger("change");
				break;
      case 'updatewidget':
        if (message.hasOwnProperty('help')) {
          var po = $(".gridster div#"+message.qaid).data('popover');
          if (po) {po.destroy();}
          $(".gridster div#"+message.qaid).popover(message.help);
        }
        break;
      case 'options':
        if (message.options.addwidget===undefined) message.options.addwidget=true;
        if (message.options.addwidget) {
          $("a.gridsterAction[gridster-action='addwidget']").parent('li').removeClass('disabled');
          $("a.gridsterAction[gridster-action='addwidget']").attr('href','#modalAddWidget');
        } else {
          $("a.gridsterAction[gridster-action='addwidget']").parent('li').addClass('disabled');
          $("a.gridsterAction[gridster-action='addwidget']").attr('href','#');
        }
  			break;
			}
		}
	);

// kann man eventuell zusammenfassen:


$(".gridsterAction").on("click", function(evt) {
  evt.preventDefault();
  var el = $(evt.target);
  var gaction = el.attr("gridster-action");
  var toel = $(".gridsterButton");
  switch( gaction ) {
    case "saveconfig":
    case "updateconfig":
      toel.attr("gridster-action",el.attr("gridster-action"));
      toel.parent().removeClass("open");
      toel.trigger("change");
      break;
    case "deletemode":
      $(".icon-remove-sign").removeClass("hidden");
      el.attr("gridster-action","canceldelete");
      el.html("Delete OFF");
      toel.attr("style","box-shadow: 1px 1px 10px #F00;");
      toel.parent().removeClass("open");
      break;
    case "canceldelete":
      $(".icon-remove-sign").addClass("hidden");
      el.attr("gridster-action","deletemode");
      el.html("Delete ON");
      toel.removeAttr("style");
      toel.parent().removeClass("open");
      break;
  }
});

//$(document).on("click", "i.gridsterAction", function(evt) {

  // evt.target is the button that was clicked
//  var el = $(evt.target);

  // Raise an event to signal that the value changed
  // TODO: But only if not deactivated "gridster-action"=none
//  el.trigger("change");
//});


var gridsterButtonBinding = new Shiny.InputBinding();
$.extend(gridsterButtonBinding, {
  find: function(scope) {
    return $(scope).find(".gridsterButton");
  },
  getValue: function(el) {
    var gaction = $(el).attr("gridster-action");
    switch( gaction ) {
    case "updateconfig":
      var gridster = $(".gridster > ul").gridster().data('gridster');
      var widgetsconfig = {type:"update", widgets:gridster.serialize()}; //TODO
      var wconfjson = JSON.stringify(widgetsconfig);
      $.cookie('gridster',wconfjson, { expires: 7 });
      return wconfjson;
      break;
    case "saveconfig":
      var gridster = $(".gridster > ul").gridster().data('gridster');
      var widgetsconfig = {type:"save", widgets:gridster.serialize()}; //TODO
      var wconfjson = JSON.stringify(widgetsconfig);
      $.cookie('gridster',wconfjson, { expires: 7 });
      return wconfjson;
      break;
    case "sendcookie":
      //$.cookie.json = true;
      var cookieconf = $.cookie('gridster');
      if (cookieconf === undefined) {cookieconf = '[]';}
      return cookieconf;
      break;
    case "deletemode":
      
      //TODO: display delete handle on each gridsteritem and bind this to an 
      // action="deleteitem". Also get hte gridster button, color it red by setting the
      // button's class=gidsterAction ans "gridster-action"="canceldelete""
      break;
    case "deleteitem":
      //TODO: if deltemode is active, then delete that item and cancel deletemode for other items
      // also netralize the gridster button (remove class "gridsterAction" and 
      // remove attribute "gridster-action")
      break;
    case "canceldelete":
      // TODO: cancel deletemode for all items and neutralize gridster button
      //return parseInt($(el).text());
      break;
    }
  },
  setValue: function(el, value) {
    $(el).text(value); // TODO
  },
  subscribe: function(el, callback) {
    $(el).on("change.gridsterButtonBinding", function(e) {
      callback();
    });
  },
  unsubscribe: function(el) {
    $(el).off(".gridsterButtonBinding");
  }
});

Shiny.inputBindings.register(gridsterButtonBinding);

});
