$(function(){ //DOM Ready
 
  // Initialize any divs with class gridster
  // $(".gridster ul").each(function() {
    // $el = $(this);

    // var marginx = Number(this.getAttribute('data-marginx')) || 10;
    // var marginy = Number(this.getAttribute('data-marginy')) || 10;
    // var width   = Number(this.getAttribute('data-width'))   || 140;
    // var height  = Number(this.getAttribute('data-height'))  || 140;

    // $(this).gridster({
      // widget_margins: [marginx, marginy],
      // widget_base_dimensions: [width, height]
    // });
  // });
  
  Shiny.addCustomMessageHandler("GridsterMessage",
  	function(message) {		
			switch(message.msgname) {
			case 'addWidget': 
           var gridster = $(".gridster ul").gridster().data('gridster');
           gridster.add_widget( message.html, message. size_x, message.size_y, 1, 1 );
           break;
			}
		}
	);
});