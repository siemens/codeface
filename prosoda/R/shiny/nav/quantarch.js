var Quantarch = {		
// depends on siemens.breadcrumb.js		

	subscribeToIframe: function() {
		var iframe = document.getElementById("myFrame");

		// Wird aufgerufen bei einer neuen Nachricht
		// Auf Hauptseite nur um Höhe und Breite anzupassen und um die 
		// Navigation zu erweitern
		function listener(e){

			var eventName = e.data[0],
				data      = e.data[1];

			switch (eventName) {
				case 'setHeight': $(iframe).height(data); break;
				case 'setWidth': $(iframe).width(data); break;
				case 'setNav' : 
					//$('#breadcrumb > section > .nav > nav > h1 > a > b').contents().unwrap();
					$('#breadcrumb h1 b').contents().unwrap();					
					$('#breadcrumb .quantarch').remove();
					$('#breadcrumb > section').append(data);
					$('#breadcrumb h1').filter(':last').children().contents().wrap('<b/>');	
					$('#breadcrumb .quantarch a').attr('target','myFrame');		
					siemens.breadcrumb.init();
					break;
			}

		};

		// Listener für alle Browser
		if (window.addEventListener){
			window.addEventListener("message", listener, false);
		} else {
			window.attachEvent("onmessage", listener);
		}
	},
	
	configureIframe: function() {
		
		var iframe = document.getElementById("myFrame");
		
		iframe.onload = function() {
			
			// Schicke aktuelle URL an Iframe
			iframe.contentWindow.postMessage(['referrer', window.location.href], '*');

			// Style-Eigenschaften an Iframe senden
			iframe.contentWindow.postMessage(['styles', getIframeStyles(iframe)], '*');

		};

		// Get styles and send them to iframe
		function getIframeStyles(iframe) {

			var $span = $('<span/>').appendTo(iframe.parentNode);

			var styles = {
				color:      $span.css('color'),
				fontFamily: $span.css('font-family'),
				fontSize:   $span.css('font-size')
			};

			$span.remove(); // Cleanup
			return styles;

		};
	}
};

// execute on document load
$( document ).ready( Quantarch.subscribeToIframe );
//$( document ).ready( Quantarch.configureIframe );