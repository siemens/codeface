$(document).ready( function() {		// Wird aufgerufen bei einer neuen Nachricht
	// function listener(e){

		// var eventName = e.data[0],
			// data      = e.data[1];

		// switch (eventName) {
			// case 'referrer': setNewBase(data); break;
			// case 'styles':   injectStyles(data); break;
		// }

	// }


	// // Listener für alle Browser
	// if (window.addEventListener){
		// window.addEventListener("message", listener, false);
	// } else {
		// window.attachEvent("onmessage", listener);
	// }


	// // Aktualisiere Base-URL
	// function setNewBase(url) {
	// //	document.getElementsByTagName('base')[0].href = url;
	// }

	// // Styles empfangen und auf <body> anwenden
	// function injectStyles(styles) {
		// $(document.body).css(styles);
	// }

	// Resize-Funktion zum Ermitteln der Höhe (getestet, funktioniert)
	// TODO: sicherheit / letzten parameter spezifisch an site anpassen z.B. 'https://swi.siemens.de/'
	function resize() {
		window.parent.postMessage(['setHeight', $(document).height()], '*');
		window.parent.postMessage(['setWidth', $(document).width()], '*');
	}

	// Alle 500 Millisekunden die Höhe des Frames anpassen
	window.setInterval(resize, 500);


	// Handle messages from server - update graph
	Shiny.addCustomMessageHandler("sendToIframeParent",
		function(message) {		
			// var newnav = '<div class="nav quantarch"><nav><h1><a href="#"><b>Level 4</b></a></h1></nav></div>'; 
			//window.parent.postMessage([message.msgname, message.navhtml], '*')
			switch(message.msgname) {
			case 'setNav': window.parent.postMessage(['setNav', message.navhtml], '*'); break;
			}
		}
	);
	
});
