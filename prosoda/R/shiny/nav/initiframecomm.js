$(document).ready( function() {		// Called when a new message arrives
	// function listener(e){

		// var eventName = e.data[0],
			// data      = e.data[1];

		// switch (eventName) {
			// case 'referrer': setNewBase(data); break;
			// case 'styles':   injectStyles(data); break;
		// }

	// }


	// // Listener for all browsers
	// if (window.addEventListener){
		// window.addEventListener("message", listener, false);
	// } else {
		// window.attachEvent("onmessage", listener);
	// }


	// // Bring base URL up to date
	// function setNewBase(url) {
	// //	document.getElementsByTagName('base')[0].href = url;
	// }

	// // Receive style and apply on <body>
	// function injectStyles(styles) {
		// $(document.body).css(styles);
	// }

	// Resize function to determine height (tested, works)
	// TODO: sicherheit / letzten parameter spezifisch an site anpassen z.B. 'https://swi.siemens.de/'
	function resize() {
		window.parent.postMessage(['setHeight', $(document).height()], '*');
		window.parent.postMessage(['setWidth', $(document).width()], '*');
	}

	// Adapt height of frame eveyr 500 ms
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
