$(function(){ //document ready
              
$('body').on('click', 'div[.modal-footer] .action-button', function ( e ) {        
  var el = $(this).parents('.modal')[0];
  $(el).modal('hide');
});

});