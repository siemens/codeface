// This output binding handles statusOutputBindings
var statusOutputBinding = new Shiny.OutputBinding();
$.extend(statusOutputBinding, {
  find: function(scope) {
    return scope.find('.status_output');
  },
  renderValue: function(el, data) {
    var $el = $(el);
    $el.children('.grid_bigtext').text(data.text || '');
    $el.children('p').text(data.subtext || '');
    
    var $grid = $el.parent('li.gs_w');
    
    // Remove the previously set grid class
    var lastGridClass = $el.data('gridClass');
    if (lastGridClass)
      $grid.removeClass(lastGridClass);
    
    $el.data('gridClass', data.gridClass);
    
    if (data.gridClass) {
      $grid.addClass(data.gridClass);
    }
  }
});
Shiny.outputBindings.register(statusOutputBinding, 'dashboard.statusOutputBinding');
