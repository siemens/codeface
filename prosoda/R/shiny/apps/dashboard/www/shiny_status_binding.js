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

// This output binding handles statusOutputBindings
var statusOutputBinding = new Shiny.OutputBinding();
$.extend(statusOutputBinding, {
  find: function(scope) {
    return scope.find('.status_output');
  },
  renderValue: function(el, data) {
    var $el = $(el);
    $el.children('.grid_inserttext').text(data.text || '');
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
