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
var justgageOutputBinding = new Shiny.OutputBinding();
$.extend(justgageOutputBinding, {
  find: function(scope) {
    return scope.find('.justgage_output');
  },
  renderValue: function(el, data) {
    if (!$(el).data('gauge')) {
      // If we haven't initialized this gauge yet, do it
      $(el).data('gauge', new JustGage({
        id: this.getId(el),
        value: 0,
        min: $(el).data("min"),
        max: $(el).data("max"),
        title: $(el).data("title"),
        label: $(el).data("units")
      }));
    }
    $(el).data('gauge').refresh(data);
  }
});
Shiny.outputBindings.register(justgageOutputBinding, 'dashboard.justgageOutputBinding');
