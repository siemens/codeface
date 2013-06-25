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
// Copyright Josh Holbrook jesusabdullah on Friday, Aug 26 2011
// http://docs.nodejitsu.com/articles/intermediate/how-to-log

var logger = exports;
  logger.debugLevel = 'warn';
  logger.log = function(level, message) {
    var levels = ['error', 'warn', 'info'];
    if (levels.indexOf(level) <= levels.indexOf(logger.debugLevel) ) {
      if (typeof message !== 'string') {
        message = JSON.stringify(message);
      };
      console.log(level+': '+message);
    }
  }