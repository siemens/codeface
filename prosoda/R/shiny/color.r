# This file is part of prosoda.  prosoda is free software: you can
# redistribute it and/or modify it under the terms of the GNU General Public
# License as published by the Free Software Foundation, version 2.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#
# Copyright 2013 by Siemens AG, Johannes Ebke <johannes.ebke.ext@siemens.com>

## These colours are given in the RGB Palette in SIE_Farbfaecher_2011.pdf
color.siemens.petrol <- "#009999"
color.siemens.snow <- "#FFFFFF"
color.siemens.stone <- "#879baa"
color.siemens.stone.35 <- "#becdd7"
color.siemens.sand <- "#aaaa96"
color.siemens.sand.35 <- "#d7d7cd"
color.siemens.yellow.dark <- "#eb780a"
color.siemens.yellow.light <- "#ffb900"
color.siemens.red.dark <- "#641946"
color.siemens.red.light <- "#af235f"
color.siemens.blue.dark <- "#006487"
color.siemens.blue.light <- "#55a0b9"
color.siemens.green.dark <- "#647d2d"
color.siemens.green.light <- "#aab414"
color.siemens.gray.dark <- "#0f1923"
color.siemens.gray.light <- "#505a64"

#color.neutral <- color.siemens.stone.35
color.neutral <- "#ebf0f5" ## Siemens Stone lightest UI color
color.good <- color.siemens.green.light
color.warn <- color.siemens.yellow.light
color.bad <- color.siemens.red.light

## Alternative colours:
## The colours were chosen with the help of the Color scheme designer
## http://colorschemedesigner.com/#3i415w0w0w0w0
## using siemens petrol #009999 as a baseline

#color.list.neutral <- c("#009999", "#1D7373", "#006363", "#33CCCC", "#5CCCCC")
color.list.good    <- c("#00B74A", "#228A4C", "#007730", "#37DB79", "#63DB93")
color.list.warn    <- c("#FF7400", "#BF7130", "#A64B00", "#FF9640", "#FFB273")
color.list.bad     <- c("#FF3100", "#BF4B30", "#A62000", "#FF6440", "#FF8E73")

### Styles: 1 (normal), 2 (dark), 3 (darker), 4 (light), 5 (lighter)
color.style <- 4
#color.neutral <- color.list.neutral[[color.style]]
color.good <- color.list.good[[color.style]]
color.warn <- color.list.warn[[color.style]]
color.bad <- color.list.bad[[color.style]]


