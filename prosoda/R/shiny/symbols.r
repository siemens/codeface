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

## Here follow lists of Unicode symbols which are useful as status indicators
##
## Weather-type indicators, in order of severity
## A good listing is at
## http://de.wikipedia.org/wiki/Unicodeblock_Verschiedene_Symbole

make.symbol <- function(unicode.hex) {
  div(style="font-family: symbola; display: inline; font-weight: 500;", intToUtf8(unicode.hex))
  ## Uncomment this to make the example html file
  #intToUtf8(unicode.hex)
}

symbol.weather.sunny <- make.symbol(0x1f323)
symbol.weather.sun.and.cloud <- make.symbol(0x1f324)
symbol.weather.cloudy <- make.symbol(0x1f325)
symbol.weather.rainy <- make.symbol(0x1f327)
symbol.weather.snowy <- make.symbol(0x1f328)
symbol.weather.thunderstorm <- make.symbol(0x26c8)
symbol.volcano <-  make.symbol(0x1F30B)
symbols.weather <- list(
                        good = symbol.weather.sunny,
                        mostly.good = symbol.weather.sun.and.cloud,
                        warn = symbol.weather.cloudy,
                        mostly.bad = symbol.weather.rainy,
                        bad = symbol.weather.thunderstorm,
                        error = "?"
                        )


## Emoticon-type indicators
## http://en.wikipedia.org/wiki/List_of_emoticons#Unicode_characters
symbol.emotion.very.happy <- make.symbol(0x1f603)
symbol.emotion.happy <- make.symbol(0x1f642)
symbol.emotion.neutral <- make.symbol(0x1f610)
symbol.emotion.disappointed <- make.symbol(0x1f641)
symbol.emotion.very.disappointed <- make.symbol(0x1f626)
symbols.emotion <- list(
                        good = symbol.emotion.very.happy,
                        mostly.good = symbol.emotion.happy,
                        warn = symbol.emotion.neutral,
                        mostly.bad = symbol.emotion.disappointed,
                        bad = symbol.emotion.very.disappointed,
                        error = "?"
                        )


#symbol.abstract.checkmark.white <- make.symbol(0x2705) # alternative
symbol.abstract.checkmark <- make.symbol(0x2713) # alternative
#symbol.abstract.checkmark.heavy <- make.symbol(0x2714)
symbol.abstract.warning <- make.symbol(0x26A0)
symbol.abstract.cross <- make.symbol(0x2715) # alternative
symbol.abstract.cross.heavy <- make.symbol(0x2716)
#symbol.abstract.cross <- make.symbol(0x274c)
#symbol.abstract.highvoltage <- make.symbol(0x26A1)
#symbol.abstract.radioactive <- make.symbol(0x2622)
#symbol.abstract.biohazard <- make.symbol(0x2623)
symbol.abstract.error <- make.symbol(0x21af)
symbols.abstract <- list(
                         good = symbol.abstract.checkmark,
                         mostly.good = paste("(", symbol.abstract.checkmark, ")", sep=""),
                         warn = symbol.abstract.warning,
                         mostly.bad = symbol.abstract.cross,
                         bad = symbol.abstract.cross.heavy,
                         error = "?"
                         )



## Most of the following symbols are taken from
## http://de.wikipedia.org/wiki/Unicodeblock_Verschiedene_piktografische_Symbole
symbols.animals.leopard <- make.symbol(0x1f406)
symbols.animals.bird <- make.symbol(0x1f426)
symbols.animals.horse <- make.symbol(0x1f40e)
symbols.animals.rabbit <- make.symbol(0x1f407)
symbols.animals.turtle <- make.symbol(0x1f422)
symbols.animals.snail <- make.symbol(0x1f40c)
symbols.animals.bug <- make.symbol(0x1f41b)
symbols.animals <- list(
                        good = symbols.animals.leopard,
                        mostly.good = symbols.animals.horse,
                        warn = symbols.animals.rabbit,
                        mostly.bad = symbols.animals.turtle,
                        bad = symbols.animals.snail,
                        error = symbols.animals.bug
                        )


symbols.gestures.ok <- make.symbol(0x1f44c)
symbols.gestures.thumbsup <- make.symbol(0x1f44d)
symbols.gestures.hand <- make.symbol(0x1f44b)
symbols.gestures.thumbsdown <- make.symbol(0x1f44e)
symbols.gestures.twohands <- make.symbol(0x1f450)
symbols.gestures <- list(
                        good = symbols.gestures.thumbsup,
                        mostly.good = symbols.gestures.ok,
                        warn = symbols.gestures.hand,
                        mostly.bad = symbols.gestures.thumbsdown,
                        bad = symbols.gestures.thumbsdown,
                        error = "?"
                        )


symbols.arrows.up.thick <- make.symbol(0x21D1)
symbols.arrows.upward.thick <- make.symbol(0x21D7)
symbols.arrows.upward <- make.symbol(0x2197)
symbols.arrows.right <- make.symbol(0x219d)
symbols.arrows.downward <- make.symbol(0x2198)
symbols.arrows.downward.thick <- make.symbol(0x21D8)
symbols.arrows.error <- make.symbol(0x21AF)
symbols.arrows = list(
                      good = symbols.arrows.upward.thick,
                      mostly.good = symbols.arrows.upward,
                      warn = symbols.arrows.right,
                      mostly.bad = symbols.arrows.downward,
                      bad = symbols.arrows.downward.thick,
                      error = "?"
                      )

symbols.arrows.up.is.bad = list(
                      good = symbols.arrows.downward,
                      mostly.good = symbols.arrows.right,
                      warn = symbols.arrows.upward,
                      mostly.bad = symbols.arrows.upward.thick,
                      bad = symbols.arrows.up.thick,
                      error = "?"
                      )

## Symbols for processing steps
symbol.cluster <- make.symbol(0x260D) # two connected nodes
symbol.commit <- make.symbol(0x1f4dd) # memo
#symbol.commit <- make.symbol(0x270d) # Signing hand
#symbol.email <- make.symbol(0x1f4e7)
symbol.email <- make.symbol(0x1f4e8)
symbol.analysis <- make.symbol(0x1F50D) # magnifying glass
symbol.bug <- make.symbol(0x1F41C) # (actually, an Ant)
symbol.timeseries <- make.symbol(0x1f4c8) # chart


## Symbols for information groups
symbol.basics <- make.symbol(0x1F481) # Information desk person
#symbol.basics <- make.symbol(0x1F4ca) ## alternative: bar chart

#symbol.communication <- make.symbol(0x1f4e1)
#symbol.communication <- make.symbol(0x1f4ac) ## alternative communication, speech bubble
symbol.communication <- make.symbol(0x1f5eb) ## alternative communication, speech bubble

#symbol.collaboration <- make.symbol(0x21c4) ## alternative collaboration, arrows
symbol.collaboration <- make.symbol(0x1f465) ## Two persons
#symbol.collaboration <- make.symbol(0x1f517) ## link symbol
#symbol.complexity <- make.symbol(0x2102) ## Complex number sign
symbol.complexity <- make.symbol(0x1f3ef) ## japanese castle
#symbol.construction <- make.symbol(0x1f3ed) # Factory
symbol.construction <- make.symbol(0x1f528) # Hammer
#symbol.construction <- make.symbol(0x1f4d0) # Triangular ruler

## List of other interesting Unicode Symbols which might be useful:
## 2388 -- steering wheel
## 1F3AF -- direct hit
## 1F4C8/9/A -- chart up/down/barchart
## 1F4C8 -- cheering megaphone


symbol.test.file <- function(filename) {
  fileConn <- file(filename)
  writeLines(c('<html><head>',
               '<link rel="stylesheet" type="text/css" href="styles.css"/>',
               '</head><body style="font-size: 100px; font-family: symbola;">',
               unlist(symbols.weather),
               "<br>",
               unlist(symbols.emotion),
               "<br>",
               unlist(symbols.abstract),
               "<br>",
               unlist(symbols.animals),
               "<br>",
               unlist(symbols.gestures),
               "<br>",
               unlist(symbols.arrows),
               "<br>",
               symbol.cluster, symbol.commit, symbol.email, symbol.analysis,
               symbol.bug, symbol.timeseries,
               "<br>",
               symbol.basics,
               symbol.communication, symbol.collaboration, symbol.complexity,
               symbol.construction,
               "</body>"), fileConn)
  close(fileConn)
}

