
## Here follow lists of Unicode symbols which are useful as status indicators
##
## Weather-type indicators, in order of severity
## A good listing is at
## http://de.wikipedia.org/wiki/Unicodeblock_Verschiedene_Symbole
symbol.weather.sunny <- intToUtf8(0x2600)
symbol.weather.sun.and.cloud <- intToUtf8(0x26C5)
symbol.weather.cloudy <- intToUtf8(0x2601)
symbol.weather.rainy <- intToUtf8(0x2614)
symbol.weather.snowy <- intToUtf8(0x2603)
symbol.weather.thunderstorm <- intToUtf8(0x2608)
symbol.volcano <-  intToUtf8(0x1F30B)
symbols.weather <- list(
                        good = symbol.weather.sunny,
                        mostly.good = symbol.weather.sun.and.cloud,
                        warn = symbol.weather.cloudy,
                        mostly.bad = symbol.weather.rainy,
                        bad = symbol.weather.thunderstorm,
                        error = symbol.volcano
                        )


## Emoticon-type indicators
## http://en.wikipedia.org/wiki/List_of_emoticons#Unicode_characters
symbol.emotion.happy <- intToUtf8(0x1f60A)
symbol.emotion.neutral <- intToUtf8(0x1f610)
symbol.emotion.confounded <- intToUtf8(0x1f616)
symbol.emotion.disappointed <- intToUtf8(0x1f61E)
symbol.emotion.fear <- intToUtf8(0x1f631)
symbols.emotion <- list(
                        good = symbol.emotion.happy,
                        mostly.good = symbol.emotion.confounded,
                        warn = symbol.emotion.neutral,
                        mostly.bad = symbol.emotion.disappointed,
                        bad = symbol.emotion.disappointed,
                        error = symbol.emotion.fear
                        )


#symbol.abstract.checkmark.white <- intToUtf8(0x2705) # alternative
symbol.abstract.checkmark <- intToUtf8(0x2713) # alternative
#symbol.abstract.checkmark.heavy <- intToUtf8(0x2714)
symbol.abstract.warning <- intToUtf8(0x26A0)
symbol.abstract.cross <- intToUtf8(0x2715) # alternative
symbol.abstract.cross.heavy <- intToUtf8(0x2716)
#symbol.abstract.cross <- intToUtf8(0x274c)
#symbol.abstract.highvoltage <- intToUtf8(0x26A1)
#symbol.abstract.radioactive <- intToUtf8(0x2622)
#symbol.abstract.biohazard <- intToUtf8(0x2623)
symbol.abstract.error <- intToUtf8(0x21af)
symbols.abstract <- list(
                         good = symbol.abstract.checkmark,
                         mostly.good = paste("(", symbol.abstract.checkmark, ")", sep=""),
                         warn = symbol.abstract.warning,
                         mostly.bad = symbol.abstract.cross,
                         bad = symbol.abstract.cross.heavy,
                         error = symbol.abstract.error
                         )



## Most of the following symbols are taken from
## http://de.wikipedia.org/wiki/Unicodeblock_Verschiedene_piktografische_Symbole
symbols.animals.bird <- intToUtf8(0x1f426)
symbols.animals.horse <- intToUtf8(0x1f40e)
symbols.animals.rabbit <- intToUtf8(0x1f430)
symbols.animals.turtle <- intToUtf8(0x1f422)
symbols.animals.snail <- intToUtf8(0x1f40c)
symbols.animals.bug <- intToUtf8(0x1f41b)
symbols.animals <- list(
                        good = symbols.animals.bird,
                        mostly.good = symbols.animals.horse,
                        warn = symbols.animals.rabbit,
                        mostly.bad = symbols.animals.turtle,
                        bad = symbols.animals.snail,
                        error = symbols.animals.bug
                        )


symbols.gestures.ok <- intToUtf8(0x1f44c)
symbols.gestures.thumbsup <- intToUtf8(0x1f44d)
symbols.gestures.hand <- intToUtf8(0x1f44b)
symbols.gestures.thumbsdown <- intToUtf8(0x1f44e)
symbols.gestures.twohands <- intToUtf8(0x1f450)
symbols.gestures <- list(
                        good = symbols.gestures.thumbsup,
                        mostly.good = symbols.gestures.ok,
                        warn = symbols.gestures.hand,
                        mostly.bad = symbols.gestures.thumbsdown,
                        bad = symbols.gestures.thumbsdown,
                        error = symbols.gestures.twohands
                        )


symbols.arrows.upward.thick <- intToUtf8(0x21D7)
symbols.arrows.upward <- intToUtf8(0x2197)
symbols.arrows.right <- intToUtf8(0x219d)
symbols.arrows.downward <- intToUtf8(0x2198)
symbols.arrows.downward.thick <- intToUtf8(0x21D8)
symbols.arrows.error <- intToUtf8(0x21AF)
symbols.arrows = list(
                      good = symbols.arrows.upward.thick,
                      mostly.good = symbols.arrows.upward,
                      warn = symbols.arrows.right,
                      mostly.bad = symbols.arrows.downward,
                      bad = symbols.arrows.downward.thick,
                      error = symbols.arrows.error
                      )

## Symbols for processing steps
symbol.cluster <- intToUtf8(0x260D) # two connected nodes
symbol.commit <- intToUtf8(0x1f4dd) # memo
#symbol.commit <- intToUtf8(0x270d) # Signing hand
symbol.email <- intToUtf8(0x1f4e7)
symbol.analysis <- intToUtf8(0x1F50D) # magnifying glass
symbol.bug <- intToUtf8(0x1F41C) # (actually, an Ant)
symbol.timeseries <- intToUtf8(0x1f4c8) # chart


## Symbols for information groups
symbol.basics <- intToUtf8(0x1F481) # Information desk person
#symbol.basics <- intToUtf8(0x1F4ca) ## alternative: bar chart

symbol.communication <- intToUtf8(0x1f4e1)
#symbol.communication <- intToUtf8(0x1f4ac) ## alternative communication, speech bubble

#symbol.collaboration <- intToUtf8(0x21c4) ## alternative collaboration, arrows
symbol.collaboration <- paste(intToUtf8(0x1f464), intToUtf8(0x1f464), sep="") ## Two persons
#symbol.collaboration <- intToUtf8(0x1f517) ## link symbol
#symbol.complexity <- intToUtf8(0x2102) ## Complex number sign
symbol.complexity <- intToUtf8(0x1f3ef) ## japanese castle
#symbol.construction <- intToUtf8(0x1f3ed) # Factory
symbol.construction <- intToUtf8(0x1f528) # Hammer
#symbol.construction <- intToUtf8(0x1f4d0) # Triangular ruler

## List of other interesting Unicode Symbols which might be useful:
## 2388 -- steering wheel
## 1F3AF -- direct hit
## 1F4C8/9/A -- chart up/down/barchart
## 1F4C8 -- cheering megaphone
