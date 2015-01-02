sources = \
	1-intro-to-typeclasses.md \
	2-show-and-read.md \
	3-num-and-derivatives.md \
	4-other-standard-typeclasses.md \
	5-deriving-and-extensions.md

results = $(sources:.md=.html)

.PHONY: all clean

all: $(results)

%.html: %.md header.html
	pandoc --include-in-header=header.html --css=style.css --smart --no-highlight -t html -o $@ $<
	sed -i 's/<code>/<code class="haskell">/g' $@

clean:
	rm -f $(results)
