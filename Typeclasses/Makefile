text.html: text.md header.html
	pandoc --include-in-header=header.html --css=style.css --smart --no-highlight -t html -o $@ $<
	sed -i 's/<code>/<code class="haskell">/g' text.html
