purs = purescript/src/Main.purs
scss = style.scss
hs   = svg-generation/app/Main.hs svg-generation/src/HoneyTime.hs

site : site/style.css site/index.js site/index.html

run : site
	python3 -m http.server -d site

site/style.css : $(scss)
	mkdir -p site
	sass style.scss site/style.css

site/index.js : $(purs)
	mkdir -p site
	pushd purescript && \
	spago build --purs-args '--censor-lib' && \
	spago bundle-app --main Main --to ../site/index.js && \
	popd

svg-generation/output/clock.svg : $(hs)
	pushd svg-generation && \
	mkdir -p output && \
	stack run > output/clock.svg && \
	popd

site/index.html : svg-generation/output/clock.svg template.html
	mkdir -p site
	python3 assemble_page.py template.html > site/index.html

clean :
	rm -rf site

.PHONY : clean
