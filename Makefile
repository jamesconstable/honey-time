MKDIR   := @mkdir -p
PYTHON  := python3
SASS    := sass
SPAGO   := spago -q
PSFLAGS := --purs-args '--censor-lib'
STACK   := stack

purs_deps  = $(shell find purescript -not -path '*/output/*' -not -path '*/.spago/*' -name '*.purs' -o -name '*.dhall')
hs_deps    = $(shell find svg-generation -not -path '*/.stack-work/*' -name '*.hs' -o -name '*.yaml' -o -name '*.cabal')
svg_assets = $(shell find svg-generation/assets -name '*.svg')
svg_dir    = svg-generation/output
svgs       = $(svg_dir)/clock-dial.svg $(svg_dir)/date-dial.svg $(svg_dir)/myth-dial.svg $(svg_assets)

site : site/index.js site/index.html site/style.css

run : site
	$(PYTHON) -m http.server -d site

site/index.js : $(purs_deps)
	$(MKDIR) site
	cd purescript && \
	$(SPAGO) build $(PSFLAGS) && \
	$(SPAGO) bundle-app --main Main --to ../$@

site/index.html : template.html assemble_page.py $(svgs)
	$(MKDIR) site
	$(PYTHON) assemble_page.py $< > $@

$(svgs) : $(hs_deps)
	$(MKDIR) svg-generation/output
	cd svg-generation && $(STACK) run

site/style.css : style.scss
	$(MKDIR) site
	$(SASS) $< $@

clean :
	rm -rf site purescript/output svg-generation/output
	cd svg-generation && $(STACK) clean

purge: clean
	rm -rf purescript/.spago
	cd svg-generation && $(STACK) purge

.PHONY : clean purge
