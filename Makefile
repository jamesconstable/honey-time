MKDIR   := @mkdir -p
PYTHON  := python3
SASS    := sass
SPAGO   := spago -q
PSFLAGS := --purs-args '--censor-lib'
STACK   := stack

purs_deps  = $(shell find purescript -not -path '*/output/*' -not -path '*/.spago/*' -name '*.purs' -o -name '*.dhall' -o -name '*.js')
hs_deps    = $(shell find svg-generation -not -path '*/.stack-work/*' -name '*.hs' -o -name '*.yaml' -o -name '*.cabal')
svg_assets = $(shell find svg-generation/assets -name '*.svg')
svg_dir    = svg-generation/output
svg_glyphs = $(svg_dir)/honey-numerals.svg $(svg_dir)/myth-role-icons.svg $(svg_dir)/season-icons.svg $(svg_dir)/honey-letters.svg $(svg_dir)/sun-moon.svg
svgs       = $(svg_dir)/clock-dial.svg $(svg_dir)/date-dial.svg $(svg_dir)/myth-dial.svg $(svg_dir)/background.svg $(svg_dir)/background-mesh.svg

site : site/index.js site/index.html site/style-v2.1.css

run : site
	$(PYTHON) -m http.server -d site

site/index.js : $(purs_deps)
	$(MKDIR) site
	cd purescript && \
	$(SPAGO) build $(PSFLAGS) && \
	$(SPAGO) bundle-app --main Main --to ../$@

site/index.html : template.html assemble_page.py $(svgs) $(svg_glyphs)
	$(MKDIR) site
	$(PYTHON) assemble_page.py $< > $@

$(svgs) : $(hs_deps)
	$(MKDIR) svg-generation/output
	cd svg-generation && $(STACK) run

$(svg_glyphs) : preprocess_svg.py $(svg_assets)
	$(PYTHON) $<

site/style-v2.1.css : style.scss paper.jpeg watercolour.jpeg
	$(MKDIR) site
	$(SASS) $< $@
	cp paper.jpeg site/paper.jpeg
	cp watercolour.jpeg site/watercolour.jpeg

clean :
	rm -rf site purescript/output svg-generation/output
	cd svg-generation && $(STACK) clean

purge: clean
	rm -rf purescript/.spago
	cd svg-generation && $(STACK) purge

.PHONY : clean purge
