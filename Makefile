NAME := unitparty
DIST := dist
DISTDIRS = $(addprefix $(DIST)/, js css)
LEINTARG = target/cljs/unitparty.js

HTML = $(DIST)/$(NAME).html
JS   = $(DIST)/js/$(NAME).js
CSS  = $(DIST)/css/$(NAME).css

dist: $(DISTDIRS) $(HTML) $(JS) $(CSS)

clean:
	@rm -rf $(DIST)

$(DISTDIRS):
	@mkdir -p $@

$(HTML): src/haml/$(NAME).haml
	@echo "Compiling Haml."
	@haml $< > $@

test:
	@lein cljsbuild test

$(CSS): src/css/$(NAME).css
	@cp $< $@

$(LEINTARG):
	@lein cljsbuild once prod

$(JS): $(LEINTARG)
	@cp $< $@

.PHONY: dist clean

