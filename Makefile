NAME := unitparty
DIST := dist
TARG = target/dist
TARGDIRS = $(addprefix $(TARG)/, js css)

HTML = $(TARG)/$(NAME).html
JS   = $(TARG)/js/$(NAME).js
CSS  = $(TARG)/css/$(NAME).css

dist: $(TARGDIRS) $(HTML) $(JS) $(CSS)
	@mv $(TARG) $(DIST)

clean:
	@rm -rf $(DIST)

$(TARGDIRS):
	@mkdir -p $@

$(HTML): src/haml/$(NAME).haml
	@echo "Compiling Haml."
	@haml $< > $@

html: $(HTML)
css: $(CSS)
js: $(JS)

test:
	@lein cljsbuild test

$(CSS): src/scss/$(NAME).scss
	@echo "Compiling SCSS."
	@sass $< $@ > /dev/null

$(JS):
# this is what lein is for
	@lein cljsbuild once prod

.PHONY: dist clean html css js test

