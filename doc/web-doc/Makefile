DIA = $(patsubst examples/%.j, images/%.svg, $(wildcard examples/*.j))

.PHONY: build rebuild

all: build

build: $(DIA)
	jcc-web-doc build

rebuild: $(DIA)
	jcc-web-doc rebuild

images/%.svg: examples/%.j
	jcc $^ -O -d $@
