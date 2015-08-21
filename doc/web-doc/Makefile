DIA = $(addprefix images/, \
			 	basicExample.svg add.svg mult.svg)

all: $(DIA)

images/%.svg: examples/%.j
	jcc $^ -O -d $@
