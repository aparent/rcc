DIA = $(addprefix images/, \
			 	basicExample.svg add.svg mult.svg ifExample.svg)

all: $(DIA)

images/%.svg: examples/%.j
	jcc $^ -O -d $@
