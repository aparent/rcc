DIA = $(addprefix images/, \
			 	basic_example.svg)

all: $(DIA)

images/%.svg: examples/%.j
	jcc $^ -O -d $@
