.PHONY: all clean depend install

all:
	obuild build

configure:
	obuild configure

test: all
	dist/build/test-css/test-css lib_tests/bootstrap.css

clean:
	obuild clean
