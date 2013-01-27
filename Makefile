.PHONY: all clean depend install

all:
	obuild build

configure:
	obuild configure --enable-tests

test: all
	dist/build/test-css/test-css lib_tests/css/bootstrap.css
	dist/build/test-md/test-md lib_tests/md

clean:
	obuild clean
