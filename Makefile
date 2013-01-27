.PHONY: all clean depend install

all:
	obuild build

configure:
	obuild configure --enable-tests

test: all
	obuild test

test-verbose:
	dist/build/test-css/test-test-css lib_tests/css/bootstrap.css
	dist/build/test-md/test-test-md lib_tests/md

clean:
	obuild clean
