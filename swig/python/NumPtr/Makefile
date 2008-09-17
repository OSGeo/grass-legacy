# build NumPtr

all: numptr

numptr:
	python setup.py build --build-lib build/lib
	# build bytecode .pyc file
	python build/lib/test.py

clean:
	-rm -rf build/
