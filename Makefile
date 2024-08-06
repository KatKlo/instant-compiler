.PHONY : all clean

all :
	make -C src
	make cleanDist

clean :
	make clean -C src

cleanDist :
	make cleanDist -C src
