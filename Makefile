.PHONY : all clean

all :
	make -C src
	make clean

clean :
	make clean -C src
