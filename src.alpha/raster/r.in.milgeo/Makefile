read_dhm: main.o dsiread.o accread.o datacompress.o dataread.o atoint.o error.o rotation.o
	cc -listing main.o dsiread.o accread.o datacompress.o dataread.o rotation.o atoint.o error.o -g -o read_dhm

main.o: main.c
	cc -c -g main.c

dsiread.o: dsiread.c
	cc -c -g dsiread.c

accread.o: accread.c
	cc -c -g accread.c

datacompress.o: datacompress.c
	cc -c -g datacompress.c

dataread.o: dataread.c
		cc -c -g dataread.c

rotation.o: rotation.c
	cc -c -g rotation.c

atoint.o: atoint.c
	cc -c -g atoint.c

error.o: error.c
	cc -c -g error.c

