/*  @(#)b_a_dig.c	2.1  6/26/87  */
#include "stdio.h"
#include "dig_head.h"

main(argc, argv)
	int argc ;
	char *argv[] ;
{
	FILE *binary, *ascii, *fopen() ;

	if (argc != 3)
	{
		printf("USAGE: %s old-digit-binary new-digit-ascii\n", argv[0]) ;
		exit(-1) ;
	}

	if ( (binary = fopen(argv[1], "r") ) == NULL )
	{
		printf("Not able to open <%s>\n", argv[1]) ;
		exit(-1) ;
	}

	if ( (ascii = fopen(argv[2], "w") ) == NULL )
	{
		printf("Not able to open <%s>\n", argv[2]) ;
		exit(-1) ;
	}

	dig_read_head_binary(binary, &head) ;

	write_head_ascii(ascii) ;

	bin_to_asc(binary, ascii) ;

	fclose(binary) ;
	fclose(ascii) ;

	exit(0) ;
}
