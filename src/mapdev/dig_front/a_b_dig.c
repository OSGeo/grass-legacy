
/*  @(#)a_b_dig.c	2.1 6/26/87 */
#include "stdio.h"
#include "dig_head.h"

main(argc, argv)
	int argc ;
	char *argv[] ;
{
	FILE *binary, *ascii, *fopen() ;

	if (argc != 3)
	{
		printf("USAGE: %s new-digit-ascii old-digit-binary\n", argv[0]) ;
		exit(-1) ;
	}

	if ( (ascii = fopen(argv[1], "r") ) == NULL )
	{
		printf("Not able to open <%s>\n", argv[1]) ;
		exit(-1) ;
	}

	if ( (binary = fopen(argv[2], "w") ) == NULL )
	{
		printf("Not able to open <%s>\n", argv[2]) ;
		exit(-1) ;
	}

	if ( read_head_ascii(ascii) < 0)
		perror(" Can't read ascii file header") ;

	dig_write_head_binary(binary, &head) ;

	asc_to_bin(ascii, binary) ;

	fclose(binary) ;
	fclose(ascii) ;

	exit(0) ;
}
