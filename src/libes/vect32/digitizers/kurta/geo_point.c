
#include <stdio.h>

#define MAIN

int 
main (int argc, char **argv)
{
    if (argc != 3)
    {
	fprintf(stderr, "Usage: %s control_file_name point_file\n", argv[0]);
	exit(-1);
    }

    geo_point_main( argv[1], argv[2]) ;

}

