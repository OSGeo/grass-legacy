
#include <stdio.h>

#define MAIN

main(argc, argv)
    int argc;
    char **argv;
{
    if (argc != 6)
    {
	fprintf(stderr, "Usage: %s   digitizer_driver_name  digitizer_tty  file_name  lock_name  lock_pid\n", argv[0]);
	exit(-1);
    }

    geo_reg_main( argv[1], argv[2], argv[3], argv[4], argv[5]) ;

}

