#include <stdio.h>

cry_and_die(message) char *message ;
{
        fprintf(stderr,"%s\n", message) ;
        exit(-1) ;
}
