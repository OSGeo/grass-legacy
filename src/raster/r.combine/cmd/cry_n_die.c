#include <stdio.h>

int 
cry_and_die (char *message)
{
        fprintf(stderr,"%s\n", message) ;
        exit(-1) ;
}
