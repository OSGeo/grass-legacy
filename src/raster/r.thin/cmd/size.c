#include <stdio.h>

int 
main (void)
{
	fprintf(stdout,"size of char   = %d\n",sizeof(char));
	fprintf(stdout,"size of int    = %d\n",sizeof(int));
	fprintf(stdout,"size of long   = %d\n",sizeof(long));
	fprintf(stdout,"size of char * = %d\n",sizeof(char *));
	fprintf(stdout,"size of int  * = %d\n",sizeof(int *));
	fprintf(stdout,"size of long * = %d\n",sizeof(long *));
}
