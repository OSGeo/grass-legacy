#include <stdio.h>

int 
iteration (int n)
{

    if (n > 0 )
    {
	fprintf (stderr,"%4d\b\b\b\b",n);
	fflush (stderr);
    }
    else
	fprintf (stderr,"\n");

  return 0;
}
