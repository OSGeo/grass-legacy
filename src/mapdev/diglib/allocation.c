#include "digit.h"

/*  functions - alloc_space(), falloc(), frealloc() _falloc() _frealloc() */


/*   alloc_space ()    allocates space if needed.
*   	All allocated space is created by calloc (2).
*
*   args: number of elements wanted, pointer to number of currently allocated
*   elements, size of chunks to allocate,  pointer to current array, sizeof
*   a element.
*/

char *
dig_alloc_space(n_wanted, n_elements, chunk_size, ptr, element_size)
    int   n_wanted;
    int   *n_elements;
    int   chunk_size;
    char  *ptr;
    int   element_size;
{
    char *p;
    char *dig__alloc_space() ;

    p = dig__alloc_space(n_wanted, n_elements, chunk_size, ptr, element_size) ;

    if (p == NULL)
    {
	fprintf(stderr, "\nERROR: out of memory.  memory asked for: %d\n",
	    n_wanted) ;
	exit(-1) ;
    }

    return (p) ;
}

char *
dig__alloc_space(n_wanted, n_elements, chunk_size, ptr, element_size)
    int   n_wanted;
    int   *n_elements;
    int   chunk_size;
    char  *ptr;
    int   element_size;
{
    int to_alloc ;

    char *calloc() ;
    char *dig_frealloc() ;

    to_alloc = *n_elements ;

  /*  do we need to allocate more space  */
    if (n_wanted < to_alloc)
	return(ptr);
    
  /*  calculate the number needed by chunk size */
    /*  ORIGINAL
    while (n_wanted >= to_alloc)
	to_alloc += chunk_size;
    */
    /*
    **  This was changed as a test on Aug 21, 1990
    **  Build.vect was taking outrageous amounts of
    **  memory to run, so instead of blaming my
    **  code, I decided that it could be the realloc/malloc
    **  stuff not making efficient use of the space.
    **  So the fix is to instead of asking for many small
    **  increments, ask for twice as much space as we are currently
    **  using, each time we need more space.
    */
    while (n_wanted >= to_alloc)
	to_alloc += *n_elements ? *n_elements : chunk_size;

  /*  first time called allocate initial storage  */
    if (*n_elements == 0)
	ptr = calloc( (unsigned)to_alloc, (unsigned)element_size);
    else
	ptr = dig__frealloc( (char *)ptr, to_alloc, element_size, *n_elements);

    *n_elements = to_alloc;

    return(ptr);
}


char *
dig_falloc (nelem, elsize)
    int nelem, elsize;
{
    char *ret;
    if ((ret = dig__falloc (nelem, elsize)) == NULL)
    {
    fprintf (stderr, "Out of Memory.\n");
    sleep (2);
    exit (-1);
    }
    return (ret);
}

char *
dig_frealloc(oldptr, nelem, elsize, oldnelem)
    char *oldptr;
    int nelem, elsize;
    int oldnelem;
{
    char *ret;

    if ((ret = dig__frealloc(oldptr, nelem, elsize, oldnelem)) == NULL)
    {
	fprintf (stderr, "\nOut of Memory on realloc.\n");
	sleep (2);
	exit (-1);
    }
    return (ret);
}

/*  these functions don't exit on "no more memory",  calling funtion should
check the return value  */

char *
dig__falloc(nelem, elsize)
    int nelem, elsize;
{
    char *calloc();
    char *ptr;

    if (elsize == 0) {
        elsize = 4;
    }
    if (nelem == 0) {
        nelem = 1;
    }

    ptr = calloc( (unsigned)nelem, (unsigned)elsize);
    return(ptr);
}

char *
dig__frealloc(oldptr, nelem, elsize, oldnelem)
    char *oldptr;
    int nelem, elsize;
    int oldnelem;
{
    char *calloc();
    char *ptr;

    if (elsize == 0) {
        elsize = 4;
    }
    if (nelem == 0) {
        nelem = 1;
    }

    ptr = calloc((unsigned)nelem, (unsigned)elsize);

    /*  out of memory  */
    if (!ptr)
	return(ptr);

    {
	register char *a;
	register char *b;
	register long n;

	n = oldnelem * elsize;
	a = ptr;
	b = oldptr;
	while(n--)
	    *a++ = *b++;
    }

    free(oldptr);
    return(ptr);
}
