/**************************************************************
 *  routines to create a sorted list of random numbers
 *  NOTE: STORAGE MUST BE ALLOCATED IN THE CALLING ROUTINE
 *  Calling sequence is:
 *        create_rand(base, targets, count)
 *        int *base;    pointer to enough space to store n targets
 *        int targets;  number of sorted randoms to generate
 *        long count;   generated randoms will be in range [1,count]
 *  Duplicate values are discarded and replaced
 *  Random number seed is the current process id
 *************************************************************/
#include <math.h>

static
cmp(n1,n2)		/* functon to compare two integers */
int *n1, *n2;		/*   called by qsort */
{
    return (*n1 - *n2);
}

create_rand(base, targets, count)
    int *base;
    long targets;
    long count;
{
    int repeat;
    long i ;
    int *pix;

    srand(getpid());
    pix = base;
    i = 0;
    for (i=0; i < targets; i++)
	*pix++  = make_rand() % count + 1;  /* get the numbers */

    repeat=1;
    while (repeat)
    {
	repeat = 0;
	qsort (base, targets, sizeof(int), cmp);

	/* check for duplicates */

	for (pix = base, i=0; i < targets-1; i++, pix++)
	{
	    if (*pix == *(pix+1) )
	    {
		*pix = make_rand() % count + 1;
		repeat = 1;
	    }
	}
    }
}

/* rand() itself doesn't generate large enough numbers */
/* call it twice and "add" them up */
make_rand()
{
    return abs(rand() + (rand() << 16));
}

static
abs(n)
{
    return n < 0 ? (-n) : n;
}
