/**********************************************************************
 *   char *
 *   G_malloc (n)
 *
 *   returns pointer to allocated memory of n bytes
 *
 *   parms:
 *      int n      memory, in bytes, requested
 *
 *   returns:
 *      char *  pointer to memory of n bytes
 *
 *   note:
 *      dies with error message on memory allocation fail
 **********************************************************************
 *   char *
 *   G_calloc (m, n)
 *
 *   returns pointer to allocated memory of sufficient size to hold 
 *   an array of m entries each of size n.
 *
 *   parms:
 *      int n      number of elements
 *      int m      element size
 *
 *   returns:
 *      char *  pointer to memory of n * m bytes
 *
 *   note:
 *      dies with error message on memory allocation fail
 **********************************************************************
 *   char *
 *   G_realloc (buff, n)
 *
 *   returns pointer to reallocated memory which maintains the 
 *   information in the original array buff.  Size of reallocated
 *   memory is n bytes.
 *
 *   parms:
 *      char *buff   buffer holding original data
 *      int n        array size
 *
 *   returns:
 *      char *  pointer to memory of n bytes
 *
 *   note:
 *      if buff is NULL call is converted to G_malloc()
 *      else buff must point to memory that has been dynamically
 *      allocated by G_malloc(), G_calloc(), G_realloc(),
 *      malloc(3), alloc(3), or realloc(3).
 **********************************************************************/

extern char *malloc();
extern char *calloc();
extern char *realloc();

char *
G_malloc (n)
{
    char *buf;

    if (n <= 0) n = 1;	/* make sure we get a valid request */

    if (buf = malloc(n)) return buf;

    G_fatal_error ("G_malloc: out of memory");
    return 0;
}

char *
G_calloc (m,n)
{
    char *buf;

    if (m <= 0) m = 1;	/* make sure we get a valid requests */
    if (n <= 0) n = 1;

    if (buf = calloc(m,n)) return buf;

    G_fatal_error ("G_calloc: out of memory");
    return 0;
}

char *
G_realloc (buf,n)
    char *buf;
{
    if (n <= 0) n = 1;	/* make sure we get a valid request */

    if (!buf) buf = malloc (n);
    else      buf = realloc(buf,n);

    if (buf) return buf;

    G_fatal_error ("G_realloc: out of memory");
    return 0;
}
