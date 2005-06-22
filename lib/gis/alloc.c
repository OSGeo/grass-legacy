#include <stdlib.h>
#include "gis.h"


/*!
 * \brief memory allocation
 *
 * Allocates a block of
 * memory at least <b>n</b> bytes which is aligned properly for all data
 * types. A pointer to the aligned block is returned.
 * Dies with error message on memory allocation fail.
 *
 *  \param size
 *  \return void * 
 */

void *G_malloc (size_t n)
{
    void *buf;

    if (n <= 0) n = 1;	/* make sure we get a valid request */

    buf = malloc(n);
    if(buf) return buf;

    G_fatal_error ("G_malloc: out of memory");
    return NULL;
}


/*!
 * \brief memory allocation
 *
 * Allocates a
 * properly aligned block of memory <b>n</b>*<b>m</b> bytes in length,
 * initializes the allocated memory to zero, and returns a pointer to the
 * allocated block of memory.
 * Dies with error message on memory allocation fail.
 * <b>Note.</b> Allocating memory for reading and writing raster files is
 * discussed in Allocating_Raster_I_O_Buffers.
 *
 *  \param n number of elements
 *  \param m element size
 *  \return void * 
 */

void *G_calloc (size_t m, size_t n)
{
    void *buf;

    if (m <= 0) m = 1;	/* make sure we get a valid requests */
    if (n <= 0) n = 1;

    buf = calloc(m,n);
    if (buf) return buf;

    G_fatal_error ("G_calloc: out of memory");
    return NULL;
}


/*!
 * \brief memory allocation
 *
 * Changes the
 * <b>size</b> of a previously allocated block of memory at <b>ptr</b> and
 * returns a pointer to the new block of memory. The <b>size</b> may be larger
 * or smaller than the original size. If the original block cannot be extended
 * "in place", then a new block is allocated and the original block copied to the
 * new block.
 * <b>Note.</b> If <b>buf</b> is NULL, then this routine simply allocates a
 * block of <b>n</b> bytes else <b>buf</b> must point to memory that has been dynamically
 * allocated by G_malloc(), G_calloc(), G_realloc(),
 * malloc(3), alloc(3), or realloc(3).. This routine works around broken realloc( )
 * routines, which do not handle a NULL <b>buf</b>.
 *
 *  \param buf buffer holding original data
 *  \param n array size
 *  \return void * 
 */

void *G_realloc (void *buf, size_t n)
{
    if (n <= 0) n = 1;	/* make sure we get a valid request */

    if (!buf) buf = malloc (n);
    else      buf = realloc(buf,n);

    if (buf) return buf;

    G_fatal_error ("G_realloc: out of memory");
    return NULL;
}

/*!
 * \brief free memory allocation
 *
 *  \param buf buffer holding original data
 *  \return void
 */

void G_free(void *buf)
{
	free(buf);
}
