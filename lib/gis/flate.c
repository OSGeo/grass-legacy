/*
 * $Id$
 *
 ****************************************************************************
 * 	               -- GRASS Development Team --
 *
 * MODULE: 	GRASS gis library
 * FILENAME:	flate.c
 * AUTHOR(S):	Eric G. Miller <egm2@jps.net>
 * PURPOSE: 	To provide an interface to libz for compressing and 
 *              decompressing data using DEFLATE.  It's primary use is in
 *              the storage and reading of GRASS floating point rasters.
 *              It replaces the patented LZW compression interface.
 * DATE CREATED: Nov 19 2000
 * COPYRIGHT:  	(C) 2000 by the GRASS Development Team
 *
 *   	    	This program is free software under the GNU General Public
 *   	    	License (version 2 or greater). Read the file COPYING that 
 *   	    	comes with GRASS for details.
 *
 *****************************************************************************/

/********************************************************************
 * int                                                              *
 * G_zlib_read (fd, dst, nbytes)                                    *
 *     int fd, nbytes;                                              *
 *     unsigned char *dst;                                          *
 * ---------------------------------------------------------------- *
 * This is the basic function for reading a compressed chunk of a   *
 * data file.  The file descriptor should be in the proper location *
 * and the 'dst' array should have enough space for the data.       *
 * 'nbytes' is the size of 'dst'.  For best results, 'nbytes'       *
 * should be the exact amount of space needed for the expansion.    *
 * Too large a value of nbytes may cause more data to be expanded   *
 * than is desired.                                                 *
 * Returns: The number of bytes decompressed into dst, or an error. *
 *                                                                  *
 * Errors include:                                                  *
 *        -1  -- Not enough space in dst.  You must make dst larger *
 *               and then call the function again (remembering to   *
 *               reset the file descriptor to it's proper location. *
 *        -2  -- Error Reading or Decompressing data.               *
 *                                                                  *
 * ================================================================ *
 * int                                                              *
 * G_zlib_write (fd, src, nbytes)                                   *
 *     int fd, nbytes;                                              *
 *     unsigned char *src;                                          *
 * ---------------------------------------------------------------- *
 * This is the basic function for writing and compressing a data    *
 * chunk to a file.  The file descriptor should be in the correct   *
 * location prior to this call. The function will compress 'nbytes' *
 * of 'src' and write it to the file 'fd'.  Returns the number of   *
 * bytes written or an error code:                                  *
 *                                                                  *
 * Errors include:                                                  *
 *        -1 -- Compression Failed.                                 *
 *        -2 -- Unable to write to file.                            *
 *                                                                  *
 * ================================================================ *
 * int                                                              *
 * G_zlib_compress (src, srz_sz, dst, dst_sz)                       *
 *     int src_sz, dst_sz;                                          *
 *     unsigned char *src, *dst;                                    *
 * ---------------------------------------------------------------- *
 * This function is a wrapper around the zlib deflate() function.   *
 * It uses an all or nothing call to deflate().  If you need a      *
 * continuous compression scheme, you'll have to code your own.     *
 * In order to do a single pass compression, the input src must be  *
 * copied to a buffer 1% + 12 bytes larger than the data.  This may *
 * cause performance degradation.                                   *
 *                                                                  *
 * The function either returns the number of bytes of compressed    *
 * data in dst, or an error code.                                   *
 *                                                                  *
 * Errors include:                                                  *
 *        -1 -- Compression failed.                                 *
 *        -2 -- dst is too small.                                   *
 *                                                                  *
 * ================================================================ *
 * int                                                              *
 * G_zlib_expand (src, src_sz, dst, dst_sz)                         *
 *     int src_sz, dst_sz;                                          *
 *     unsigned char *src, *dst;                                    *
 * ---------------------------------------------------------------- *
 * This function is a wrapper around the zlib inflate() function.   *
 * It uses a single pass call to inflate().  If you need a contin-  *
 * uous expansion scheme, you'll have to code your own.             *
 *                                                                  *
 * The function returns the number of bytes expanded into 'dst' or  *
 * and error code.                                                  *
 *                                                                  *
 * Errors include:                                                  *
 *        -1 -- Expansion failed.                                   *
 *        -2 -- dst is too small.                                   *
 *                                                                  *
 ********************************************************************
 */

#include "config.h"

#ifndef HAVE_ZLIB_H

static void
break_compile (void)
{
    NULL = 1;
}

#else

#include <zlib.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "gis.h"

static void
_init_zstruct (z)
    z_stream *z;
{
    /* The types are defined in zlib.h, we set to NULL so zlib uses
     * its default functions 
     */
    z->zalloc = (alloc_func)0;
    z->zfree  = (free_func)0;
    z->opaque = (voidpf)0;
}

int
G_zlib_read (fd, dst, nbytes)
    int fd, nbytes;
    unsigned char *dst;
{
    int bsize, nread, err;
    unsigned char *b;
    z_stream c_stream;

    if (dst == NULL || nbytes < 0)
        return -2;
    
    /* Our temporary input buffer for read */
    if (NULL == (b = (unsigned char *) 
                G_calloc (nbytes, sizeof(unsigned char))))
        return -1;

    /* Use default functions from library */
    _init_zstruct(&c_stream);
    
    /* Read from the file until we get our nbytes or an error */
    bsize = 0;
    do {
        nread = read (fd, b + bsize, nbytes - bsize);
        if (nread >= 0)
            bsize += nread;
    } while (nread > 0 && bsize < nbytes);

    /* If the bsize if less than nbytes and we didn't get an error.. */
    if (bsize < nbytes && nread > 0)
    {
        free (b);
        return -1;
    }

    /* Set the z_stream data */
    c_stream.avail_in = bsize;
    c_stream.next_in = b;
    c_stream.avail_out = nbytes;
    c_stream.next_out = dst;

    /* Initialize the z_stream */
    err = inflateInit (&c_stream);
    switch (err)
    {
        case Z_OK:
            break;
        case Z_MEM_ERROR:
        case Z_VERSION_ERROR:
            free (b);
            return -2;
            break;
        default:
            free (b);
            return -2;
            break;
    }

    /* Do a one pass inflation() */
    err = inflate (&c_stream, Z_FINISH);
    switch (err)
    {
        case Z_STREAM_END:
            break; /* All input was consumed */
        case Z_OK:
            break; /* Not all input was consumed, okay we read extra */
        default:
            free (b);
            inflateEnd (&c_stream);
            return -2;
            break;
    }

    /* Calculate the number of bytes actually expanded */
    nread = nbytes - c_stream.avail_out;

    /* Clean up */
    free (b);
    inflateEnd (&c_stream);

    return nread;
} /* G_zlib_read() */


int
G_zlib_write (fd, src, nbytes)
    int fd, nbytes;
    unsigned char *src;
{
    int bsize, nwritten, err;
    unsigned char *b;
    z_stream c_stream;
    
    /* For single pass deflate, dst size must be 1% + 12bytes bigger */
    bsize = (int) nbytes * 1.01 + 32;
    if (NULL == (b = (unsigned char *) 
                G_calloc (bsize, sizeof (unsigned char))))
        return -1;

    /* Setup up default z_stream funcs */
    _init_zstruct (&c_stream);

    /* Set up z_stream data */
    c_stream.avail_in  = nbytes;
    c_stream.next_in   = src;
    c_stream.avail_out = bsize;
    c_stream.next_out  = b;

    /* Initialize */
    err = deflateInit (&c_stream, Z_DEFAULT_COMPRESSION);
    if (err != Z_OK)
    {
        free (b);
        return -1;
    }

    /* do compression */
    err = deflate (&c_stream, Z_FINISH);
    if (err != Z_STREAM_END)
    {
        deflateEnd (&c_stream);
        free (b);
        return -1;
    }
    
    /* Write to the "file" (could be a socket ...) */
    nwritten = 0;
    do
    {
        err = write (fd, b + nwritten, bsize - c_stream.avail_out - nwritten);
        if (err >= 0)
            nwritten += err;
    } while (err >= 0 && nwritten < bsize - c_stream.avail_out);

    /* If we didn't write all the data return an error */
    if (nwritten != bsize - c_stream.avail_out)
    {
        deflateEnd (&c_stream);
        free (b);
        return -2;
    }

    /* Clean up */
    deflateEnd (&c_stream);
    free (b);

    return nwritten;
} /* G_zlib_write() */


int
G_zlib_compress (src, src_sz, dst, dst_sz)
    int src_sz, dst_sz;
    unsigned char *src, *dst;
{
    /* TODO */
    return -1;
}

int
G_zlib_expand (src, src_sz, dst, dst_sz)
    int src_sz, dst_sz;
    unsigned char *src, *dst;
{
    /* TODO */
    return -1;
}
#endif /* HAVE_ZLIB_H */



/* vim: set softtabstop=4 shiftwidth=4 expandtab: */
