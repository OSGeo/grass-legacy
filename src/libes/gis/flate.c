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
 *        -1  -- Error Reading or Decompressing data.               *
 *        -2  -- Not enough space in dst.  You must make dst larger *
 *               and then call the function again (remembering to   *
 *               reset the file descriptor to it's proper location. *
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

    if (dst == NULL || nbytes < 0)
        return -2;
    
    /* Our temporary input buffer for read */
    if (NULL == (b = (unsigned char *) 
                G_calloc (nbytes, sizeof(unsigned char))))
        return -1;

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
        G_free (b);
        return -1;
    }

    /* Just call G_zlib_expand() with the buffer we read */
    err = G_zlib_expand (b, bsize, dst, nbytes);

    /* We're done with b */
    G_free (b);
    
    /* Return whatever G_zlib_expand() returned */
    return err;
    
} /* G_zlib_read() */


int
G_zlib_write (fd, src, nbytes)
    int fd, nbytes;
    unsigned char *src;
{
    int dst_sz, nwritten, err;
    unsigned char *dst;
    
    /* Catch errors */
    if (src == NULL || nbytes < 0)
        return -1;
    
    /* set dst_sz equal to input size */
    dst_sz = nbytes;
    if (NULL == (dst = (unsigned char *) 
                G_calloc (dst_sz, sizeof (unsigned char))))
        return -1;

    /* Now just call G_zlib_compress() */
    err = G_zlib_compress (src, nbytes, dst, dst_sz);

    /* If nothing got compressed, return the error, or zero byte count */
    if (err <= 0)
    {
        G_free (dst);
        return err;
    }

    /* set dst_sz to the bytes of compressed data in dst */
    dst_sz = err;
    
    /* Write to the "file" (could be a socket ...) */
    nwritten = 0;
    do
    {
        err = write (fd, dst + nwritten, dst_sz - nwritten);
        if (err >= 0)
            nwritten += err;
    } while (err >= 0 && nwritten < dst_sz);

    /* Done with the dst buffer */
    G_free (dst);
    
    /* If we didn't write all the data return an error */
    if (nwritten != dst_sz)
        return -2;

    return nwritten;
} /* G_zlib_write() */

int
G_zlib_compress (src, src_sz, dst, dst_sz)
    int src_sz, dst_sz;
    unsigned char *src, *dst;
{
    int err, nbytes, buf_sz;
    unsigned char *buf;
    z_stream c_stream;

    /* Catch errors early */
    if (src == NULL || dst == NULL)
        return -1;

    /* Don't do anything if either of these are true */
    if (src_sz <= 0 || dst_sz <= 0)
        return 0;

    /* Input buffer has to be 1% + 12 bytes bigger for single pass deflate */
    buf_sz = (int) ( (double) src_sz * 1.01 + (double) 12 );
    if (NULL == (buf = (unsigned char *)
                        G_calloc (buf_sz, sizeof(unsigned char))))
        return -1;

    /* Copy src_sz bytes from src into buf */
    for (nbytes = 0; nbytes < src_sz; nbytes++)
        buf[nbytes] = src[nbytes];
    
    /* Set-up for default zlib memory handling */
    _init_zstruct (&c_stream);

    /* Set-up the stream */
    c_stream.avail_in  = buf_sz;
    c_stream.next_in   = buf;
    c_stream.avail_out = dst_sz;
    c_stream.next_out  = dst;

    /* Initialize using default compression (usually 6) */
    err = deflateInit (&c_stream, Z_DEFAULT_COMPRESSION);

    /* If there was an error initializing, return -1 */
    if (err != Z_OK)
    {
        G_free (buf);
        return -1;
    }
    
    /* Do single pass compression */
    err = deflate (&c_stream, Z_FINISH);
    if (err != Z_STREAM_END)
    {
        switch (err)
        {
            case Z_OK:  /* Destination too small */
                G_free (buf);
                deflateEnd (&c_stream);
                return -2;
                break;
            default:    /* Give other error */
                G_free (buf);
                deflateEnd (&c_stream);
                return -1;
                break;
        }
    }

    /* avail_out is updated to bytes remaining in dst, so bytes of compressed
     * data is the original size minus that
     */
    nbytes = dst_sz - c_stream.avail_out;
    G_free (buf);
    deflateEnd (&c_stream);

    return nbytes;
} /* G_zlib_compress() */

int
G_zlib_expand (src, src_sz, dst, dst_sz)
    int src_sz, dst_sz;
    unsigned char *src, *dst;
{
    int err, nbytes;
    z_stream c_stream;

    /* Catch error condition */
    if (src == NULL || dst == NULL)
        return -2;

    /* Don't do anything if either of these are true */
    if (src_sz <= 0 || dst_sz <= 0)
        return 0;

    /* Set-up default zlib memory handling */
    _init_zstruct (&c_stream);

    /* Set-up I/O streams */
    c_stream.avail_in  = src_sz;
    c_stream.next_in   = src;
    c_stream.avail_out = dst_sz;
    c_stream.next_out  = dst;

    /* Call zlib initilization function */
    err = inflateInit (&c_stream);

    /* If not Z_OK return error -1 */
    if (err != Z_OK)
        return -1;

    /* Do single pass inflate */
    err = inflate (&c_stream, Z_FINISH);
   
    /* Z_STREAM_END means all input was consumed, 
     * Z_OK means only some was processed (not enough room in dst)
     */
    if (!(err == Z_STREAM_END || err == Z_OK))
    {
        inflateEnd (&c_stream);
        return -1;
    }

    /* Number of bytes inflated to output stream is
     * original bytes available minus what avail_out now says
     */
    nbytes = dst_sz - c_stream.avail_out;
    inflateEnd (&c_stream);

    return nbytes;
} /* G_zlib_expand() */

#endif /* HAVE_ZLIB_H */



/* vim: set softtabstop=4 shiftwidth=4 expandtab: */
