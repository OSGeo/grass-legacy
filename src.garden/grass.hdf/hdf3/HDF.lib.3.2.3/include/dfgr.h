/***************************************************************************
*
*
*                         NCSA HDF version 3.2r3
*                            December 1, 1992
*
* NCSA HDF Version 3.2 source code and documentation are in the public
* domain.  Specifically, we give to the public domain all rights for future
* licensing of the source code, all resale rights, and all publishing rights.
*
* We ask, but do not require, that the following message be included in all
* derived works:
*
* Portions developed at the National Center for Supercomputing Applications at
* the University of Illinois at Urbana-Champaign, in collaboration with the
* Information Technology Institute of Singapore.
*
* THE UNIVERSITY OF ILLINOIS GIVES NO WARRANTY, EXPRESSED OR IMPLIED, FOR THE
* SOFTWARE AND/OR DOCUMENTATION PROVIDED, INCLUDING, WITHOUT LIMITATION,
* WARRANTY OF MERCHANTABILITY AND WARRANTY OF FITNESS FOR A PARTICULAR PURPOSE
*
****************************************************************************
*/
/*
$Header$

$Log$
Revision 1.2  2000-02-04 08:23:05  markus
updated

Revision 1.1.1.1  1999/12/29 15:12:35  markus
initial CVS import

 * Revision 1.3  1992/10/23  00:14:11  koziol
 * Changed all DFIstr*() and DFImem*() calls to HDstr*() and HDmem*() calls
 * #ifdef'd out the macros Jason defined for Hopen, Hclose, etc. for Vsets
 * Replaced Vset VFREESPACE and VGETSPACE calls with actual calls to HDfreespace
 * and HDgetspace
 * Added a MS-Windows lower lower for file I/O (which may not be completely working
 *
 * Revision 1.1  1992/08/25  21:40:44  koziol
 * Initial revision
 *
*/
/*-----------------------------------------------------------------------------
 * File:    dfgr.h
 * Purpose: header file for the Raster Image set
 * Invokes: df.h
 * Contents:
 *  Structure definitions: DFGRdr, DFGRrig
 * Remarks: This is included with user programs which use general raster
 *---------------------------------------------------------------------------*/


#ifndef DFGR_H                      /* avoid re-inclusion */
#define DFGR_H

#include "hdf.h"

/* description record: used to describe image data, palette data etc. */
typedef struct {
    int32 xdim, ydim;               /* dimensions of data */
    DFdi nt;                        /* number type of data */
    int16 ncomponents, interlace;   /* data ordering: chunky / planar etc */
    DFdi compr;                     /* compression */
        /* ### Note: compression is currently uniquely described with a tag.
            No data is attached to this tag/ref.  But this capability is
            provided for future expansion, when this tag/ref might point to
            some data needed for decompression, such as the actual encodings */
} DFGRdr;

/* structure to hold RIG info */
typedef struct {
    DFdi data[3];                   /* image/lut/mattechannel */
    DFGRdr datadesc[3];             /* description of image/lut/mattechannel */
    int32 xpos, ypos;               /* X-Y position of image on screen */
    float32 aspectratio;            /* ratio of pixel height to width */
    float32 ccngamma,               /* color correction parameters */
        ccnred[3], ccngrren[3], ccnblue[3], ccnwhite[3];
    char *cf;                       /* color format */
} DFGRrig;

#endif /* DFGR_H */
