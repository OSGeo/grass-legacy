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
#ifdef RCSID
static char RcsId[] = "@(#)$Revision: 1.4 $";
#endif
/*
$Header: /hdf/hdf/v3.2r2/src/RCS/dfutilf.c,v 1.4 1992/10/27 22:18:31 koziol beta $

$Log: dfutilf.c,v $
 * Revision 1.4  1992/10/27  22:18:31  koziol
 * deleted fortran stubs for DFtransarray call
 *
 * Revision 1.3  1992/10/12  18:11:51  koziol
 * Updated for v3.2r2 release
 *
 * Revision 1.2  1992/09/11  14:15:04  koziol
 * Changed Fortran stubs' parameter passing to use a new typedef, intf,
 * which should be typed to the size of an INTEGER*4 in whatever Fortran
 * compiler the C functions need to be compatible with.  (This is mostly
 * for the PC and possibly for the Mac)
 *
 * Revision 1.1  1992/08/25  21:40:44  koziol
 * Initial revision
 *
*/
/*-----------------------------------------------------------------------------
 * File:    dfutilF.c
 * Purpose: C stubs for Fortran utility routines
 * Invokes: dfutil.c
 * Contents: 
 *  dfindnr_:       For a given tag, find the next ref after the given ref
 *---------------------------------------------------------------------------*/

#include "hdf.h"

#ifndef DFUTIL_FNAMES
#   define DFUTIL_FNAMES
#ifdef DF_CAPFNAMES
#   define ndfindnr          FNAME(DFINDNR)
#   define ndffindnextref    FNAME(DFFINDNEXTREF)
#else
#   define ndfindnr          FNAME(dfindnr)
#   define ndffindnextref    FNAME(dffindnextref)
#endif /* DF_CAPFNAMES */
#endif /* DFUTIL_FNAMES */

/*-----------------------------------------------------------------------------
 * Name:    dfindnr
 * Purpose: For this tag, find the ref after lref
 * Inputs:  dfile: ptr to open DF file
 *          tag:   tag to look for
 *          lref:  ref after which to search
 *
 * Returns: the desired ref if successful, on failure with	DFerror set
 * Users:   HDF Fortran programmers
 * Invokes: DFfindnextref
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndfindnr(intf *dfile, intf *tag, intf *lref)
#else
ndfindnr(dfile, tag, lref)
     intf *dfile;
     intf *tag, *lref;
#endif /* PROTOTYPE */
{
    return(DFfindnextref(*dfile, *tag, *lref));
}

/*
CEND7MAX
*/

/*-----------------------------------------------------------------------------
 * Name:    dffindnextref
 * Purpose: For this tag, find the ref after lref
 * Inputs:  dfile: ptr to open DF file
 *          tag:   tag to look for
 *          lref:  ref after which to search
 *
 * Returns: the desired ref if successful, on failure with	DFerror set
 * Users:   HDF Fortran programmers
 * Invokes: DFfindnextref
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndffindnextref(intf *dfile, intf *tag, intf *lref)
#else
ndffindnextref(dfile, tag, lref)
     intf *dfile;
     intf *tag, *lref;
#endif /* PROTOTYPE */
{
    return(DFfindnextref(*dfile, *tag, *lref));
}

