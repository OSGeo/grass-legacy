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
static char RcsId[] = "@(#)$Revision: 1.2 $";
#endif
/*
$Header: /hdf/hdf/v3.2r2/src/RCS/herrf.c,v 1.2 1992/09/11 14:15:04 koziol beta $

$Log: herrf.c,v $
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
 * File:    herrF.c
 * Purpose: C stubs for error-handling Fortran routines
 * Invokes: herr.c.
 * Contents: 
 *     heprnt_:    Call HEprint to print error message
 * Remarks: none
 *---------------------------------------------------------------------------*/

#include "hdf.h"

#ifndef HERR_FNAMES
#   define HERR_FNAMES
#ifdef DF_CAPFNAMES
#   define nheprnt   FNAME(HEPRNT)
#else
#   define nheprnt   FNAME(heprnt)
#endif /* DF_CAPFNAMES */
#endif /* HERR_FNAMES */

/*-----------------------------------------------------------------------------
 * Name:    heprnt
 * Purpose: call HEprint to print error messages, starting from top of stack
 * Inputs:  print_levels: number of levels to print
 * Returns: 0 on success, FAIL on failure 
 * Users:   Fortran stub routine
 * Invokes: HEprint
 * Remarks: This routine has one less parameter than HEprint, because it
 *          doesn't allow the user to specify the stream to print to.
 *          Instead it prints automatically to stdout.
 *---------------------------------------------------------------------------*/

    FRETVAL(VOID)
#ifdef PROTOTYPE
nheprnt(intf *print_levels)
#else
nheprnt(print_levels)
    intf *print_levels;
#endif /* PROTOTYPE */
{
    HEprint(stderr, *print_levels);
}

