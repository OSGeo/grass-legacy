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
static char RcsId[] = "@(#)$Revision: 1.3 $";
#endif
/*
$Header: /hdf/hdf/v3.2r2/src/RCS/hfilef.c,v 1.3 1992/10/23 00:14:11 koziol beta $

$Log: hfilef.c,v $
 * Revision 1.3  1992/10/23  00:14:11  koziol
 * Changed all DFIstr*() and DFImem*() calls to HDstr*() and HDmem*() calls
 * #ifdef'd out the macros Jason defined for Hopen, Hclose, etc. for Vsets
 * Replaced Vset VFREESPACE and VGETSPACE calls with actual calls to HDfreespace
 * and HDgetspace
 * Added a MS-Windows lower lower for file I/O (which may not be completely working
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
 * File:    hfileF.c
 * Purpose: C stubs for Fortran low level routines
 * Invokes: hfile.c
 * Contents: 
 *  hiopen_:   call Hopen to open HDF file
 *  hclose_:   call Hclose to close HDF file
 *---------------------------------------------------------------------------*/

#include "hdf.h"

#ifndef HFILE_FNAMES
#   define HFILE_FNAMES
#ifdef DF_CAPFNAMES
#   define nhiopen   FNAME(HIOPEN)
#   define nhclose   FNAME(HCLOSE)
#else
#   define nhiopen   FNAME(hiopen)
#   define nhclose  FNAME(hclose)
#endif /* DF_CAPFNAMES */
#endif /* HFILE_FNAMES */

/*-----------------------------------------------------------------------------
 * Name:    hiopen
 * Purpose: call Hopen to open HDF file
 * Inputs:  name: name of file to open
 *          access: access mode - integer with value DFACC_READ etc. 
 *          defdds: default number of DDs per header block
 *          namelen: length of name
 * Returns: 0 on success, -1 on failure with error set
 * Users:   HDF Fortran programmers
 * Invokes: Hopen
 * Method:  Convert filename to C string, call Hopen
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
nhiopen(_fcd name, intf *access, intf *defdds, intf *namelen)
#else
nhiopen(name, access, defdds, namelen)
    _fcd name;
    intf *access;
    intf *defdds;
    intf *namelen;
#endif /* PROTOTYPE */
{
    char *fn;
    intf ret;
    
    fn = HDf2cstring(name, *namelen);
    ret = (intf) Hopen(fn, *access, *defdds);
    HDfreespace(fn);
    return(ret);
}


/*-----------------------------------------------------------------------------
 * Name:    hclose
 * Purpose: Call DFclose to close HDF file
 * Inputs:  file_id: handle to HDF file to close
 * Returns: 0 on success, FAIL on failure with error set
 * Users:   HDF Fortran programmers
 * Invokes: Hclose
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
nhclose(intf *file_id)
#else
nhclose(file_id)
    intf *file_id;
#endif /* PROTOTYPE */
{
    return(Hclose(*file_id));
}

