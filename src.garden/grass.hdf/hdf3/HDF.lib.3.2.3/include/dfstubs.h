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

Revision 1.1.1.1  1999/12/29 15:12:36  markus
initial CVS import

 * Revision 1.1  1992/08/25  21:40:44  koziol
 * Initial revision
 *
*/
/*
** FILE
**	dfstubs.h
** PURPOSE
**	Header file for "dfstubs.c" HDF 3.1 emulation using new routines
**	from "hfile.c".
** AUTHOR
**	Doug Ilg
*/

#ifndef DFSTUBS_H  /* avoid re-inclusion */
#define DFSTUBS_H
/* This is the master HDF driver (taking the place of df.c), so... */
#define DFMASTER
#undef PERM_OUT  /* used to "comment out" code */

#include "df.h"
#undef DFMASTER

#if !defined(__GNUC__) & !defined(CONVEX) & !defined(VMS) & !defined(PC)
#include <memory.h>
#endif  /* !__GNUC__ & !CONVEX & !VMS & !PC */

#define DFACC_APPEND	8
#define DFEL_ABSENT	0
#define DFEL_RESIDENT	1
#define DFSRCH_OLD	0
#define DFSRCH_NEW	1

PRIVATE int32	DFid;
PRIVATE int32	DFaid;
PRIVATE int	DFaccmode;
PRIVATE int	DFelaccmode;
PRIVATE uint16	search_tag;
PRIVATE uint16	search_ref;
PRIVATE int	search_stat	= DFSRCH_NEW;
PRIVATE int32	search_aid	= 0;
PRIVATE int	DFelstat	= DFEL_ABSENT;
PRIVATE int32	DFelsize	= 0;
PRIVATE int32	DFelseekpos	= 0;
PRIVATE uint16	acc_tag		= 0;
PRIVATE uint16	acc_ref		= 0;
PRIVATE char	*DFelement	= NULL;

extern DF *makedf();

/* prototypes for internal routines */
PRIVATE int DFIclearacc
   PROTO((void));

PRIVATE int DFIcheck
   PROTO((DF *dfile));

#endif /* DFSTUBS_H */
