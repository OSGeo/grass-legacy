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
$Header: /hdf/hdf/v3.2r2/src/RCS/vparse.c,v 1.3 1992/10/23 00:14:11 koziol beta $

$Log: vparse.c,v $
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
/*****************************************************************************
*
* vparse.c
* Part of the HDF VSet interface.
*
************************************************************************/

#include "vg.h"

#define ISCOMMA(c) ( (c==',') ? 1:0 )

/* ------------------------------------------------------------------ */

/*
** Given a string (attrs) , the routine parses it into token strings,
** and returns a ptr (attrv) to an array of ptrs where the tokens 
** are stored.  The no of tokens are returned in attrc.
**
** Currently used only by routines that manipulate field names.
** As such each field string is truncated to a max length of
** FIELDNAMELENMAX (as defined in vg.h). For most cases, this
** truncation doesn't happen because FIELDNAMELENMAX is a big number.
**
** RETURN -1 if error.
** RETURN 1 if ok.
**
** Current implementation: all strings inputs converted to uppercase.    
** tokens must be separated by COMMAs.
**
** Tokens are stored in static area sym , and pointers are returned
** to calling routine. Hence, tokens must be used before next call 
** to scanattrs.
**
*/

PRIVATE char* 	symptr[50];
PRIVATE char    sym[50][FIELDNAMELENMAX+1];
PRIVATE	intn 	nsym;

#ifdef PROTOTYPE
int32 scanattrs (char *attrs, int32 *attrc, char ***attrv)
#else
int32 scanattrs (attrs,attrc,attrv)

	char	*attrs;		/* field string (input) */
	int32	*attrc;		/* # of fields (output) */
	char	***attrv;	/* array of char ptrs to fields (output) */
#endif

{
  register char   *s, *s0, *ss;
  register intn   i, slen, len;
  char * FUNC = "scanattrs";
  
  s = attrs;
  slen = HDstrlen(s);
  nsym = 0;
  
  s0 = s;
  for (i = 0; i < slen; i++, s++)
    if ( ISCOMMA(*s) ) {
      len = s - s0; 
      if (len <= 0) return(FAIL);

      /* save that token */
      ss = symptr[nsym] = sym[nsym]; 
      nsym++;
      
      if ( len > FIELDNAMELENMAX) len = FIELDNAMELENMAX;
      HIstrncpy(ss, s0, len + 1);
      s0 = s+1;
    }
  
  /* save the last token */
  len = s - s0; 
  if (len <= 0) return(FAIL);
  ss = symptr[nsym] = sym[nsym]; 
  nsym++;
  
  if ( len > FIELDNAMELENMAX) len = FIELDNAMELENMAX;
  HIstrncpy(ss, s0, len + 1);
  
  /* convert all fields tokens to uppercase */
  for (i = 0; i < nsym; i++) {
    s = symptr[i];
    while(*s != '\0') {
      if (*s >= 'a' && *s <= 'z') *s=(char)toupper(*s);
      s++;
    }
  }
  
  symptr[nsym] = NULL;
  *attrc = nsym;
  *attrv = (char**) symptr;
  
  return(SUCCEED); /* ok */
  
} /* scanattrs */

/* ------------------------------------------------------------------ */
