B.1	NCSA HDF Calling Interfaces and Utilities

Header Files and FORTRAN Constants File	B.1

National Center for Supercomputing Applications

July 1990

                                                                

July 1990




Appendix  B	Header Files and FORTRAN Constants File




This appendix includes the general header files used in compiling 
all HDF libraries. These files do not have to be included with most 
applications, but they could be usefully added to code that refers to 
specific tags by name, or code that is designed to be responsive to 
different error conditions. You get these files when you get the 
HDF source code.

You will see that the C header (df.h) shown in Figure B.2 is more 
completely commented than the FORTRAN header () shown in 
Figure B.1. Thus, you may want to look at the C header for 
explanations of some definitions, even if you plan to use the 
#define statements from the FORTRAN header.

The file constants.f shown in Figure B.3 is equivalent to dfF.h 
(Figure B.1), put uses the FORTRAN PARAMETER statement to 
declare constants, and hence may be more useful for inserting into 
your FORTRAN code.

Other headers used for specific applications, such as the RIS 
interface and the SDS interface can be found by obtaining the 
source code from NCSA. These headers also do not normally have 
to be included with your source code.



Figure B.1  FORTRAN Header File: dfF.h

/*****************************************************************************
* 
*							  NCSA HDF version 2.0
*								December 20, 1988
*
* NCSA HDF Version 2.0 source code and documentation are in the public domain.
* Specifically, we give to the public domain all rights for future licensing
* of the source code, all resale rights, and all publishing rights.
* 
* We ask, but do not require, that the following message be included in all
* derived works:
* 
* Portions developed at the National Center for Supercomputing Applications at
* the University of Illinois at Urbana-Champaign.
* 
* THE UNIVERSITY OF ILLINOIS GIVES NO WARRANTY, EXPRESSED OR IMPLIED, FOR THE
* SOFTWARE AND/OR DOCUMENTATION PROVIDED, INCLUDING, WITHOUT LIMITATION,
* WARRANTY OF MERCHANTABILITY AND WARRANTY OF FITNESS FOR A PARTICULAR PURPOSE
* 
*****************************************************************************/

C *-----------------------------------------------------------------------------
C * File: 	dfF.h
C * Purpose:	Fortran header file for HDF routines
C * Contents: 
C *	Tag definitions
C * Error return codes
C *	Logical constants
C * Remarks: This file can be included with user programs
C *----------------------------------------------------------------------------*

#ifndef DFTAG_NULL

#define	DFREF_WILDCARD 0
#define	DFTAG_WILDCARD 0
#define	DFTAG_NULL	1

#define	DFTAG_FID	100
#define	DFTAG_FD	101
#define	DFTAG_TID	102
#define	DFTAG_TD	103
#define	DFTAG_DIL	104
#define	DFTAG_DIA	105
#define	DFTAG_NT	106
#define	DFTAG_MT	107

#define	DFTAG_ID8	200
#define	DFTAG_IP8	201
#define	DFTAG_RI8	202
#define	DFTAG_CI8	203
#define	DFTAG_II8	204

#define	DFTAG_ID	300
#define	DFTAG_LUT	301
#define	DFTAG_RI	302
#define	DFTAG_CI	303

#define	DFTAG_RIG	306
#define	DFTAG_LD	307
#define	DFTAG_MD	308
#define	DFTAG_MA	309


Figure B.1  FORTRAN Header File: dfF.h (Continued)

#define	DFTAG_CCN	310
#define	DFTAG_CFM	311
#define	DFTAG_AR	312

#define	DFTAG_DRAW	400
#define	DFTAG_RUN	401

#define	DFTAG_XYP	500
#define	DFTAG_MTO	501

#define	DFTAG_T14	602
#define	DFTAG_T105	603

#define	DFTAG_RLE	11
#define	DFTAG_IMCOMP 12

C 					Error Return Codes 

#define	DFE_NOERROR	0
#define	DFE_FNF	-1
#define	DFE_DENIED	-2
#define	DFE_ALROPEN	-3
#define	DFE_TOOMANY	-4
#define	DFE_BADNAME	-5
#define	DFE_BADACC	-6 
#define	DFE_NOTOPEN	-8
#define	DFE_CANTCLOSE	-9	
#define	DFE_DFNULL	-10	
#define	DFE_ILLTYPE	-11	
#define	DFE_UNSUPPORTED	-12
#define	DFE_BADDDLIST		-13
#define	DFE_NOTDFFILE		-14
#define	DFE_SEEDTWICE		-15
#define	DFE_NOSPACE		-16
#define	DFE_READERROR		-18
#define	DFE_WRITEERROR	-19
#define	DFE_SEEKERROR		-20
#define	DFE_NOFREEDD		-21
#define	DFE_BADTAG		-22
#define	DFE_BADREF		-23
#define	DFE_RDONLY		-24
#define	DFE_BADCALL		-25
#define	DFE_BADPTR		-26
#define	DFE_BADLEN		-27
#define	DFE_BADSEEK		-28
#define	DFE_NOMATCH		-29
#define	DFE_NOTINSET		-30
#define	DFE_BADDIM		-31
#define	DFE_BADOFFSET		-32
#define	DFE_BADSCHEME		-33
#define	DFE_NODIM		-34
#define	DFE_NOTENOUGH		-35

C						Logical Constants

#define	DFACC_READ		1
#define	DFACC_WRITE		2
#define	DFACC_CREATE		4
#define	DFACC_ALL		7

#endif DFTAG_NULL


Figure B.2  C Header File: df.h

/*****************************************************************************
* 
*							  NCSA HDF version 2.0
*								December 20, 1988
*
* NCSA HDF Version 2.0 source code and documentation are in the public domain.
* Specifically, we give to the public domain all rights for future licensing
* of the source code, all resale rights, and all publishing rights.
* 
* We ask, but do not require, that the following message be included in all
* derived works:
* 
* Portions developed at the National Center for Supercomputing Applications at
* the University of Illinois at Urbana-Champaign.
* 
* 
* THE UNIVERSITY OF ILLINOIS GIVES NO WARRANTY, EXPRESSED OR IMPLIED, FOR THE
* SOFTWARE AND/OR DOCUMENTATION PROVIDED, INCLUDING, WITHOUT LIMITATION,
* WARRANTY OF MERCHANTABILITY AND WARRANTY OF FITNESS FOR A PARTICULAR PURPOSE
* 
*****************************************************************************/

/*------------------------------------------------------------------------------
 * File: 	df.h
 * Purpose:	header file for HDF routines
 * Invokes:	dfi.h
 * Contents: 
 *	Structure definitions: DFddh, DFdd, DFdesc, DFdle, DF, DFdi, DFdata
 *	Procedure type definitions
 *	Global variables
 *	Tag definitions
 *  Error return codes
 *	Logical constants
 * Remarks: This file is included with user programs
 *		Since it includes stdio.h etc., do not include these after df.h
 *----------------------------------------------------------------------------*/


#ifndef DFTAG_NULL				/* avoid re-inclusion */

/* include DF (internal) header information */
#include "dfi.h"

/*--------------------------------------------------------------------------*/
/*Type declarations 								*/

typedef struct DFddh {		/* format of data descriptor headers as in file */
	int16
		dds;			/* number of dds in header block */
	int32	
		next;			/* offset of next header block */
	} DFddh;

		
typedef struct 
	uint16
		tag,			/* data tag */
		ref;			/* data reference number */
	int32
		offset,		/* offset of data element in file */
		length;		/* number of bytes */
	} DFdd;

Figure B.2  C Header File: df.h (Continued)

	/* descriptor structure is same as dd structure.  ###Note: may be changed */
#define	DFdesc	DFdd


	/* DLE is the internal structure that stores data descriptor information */
	/* It is a linked list of DDs */
typedef struct DFdle {		/* Data List element */
	struct DFdle
		*next;			/* link to next dle */
	DFddh
		ddh;			/* To store headers */
	DFdd
		dd[1];			/* dummy size */
	} DFdle;	


	/* DF is the internal structure associated with each DF file */
	/* It holds information associated with the file as a whole */
	/* ### Note: there are hooks for having multiple DF files open at a time */
typedef struct DF {
	DFdle
		*list,		/* Pointer to the DLE list */
		*last_dle;	/* last_dle and last_dd are used in searches to indicate
					element returned by previous call to DFfind */
	int
		type,		/* 0= not in use, 1= normal, -1 = multiple */
				/* this is a hook for when multiple files are open */
		access,	/* permitted access types: 0=none, 1=r, 2=w, 3=r/w */
		changed, 	/* True if anything in DDs modified since last write */
		last_tag,	/* Last tag searched for by DFfind */
		last_ref,	/* Last reference number searched for */
		last_dd,	/* see last_dle */
		defdds,	/* default number of DD's in each block */
		up_access;	/* access permissions to element being read/updated */
				/* Used by DFstart */
	DFdd
		*up_dd;	/* DD of element being read/updated, used by DFstart */
  				/* file handle is a file pointer or file descriptor */
				/* depending whether we use buffered or unbuffered i/o */

#ifdef DF_BUFFIO
	FILE *			/* file pointer */
#else DF_BUFFIO
	int			/* file descriptor */
#endif DF_BUFFIO
		file;		/* File handle for real file */
} DF;


typedef struct DFdi {	/* data identifier: specifies data element uniquely */
	uint16 tag;
	uint16 ref;
} DFdi;



typedef struct DFdata {	/* structure for returning status information */
	int version;		/* version number of program */
} DFdata;


Figure B.2  C Header File: df.h (Continued)

/*--------------------------------------------------------------------------*/
/*		Procedure types 						*/

DF *DFopen();
int32 DFgetelement();
int32 DFread();
int32 DFseek();
int32 DFwrite();

/*--------------------------------------------------------------------------*/
/*		Global Variables 						*/

#ifndef DFMASTER
extern
#endif DFMASTER
int
	DFerror;			/* Error code for DF routines */

/*--------------------------------------------------------------------------*/
/*		Tag Definitions						    */

#define	DFREF_WILDCARD 0	/* wildcard ref for searches */

#define	DFTAG_WILDCARD 0	/* wildcard tag for searches */
#define	DFTAG_NULL	1		/* empty DD */

	/* utility set */
#define	DFTAG_FID	100		/* File identifier */
#define	DFTAG_FD	101		/* File description */
#define	DFTAG_TID	102		/* Tag identifier */
#define	DFTAG_TD	103		/* Tag descriptor */
#define	DFTAG_DIL	104		/* data identifier label */
#define	DFTAG_DIA	105		/* data identifier annotation */
#define	DFTAG_NT	106		/* number type */
#define	DFTAG_MT	107		/* machine type */

	/* raster-8 set */
#define	DFTAG_ID8	200		/* 8-bit Image dimension */
#define	DFTAG_IP8	201		/* 8-bit Image palette */
#define	DFTAG_RI8	202		/* Raster-8 image */
#define	DFTAG_CI8	203		/* RLE compressed 8-bit image */
#define	DFTAG_II8	204		/* IMCOMP compressed 8-bit image */

	/* Raster Image set */
#define	DFTAG_ID	300		/* Image DimRec */
#define	DFTAG_LUT	301		/* Image Palette */
#define	DFTAG_RI	302		/* Raster Image */
#define	DFTAG_CI	303		/* Compressed Image */

#define	DFTAG_RIG	306		/* Raster Image Group */
#define	DFTAG_LD	307		/* Palette DimRec */
#define	DFTAG_MD	308		/* Matte DimRec */
#define	DFTAG_MA	309		/* Matte Data */
#define	DFTAG_CCN	310		/* color correction */
#define	DFTAG_CFM	311		/* color format */
#define	DFTAG_AR	312		/* aspect ratio */
	
#define	DFTAG_DRAW	400		/* Draw these images in sequence */
#define	DFTAG_RUN	401		/* run this as a program/script */


Figure B.2  C Header File: df.h (Continued)

#define	DFTAG_XYP	500		/* x-y position */
#define	DFTAG_MTO	501		/* machine-type override */

	/* Tektronix */
#define	DFTAG_T14	602		/* TEK 4014 data */
#define	DFTAG_T105	603		/* TEK 4105 data */

	/* Scientific Data set */
#define	DFTAG_SDG	700		/* Scientific Data Group */
#define	DFTAG_SDD	701		/* Scientific Data DimRec */
#define	DFTAG_SD	702		/* Scientific Data */
#define	DFTAG_SDS	703		/* Scales */
#define	DFTAG_SDL	704		/* Labels */
#define	DFTAG_SDU	705		/* Units */
#define	DFTAG_SDF	706		/* Formats */
#define	DFTAG_SDM	707		/* Max/Min */
#define	DFTAG_SDC	708		/* Coord sys */

	/* compression schemes */
#define	DFTAG_RLE	11		/* run length encoding */
#define	DFTAG_IMCOMP 12		/* IMCOMP compression */

/*--------------------------------------------------------------------------*/
/*		Error Return Codes 						*/

#define	DFE_NOERROR		0	/* No error */
#define	DFE_FNF		-1	/* File not found error */
#define	DFE_DENIED		-2	/* Access to file denied */
#define	DFE_ALROPEN		-3	/* File already open */
#define	DFE_TOOMANY		-4	/* Too Many DF's or files open */
#define	DFE_BADNAME		-5	/* Bad file name on open */
#define	DFE_BADACC		-6	/* Bad file access mode */
#define	DFE_BADOPEN		-7	/* Other open error */
#define	DFE_NOTOPEN		-8	/* File can't be closed: it isn't open */
#define	DFE_CANTCLOSE		-9	/* fclose wouldn't work! */
#define	DFE_DFNULL		-10	/* DF is a null pointer */
#define	DFE_ILLTYPE		-11	/* DF has an illegal type: internal error */
#define	DFE_UNSUPPORTED	-12	/* Feature not currently supported */
#define	DFE_BADDDLIST		-13	/* No DD list: internal error */
#define	DFE_NOTDFFILE		-14	/* Not a DF file and it is not 0 length */
#define	DFE_SEEDTWICE		-15	/* DD list already seeded: internal error */
#define	DFE_NOSPACE		-16	/* Malloc failed */
#define	DFE_NOSUCHTAG		-17	/* No such tag in the file: search failed*/
#define	DFE_READERROR		-18	/* There was a read error */
#define	DFE_WRITEERROR	-19	/* There was a write error */
#define	DFE_SEEKERROR		-20	/* There was a seek error */
#define	DFE_NOFREEDD		-21	/* No free DD's left: internal error */
#define	DFE_BADTAG		-22	/* illegal WILDCARD tag */
#define	DFE_BADREF		-23	/* illegal WILDCARD reference # */
#define	DFE_RDONLY		-24	/* The DF is read only */
#define	DFE_BADCALL		-25	/* Calls in wrong order */
#define	DFE_BADPTR		-26 	/* NULL ptr argument */
#define	DFE_BADLEN		-27	/* negative len specified */
#define	DFE_BADSEEK		-28	/* Attempt to seek past end of element */
#define	DFE_NOMATCH		-29	/* No (more) DDs which match specified tag/ref */
#define	DFE_NOTINSET		-30	/* Warning: Set contained unknown tag: ignored */
#define	DFE_BADDIM		-31	/* negative or zero dimensions specified */
#define	DFE_BADOFFSET		-32	/* Illegal offset specified */
#define	DFE_BADSCHEME		-33	/* Unknown compression scheme specified */
#define	DFE_NODIM		-34	/* No dimension record associated with image */
#define	DFE_NOTENOUGH		-35	/* space provided insufficient for size of data 


Figure B.2  C Header File: df.h (Continued)

*/
#define	DFE_NOVALS		-36	/* Values not available */
#define	DFE_CORRUPT		-37	/* File is corrupted */
#define	DFE_BADCONV		-37	/* Don't know how to convert data type */
#define	DFE_BADFP		-38	/* The file contained an illegal floating point 
no*/

/*--------------------------------------------------------------------------*/
/*		Logical Constants 						*/

#define	DFACC_READ		1	/* Read Access */
#define	DFACC_WRITE		2	/* Write Access */
#define	DFACC_CREATE		4	/* force file to be created */
#define	DFACC_ALL		7	/* logical and of all the above values */

#endif DFTAG_NULL




Figure B.3	FORTRAN:  Constants File constants.h

/*****************************************************************************
* 
*							  NCSA HDF version 2.0
*								December 20, 1988
*
* NCSA HDF Version 2.0 source code and documentation are in the public domain.
* Specifically, we give to the public domain all rights for future licensing
* of the source code, all resale rights, and all publishing rights.
* 
* We ask, but do not require, that the following message be included in all
* derived works:
* 
* Portions developed at the National Center for Supercomputing Applications at
* the University of Illinois at Urbana-Champaign.
* 
* THE UNIVERSITY OF ILLINOIS GIVES NO WARRANTY, EXPRESSED OR IMPLIED, FOR THE
* SOFTWARE AND/OR DOCUMENTATION PROVIDED, INCLUDING, WITHOUT LIMITATION,
* WARRANTY OF MERCHANTABILITY AND WARRANTY OF FITNESS FOR A PARTICULAR PURPOSE
* 
*****************************************************************************/
C *-----------------------------------------------------------------------------
C * File: 	dfF.h
C * Purpose:	Fortran header file for HDF routines
C * Contents: 
C *	Tag definitions
C * Error return codes
C *	Logical constants
C * Remarks: This file can be included with user programs
C *----------------------------------------------------------------------------*

      PARAMETER (DFREF_WILDCARD = 0,
     *           DFTAG_WILDCARD = 0,
     *           DFTAG_NULL     = 1)

      PARAMETER (DFTAG_FID	 = 100,
     *           DFTAG_FD	 = 101,
     *           DFTAG_TID	 = 102,
     *           DFTAG_TD	 = 103,
     *           DFTAG_DIL	 = 104,
     *           DFTAG_DIA	 = 105,
     *           DFTAG_NT	 = 106,
     *           DFTAG_MT	 = 107,
     *
     *           DFTAG_ID8	 = 200,
     *           DFTAG_IP8	 = 201,
     *           DFTAG_RI8	 = 202,
     *           DFTAG_CI8	 = 203,
     *           DFTAG_II8	 = 204)
     *
      PARAMETER (DFTAG_ID	 = 300,
     *           DFTAG_LUT	 = 301,
     *           DFTAG_RI	 = 302,
     *           DFTAG_CI	 = 303,
     *
     *           DFTAG_RIG	 = 306,
     *           DFTAG_LD	 = 307,
     *           DFTAG_MD	 = 308,
     *           DFTAG_MA	 = 309,


Figure B.3	FORTRAN:  Constants File constants.h (Continued)
      PARAMETER (DFTAG_CCN	 = 310,
     *           DFTAG_CFM	 = 311,
     *           DFTAG_AR	 = 312,
     *           DFTAG_DRAW	 = 400,
     *           DFTAG_RUN	 = 401,
     * 
     *           DFTAG_XYP	 = 500,
     *           DFTAG_MTO	 = 501,
     * 
     *           DFTAG_T14	 = 602,
     *           DFTAG_T105	 = 603,
     * 
     *           DFTAG_RLE	 = 11,
     *           DFTAG_IMCOMP	 = 12)

C 					Error Return Codes 
      PARAMETER (DFE_NOERROR     	  =  0,
     *           DFE_FNF         	  = -1,
     *           DFE_DENIED      	  = -2,
     *           DFE_ALROPEN     	  = -3,
     *           DFE_TOOMANY     	  = -4,
     *           DFE_BADNAME     	  = -5,
     *           DFE_BADACC      	  = -6 ,
     *           DFE_NOTOPEN     	  = -8,
     *           DFE_CANTCLOSE   	  = -9,
     *           DFE_DFNULL      	 = -10,
     *           DFE_ILLTYPE     	 = -11,
     *           DFE_UNSUPPORTED 	 = -12,
     *           DFE_BADDDLIST   	 = -13,
     *           DFE_NOTDFFILE   	 = -14,
     *           DFE_SEEDTWICE   	 = -15,
     *           DFE_NOSPACE     	 = -16,
     *           DFE_READERROR   	 = -18,
     *           DFE_WRITEERROR  	 = -19)

      PARAMETER (DFE_SEEKERROR   	 = -20,
     *           DFE_NOFREEDD    	 = -21,
     *           DFE_BADTAG      	 = -22,
     *           DFE_BADREF      	 = -23,
     *           DFE_RDONLY      	 = -24,
     *           DFE_BADCALL     	 = -25,
     *           DFE_BADPTR      	 = -26,
     *           DFE_BADLEN      	 = -27,
     *           DFE_BADSEEK     	 = -28,
     *           DFE_NOMATCH     	 = -29,
     *           DFE_NOTINSET    	 = -30,
     *           DFE_BADDIM      	 = -31,
     *           DFE_BADOFFSET   	 = -32,
     *           DFE_BADSCHEME   	 = -33,
     *           DFE_NODIM       	 = -34,
     *           DFE_NOTENOUGH   	 = -35)

C				Logical Constants
      PARAMETER (DFACC_READ       	 = 1,
     *           DFACC_WRITE      	 = 2,
     *           DFACC_CREATE     	 = 4,
     *           DFACC_ALL        	 = 7)



