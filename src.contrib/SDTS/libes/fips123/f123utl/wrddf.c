/*
                           U.S. Geological Survey
                          National Mapping Division

                 Spatial Data Transfer Standard (SDTS)
                                 FIPS 173


                          Example Program

                 program:         wrddf.c
                 author:          Bob Lazar
                 date:            March 3, 1993
                 language:        C

         This program uses the FIPS 123 Function Library to create a sample 
         SDTS Identification module.

         This program is an example which uses the FIPS 123 Function Library.  
         This is a public domain software library which supports the SDTS, 
         FIPS 173, an exchange standard for digital spatial data which uses 
         ANSI/ISO 8211 (FIPS 123) for its physical file implementation.

         The FIPS 123 Function Library has been developed on the MS/DOS
         and Data General AViiON Unix platforms.  It has been developed
         using the ANSI standard C language.  It has not been fully
         tested on other platforms.

         To obtain this library or obtain additional information on the
         SDTS, contact the following address:

              SDTS Request
              U.S. Geological Survey
              526 National Center
              Reston, VA   22092

              Fax:    (703) 648-5542
              Internet: sdts@usgs.gov

         To compile and link under DOS using Microsoft C:

                 CL /AL WRDDF.C DOS_0293.LIB

          To compile and link using the GNU C compiler on Data General
          workstations:

                 gcc wrddf.c dg_0293.a -o wrddf

         Note that library names will vary depending upon the version of
         the software.



*/
#include <stdio.h>
#include <string.h>
#include "stc123.h"

FILE *fp;                /* File pointer for ISO 8211 file */
char mode;               /* File open mode */
char ice[2];             /* Inline code extensions */
char ccs[4];             /* Code character set */
char tag[5];             /* ISO 8211 tag */
char wr_str[2000];       /* string to store field contents */
char leadid;             /* leader ID */
int option;              /* option for write functions */
long str_len;            /* length of wr_str */
long int_level;          /* ISO 8211 DDF level */

main()
{

/*****************************************************************************

         Open ISO 8211 file.

*****************************************************************************/

/*       Set ISO 8211 file options        */

int_level = 2;           /* ISO 8211 level 2 */
mode = 'w';              /* Open mode = write */
strcpy (ice," ");        /* No inline escape sequences are used */
strcpy (ccs,"   ");      /* default character set */

/*       Open ISO 8211 file               */

if (!beg123file ("test.ddf",mode,&int_level,ice,ccs,&fp))
         {
         printf ("\nError on beg123file");
         exit;
         }

/*****************************************************************************

         Create ISO 8211 data descriptive record (DDR) for
         Identification module.

*****************************************************************************/


/*       Begin data descriptive record (DDR)      */

if (!beg123ddrec (fp))
         {
         printf ("\nError on beg123ddrec");
         exit;
         }

/*       Write ISO 8211 file control field to DDR          */

option = 2;              /* start of record option         */
strcpy (tag, "0000");
strcpy (wr_str,"0000;&EXAMPLE SDTS IDENTIFICATION MODULE");
if (!wr123ddfld (fp, tag, wr_str,option))
         {
         printf ("\nError on wr123ddfld");
         exit;
         }

/*       Write ISO 8211 record identifier field to DDR             */

option = 1;              /* "OK" option   */
strcpy (tag, "0001");
strcpy (wr_str,"0100;&DDF RECORD IDENTIFIER");
if (!wr123ddfld (fp, tag, wr_str,option))
         {
         printf ("\nError on wr123ddfld");
         exit;
         }

/*       Write Identification field to DDR                 */

option = 1;              /* "OK" option   */
strcpy (tag, "IDEN");
strcpy (wr_str,"1600;&IDENTIFICATION");
strcat (wr_str,"\x1f");           /* unit terminator */
strcat (wr_str,"MODN!RCID!STID!STVS!DOCU!PRID!PRVS!PDOC!TITL!DAID!");
strcat (wr_str,"DAST!MPDT!DCDT!SCAL");
strcat (wr_str,"\x1f");
strcat (wr_str,"(A,I,11A,I,A)");
if (!wr123ddfld (fp, tag, wr_str,option))
         {
         printf ("\nError on wr123ddfld");
         exit;
         }

/*       Write Conformance field to DDR           */

option = 3;              /* end of record option  */
strcpy (tag, "CONF");
strcpy (wr_str,"1600;&CONFORMANCE");
strcat (wr_str,"\x1f");
strcat (wr_str,"FFYN!VGYN!GTYN!RCYN!EXSP!FTLV");
strcat (wr_str,"\x1f");
strcat (wr_str,"(4A,2I)");
if (!wr123ddfld (fp, tag, wr_str,option))
         {
         printf ("\nError on wr123ddfld");
         exit;
         }

/*       End writing of DDR               */

if (!end123ddrec (fp))
         {
         printf ("\nError on end123ddrec");
         exit;
         }

/*****************************************************************************

         Create ISO 8211 data record (DR) for
         Identification module.

*****************************************************************************/


/*       Begin writing of data record (DR)        */

if (!beg123rec (fp))
         {
         printf ("\nError on beg123rec");
         exit;
         }

/*       Write ISO 8211 record identifier field to DR                      */

strcpy (tag,"0001");
strcpy (wr_str,"    1");
str_len = strlen (wr_str);
option = 2;
leadid='D';
if (!wr123fld (fp,tag,leadid,wr_str,str_len,option))
         {
         printf ("\nError on wr123fld");
         exit;
         }

/*       Write Identification field to DR         */

strcpy (tag,"IDEN");
strcpy (wr_str,"IDEN");
strcat (wr_str,"\x1f");
strcat (wr_str,"1");
strcat (wr_str,"\x1f");
strcat (wr_str,"SPATIAL DATA TRANSFER STANDARD"); 
strcat (wr_str,"\x1f");
strcat (wr_str,"1992 AUGUST 28"); 
strcat (wr_str,"\x1f");
strcat (wr_str,"FIPS PUB 173"); 
strcat (wr_str,"\x1f");
strcat (wr_str,"SDTS TOPOLOGICAL VECTOR PROFILE"); 
strcat (wr_str,"\x1f");
strcat (wr_str,"FIPS 173 VERSION 01 19930215"); 
strcat (wr_str,"\x1f");
strcat (wr_str,"FIPS 173 PART 4");
strcat (wr_str,"\x1f");
strcat (wr_str,"WILMINGTON SOUTH, DE-NJ HYDROGRAPHY"); 
strcat (wr_str,"\x1f");
strcat (wr_str,"\x1f");
strcat (wr_str,"DLG-3"); 
strcat (wr_str,"\x1f");
strcat (wr_str,"1987"); 
strcat (wr_str,"\x1f");
strcat (wr_str,"Wed Feb 17 17:33:59 1993"); 
strcat (wr_str,"\x1f");
strcat (wr_str,"   24000"); 
option = 1;              /* option = "okay" */
str_len = strlen (wr_str);
if (!wr123fld (fp,tag,leadid,wr_str,str_len,option))
         {
         printf ("\nError on wr123fld");
         exit;
         }

/*       Write Conformance field  to DR   */

strcpy (tag,"CONF");
strcpy (wr_str,"N");
strcat (wr_str,"\x1f");
strcat (wr_str,"N");
strcat (wr_str,"\x1f");
strcat (wr_str,"Y");
strcat (wr_str,"\x1f");
strcat (wr_str,"N");
strcat (wr_str,"\x1f");
strcat (wr_str,"1");
strcat (wr_str,"\x1f");
strcat (wr_str,"4");
option = 3;              /* option = end of record */
str_len = strlen (wr_str);
if (!wr123fld (fp,tag,leadid,wr_str,str_len,option))
         {
         printf ("\nError on wr123fld");
         exit;
         }

/*       End writing of DR                */

if (!end123rec (fp))
         {
         printf ("\nError on end123rec");
         exit;
         }

/*       Close file and stop processing           */

if (!end123file (&fp))
         {
         printf ("\nError on end123file");
         exit;
         }
}
