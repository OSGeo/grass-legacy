/*
** DKS: 1/91 Modified for Grass4.0: premature EOF trap added. 
**
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "gis.h"
#include "AtoG.h"

int FindAttAndCat (
    FILE *fp,      /* text file */
    int AttCol,   /* number of attribute column */
    int CatCol,   /* number of category column */
    int IDCol,    /* number of id column */
    int IDNum,    /* id number */
    char *AttText, /* attribute text */
    int *CatNum
)
{
char txtbuf[512];
char colbuf[512];
char errmsg[200];
int  done=0;
int  RowCount=0;

rewind(fp);
if (fgets(colbuf,512,fp)==NULL) return(-1); /* skip first record */
while (1)
   {
   if (fgets(colbuf,512,fp)==NULL)
   {
		sprintf (errmsg, "Premature Text-Label EOF. ID #%d not found.\n", IDNum);
		G_fatal_error (errmsg);
   }
   if (GetColumn(colbuf,IDCol,txtbuf) < 0) return(-2);
   if (IDNum == atoi(txtbuf))
      {
      if (AttCol!=-1)
         {
         if (GetColumn(colbuf,AttCol,txtbuf) < 0) return(-3);
         strcpy(AttText,txtbuf);
         }
      if (GetColumn(colbuf,CatCol,txtbuf) < 0) return(-4);
      *CatNum = atoi(txtbuf);
      return(0);
      }
   }
}
