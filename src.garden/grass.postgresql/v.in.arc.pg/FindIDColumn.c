#include <stdio.h>
#include <stdlib.h>
#include "AtoG.h"

/*main()
{
FILE *fp;
fp = fopen("testit","r");
fprintf (stdout,"IDCol=%d\n",FindIDColumn(fp));
}*/

/*******************************************************************/
int 
FindIDColumn (FILE *fp)
/* 
 * RETURNS
 *
 *   >0   is a valid IDCol
 *   -1   ERROR, less than 2 columns 
 *   -2   ERROR, less than 1 line
 *   -3   ERROR, no IDCol found on first line
 *   -4   ERROR, cannot read second line's IDCol
 *   -5   ERROR, second line's IDCol is not a 2 
 *   -6   ERROR, unable to read first line
 *   -7   ERROR, unable to read first line
 *
 ********************************************************************/
{
char colbuf[512];
char txtbuf[512];
int col = 2,
    ColCount = 0,
    LineCount = 0,
    IDCol = 0;

/* count lines */
if ((LineCount=CountLines(fp)) < 1) return(-2);

/* get first line and count its columns */
rewind(fp); /*this rewind is inappropriate. means fuinction only works if
			  top line is readable for columns; not the case with headerless
			  stuff*/

if (fgets(colbuf,512,fp)==NULL) return(-6);  /* skip header record */
if (fgets(colbuf,512,fp)==NULL);
if ((ColCount=CountColumns(colbuf)) < 2) return(-1);

/* find next column with number 1 */
while (col<=ColCount && col!=IDCol)
   {
   if (GetColumn(colbuf,col,txtbuf) < 0) return(-3);
   if (atoi(txtbuf)==1) IDCol=col;   
   else
      col++;
   }

/* Make sure that same column in line 2 has the number 2 */
if (fgets(colbuf,512,fp)==NULL) return(-7);
rewind(fp);
if (txtbuf == NULL) 
   return(IDCol);         /* A-OK, there is no second line */
else 
   {
   if (GetColumn(colbuf,IDCol,txtbuf) < 0)
      return(-4);         /* ERROR, can't read second line's IDCol */
   else if (atoi(txtbuf) != 2)
      return(-5);         /* ERROR, second line's IDCol is not a 2 */
   else
      return(IDCol);      /* A-OK, second line's IDCol has a two */
   }
}
