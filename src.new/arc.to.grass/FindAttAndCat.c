
#include <stdio.h>

int FindAttAndCat(fp,AttCol,CatCol,IDCol,IDNum,AttText,CatNum)
FILE *fp;      /* text file */
int  AttCol,   /* number of attribute column */
     CatCol,   /* number of category column */
     IDCol,    /* number of id column */
     IDNum;    /* id number */ 
char *AttText; /* attribute text */
int  *CatNum;
{
char txtbuf[512];
char colbuf[512];
int  done=0;
int  RowCount=0;

rewind(fp);
while (1)
   {
   if (fgets(colbuf,512,fp)==NULL) return(-1);
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
