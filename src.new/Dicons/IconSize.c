
#include <stdio.h>

/**************************************************************/
IconSize(fp,height,width,cx,cy)
FILE *fp;
int *height,
    *width,
    *cx,
    *cy;
/**************************************************************/
{
int colcount=0,c;
*height=0;
*width=0;
*cx=0;
*cy=0;

rewind(fp);
do {
   c = fgetc(fp);
   if (c=='\n')
      {
      (*height)++;
      if (colcount>*width)
         *width=colcount;
      colcount=0;
      }
   else if (c=='x' || c==' ')
      colcount++;
   else if (c=='+')
      {
      colcount++;
      *cx=colcount;
      *cy=*height;
      }
   }
while (c!=EOF);
if (*cy==0)
   *cy==*height/2;
if (*cx==0)
   *cx==*width/2;
rewind(fp);
}
