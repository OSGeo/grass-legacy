
#include <stdio.h>

int CountCats(fp,col)
FILE *fp;
int col;
/* 
 * RETURNS 
 * 
 *   >0   successful, category count
 *   -1   error 
 *
 */
{
char txtbuf[512];
char buf[512];
int  flag=0;
int  num;
int  max,min; 
int  count=0;

rewind(fp);
while (fgets(buf,512,fp)!=NULL)
   {
   if (GetColumn(buf,col,txtbuf)<0) return(-1);
   num=atoi(txtbuf);
   if (flag==0)
      {
      max=num;
      min=num;
      flag=1;
      }
   else
      {
      if (num>max)
         max=num;
      if (num<min)
         min=num;
      }
   }
count = max-min;
return(count);
}
