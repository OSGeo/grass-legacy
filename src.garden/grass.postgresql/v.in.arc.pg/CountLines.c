
#include <stdio.h>

/**************************************************************/
int 
CountLines (FILE *fp)
/**************************************************************/
{
int count=0,c;
rewind(fp);
do {
   c = fgetc(fp);
   if (c=='\n')
      count++;
   }
while ( c!=EOF );
rewind(fp);
return(count); 
}
