#include <stdio.h>
#include <string.h>

/**********************************************************************/

int CountColumns (char *txt)
{
int i=0,
    count=0,
    flag=0;

while (txt[i]!='\0')
   {
   /*fprintf (stderr, "'%c'", txt[i]);*/ /*DEBUG*/
   /*newline trap added--dks 2-91*/
   if (txt[i]!=' ' && txt[i] != '\n') 
      flag=1;
   else if (txt[i]==' ' && flag==1)
      {
      count++;
/*	  fprintf (stderr, "\ncount = %d\n", count);*/ /*DEBUG*/
      flag=0;
      }
   i++;
   }
if (flag==1)
   count++;
 /* fprintf (stderr, "\nreturning: count = %d\n", count);*/ /*DEBUG*/
return(count);
}

/**********************************************************************/

int 
GetColumn (char *txt, int colnum, char *coltxt)
{
int  i=0,             /* which character are we on? */
     count=0,         /* which column are we on? */
     bufi=0,
     got_a_string=0,
     flag=0;
char buf[512];

while (txt[i]!='\0')
   {
   if (txt[i]!=' ') 
      { 
      buf[bufi++] = txt[i];
      flag=1;
      }
   else if (txt[i]==' ' && flag==1)
      {
      buf[bufi++]='\0';
      count++;
      got_a_string=1;
      }
   if (count==colnum)
      {
      strcpy(coltxt,buf);
      return(1);
      }
   else if (got_a_string) 
      {
      buf[0]='\0';
      bufi=0; 
      got_a_string=0;
      flag = 0;
      }
   i++;
   }
if (flag==1)
   {
   buf[bufi]='\0';
   strcpy(coltxt,buf);
   return(1);
   }
return(-1);
}
