/*======================================================================
Filename:   utils.c
Module:	    i.landsat.tm
Author:	    Christopher Lesher
======================================================================*/

#include <string.h>
#include <ctype.h>
#include <math.h>
#include "landsat.h"
#include "local_proto.h"


/*======================================================================
			    StartProgress
			    PrintProgress

Display the progress of the import of a single band.  Show either percent
done or current line.  Do a fprintf(stderr, "\n") when after PrintProgress() is
called for the last time.
======================================================================*/
void StartProgress (Landsat *landsat, int band)
{
   if(landsat->quiet) return;
   if(landsat->percent)
      fprintf(stderr, "Importing Band %d Percent Done:   0", band);
   else
      fprintf(stderr, "Importing Band %d Reading line    1", band);
   fflush(stdout);
}

void PrintProgress (
    Landsat *landsat,
    int line,
    int total
)
{
   if(landsat->quiet) return;
   if(landsat->percent)
      fprintf(stderr, "\b\b\b%3d", (line*100)/total);
   else
      fprintf(stderr, "\b\b\b\b%4d", line);
   fflush(stdout);
}

void EndProgress (void)
{  fprintf(stderr, "\n"); }


/*======================================================================
				Field
Return pointer to string containing buffer[start] - buffer[end]
======================================================================*/
char *Field (char *buffer, int start, int end)
{
   static char holdthis[1000];

   strncpy(holdthis, buffer+start, end-start+1);
   holdthis[end-start+1]=0;
   return(holdthis);
}

/*======================================================================
				FieldBinary
Return interger converted from binarys stored at
buffer[start] - buffer[end]
Max lenth = 32
======================================================================*/
int FieldBinary (char *buffer, int start, int end)
{
   static unsigned char holdthis[33];
   int    i;
   int    val;
   int    length;
   double mult;


   length = end-start + 1;

   val = 0;
   for (i = 0; i < length; i++) {
     mult = pow((double) 256.0, (double) (length - 1 - i));
     val += (int) (mult * (unsigned char) buffer[start - 1 + i]);

   }
   return(val);
}




/*======================================================================
				 Ask

Ask user if we should continue.  If user just presses return to prompt,
return 0.  If user enters 'quit', return 1.
======================================================================*/
int Ask (char *prompt)
{
   char *p, buffer[10];

   fprintf(stderr, "%s", prompt);
   fgets(buffer, sizeof(buffer), stdin);
   p=buffer;
   while(*p) {
      *p = toupper(*p);
      ++p;
   }
   return(! strcmp(buffer, "QUIT"));
}
