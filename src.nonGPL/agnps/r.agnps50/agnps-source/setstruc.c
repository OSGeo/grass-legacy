/* get_cells */
 
#ifdef _DOS
 
 #include <stdio.h>
 #include <alloc.h>
 #include <stdlib.h>
 #include "input.h"
 #include "debugflg.h"

#else

#include <stdio.h>
#include "input.h"
#include "debugflg.h"

#endif
 
extern COLUMN_INFOPTR *columndata;
extern SEDIMENT_DATA outlet_sediment[7];
extern int columns;
 
 
#ifdef _UNIX_K_AND_R
 
 extern void memory_out();
 
#else
 
 extern void memory_out(int location, int column_number);
 
#endif
 
 
void setstruc()
 
{
int counter;
 
#ifdef _DOS
columndata = (COLUMN_INFOPTR*) farcalloc(columns+2,sizeof(COLUMN_INFOPTR));

#else
columndata = (COLUMN_INFOPTR*) calloc(columns+2,sizeof(COLUMN_INFOPTR));

#endif

if (columndata == NULL)
   memory_out(0,0);

for (counter=1; counter<columns+2; counter++)
   {
    columndata[counter] = (COLUMN_INFOPTR) calloc(1, sizeof(COLUMN_INFO));
 
   if (columndata[counter] == NULL)
       memory_out(1,counter);
 
   columndata[counter]->feedlot     = NULL;
   columndata[counter]->gully       = NULL;
   columndata[counter]->pesticide   = NULL;
   columndata[counter]->nonfeedlot  = NULL;
   columndata[counter]->impound     = NULL;
   columndata[counter]->feed_totals = NULL;
 
   columndata[counter]->channel = (CHANNEL_INFO*)
					calloc(1,sizeof(CHANNEL_INFO));
 
   if (columndata[counter]->channel == NULL)
       memory_out(2,counter);
 
   columndata[counter]->management = (MANAGEMENT_INFO*)
					calloc(1,sizeof(MANAGEMENT_INFO));
 
   if (columndata[counter]->management == NULL)
       memory_out(3,counter);
 
   columndata[counter]->slope = (SLOPE_INFO*) calloc(1,sizeof(SLOPE_INFO));
 
   if (columndata[counter]->slope == NULL)
       memory_out(4,counter);
 
   columndata[counter]->soil = (SOIL_INFO*) calloc(1,sizeof(SOIL_INFO));
 
   if (columndata[counter]->soil == NULL)
       memory_out(5,counter);
 
   columndata[counter]->runoff = (RUNOFF_INFO*) calloc(1,sizeof(RUNOFF_INFO));
 
   if (columndata[counter]->runoff == NULL)
       memory_out(6,counter);
 
   columndata[counter]->accumulated = (ACCUM_VALUES*) calloc( 1,
				   sizeof(ACCUM_VALUES));
   if (columndata[counter]->accumulated == NULL)
       memory_out(700, counter);
 
/*   fprintf (stderr,"There is %lu bytes after loading up cell num %d \n",
       (unsigned long) coreleft(),counter); */
 
   }
 
 
for (counter=0; counter<=6; counter++)     /* index of 0 is never used */
   {
   outlet_sediment[counter].area_weighted_erosion = 0.0;
   outlet_sediment[counter].gully_erosion         = 0.0;
   }
 
 
 
 
return;
}
