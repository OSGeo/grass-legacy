#define USE_TABLE

#include <stdio.h>
#include "globals.h"


#define UNUSED 0
#define USED 1
#define NUMBER_OF_TABLES 10


int tables_used[NUMBER_OF_TABLES] = {UNUSED,UNUSED,UNUSED,UNUSED,UNUSED,
                                       UNUSED,UNUSED,UNUSED,UNUSED,UNUSED};


/* sets aside a table number for use, to avoid trying to use the same
   table number for more than one table at a time during heavy access.
   This routine returns the table number that was allocated or zero if
   there was an error.
   */
use_table(table_var, table_num, warn)
int *table_var, table_num, warn;
{
   char local_buf[100];

   if (table_num<=NUMBER_OF_TABLES && table_num>0) {
      if (tables_used[table_num-1]==UNUSED) {
         tables_used[table_num-1] = USED;
         *table_var = table_num;
         return(table_num);
      }
      else if (warn==TRUE) {
         sprintf(local_buf,
               "Attempt to use table #%d when it was already allocated.",
               table_num);
         G_fatal_error(local_buf);
       }
      else return(0);
   }
   else {
       sprintf(local_buf,
            "Attempt to use table #%d which is an invalid table number.",
            table_num);
       G_fatal_error(local_buf);
   }
}


/* This routine deallocates a table number for use.  Its return value
   has no significance.  It sets the value of table_var to UNUSED.
   */
free_table(table_var, table_num)
int *table_var, table_num;
{
   char local_buf[100];

   if (table_num<=NUMBER_OF_TABLES && table_num>0) {
         tables_used[table_num-1] = UNUSED;
         *table_var = UNUSED;
      }
   else {
       sprintf(local_buf,
            "Attempt to free table #%d which is not a valid table number.",
            table_num);
       G_fatal_error(local_buf);
   }
}


