#define SHOW

#include <stdio.h>
#include "globals.h"
#include "rim.h"


show()
{
   int status, line_num;
   char line_text[INP_SIZ];

   crim_w_err(SCREEN_TABLE, "select from screenlayout");

   while ((status = crimdm(SCREEN_TABLE, GET, Rim_buffer))!=RIM_EOT) {
      if (status!=0) rim_error(status);
      ret_s_table(Rim_buffer, &line_num, line_text);
      fprintf(Outfile, "\n%s", line_text);
   }

   fprintf(Outfile, "\n\n");
}

