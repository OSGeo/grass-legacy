#define MAKE_INIT

#include <stdio.h>
#include "globals.h"
#include "make.h"
#include "rim.h"
#include "gis.h"



make_init()
{
   Make_OK = TRUE;
   Line_num = Field_num = 0;
   Required_fields = 0x0;

/* get and open a temporary file */
   Temp_name = G_tempfile();
   if ((Tempf = fopen(Temp_name, "w"))==NULL)
      G_fatal_error("Couldn't open temp file.");

/* write table definition to temp file */
   fprintf(Tempf, "define %s\n", Base_Name(File_name));
   fprintf(Tempf, "columns\n");
   fprintf(Tempf, "field_num    int\n");
   fprintf(Tempf, "field_name   text    16\n");
   fprintf(Tempf, "field_type   text    1\n");
   fprintf(Tempf, "rec_offset   int\n");
   fprintf(Tempf, "line_num     int\n");
   fprintf(Tempf, "column_num   int\n");
   fprintf(Tempf, "length       int\n");
   fprintf(Tempf, "split_field0 int\n");
   fprintf(Tempf, "split_field1 int\n");
   fprintf(Tempf, "line_text    text    80\n");
   fprintf(Tempf, "map_name     text    20\n");
   fprintf(Tempf, "map_mapset   text    20\n");
   fprintf(Tempf, "map_id       int\n");
   fprintf(Tempf, "tables\n");
   fprintf(Tempf, "fieldnames with field_num field_name rec_offset field_type line_num column_num length split_field0 split_field1\n");
   fprintf(Tempf, "screenlayout with line_num line_text\n");
   fprintf(Tempf, "referencemaps with map_id map_name map_mapset\n");
   fprintf(Tempf, "end\n");

   /* change the prompt for the make command */
   strcpy(Prompt, MAKE_PROMPT);

}
