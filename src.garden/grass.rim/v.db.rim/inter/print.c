#define PRINT
#include "gis.h"
#include "globals.h"
#include "rim.h"
#include "make.h"

#ifdef DBVECT
#define Outfile print_file
extern FILE *print_file;
#endif

#define LIST_OPTION 'l'
#define ADD_OPTION 'a'

print(inp_buf)
char *inp_buf;
{
   int count, list, add_list, status;
   struct query_record *curr_rec;
   char buffer[100];

   G_squeeze(inp_buf);
   count = strcspn(inp_buf, " \t");
   count += strspn(&inp_buf[count], " \t-");

   list = FALSE;
   add_list = FALSE;
   if (inp_buf[count]==LIST_OPTION) list = TRUE;
   if (inp_buf[count]==ADD_OPTION) {
      list = TRUE;
      add_list = TRUE;
   }

   if (Last_record!=NULL) {
      curr_rec = Record_list;
      do {
         sprintf(buffer, "select from data where %s = %d",
                 Field_info[Sequence_field].column_name,
                 curr_rec->record_number);
         if ((status=crim(DATA_TABLE, buffer))>0)
            rim_error(status);
         else if (status==RIM_EOT) {
            sprintf(buffer, "Unable to find %s %d in database.",
                    Field_info[Sequence_field].column_name,
                    curr_rec->record_number);
            G_warning(buffer);
         }
         else {
            crimdm_w_err(DATA_TABLE, GET, Rim_buffer);
            fill_values();
            if (list==TRUE) print_list(add_list);
            else print_form();
            fprintf(Outfile, "\n");
         }
      } while (curr_rec++!=Last_record);
   }
   else fprintf(Outfile, "No records selected.\n");
}

/* this routine sends the current record to Outfile in list format */
print_list(add_opt)
   int add_opt;
{
   int count, split, tempint;
   char buffer[100];

   if (add_opt==TRUE) fprintf(Outfile, ".add\n");
   for (count=0; count<Field_num; count++) {
      if (Field_info[count].column_type==T_FIELD_CHAR &&
          Field_info[count].next_field[0]!=MAX_FIELDS) {
         tempint = Field_info[count].next_field[0];  /*get first split field*/
         split = 1;
         while (tempint!=count) {
            split++;
            tempint = Field_info[tempint].next_field[1];
         }
      }
      else split = 0;

      if (split==0)
         fprintf(Outfile, "%-19s ", Field_info[count].column_name);
      else {
         sprintf(buffer, "%s.%d", Field_info[count].column_name, split);
         fprintf(Outfile, "%-19s ",buffer);
      }

      val_to_str(buffer, count);
      fprintf(Outfile, "%s\n", buffer);
   }
   if (add_opt==TRUE) fprintf(Outfile, ".end\n");

}


/* This routine sends a record to Outfile in the page layout that the
   database was defined by.  It assumes the fields were scanned from top
   to bottom and left to right, so that all of the fields on a given line
   are next to each other and the clumps of fields are in the same order as
   the lines on which they appeared. */
print_form()
{
   int status, line_num;
   char line_text[INP_SIZ+20];
   int count;
   char buffer[INP_SIZ+20];

   crim_w_err(SCREEN_TABLE, "select from screenlayout");
   count=0;

   while((status = crimdm(SCREEN_TABLE, GET, Rim_buffer))!=RIM_EOT) {
      if (status!=0) rim_error(status);
      ret_s_table(Rim_buffer, &line_num, line_text);
      strip_fields(line_text);

      while (Field_info[count].line_num==line_num) {
         val_to_str(buffer, count);
         strncpy(&line_text[Field_info[count].column_num], buffer,
                 strlen(buffer));
         count++;
      }
      fprintf(Outfile, "%s\n", line_text);
   }

}

