#define DELETE
#include "gis.h"
#include "globals.h"
#include "rim.h"


#define DELETE_PROMPT "delete"


delete_init()
{
   strcpy(Prompt, DELETE_PROMPT);
}

delete_record(inp_buf)
char *inp_buf;
{
   int recordnum;
   char tempbuf[100];

   if (sscanf(inp_buf, "%d", &recordnum)!=1) {
      fprintf(Outfile, "Unable to scan the record number from the input line; nothing deleted.\n");
      return;
   }

   sprintf(tempbuf, "select from data where %s = %d",
           Field_info[Sequence_field].column_name, recordnum);
   if (crim(DATA_TABLE, tempbuf)==RIM_EOT) {
      fprintf(Outfile, "Record with %s = %d not found.  It was not deleted.\n",
              Field_info[Sequence_field].column_name, recordnum);
      return;
   }

   crimdm_w_err(DATA_TABLE, GET, Rim_buffer);
   crimdm_w_err(DATA_TABLE, DEL, Rim_buffer);
   Number_of_records--;

   fprintf(Outfile, "Record %s = %d deleted.\n",
           Field_info[Sequence_field].column_name, recordnum);
}

delete_done()
{
   strcpy(Prompt, PROMPT);
}

delete_found_done()
{
   char tempbuf[50];
   struct query_record *curr_record;

   if (Last_record==NULL) {
      fprintf(Outfile, "Query list is empty.  No records were deleted.\n");
   }
   else {
      if (my_G_yes(Infile, Outfile, "Do you want to delete all the records in the query list?", 0)==0) {
         fprintf(Outfile, "No records deleted.\n");
      }
      else {
         curr_record = Record_list;
         do {
            sprintf(tempbuf, "%d", curr_record->record_number);
            delete_record(tempbuf);
         } while (curr_record++ != Last_record);
         fprintf(Outfile, "The records in the query list were deleted.\n");
         Last_record = NULL;
      }
   }
   strcpy(Prompt, PROMPT);
}

