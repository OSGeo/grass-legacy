#define ADD_CHANGE
#include "gis.h"
#include "globals.h"
#include "rim.h"
#include "make.h"


char *strpbrk();

static int change_list = 0;

#define ADD_PROMPT "add"
#define CHANGE_PROMPT "change"

int Add_ok, Found_site;
/* this array is used to determine if a field was changed. */
short int field_changed[MAX_FIELDS];

/* Set up for both an .add or a .change by clearing value data. */
add_change_init()
{
   int count;

   Add_ok = TRUE;
   Found_site = FALSE;
   clear_values();
   change_list = 0;
}


/* Get ready for the fields to add to the record. */
add_init()
{
   add_change_init();
   strcpy(Prompt, ADD_PROMPT);
}



/* Works through the input line and extracts the field name and the
   value and if the field name matches a field in this database it
   scans the value out of the input buffer and into the field value
   pointer */
add_change_field(inp_buf)
char *inp_buf;
{
   int count, f_num, split_num;
   char *buf_ptr, tempstr[100];

   /* isolate the field name from the input string */
   if ((buf_ptr=strpbrk(inp_buf, "\n")) != NULL) *buf_ptr='\0';
   count = strspn(inp_buf, " \t"); /* span over any white space */
   if (count == strlen(inp_buf)) return(0); /*blank line*/
   buf_ptr = inp_buf + count;
   count = strcspn(buf_ptr, ". \t");
   strncpy(tempstr, buf_ptr, count);
   tempstr[count]='\0';
   G_tolcase(tempstr);

   /* search for a field name that matches */
   f_num=0;
   while (f_num<MAX_FIELDS && strcmp(Field_info[f_num].column_name,tempstr)!=0)
      f_num++;
   if (f_num>=MAX_FIELDS) {
      Add_ok = FALSE;
      fprintf(Outfile, "Unknown field name: %s.\n", tempstr);
      return(-1);
   }
   /* scan the line for the value */
   else {
      buf_ptr = buf_ptr + count;
      /* if there is a '.' then it's a split field */
      if (*buf_ptr=='.') {
         if (sscanf(buf_ptr+1, "%d", &split_num)!=1) {
            fprintf(Outfile, "Unable to scan the split field number.\n");
            Add_ok = FALSE;
            return(-1);
         }
         count = strcspn(buf_ptr, " \t");
         buf_ptr = buf_ptr + count;
      }
      else
         split_num = 0;
      count = strspn(buf_ptr, " \t");
      buf_ptr = buf_ptr + count;
      if (val_from_str(&f_num, split_num, buf_ptr) != 1) {
         fprintf(Outfile, "Unable to scan the value from the input line.\n");
         Add_ok = FALSE;
         return(-1);
      }
      else field_changed[f_num] = TRUE;
      if (f_num==Site_field) Found_site = TRUE;
   }

}

/* Checks that a site number was given and there were no ill-formed
   inputs and them sends the fields to RIM. */
add_done()
{
   int count;
   char textbuf[100], tempstr[200];

#ifndef DBSITES
   /* Site field required */
   if (Found_site==FALSE) Add_ok=FALSE;
#endif

   if (Add_ok == TRUE) {
      /* cannot have more than one record with a given site number */
      sprintf(textbuf, "select from data where %s = %d",
              Field_info[Site_field].column_name,
              *((int *) Field_info[Site_field].value));
      if (crim(DATA_TABLE, textbuf) != RIM_EOT) {
         sprintf(tempstr, "Site number (%d) duplicated one in the database, not added.\n", *((int *) Field_info[Site_field].value));
         G_warning(tempstr);
         SLEEP3;
      }
      else {
         /* Put all the fields into the RIM buffer */
         fill_fields();
         crim_w_err(DATA_TABLE, "load data");
         crimdm_w_err(DATA_TABLE, LOAD, Rim_buffer);
         Number_of_sites++;
      }
   }
   else {
      fprintf(Outfile, "Error during .add; Record not added to data base.\n");
      SLEEP3;
   }

   strcpy(Prompt, PROMPT);
}


/*  gets ready for the fields for a .change command */
change_init(buf)
char *buf;
{
   int count;

   add_change_init();

   while(*buf) /* set up to change the whole query/find list */
     if (!strncmp(buf++," -l",3)) {
         change_list = 1;
         break;
     }

   for (count=0; count<Field_num; count++)
      field_changed[count]=FALSE;

   strcpy(Prompt, CHANGE_PROMPT);
}


change_done()
{
   struct query_site *s1;
   int count;
   char textbuf[100], tempstr[81];

#ifndef DBSITES
   /* Site field required */
   if (Found_site==FALSE && !change_list) Add_ok=FALSE;
   if (change_list && Last_site == NULL) {
      G_warning("No sites selected by .query or .find yet.  No changes made.");
      return(-1);
   }
#endif
   s1 = Site_list;
   if (Add_ok==TRUE)
   do {
      if (change_list)
              *((int *) Field_info[Site_field].value) = s1->site_number ;
      /* cannot have more than one record with a given site number */
      sprintf(textbuf, "select from data where %s = %d",
              Field_info[Site_field].column_name,
              *((int *) Field_info[Site_field].value));
      if ((count = crim(DATA_TABLE, textbuf)) == RIM_EOT) {
         G_warning("Site number in .change not in the database, it cannot be modified.\n");
         SLEEP3;
      }
      else if (count != 0) rim_error(count);
      else {
         /* get the record */
         crimdm_w_err(DATA_TABLE, GET, Rim_buffer);
         /* Put the modified fields into the RIM buffer */
         for (count=0; count<Field_num; count++) {
            if (field_changed[count]==TRUE) {
               if (Field_info[count].next_field[0]==MAX_FIELDS)
                  fill_field(count);
               else { /* a split field */
                  strcpy(tempstr, Field_info[count].value);
                  fill_value(count);
                  strcpy(Field_info[count].value, tempstr);
                  fill_field(count);
               }
            }
         }
         /* put the modified record back in the original records place */
         crimdm_w_err(DATA_TABLE, PUT, Rim_buffer);
         if (change_list) /* update the query list info */
           for (count=0; count<Field_num; count++) {
              if(count==East_field && field_changed[count]==TRUE)
                 s1->east  = *((double *) Field_info[East_field].value);
              if(count==North_field && field_changed[count]==TRUE)
                 s1->north = *((double *) Field_info[North_field].value);
            }

      }
   } while (change_list && (++s1 <= Last_site));

   else {
      G_warning("Error during change; Record not modified.\n");
      SLEEP3;
   }

   strcpy(Prompt, PROMPT);
}

