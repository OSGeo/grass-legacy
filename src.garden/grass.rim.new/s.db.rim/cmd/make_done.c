#define MAKE_DONE

#include "gis.h"
#include <sys/types.h>
#include <sys/dir.h>
#include "globals.h"
#include "parse.h"
#include "make.h"
#include "rim.h"

make_done()
{
   char buffer[MAX_BUFFER];
   int  offset, entry, count;
   char field_type;

   /* check for valid screen length */
   if (Line_num>SCREEN_LENGTH) {
      fprintf(Outfile,"\nForm Definition Error: Form too long (greater than %d lines).\n", SCREEN_LENGTH);
      Make_OK = FALSE;
   }

   /* check for exactly one of s, x, & y field types */
   if (Required_fields != S_FIELD+X_FIELD+Y_FIELD) {
      fprintf(Outfile,"\nForm Definition Error: Required field(s) missing -\n");
      if (!(Required_fields & S_FIELD))
         fprintf(Outfile,"Type 's' (Site number).\n");
      if (!(Required_fields & X_FIELD))
         fprintf(Outfile,"Type 'x' (East Coordinate).\n");
      if (!(Required_fields & Y_FIELD))
         fprintf(Outfile,"Type 'y' (North Coordinate).\n");
      fprintf(Outfile,"\n");
      Make_OK = FALSE;
   }

   /* resolve the split field numbers into next_field numbers and
      handle number of floating decimal places for f, x, y types */
   if (res_split_f()==FALSE) Make_OK = FALSE;

   /* acknowledge form definition is ok */
   if (Make_OK == TRUE) {
      fprintf(Outfile,"\nForm Definition Accepted.\n");
      sprintf(RIM_db_path, "%s/%s/rim/sites", G_location_path(), G_mapset() );

    /* make sure that the directory exists... */
      G__make_mapset_element("rim/sites");

      fprintf(Outfile,"Building the RIM database in directory %s\n",
            RIM_db_path);

      /* define all of the columns */
      count = 0;
      fprintf(Tempf, "define %s\n", File_name);
      fprintf(Tempf, "columns\n");
      while (count<Field_num) {
         field_type = Field_info[count].column_type;
         if (Field_info[count].next_field[0]==count ||
               Field_info[count].next_field[0]==MAX_FIELDS) {
            if (field_type==T_FIELD_CHAR)
               fprintf(Tempf, "%s\t%s\t%d\n", Field_info[count].column_name,
                        rim_type(field_type, buffer), rim_text_len(count));
            else if (field_type==F_FIELD_CHAR || field_type==X_FIELD_CHAR
                     || field_type==Y_FIELD_CHAR)
               fprintf(Tempf, "%s\t%s\t\tformat f%d.%d\n",
                   Field_info[count].column_name, rim_type(field_type, buffer),
                   Field_info[count].length, Field_info[count].next_field[1]);
            else
               fprintf(Tempf, "%s\t%s\n", Field_info[count].column_name,
                        rim_type(field_type, buffer));
         }

         /* hold onto the field numbers for s, x, and y */
         if (field_type==S_FIELD_CHAR)
            Site_field = count;
         if (field_type==Y_FIELD_CHAR)
            North_field = count;
         if (field_type==X_FIELD_CHAR)
            East_field = count;
         count++;
      }

      /* define the data table */
      fprintf(Tempf, "tables \n");
      entry = 0;

      sprintf(buffer,"data with %s %s %s ", Field_info[Site_field].column_name,
                                          Field_info[East_field].column_name,
                                          Field_info[North_field].column_name);
      Field_info[Site_field].rec_offset = 0;
      Field_info[East_field].rec_offset = INT_OFFSET;
      offset = Field_info[North_field].rec_offset = INT_OFFSET + DOUBLE_OFFSET;
      count = 0;
      entry = 4;
      offset += DOUBLE_OFFSET;

      while (count<Field_num) {
         field_type = Field_info[count].column_type;

         /* if this field is data (not site, north, east, or split text)
            add it to the list */
         if (field_type!=S_FIELD_CHAR && field_type!=X_FIELD_CHAR &&
               field_type!=Y_FIELD_CHAR && !(field_type==T_FIELD_CHAR
               && Field_info[count].next_field[0]!=MAX_FIELDS
               && Field_info[count].next_field[0]!=count)) {
            if (entry%8 == 0) {
               fprintf(Tempf, "%s + \n", buffer);
               strcpy(buffer, "");
               entry = 0;
            }
            strcat(buffer, Field_info[count].column_name);
            strcat(buffer, " ");
            Field_info[count].rec_offset = offset;
            offset += field_offset(count);
            entry++;
         }
         count++;
      } /* end of while (count<Field_num) */

      fprintf(Tempf, "%s\n", buffer);
      fprintf(Tempf, "end\n");
/*    fprintf(Tempf, "build key for %s in data\n",
                     Field_info[Site_field].column_name);
*/
      fclose(Tempf);

      /* invoke rim in a shell to process the temp file */
      G_system(buffer);
      sprintf(buffer,"cd %s;%s %s 2>&1 >%s;exit 0",RIM_db_path,RIM_COMMAND_STR,
               Temp_name, Tempdumpf);
      if (G_system(buffer)!=0) {
         unlink(Temp_name);
         G_fatal_error("Unable to invoke RIM on the temp file to define the data base.");
      }
      unlink(Temp_name);
      fprintf(Outfile,"\nDatabase is now 'made'.\n");

      /* Now open the database */
      strcpy(RIM_db_mapset, G_mapset());
      if (open_db(File_name,TRUE)==FALSE)
			  G_fatal_error("Unable to open database that was just made.");

      /* Put the Field_info in the database and lose it again */
      put_field_info();
      close_db(File_name);

      /* Now reopen it to use */
      if (open_db(File_name,FALSE)==FALSE)
			  G_fatal_error("Unable to open database that was just made.");

      get_field_info();
      init_field_val();

   } /* end of if Make_OK == TRUE */
   else {
      fprintf(Outfile,"\nMake failed due to Form Definition Errors.\n");
      /* alert parser of this status */
      parser(P_INST_DB_NA,"");
      unlink(Temp_name);
   }

   /* change prompt back to top_level prompt */
   strcpy(Prompt, PROMPT);

}

