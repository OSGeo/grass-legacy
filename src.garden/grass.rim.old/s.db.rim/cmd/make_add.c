#define MAKE_ADD

#include <stdio.h>
#include "globals.h"
#include "make.h"



make_add(inp_buf)
   char *inp_buf;
{
   int pos, field_col, inp_length, temp_length;
   char field_type, field_name[FIELD_NAME_LENGTH+1];
   int split_field;

   pos = 0;
   inp_length = strlen(inp_buf);

   while (pos<inp_length) {
      /* initialize the field variables */
      field_type = 0;
      field_name[0] = field_type;
      split_field = field_col = 0;

      /* scan just after the first '~' (the next field) */
      pos += strcspn(inp_buf+pos, TILDE);
      field_col = pos;
      pos++;

      if (pos<inp_length) {
         /* get the field type */
         field_type = inp_buf[pos++];

         /* get the field name */
         temp_length = strcspn(inp_buf+pos, SPECIAL_CHARS);
         strncpy(field_name, inp_buf+pos, temp_length);
         field_name[temp_length] = 0;
         pos += temp_length;

         /* check what character stopped the span and move to end of field */
         if (inp_buf[pos]==SPACE_CHAR || inp_buf[pos]==TAB_CHAR
              || inp_buf[pos]==NULL_CHAR)
            {} /* do nothing */
         else if (inp_buf[pos]==TILDE_CHAR)
            pos += strspn(inp_buf+pos, TILDE);
         else if (inp_buf[pos]==DOT_CHAR) {
            pos++;
            sscanf(inp_buf+pos, "%d", &split_field);
            pos+=strspn(inp_buf+pos, DIGITS);
            pos+=strspn(inp_buf+pos, TILDE);
         }
         else {
            fprintf(Outfile,"\nForm Definition Error: Illegal special character '%c' in field name.\n", inp_buf[pos]);
            pos = inp_length;
            Make_OK = FALSE;
            break;
         }

         /* now at end of field */
         /* check for valid field type */
         if (field_type==S_FIELD_CHAR) {
            if ((Required_fields & S_FIELD)!=0) {
               fprintf(Outfile,"\nForm Definition Error: More than one fields of type 's' (site).\n");
               Make_OK = FALSE;
            }
            else
               Required_fields += S_FIELD;
         }
         if (field_type==X_FIELD_CHAR) {
            if ((Required_fields & X_FIELD)!=0) {
               fprintf(Outfile,"\nForm Definition Error: More than one fields of type 'x' (easting).\n");
               Make_OK = FALSE;
            }
            else
               Required_fields += X_FIELD;
         }
         if (field_type==Y_FIELD_CHAR) {
            if ((Required_fields & Y_FIELD)!=0) {
               fprintf(Outfile,"\nForm Definition Error: More than one fields of type 'y' (northing).\n");
               Make_OK = FALSE;
            }
            else
               Required_fields += Y_FIELD;
         }

         if (strchr(FIELD_TYPES, field_type)==NULL) {
            fprintf(Outfile,"\nForm Definition Error: Illegal field type '%c' in field definition.\n", field_type);
            pos = inp_length;
            Make_OK = FALSE;
            break;
         }

         /* only text fields are allowed to be split */
         if (split_field!=0 && field_type!=T_FIELD_CHAR) {
            fprintf(Outfile,"\nForm Definition Error: Only Text fields can be split.\n");
            pos = inp_length;
            Make_OK = FALSE;
            break;
         }

         /* check for acceptable field name length */
         if (strlen(field_name)>FIELD_NAME_LENGTH || strlen(field_name)<1) {
            fprintf(Outfile,"\nForm Definition Error: Field name too short or too long (1<=length<=%d).\n", FIELD_NAME_LENGTH);
            pos = inp_length;
            Make_OK = FALSE;
            break;
         }

         /* check to see if beyond the allowed number of fields */
         if (Field_num>=MAX_FIELDS) {
            fprintf(Outfile, "\nForm Definition Error: Too many fields (only %d fields allowed).", MAX_FIELDS);
            pos = inp_length;
            Make_OK = FALSE;
            break;
         }

         /* save field information in Field_info array */
         save_finfo(Field_num, field_name, 0, field_type, Line_num, field_col,
                     pos-field_col, 0, -split_field);
         if (Field_num<MAX_FIELDS) Field_num++; /* global var */

      } /* end of if */
      else if (inp_buf[pos-1]==TILDE_CHAR) {
         fprintf(Outfile,"\nForm Definition Error: Field incompletely defined.\n");
         pos = inp_length;
         Make_OK = FALSE;
         break;
      }

   } /* end of while loop */

   /* Add inp_buf to the screenlayout database */
   fprintf(Tempf, "load screenlayout\n");
   fprintf(Tempf, "%d \"%s\"\n", Line_num, inp_buf);
   fprintf(Tempf, "end\n");
   Line_num++; /*global var*/

}


