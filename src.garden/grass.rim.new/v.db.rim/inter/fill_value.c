#define FILL_VALUE
#include "gis.h"
#include "globals.h"
#include "make.h"
#include "rim.h"


/* This routine is called to fill the field value from the RIM buffer.
   It puts the value into the space indicated by the Field_info.value
   pointer.  */
fill_value(field_num)
   int field_num;
{
  int first, offset;
  char tempstr[4000];

  switch (Field_info[field_num].column_type) {
  case S_FIELD_CHAR:
  case I_FIELD_CHAR:
  case M_FIELD_CHAR:
    retr_buf_i(&Rim_buffer[Field_info[field_num].rec_offset],
               (int *) Field_info[field_num].value);
    break;
  case X_FIELD_CHAR:
  case Y_FIELD_CHAR:
  case F_FIELD_CHAR:
    retr_buf_d(&Rim_buffer[Field_info[field_num].rec_offset],
               (double *) Field_info[field_num].value);
    break;
  case V_FIELD_CHAR:
  case T_FIELD_CHAR:
    /* If it is not a split text field retrieve it normally */
    if ((first=Field_info[field_num].next_field[0])==MAX_FIELDS)
      retr_buf_t(&Rim_buffer[Field_info[field_num].rec_offset],
                 Field_info[field_num].value,
                 Field_info[field_num].length);
    /* otherwise retrieve the combined the split text field */
    else {
      offset=Field_info[field_num].value - Field_info[first].value;
      retr_buf_t(&Rim_buffer[Field_info[first].rec_offset],
                 tempstr, rim_text_len(first));
      strncpy(Field_info[field_num].value, &tempstr[offset],
              Field_info[field_num].length);
      Field_info[field_num].value[Field_info[field_num].length] = '\0';
    }
    G_strip(Field_info[field_num].value);
    break;
  default:
    G_fatal_error("Unknown field type in fill_value().");
  }
}

fill_values()
{
int i;

for (i=0; i<Field_num; i++)
        fill_value(i);
}
