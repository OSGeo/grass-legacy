
#define FIELD_VAL
#include "gis.h"
#include "globals.h"
#include "make.h"
#include "rim.h"

/* This routine initializes all of the Field_info pointers and allocates
   the space for the values*/
init_field_val()
{
  int count, temp_cnt;
  char *temp_ptr;
  unsigned size;

  if(Field_num!=0) {
    for (count=0; count<Field_num; count++) {
      switch (Field_info[count].column_type) {
      case S_FIELD_CHAR:
      case I_FIELD_CHAR:
      case M_FIELD_CHAR:
        if (Field_info[count].value==NULL)
          Field_info[count].value = G_malloc(sizeof(int));
        else
          Field_info[count].value = G_realloc(Field_info[count].value,
                                              sizeof(int));
        break;
      case F_FIELD_CHAR:
      case X_FIELD_CHAR:
      case Y_FIELD_CHAR:
        size = sizeof(double);
        if (Field_info[count].value==NULL)
          Field_info[count].value = G_malloc(sizeof(double));
        else
          Field_info[count].value = G_realloc(Field_info[count].value,
                                              sizeof(double));
        break;
      case T_FIELD_CHAR:
      case V_FIELD_CHAR:
        size = rim_text_len(count) + 1;
        /* If this is a non-split text field */
        if (Field_info[count].next_field[0]==MAX_FIELDS) {
          if (Field_info[count].value==NULL)
            Field_info[count].value = G_malloc(size);
          else
            Field_info[count].value = G_realloc(Field_info[count].value,
                                                size);
        }
        else {
          /* If this is the beginning of a split text field */
          if (Field_info[count].next_field[0]==count) {
            temp_cnt = count;
            while ((temp_cnt=Field_info[temp_cnt].next_field[1])
                   !=count) size++;
            if (Field_info[count].value==NULL)
              Field_info[count].value = G_malloc(size);
            else
              Field_info[count].value=G_realloc(Field_info[count].value,
                                                size);
            /* step thru the sub fields pointing them into the string */
            temp_ptr = Field_info[count].value;
            size = Field_info[count].length + 1;
            temp_cnt = Field_info[count].next_field[1];
            do {
              Field_info[temp_cnt].value = temp_ptr + size;
              size += Field_info[temp_cnt].length + 1;
              temp_cnt = Field_info[temp_cnt].next_field[1];
            } while (temp_cnt!=count);
          }
        }
        break;
      default:
        G_fatal_error("Unknown field type in init_field_val(). %d", Field_info[count].column_type);
      }
    }
  }
  else {
    for (count=0; count<MAX_FIELDS; count++)
      Field_info[count].value = NULL;
  }
}



