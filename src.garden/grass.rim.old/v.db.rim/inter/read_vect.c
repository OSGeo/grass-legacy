/* To add records to data base with locations from a vector map */

#include "gis.h"
#include "globals.h"
#include "make.h"
#include "vect.h"

extern int Found_vect;

read_vect(buffer)
     char *buffer;
{
  char file[50], *mapset;
  char att_field[FIELD_NAME_LENGTH+1], cat_field[FIELD_NAME_LENGTH+1];
  double east,north;
  char type, *buf;
  int attribute, category;
  int cat_field_num = 0, att_field_num = 0;
  struct Categories cats_table;
  int cat_field_len = 0;
  int count, seq, map_id_num;
  FILE *att_fp, *cat_fp;
  char line[61], tempstr[100];

  G_squeeze(buffer);

  /* get the map name and mapset */
  buf = buffer + strcspn(buffer," \t");
  buf += strspn(buf, " \t");
  if (! *buf) {
    G_warning("Data not added: .read_vect requires a vector map name.");
    SLEEP3;
    return(-1);
  }
  sscanf(buf, " %s ", file);
  G_squeeze(file);
  mapset = G_find_vector(file, "");
  if (mapset == NULL) {
    G_warning(".read_vect: Requested vector map not found in any current mapsets.");
#ifdef DBVECT
    SLEEP3;
#endif
    return (-1);
  }

  /* get the next available map_id number and add this map to the table */
  map_id_num = get_max_map() + 1;
  sprintf(tempstr, ".map %d %s ", map_id_num, file);
  map(tempstr);

  /* get attribute field name and number, if it exists */
  buf += strcspn(buf, " \t");
  if (*buf) {
    sscanf(buf, "%s", att_field);
    G_tolcase(att_field);

    /* search for a field name that matches */
    att_field_num=0;
    while (att_field_num<MAX_FIELDS && strcmp(Field_info[att_field_num].column_name,att_field)!=0)
      att_field_num++;
    if (att_field_num>=MAX_FIELDS) {
      fprintf(Outfile, "Unknown field name: %s.  Data not added.\n",att_field);
#ifdef DBVECT
      SLEEP3;
#endif
      return(-1);
    }
    if (Field_info[att_field_num].column_type != I_FIELD_CHAR) {
      G_warning("Field to fill with attribute number not an integer field.\nData not added.");
#ifdef DBVECT
      SLEEP3;
#endif
      return(-1);
    }
    /* get category decription field name and number, if it exists */
    buf += strspn(buf, " \t");
    buf += strcspn(buf, " \t");
    if (*buf) {
      sscanf(buf, "%s", cat_field);
      G_tolcase(cat_field);

      /* search for a field name that matches */
      cat_field_num=0;
      while (cat_field_num<MAX_FIELDS && strcmp(Field_info[cat_field_num].column_name,cat_field)!=0)
        cat_field_num++;
      if (cat_field_num>=MAX_FIELDS) {
        fprintf(Outfile, "Unknown field name: %s.  Data not added.\n", cat_field);
        return(-1);
      }
      if (Field_info[cat_field_num].column_type != T_FIELD_CHAR) {
        G_warning("Field to fill with category description not a text field.\nData not added.");
#ifdef DBVECT
        SLEEP3;
#endif
        return(-1);
      }
    }
  }

  /* Open the attributes file */
  att_fp = G_fopen_old("dig_att",file,mapset);
  if (att_fp == NULL) {
    G_warning(".read_vect: Could not open the requested vector map.\nData not added.");
#ifdef DBVECT
    SLEEP3;
#endif
    return (-1);
  }

  /* if needed open the categories file */
  if (cat_field_num) {
    if (G_find_file("dig_cats", file, mapset)==NULL) {
      G_warning("Unable to find the category file for the vector map.\nData not added.");
#ifdef DBVECT
      SLEEP3;
#endif
      return(-1);
    }
    if (G_read_vector_cats(file, mapset, &cats_table) != 0) {
      G_warning("Unable to read the category table for the vector map.\nData not added.");
      SLEEP3;
      return(-1);
    }
    cat_field_len = Field_info[cat_field_num].length-1;
  }

  count = 0;
  seq = get_max_seq();
  add_change_init();
  Found_vect = TRUE;

  while (fgets(line, 60, att_fp) != NULL) {
    if (sscanf(line,"%c %lf %lf %d ",&type,&east,&north,&attribute) == 4) {
      /* if the line starts with one of the correct type add it to db */
      if (type == AREA_CHAR || type == LINE_CHAR || type == SITE_CHAR) {
        seq++; count++;
        *((int *) Field_info[Sequence_field].value) = seq;
        *((double *) Field_info[East_field].value) = east;
        *((double *) Field_info[North_field].value) = north;
        sprintf(Field_info[Vect_type_field].value, "%c", type);
        *((int *) Field_info[Map_field].value) = map_id_num;
        if (att_field_num)
          *((int *) Field_info[att_field_num].value) = attribute;
        if (cat_field_num) {
          strncpy(Field_info[cat_field_num].value,
                  G_get_cat((CELL)attribute, &cats_table),
                  cat_field_len);
          Field_info[cat_field_num].value[cat_field_len] = '\0';
        }

        add_done();
      }
    }
    else
      G_warning("Bad line in vector attributes file, skipping...");
  }
  fclose(att_fp);
  fprintf(Outfile,"%d records added to the database.\n", count);
}


