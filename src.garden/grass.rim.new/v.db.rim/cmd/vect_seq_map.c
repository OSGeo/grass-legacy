#define VECT_SEQ_MAP

#include "gis.h"
#include "globals.h"
#include "rim.h"
#include "make.h"
#include "vect.h"
#include "Vect.h"
#include "dig_atts.h"

struct my_Map_info {
  int number;
  struct Map_info info;
};

struct Map_info new_vect_map;

static struct Categories Cats_struct;
static struct my_Map_info Curr_map;
static char quote_type,*catstring;
static int fixed_att,fixed_att_flag;

/* This routine creates a vector map after a successful query.
   Parameters are the vector map name, the field name containing the
   offset of the vector in the dig file, and optionally
   the fields to use as the attribute and category fields.
NOTE: dig files edited after the data base is loaded
      will very probably not work correctly.
*/
vect_seq_map(inp_buf)
char *inp_buf;
{
  struct query_record *New_rec_list, *New_last_rec, *s1;
  char *p,*p1,*p2, cmd[100], filename[100], catstr[102];
  int i, count;
  int status;
  FILE *fp_att;
  int att_field_num, cat_field_num, offset_field_num;
  long offset;
  char offset_field[INP_SIZ], cat_field[INP_SIZ], att_field[INP_SIZ];
  char tfc, tempstr[100],tempbuf[100];
  struct dig_head new_header;

  att_field_num = -1;
  cat_field_num = -1;
  *cat_field = *att_field = '\0';
  /* line below added 6/7/91  msl */
  fixed_att_flag=0;

  if (Last_record == NULL) {    /* check for valid site list */
    G_warning("Query or find not run or no records selected.  No action taken");
    SLEEP3;
    return (-1);
  }
  /* find and extract fixed cat string, if present */
  /* quote_type will be null if no catstring */
  catstring = catstr;
  p1 = inp_buf;
  while (*p1 && *p1!='\"' && *p1!='\'') p1++;
  if (quote_type = *p1++) {
    p2 = catstring;
    while (*p2 = *p1++)
       if ((*p2++  == quote_type)|| (p2-catstring)>100)
                       {*(p2-1) = '\0'; break;}
  }
/* decode the input line */
  G_squeeze(inp_buf);
  i = sscanf(inp_buf,"%s -s %s %s %s %s",
             cmd,offset_field,filename,att_field,cat_field);
  if (i < 3) {
    G_warning(".vect_map -s needs an offset field and vector map name.\n  No action taken.  See manual.");
    SLEEP3;
    return (-1);
  }
/* Check sequence number field */
    G_tolcase(offset_field);
    offset_field_num=0;
    while (offset_field_num<MAX_FIELDS &&
             strcmp(Field_info[offset_field_num].column_name,offset_field)!=0)
      offset_field_num++;
    if (offset_field_num>=MAX_FIELDS) {
      sprintf(cmd, "Unknown sequence field name: %s.  No action taken.\n",
              offset_field);
      G_warning(cmd);
#ifdef DBVECT
      SLEEP3;
#endif
      return(-1);
     }

  /* get attribute field name and number, if it exists */
  if (i > 3) {
    G_tolcase(att_field);
  /* search for a field name that matches */
    fixed_att_flag=0;
    att_field_num=0;
    while (att_field_num<MAX_FIELDS && strcmp(Field_info[att_field_num].column_name,att_field)!=0)
      att_field_num++;
    if (att_field_num>=MAX_FIELDS)
     if (sscanf(att_field,"%d",&fixed_att)!=1) {
      sprintf(cmd, "Unknown field name: %s.  No action taken.\n", att_field);
      G_warning(cmd);
#ifdef DBVECT
      SLEEP3;
#endif
      return(-1);
     }
     else {fixed_att_flag = 1; att_field_num = -2;}

    if (!fixed_att_flag) {
     tfc = Field_info[att_field_num].column_type;
     if (tfc!=I_FIELD_CHAR && tfc!=S_FIELD_CHAR && tfc!=M_FIELD_CHAR) {
      G_warning("Field to fill with attribute number not an integer field.  No action taken.");
#ifdef DBVECT
      SLEEP3;
#endif
      return(-1);
     }
    }
  }  /* end of if i>3  */

    /* get category decription field name and number, if it exists */
  if (i > 4) {
      G_tolcase(cat_field);
      /* search for a field name that matches */
      cat_field_num=0;
      while (cat_field_num<MAX_FIELDS && strcmp(Field_info[cat_field_num].column_name,cat_field)!=0)
        cat_field_num++;
      if (cat_field_num>=MAX_FIELDS) {
       cat_field_num = -2;
       if (!quote_type) {
        sprintf(cmd, "Unknown field name: %s.  No action taken.\n", cat_field);
        G_warning(cmd);
#ifdef DBVECT
        SLEEP3;
#endif
        return(-1);
       }
      }
    }

  /* open the new vector map */
  if (G_find_vector(filename,G_mapset()) != NULL) {
    sprintf(tempbuf,
         "Vector map <%s> exists! Do you want to replace it?", filename);
    if(my_G_yes(Infile, Outfile, tempbuf, 1) == 0) {
      fprintf(Outfile, "Vector map <%s> NOT replaced.\n", filename);
      return(-1);
    }
  }
  /*  if (Vect_open_new(new_vect_map, filename) < 0)  7/6/91 msl */

  if (Vect_open_new(&new_vect_map, filename) < 0)
    G_fatal_error("Can't open the new vector map in current mapset.");

  /* open attribute file if necessary */
  if (att_field_num>=0 || fixed_att_flag) {
    if ((fp_att = G_fopen_new("dig_att", filename)) == NULL)
      G_fatal_error("Can't create the attributes file for new vector map.");
  }

  /* initialize the category structure, if necessary */
  if (cat_field_num>=0 || quote_type)
    G_init_cats((CELL)0, "Created by v.db.rim", &Cats_struct);

  /* copy and sort the query list */
  copy_sort_query_lst(&New_rec_list, &New_last_rec);
  /* open the needed vector file  */
  open_ref_v(New_rec_list->map_num, &Curr_map);
  /* build the new header for vector map */
  build_new_hdr(&new_header, &Curr_map);

  /* put the areas and lines in the list into the vector map */
  for (s1=New_rec_list; s1<=New_last_rec; s1++)
    {
      /* get the record from the data base, if needed */
      if (offset_field_num > 0 || att_field_num > 4 || cat_field_num > 4)
        {
        sprintf(cmd, "select from data where %s = %d",
                Field_info[Sequence_field].column_name, s1->record_number);
        if ((status=crim(DATA_TABLE, cmd))>0)
          rim_error(status);
        else if (status==RIM_EOT) {
          sprintf(tempstr, "Unable to find %s %d in database.\nNot copied to the new vector map", Field_info[Sequence_field].column_name, s1->record_number);
          G_warning(tempstr);
#ifdef DBVECT
          SLEEP3;
#endif
          continue; /* skip back to the beginning of the loop */
        }
        else {
          crimdm_w_err(DATA_TABLE, GET, Rim_buffer);
          fill_values();
        }
      }

      /* check that the map number hasn't changed */
      if (s1->map_num != Curr_map.number) {
        close_ref_v(&Curr_map);
        open_ref_v(s1->map_num, &Curr_map);
        /* update the new header for vector map */
        update_new_hdr(&new_header, &Curr_map);
      }

      /* process the vector */
      if (offset_field_num>4)
         offset= (long) *((int *)Field_info[offset_field_num].value) ;
          else if (offset_field_num=0) offset = (long) s1->record_number;
            else G_fatal_error("Invalid offset field number\n");
/*   7/6/91 msl
      read_write_lne(s1, offset, &Curr_map, &new_vect_map, fp_att,
                    att_field_num, cat_field_num); */

      read_write_lne(s1, offset, &(Curr_map.info), &new_vect_map, fp_att,
                    att_field_num, cat_field_num);
   } /* end of for loop */

  /* close the vector reference map */
  close_ref_v(&Curr_map);

  /* write the header for the new vector map */
  Vect_copy_head_data(&new_header,&(new_vect_map.head));

  /* release temp query list  and buffer space */
  free(New_rec_list);

  /* close the new attributes file, if there is one */
  if(att_field_num>=0 || fixed_att_flag)
    fclose(fp_att);

  /* write the new cats file, if there is one */
  if (cat_field_num>=0 || quote_type) {
    if (G_write_vector_cats(filename, &Cats_struct)==-1)
      G_warning("Error writing the category descriptions!");
    G_free_cats(&Cats_struct);
  }

  /* close the new vector map */
  /* Vect_close(new_vect_map);   7/6/91 msl */

  Vect_close(&new_vect_map);
  fprintf(Outfile, "\nVector map <%s> has ben built.\n", filename);
  fprintf(Outfile, "Run support.vect on this map to build the topology information, if needed.\n");
}  /* end of main */


/****************************************************************/


/* This routine opens the vector file for level 1 access. */

open_ref_v(num, map_struct)
     int num;
     struct my_Map_info *map_struct;
{
  char textbuf[100];
  int id;
  char mapname[MAP_NAME_LENGTH+1], mapset[MAP_NAME_LENGTH+1];

  /* get mapname and mapset from the database */
  sprintf(textbuf, "select from referencemaps where map_id = %d", num);
  if (crim(MAP_TABLE, textbuf) == RIM_EOT) {
    sprintf(textbuf, "Map id number (%d) not found in the database.", num);
    G_fatal_error(textbuf);
  }
  crimdm_w_err(MAP_TABLE, GET, Rim_buffer);
  ret_m_table(Rim_buffer, &id, mapname, mapset);
  G_squeeze(mapname);
  G_squeeze(mapset);

  map_struct->number = num;

  /* open the vector files for level 1 access */
  Vect_set_open_level(1);

  /* if (1 != Vect_open_old(map_struct, mapname, mapset)) {   7/6/91 msl */

  if (1 != Vect_open_old(&(map_struct->info), mapname, mapset)) {
     sprintf(textbuf, "Vector map <%s in %s> could not be opened at level 1.",
              mapname,mapset);
     G_fatal_error(textbuf);
  }
}

/* This routine closes the current input vector map */
/*   7/6/91 msl
     struct Map_info *map_struct;
{
  Vect_close(map_struct);
}
*/

close_ref_v(map_struct)
     struct my_Map_info *map_struct;
{
  Vect_close(&(map_struct->info));
}

/****************************************************************/

/* Read a line from one of the source vector maps and write it to the
   new map.  Also, update the line list, attributes file and cats struct. */
read_write_lne(rec, offset, map, new_vect, fp_att, att, cat)
     struct query_record *rec;
     int att, cat;
     FILE *fp_att;
     long offset;
     struct Map_info *map, *new_vect;
{
  char tempstr[300];
  int np,type;
  struct line_pnts *lpts;
  CELL t_att;

  /* get the line and put it into the new file */
  lpts = Vect_new_line_struct();
  type = V1_read_line(map, lpts, offset);
  if (type < 0) {
    sprintf(tempstr, "No line found for offset %ld.", offset);
    G_warning(tempstr);
    return;
  }
  Vect_write_line(new_vect, type, lpts);
  Vect_destroy_line_struct(lpts);

  /* put an entry in the attributes file */
  if (att>=0)
      write_att(fp_att, rec->vect_type, rec->east, rec->north, 
                *((int *)Field_info[att].value));
/*    fprintf(fp_att,"%c  %12.2f  %12.2f  %9d         \n",
            rec->vect_type, rec->east, rec->north,
            *((int *)Field_info[att].value));
*/
  else
    if (fixed_att_flag)
      write_att(fp_att, rec->vect_type, rec->east, rec->north, 
                fixed_att);
/*      fprintf(fp_att,"%c  %12.2f  %12.2f  %9d         \n",
              rec->vect_type, rec->east, rec->north,
              fixed_att);
*/

  /* put an entry in the cats structure */
  if (cat>=0) {
    /* convert any value to string in tempstr */
    val_to_str(tempstr,cat);
    if (att>=0) {
      t_att = *((CELL *)Field_info[att].value);
      G_set_cat(t_att, tempstr, &Cats_struct);
    }
    else
      if (fixed_att_flag) {
        t_att = (CELL)fixed_att;
        G_set_cat(t_att, tempstr, &Cats_struct);
      }
  }

  if (cat==-2 && quote_type != '\0')
    {
      if (att>=0){
        t_att = *((CELL *)Field_info[att].value);
        G_set_cat(t_att, catstring, &Cats_struct);
      }
      else
        if (fixed_att_flag) {
          t_att = (CELL)fixed_att;
          G_set_cat(t_att, catstring, &Cats_struct);
        }
    }

}

/* build the new header as a composite of the old headers */
build_new_hdr(new_head, map_struct)
     struct dig_head *new_head;
     struct my_Map_info *map_struct;
{
  int i;
  char tempstr[100];
  FILE *pipe;

  /*get initial values for each header member */
  strcpy(new_head->organization, map_struct->info.head.organization);
  new_head->date[0] = '\0';
  new_head->your_name[0] = '\0';
  strcpy(new_head->map_name, "Created by v.db.rim on...");
  /* get the current date */
  pipe = popen("date +%m/%d/%y", "r");
  fscanf(pipe, "%10s", new_head->source_date);
  pclose(pipe);
  new_head->orig_scale = map_struct->info.head.orig_scale;
  new_head->plani_zone = map_struct->info.head.plani_zone;
  new_head->W = map_struct->info.head.W;
  new_head->E = map_struct->info.head.E;
  new_head->S = map_struct->info.head.S;
  new_head->N = map_struct->info.head.N;
  new_head->digit_thresh = 0.0;
  new_head->map_thresh = 0.0;
}

update_new_hdr(new_head, map_struct)
     struct dig_head *new_head;
     struct my_Map_info *map_struct;
{
  if (new_head->orig_scale > map_struct->info.head.orig_scale)
    new_head->orig_scale = map_struct->info.head.orig_scale;

/*    7/9/91    msl
  if (new_head->W < map_struct->info.head.W)
    new_head->W = map_struct->info.head.W;  get greatest westing 
  if (new_head->E > map_struct->info.head.E)
    new_head->E = map_struct->info.head.E;  get smallest easting 
*/

  if (new_head->W > map_struct->info.head.W)
    new_head->W = map_struct->info.head.W; /* get smallest westing */
  if (new_head->E < map_struct->info.head.E)
    new_head->E = map_struct->info.head.E; /* get greatest easting */

  if (new_head->N < map_struct->info.head.N)
    new_head->N = map_struct->info.head.N; /* get greatest northing */
  if (new_head->S > map_struct->info.head.S)
    new_head->S = map_struct->info.head.S; /* get smallest southing */
}

/*****************************************************************/

int map_comp(r1, r2)
     struct query_record *r1, *r2;
{
  return (r1->map_num - r2->map_num);
}


copy_sort_query_lst(rec_list, last_rec)
     struct query_record **rec_list, **last_rec;
{
  struct query_record *recptr;

  *rec_list = (struct query_record *) G_malloc((Last_record - Record_list + 1)
                                       * SIZEOF_QUERY_RECORD);
  *last_rec = *rec_list;
  for (recptr = Record_list; recptr<=Last_record; recptr++) {
    (*last_rec)->record_number = recptr->record_number;
    (*last_rec)->east = recptr->east ;
    (*last_rec)->north = recptr->north ;
    (*last_rec)->map_num = recptr->map_num ;
    (*last_rec)->vect_type = recptr->vect_type ;
    (*last_rec)++;
  }
  (*last_rec)--;

  qsort((char *)(*rec_list), (*last_rec)-(*rec_list) + 1,
        SIZEOF_QUERY_RECORD, map_comp);
}
