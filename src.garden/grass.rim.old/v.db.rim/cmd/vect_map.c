
#define VECT_MAP

#include "gis.h"
#include "Vect.h"
#include "dig_atts.h"
#include "globals.h"
#include "rim.h"
#include "make.h"
#include "vect.h"


/* the header structure for vector maps is supposed to be called dig_head,
   but it is actually called head */

struct my_Map_info {
  int number;
  struct Map_info info;
};

struct list_member {
  int line_number;
  struct list_member *next;
};

struct list_head {
  int map_number;
  struct list_member *first;
};

static struct Categories Cats_struct;
static struct my_Map_info Curr_map;
static struct list_head Line_list;
static char quote_type,*catstring;
static int fixed_att,fixed_att_flag;

/* This routine creates a vector map after a successful query.*/
/* Parameters are the vector map name and optionally the field to */
/*   use as the category field in the attributes file */

vect_map(inp_buf)
char *inp_buf;
{
  struct query_record *New_rec_list, *New_last_rec, *s1;
  char *p,*p1,*p2, cmd[100], filename[200], catstr[102];
  int count;
  int status;
  FILE *fp_att;
  struct Map_info new_vect_map;
  int att_field_num, cat_field_num;
  char cat_field[INP_SIZ], att_field[INP_SIZ];
  int cur_line, cur_area;
  char tfc, cur_type;
  char tempstr[100],tempbuf[100];
  struct dig_head new_header;
  P_AREA *area_ptr;

  att_field_num = -1;
  cat_field_num = -1;

  if (Last_record == NULL) {    /* check for valid site list */
    G_warning("Query or find not run or no records selected.  No action taken");
    SLEEP3;
    return;
  }
  /* find and extract fixed cat string, if present */
  /* quote_type will be null if no catstring */
  catstring = catstr;
  p1 = inp_buf;
  while (*p1 && *p1!='\"' && *p1!='\'') p1++;
  if (quote_type = *p1++) {
    p2 = catstring;
    while (*p2 = *p1++)
       if ((*p2++  == quote_type) || (p2-catstring) >100)
            {*(p2-1) = '\0'; break;}
  }
  G_squeeze(inp_buf);
  /* check for the -s flag */
  if ((sscanf(inp_buf,"%s -s %s",cmd,filename)) == 2)
     return ( vect_seq_map(inp_buf) );
  if ((p=index(inp_buf,' '))==NULL) {
    G_warning(".vect_map needs a vector map name parameter. No action taken.");
    SLEEP3;
    return (-1);
  }
  p++;                          /* point to file name */
  sscanf(p,"%s",filename);

  /* get attribute field name and number, if it exists */
  p += strcspn(p, " \t");
  if (*p) {
    sscanf(p, "%s", att_field);
    G_tolcase(att_field);

    /* search for a field name that matches */
    fixed_att_flag=0;
    att_field_num=0;
    while (att_field_num<MAX_FIELDS && strcmp(Field_info[att_field_num].column_name,att_field)!=0)
      att_field_num++;
    if (att_field_num>=MAX_FIELDS)
     if (sscanf(att_field,"%d",&fixed_att)!=1) {
      fprintf(Outfile, "Unknown field name: %s.  No action taken.\n",
              att_field);
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
    /* get category decription field name and number, if it exists */
    p += strspn(p, " \t");
    p += strcspn(p, " \t");
    if (*p) {
      sscanf(p, "%s", cat_field);
      G_tolcase(cat_field);

      /* search for a field name that matches */
      cat_field_num=0;
      while (cat_field_num<MAX_FIELDS && strcmp(Field_info[cat_field_num].column_name,cat_field)!=0)
        cat_field_num++;
      if (cat_field_num>=MAX_FIELDS) {
       cat_field_num = -2;
       if (!quote_type) {
        fprintf(Outfile, "Unknown field name: %s.  No action taken.\n", cat_field);
        return(-1);
       }
      }
    }
  }



  /* open the new vector map */
  if (G_find_vector(filename,G_mapset()) != NULL) {
    sprintf(tempbuf,
         "Vector map <%s> exists! Do you want to replace it?", filename);
    if(my_G_yes(Infile, Outfile, tempbuf, 1) == 0) {
      fprintf(Outfile, "Vector Map <%s> NOT replaced.\n", filename);
      return(-1);
    }
  }
  if (Vect_open_new(&new_vect_map,filename) < 0)
    G_fatal_error("Can't open the new vector map in current mapset.");

  /* open attribute file if necessary */

  /*   1 jul 91   dpg
  if (att_field_num) {
  */

  /*  6 jul 91    msl
  if (att_field_num>=0) {
  */

  if (fixed_att_flag || att_field_num>=0) {
    if ((fp_att = G_fopen_new("dig_att", filename)) == NULL)
      G_fatal_error("Can't create the attributes file for new vector map.");
  }

  /* initialize the category structure, if necessary */
  if (cat_field_num>=0 || quote_type)
    G_init_cats((CELL)0, "Created by v.db.rim", &Cats_struct);

  /* copy and sort the query list */
  copy_sort_query_list(&New_rec_list, &New_last_rec);

  /* open the needed vector file and allocate a line list */
  open_ref_vect(New_rec_list->map_num, &Curr_map);
  /* build the new header for vector map */
  build_new_header(&new_header, &Curr_map);


  /* put the areas and lines in the list into the vector map */
  for (s1=New_rec_list; s1<=New_last_rec; s1++)
    {
      /* get the record from the data base, if needed */
      if (att_field_num>=0) {
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
        close_ref_vect(&Curr_map);
        open_ref_vect(s1->map_num, &Curr_map);
        /* update the new header for vector map */
        update_new_header(&new_header, &Curr_map);
      }

      /* process the vector */
      switch (s1->vect_type) {
      case AREA_CHAR:
        cur_area = dig_point_to_area(&(Curr_map.info), s1->east, s1->north);
/*DEBUG fprintf (stderr, "point to AREA (%lf,%lf) = %d\n", s1->east,s1->north, cur_line);*/
        if (V2_get_area(&(Curr_map.info), cur_area, &area_ptr)<0) {
          sprintf(tempstr, "The area number (%d) in record number (%d) not found.\nNot copied to the new map.", cur_area, s1->record_number);
          G_warning(tempstr);
        }
        else {
          cur_type = AREA;
          for (count=0; count<area_ptr->n_lines; count++) {
            cur_line = area_ptr->lines[count];
            read_write_line(s1, cur_line, &(Curr_map.info), cur_type,
                   &new_vect_map, fp_att, att_field_num, cat_field_num);
          }
        }
        break;

      case LINE_CHAR:
        /* get the line and add it to the binary vector file */
        cur_line = dig_point_to_line(&(Curr_map.info), s1->east,
                                     s1->north, LINE);
/*DEBUG fprintf (stderr, "point to LINE (%lf,%lf) = %d\n", s1->east,s1->north, cur_line);*/
        cur_type = LINE;
/*fprintf(stdout,"Returned line value: %d\n",cur_line);
fprintf(stdout,"Easting/Northing: %lf / %lf\n",s1->east,s1->north);*/

        /* deal with the fact that area edges can be labelled as lines too!! */
        if (cur_line == 0) 
	{
          cur_line = dig_point_to_line(&(Curr_map.info), s1->east,
                                       s1->north, AREA);
/*DEBUG fprintf (stderr, "point to AREAL (%lf,%lf) = %d\n", s1->east,s1->north, cur_line);*/
          cur_type = AREA;
        }
/*fprintf(stdout,"Returned line value: %d\n",cur_line);*/
        read_write_line(s1, cur_line, &(Curr_map.info), cur_type,&new_vect_map,
                        fp_att, att_field_num, cat_field_num);
        break;

      case SITE_CHAR:
        /* get the line and add it to the binary vector file */
        cur_line = dig_point_to_line(&(Curr_map.info), s1->east,
                                     s1->north, DOT);
        cur_type = DOT;

        read_write_line(s1, cur_line, &(Curr_map.info), cur_type,&new_vect_map,
                        fp_att, att_field_num, cat_field_num);
        break;
      default:
        sprintf(tempstr,"Record number %d has a bad vector type (%c). Not added to new map.", s1->record_number, s1->vect_type);
        G_warning(tempstr);
        break;
      } /* end of switch */
    } /* end of for loop */

  /* close the vector reference map and deallocate the space */
  close_ref_vect(&Curr_map);
  /* release temp query list  and buffer space */
  free(New_rec_list);

  /* write the header for the new vector map */
  Vect_copy_head_data(&new_header,&(new_vect_map.head));

  /* close the new attributes file, if there is one */

/*  if(att_field_num) msl 7/6/91 */

  if (fixed_att_flag || att_field_num>=0)
    fclose(fp_att);

  /* write the new cats file, if there is one */
  if (cat_field_num>=0 || quote_type) {
    if (G_write_vector_cats(filename, &Cats_struct)==-1)
      G_warning("Error writing the category descriptions!");
    G_free_cats(&Cats_struct);
  }

  /* close the new vector map */
  Vect_close(&new_vect_map);
  fprintf(Outfile, "\nVector map <%s> has been built.", filename);
  fprintf(Outfile, "\nRun v.support on this map to build the topology information\n");
}


/****************************************************************/



/* This routine initializes a line list */
  allocate_map_line_list(num)
     int num;
{
  Line_list.map_number = num;
  Line_list.first = NULL;
}


/* This routine opens the vector file and allocates the
   line list header.  It opens a vector file for level 2 access. */

open_ref_vect(num, map_struct)
     int num;
     struct my_Map_info *map_struct;
{
  char textbuf[100], tempstr[100];
  int id;
  char mapname[MAP_NAME_LENGTH+1], mapset[MAP_NAME_LENGTH+1];

  /* get mapname and mapset from the database */
  sprintf(textbuf, "select from referencemaps where map_id = %d", num);
  if (crim(MAP_TABLE, textbuf) == RIM_EOT) {
    sprintf(tempstr, "Map id number (%d) not found in the database.", num);
    G_fatal_error(tempstr);
  }
  crimdm_w_err(MAP_TABLE, GET, Rim_buffer);
  ret_m_table(Rim_buffer, &id, mapname, mapset);
  G_squeeze(mapname);
  G_squeeze(mapset);

  map_struct->number = num;

  /* open the vector files for level 2 access */
  Vect_set_open_level(2);
  Vect_open_old(&(map_struct->info), mapname, mapset);

  /* allocate the line list for this map */
  allocate_map_line_list(num);
}

/* This routine closes all of the vector maps and deallocates the space
   for the lines and map_info structures. */
close_ref_vect(map_struct)
     struct my_Map_info *map_struct;
{
  /* close file and free the structure */
  Vect_close(&(map_struct->info));

  /* free up the line list */
  free_list(Line_list.first);
}

free_list(myhead)
     struct list_member *myhead;
{
  struct list_member *ptr;

  /* if at the end of a list, there is nothing to free up */
  if (myhead==NULL) return;

  /* otherwise work down through the list freeing up the members */
  while ((ptr=myhead->next) != NULL) {
    free(myhead);
    myhead = ptr;
  }
  free(myhead);
}
/****************************************************************/

/* This routine returns 1 (true) if the line number (line_num) is in
   the list for the map number (map_num) given.
   Otherwise, it returns 0. */
int line_in_list(map_num, line_num)
     int map_num, line_num;
{
  struct list_member *ptr;
  int i;

  /* scan the list until end or find the line number */
  ptr = Line_list.first;
  while (ptr != NULL && ptr->line_number < line_num) {
    ptr = ptr->next; /* get next one on list */
  }
  if (ptr != NULL && ptr->line_number == line_num)
    return(1);
  else
    return(0); /* either at the end of list or number doesn't match */
}


/* This routine adds a line number to a map's line list */
/* It has been assumed that the line number to be added is not already
   on the list! (check with line_in_list() before calling this routine. */
add_line_to_list(map_num, line_num)
     int map_num, line_num;
{
  struct list_member *ptr, *last, *new;
  int i;

  /* scan the list until end or find a line number larger than line_num */
  ptr = Line_list.first;
  last = ptr;
  while (ptr != NULL && ptr->line_number < line_num) {
    last = ptr;
    ptr = ptr->next;
  }

  /* now insert a new element in the list here with line_num */
  new = (struct list_member *)G_malloc(sizeof(struct list_member));
  new->line_number = line_num;
  new->next = ptr;
  if (last == Line_list.first) { /* only if this is an empty list */
    Line_list.first = new;
  }
  else {
    last->next = new;
  }
}

/*****************************************************************/

/* Read a line from one of the source vector maps and write it to the
   new map.  Also, update the line list, attributes file and cats struct. */
read_write_line(rec, line, map, type, new_vect, fp_att, att, cat)
     struct query_record *rec;
     int line, att, cat;
     struct Map_info *map, *new_vect;
     FILE *fp_att;
     char type;
{
  static struct line_pnts *lpts = NULL;
  char tempstr[300];
  CELL t_att;

    if (lpts == NULL)
    {
      lpts = Vect_new_line_struct();
    }

  /* get the line and put it into the new file */
  line = (line)<0 ? -line : line; /* absolute value */
  if (line > 0) {               /* found a line */
    if (line_in_list(rec->map_num, line) == 0) {
      /*
      lpts = Vect_new_line_struct();
      */
      add_line_to_list(rec->map_num, line);
      V2_read_line(map, lpts, line);
      Vect_write_line(new_vect, type, lpts);
      /*
      Vect_destroy_line_struct(lpts);
      */
    }
    /* put an entry in the attributes file */
    if (att>=0)
    write_att(fp_att, rec->vect_type, rec->east, rec->north, 
                  *((int *)Field_info[att].value));
/*
      fprintf(fp_att,"%c  %12.2f  %12.2f  %9d         \n",
              rec->vect_type, rec->east, rec->north,
              *((int *)Field_info[att].value));
*/
    else
      if (fixed_att_flag)
      write_att(fp_att, rec->vect_type, rec->east, rec->north, 
                 fixed_att);
/*
        fprintf(fp_att,"%c  %12.2f  %12.2f  %9d         \n",
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

  else {
    sprintf(tempstr, "No line number (%d) found for record number %d.",
            line, rec->record_number);
    G_warning(tempstr);
  }
}


/* build the new header as a composite of the old headers */
build_new_header(new_head, map_struct)
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

update_new_header(new_head, map_struct)
     struct dig_head *new_head;
     struct my_Map_info *map_struct;
{
  if (new_head->orig_scale > map_struct->info.head.orig_scale)
    new_head->orig_scale = map_struct->info.head.orig_scale;

/*   7/9/91     msl
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

int map_cmp(r1, r2)
     struct query_record *r1, *r2;
{
  if (r1->map_num < r2->map_num) return (-1);
  if (r1->map_num = r2->map_num) return (0);
  return(1);
}


copy_sort_query_list(rec_list, last_rec)
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
        SIZEOF_QUERY_RECORD, map_cmp);
}
