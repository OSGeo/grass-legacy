
/*****
first obj-attr combo

   serial_num = 1, fid = 1, obj_code, attr_code
   put attr_code prefix on attr_mod list

for each call (new obj-attr combo):

   increment serial_num
   add a record with ++serial_num, obj_code, attr_code 
     if new obj, increment fid before adding
   check attr_prefix against atr_mod_list, add to list if new

*******/

#include <stdio.h>
#include "sdts_globals.h"
#include "sdts_in.h"
#include "gis.h"
#include "Vect.h"
/*#include "dig_atts.h"*/


#define CHUNKSIZE  100

struct obj_attr_modn_list Obj_attr_modn;

struct attr_pnt_list *Attr_pnt_list;
struct obj_attr_list *Obj_attr_list;
struct ff_attr_list *FF_attr_list;
struct ff_elem_list *FF_elem_list;

struct area_pnt_list *Area_pnt_list;

struct obj_attr_list **Attr_obj_list; /*will point to Obj_attr_list,sorted*/
									 /*by attribute*/


struct attr_pnt_list * alloc_attr_point_recs();
struct obj_attr_list * alloc_obj_attr_recs();
struct ff_attr_list * alloc_ff_attr_recs();
struct ff_elem_list * alloc_ff_elem_recs();
struct area_pnt_list * alloc_area_pnt_recs();

static  char cur_obj_code[] = "XXXX_XXXXXXXX";
/*static int cur_fid = 0;*/
static int obj_attr_serial = 0;
static int ff_attr_serial = 0;
static int ff_elem_serial = 0;

/* two attr_pnt counters:
  1. Attr_pnt_list_cnt = number of attr_pnt records, including those
	 with composite links AND those with their own attribute links
  2. Attr_pnt_with_attrs_list_cnt = number of attr_pnt records that have real
	 attribute links (with or w/o composite links)
*/
/*

long area_pnt_list_cnt = 0;
long obj_attr_list_cnt = 0;
*/
long ff_attr_list_cnt = 0;
long ff_elem_list_cnt = 0;

/*to hold temporary list of atids for a single object rec*/
char **tmp_atid_list; 

add_atid_to_tmp_list (atid, count)
   char *atid;
   int count;
{
    static int alloced = 0;
	char *p;

	if (! (p = 
		 dig_alloc_space (count, &alloced, CHUNKSIZE, 
			(char *) tmp_atid_list, sizeof (char *))))
    {
		 G_fatal_error ("Can't allocate memory for tmp_atid_list.");
	}
	tmp_atid_list = (char **) p;

    tmp_atid_list[count -1] = G_store (atid);	
	/*DEBUG*//* fprintf (stderr, "count = %d code = '%s'\n", count, tmp_atid_list[count -1]);*/

	return;
}

build_attr_structs_from_tmp_list (obj_code, obj_type, atid_count, att_x, att_y) 
    char *obj_code; 
	int obj_type, atid_count;
	double att_x, att_y;
{
    int i;

	for (i = 0; i < atid_count; i++)
		 build__attr_structs (obj_code, tmp_atid_list[i], obj_type, att_x, att_y);
	return;
}

/*returns cur_fid*/
build__attr_structs (obj_code, attr_code, obj_type, att_x, att_y)
   char *obj_code, *attr_code;
   int obj_type;
   double att_x, att_y;
{

	G_strip (obj_code);
    if (strcmp (cur_obj_code, obj_code))
	{
	  strcpy (cur_obj_code, obj_code);
	  Cur_fid++;
	  /*cur_fid++;*/
	  add_att_point_to_list (Cur_fid, obj_code, obj_type, att_x, att_y);
	  if (attr_code)
	     Attr_pnt_with_attrs_list_cnt++;
	     /*cur_attr_pnt_with_attrs_list_cnt++; */
	  else
		 return (Cur_fid);
    }

	add_obj_attr_to_list (Cur_fid, obj_code, attr_code);
	
	add_attr_modn_to_list (attr_code);
	 
    return (Cur_fid); 

}

add_att_point_to_list (cur_fid, obj_code, obj_type, att_x, att_y)
   int cur_fid;
   char *obj_code; 
   int obj_type;
   double att_x, att_y;
{
	struct attr_pnt_list *loc_ptr;

    loc_ptr = alloc_attr_point_recs (Attr_pnt_list); 
	Attr_pnt_list = loc_ptr;

	loc_ptr[Attr_pnt_list_cnt].fid = cur_fid;
	loc_ptr[Attr_pnt_list_cnt].manifold = Cur_manifold;
	loc_ptr[Attr_pnt_list_cnt].obj_type = obj_type;
	strcpy (loc_ptr[Attr_pnt_list_cnt].obj_code, obj_code);
	loc_ptr[Attr_pnt_list_cnt].att_x = att_x;
	loc_ptr[Attr_pnt_list_cnt].att_y = att_y;

	Attr_pnt_list_cnt++;

}

add_obj_attr_to_list (cur_fid, obj_code, attr_code)
   int cur_fid;
   char *obj_code, *attr_code;
{
	struct obj_attr_list *loc_ptr;

	obj_attr_serial++;

    loc_ptr = alloc_obj_attr_recs (Obj_attr_list); 
	Obj_attr_list = loc_ptr;

	loc_ptr[Obj_attr_list_cnt].serial = obj_attr_serial;
	loc_ptr[Obj_attr_list_cnt].fid = cur_fid;
	loc_ptr[Obj_attr_list_cnt].manifold = Cur_manifold;
	strcpy (loc_ptr[Obj_attr_list_cnt].obj_code, obj_code);
	strcpy (loc_ptr[Obj_attr_list_cnt].attr_code, attr_code);

	Obj_attr_list_cnt++;

}


struct attr_pnt_list *
alloc_attr_point_recs (attr_pnt_list_ptr )
   struct attr_pnt_list *attr_pnt_list_ptr;
{
     static int alloced = 0;
	 char *p;
	 int num;

	 num = Attr_pnt_list_cnt + 1;
	 if (! (p = 
		 dig_alloc_space (num, &alloced, CHUNKSIZE, 
			(char *) attr_pnt_list_ptr,
			sizeof (struct attr_pnt_list))))
     {
		 G_fatal_error ("Can't allocate memory for attr_pnt_list.\n");
	 }
	 attr_pnt_list_ptr = (struct attr_pnt_list *) p;
	 return (attr_pnt_list_ptr);
}


struct obj_attr_list *
alloc_obj_attr_recs (obj_attr_list_ptr )
   struct obj_attr_list *obj_attr_list_ptr;
{
     static int alloced = 0;
	 char *p;
	 int num;

	 num = Obj_attr_list_cnt + 1;

	 if (! (p = 
		 dig_alloc_space (num, &alloced, CHUNKSIZE, 
			(char *) obj_attr_list_ptr,
			sizeof (struct obj_attr_list))))
     {
		 G_fatal_error ("Can't allocate memory for obj_attr_list.\n");
	 }
	 obj_attr_list_ptr = (struct obj_attr_list *) p;
	 return (obj_attr_list_ptr);
}


add_attr_modn_to_list (attr_code)
    char *attr_code;
{
    char modn [5];
	int i;

    static int first = 1;

	/*
    modn_list_ptr = Obj_attr_modn.modn_list;
	*/

	strncpy (modn, attr_code, 4);
	modn[4] = '\0';

	/*
	fprintf (stderr, "adding modn '%s' '%s', cnt = %d\n", attr_code, modn, 
		Obj_attr_modn.cnt);
		*/

	if (first)
	{
		Obj_attr_modn.cnt = 1;

		Obj_attr_modn.modn_list[0] = (char *) G_malloc (5);
		/*
		modn_list_ptr[0] = (char *) G_malloc (5);
		*/
		strcpy  (Obj_attr_modn.modn_list[0], modn);
		first = 0;
    }	    
	else
	{
	    for (i = 0; i < Obj_attr_modn.cnt; i++) 
		   if (strcmp (Obj_attr_modn.modn_list[i], modn) == 0)
		   {
			  /*
			  fprintf (stderr, "have modn, cnt = %d\n", Obj_attr_modn.cnt);
			  */
			  return;
           }

		/*if not in list*/
		if (Obj_attr_modn.cnt == MAX_ATTR_MODS)
        {
		   sprintf (Error_msg, "Too many attribute modules (%d)\n", MAX_ATTR_MODS);
		   G_fatal_error (Error_msg);
		}
		else
		{
		   /*
		   fprintf (stderr, "adding an attribute module: '%s'\n", modn);
		   */
		   Obj_attr_modn.modn_list[Obj_attr_modn.cnt] =  (char *) G_malloc (5);
		   strcpy (Obj_attr_modn.modn_list[Obj_attr_modn.cnt], modn);
		   Obj_attr_modn.cnt++;
		}
	}
}



do_dig_atts (fp_att)
   FILE *fp_att;
{
    struct attr_pnt_list *a_p_list;
 
    int i;


	for (i = 0; i < Attr_pnt_list_cnt ; i++)
	{
	   a_p_list = &Attr_pnt_list[i];
	   /*DEBUG*//* fprintf (stderr, " '%d' fid = %d\n", i, a_p_list->fid);*/
	   if (a_p_list->manifold != Cur_manifold)
		   continue;
	   switch (a_p_list->obj_type)
	   {
		  case LINE:
	        write_att (fp_att, FILE_LINE, a_p_list->att_x, a_p_list->att_y,
						a_p_list->fid);
			break;
		  case AREA:
	        write_att (fp_att, FILE_AREA, a_p_list->att_x, a_p_list->att_y,
						a_p_list->fid);
			break;
		  case DOT:
	        write_att (fp_att, FILE_DOT, a_p_list->att_x, a_p_list->att_y,
						a_p_list->fid);
			break;
		  default:
			break;
	   }
    }

	return (1);

}

build_ff_attr_structs (ff_code, attr_code, obj_type)
   char *ff_code, *attr_code;
   int obj_type;
{

	G_strip (ff_code);

	add_ff_attr_to_list (ff_code, attr_code);
	
	/*this looks like a bug--shouldn't add this to obj_attr modn list!*/
	/*
	add_attr_modn_to_list (attr_code);
	*/
	 
    return ; /* 1 if new obj_code, 0 if not*/

}

add_ff_attr_to_list (ff_code, attr_code)
   char *ff_code, *attr_code;
{
	struct ff_attr_list *loc_ptr;

	ff_attr_serial++;

    loc_ptr = alloc_ff_attr_recs (FF_attr_list); 
	FF_attr_list = loc_ptr;

	loc_ptr[ff_attr_list_cnt].serial = ff_attr_serial;
	strcpy (loc_ptr[ff_attr_list_cnt].ff_code, ff_code);
	strcpy (loc_ptr[ff_attr_list_cnt].attr_code, attr_code);

	ff_attr_list_cnt++;

}

struct ff_attr_list *
alloc_ff_attr_recs (ff_attr_list_ptr )
   struct ff_attr_list *ff_attr_list_ptr;
{
     static int alloced = 0;
	 char *p;
	 int num;

	 num = ff_attr_list_cnt + 1;

	 if (! (p = 
		 dig_alloc_space (num, &alloced, CHUNKSIZE, 
			(char *) ff_attr_list_ptr,
			sizeof (struct ff_attr_list))))
     {
		 G_fatal_error ("Can't allocate memory for ff_attr_list.\n");
	 }
	 ff_attr_list_ptr = (struct ff_attr_list *) p;
	 return (ff_attr_list_ptr);
}


add_area_pnt_to_list (NA_code, PC_code, att_x, att_y)
   char *NA_code, *PC_code; 
   double att_x, att_y;
{
	struct area_pnt_list *loc_ptr;

	G_strip (NA_code);
	G_strip (PC_code);

    loc_ptr = alloc_area_pnt_recs (Area_pnt_list); 

	strcpy (loc_ptr[Cur_area_pnt_list_cnt].NA_code, NA_code);
	strcpy (loc_ptr[Cur_area_pnt_list_cnt].PC_code, PC_code);
	loc_ptr[Cur_area_pnt_list_cnt].att_x = att_x;
	loc_ptr[Cur_area_pnt_list_cnt].att_y = att_y;
	
	Area_pnt_list = loc_ptr;

	Cur_area_pnt_list_cnt++;
	 
    return;
}

struct area_pnt_list *
alloc_area_pnt_recs (area_pnt_list_ptr )
   struct area_pnt_list *area_pnt_list_ptr;
{
     static int alloced = 0;
	 char *p;
	 int num;

     num = Cur_area_pnt_list_cnt + 1;

	 if (! (p = 
		 dig_alloc_space (num, &alloced, CHUNKSIZE, 
			(char *) area_pnt_list_ptr,
			sizeof (struct area_pnt_list))))
     {
		 G_fatal_error ("Can't allocate memory for area_pnt_list.\n");
	 }
	 area_pnt_list_ptr = (struct area_pnt_list *) p;
	 return (area_pnt_list_ptr);
}

/* return 1 if matching pc_code found; 0 if not found*/
get_xy_from_area_pnt_list (pc_code, x, y)
   char *pc_code;
   double *x, *y;
{
	 int i;

     for (i= 0; i < Cur_area_pnt_list_cnt; i++)
		 if (strcmp (Area_pnt_list[i].PC_code, pc_code) == 0)
		 {
			 *x = Area_pnt_list[i].att_x;
			 *y = Area_pnt_list[i].att_y;
			 return (1);
		 }
	 return (0);
}

add_ff_elem_to_list (ff_code, elem_code)
   char *ff_code, *elem_code;
{
	struct ff_elem_list *loc_ptr;

	ff_elem_serial++;

	G_strip (ff_code);
	G_strip (elem_code);

    loc_ptr = alloc_ff_elem_recs (FF_elem_list); 

	loc_ptr[ff_elem_list_cnt].serial = ff_elem_serial;
	strcpy (loc_ptr[ff_elem_list_cnt].ff_code, ff_code);
	strcpy (loc_ptr[ff_elem_list_cnt].elem_code, elem_code);
	
	FF_elem_list = loc_ptr;

	ff_elem_list_cnt++;

}


struct ff_elem_list *
alloc_ff_elem_recs (ff_elem_list_ptr )
   struct ff_elem_list *ff_elem_list_ptr;
{
     static int alloced = 0;
	 char *p;
	 int num;

	 num = ff_elem_list_cnt + 1;

	 if (! (p = 
		 dig_alloc_space (num, &alloced, CHUNKSIZE, 
			(char *) ff_elem_list_ptr,
			sizeof (struct ff_elem_list))))
     {
		 G_fatal_error ("Can't allocate memory for ff_elem_list.\n");
	 }
	 ff_elem_list_ptr = (struct ff_elem_list *) p;
	 return (ff_elem_list_ptr);
}

display_attr_pnt_list ()
{
   int i;

	     fprintf (stderr, "Attr_pnt_with_attrs_list_cnt = %ld\n", Attr_pnt_with_attrs_list_cnt);
	     fprintf (stderr, "Attr_pnt_list_cnt = %ld\n", Attr_pnt_list_cnt);
   for (i = 0;  i < Attr_pnt_list_cnt; i++)
	   if (i == 20)
		  return;
       else
	   fprintf (stderr, "cur_attr_pnt_list fid '%d' x = '%f\n", 
	   Attr_pnt_list[i].fid,
	   Attr_pnt_list[i].att_x);
}

display_area_pnt_list ()
{
   int i;

   for (i = 0;  i < Cur_area_pnt_list_cnt; i++)
	   if (i == 10)
		  return;
       else
	   fprintf (stderr, "area_pnt_list na code '%s' '%s' '%f' '%f' \n", 
	   Area_pnt_list[i].NA_code,
	   Area_pnt_list[i].PC_code,
	   Area_pnt_list[i].att_x,
	   Area_pnt_list[i].att_y);
}

display_obj_attr_list ()
{
   int i;

   for (i = 0;  i < Obj_attr_list_cnt; i++)
	   if (i == 20)
		  return;
       else
	   fprintf (stderr, "obj_attr_list serial '%d' fid '%d' '%s' '%s'\n", 
		  Obj_attr_list[i].serial,
		  Obj_attr_list[i].fid,
		  Obj_attr_list[i].obj_code,
		  Obj_attr_list[i].attr_code);
}

display_ff_attr_list ()
{
   int i;

   for (i = 0;  i < ff_attr_list_cnt; i++)
	   if (i == 10)
		  return;
       else
	   fprintf (stderr, "ff_attr_list serial '%d' '%s' '%s'\n", 
		  FF_attr_list[i].serial,
		  FF_attr_list[i].ff_code,
		  FF_attr_list[i].attr_code);
}

display_ff_elem_list ()
{
   int i;

   for (i = 0;  i < ff_elem_list_cnt; i++)
	   if (i == 10)
		  return;
       else
	   fprintf (stderr, "ff_elem serial '%d' '%s' '%s'\n", 
		  FF_elem_list[i].serial,
		  FF_elem_list[i].ff_code,
		  FF_elem_list[i].elem_code);
}

display_obj_attr_modn_list ()
{
   int i;


   for (i = 0; i < Obj_attr_modn.cnt; i++)
   {
	  fprintf (stderr, "attr modn: '%s'\n", Obj_attr_modn.modn_list[i]);
   }
}


static
a_o_cmp (a, b)
  struct obj_attr_list ** a, **b;
{
	char a_modn[5], b_modn[5];
	int a_code, b_code;
	int ret;

    strncpy (a_modn, (*a)->attr_code, 4);
	a_modn[4] = '\0';
    strncpy (b_modn, (*b)->attr_code, 4);
	b_modn[4] = '\0';

	a_code = atoi ((*a)->attr_code + 5);
	b_code = atoi ((*b)->attr_code + 5);

/*if module names are different, return diference*/
/*
    (ret = strcmp ((*a)->attr_code, (*b)->attr_code));
*/
    ret = strcmp (a_modn, b_modn);
	if (ret != 0)
	   return (ret);
/*else, return difference between module codes*/
    else
	  return (a_code - b_code);
}

sort_obj_attr_list_by_attr()
{
   int i;

   Attr_obj_list = (struct obj_attr_list **) G_malloc 
					( Obj_attr_list_cnt * sizeof ( struct obj_attr_list *));

   for (i = 0; i < Obj_attr_list_cnt; i++)
	   Attr_obj_list[i] = &Obj_attr_list[i];

   qsort (Attr_obj_list, Obj_attr_list_cnt, sizeof (struct obj_attr_list *), a_o_cmp);

}


/*DEBUG*/
display_attr_obj_list ()
{
   int i;

   fprintf (stderr, "printing attr_objs, cnt = %d\n", Obj_attr_list_cnt);
   for (i = 0;  i < Obj_attr_list_cnt; i++)
	  if (i % 10 == 0)
	   fprintf (stderr, "attr_obj_list serial '%d' fid '%d' '%s' '%s'\n", 
		  Attr_obj_list[i]->serial,
		  Attr_obj_list[i]->fid,
		  Attr_obj_list[i]->obj_code,
		  Attr_obj_list[i]->attr_code);
}

/*find duplicate attributes in attr_obj list, to determine N:N relations:
*     returns 1 = Many to N obj-attr relations; 
*	          0 = 1 to N obj-attr relations
*/
find_dup_attrs_in_attr_obj_list()
{
	int i;
	char cur_attr_code [20];

	strcpy (cur_attr_code, "xxxxx");
    for (i = 0;  i < Obj_attr_list_cnt; i++)
	{
		if (strcmp (cur_attr_code, Attr_obj_list[i]->attr_code) == 0)
			return (1); /*at least one duplicate attr_code = M:1 or M:M*/
		else
		  strcpy (cur_attr_code, Attr_obj_list[i]->attr_code);
	}
	return (0);  /*no duplicate attr_codes in sorted list:  1:1 or 1:M*/
}

/*since objects may only be attributed through composites, as in DLG-E, 
* ff_elem_list be checked before assuming an object has no attribute
*/
find_objs_in_ff_elem_list(obj_code)
    char *obj_code;
{
   int i;

   for (i = 0; i < ff_elem_list_cnt; i++)
	  if (strcmp (FF_elem_list[i].elem_code, obj_code) == 0)
		 return (1);

   return (0);
}

get_obj_info_from_attr_obj_list (attr_code, fid, obj_code)
    char *attr_code;
	long *fid;
	char *obj_code;
{
    int i;

	for (i = 0; i < Obj_attr_list_cnt; i++)
	{
	    if (strcmp (attr_code, Attr_obj_list[i]->attr_code) == 0)
	    {
			 *fid = Attr_obj_list[i]->fid;
			 strcpy (obj_code,  Attr_obj_list[i]->obj_code);
			 return (1);
	    }
	}
	return (-1); /*no match found*/
}
