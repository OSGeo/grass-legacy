#define MAX_MODULE 30
#define MAX_MFOLDS 200
#include <stdio.h>
#include "gis.h"
#include "sdts_globals.h"
#include "sdts_in.h"

char *Indomain_name, *Inmap_name, *Intheme_name, *Inagob_name;
extern short info_flag;
int Multilayer_flag;

int
mfold_match ( s1, s2)
     char * s1;
     char * s2;
{
  while (*s1 && *s2  && *s1 == *s2)
    s1++, s2++;

  if (*s1 == *s2 || *s1 == '*' || *s2 == '*')
    return 0;
  else
    return *s1-*s2;
}

struct Sdts_manifold *
get_manifold_entry (mfold_name, domain_name, map_name, theme_name, info)
     char * mfold_name, *domain_name, *map_name, *theme_name;
     struct Sdts_info *info;
{
    static alloced_mfold = 0;
    struct Sdts_manifold * mp;

    if (domain_name == NULL && map_name == NULL) 
    {
        fprintf (stderr, "Warning: neither the Domain (DOMN) nor Map (MAP) is specified\n");
        return NULL;
    }
  
    if ((mp = info->S_mfold) == 0) 
    {
        /*  The first time this routine is called, alloc one manifold
	    structs. */
        if ((mp=(struct Sdts_manifold *)calloc(1,sizeof(struct Sdts_manifold)))==NULL)
	    {
	      sprintf (Error_msg,"Out of memory when allocating space for Manifold.");
	      G_fatal_error (Error_msg);
	    }
        else
	    {
	      alloced_mfold ++;
	      info->n_manifolds ++;
	      mp->index = 1;
	      mp->mfold_name = mfold_name;
	      mp->domain_name = domain_name;
	      mp->map_name = map_name;
	      mp->theme_name = theme_name;
	      info->S_mfold = mp;
	      return mp;
	    }
    }
    else
    {
      int i;
      
      /* search for the manifold in the Sdts_manifold array */
      for (i = 0 ; i<info->n_manifolds ; i++, mp++)
	    if (strcmp (mp->mfold_name, mfold_name) == 0)
	       break;

      if (i < info->n_manifolds)
	  {
	  /* found that manifold */
	     return mp;
	  }
      else
	  {
	    struct Sdts_manifold *new_mp;
	  
	   /* New  manifold, realloc space for manifold struct */
	    if ((new_mp = (struct Sdts_manifold *)calloc (alloced_mfold+1,
			   sizeof (struct Sdts_manifold)))==NULL)
	    {
	      sprintf (Error_msg,"Out of memory when allocating space for Manifold.");
	      G_fatal_error (Error_msg);
	    }
	    else
	    {
	      mp = info->S_mfold;
	      memcpy (new_mp, mp, alloced_mfold * sizeof(struct Sdts_manifold));
	      free (mp);
	      info->S_mfold = mp = new_mp;
	      mp = mp + info->n_manifolds;
	      alloced_mfold ++;
	      info->n_manifolds ++;
	      mp->mfold_name = mfold_name;
	      mp->domain_name = domain_name;
	      mp->map_name = map_name;
	      mp->theme_name = theme_name;
	      mp->index = alloced_mfold;
	      return mp;
	    }
	  }
    }
}

struct Sdts_catd *
find_module (mod_name, info, compared_mod_name)
     char * mod_name;
     struct Sdts_info *info;
     char * compared_mod_name;
{
  struct Sdts_catd *cptr;
  int i;

  cptr = info->S_catd;
  
  for (i = 0; i < info->n_files ; i++)
    if (strcmp (cptr[i].mod_name, mod_name) == 0)	/* found it! */
      break;

  if (i < info->n_files)
    return &cptr[i];
  else
    {
	  if (compared_mod_name)
		  fprintf (stderr, "Warning: Module %s is specified in %s file but not in CATD file.\n", mod_name, compared_mod_name);
	  else
		  fprintf (stderr, "Warning: Module %s is not specified in CATD file.\n", mod_name);
      return 0;
    }
}
    
  
int
get_manifolds ( cats_fname, info )
     char *cats_fname;
     struct Sdts_info *info;
{
  FILE *fp_cats;
  char ice[2], ccs[4];
  char tag[10];
  char string[5000];
  char frmts[500];
  char leadid;
  char stag[5000];
  long int_level, bytlen;
  int  status;
  struct Sdts_manifold *mp;
  int cur_mod_index;
  char err_mess[200];
  char *cur_mod_name=0, *cur_domain=0, *cur_map=0, *cur_theme=0, *cur_agob=0;

  
   if (!open_sdts_rfile (cats_fname, &int_level, ice, ccs, &fp_cats))
	    return (0);


   if (!read_dd_rec (cats_fname, fp_cats, string, &status))
   {
	   end123file (&fp_cats);
	   return (0);
   }

   while (status != 4)
   {
      if (!read_data_sfld (cats_fname, fp_cats, tag, &leadid, string, &bytlen, &status))
	  {
	    end123file (&fp_cats);
	    return (0);
	  }
	  
	  G_strip (string);

      if (!check_data_sfld (cats_fname, fp_cats, tag, stag, frmts))
	  {
	     end123file (&fp_cats);
	     return (0);
	  }

      if (strcmp (stag, "NAME") == 0)
	  {
	    cur_mod_index = get_mod_index (string);
	    cur_mod_name = G_malloc (MODN_SIZE);
	    strcpy (cur_mod_name, string);
	  }
      else if (strcmp (stag, "TYPE") == 0)
	  {
	  /* the current_mod module's primary field name */
	  /* ??? what's it for? ??? */
	  }
      else if (strcmp (stag, "DOMN") == 0) 
	  { 
	      cur_domain = (char *) calloc (1, strlen (string)+1); 
          strcpy (cur_domain, string); 
	  }
      else if (strcmp (stag, "MAP") == 0)
	  {
	      cur_map = (char *) calloc (1, strlen (string)+1);
	      strcpy (cur_map,  string);
	  }
      else if (strcmp (stag, "THEM") == 0)
	  {
	     cur_theme = (char *) calloc (1, strlen (string)+1);
	     strcpy (cur_theme, string);
	  }
      else if (strcmp (stag, "AGOB") == 0)
	  {
	     static char *last_agob = 0;

	     cur_agob = (char *) calloc (1, strlen (string)+1);
	     strcpy (cur_agob, string);
	     if (last_agob && mfold_match (cur_agob, last_agob))
	        Multilayer_flag = 1;
          else
	        last_agob = cur_agob;
	  }
      else if (strcmp (stag, "AGTF") == 0)
	  {
	  /* TVP 5.11 c: The Aggregate Object Type subfield shall contain the
	     object representation code "GT" indicating that the module
	     references a 2-D manifolds. */

	     if (strcmp (string, "GT"))
	        fprintf(stderr, "The AGTF subfield should contain GT, illegal %s\n", string);
	  }
      else if (strcmp (stag, "COMT") == 0)
	  {

	  }
      if (status == 3 || status == 4)	/* the end of record */
	  {
	      if (Indomain_name && (!cur_domain || mfold_match (Indomain_name, cur_domain)))
	          continue;
          if (Inmap_name && (!cur_map || mfold_match (Inmap_name, cur_map)))
	          continue;
          if (Intheme_name && (!cur_theme || mfold_match (Intheme_name, cur_theme)))
	          continue;
          if (Inagob_name && (!cur_agob || mfold_match (Inagob_name, cur_agob)))
	          continue;
	      
	      if (cur_mod_index >= FIRST_OBJ_MODULE)
	      {
	         if (!cur_agob)
		     {
		        fprintf (stderr, "Warning: No AGOB is given for %s\n", cur_mod_name);
		        cur_agob = "";
		     }
	      
	         mp = get_manifold_entry (cur_agob, cur_domain, cur_map, cur_theme,
				        info);
	         cur_domain = cur_map = cur_theme = cur_agob = NULL;
	      
	         if (!mp->mod_name[cur_mod_index])
		     {
		        struct Sdts_catd *cptr;
	      
		        mp->mod_name [cur_mod_index] = cur_mod_name;
		        cptr = find_module (cur_mod_name, info, "CATS");
 		        if (cptr)
		         {
		              mp->mod_nrec[cur_mod_index] = cptr->mod_nrec;
		              mp->file_name[cur_mod_index] = cptr->file_name;
		         }
		     }
	         else
		     {
		         fprintf (stderr, "Warning: In manifold %s there are two %s modules\n",
			     string, Mod_in[cur_mod_index]);
		     }
	      }
	      else /*add record to "global" manifold struct*/
	      {
	           struct Sdts_catd *cptr;
			  
	           mp = &global_mfold;
	           mp->mod_name[cur_mod_index] = cur_mod_name;
	           cptr = find_module (cur_mod_name, info, "CATS");
	           if (cptr)
		       {
		         mp->mod_nrec[cur_mod_index] = cptr->mod_nrec;
		         mp->file_name[cur_mod_index] = cptr->file_name;
		       }
	      }
	   }
    }
  
    if (!end_sdts_rfile (cats_fname, &fp_cats))
	    return (0);

    if (info->n_manifolds == 0)
    {
       if (Indomain_name || Inmap_name || Intheme_name || Inagob_name)
	   {
          if (Indomain_name)
	        sprintf (err_mess, "Domain name <%s> ", Indomain_name);
          if (Inmap_name)
	        sprintf (err_mess, "Map name <%s> ", Inmap_name);
          if (Intheme_name)
	        sprintf (err_mess, "Theme name <%s> ", Intheme_name);
          if (Inagob_name)
	        sprintf (err_mess, "Aggregate object (AGOB) name <%s> ", Inagob_name);

          strcat (err_mess, " is not found in the data set.");
        }
        else
	      sprintf (err_mess, "No manifold can be found in CATS file\n");

		put_err_mess (err_mess, 0);
        return  (0);
    }
  
/*  if (!Indomain_name && !Inmap_name && !Intheme_name && !Inagob_name) */
    check_partition (info);

	/*add any global modules to global manifold that weren't represented in
	  CATS module--e.g., noaa datasets*/

    complete_global_manifold ( info);

    return (1);
}

static   struct Sdts_manifold *mfold_list [MAX_MFOLDS];

int
check_partition (info)
     struct Sdts_info *info;
{
  short multi_partitions = 0;
  struct Sdts_manifold *tmp;
  int i, j;

  for (i = 0 ; i < info->n_manifolds; i++)
    mfold_list[i] = info->S_mfold + i;

/***omit check on themes*/
/*
  if (mfold_list[0]->theme_name != NULL)
    for (i = 0 ; i < info->n_manifolds; i++)
    {
      
	   for (j=1 ; j < info->n_manifolds - i; j++)
	   {
	      register char * p, *q;
	      int res = 0;

	      p = mfold_list[j-1]->theme_name;
	      q = mfold_list[j]->theme_name;

	      if ((res = strcmp (p, q))>0)
	      {
		    tmp = mfold_list[j-1];
		    mfold_list[j-1] = mfold_list[j];
		    mfold_list[j] = tmp;
	      }

	      if (res != 0)
	        multi_partitions ++;
	    }
     }
*/

  if (mfold_list[0]->map_name != NULL)
    for (i = 0 ; i < info->n_manifolds; i++)
    {
	  for (j=1 ; j < info->n_manifolds - i; j++)
	  {
	    register char * p, *q;
	    int res = 0;;

	    p = mfold_list[j-1]->map_name;
	    q = mfold_list[j]->map_name;
	  
	    if ((res = strcmp (p, q))>0)
	    {
		  tmp = mfold_list[j-1];
		  mfold_list[j-1] = mfold_list[j];
		  mfold_list[j] = tmp;
	    }
	    if (res != 0)
	      multi_partitions ++;
	  }
    }

  if (mfold_list[0]->domain_name != NULL)
    for (i = 0 ; i < info->n_manifolds; i++)
    {
	  for (j=1 ; j < info->n_manifolds - i; j++)
	  {
	    register char * p, *q;
	    int res = 0;

	    p = mfold_list[j-1]->domain_name;
	    q = mfold_list[j]->domain_name;
	  
	    if ((res = strcmp (p, q))>0)
	    {
		  tmp = mfold_list[j-1];
		  mfold_list[j-1] = mfold_list[j];
		  mfold_list[j] = tmp;
	    }

	    if (res != 0)
	      multi_partitions ++;
	  }
    }

  
  if (multi_partitions)
    {
      fprintf  (stderr, "The SDTS data set contains multiple partitions.");
      fprintf  (stderr, "Use the 'map' or 'domain' parameter to select\n");
      fprintf  (stderr, "a single partition.\n\n");
      fprintf  (stderr, "Entering infomation only mode.\n");
      info_flag ++;
    }

    return (1);
}

void 
display_manifold (info, globals)
     struct Sdts_info *info;
     struct Sdts_globals *globals;
{

  int indent = 0;
  char *cur_domain, *cur_map, *cur_theme;
  struct Bounds bounds;
  int i;
  
  
  if (info->n_manifolds > 1)
    {
      fprintf (stderr, "Information about individual manifolds:\n");
      printf ("\t");
      if (mfold_list[0]->domain_name != NULL)
        printf ("DOMAIN\t");
      if (mfold_list[0]->map_name != NULL)
        printf ("MAP\t");
      if (mfold_list[0]->theme_name != NULL)
        printf ("THEME\t");
      if (mfold_list[0]->mfold_name != NULL)
        {
          printf ("MANIFOLD\t");
          printf ("MIN_X\tMIN_Y\tMAX_X\tMAX_Y\n");
        }
    
      for (i = 0 ; i < info->n_manifolds; i++)
        {
          if (mfold_list[i]->domain_name != NULL) 
            printf ("%-16.16s ", mfold_list[i]->domain_name);
          if (mfold_list[i]->map_name != NULL)
    	    printf ("%-16.16s ", mfold_list[i]->map_name);
          if (mfold_list[i]->theme_name != NULL)
    	    printf ("%-10.10s ", mfold_list[i]->theme_name);
          if (mfold_list[i]->mfold_name != NULL)
    	    {
    	      printf ("%-10s ", mfold_list[i]->mfold_name);
              get_boundary (mfold_list[i], globals, &bounds);
    	      printf ("%f  %f  %f  %f\n", globals->Iref_sfax * bounds.min_x, 
		globals->Iref_sfay * bounds.min_y, globals->Iref_sfax * 
		   bounds.max_x, globals->Iref_sfay * bounds.max_y);
            }
         }
    } 
  else
    {
      if (mfold_list[0]->domain_name != NULL)
	{
          printf ("DOMAIN:\t");
          printf ("%s\n", mfold_list[0]->domain_name);
        }
      if (mfold_list[0]->map_name != NULL)
	{
          printf ("MAP:\t");
	  printf ("%s\n", mfold_list[0]->map_name);
        }
      if (mfold_list[0]->theme_name != NULL)
	{
          printf ("THEME:\t");
          printf ("%s\n", mfold_list[0]->theme_name);
        }
      if (mfold_list[0]->mfold_name != NULL)
        {
	  printf ("MANIFOLD:\t");
	  printf ("%s\n", mfold_list[0]->mfold_name);
          get_boundary (mfold_list[0], globals, &bounds);
	  printf ("BOUNDARY:\n");
          printf ("\tMIN_X\tMIN_Y\tMAX_X\tMAX_Y\n");
    	  printf ("\t%f  %f  %f  %f\n", globals->Iref_sfax * bounds.min_x, 
		globals->Iref_sfay * bounds.min_y, globals->Iref_sfax * 
		   bounds.max_x, globals->Iref_sfay * bounds.max_y);

        }
    }
#if 0
  if ((cur_domain = mfold_list[0]->domain_name) != NULL)
    {
      printf ("%s\n", cur_domain);
      indent ++;
    }
      
  if ((cur_map = mfold_list[0]->map_name) != NULL)
    {
      int i = indent;
      
      while (i--) putc ('\t', stderr);
      printf ("%s\n", cur_map);
      indent ++;
    }
      
  if ((cur_theme = mfold_list[0]->theme_name) != NULL)
    {
      int i = indent;
      
      while (i--) putc ('\t', stderr);
      printf ("%s\n\t", cur_theme);
      indent ++;
    }

  for (i = 0 ; i < info->n_manifolds; i++)
    {
      short change_flag = 0;
	  
      indent = 0;
	  
      if (cur_domain != NULL)
	{
	  indent ++;
	  if (strcmp (cur_domain, mfold_list[i]->domain_name))
	    {
	      printf ("%s\n", cur_domain = mfold_list[i]->domain_name);
	      change_flag ++;
	    }
	}

      if (cur_map != NULL)
	{
	  indent ++;
	  if (change_flag || strcmp (cur_map, mfold_list[i]->map_name))
	    {
	      int i = indent;
	      while (i--) putc ('\t', stderr);
	      printf ("%s\n", cur_map = mfold_list[i]->map_name);
	      change_flag ++;
	    }
	}

      if (cur_theme != NULL)
	{
	  indent ++;
	  if (change_flag || strcmp (cur_theme, mfold_list[i]->theme_name))
	    {
	      int i = indent;
	      while (i--) putc ('\t', stderr);
	      printf ("%s\n", cur_theme = mfold_list[i]->theme_name);
	    }
	}

      while (indent --) putc ('\t', stderr);
      printf ("%s\n", mfold_list[i]->mfold_name);
    }  
#endif

} 

display_manifold_obj_summary (info, globals)
     struct Sdts_info *info;
     struct Sdts_globals *globals;
{
  int i ;
  long cur_n_lines;
  long cur_n_nodes;

  fprintf (stderr, "\nFeatures imported:\n\n");
  if (info->n_manifolds > 1)
    {
      for (i = 0 ; i < info->n_manifolds; i++)
        {
          if (mfold_list[i]->mfold_name != NULL)
    	      printf ("%s:\n", mfold_list[i]->mfold_name);

		  cur_n_lines = mfold_list[i]->n_lines +
						mfold_list[i]->n_entity_pnts +
						mfold_list[i]->n_attr_nodes;

		  cur_n_nodes = mfold_list[i]->n_nodes + mfold_list[i]->n_entity_pnts;				  
	  printf ("  Lines:         %8ld\t(%ld LE + %ld NE + %ld attributed NO)\n", 
	      cur_n_lines,
	      mfold_list[i]->n_lines, 
	      mfold_list[i]->n_entity_pnts, 
	      mfold_list[i]->n_attr_nodes); 


	  printf ("  Nodes:         %8ld\t(%ld NO + %ld NE)\n", 
		  cur_n_nodes,
		  mfold_list[i]->n_nodes,
		  mfold_list[i]->n_entity_pnts);

	  printf ("  Areas:         %8ld\n", mfold_list[i]->n_polygons);
	  printf ("  Attrib. Objs.: %8ld\n", mfold_list[i]->n_attr_objs);
         }
    } 
  else
    {
      if (*(mfold_list[0]->mfold_name) != '\0')
	     printf ("%s\n", mfold_list[0]->mfold_name);
      
	  cur_n_lines = mfold_list[0]->n_lines +
	                mfold_list[0]->n_entity_pnts +
	                mfold_list[0]->n_attr_nodes;

      cur_n_nodes = mfold_list[0]->n_nodes + mfold_list[0]->n_entity_pnts;				  
	  printf ("  Lines:         %8ld\t(%ld LE + %ld NE + %ld attributed NO)\n", 
	      cur_n_lines,
	      mfold_list[0]->n_lines, 
	      mfold_list[0]->n_entity_pnts, 
	      mfold_list[0]->n_attr_nodes); 


	  printf ("  Nodes:         %8ld\t(%ld NO + %ld NE)\n", 
		  cur_n_nodes,
		  mfold_list[0]->n_nodes,
		  mfold_list[0]->n_entity_pnts);

	  printf ("  Areas:         %8ld\n", mfold_list[0]->n_polygons);
	  printf ("  Attrib. Objs.: %8ld\n", mfold_list[0]->n_attr_objs);

    }
}

complete_global_manifold (info)
  struct Sdts_info *info;
{
  _complete_global_manifold ("IDEN", info, 1);
  _complete_global_manifold ("XREF", info, 1);
  _complete_global_manifold ("IREF", info, 1);
  _complete_global_manifold ("DDSH", info, 1);
  _complete_global_manifold ("SPDM", info, 0);

}
_complete_global_manifold (cur_modstr, info, required)
   char *cur_modstr;
   struct Sdts_info *info;
   int required;
{
   struct Sdts_manifold *mp;
   struct Sdts_catd *cptr;
   char *cur_mod_name;
   int cur_mod_index;
   char err_mess [200];

   cur_mod_name = G_malloc (MODN_SIZE);
   strcpy (cur_mod_name, cur_modstr);

   mp =&global_mfold;

   cur_mod_index = get_mod_index (cur_mod_name);

   /*if global module name has not already been entered (from CATS record)*/
   if (!mp->mod_name [cur_mod_index])
   {
	  mp->mod_name [cur_mod_index] = cur_mod_name;
	  cptr = find_module (cur_mod_name, info, (char *) NULL);
	  if (cptr)
	  {
		 mp->mod_nrec [cur_mod_index] = cptr->mod_nrec;
		 mp->file_name [cur_mod_index] = cptr->file_name;
	  }
	  else
		if (required)
	    {
		   sprintf (err_mess, "Error completing global manifold.\nCannot find %s module record in CATD file.\n", cur_modstr);
		   G_fatal_error (err_mess);
		}

   }

   return (1);
}
