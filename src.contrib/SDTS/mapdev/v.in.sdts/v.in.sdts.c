/*
 * $Id$
 */

#include     <stdio.h>
#include     <signal.h>
#include     "gis.h"
#include     "Vect.h"
#include     <stc123.h>

#define MAIN
#include     "sdts_in.h"
#include  "sdts_globals.h"

/*
** v.in.sdts:
** 
** Usage:
**  v.in.sdts [-il] catd=name [output=name] [dbpath=name] [domain=name]
**    [map=name] [theme=name] [manifold=name]
** 
** Flags:
**   -i   Information only mode
**   -l   object link table only; no attribute tables
** 
** Parameters:
**       catd   SDTS CATD filename
**     output   vector output file
**     dbpath   output site for import SDTS attributes
**     domain   name of the domain that will be imported
**        map   name of the map that will be imported
**      theme   name of the theme that will be imported
**   manifold   name of the manifold that will be imported
**  
*/


#define         SDTS            "sdts"
#define         ATT             "dig_att"
#define         MAX_OUTNAME     11 

extern char *Indomain_name, *Inmap_name, *Intheme_name, *Inagob_name;

short info_flag;
short obj_link_only_flag;

static char  *current_mapset ;
static char  *gbase ;

char *get_err_mess();

main (argc, argv)
        int  argc ;
        char  *argv[] ;
{
    char *catd_fname;
    char sdts_prefix[5];
    char *dig_name;
    struct Option *old, *new, *dbpath;
    struct Option *domain_opt, *map_opt, *theme_opt, *agob_opt;
    struct Flag *i_flag, *l_flag;
    short set_flag = 0;

    G_gisinit (argv[0]);

    old = G_define_option();
    old->key            = "catd";
    old->type            = TYPE_STRING;
    old->required        = YES;
    old->multiple        = NO;
/*  It is not necessary that an SDTS transfer is under GRASS vector directory 
    old->gisprompt       = "old,sdts,catd";
*/
    old->description        = "SDTS CATD filename";
    

    new = G_define_option();
    new->key            = "output";
    new->type            = TYPE_STRING;
    new->required        = NO;
    new->multiple        = NO;
    new->gisprompt        = "new,dig,vector";
    new->description        = "vector output file";

    dbpath = G_define_option();
    dbpath->key            = "dbpath";
    dbpath->type            = TYPE_STRING;
    dbpath->required        = NO;
    dbpath->multiple        = NO;
    dbpath->description      = "output site for import SDTS attributes";
    
    domain_opt = G_define_option();
    domain_opt->key            = "domain";
    domain_opt->type            = TYPE_STRING;
    domain_opt->required        = NO;
    domain_opt->multiple        = NO;
    domain_opt->description      = "name of the domain that will be imported";

    map_opt = G_define_option();
    map_opt->key            = "map";
    map_opt->type            = TYPE_STRING;
    map_opt->required        = NO;
    map_opt->multiple        = NO;
    map_opt->description      = "name of the map that will be imported";

    theme_opt = G_define_option();
    theme_opt->key            = "theme";
    theme_opt->type            = TYPE_STRING;
    theme_opt->required        = NO;
    theme_opt->multiple        = NO;
    theme_opt->description      = "name of the theme that will be imported";

    agob_opt = G_define_option();
    agob_opt->key            = "manifold";
    agob_opt->type            = TYPE_STRING;
    agob_opt->required        = NO;
    agob_opt->multiple        = NO;
    agob_opt->description      = "name of the manifold that will be imported";

    i_flag = G_define_flag ();
    i_flag->key          = 'i';
    i_flag->description  = "Information only mode";

    l_flag = G_define_flag ();
    l_flag->key          = 'l';
    l_flag->description  = "object link table only; no attribute tables";

    gbase = G_gisbase();
    current_mapset = G_mapset();

    if (G_parser (argc, argv))
    exit (-1);

	info_flag = i_flag->answer;

	obj_link_only_flag = l_flag->answer;

    catd_fname  = old->answer;
    if ((dig_name = new->answer) == NULL)
      info_flag = 1;
    else if ((int)strlen (dig_name) > MAX_OUTNAME)
      {
     dig_name[MAX_OUTNAME-1] = 0;
     fprintf (stderr, "Warning: the length of the output file name exceeds %d characters.\n  It is truncated to be '%s'.\n", MAX_OUTNAME, dig_name);
      }

   if (Indomain_name = domain_opt->answer)
       set_flag ++;

    if (Inmap_name = map_opt->answer) {
       if (set_flag) {
          fprintf (stderr, "Option domain, map, theme and manifold cannot be specified at the same time\n");
          exit (-1);
       }
       set_flag ++;
    }

    if (Intheme_name = theme_opt->answer) {
        if (set_flag) {
           fprintf (stderr, "Option domain, map, theme and manifold cannot be specified at the same time\n");
           exit (-1);
        }
        set_flag ++;
     }

     if (Inagob_name = agob_opt->answer) {
         if (set_flag) {
            fprintf (stderr, "Option domain, map, theme and manifold cannot be specified at the same time\n");
            exit (-1);
         }
         set_flag ++;
     }


    /*verify sdts catd filename and file*/

    if (strncmp (catd_fname + 4, "CATD", 4))
       G_fatal_error ("Input filename must be of the form xxxxCATD.xxx");

    G_strncpy (sdts_prefix, catd_fname, 4);


   /*before importing a potentially object set, make sure dbpath is
   spelled right if it's specified*/
   if (dbpath->answer != NULL)
	 if (access (dbpath->answer, 2))
	 {
		sprintf ( Error_msg, "Cannot access dbpath <%s>\n", dbpath->answer);
		G_fatal_error (Error_msg);
     }
  
   /*this is a dummy value*/
   Level = 1;
   doit (dig_name, catd_fname, dbpath->answer, sdts_prefix);

	fprintf (stderr, "Done.\n");
    exit(0);
}


struct Sdts_manifold *alloc_Sdts_manifold_struct ();
struct Sdts_catd *alloc_Sdts_catd_rec();

doit(dig_name, catd_fname, dbpath, sdts_prefix)
    char *dig_name, *catd_fname, *dbpath, *sdts_prefix;
{
	char cur_vectname[30];
    FILE *fp_att, *fopen();
    struct Sdts_info S_info;
	struct Sdts_manifold *cur_mfold;
	struct Bounds bounds;
    struct Map_info Map;
	struct line_pnts *Points;
    struct dig_head head;
	int Obj_num;
	int i;
	int failed_modules = 0;


/*initialize Sdts_info structure*/
    init_Sdts_info (&S_info);
    init_Sdts_globals (&S_globals);
	init_Sdts_att_ff_struct (&Att_FF_info);



    /*get SDTS CAT/DICTIONARY info*/
    if (!do_CATD_module (catd_fname, &head, &S_info)) 
	{
	  fprintf (stderr, "Error: %s\n", get_err_mess());
      G_fatal_error ("Unable to process SDTS CATS file. Exiting");
    }

	/*
    display_att_ff (&Att_FF_info);
	*/


    fprintf (stderr, "Processing IDEN module\n");
    if (!do_IDEN_module (&global_mfold, &S_globals))
	{
	  fprintf (stderr, "Error: %s\n", get_err_mess());
	  clear_err_mess ();
      G_fatal_error ("Unable to process SDTS Identification Module. Exiting");
	}

    fprintf (stderr, "Processing XREF module\n");
    if (!do_XREF_module (&global_mfold, &S_globals))
	{
	  fprintf (stderr, "Error: %s\n", get_err_mess());
	  clear_err_mess ();
      G_fatal_error ("Unable to process SDTS XREF file. Exiting");
	}

/*there can be multiple IREF modules in a transfer, but only one IREF
module per partition. thus only a single IREF module will be processed
during the import procedure, which is restricted to a single partition.
*/

    fprintf (stderr, "Processing IREF module\n");
	if (!do_IREF_module (&global_mfold, &S_globals))
	{
	  fprintf (stderr, "Error: %s\n", get_err_mess());
	  clear_err_mess ();
	  G_fatal_error ("Unable to process SDTS IREF file. Exiting");
	}


    if (info_flag)
      display_info (&S_info, &S_globals, &global_mfold);
	else
	{

	    /*initialize global counters*/
	     Attr_pnt_list_cnt = 0;
	     Attr_pnt_with_attrs_list_cnt = 0;
		 Obj_attr_list_cnt = 0;
	/**composites must be processed BEFORE object modules, since we need
	*to know which objects are referenced by composites
	*And, each simple object routine must consult composite list
	*before deciding a simple object has no attribute
	**/

	if (!do_composites (&Att_FF_info))
	{
		fprintf (stderr, "Error: %s\n", get_err_mess());
		clear_err_mess ();
		fprintf (stderr, "Unable to process composite modules.  Continuing.\n");
	}

	/*DEBUG*/
	/* fprintf (stderr, "num manifolds = '%d', multilayer_flag = /%d/\n",
	S_info.n_manifolds, Multilayer_flag);
	*/
	
    /*MANIFOLD PROCESSING LOOP*/
    for (i = 0; i < S_info.n_manifolds; i++)
      {
	Obj_num = 0;
    cur_mfold = S_info.S_mfold + i;
    fprintf (stderr, "Processing manifold: %s\n", cur_mfold->mfold_name);


   /*create cur_vectname for this manifold*/
    if (S_info.n_manifolds > 1)
      {
        sprintf (cur_vectname, "%s_%d", dig_name, cur_mfold->index);
          }
        else
          strcpy (cur_vectname, dig_name);


	/*set global Cur_manifold number*/
	/*couldn't I use S_info.S_mfold[i].index for this?*/
	Cur_manifold = i + 1;

	/*intialize global FID counter for this manifold*/
	Cur_fid = 0;
	Cur_area_pnt_list_cnt = 0;
	cur_attr_pnt_list_cnt = 0;
	cur_attr_pnt_with_attrs_cnt = 0;
	cur_obj_attr_list_cnt = 0;

    /*setup GRASS output files for this manifold*/

	if (0 > Vect_open_new (&Map, cur_vectname))
	  G_fatal_error ("Not able to create vector file ");

    G__make_mapset_element (ATT);
	if (! (fp_att = G_fopen_new (ATT, cur_vectname)) )
	  G_fatal_error ("Not able to create att file ");

	Points = Vect_new_line_struct ();



	if (!do_NA_module (cur_mfold, &S_globals, &bounds, &Map, Points, &Obj_num))
	{
		fprintf (stderr, "Error: %s\n", get_err_mess());
		clear_err_mess ();
		fprintf (stderr, "Unable to process NA-planar node (area points) module.\n");
		fprintf (stderr, "Area points will be generated.\n");
    }

	if (!do_LE_module (cur_mfold, &S_globals, &bounds, &Map, Points ))
	{
		fprintf (stderr, "Error: %s\n", get_err_mess());
		clear_err_mess ();
	    G_fatal_error ("Unable to process LE-complete chain module. Exiting");
    }

	if (!do_PC_module (cur_mfold, &S_globals, &bounds, &Map, Points))
	{
		fprintf (stderr, "Error: %s\n", get_err_mess());
		clear_err_mess ();
	    G_fatal_error ("Unable to process polygon module. Exiting");
    }

	if (!do_NO_module (cur_mfold, &S_globals, &bounds, &Map, Points, &Obj_num))
	{
		fprintf (stderr, "Error: %s\n", get_err_mess());
		clear_err_mess ();
	    G_fatal_error ("Unable to process NO-planar node module. Exiting");
    }

	if (!do_NE_module (cur_mfold, &S_globals, &bounds, &Map, Points, &Obj_num))
	{
		fprintf (stderr, "Error: %s\n", get_err_mess());
		clear_err_mess ();
	    G_fatal_error ("Unable to process NE-planar node module. Exiting");
    }


	    fprintf (stderr, "Processing GRASS dig_atts\n");
		if (do_dig_atts (fp_att) < 0)
			G_fatal_error ("Unable to process attributes. Exiting");

        make_dig_head (&S_globals, sdts_prefix, &bounds, &head, cur_mfold);

		Vect_copy_head_data (&head, &Map.head);

		Vect_destroy_line_struct (Points);

		Vect_close (&Map);

		fclose (fp_att);
	}
	/*END MANIFOLD PROCRESSING LOOP*/

	/*write database attributes. use dig_name for filename root*/

	if ((Schema_type = determine_attr_schema (&Att_FF_info, Multilayer_flag)) == -1)
	   G_warning ("Cannot determine attribute schema. Not importing attributes.\n");
	else
	{
		fprintf (stderr, "Schema Type = '%d'\n", Schema_type);
		fprintf (stderr, "Processing attribute modules . . .\n");
		write_attributes (&S_info, &Att_FF_info, sdts_prefix, dig_name, dbpath, Schema_type, Multilayer_flag, obj_link_only_flag);
	}

	/*Only create db scripts if full attribute set is being imported*/
    if (dbpath != NULL && !obj_link_only_flag)
	{
         if (!do_DDSH_module (&global_mfold, &S_globals))
		 {
		   fprintf (stderr, "Error: %s\n", get_err_mess());
		   clear_err_mess ();
           fprintf (stderr, "Unable to process SDTS DDSH file. ");
           fprintf (stderr, "Therefore, table information for RDBMS scripts");
           fprintf (stderr, "is not accessible.  Scripts will not be created.");
         }
		 else
           make_db_scripts (dbpath);
	}

    display_summary_info (&S_info, &S_globals, &global_mfold);

	   

	/*DEBUG*/ 
	/*
		display_globals (S_globals, &head);
		display_attr_pnt_list ();
		display_obj_attr_list ();
		display_ff_attr_list ();
		display_ff_elem_list ();
		display_area_pnt_list ();
		display_obj_attr_modn_list ();
	*/

    }


}

do_CATD_module (catd_fname, head, S_info)
    char *catd_fname;
    struct dig_head *head;
    struct Sdts_info *S_info;
{
    FILE *fp_catd;
    struct Sdts_catd *catd_ptr, *cur_catd_ptr;
    char ice[2], ccs[4];
    long bytlen, int_level; 
	int status;
    char string[5000];
    char stag[5000];
    char frmts[5000];
    char tag[10];
    char leadid;
    int start_record;
    char stat_fname[30], cats_fname[30];


	fprintf (stderr, "Processing CATD module\n");

    if (!open_sdts_rfile (catd_fname, &int_level, ice, ccs, &fp_catd))
	    return (0);


    if (!read_dd_rec (catd_fname, fp_catd, string, &status))
    {
		end123file (&fp_catd);
		return (0);
    }

   /*while not EOF*/

   start_record = 1; 

   while (status !=4)
     {
        if (!read_data_sfld (catd_fname, fp_catd, tag, &leadid, string, &bytlen, &status)) 
		{
		   end123file (&fp_catd);
		   return (0);
        }

		G_strip (string);

        if (!check_data_sfld (catd_fname, fp_catd, tag, stag, frmts))
		{
		   end123file (&fp_catd);
		   return (0);
        }

       if (strcmp (stag, "NAME") == 0)
       {
		  if (!start_record)
		  {
		    put_err_mess ("Corrupt CATD file; Name field out of order.", 0);
			end123file (&fp_catd);
			return (0);
		  }
		  start_record = 0;

          if (!(catd_ptr = alloc_Sdts_catd_rec (S_info)))
		  {
		     put_err_mess ("Unable to allocate catd struct.", 0);
			 end123file (&fp_catd);
			 return (0);
		  }

          cur_catd_ptr = catd_ptr + (S_info->n_files -1); 

          strcpy (cur_catd_ptr->mod_name, string);
	      cur_catd_ptr->mod_nrec = 0;
          S_info->S_catd = catd_ptr;
       }
	   /*process attribute and composite modules*/
	   else if (strcmp (stag, "TYPE") == 0)
	   {
		  G_strip (string);
		  strcpy (cur_catd_ptr->type, string);
	   }
       else if (strcmp (stag, "FILE") == 0)
       {
		 start_record = 1;
		 strcpy (cur_catd_ptr->file_name, string);
		 if (strcmp (cur_catd_ptr->mod_name, "STAT") == 0)
		 {
		   strcpy (stat_fname, cur_catd_ptr->file_name);
			}
			else
		   if (strcmp (cur_catd_ptr->mod_name, "CATS") == 0)
			strcpy (cats_fname, cur_catd_ptr->file_name);


/*DEBUG*/
	   /*
	   fprintf (stderr, "TAG   = '%s'   ", tag);
	   fprintf (stderr, "DESCR = '%s'   ", stag);
	   fprintf (stderr, "mod name = %s file name = '%s' '%s' '%s'\n", cur_catd_ptr->mod_name, cur_catd_ptr->file_name, stat_fname, cats_fname);
       */
	   
	        /*PROCESS ATTRIBUTE AND COMPOSITE MODULES*/
	      {
		 if (strncmp (cur_catd_ptr->type, "Attribute P", 11) == 0 ||
			 strncmp (cur_catd_ptr->type, "ATTRIBUTE P", 11) == 0 )
			 {
			  build_att_ff_file_struct (cur_catd_ptr->file_name, &(Att_FF_info.AP), &(Att_FF_info.n_ap_files));
			 }
		 else
		 if (strncmp (cur_catd_ptr->type, "Attribute S", 11) == 0 ||
			 strncmp (cur_catd_ptr->type, "ATTRIBUTE S", 11) == 0 )
			 {
			  build_att_ff_file_struct (cur_catd_ptr->file_name, &(Att_FF_info.AS), &(Att_FF_info.n_as_files));
			 }
		 else
		 if (strncmp (cur_catd_ptr->type, "Composite", 9) == 0 ||
			 strncmp (cur_catd_ptr->type, "COMPOSITE", 9) == 0 )
			 {
			  build_att_ff_file_struct (cur_catd_ptr->file_name, &(Att_FF_info.FF), &(Att_FF_info.n_ff_files));
			 }
	      }
       }
   }


    if (!end_sdts_rfile (catd_fname, &fp_catd))
	   return (0);

    if (!do_STAT_module ( stat_fname, S_info ))
	{
	  /*if STAT module can't be processed, abort, since it is needed for other
	  * module processing */
	  fprintf (stderr, "Error: %s\n", get_err_mess());
      fprintf (stderr, "Unable to process SDTS STAT module.\n");
	  return (0);
    }


   if (!get_manifolds (cats_fname, S_info))
   {
	  fprintf (stderr, "Error: %s\n", get_err_mess());
	  G_fatal_error ("Unable to build manifold struct. Exiting");
   }

    /*DEBUG*//*display_manifolds (S_info);*/

    /*DEBUG*//* display_files(S_info);*/

    return (1);
}

do_IDEN_module (cur_mfold, S_globals)
    struct Sdts_manifold *cur_mfold;
    struct Sdts_globals *S_globals;
{
   FILE *fp_iden;
   char filename[100];
   char ice[2], ccs[4];
   long bytlen, int_level; 
   int status;
   char string[5000];
   char stag[5000];
   char frmts[5000];
   char tag[10];
   char leadid;
   
   strcpy (filename, cur_mfold->file_name[IDEN]);

   if (!open_sdts_rfile (filename, &int_level, ice, ccs, &fp_iden))
	   return (0);

   if (!read_dd_rec (filename, fp_iden, string, &status)) {
	  end123file (&fp_iden);
      return (0);
   }

   /*while not EOF*/
   while (status !=4)
   {
       if (!read_data_sfld (filename, fp_iden, tag, &leadid, string, &bytlen, &status)) 
	   {
		  end123file (&fp_iden);
		  return (0);
       }

	   G_strip (string);

       if (!check_data_sfld (filename, fp_iden, tag, stag, frmts))
	   {
		  end123file (&fp_iden);
		  return (0);
       }

       if (strcmp (stag, "TITL") == 0)
          G_strncpy (S_globals->Iden_titl, string, 40);
       else if (strcmp (stag, "MPDT") == 0)
          G_strncpy (S_globals->Iden_mpdt, string, 19);
       else if (strcmp (stag, "DCDT") == 0)
          G_strncpy (S_globals->Iden_dcdt, string, 19);
       else if (strcmp (stag, "SCAL") == 0)
          S_globals->Iden_scal = atol (string);

   
   }

   /*DEBUG*/
   /*
   fprintf (stderr, "map_name = '%s'\n", S_globals->Iden_titl);
   fprintf (stderr, "date = '%s'\n", S_globals->Iden_mpdt);
   fprintf (stderr, "cdate = '%s'\n", S_globals->Iden_dcdt);
   fprintf (stderr, "orig_scale = '%ld'\n", S_globals->Iden_scal);
   */

   if (!end_sdts_rfile (filename, &fp_iden))
	  return (0);

   return (1);

}

do_XREF_module (cur_mfold, S_globals)
    struct Sdts_manifold *cur_mfold;
    struct Sdts_globals *S_globals;
{
   FILE *fpin;
   char filename[100];
   char ice[2], ccs[4];
   long bytlen, int_level; 
   int status;
   char string[5000];
   char stag[5000];
   char frmts[5000];
   char tag[10];
   char leadid;
   
   strcpy (filename, cur_mfold->file_name[XREF]);

   if (!open_sdts_rfile (filename, &int_level, ice, ccs, &fpin))
	   return (0);

   if (!read_dd_rec (filename, fpin, string, &status)) 
   {
	   end123file (&fpin);
	   return (0);
   }

   /*while not EOF*/
   while (status !=4)
   {
       if (!read_data_sfld (filename, fpin, tag, &leadid, string, &bytlen, &status)) 
	   {
		   end123file (&fpin);
		   return (0);
       }

	   G_strip (string);

       if (!check_data_sfld (filename, fpin, tag, stag, frmts))
	   {
		   end123file (&fpin);
		   return (0);
       }

       if (strcmp (stag, "RSNM") == 0)
          G_strncpy (S_globals->Xref_rsnm, string, 4);
	   /*this isn't adequate to handle state plane or UPS, which can have 
	   letters*/
       else if (strcmp (stag, "ZONE") == 0)
          S_globals->Xref_zone = atoi (string);
   }

	   if (strcmp (S_globals->Xref_rsnm, "GEO") == 0)
			Xref_system = GEO;
	   else if (strcmp (S_globals->Xref_rsnm, "UTM") == 0)
			Xref_system = UTM;
	   else if (strcmp (S_globals->Xref_rsnm, "UPS") == 0)
			Xref_system = UPS;
       else if (strcmp (S_globals->Xref_rsnm, "SPCS") == 0)
            Xref_system = SPCS;
	   else
			Xref_system = OTHR;

   /*DEBUG*/
   /*
   fprintf (stderr, "ref system = '%s'\n", S_globals->Xref_rsnm);
   fprintf (stderr, "zone = '%d'\n", S_globals->Xref_zone);
   */

   if (!end_sdts_rfile (filename, &fpin))
	  return (0);

   if (Xref_system == SPCS)
   {
     G_fatal_error ("State Plane reference system not implemented in current version of v.in.sdts.\n");
   }

   if (Xref_system == OTHR)
   {
     G_fatal_error ("Only UTM, UPS, GEO, and SPCS Reference Systems are supported by the TVP.\n");
   }

   check_projection (Xref_system);

   if (Xref_system == UTM)
     check_zone (S_globals->Xref_zone);

   return (1);

}

check_projection (proj)
   int proj;
{
   int i;
   if ((i = G_projection ()) != proj)
     {
       fprintf (stderr, "Warning: Current GRASS projection (%s) does not match SDTS data set projection (%s)\n", 
	  G__projection_name (i), G__projection_name (proj));
       fprintf (stderr, "Entering 'Information Only' mode\n");
       info_flag = 1;
     }
}

check_zone (zone)
   int zone;
{
   int i;

   if ((i = G_zone()) != zone)
     {
       fprintf (stderr, "Warning: Current GRASS zone %d does not match SDTS data set zone (%d)\n", i, zone);
       fprintf (stderr, "Entering 'Information Only' mode\n");
       info_flag = 1;
     }
}

do_IREF_module (cur_mfold, S_globals)
    struct Sdts_manifold *cur_mfold;
    struct Sdts_globals *S_globals;
{
   FILE *fp_iref;
   char filename[100];
   char ice[2], ccs[4];
   long bytlen, int_level; 
   int status;
   char string[5000];
   char stag[5000];
   char frmts[5000];
   char tag[10];
   char leadid;
   

   strcpy (filename, cur_mfold->file_name[IREf]);

   if (!open_sdts_rfile (filename, &int_level, ice, ccs, &fp_iref))
	   return (0);


   if (!read_dd_rec (filename, fp_iref, string, &status)) 
   {
	  end123file (&fp_iref);
	  return (0);
   }

   /*while not EOF*/
   while (status !=4)
   {
       if (!read_data_sfld (filename, fp_iref, tag, &leadid, string, &bytlen, &status)) 
	   {
		  end123file (&fp_iref);
		  return (0);
       }

       G_strip (string);

       if (!check_data_sfld (filename, fp_iref, tag, stag, frmts))
	   {
		  end123file (&fp_iref);
		  return (0);
       }

       if (strcmp (stag, "XLBL") == 0)
          G_strncpy (S_globals->Iref_xlbl, string, 9);
       else if (strcmp (stag, "YLBL") == 0)
          G_strncpy (S_globals->Iref_ylbl, string, 9);
       else if (strcmp (stag, "SFAX") == 0)
           S_globals->Iref_sfax = atof (string);
       else if (strcmp (stag, "SFAY") == 0)
           S_globals->Iref_sfay = atof (string);
       else if (strcmp (stag, "XORG") == 0)
           S_globals->Iref_xorg = atof (string);
       else if (strcmp (stag, "YORG") == 0)
           S_globals->Iref_yorg = atof (string);
       else if (strcmp (stag, "XHRS") == 0)
           S_globals->Iref_xhrs = atof (string);
       else if (strcmp (stag, "YHRS") == 0)
           S_globals->Iref_yhrs = atof (string);
   

   }

   if (!end_sdts_rfile (filename, &fp_iref))
	 return (0);

   return (1);

}

do_STAT_module ( stat_fname,  info )
     char *stat_fname;
     struct Sdts_info *info;
{
   FILE *fp_stat;
   struct Sdts_catd *catd_ptr;
   char ice[2], ccs[4];
   long bytlen, int_level;
   int status;
   char string[5000];
   char descr[5000];
   char frmts[5000];
   char tag[10];
   char mod_name[5];
   char leadid;


   fprintf (stderr, "Processing STAT module\n");

   if (!open_sdts_rfile (stat_fname, &int_level, ice, ccs, &fp_stat))
	    return (0);


    if (!read_dd_rec (stat_fname, fp_stat, string, &status))
    {
		end123file (&fp_stat);
		return (0);
    }

	G_strip (string);

   while (status != 4)
     {
       
        if (!read_data_sfld (stat_fname, fp_stat, tag, &leadid, string, &bytlen, &status)) 
		{
		   end123file (&fp_stat);
		   return (0);
        }

	    G_strip (string);

        if (!check_data_sfld (stat_fname, fp_stat, tag, descr, frmts))
		{
		   end123file (&fp_stat);
		   return (0);
        }

       if (strcmp (tag, "STAT") == 0)
	   {
           if (!strcmp (descr, "MNRF"))
           {
                G_strncpy (mod_name, string, 4);
           }
           else
	       if (!strcmp (descr, "NREC"))
           {
                catd_ptr = find_module (mod_name, info, "STAT");
                if (catd_ptr)
                   catd_ptr->mod_nrec = atoi (string);
           }

	    }

     }

   if (!end_sdts_rfile (stat_fname, &fp_stat))
	   return (0);

   return (1);
}
	  

get_mod_index (name)
  char *name;
{
    int i;
    char tname[5];
    int cmp_chars = 4;

    strcpy (tname, name);

    if (*name == 'S' || *name == 'D' )
       cmp_chars = 3;
    else if (*name ==  'N' || *name == 'I')
       cmp_chars = 2;
    else if (*name == 'A' || *name == 'B' || *name == 'F' || *name == 'L'
        || *name == 'P' || *name == 'X')
        cmp_chars = 1;

    for (i = 1; Mod_in[i] != NULL; i++)
    {
        if (strncmp (Mod_in[i], name, cmp_chars) == 0)
           return (i);
    }

    return (0);
}

display_files (S_info)
  struct Sdts_info *S_info;
{
   int i;

   for (i = 0; i < S_info->n_files; i++)

	 fprintf (stderr, "#%d  %s %s %s\n", i, 
		S_info->S_catd[i].mod_name,
		S_info->S_catd[i].type,
		S_info->S_catd[i].file_name);
}



display_manifolds (S_info)
  struct Sdts_info S_info;
{
   int i;

   for (i = 0; i < FL_END; i++)
   {
	  if (i == IDEN || i == IREf || i == XREF)
		 fprintf (stderr, "#%d  %s\n", i, S_info.S_mfold->file_name[i]);
	  else
		 fprintf (stderr, "not filled yet\n");
   }
}

init_Sdts_globals (S_globals)
   struct Sdts_globals *S_globals;
{
      *(S_globals->Iden_titl) = '\0';
      *(S_globals->Iden_mpdt) = '\0';
      *(S_globals->Iden_dcdt) = '\0';
      S_globals->Iden_scal = 0;
      *(S_globals->Iref_xlbl) = '\0';
      *(S_globals->Iref_ylbl) = '\0';
      S_globals->Iref_sfax = 0;
      S_globals->Iref_sfay = 0;
      S_globals->Iref_xorg = 0;
      S_globals->Iref_yorg = 0;
      S_globals->Iref_xhrs = 0;
      S_globals->Iref_yhrs = 0;
      *(S_globals->Xref_rsnm) = '\0';
      S_globals->Xref_zone = 0;
}

display_globals (S_globals, head)
  struct Sdts_globals S_globals;
  struct dig_head *head;
{
     fprintf (stderr, "titl %s\n", S_globals.Iden_titl);
     fprintf (stderr, "mpdt %s\n", S_globals.Iden_mpdt);
     fprintf (stderr, "dcdt %s\n", S_globals.Iden_dcdt);
     fprintf (stderr, "xlbl %s\n", S_globals.Iref_xlbl);
     fprintf (stderr, "ylbl %s\n", S_globals.Iref_ylbl);
     fprintf (stderr, "rsnm %s\n", S_globals.Xref_rsnm);
     fprintf (stderr, "scal %d\n", S_globals.Iden_scal);
     fprintf (stderr, "sfax %f\n", S_globals.Iref_sfax);
     fprintf (stderr, "sfay %f\n", S_globals.Iref_sfay);
     fprintf (stderr, "xorg %f\n", S_globals.Iref_xorg);
     fprintf (stderr, "yorg %f\n", S_globals.Iref_yorg);
     fprintf (stderr, "xhrs %f\n", S_globals.Iref_xhrs);
     fprintf (stderr, "yhrs %f\n", S_globals.Iref_yhrs);
     fprintf (stderr, "zone %d\n", S_globals.Xref_zone);
	 fprintf (stderr, "head->N %f\n", head->N);
	 fprintf (stderr, "head->S %f\n", head->S);
	 fprintf (stderr, "head->E %f\n", head->E);
	 fprintf (stderr, "head->W %f\n", head->W);
}


make_dig_head (S_globals, sdts_prefix, bounds, head, cur_mfold)
    struct Sdts_globals *S_globals;
	char *sdts_prefix;
	struct Bounds *bounds;
	struct dig_head *head;
	struct Sdts_manifold *cur_mfold;
{
    char tmpstr[200];
    double min, max, margin;

	sprintf (tmpstr, "SDTS IMPORT: %s", sdts_prefix); 
    G_strncpy (head->organization, tmpstr, 29);
	sprintf (tmpstr, "IDEN DCDT: %s", S_globals->Iden_dcdt);
	G_strncpy (head->date, tmpstr, 19);
	G_strncpy (head->your_name, "...", 19);
	G_strncpy (head->map_name, S_globals->Iden_titl, 40);
	G_strncpy (head->source_date, S_globals->Iden_mpdt, 10);
	if (Multilayer_flag)
	{
	   sprintf (tmpstr, "SDTS manifold: %s", cur_mfold->mfold_name); 
	   G_strncpy (head->line_3, tmpstr, 72);
	}
	else
	   G_strncpy (head->line_3, "\0", 72);

	head->orig_scale = S_globals->Iden_scal;
	head->plani_zone = S_globals->Xref_zone;

	    /* Y */
	max = S_globals->Iref_sfay>0 ? bounds->max_y : bounds->min_y;
	min = S_globals->Iref_sfay>0 ? bounds->min_y : bounds->max_y;
        max *= S_globals->Iref_sfay;
	min *= S_globals->Iref_sfay;
        margin = (abs (max - min)) * .001;
        if (strcmp (S_globals->Xref_rsnm, "GEO"))
	  {
	     head->N = max + margin;
	     head->S = min - margin;
          }
        else
	  {
	     head->N = max + margin > 90 ? 90 : max + margin;
             head->S = min - margin < -90 ? -90 : min - margin;
          }

	    /* X */
	max = S_globals->Iref_sfax >0 ? bounds->max_x :bounds->min_x;
	min = S_globals->Iref_sfax >0 ? bounds->min_x : bounds->max_x;
    max *= S_globals->Iref_sfax;
	min *= S_globals->Iref_sfax;
	margin = (abs(max - min)) * .001;
	if (strcmp (S_globals->Xref_rsnm, "GEO"))
	  {
	    head->E = max + margin;
	    head->W = min - margin;
          }
        else
	  {
	    head->E = max + margin > 180 ? max + margin - 180: max + margin;
	    head->W = min - margin < -180 ? -180 - min + margin : min - margin;
          }
	/*probably the least misleading thing is just to set these values to zero*/
	head->digit_thresh = 0.00;
	head->map_thresh = 0.00;
}

