#include     <stdio.h>
#include     <signal.h>
#include     "gis.h"
#include     <Vect.h>
#include     "sdts_in.h"
#include     <stc123.h>

#define MAIN
#include  "sdts_globals.h"
#include "dig_head.h"

/*
v.in.sdts:

usage v.in.sdts catd=sdts catalogue (catd) filename out=outfile

*/

/**  data directories   **/
#define         B_DIG           "dig"
#define         A_DIG           "dig_ascii"
#define         SDTS            "sdts"
#define         ATT             "dig_att"
#define         PLUS            "dig_plus"
#define         OFFSET_ATT      "1"


static char  *current_mapset ;
static char  *gbase ;

static char  *PROG ;

main (argc, argv)
        int  argc ;
        char  *argv[] ;
{
    char *catd_fname;
    char *sdts_prefix;
    char *dig_name;
    char *mapset;
    struct Option *old, *new, *level, *add_attr, *offset;
    int zone;
    int ll_input;
    char name[128];
    char command[256];

    G_gisinit (argv[0]);

    old = G_define_option();
    old->key            = "input";
    old->type            = TYPE_STRING;
    old->required        = YES;
    old->multiple        = NO;
/*  It is not necessary that an SDTS transfer is under GRASS vector directory 
    old->gisprompt       = "old,sdts,catd";
*/
    old->description        = "sdts catd filename";
    

    new = G_define_option();
    new->key            = "output";
    new->type            = TYPE_STRING;
    new->required        = YES;
    new->multiple        = NO;
    new->gisprompt        = "new,dig,vector";
    new->description        = "vector output file";

    level = G_define_option();
    level->key            = "level";
    level->type            = TYPE_INTEGER;
    level->required        = NO;
    level->multiple        = NO;
    level->options        = "1,2,3";
    level->description      = "1 = first import stage (default),  2 = second import stage-get area attributes, 3 = info only, no import";
    
    gbase = G_gisbase();
    current_mapset = G_mapset();

    if (G_parser (argc, argv))
    exit (-1);


    catd_fname  = old->answer;
    dig_name = new->answer;


	/*set Level: Info only, First pass (default), or Second pass;*/
    if (level->answer)
	{
		/*DEBUG*/ fprintf (stderr, "answer str = '%s'\n", level->answer);
		/*DEBUG*/ fprintf (stderr, "answer int = '%d'\n", atoi(level->answer));
		Level = atoi (level->answer);
	}
    else
		Level = 1;

fprintf (stderr, "level = %d\n", Level); /*DEBUG*/

    /*verify sdts catd filename and file*/
#if 0
    if ((strncmp (catd_fname + 4, "CATD", 4)) && (strncmp (catd_fname + 4, "catd", 4)))
       G_fatal_error ("Illegal catd filename. Exiting.\n");
#endif
    if (strstr (catd_fname, "CATD") == NULL)
       G_fatal_error ("Input filename should be something like  xxxxCATD.xxx");

    doit (dig_name, catd_fname);

    exit(0);
}


struct Sdts_manifold *alloc_Sdts_manifold_struct ();
struct Sdts_catd *alloc_Sdts_catd_rec();

doit(dig_name, catd_fname)
    char *dig_name, *catd_fname;
{
    char *sdts_mapset;
    char *cur_mapset;
    char att_name[300];
    FILE *att, *fopen();
    struct Sdts_info S_info;
	struct Sdts_manifold *cur_mfold;
	struct Bounds bounds;
    struct Map_info Map;
	struct line_pnts *Points;
    struct dig_head head;
	char fname[20];
	int Obj_num;
	int i;

    cur_mapset = G_mapset();

/*initialize Sdts_info structure*/
    init_Sdts_info (&S_info);
    init_Sdts_globals (&S_globals);

/*setup GRASS output files */
    G__file_name (att_name, ATT, dig_name, cur_mapset);
    G__make_mapset_element (ATT);



/*get SDTS CAT/DICTIONARY info*/
    if ((do_CATD_module (catd_fname, &head, &S_info)) < 0)
         G_fatal_error ("Unable to process SDTS CATS file. Exiting");

    if ((get_manifolds (&S_info)) < 0)
		G_fatal_error ("Unable to build manifold struct. Exiting");

    /*DEBUG*//*display_manifolds (S_info);*/


	/*THE MAIN PROCESSING LOOP--once for each manifold*/
    for (i = 0; i < S_info.n_manifolds; i++)
	{
		Obj_num = 0;
		fprintf (stderr, "processing manifold %d, n_man = %d\n", i, S_info.n_manifolds);
		cur_mfold = S_info.S_mfold + i;

		sprintf (fname, "%s_%d", dig_name, i+1);
		if (0 > Vect_open_new (&Map, fname))
			 G_fatal_error ("Not able to create vector file ");

		sprintf (fname, "%s_%d", att_name, i+1);
		if (! (att = fopen(fname, "w")) )
			 G_fatal_error ("Not able to create att file ");

		Points = Vect_new_line_struct ();

		if (do_IDEN_module (cur_mfold, &S_globals) < 0)
		   G_fatal_error ("Unable to process SDTS IDEN file. Exiting");

		if (do_XREF_module (cur_mfold, &S_globals) < 0)
		   G_fatal_error ("Unable to process SDTS XREF file. Exiting");

		if (do_IREF_module (cur_mfold, &S_globals) < 0)
		   G_fatal_error ("Unable to process SDTS IREF file. Exiting");

		if (do_PC_module (cur_mfold, &S_globals, &bounds, &Map, Points, Level, &Obj_num) < 0)
		   G_fatal_error ("Unable to process polygon node file. Exiting");
		if (do_NO_module (cur_mfold, &S_globals, &bounds, &Map, Points, Level, &Obj_num) < 0)
		   G_fatal_error ("Unable to process no-planar node file. Exiting");

		if (do_NE_module (cur_mfold, &S_globals, &bounds, &Map, Points, Level, &Obj_num) < 0)
		   G_fatal_error ("Unable to process no-planar node file. Exiting");

		if (do_LE_module (cur_mfold, &S_globals, &bounds, &Map, Points, Level, &Obj_num) < 0)
		   G_fatal_error ("Unable to process no-planar node file. Exiting");

        make_dig_head (&S_globals, &bounds, &head);
		Vect_copy_head_data (&head, &Map.head);
		Vect_destroy_line_struct (Points);
		Vect_close (&Map);
		fclose (att);
	}
	/*DEBUG*/ display_globals (S_globals);



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
    int mod_index = 0;
    int start_record;
    char *stat_fname;


    if (!beg123file (catd_fname, 'r', &int_level, ice, ccs, &fp_catd))
    {
       sprintf (Error_msg, "Can't open SDTS CATD file: %s\n", catd_fname);
       G_fatal_error (Error_msg);
    }

    if (!rd123ddrec (fp_catd, string, &status)) {
       sprintf (Error_msg, "Unable to read data record. status = %d\n", status);
       G_fatal_error (Error_msg);
    }

   /*while not EOF*/

   start_record = 1; 

   while (status !=4)
   {
       if (!rd123sfld (fp_catd, tag, &leadid, string, &bytlen, &status)) {
         sprintf (Error_msg, "Unable to read DR subfield; status = %d\n", status);
         G_fatal_error (Error_msg);
       }

        if (!chk123sfld(fp_catd, tag, stag, frmts)){
          sprintf (Error_msg, "chk123slfd: can't read DR subfield description");
          G_fatal_error (Error_msg);
        }

       if (strcmp (stag, "NAME") == 0)
       {
		  if (!start_record)
			 G_fatal_error ("corrupt CATD file. Exiting");
		  start_record = 0;

          if (!(catd_ptr = alloc_Sdts_catd_rec (S_info)))
               G_fatal_error ( " Unable to allocate catd struct. Exiting");
          /*
          cur_catd_ptr = catd_ptr[(S_info->n_files) -1]; 
          */
          cur_catd_ptr = catd_ptr + (S_info->n_files -1); 

          strcpy (cur_catd_ptr->mod_name, string);
	  cur_catd_ptr->mod_nrec = 0;
          S_info->S_catd = catd_ptr;
       }
       else if (strcmp (stag, "FILE") == 0)
       {
			if (start_record)
			   G_fatal_error ("CATD file corrupt. Exiting.");
			start_record = 1;
            strcpy (cur_catd_ptr->file_name, string);
	    if (strcmp (cur_catd_ptr->mod_name, "STAT") == 0)
	      stat_fname = cur_catd_ptr->file_name;

/*DEBUG*/
/*
       fprintf (stderr, "TAG   = '%s'   ", tag);
       fprintf (stderr, "DESCR = '%s'   ", stag);
       fprintf (stderr, "mod name = %s file name = '%s' \n", cur_catd_ptr->mod_name, cur_catd_ptr->file_name);
*/
       }
   }

   end123file (&fp_catd);

   /*
   do_STAT_module ( stat_fname, S_info->S_catd );
   */
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
   
/*copy to IDEN filename, after lowercasing */
   /*
   strcpy (filename, G_tolcase(cur_mfold->file_name[IDEN]));
   */
   strcpy (filename, cur_mfold->file_name[IDEN]);

   /*DEBUG*/ fprintf (stderr, "iden filename '%s'\n", filename);
   if (!beg123file (filename, 'r', &int_level, ice, ccs, &fp_iden))
   {
       sprintf (Error_msg, "Can't open SDTS IDEN file: %s\n", filename);
       G_fatal_error (Error_msg);
   }

   if (!rd123ddrec (fp_iden, string, &status)) {
      sprintf (Error_msg, "Unable to read data record. status = %d\n", status);
      G_fatal_error (Error_msg);
   }

   /*while not EOF*/
   while (status !=4)
   {
       if (!rd123sfld (fp_iden, tag, &leadid, string, &bytlen, &status)) {
         sprintf (Error_msg, "Unable to read DR subfield; status = %d\n", status);
         G_fatal_error (Error_msg);
       }

        if (!chk123sfld(fp_iden, tag, stag, frmts)){
         sprintf (Error_msg, "chk123slfd: can't read DR subfield description");
         G_fatal_error (Error_msg);
        }

       if (strcmp (stag, "TITL") == 0)
          G_strncpy (S_globals->Iden_titl, string, 40);
       else if (strcmp (stag, "MPDT") == 0)
          G_strncpy (S_globals->Iden_mpdt, string, 19);
       else if (strcmp (stag, "DCDT") == 0)
          G_strncpy (S_globals->Iden_dcdt, string, 19);
       else if (strcmp (stag, "SCAL") == 0)
          S_globals->Iden_scal = atol (string);
#ifdef FOO
       if (strcmp (stag, "TITL") == 0)
          G_strncpy (head->map_name, string, 40);
       else if (strcmp (stag, "MPDT") == 0)
          G_strncpy (head->date, string, 19);
       else if (strcmp (stag, "SCAL") == 0)
       {
          head->orig_scale = atol (string);
          head->map_thresh = 0.001 * head->orig_scale;
       }
       fprintf (stderr, "TAG   = '%s'\n", tag);
       fprintf (stderr, "DESCR = '%s'\n", stag);
       fprintf (stderr, "field = '%s'\n", string);

   /*DEBUG*/fprintf (stderr, "map_name = '%s'\n", head->map_name);
   /*DEBUG*/fprintf (stderr, "date = '%s'\n", head->date);
   /*DEBUG*/fprintf (stderr, "orig_scale = '%ld'\n", head->orig_scale);
   /*DEBUG*/fprintf (stderr, "map_thresh = '%f'\n", head->map_thresh);
   
#endif FOO
   }
   /*DEBUG*/fprintf (stderr, "map_name = '%s'\n", S_globals->Iden_titl);
   /*DEBUG*/fprintf (stderr, "date = '%s'\n", S_globals->Iden_mpdt);
   /*DEBUG*/fprintf (stderr, "cdate = '%s'\n", S_globals->Iden_dcdt);
   /*DEBUG*/fprintf (stderr, "orig_scale = '%ld'\n", S_globals->Iden_scal);

   end123file (&fp_iden);
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
   
/*copy to XREF filename, after lowercasing */
   /*
   strcpy (filename, G_tolcase(cur_mfold->file_name[XREF]));
   */
   strcpy (filename, cur_mfold->file_name[XREF]);

   if (!beg123file (filename, 'r', &int_level, ice, ccs, &fpin))
   {
       sprintf (Error_msg, "Can't open SDTS XREF file: %s\n", filename);
       G_fatal_error (Error_msg);
   }

   if (!rd123ddrec (fpin, string, &status)) {
      sprintf (Error_msg, "Unable to read data record. status = %d\n", status);
      G_fatal_error (Error_msg);
   }

   /*while not EOF*/
   while (status !=4)
   {
       if (!rd123sfld (fpin, tag, &leadid, string, &bytlen, &status)) {
         sprintf (Error_msg, "Unable to read DR subfield; status = %d\n", status);
         G_fatal_error (Error_msg);
       }

        if (!chk123sfld(fpin, tag, stag, frmts)){
         sprintf (Error_msg, "chk123slfd: can't read DR subfield description");
         G_fatal_error (Error_msg);
        }

       if (strcmp (stag, "RSNM") == 0)
          G_strncpy (S_globals->Xref_rsnm, string, 4);
       else if (strcmp (stag, "ZONE") == 0)
          S_globals->Xref_zone = atoi (string);
   }

	   if (strcmp (S_globals->Xref_rsnm, "GEO") == 0)
			Xref_system = GEO;
	   else if (strcmp (S_globals->Xref_rsnm, "UTM") == 0)
			Xref_system = UTM;
	   else if (strcmp (S_globals->Xref_rsnm, "UPS") == 0)
			Xref_system = UPS;
	   else
			Xref_system = OTHR;

   /*DEBUG*/fprintf (stderr, "ref system = '%s'\n", S_globals->Xref_rsnm);
   /*DEBUG*/fprintf (stderr, "zone = '%d'\n", S_globals->Xref_zone);


   end123file (&fpin);
   return (1);

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
   

/*copy to IREF filename, after lowercasing */
   /*
   strcpy (filename, G_tolcase(cur_mfold->file_name[IREf]));
   */
   strcpy (filename, cur_mfold->file_name[IREf]);

   if (!beg123file (filename, 'r', &int_level, ice, ccs, &fp_iref))
   {
       sprintf (Error_msg, "Can't open SDTS IREF file: %s\n", filename);
       G_fatal_error (Error_msg);
   }


   if (!rd123ddrec (fp_iref, string, &status)) {
      sprintf (Error_msg, "Unable to read data record. status = %d\n", status);
      G_fatal_error (Error_msg);
   }

   /*while not EOF*/
   while (status !=4)
   {
	   fprintf (stderr, "reading IREF subfield\n");
       if (!rd123sfld (fp_iref, tag, &leadid, string, &bytlen, &status)) {
         sprintf (Error_msg, "IREF module: Unable to read DR subfield; status = %d\n", status);
         G_fatal_error (Error_msg);
       }

        if (!chk123sfld(fp_iref, tag, stag, frmts)){
         sprintf (Error_msg, "chk123slfd: can't read DR subfield description");
         G_fatal_error (Error_msg);
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

   end123file (&fp_iref);
   return (1);

}

do_STAT_module ( stat_fname,  catd_ptr )
     char *stat_fname;
     struct Sdts_catd *catd_ptr;
{
   FILE *fp_stat;
   char ice[2], ccs[4];
   long bytlen, int_level;
   int status;
   char string[5000];
   char descr[5000];
   char frmts[5000];
   char tag[10];
   char leadid;
   int  num_mod = -1;


   if (!beg123file (stat_fname, 'R', &int_level, ice, ccs, &fp_stat))
     {
       sprintf (Error_msg, "Can't open SDTS STAT file: %s\n", stat_fname);
       G_fatal_error (Error_msg);
     }

   if (! rd123ddrec (fp_stat, string, &status))
     {
       sprintf (Error_msg, "Unable to read data record. status = %d\n", status);
       G_fatal_error (Error_msg);
     }
   fprintf (stderr, "reading STAT subfield\n");

   while (status != 4)
     {
       
       if (!rd123sfld (fp_stat, tag, &leadid, string, &bytlen, &status))
	 {
	   sprintf (Error_msg, "STAT module: Unable to read DR subfield; status = %d\n", status);
	   G_fatal_error (Error_msg);
	 }

       if (!chk123sfld(fp_stat, tag, descr, frmts))
	 {
	   sprintf (Error_msg, "chk123slfd: can't read DR subfield description");
	   G_fatal_error (Error_msg);
	 }

       if (strcmp (tag, "STAT") == 0)
	 {
	   if (!strcmp (descr, "NREC"))
	     catd_ptr++->mod_nrec = atoi (string);
	 }
     }
   end123file (&fp_stat);

   return (1);
 }
	  
#ifdef FOO
Open_sdtsfile (sdts_path, file_name, fp, mode)
   char *sdts_path;
   FILE **fp;
   char mode;
{
   char full_name[250];

   G__file_name (full_name, sdts_path, file_name, G_mapset());

    if (!open_sdtsfile (full_name, fp, mode))
    {
         fprintf (stderr, "couldn't open %s outputfile. exiting.\n", file_name);
         return (0);
    };

}

open_sdtsfile (out_name, fp, mode)
   char *out_name;
   FILE **fp;
   char mode;
{
   FILE *fpout;
   long int_level;
   char ccs[4];
   char tag [10];
   char ice[2];
   int option, prv_stat, status;

   int_level = 2;
   strcpy (ice," ");
   strcpy (ccs,"   ");
   if (!beg123file (out_name, mode, &int_level, ice, ccs, fp))
   {
       printf ("couldn't open fips output file\n");
       exit (1);
   }
   return (1);

}

get_mod_index (name)
  char *name;
{
    int i;
    char tname[5];
    char err_msg[100];
    int cmp_chars = 4;

    strcpy (tname, name);

    if (*name == 'S' || *name == 'D' )
       cmp_chars = 3;
    else if (*name ==  'N' || *name == 'I')
       cmp_chars = 2;
    else if (*name == 'A' || *name == 'B' || *name == 'F' || *name == 'L'
        || *name == 'P' || *name == 'X')
        cmp_chars = 1;

    for (i = 1; Mod_in[i].name != NULL; i++)
    {
        if (strncmp (Mod_in[i].name, name, cmp_chars) == 0)
           return (Mod_in[i].index);
    }

    return (0);
}
#endif /*FOO*/

display_files (S_info)
  struct Sdts_info *S_info;
{
   int i;

   for (i = 0; i < S_info->n_files; i++)
	 fprintf (stderr, "#%d  %s\n", i, S_info->S_catd[i].file_name);
}

/*get_manifolds: at present, one manifold is assumed,  this module 
  has to be massively changed to accomadte the real stuff
*/
get_manifolds (S_info)
   struct Sdts_info *S_info;
{
   struct Sdts_manifold *mptr = NULL;
   struct Sdts_catd *cptr;
   int i, *p;

   cptr = S_info->S_catd;

   if (!(mptr = alloc_Sdts_manifold_struct (S_info)))
	 G_fatal_error ("Can't allocate manifold structure. Exiting");
  
   for (i = 0, p = mptr->mod_nrec; i < 30 ; i ++, p++)
       *p = 0;

   for (i = 0; i < S_info->n_files; i++)
   {
	  if (strcmp (cptr[i].mod_name, "IDEN") == 0)
	  {
		  mptr->mod_name[IDEN] = G_malloc (MODN_SIZE);
		  mptr->file_name[IDEN] = G_malloc (FNAME_SIZE);
		  strcpy (mptr->mod_name[IDEN], cptr[i].mod_name);
		  strcpy (mptr->file_name[IDEN], cptr[i].file_name);
		  mptr->mod_nrec[IDEN] = cptr[i].mod_nrec;
	  }
	  if (strcmp (cptr[i].mod_name, "IREF") == 0)
	  {
		  mptr->mod_name[IREf] = G_malloc (MODN_SIZE);
		  mptr->file_name[IREf] = G_malloc (FNAME_SIZE);
		  strcpy (mptr->mod_name[IREf], cptr[i].mod_name);
		  strcpy (mptr->file_name[IREf], cptr[i].file_name);
		  mptr->mod_nrec[IREf] = cptr[i].mod_nrec;
	  }
	  if (strcmp (cptr[i].mod_name, "XREF") == 0)
	  {
		  mptr->mod_name[XREF] = G_malloc (MODN_SIZE);
		  mptr->file_name[XREF] = G_malloc (FNAME_SIZE);
		  strcpy (mptr->mod_name[XREF], cptr[i].mod_name);
		  strcpy (mptr->file_name[XREF], cptr[i].file_name);
		  mptr->mod_nrec[XREF] = cptr[i].mod_nrec;
	  }
	  if (strncmp (cptr[i].mod_name, "NO", 2) == 0)
	  {
		  mptr->mod_name[NOxx] = G_malloc (MODN_SIZE);
		  mptr->file_name[NOxx] = G_malloc (FNAME_SIZE);
		  strcpy (mptr->mod_name[NOxx], cptr[i].mod_name);
		  strcpy (mptr->file_name[NOxx], cptr[i].file_name);
		  mptr->mod_nrec[NOxx] = cptr[i].mod_nrec;
	  }
	  if (strncmp (cptr[i].mod_name, "PC", 2) == 0)
	  {
		  mptr->mod_name[PCxx] = G_malloc (MODN_SIZE);
		  mptr->file_name[PCxx] = G_malloc (FNAME_SIZE);
		  strcpy (mptr->mod_name[PCxx], cptr[i].mod_name);
		  strcpy (mptr->file_name[PCxx], cptr[i].file_name);
		  mptr->mod_nrec[PCxx] = cptr[i].mod_nrec;
	  }
	  if (strncmp (cptr[i].mod_name, "NE", 2) == 0)
	  {
		  mptr->mod_name[NExx] = G_malloc (MODN_SIZE);
		  mptr->file_name[NExx] = G_malloc (FNAME_SIZE);
		  strcpy (mptr->mod_name[NExx], cptr[i].mod_name);
		  strcpy (mptr->file_name[NExx], cptr[i].file_name);
		  mptr->mod_nrec[NExx] = cptr[i].mod_nrec;
	  }

	  if (strncmp (cptr[i].mod_name, "LE", 2) == 0)
	  {
		  mptr->mod_name[LExx] = G_malloc (MODN_SIZE);
		  mptr->file_name[LExx] = G_malloc (FNAME_SIZE);
		  strcpy (mptr->mod_name[LExx], cptr[i].mod_name);
		  strcpy (mptr->file_name[LExx], cptr[i].file_name);
		  mptr->mod_nrec[LExx] = cptr[i].mod_nrec;
	  }
   }

   S_info->S_mfold = mptr;

    return (1); 
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

display_globals (S_globals)
  struct Sdts_globals S_globals;
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
}


make_dig_head (S_globals, bounds, head)
    struct Sdts_globals *S_globals;
	struct Bounds *bounds;
	struct dig_head *head;
{
    char tmpstr[200];

    G_strncpy (head->organization, "USGS DLG-3 SDTS Import", 29);
	G_strncpy (head->date, "unknown", 19);
	G_strncpy (head->your_name, "Import from SDTS-DLG", 19);
	G_strncpy (head->map_name, S_globals->Iden_titl, 40);
	G_strncpy (head->source_date, S_globals->Iden_mpdt, 10);
	sprintf (tmpstr, "data set creation: %s", S_globals->Iden_dcdt);
	G_strncpy (head->line_3, tmpstr, 72);
	head->orig_scale = S_globals->Iden_scal;
	head->plani_zone = S_globals->Xref_zone;
	head->N = 1.001 * S_globals->Iref_sfax * (double) bounds->max_y;
	head->S = .999 * S_globals->Iref_sfay * (double) bounds->min_y;
	head->E = 1.001 * S_globals->Iref_sfax * (double) bounds->max_x;
	head->W = .999 * S_globals->Iref_sfay * (double) bounds->min_x;
	head->digit_thresh = 0.001;
	head->map_thresh = 0.001 * head->orig_scale;
}
