/**** parse.c ****/
/*
**  Written by David Gerdes  Fall 1992
**  US Army Construction Engineering Research Lab
**  Copyright  David Gerdes  USA-CERL  1992
*/


#include "gis.h"
#include "Vect.h"
#include "parse.h"
#include "cutter.h"

parse_args (argc, argv, Args)
    int argc;
    char *argv[];
    struct ARGS *Args;
{
    struct Option *VectA;
    struct Option *VectB;
    struct Option *Outfile;
    struct Flag *All_flag;
    struct Flag *Quiet_flag;

    G_gisinit(argv[0]);

    VectA = G_define_option();
    VectA->key 		= "cutter";
    VectA->type		= TYPE_STRING;
    VectA->required		= YES;
    VectA->multiple		= NO;
    VectA->gisprompt		= "old,dig,Vector";
    VectA->description	= "Cutter vector file";

    VectB = G_define_option();
    VectB->key 		= "data";
    VectB->type		= TYPE_STRING;
    VectB->required		= YES;
    VectB->multiple		= NO;
    VectB->gisprompt		= "old,dig,Vector";
    VectB->description	= "Data vector file";

    if (output_open)
    {
	Outfile = G_define_option();
	Outfile->key 		= "out";
	Outfile->type		= TYPE_STRING;
	Outfile->required		= YES;
	Outfile->multiple		= NO;
	Outfile->gisprompt		= "new,dig,Vector";
	Outfile->description	= "Output vector file";
    }

    All_flag  		  = G_define_flag ();
    All_flag->key   	  =  'u';
    All_flag->description = "Output unlabled data polygons also";

    Quiet_flag  		= G_define_flag ();
    Quiet_flag->key   		=  'q';
    Quiet_flag->description  	= "Run quietly";


    if (G_parser (argc, argv))
	exit(-1);

    Args->MapA = VectA->answer;
    Args->MapB = VectB->answer;
    if (output_open)
	Args->Out = Outfile->answer;

    All   = All_flag->answer   == NULL ? 0 : 1;
    Quiet = Quiet_flag->answer == NULL ? 0 : 1;

    return (0);
}


/* open_files (Args, MapA, MapB, Out) */

open_files (Args, Maps, Out)
    struct ARGS *Args;
    struct Map_info Maps[2];
    struct Map_info *Out;
{
    int level;
    char *mapset;
    char errmsg[200];
    int a, b;
    char buf[200];

    if (NULL == (mapset = G_find_file2 ("dig", Args->MapA, "")))
    {
	sprintf (errmsg, "Could not find file '%s'", Args->MapA);
	G_fatal_error (errmsg);
    }

    level = Vect_open_old (&(Maps[A_CODE]), Args->MapA, mapset);

    if (level <= 0)
	G_fatal_error ("Failed to open file %s\n", Args->MapA);

    if (level < 2)
    {
      Vect_close (&(Maps[A_CODE]));
      G_fatal_error("Both files must be Level 2 format.  First run v.support");
    }


    if (NULL == (mapset = G_find_file2 ("dig", Args->MapB, "")))
    {
	sprintf (errmsg, "Could not find file '%s'", Args->MapB);
	G_fatal_error (errmsg);
    }

    level = Vect_open_old (&(Maps[B_CODE]), Args->MapB, mapset);
    if (level <= 0)
    {
	Vect_close (&(Maps[B_CODE]));
	G_fatal_error ("Failed to open file %s\n", Args->MapB);
    }

    if (level < 2)
    {
      Vect_close (&(Maps[A_CODE]));
      Vect_close (&(Maps[B_CODE]));
      G_fatal_error("Both files must be Level 2 format.  First run v.support");
    }

    if ( (a = (!Maps[A_CODE].all_areas || !Maps[A_CODE].all_isles)) ||
         (b = (!Maps[B_CODE].all_areas || !Maps[B_CODE].all_isles)) )
    {
      char *buf1, *buf2;

      buf1 = G_store(Maps[A_CODE].digit_file);
      buf2 = G_store(Maps[B_CODE].digit_file);

      Vect_close (&(Maps[A_CODE]));
      Vect_close (&(Maps[B_CODE]));

      if (a && b)
      {
	  sprintf(errmsg, "You first need to run %s on both input files", Vect_support_name());
	  G_fatal_error(errmsg);
      }
      if (a)
	sprintf (errmsg, "You first need to run %s on file '%s'\n", Vect_support_name (), buf1);
      else
	sprintf (errmsg, "You first need to run %s on file '%s'\n", Vect_support_name (), buf2);

      G_fatal_error(errmsg);
    }


    if (!output_open)
	return 0;

    level = Vect_open_new (Out, Args->Out);
    if (level <= 0)
    {
	Vect_close (&(Maps[A_CODE]));
	Vect_close (&(Maps[B_CODE]));
	G_fatal_error ("Failed to open file %s for create\n", Args->Out);
    }

    /*
    **  Open up dig_att file  for output map 
    */
    G__file_name (buf, "dig_att", Args->Out, G_mapset());
    Out->att_file = G_store (buf);
    if (NULL == (Out->att_fp = fopen (Out->att_file, "w")))
    {
	Vect_close (&(Maps[A_CODE]));
	Vect_close (&(Maps[B_CODE]));
	Vect_close (Out);
	G_fatal_error ("Can't open dig_att file for output");
    }

    Vect_copy_head_data (&(Maps[A_CODE].head), &(Out->head));

    return 0;
}
