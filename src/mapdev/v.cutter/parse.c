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

int parse_args (int argc, char *argv[], struct ARGS *Args)
{
    struct Option *VectA;
    struct Option *VectB;
    struct Option *Outfile;
    struct Option *Type;
    struct Flag *All_flag;
    struct Flag *Quiet_flag;
    struct Flag *Area_arcs_flag;
    struct Flag *Line_arcs_flag;

    G_gisinit(argv[0]);

    VectA = G_define_option();
    VectA->key 		= "cutter";
    VectA->type		= TYPE_STRING;
    VectA->required		= YES;
    VectA->multiple		= NO;
    VectA->gisprompt		= "old,dig,Vector";
    VectA->description	= "Cutter vector file";

    VectB = G_define_option();
    VectB->key 		= "input";
    VectB->type		= TYPE_STRING;
    VectB->required		= YES;
    VectB->multiple		= NO;
    VectB->gisprompt		= "old,dig,Vector";
    VectB->description	= "Data vector file";

    if (output_open)
    {
	Outfile = G_define_option();
	Outfile->key 		= "output";
	Outfile->type		= TYPE_STRING;
	Outfile->required		= YES;
	Outfile->multiple		= NO;
	Outfile->gisprompt		= "new,dig,Vector";
	Outfile->description	= "Output vector file";
    }

    Type = G_define_option();
    Type->key	= "type";
    Type->type	= TYPE_STRING;
    Type->required	= NO;
    Type->multiple	= NO;
    Type->options	= "area,line,both";
    Type->answer	= "line";
    Type->description	= "Type of object to extract (area, line or both)";

    All_flag  		  = G_define_flag ();
    All_flag->key   	  =  'o';
    All_flag->description = "only labeled areas will be extracted";

    Area_arcs_flag	= G_define_flag ();
    Area_arcs_flag->key	= 'a';
    Area_arcs_flag->description	="Limit line output to area-edge arcs";

    Line_arcs_flag	= G_define_flag ();
    Line_arcs_flag->key	= 'l';
    Line_arcs_flag->description	="Limit line output to line arcs";

    Quiet_flag  		= G_define_flag ();
    Quiet_flag->key   		=  'q';
    Quiet_flag->description  	= "Run quietly";

    if (G_parser (argc, argv))
	exit(-1);

    Args->MapA = VectA->answer;
    Args->MapB = VectB->answer;
    if (output_open)
	Args->Out = Outfile->answer;

    Quiet = Quiet_flag->answer == 0 ? 0 : 1;

    Do_areas=0;
    All=Do_lines=1;
    if(*Type->answer == 'a' ||
       *Type->answer == 'b' )Do_areas=1;

    if(Do_lines)
    {
       ltype=BOTH;
       if(Line_arcs_flag->answer)ltype=LINE;
       else if(Area_arcs_flag->answer)ltype=AREA;
    }

    if(Do_areas)
    {
       if(All_flag->answer)All=0;
    }

    return (0);
}


/* open_files (Args, MapA, MapB, Out) */

int open_files (struct ARGS *Args, struct Map_info Maps[2], struct Map_info *Out)
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

    if (level <= 0) {
	sprintf (errmsg,"Failed to open file %s\n", Args->MapA);
	G_fatal_error (errmsg);
    }

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
	sprintf (errmsg, "Failed to open file '%s'", Args->MapB);
	G_fatal_error (errmsg);
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
	  G_fatal_error("You first need to run %s on both input files",
	        Vect_support_name());
      }
      if (a) buf2 = buf1;

      G_fatal_error("You first need to run %s on file '%s'\n",
               Vect_support_name (), buf2);
    }


    if (!output_open)
	return 0;

    level = Vect_open_new (Out, Args->Out);
    if (level <= 0)
    {
	Vect_close (&(Maps[A_CODE]));
	Vect_close (&(Maps[B_CODE]));
	sprintf (errmsg,"Failed to open file %s for create\n", Args->Out);
	G_fatal_error (errmsg);
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
