static char rcsid[] = "$Header$";
/*
$Log$
Revision 1.1  1999-12-29 15:12:12  markus
Initial revision

 * Revision 1.1  1991/06/25  03:31:35  paul
 * Initial revision
 *
 * Revision 1.1  1991/05/29  03:50:43  paul
 * Initial revision
 *
*/

#include <stdio.h>
#include "Vect.h"
#include "digit.h"
#include "gis.h"

#define METERS_PER_INCH	0.0254

main(argc, argv)
	int argc ;
	char **argv ;
{
	int  scstig, x, verbose;
	char  *mapset;
	FILE *tig_file1, *tig_file2;
	FILE *dig,*dig_cat,*dig_att,*fopen();
	char filename1[256], filename2[256];
	char  *rindex(), *G_tempfile() ;
	char buff[256];
	struct Map_info Map;

	struct Option *tig1opt, *tig2opt, *digopt, *cfcopt;
	struct Flag *scsflag, *verflag;

	tig1opt = G_define_option();
	tig1opt->key 		= "tig1";
	tig1opt->description	= "TIGER file 1";
	tig1opt->type		= TYPE_STRING;
	tig1opt->required	= YES;
	tig1opt->gisprompt	= "old,tiger,tiger";

	tig2opt = G_define_option();
	tig2opt->key 		= "tig2";
	tig2opt->description	= "TIGER file 2";
	tig2opt->type		= TYPE_STRING;
	tig2opt->required	= YES;
	tig2opt->gisprompt	= "old,tiger,tiger";

	digopt = G_define_option();
	digopt->key 		= "out";
	digopt->description	= "New vector file";
	digopt->type		= TYPE_STRING;
	digopt->required	= YES;
	digopt->gisprompt	= "new,dig,vector";

	cfcopt = G_define_option();
	cfcopt->key 		= "cfc";
	cfcopt->description	= "CFCC codes";
	cfcopt->type		= TYPE_STRING;
	cfcopt->required	= YES;
	cfcopt->multiple	= YES;

	scsflag = G_define_flag();
	scsflag->key		= 'c';
	scsflag->description	= "Condensed  TIGER file";

	verflag = G_define_flag();
	verflag->key		= 'v';
	verflag->description	= "Verbose output";

	G_gisinit();
	if (G_parser(argc, argv))
		exit(-1);

	scstig = scsflag->answer;
	verbose = verflag->answer;

	G_strcpy(filename1, tig1opt->answer);
	mapset = G_find_file2("tiger", filename1, "");
	if ( (tig_file1 = G_fopen_old("tiger",filename1, mapset)) == NULL)
		G_fatal_error("Can't find input TIGER BASIC file") ;
	G_strcpy(filename2, tig2opt->answer);
	mapset = G_find_file2("tiger", filename2, "");
	if ( (tig_file2 = G_fopen_old("tiger",filename2,mapset)) == NULL)
		G_fatal_error("Can't find input TIGER SHAPE file");
	
	if (verbose){
		fprintf (stdout,"\nConverting the tiger import file: %s\n", filename1);
		fprintf (stdout,"     to digit file: %s\n", digopt->answer);
	}

	 if (Vect_open_new(&Map, digopt->answer) < 0)
		G_fatal_error("Can't open new vector file");
	if ( (dig_att = G_fopen_new("dig_att", digopt->answer)) == NULL)
		G_fatal_error("Can't create dig_atts");
	if ( (dig_cat = G_fopen_new("dig_cats", digopt->answer)) == NULL)
		G_fatal_error("Can't create dig_cats");
	fclose(dig_cat);
  	sprintf(buff,"cp %s/etc/CFCC.code %s/%s/dig_cats/%s\n",
		G_gisbase(), G_location_path(), G_mapset(), digopt->answer);
	G_system(buff);

	do_head(&Map, verbose);
	/* Read and write the main body */
	if (imp_tig(&Map,digopt->answer,cfcopt->answers, tig_file1,tig_file2,dig_att,scstig,verbose) == -1)
		G_fatal_error("Import failed");
	Vect_close(&Map);	

        fclose (tig_file1);
        fclose (tig_file2);
        fclose (dig_att) ;

	exit(0);
}
