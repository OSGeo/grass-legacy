 /* @(#) 1.11 6/27/90 /usr/grass3.1/src.scs/scspsu/s.vtoc.c */
 /* @(#) 1.20 4/23/91 /usr/grass4.0/src.contrib/SCS/psu/s.vtoc.c */
#include "Vect.h"
#include "gis.h"

extern int cur_category;
char * G_malloc ();
char * G_calloc ();
double fabs();
int format;

extern struct line_pnts *Points;

#define MAX_DIST  99999999.0


struct area_size {
    double size;
    int area;
};

    static unsigned char     *Carray;		
    static short *Sarray;		
    static CELL      *Iarray;		
    static int num_cols, num_rows;

    static int tmp_file;	/* if true (default) will use a temp file */
				/*  if false, will use a RAM buffer       */
				/*  do this by setting GTUNE_VTOC_RAM     */
    static FILE *tmpfp;
    static long file_size;

main(argc, argv)
	int argc ;
	char *argv[] ;
{
	register int col, row, newmap, file_row;
	int pkgs ;
	int cell_type ;
	int packages ;
	char *data;
	char *memptr, errmsg[100];
	char *old_name, *new_name;
	char *file_name;	/* for temp file */

	struct Map_info Map ;
	struct Map_info Out_Map ;
	struct Cell_head header ;
	struct Option *new, *old, *psu, *subj, *xdist, *ydist;
	CELL *record;
	int row_off;
	struct Categories cats;
	int stat;
	char *mapset;
	int area;
	long timer;
	long f_size;
	int bufsize,junk;
	FILE *psu_att, *psu_data;
	double xdst, ydst;

	/* Initialize gis library */
        G_gisinit(argv[0]);

        old = G_define_option();
        old->key                        = "input";
        old->type                       = TYPE_STRING;
        old->required           	= YES;
        old->multiple           	= NO;
        old->gisprompt          	= "old,dig,vector";
        old->description                = "vector input file";

        new = G_define_option();
        new->key                        = "output";
        new->type                       = TYPE_STRING;
        new->required           	= YES;
        new->multiple           	= NO;
        new->gisprompt          	= "new,dig,vector";
        new->description                = "vector output file";

        psu = G_define_option();
        psu->key                        = "psu";
        psu->type                       = TYPE_STRING;
        psu->required           	= YES;
        psu->multiple           	= NO;
        psu->description                = "psu_data file";

        subj = G_define_option();
        subj->key                       = "subj";
        subj->type                      = TYPE_STRING;
        subj->required           	= YES;
        subj->multiple           	= NO;
	subj->gisprompt			= "old,SUBJ,subject";
        subj->description               = "subject file";

        xdist = G_define_option();
        xdist->key                      = "xdist";
        xdist->type                     = TYPE_STRING;
        xdist->required           	= NO;
        xdist->description              = "x distance";

        ydist = G_define_option();
        ydist->key                      = "ydist";
        ydist->type                     = TYPE_STRING;
        ydist->required           	= NO;
        ydist->description              = "y distance";

        if (G_parser (argc, argv))
               exit (-1);

	old_name = old->answer;
	new_name = new->answer;

	xdst = ydst = 0.0;
	if (xdist->answer){
		sscanf(xdist->answer, "%d", &junk);
		xdst = (double) junk;
	}
	if (ydist->answer){
		sscanf(ydist->answer, "%d", &junk);
		ydst = (double) junk;
	}
		/* convert to meters */
	xdst = xdst * 0.3048;
	ydst = ydst * 0.3048;

    /* check to see if we want to run using RAM buffer instead of tmp file */
	if (getenv ("GTUNE_VTOC_RAM") != NULL)
	    tmp_file = 0;
	else
	    tmp_file = 1;

        if ((mapset = G_find_vector2 (old_name, "")) == NULL)
          {
          sprintf (errmsg, "Could not find Vector file <%s>\n", old_name);
          G_fatal_error (errmsg);
          }


        stat = Vect_open_old (&Map, old_name, mapset);
        if (stat < 0)
          {
          sprintf (errmsg, "Could not read Vector file <%s>\n", old_name);
          G_fatal_error (errmsg);
          }

	/* initialize Points structure */
	Points = Vect_new_line_struct ();

/* Do initial read of DIGIT file */
	/*printf ("\nLoading vector information.\n");*/
	
	if (0 > Vect_open_new (&Out_Map,  new_name))
		G_fatal_error("Opening new dig file");

	if ((psu_att = G_fopen_new("dig_att",new_name)) == NULL)
		G_fatal_error("Opening new att file");

	if ((psu_data = fopen(psu->answer,"r")) == NULL)
		G_fatal_error("Opening psu_data file");

	if (G__read_cats("SUBJ",subj->answer, mapset, &cats, 1) < 0)
		G_fatal_error("Opening and reading SUBJ file");
	
	Vect_copy_head_data(&(Map.head), &(Out_Map.head));
	
	packages = 0 ;

/* Process areas */

/*	pkgs = do_areas(&Map, &header) ;
	if (pkgs < 0)
		G_fatal_error("Working on digit areas.") ;
	packages += pkgs ;
*/

	pkgs = do_lines(&Map, &Out_Map, psu_att, psu_data, &cats, xdst, ydst) ;
	if (pkgs < 0)
		G_fatal_error("Working on digit lines.") ;
	packages += pkgs ;

	/*printf( "Step 4: Creating Site information...   ") ;*/
	pkgs = do_sites(&Map, &header) ;
	if (pkgs < 0)
		G_fatal_error("Working on digit sites.") ;
	packages += pkgs ;

	Vect_close(&Map) ;
	Vect_close(&Out_Map) ;

	if (G_write_vector_cats(new_name, &cats) < 0)
		G_fatal_error("Opening writing dig_cats file");

	exit(0) ;
}


do_areas (Map, header)
    struct Map_info *Map;
    struct Cell_head *header ;
{
	return(0);
}

