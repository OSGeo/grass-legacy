/*  @(#)edit_head.c    2.1  6/26/87  */
/*
**  R.L.Glenn, SCS, 12/5/89
*/

#include <signal.h>
#include "gis.h"
#include <stdio.h>

#define MAIN
#include "dig_head.h"
#include "digit.h"


#ifdef DEBUG
#include <sys/types.h>
#include <time.h>
#endif


static char *N_dig_file;
static char *N_plus_file;
static char *N_att_file;
static char *N_coor_file;
static char *N_digitizer;
static char *N_name;
static char *N_PPID;

/*
**  calling sequence
**  digit file_name path_to_mapset parent_pid digitizer_tty
*/
main(argc, argv)
    int argc;
    char **argv;
{
    FILE *digit, *fopen(), *attr, *plus;
    char buf[1024];
    int have_old;
    int have_plus;
    int have_attr;
    int ret;
    unsigned	short	getuid() ;
    unsigned	short	getgid() ;
    char *memptr;	/* for load_file */


    /* Couldn't call G_gisinit () because of UID stuff */
    G_no_gisinit ("DIGIT3.1");


    if (argc != 2)
    {
	sprintf(buf, "Usage: %s  map_name \n", argv[0]);
	G_fatal_error (buf);
    }

    /* store the original file names */
    {
	N_name = argv[1];

	sprintf (buf, "%s/%s/%s", getenv("LOCATION"), "dig", N_name);
	N_dig_file= G_store (buf);

	sprintf (buf, "%s/%s/%s", getenv("LOCATION"), "dig_plus", N_name);
	N_plus_file = G_store (buf);

	sprintf (buf, "%s/%s/%s", getenv("LOCATION"), "dig_att", N_name);
	N_att_file = G_store (buf);

	sprintf (buf, "%s/%s/%s", getenv("LOCATION"), "reg", N_name);
	N_coor_file = G_store (buf);

    }

    have_old = have_plus = have_attr = 0;
    if ( (digit = fopen(N_dig_file, "r+")) != NULL )
	have_old = 1;
    else
    {
	if ( (digit = fopen(N_dig_file, "w+") ) == NULL )
	{
	    sprintf (buf, "Not able to open <%s>\n", N_dig_file);
	    G_fatal_error (buf);
	}
	printf ( "\nCreating a new vector file\n");
	have_old = 0;
    }

    if ((plus = fopen (N_plus_file, "r+")) != NULL)
    {
	fclose (plus);
	have_plus = 1;
    }
    else
    {
	if ( (plus = fopen(N_plus_file, "w+") ) == NULL )
	{
	    if (have_old)
	    {
		G_fatal_error ("No dig_plus file exists. You must run support.vect\n");
	    }
	    sprintf (buf, "Not able to open <%s>\n", N_plus_file);
	    G_fatal_error  (buf);
	}
	fclose (plus);
	unlink (N_plus_file);
	have_plus = 0;
    }


    if ((attr = fopen (N_att_file, "r+")) != NULL)
	have_attr = 1;
    else
    {
	if ( (attr = fopen(N_att_file, "w+") ) == NULL )
	{
	    sprintf (buf, "Not able to open <%s>\n", N_att_file);
	    G_fatal_error (buf);
	}
	have_attr = 0;
    }

    if (have_old)
    {
	ret = dig_do_file_checks (N_plus_file, N_dig_file, N_att_file);
	if (ret < 0)
	{
	    fprintf (stderr, "Could not open dig_plus file\n");
	    fprintf (stderr, "You must first run support.vect\n");
	    sleep (4);
	    exit (-1);
	}
    }

    if (have_old)
	dig_read_head_binary(digit, &head);

    get_head_info(have_old);

    dig_write_head_binary(digit, &head);

    exit (ret);  /*redundant */
}


do_file_checks (map)
    struct Map_info *map;
{
    FILE *fp;
    struct Plus_head Plus;

    if ((fp = fopen (map->plus_file, "r+")) == NULL)
    {
	G_fatal_error ("Can't open Plus file for final write\n");
    }
    dig_Rd_Plus_head (&Plus, fp);
    rewind (fp);
    dig_write_file_checks (fp, map, &Plus);
    fclose (fp);
}



get_head_info(have_old)
	int  have_old ;
{

	if( ! have_old)
		strcpy(head.organization, "US Army Const. Eng. Rsch. Lab") ;

	V_clear() ;
	V_line(1,"Provide the following information:") ;

	V_line(3,"Your organization") ;
	V_line(4,"Todays date (mon,yr)") ;
	V_line(5,"Your name") ;
	V_line(6,"Map's name") ;
	V_line(7,"Map's date") ;
	V_line(8,"Map's scale         1:") ;
	V_line(9,"Other info") ;
	V_line(10,"Zone") ;
	V_line(11,"Map Threshhold ") ;
	V_line(12,"West edge of area") ;
	V_line(13,"South edge of area") ;
	V_line(14,"East edge of area") ;
	V_line(15,"North edge of area") ;

	V_ques( head.organization, 's', 3,  20, 30-1) ;
	V_ques( head.date,         's', 4,  20, 20-1) ;
	V_ques( head.your_name,    's', 5,  20, 20-1) ;
	V_ques( head.map_name,     's', 6,  20, 41-1) ;
	V_ques( head.source_date,  's', 7,  20, 11-1) ;
	V_ques( &head.orig_scale,  'i', 8,  22, 9) ;
	V_ques( head.line_3,       's', 9,  20, 59-1) ;
	V_ques( &head.plani_zone,  'i', 10, 20, 5)  ;
	V_ques( &head.map_thresh,  'd', 11, 20, 14) ;

	V_ques( &head.W,           'd', 12, 20, 14) ;
	V_ques( &head.S,           'd', 13, 20, 14) ;
	V_ques( &head.E,           'd', 14, 20, 14) ;
	V_ques( &head.N,           'd', 15, 20, 14) ;
	

	V_call() ;

}
