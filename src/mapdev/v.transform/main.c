/*
*  This takes an ascii digit file in one coordinate system and converts
*  the map to another coordinate system.
*  Uses the transform library:  ../src/libes/vect32/libes/libtrans.a .
*
*  Written during the ice age of Illinois, 02/16/90, by the GRASS team, -mh.
*
** Modified by Dave Gerdes  1/90  for dig_head stuff
**
** converted to grass4.0--new parser installed 1/91 - dks
*/

/*
*  Current is the existing file to be converted.
*  Trans is the new transformed file.
*/

#define MAIN
#ifdef OLD
#define USAGE	"[mapin=name] [mapout=name] [coord=name] [verbose=(yes,no)]"
#endif

#include <string.h>
#include  "trans.h"
#include "gis.h"
#include "Vect.h"
#include "local_proto.h"

static  char  *Prog_name ;

int main (int argc, char *argv[])
{

	struct dig_head dhead;
	struct  file_info  Current ;
	struct  file_info  Trans ;
	struct  file_info  Coord ;
	struct GModule *module;
	struct Option *old, *new, *pointsfile;
	struct Flag *quiet_flag;

#ifdef OLD
	extern int stash_away() ;
	extern int my_help() ;
#endif

	G_gisinit(argv[0]) ;

	module = G_define_module();
	module->description =
		"Transforms an ASCII vector map layer from one "
		"coordinate system into another coordinate system.";

	quiet_flag = G_define_flag();
	quiet_flag->key		= 'y';
	quiet_flag->description = "suppress display of residuals or other information"; 

        old = G_define_option();
        old->key			= "input";
        old->type			= TYPE_STRING;
        old->required			= YES;
        old->multiple			= NO;
        old->gisprompt			= "old,dig_ascii,ascii";
        old->description		= "ascii vector map to be transformed";
        
        new = G_define_option();
        new->key			= "output";
        new->type			= TYPE_STRING;
        new->required			= YES;
        new->multiple			= NO;
        new->gisprompt			= "new,dig_ascii,ascii";
        new->description		= "resultant ascii vector map";

        pointsfile = G_define_option();
        pointsfile->key			= "pointsfile";
        pointsfile->type			= TYPE_STRING;
        pointsfile->required		= NO;
        pointsfile->multiple		= NO;
        pointsfile->description		= "file holding transform coordinates";
        
    if (G_parser (argc, argv))
	exit (-1);
    strcpy (Current.name, old->answer);
    strcpy (Trans.name, new->answer);
    if (pointsfile->answer != NULL)
	strcpy (Coord.name, pointsfile->answer);
    else
	Coord.name[0] = '\0';
    
    /*DEBUG*/
/*
    if (!*Coord.name)
      fprintf (stderr, "coord = blank");
    else
      fprintf (stderr, "coord.name[0] = %c\n", *Coord.name);
*/

    if (!*Current.name  || !*Trans.name )
    {
        fprintf (stderr, "\n%s: Command line error: missing input or output name.\n\n", argv[0]);
	G_usage();
        exit (-1);
    }

	Prog_name = argv[0] ;


/* Check command line */
#ifdef OLD
	set_default_options( &Current, &Trans, &Coord, &Flags ) ;

	if ( argc > 1 )
	{

		G_set_parse_command_usage( my_help) ;
		if (G_parse_command(argc, argv, com_keys, stash_away))
		{
			fprintf(stderr,"Usage: %s %s\n", argv[0], USAGE) ;
			pr_options() ;
			exit(-1) ;
		}
	}
#endif

/*let's get to work*/

	open_vect_files ( &Current, &Trans, &Coord ) ;

	/*  read the header first, so that if there any errors reading the
	*   the header they don't have to type in the numbers before
        *   finding out.
	*/

	if ( 0 > dig_read_head_ascii(Current.fp, &dhead) )
	{
		fprintf( stderr, "\nERROR: Could not read the header information in the ascii digit file: %s .\n\n",  Current.full_name ) ;
		exit (-1) ;
	}

	create_transform_conversion( &Coord, quiet_flag->answer);

	transform_head_info(&dhead) ;
	dig_write_head_ascii( Trans.fp, &dhead) ;


	if (!quiet_flag->answer)
		fprintf (stdout,"\n\n Now transforming the vectors ...") ;
	transform_digit_file( Current.fp, Trans.fp) ;

	fflush(Trans.fp) ;
	if (Coord.name[0] != '\0')
		fclose( Coord.fp) ;
	fclose( Current.fp) ;
	fclose( Trans.fp) ;
	if (!quiet_flag->answer)
		fprintf (stdout,"\n '%s' has finished the transformation of the vectors.\n", argv[0]) ;

	if ( open_att_files ( &Current, &Trans) )
	{
		if (!quiet_flag->answer)
			fprintf (stdout,"\nThere was no dig_att file to convert with this vector file. \n") ;
		exit(0) ;
	}

	transform_att_file( Current.fp, Trans.fp) ;

	if (!quiet_flag->answer)
	{
		fprintf (stdout," '%s' has finished the transformation of the vector's attribute file.\n", argv[0]) ;
		fprintf (stdout,"Transformation is complete.\n") ;
	}
	fclose( Current.fp) ;
	fclose( Trans.fp) ;

	exit(0) ;

}


#ifdef OLD
int 
my_help (void)
{
	/*G_parse_command_usage( Prog_name, com_keys, USAGE_SHORT) ;*/
}

int 
pr_options (void)
{
	fprintf(stderr,"\n    Options:\n") ;
	fprintf(stderr,"     mapin  - name of existing map to transform.\n") ;
	fprintf(stderr,"     mapout - name of transformed map.\n") ;
	fprintf(stderr,"     coord  - name of file holding transformation coordinates.\n") ;
	fprintf(stderr,"     verbose  - print the stats or not (default is yes).\n") ;
}
#endif
