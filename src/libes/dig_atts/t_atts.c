
#include	<stdio.h>
#include	"dig_atts.h"

main()
{

	int i ;
	FILE	*fp,  *fopen() ;
	FILE	*fp_out ;
	struct attribute att ;
	struct atts_index atts_index ;


	if ( (fp = fopen("atts_sample", "r"))  ==  NULL)
	{
		fprintf(stderr, "Can't open file for read\n") ;
		exit(-1) ;
	}

	if ( (fp_out = fopen("atts.out", "w"))  ==  NULL)
	{
		fprintf(stderr, "Can't open file for write\n") ;
		exit(-1) ;
	}

/**  read thru the attribute file and store indexes in atts_index  **/
	if (atts_init( fp, &atts_index) < 0)
	{
		fprintf(stderr, "ERROR: Problem in initial read of attribute file\n") ;
		exit(-1) ;
	}

/**  start from the beginning of the files  */
	rewind(fp) ;
	rewind(fp_out) ;

/**  show information () loaded into the structure  **/

	fprintf (stdout,"\n\n  DLG ATTRIBUTE information:\n\n") ;
	fprintf (stdout," max:   areas %d,  lines %d,  atts %d\n", atts_index.max_areas,
		atts_index.max_lines, atts_index.max_atts) ;
	fprintf (stdout," area_alloc %d,  line_alloc %d\n", atts_index.area_alloc, atts_index.line_alloc) ;


/**  read all the area attributes, print to screen and write to file **/
	fprintf (stdout,"\n\n  AREA ATTRIBUTES:\n\n") ;
	for( i = 1;  i <= atts_index.max_areas;  ++i)
	{
		fprintf (stdout,"   area offset: %ld \n", atts_index.area_off[i]) ;

	    /*  read the area att  */
		if (read_area_att( fp, &atts_index,  &att, i))
		{
			fprintf (stdout,"\n Cannot read attribute file\n") ;
			break ;
		}

	    /*  write the attribute  */
		if (write_att_struct( fp_out, &att))
		{
			fprintf (stdout,"\n Cannot write att file\n") ;
			break ;
		}


		fprintf (stdout,WRITE_ATT_FORMAT, att.type, att.x, att.y, att.cat) ;
		fprintf (stdout,"   offset: %ld \n", att.offset) ;

		fprintf (stdout,"\n") ;

	}


/**  read all the line att, print to screen and write to file **/

	fprintf (stdout,"\n\n  LINE ATTRIBUTES:\n\n") ;
	for( i = 1;  i <= atts_index.max_lines;  ++i)
	{
		fprintf (stdout,"   line offset: %ld \n", atts_index.line_off[i]) ;
		if (read_line_att( fp, &atts_index,  &att, i))
		{
			fprintf (stdout,"\n Cannot read att file\n") ;
			break ;
		}

		if (write_att_struct( fp_out, &att))
		{
			fprintf (stdout,"\n Cannot write att file\n") ;
			break ;
		}


		fprintf (stdout,WRITE_ATT_FORMAT, att.type, att.x, att.y, att.cat) ;
		fprintf (stdout,"   offset: %ld \n", att.offset) ;

		fprintf (stdout,"\n") ;

	}

	free_atts(&atts_index) ;

	fclose(fp) ;
	fclose(fp_out) ;
}


