/*
**  Written by: Mike Higgins 5 1988
**  US Army Construction Engineering Research Lab
*/

#include	<stdio.h>
#include	"Vect.h"

int dig_write_plus_file(
	FILE   *fp_plus ,
	struct Map_info *Map ,
	struct Plus_head *Plus )
{


	rewind (fp_plus);  /* dpg 12/89 */

	if ( dig_Wr_Plus_head(Map,  Plus, fp_plus) < 0)
	{
		fprintf( stderr, "\nERROR: Can't write head to plus file.\n") ;
		return(-1) ;
	}

	if ( dig_write_nodes( fp_plus, Map, Plus) < 0)
	{
		fprintf( stderr, "\nERROR: Can't write nodes to plus file.\n") ;
		return(-1) ;
	}

	if ( dig_write_lines( fp_plus, Map, Plus) < 0)
	{
		fprintf( stderr, "\nERROR: Can't write lines to plus file.\n") ;
		return(-1) ;
	}

	if ( dig_write_areas( fp_plus, Map, Plus) < 0)
	{
		fprintf( stderr, "\nERROR: Can't write areas to plus file.\n") ;
		return(-1) ;
	}

	if ( dig_write_atts( fp_plus, Map, Plus) < 0)
	{
		fprintf( stderr, "\nERROR: Can't write atts to plus file.\n") ;
		return(-1) ;
	}

	if ( dig_write_isles( fp_plus, Map, Plus) < 0)
	{
		fprintf( stderr, "\nERROR: Can't write isles to plus file.\n") ;
		return(-1) ;
	}


	rewind(fp_plus) ;
	if ( dig_Wr_Plus_head(Map, Plus, fp_plus) < 0)
	{
		fprintf( stderr, "\nERROR: Can't write head to plus file.\n") ;
		return(-1) ;
	}

	rewind(fp_plus) ;
	if (dig_write_file_checks (fp_plus, Map, Plus))
	{
		fprintf( stderr, "\nERROR: writing file_checks.\n") ;
		return(-1) ;
	}

	fflush (fp_plus);
	return(0) ;
}	/*  write_plus_file()  */


int dig_write_nodes(
	FILE   *plus ,
	struct Map_info *Map ,
	struct Plus_head *Plus )
{
	int  i ;


	Plus->Node_offset = ftell(plus) ;

	for ( i = 1; i <= Plus->n_nodes; i++ )
	{
		if ( dig_Wr_P_node(Map, &Map->Node[i], plus) < 0)
			return(-1) ;
	}

	return(0) ;
}	/*  write_nodes()  */


int dig_write_lines(
	FILE   *plus ,
	struct Map_info *Map ,
	struct Plus_head *Plus )
{
	int  i ;


	Plus->Line_offset = ftell(plus) ;

	for ( i = 1; i <= Plus->n_lines; i++ )
	{
		if ( dig_Wr_P_line(Map, &Map->Line[i], plus) < 0)
			return(-1) ;
	}

	return(0) ;

}	/*  write_line()  */

int dig_write_areas(
	FILE   *plus ,
	struct Map_info *Map ,
	struct Plus_head *Plus )
{
	int  i ;


	Plus->Area_offset = ftell(plus) ;

	for ( i = 1; i <= Plus->n_areas; i++ )
	{
		if ( dig_Wr_P_area(Map, &Map->Area[i], plus) < 0)
			return(-1) ;
	}

	return(0) ;

}	/*  write_areas()  */

int dig_write_atts(
	FILE   *plus ,
	struct Map_info *Map ,
	struct Plus_head *Plus )
{
	int  i ;

	Plus->Att_offset = ftell(plus) ;

	for ( i = 1; i <= Plus->n_atts; i++ )
	{
		if ( dig_Wr_P_att(Map, &Map->Att[i], plus) < 0)
			return(-1) ;
	}
	return(0) ;
}	/*  write_atts()  */

int dig_write_isles(
	FILE   *plus ,
	struct Map_info *Map ,
	struct Plus_head *Plus )
{
	int  i ;


	Plus->Isle_offset = ftell(plus) ;

	for ( i = 1; i <= Plus->n_isles; i++ )
	{
		if ( dig_Wr_P_isle(Map, &Map->Isle[i], plus) < 0)
			return(-1) ;
	}

	return(0) ;

}	/*  write_isles()  */
