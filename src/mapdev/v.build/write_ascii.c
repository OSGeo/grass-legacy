#include	<stdio.h>
#include	"Vect.h"
#include "vbuildlib.h"

int write_plus_asc (struct Map_info *map, FILE *fp_ascii, FILE *fp_plus)
{

	struct Plus_head Plus ;

	if ( dig_Rd_Plus_head(map, &Plus, fp_plus) < 0)
	{
		fprintf( stderr, "\nERROR: Can't read Dig_plus head.\n") ;
		exit(-1) ;
	}

	if ( Wr_Plus_head_asc( &Plus, fp_ascii) < 0)
	{
		fprintf( stderr, "\nERROR: Can't write ascii head.\n") ;
		exit(-1) ;
	}

	fprintf( fp_ascii, "\n\n  NODES:  %d,    current offset: %ld\n  -----\n\n",
		Plus.n_nodes, ftell(fp_plus)) ;

	fflush(fp_ascii) ;

	if ( write_nodes_asc( map, fp_ascii, fp_plus, &Plus) < 0)
	{
		fprintf( stderr, "\nERROR: Can't write ascii nodes.\n") ;
		exit(-1) ;
	}

	fprintf( fp_ascii, "\n\n  LINES:  %d,  current offset: %ld\n  -----\n\n",
		Plus.n_lines, ftell(fp_plus)) ;

	fflush(fp_ascii) ;

	if ( write_lines_asc(map,  fp_ascii, fp_plus, &Plus) < 0)
	{
		fprintf( stderr, "\nERROR: Can't write ascii lines.\n") ;
		exit(-1) ;
	}

	fprintf( fp_ascii, "\n\n  AREAS:  %d,  current offset: %ld\n  -----\n\n",
		Plus.n_areas, ftell(fp_plus)) ;

	fflush(fp_ascii) ;

	if ( write_areas_asc(map,  fp_ascii, fp_plus, &Plus) < 0)
	{
		fprintf( stderr, "\nERROR: Can't write ascii areas.\n") ;
		exit(-1) ;
	}

	fprintf( fp_ascii, "\n\n  ATTRIBUTES:  %d,  current offset: %ld\n  -----\n\n",
		Plus.n_atts, ftell(fp_plus)) ;

	fflush(fp_ascii) ;

	if ( write_atts_asc(map,  fp_ascii, fp_plus, &Plus) < 0)
	{
		fprintf( stderr, "\nERROR: Can't write ascii atts.\n") ;
		exit(-1) ;
	}


	fprintf( fp_ascii, "\n\n  ISLANDS:  %d,  current offset: %ld\n  -----\n\n",
		Plus.n_isles, ftell(fp_plus)) ;

	fflush(fp_ascii) ;

	if ( write_isles_asc(map,  fp_ascii, fp_plus, &Plus) < 0)
	{
		fprintf( stderr, "\nERROR: Can't write ascii isles.\n") ;
		exit(-1) ;
	}


	return(0) ;

}	/*  write_plus_asc()  */


int 
write_nodes_asc (struct Map_info *map, FILE *ascii, FILE *plus, struct Plus_head *Plus)
{
	register int  i ;
	struct P_node Node ;
	register int	cnt ;


	cnt = 0 ;

	Node.n_lines = Node.alloc_lines = 0;
	for ( i = 1; i <= Plus->n_nodes; i++ )
	{
		if ( dig_Rd_P_node(map, &Node, plus) < 0)
			return(0) ;
		if ( Wr_P_node_asc( i, &Node, ascii) < 0)
			return(-1) ;
		++cnt ;
	}

	fprintf( stderr, " node count: %d\n", ++cnt) ;
	return(0) ;

}	/*  write_nodes()  */


int 
write_lines_asc (struct Map_info *map, FILE *ascii, FILE *plus, struct Plus_head *Plus)
{
	register int  i ;
	struct P_line Line ;


	for ( i = 1; i <= Plus->n_lines; i++ )
	{
		if ( dig_Rd_P_line(map,&Line, plus) < 0)
			return(0) ;
		if ( Wr_P_line_asc( i, &Line, ascii) < 0)
			return(-1) ;
	}
	return(0) ;

}	/*  write_line()  */




int 
write_areas_asc (struct Map_info *map, FILE *ascii, FILE *plus, struct Plus_head *Plus)
{
	register int  i ;
	struct P_area Area ;


	Area.n_lines = Area.alloc_lines = 0;
	for ( i = 1; i <= Plus->n_areas; i++ )
	{
		if ( dig_Rd_P_area(map, &Area, plus) < 0)
			return(0) ;
		if ( Wr_P_area_asc(i, &Area, ascii) < 0)
			return(-1) ;
	}
	return(0) ;

}	/*  write_areas()  */


int 
write_atts_asc (struct Map_info *map, FILE *ascii, FILE *plus, struct Plus_head *Plus)
{
	register int  i ;
	struct P_att Att ;

	for ( i = 1; i <= Plus->n_areas; i++ )
	{
		if ( dig_Rd_P_att(map, &Att, plus) < 0)
			return(0) ;
		if ( Wr_P_att_asc(i, &Att, ascii) < 0)
			return(-1) ;
	}
	return(0) ;

}	/*  write_areas()  */

int 
write_isles_asc (struct Map_info *map, FILE *ascii, FILE *plus, struct Plus_head *Plus)
{
	register int  i ;
	struct P_isle Isle ;


	Isle.n_lines = Isle.alloc_lines = 0;
	for ( i = 1; i <= Plus->n_isles; i++ )
	{
		if ( dig_Rd_P_isle(map, &Isle, plus) < 0)
			return(0) ;
		if ( Wr_P_isle_asc(i, &Isle, ascii) < 0)
			return(-1) ;
	}
	return(0) ;

}	/*  write_isles()  */
