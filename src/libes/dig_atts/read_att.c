
/*  ./src/libes/atts/read_att.c
 *******************************************************************
 *
 *  This file includes read_area_att(), read_line_att(),  read_att() and
 *  read_att_struct(), check_type().
 *
 *  #include "dig_atts.h"
 *
 *  read_area_att (fp, atts_index, att, num)
 *          OR
 *  read_line_att (fp, atts_index, att, num)
 *      FILE *fp              file containing attributes
 *      struct atts_index *atts_index
 *               structure containing offsets to attribute file
 *      struct attribute *att
 *               structure to load info into
 *      int num              area/line # attribute to access
 *
 * returns:  -1 on error
 *            0 on completion
 *
 */

 /*
 *******************************************************************
 *  Two different functions to read from the att file.
 *  read_att_struct() reads attribute information from file and loads it
 *  into an att structure.
 *  read_att() reads attribute information and loads into arguments instead of
 *  structure.
 *
 *  #include "dig_atts.h"
 *
 *  read_att_struct (fp ,att)
 *      FILE *fp                 file containing attributes
 *      struct attribute *att  structure to load info into
 *
 *  read_att (fp, type, x, y, cat, offset)
 *	FILE	*fp ;
 *	int	*type ;
 *	double	*x,  *y ;
 *	int	*cat ;
 *	long	*offset ;
 *
 *	Notice that this does no seeking.  It assumes that the file pointer is
 *	at the proper point in the file.
 *
 * returns:  -1 on error
 *            0 on completion
 *            1 on end of file
 */

 /***
 *  check_type(c)
 *	char c;
 *  Check if type is a valid attribute type.
 *  return 1 - valid
 *  return 0 - invalid
 ***/


#include "dig_atts.h"
#include <stdio.h>


#define		BUFFERSIZE	128

read_area_att (fp, atts_index, att, num)
    FILE *fp ;
	struct atts_index *atts_index ;
	struct attribute *att ;
    	int num ;
{
	if (fseek(fp, atts_index->area_off[num], 0) != 0)
		return(-1) ;
	return( read_att_struct(fp, att)) ;

}	 /*  read_area_att  */


read_line_att (fp, atts_index, att, num)
    FILE *fp ;
	struct atts_index *atts_index ;
	struct attribute *att ;
    	int num ;
{
	if (fseek(fp, atts_index->line_off[num], 0) != 0)
		return(-1) ;
	return( read_att_struct(fp, att)) ;

}	 /*  read_line_att  */


read_att_struct (fp, att)
	FILE	*fp ;
	struct attribute *att ;
{

	int	num_read ;
	char	buf[BUFFERSIZE];
	long	ftell() ;

	att->offset = ftell(fp) ;

	if ( fgets( buf, BUFFERSIZE-1, fp ) == NULL)
		return(1) ;

	num_read = sscanf( buf, READ_ATT_FORMAT, &att->type, &att->x, &att->y,
		&att->cat) ;

	if (num_read != 4)
		return(-1) ;
	return( check_type(&(att->type)) ? 0 : -1) ;

}	/*  read_att_struct()  */


read_att (fp, type, x, y, cat, offset)
	FILE	*fp ;
	char	*type ;
	double	*x,  *y ;
	int	*cat ;
	long	*offset ;
{

	int	num_read ;
	char	buf[BUFFERSIZE];

	long	ftell() ;

	*offset = ftell(fp) ;

	if ( fgets( buf, BUFFERSIZE-1, fp ) == NULL)
		return(1) ;

	num_read = sscanf( buf, READ_ATT_FORMAT, type, x, y, cat) ;

	if (num_read != 4)
	{
		return(-1) ;
	}

	return( check_type(type) ? 0 : -1) ;

}	/*  read_att()  */


static
check_type(type)
	char	*type ;
{
	register int i;
	register char  *ptr ;

	ptr = ATT_TYPES;
	for ( i=0; ptr[i] != '\0'; i++)
		if(ptr[i] == *type)
		{
			*type = i;
			return(1) ;
		}
	return(0) ;
}
