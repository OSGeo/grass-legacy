
/*
*	show_driver_names() - show the names and descriptions of the digitizers
*	from the digiticap file.
*
*	get_driver_name() - verifies that a name is a valid digitizer name and
*	also copies information about that matched digitizer.
*		-  returns the number of digitizers defined in the digitcap file until
*          it finds a matching digitizer.  if a null is passed as the name
*          it would return a total count of digitizers.
*		-  returns 0 if there was nothing in the digitcap file.  
*
*	get_driver() - verifies driver selection by its position in the
*       digitcap file.
*
*	read_cap_line() - read a string (skips comment lines) from digitcap file
*	and parse it.
*/

#include	<stdio.h>
#include	"bin_digit.h"

#define		BUFFERSIZE	256
#define		COMMENT_CHAR	'#'

show_driver_names( fp, Driver)
	FILE	*fp ;
	struct  driver_desc  *Driver ;
{

	int		status ;
	int		cnt ;

/*  show available digitizers on the system   */

	printf("\n\n\n\n\n                 Available  Digitizers\n\n") ;
	printf("           Name                   Description\n") ;
	printf("           ----                   -----------\n") ;
	
	fseek( fp, 0L, 0) ;
	cnt = 1 ;

	while ( (status = read_cap_line( fp, Driver)))
	{
		if (status < 0)
		{
				fprintf( stderr,"\n Error in reading digitcap file.\n") ;
				fprintf(stderr, "\n Contact your GRASS system administrator\n") ;
				fclose(fp) ;
				exit(-1) ;
		}

		printf("   [%d]    %-20s %s", cnt++,  Driver->name, Driver->dig_desc) ;
	}

}			/*  show_driver_names  */



read_cap_line( fp, Driver)
	FILE	*fp ;
	struct  driver_desc  *Driver ;
{
    int		num_read ;
    char	buf[BUFFERSIZE];
    char        cbuf[10];

    while ( fgets( buf, BUFFERSIZE-1, fp ) != NULL)
    {


	/*  skip commented lines  */
	if (1 != sscanf (buf, "%1s", cbuf))
	    continue;

	if ( buf[0] == COMMENT_CHAR)
	    continue ;

/*DEBUG fprintf (stderr, "%s", buf);*/

	*Driver->name = NULL ;  *Driver->device = NULL ;
	*Driver->dig_filename = NULL ;  *Driver->dig_desc = NULL ;

	/*  
	*	the notation '%[^:]' in the sscanf means copy all characters
	*	into the the assigned memory (name,..) until a ':' is found.
	*/
	num_read = sscanf( buf, "%[^:]:%[^:]:%[^:]:%[^:]", Driver->name,
	Driver->device, Driver->dig_filename, Driver->dig_desc) ;

	if ( ! num_read)
	{
/*DEBUG fprintf (stderr, "Scanf failed\n");*/
	    continue ;
	}

	/* check to make sure that values were copyed into these fields,
	*  there doesn't have to be a digitizer description
	*/
	if ( *Driver->name  &&  *Driver->device  &&  *Driver->dig_filename)
	{
/*DEBUG fprintf (stderr, "GOOD read\n");*/
	    return(1) ;
	}

/*DEBUG fprintf (stderr, "Bad read\n");*/

	return(-1) ;
    }

/*DEBUG fprintf (stderr, "END OF FILE\n");*/

    return(0) ;

}


get_driver( fp, which_driver, Driver)
	FILE  *fp ;
	int  which_driver ;
	struct  driver_desc  *Driver ;
{

	int		status ;
	int		cnt ;

	cnt = 0 ;
	fseek( fp, 0L, 0) ;

	while ( (status = read_cap_line( fp, Driver)))
	{

		if (status < 0)
		{
				fprintf( stderr," Error in reading digitcap file.\n") ;
				fprintf(stderr, "Contact your GRASS system administrator\n") ;
				fclose(fp) ;
				exit(-1) ;
		}

		if ( which_driver == (cnt+1))
			return(++cnt) ;
		++cnt ;

	}

	return(0) ;

}			 /*  get_driver_name()  */


get_driver_name( fp, selected_name, Driver)
	FILE  *fp ;
	char  *selected_name ;
	struct  driver_desc  *Driver ;
{

	int		status ;
	int		cnt ;

	cnt = 0 ;
	fseek( fp, 0L, 0) ;

	while ( (status = read_cap_line( fp, Driver)))
	{

		if (status < 0)
		{
				fprintf( stderr," Error in reading digitcap file.\n") ;
				fprintf(stderr, "Contact your GRASS system administrator\n") ;
				fclose(fp) ;
				exit(-1) ;
		}

		if ( ! strcmp(selected_name, Driver->name))
			return(++cnt) ;
		++cnt ;

	}

	if ( *selected_name  ==  NULL)
		return(cnt) ;
	else
		return(0) ;

}			 /*  get_driver_name()  */

