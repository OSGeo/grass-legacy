
/*
*    get_driver_name() - verifies that a name is a valid digitizer name and
*    also copies information about that matched digitizer.
*    -  returns the number of digitizers defined in the digitcap file until
*          it finds a matching digitizer.  if a null is passed as the name
*          it would return a total count of digitizers.
*    -  returns 0 if there was nothing in the digitcap file.  
*
*    read_cap_line() - read a string (skips comment lines) from digitcap file
*    and parse it.
*/

#include    <stdio.h>
#include    "bin_digit.h"

#define        BUFFERSIZE    256
#define        COMMENT_CHAR    '#'

read_cap_line( fp, Driver)
    FILE    *fp ;
    struct  driver_desc  *Driver ;
{
    int        num_read ;
    char    buf[BUFFERSIZE];
    char        cbuf[10];

    while ( fgets( buf, BUFFERSIZE-1, fp ) != NULL)
    {


    /*  skip commented lines  */
    if (1 != sscanf (buf, "%1s", cbuf))
        continue;

    if ( buf[0] == COMMENT_CHAR)
        continue ;


    *Driver->name = NULL ;  *Driver->device = NULL ;
    *Driver->dig_filename = NULL ;  *Driver->dig_desc = NULL ;

    /*  
    *    the notation '%[^:]' in the sscanf means copy all characters
    *    into the the assigned memory (name,..) until a ':' is found.
    */
    num_read = sscanf( buf, "%[^:]:%[^:]:%[^:]:%[^:]", Driver->name,
    Driver->device, Driver->dig_filename, Driver->dig_desc) ;

    if ( ! num_read)
    {
        continue ;
    }

    /* check to make sure that values were copyed into these fields,
    *  there doesn't have to be a digitizer description
    */
    if ( *Driver->name  &&  *Driver->device  &&  *Driver->dig_filename)
    {
        return(1) ;
    }


    return(-1) ;
    }


    return(0) ;

}


get_driver_name( fp, selected_name, Driver)
    FILE  *fp ;
    char  *selected_name ;
    struct  driver_desc  *Driver ;
{

    int        status ;
    int        cnt ;

    cnt = 0 ;
    fseek( fp, 0L, 0) ;

    while ( (status = read_cap_line( fp, Driver)) > 0)
    {


        if ( ! strcmp(selected_name, Driver->name))
            return(++cnt) ;
        ++cnt ;

    }
    if (status < 0)
	return (status);

    if ( *selected_name  ==  NULL)
        return(cnt) ;
    else
        return(0) ;

}             /*  get_driver_name()  */

