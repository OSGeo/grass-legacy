/*
**  Modified by Dave Gerdes  9/1988    portable
**  US Army Construction Engineering Research Lab
**
**  Modified by Dave Gerdes 1/91   more portable
*/
#include <stdio.h>
#include <string.h>
#include "Vect.h"
#include "gis.h"


/* routines to read and write DIGIT header */
static unsigned char Dig_Code[3] = { 5, 22, 62 };
static unsigned char Vers_Code[2] = { VERSION_MAJOR, VERSION_MINOR };
static unsigned char Back_Vers_Code[2] = { VERSION_MAJOR, VERSION_MINOR };

/*
**  THIS HEADER IS THE SAME SIZE AS 3.0 
*/

/*
**  Write 4.0 Dig head to file.
**  Note all files are written with the same portability options
**  These are available in the dig_head struct returned by 
**	dig__get_default_out_head()
**
*/
int Vect_x_write_head_binary (
    struct Map_info *Map,
    struct dig_head *dhead)
{
    unsigned char buf[1024];
    struct dig_head *Defhd;
    int i;

    fseek(Map->dig_fp, 0L, 0) ;

    Defhd = Vect__get_default_out_head ();


/******************************  3.0 and USER stuff ***************************/
    if (0 >= dig__fwrite_port_C (dhead->organization, sizeof(dhead->organization), Map->dig_fp) ) return -1;
    if (0>= dig__fwrite_port_C(dhead->date,       sizeof(dhead->date       ),Map->dig_fp)) return -1;
    if (0>= dig__fwrite_port_C(dhead->your_name,  sizeof(dhead->your_name  ),Map->dig_fp)) return -1;
    if (0>= dig__fwrite_port_C(dhead->map_name,   sizeof(dhead->map_name   ),Map->dig_fp)) return -1;
    if (0>= dig__fwrite_port_C(dhead->source_date,sizeof(dhead->source_date),Map->dig_fp)) return -1;

     /************ Stuff in 4.0 DATA at end of old OTHER INFO line *******/

    strncpy (buf, dhead->line_3, NEW_LINE_3_SIZE);
    buf[NEW_LINE_3_SIZE-1] = 0;
    if (0>= dig__fwrite_port_C (buf ,NEW_LINE_3_SIZE, Map->dig_fp)) return -1;

    for (i = 0 ; i < VERS_4_DATA_SIZE ; i++)
	buf[i] = 0;

    buf[0] = '%';
    buf[1] = '%';		/* Vers 4 code */
    buf[2] = VERSION_MAJOR;
    buf[3] = VERSION_MINOR;
    buf[4] = EARLIEST_MAJOR;
    buf[5] = EARLIEST_MINOR;
    buf[6] = Defhd->portable;
    buf[7] =  (unsigned char) ~(Defhd->portable);
    if (0>= dig__fwrite_port_C (buf ,VERS_4_DATA_SIZE, Map->dig_fp)) return -1;

     /******************************* and continue ***********************/

    if (0>= dig__fwrite_port_L (&dhead->orig_scale, 1, Map->dig_fp)) return -1;
    if (0>= dig__fwrite_port_I (&dhead->plani_zone, 1, Map->dig_fp)) return -1;

    if (0>= dig__fwrite_port_D (&dhead->W,          1, Map->dig_fp)) return -1;
    if (0>= dig__fwrite_port_D (&dhead->E,          1, Map->dig_fp)) return -1;
    if (0>= dig__fwrite_port_D (&dhead->S,          1, Map->dig_fp)) return -1;
    if (0>= dig__fwrite_port_D (&dhead->N,          1, Map->dig_fp)) return -1;
    if (0>= dig__fwrite_port_D (&dhead->map_thresh, 1, Map->dig_fp)) return -1;
/******************************************************************************/

    /* fix up offset info */

#ifdef FOO
    data_offset = ftell (Map->dig_fp);
    fseek (Map->dig_fp, offset_offset, 0);
    if (0>= dig__fwrite_port_L (&data_offset,        1, Map->dig_fp)) return -1;
    fseek (Map->dig_fp, data_offset, 0);
#endif

    return(0) ;
}

/*
** note there are potentially 2 head structs that I need to fill in here
**
**  This is cuz of slop left over from the old design, where users
**   provided the head struct.  Future should replace this by the
**   system maintaining control over it.

**
**  I will assume that the only time you want to fill in the REAL 
**   struct associated with Head_array is if dhead and fhead are the same.
**   Otherwize, just fill the the struct the user provided (which of course
**    assumes that the first time this is called for a file will be from
**    dig_init or something comparable, so that the portable stuff gets 
**    initted correctly.)
**  
*/

/*
**   dhead is the one that gets read into, except that fhead gets
**   initialized w/ default settings in case this is the first time
**   through.
*/

int Vect_x_read_head_binary (
    struct Map_info *Map,
    struct dig_head *dhead)
{
    unsigned char buf[1024];
    struct dig_head *Defhd, *fhead;
    FILE *digit;

    Defhd = Vect__get_default_in_head ();	/* get default setup */
    fhead = &(Map->head);

	/* set the file head to default */
    Vect__copy_portable_info (Defhd, fhead);

    dig__set_cur_in_head (fhead);	/* and make that the current */


    digit = Map->dig_fp;

    fseek (digit, 0L, 0) ;		/* Back to the beginning */

    if (0 >= dig__fread_port_C (dhead->organization, sizeof(dhead->organization), digit)) return -1;
	dhead->organization[DIG_ORGAN_LEN-1] = 0;


    if (0 >= dig__fread_port_C(dhead->date,       sizeof(dhead->date       ),digit)) return -1;
	dhead->date[DIG_DATE_LEN-1] = 0;
    if (0 >= dig__fread_port_C(dhead->your_name,  sizeof(dhead->your_name  ),digit)) return -1;
	dhead->your_name[DIG_YOUR_NAME_LEN-1] = 0;
    if (0 >= dig__fread_port_C(dhead->map_name,   sizeof(dhead->map_name   ),digit)) return -1;
	dhead->map_name[DIG_MAP_NAME_LEN-1] = 0;
    if (0 >= dig__fread_port_C(dhead->source_date,sizeof(dhead->source_date),digit)) return -1;
	dhead->source_date[DIG_SOURCE_DATE_LEN-1] = 0;

    if (0 >= dig__fread_port_C(dhead->line_3,     NEW_LINE_3_SIZE, digit)) return -1;
    dhead->line_3[NEW_LINE_3_SIZE-1] = 0;	/* terminate string */
    if (0 >= dig__fread_port_C(buf, VERS_4_DATA_SIZE, digit)) return -1;

    if (buf[0] != '%' || buf[1] != '%') /* Version3.0 */
    {
	/* do nothing, cuz already set up correctly */

	/* mark file as old */
	fhead->Back_Major = fhead->Version_Major = 3;
	fhead->Back_Minor = fhead->Version_Minor = 0;

	dhead->Back_Major = dhead->Version_Major = 3;
	dhead->Back_Minor = dhead->Version_Minor = 0;
    }
    else	/* Version 4.0 */
    {

	/* determine if can read this version */
	if (buf[2] != VERSION_MAJOR)
	{
	    char tbuff[200];

	    if (buf[4] > VERSION_MAJOR || 
	    (buf[4] == VERSION_MAJOR && buf[5] > VERSION_MINOR))
	    {
	      sprintf (tbuff, "Don't know how to read VECT file version %d.%02d", (int) buf[2], (int) buf[3]);
	      G_fatal_error (tbuff);
	    }
	}

	/* determine if in portable format or not */
        if (buf[6] == 1 && (~buf[6]&0xff) == buf[7])    /* portable ? */
	{
	    Defhd = Vect__get_default_port_head ();
	    Vect__copy_portable_info (Defhd, fhead);
	}
	/* else already set up */

	dhead->Version_Major = buf[2];
	dhead->Version_Minor = buf[3];
	dhead->Back_Major =    buf[4];
	dhead->Back_Minor =    buf[5];
    }


    if (0 >= dig__fread_port_L (&(dhead->orig_scale), 1, digit)) return -1;
    if (0 >= dig__fread_port_I (&(dhead->plani_zone), 1, digit)) return -1;

    if (0 >= dig__fread_port_D (&(dhead->W),          1, digit)) return -1;
    if (0 >= dig__fread_port_D (&(dhead->E),          1, digit)) return -1;
    if (0 >= dig__fread_port_D (&(dhead->S),          1, digit)) return -1;
    if (0 >= dig__fread_port_D (&(dhead->N),          1, digit)) return -1;
    if (0 >= dig__fread_port_D (&(dhead->map_thresh), 1, digit)) return -1;

    return(0) ;
}
