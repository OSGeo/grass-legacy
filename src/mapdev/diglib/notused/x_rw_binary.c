/*
**  Modified by Dave Gerdes  9/1988    portable
**  US Army Construction Engineering Research Lab
**
**  Modified by Dave Gerdes 1/91   more portable
*/
#include "digit.h"
#include "dig_head.h"
#include <stdio.h>


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
dig_x_write_head_binary(digit, dhead)
	FILE *digit ;
	struct dig_head *dhead;
{
    unsigned char buf[1024];
    long   ltmp;
    double dtmp;
    long offset_offset = 0;
    long data_offset = 0;
    struct dig_head *Defhd;
    unsigned char port_flag;
    int i;

    fseek(digit, 0L, 0) ;

    Defhd = dig__get_default_out_head ();


/***************************  4.0 Stuff ***************************************/

#ifdef FOO
    if (0 >= dig__fwrite_port_C (Dig_Code, 3, digit)) return -1;
    if (0 >= dig__fwrite_port_C (Vers_Code, 2, digit)) return -1;
    if (0 >= dig__fwrite_port_C (Back_Vers_Code, 2, digit)) return -1;

    port_flag = Defhd->portable;
    if (0 >= dig__fwrite_port_C (&port_flag,  1, digit)) return -1;

    
    /* if not portable, then dont write out portable data info */
    if (port_flag)
    {
    if (0 >= dig__fwrite_port_C (Defhd->dbl_cnvrt,  DBL_SIZ, digit)) return -1;
    if (0 >= dig__fwrite_port_C (Defhd->flt_cnvrt,  FLT_SIZ, digit)) return -1;
    if (0 >= dig__fwrite_port_C (Defhd->lng_cnvrt,  LNG_SIZ, digit)) return -1;
    if (0 >= dig__fwrite_port_C (Defhd->shrt_cnvrt,SHRT_SIZ, digit)) return -1;
    }

    offset_offset = ftell (digit);
    if (0 >= dig__fwrite_port_L (&data_offset,             1, digit)) return -1;
#endif

/******************************  3.0 and USER stuff ***************************/
    if (0 >= dig__fwrite_port_C (dhead->organization, sizeof(dhead->organization), digit) ) return -1;
    if (0>= dig__fwrite_port_C(dhead->date,       sizeof(dhead->date       ),digit)) return -1;
    if (0>= dig__fwrite_port_C(dhead->your_name,  sizeof(dhead->your_name  ),digit)) return -1;
    if (0>= dig__fwrite_port_C(dhead->map_name,   sizeof(dhead->map_name   ),digit)) return -1;
    if (0>= dig__fwrite_port_C(dhead->source_date,sizeof(dhead->source_date),digit)) return -1;

     /************ Stuff in 4.0 DATA at end of old OTHER INFO line *******/

    strncpy (buf, dhead->line_3, NEW_LINE_3_SIZE);
    buf[NEW_LINE_3_SIZE-1] = 0;
    if (0>= dig__fwrite_port_C (buf ,NEW_LINE_3_SIZE, digit)) return -1;

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
    if (0>= dig__fwrite_port_C (buf ,VERS_4_DATA_SIZE, digit)) return -1;

     /******************************* and continue ***********************/

    if (0>= dig__fwrite_port_L (&dhead->orig_scale, 1, digit)) return -1;
    if (0>= dig__fwrite_port_I (&dhead->plani_zone, 1, digit)) return -1;

    if (0>= dig__fwrite_port_D (&dhead->W,          1, digit)) return -1;
    if (0>= dig__fwrite_port_D (&dhead->E,          1, digit)) return -1;
    if (0>= dig__fwrite_port_D (&dhead->S,          1, digit)) return -1;
    if (0>= dig__fwrite_port_D (&dhead->N,          1, digit)) return -1;
    if (0>= dig__fwrite_port_D (&dhead->map_thresh, 1, digit)) return -1;
/******************************************************************************/

    /* fix up offset info */

#ifdef FOO
    data_offset = ftell (digit);
    fseek (digit, offset_offset, 0);
    if (0>= dig__fwrite_port_L (&data_offset,        1, digit)) return -1;
    fseek (digit, data_offset, 0);
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

#ifndef FOO
dig_x_read_head_binary (digit, dhead)
	FILE *digit ;
	struct dig_head *dhead;
{
    return dig__old_read_head_binary (digit, dhead);
}

#else	
/* THIS CODE NOT CURRENTLY USED  (Probably never will be) */
dig_x_read_head_binary (digit, dhead)
	FILE *digit ;
	struct dig_head *dhead;
{
    long ltmp;
    unsigned char code[3], vers[2], back_vers[2];
    int Old = 0;
    char port_flag;
    struct dig_head *fhead;
    long data_offset;
    register int i;

    fhead = dig__get_head (digit);
    dig__set_cur_in_head (fhead);

    fseek (digit, 0L, 0) ;


    if (0 >= dig__fread_port_C (code, 3, digit)) return -1;
	for (i = 0 ; i < 3 ; i++)
	    if (Dig_Code[i] != code[i])
		Old = 1;
    if (Old)
	return dig__old_read_head_binary (digit, dhead);
	

    if (0 >= dig__fread_port_C (vers, 2, digit)) return -1;
    if (0 >= dig__fread_port_C (back_vers, 2, digit)) return -1;

    if (vers[0] != VERSION_MAJOR)
    {
	char buf[100];

	if (back_vers[0] > VERSION_MAJOR || 
	    back_vers[0] == VERSION_MAJOR  & back_vers[1] > VERSION_MINOR)
	{
	  sprintf (buf, "Can't read dig file version %d.%d", code[0], code[1]);
	  G_fatal_error (buf);
	}
    }
    dhead->Version_Major = vers[0];
    dhead->Version_Minor = vers[1];
    dhead->Back_Major = back_vers[0];
    dhead->Back_Minor = back_vers[1];

    if (0 >= dig__fread_port_C (&port_flag,  1, digit)) return -1;
    dhead->portable = port_flag;

    if (dhead->portable)
    {
    if (0 >= dig__fread_port_C (dhead->dbl_cnvrt,  DBL_SIZ, digit)) return -1;
    if (0 >= dig__fread_port_C (dhead->flt_cnvrt,  FLT_SIZ, digit)) return -1;
    if (0 >= dig__fread_port_C (dhead->lng_cnvrt,  LNG_SIZ, digit)) return -1;
    if (0 >= dig__fread_port_C (dhead->shrt_cnvrt,SHRT_SIZ, digit)) return -1;

	/* set up quick flags */
	dhead->dbl_quick=1;
	for (i = 0 ; i < DBL_SIZ ; i++)
	    if (dhead->dbl_cnvrt[i] != i)
		dhead->dbl_quick=0;

	dhead->flt_quick=1;
	for (i = 0 ; i < FLT_SIZ ; i++)
	    if (dhead->flt_cnvrt[i] != i)
		dhead->flt_quick=0;

	dhead->lng_quick=1;
	for (i = 0 ; i < LNG_SIZ ; i++)
	    if (dhead->lng_cnvrt[i] != i)
		dhead->lng_quick=0;

	dhead->shrt_quick=1;
	for (i = 0 ; i < SHRT_SIZ ; i++)
	    if (dhead->shrt_cnvrt[i] != i)
		dhead->shrt_quick=0;

    }
    /* Portable info now installed (if dhead == fhead) */

    if (0 >= dig__fread_port_L (&data_offset, 1, digit)) return -1;


    if (0 >= dig__fread_port_C (dhead->organization, sizeof(dhead->organization), digit)) return -1;
    if (0 >= dig__fread_port_C(dhead->date,       sizeof(dhead->date       ),digit)) return -1;
    if (0 >= dig__fread_port_C(dhead->your_name,  sizeof(dhead->your_name  ),digit)) return -1;
    if (0 >= dig__fread_port_C(dhead->map_name,   sizeof(dhead->map_name   ),digit)) return -1;
    if (0 >= dig__fread_port_C(dhead->source_date,sizeof(dhead->source_date),digit)) return -1;
    if (0 >= dig__fread_port_C(dhead->line_3,     sizeof(dhead->line_3     ),digit)) return -1;

    if (0 >= dig__fread_port_L (&dhead->orig_scale, 1, digit)) return -1;
    if (0 >= dig__fread_port_I (&dhead->plani_zone, 1, digit)) return -1;

    if (0 >= dig__fread_port_D (&dhead->W,          1, digit)) return -1;
    if (0 >= dig__fread_port_D (&dhead->E,          1, digit)) return -1;
    if (0 >= dig__fread_port_D (&dhead->S,          1, digit)) return -1;
    if (0 >= dig__fread_port_D (&dhead->N,          1, digit)) return -1;
    if (0 >= dig__fread_port_D (&dhead->map_thresh, 1, digit)) return -1;

    /* 
    **  for forward compatibilty, any future data after this point will be
    **  skipped with the following fseek()*
    */
    fseek (digit, data_offset, 0);	/* seek to beginning of data */

    return(0) ;
}
#endif


/*
**  This is similar to above, in that fhead and dhead can be different
**   dhead is the one that gets read into, except that fhead gets
**   initialized w/ default settings in case this is the first time
**   through.
*/
dig__old_read_head_binary (digit, dhead)
	FILE *digit ;
	struct dig_head *dhead;
{ 
    unsigned char buf[1024];
    long ltmp;
    struct dig_head *Defhd, *fhead;

    Defhd = dig__get_default_in_head ();	/* get default setup */
    fhead = dig__get_head (digit);

	/* set the file head to default */
    dig_struct_copy (Defhd, fhead, sizeof (struct dig_head));  /*from, to, n*/

    dig__set_cur_in_head (fhead);	/* and make that the current */

#ifdef DEBUG
/*DEBUG*/ fprintf (stderr, "READ_HEAD default: ");
/*DEBUG*/ debug_dump_port_info (fhead);

/*DEBUG*/ fprintf (stderr, "Def PORT head: ");
/*DEBUG*/ Defhd = dig__get_default_port_head ();	/* get default setup */
/*DEBUG*/ debug_dump_port_info (Defhd);
#endif

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
    if (buf[0] != '%' && buf[1] != '%') /* Version3.0 */
    {

/*DEBUG*/ debugf ("READ_HEAD:   Version 3.0 found\n");
	/* do nothing, cuz already set up correctly */

	/* mark file as old */
	fhead->Back_Major = fhead->Version_Major = 3;
	fhead->Back_Minor = fhead->Version_Minor = 0;

	dhead->Back_Major = dhead->Version_Major = 3;
	dhead->Back_Minor = dhead->Version_Minor = 0;

    }
    else	/* Version 4.0 */
    {
/*DEBUG*/ debugf ("READ_HEAD:   Version 4.0 found: ");

	/* determine if can read this version */
	if (buf[2] != VERSION_MAJOR)
	{
	    char tbuff[200];

	    if (buf[4] > VERSION_MAJOR || 
	    buf[4] == VERSION_MAJOR  & buf[5] > VERSION_MINOR)
	    {
	      sprintf (tbuff, "Don't know how to read VECT file version %d.%02d", buf[2], buf[3]);
	      G_fatal_error (tbuff);
	    }
	}

	/* determine if in portable format or not */
	if (buf[6] == 1 && (~buf[6]&0xff) == buf[7])	/* portable ? */
	{
/*DEBUG*/ debugf ("FILE is PORTABLE\n");
	    Defhd = dig__get_default_port_head ();
	    dig_struct_copy (Defhd, fhead, sizeof(struct dig_head));
	}
/*DEBUG*/ else debugf ("FILE is NOT portable  buf6 %02x  buf7 %02x\n", buf[6], buf[7]);
	/* else already set up */

	dhead->Version_Major = buf[2];
	dhead->Version_Minor = buf[3];
	dhead->Back_Major =    buf[4];
	dhead->Back_Minor =    buf[5];
    }

/*DEBUG*/     debugf ("read_head:  portable %d,  Version %d.%d\n", buf[6], dhead->Version_Major, dhead->Version_Minor);

    if (0 >= dig__fread_port_L (&dhead->orig_scale, 1, digit)) return -1;
    if (0 >= dig__fread_port_I (&dhead->plani_zone, 1, digit)) return -1;

    if (0 >= dig__fread_port_D (&dhead->W,          1, digit)) return -1;
    if (0 >= dig__fread_port_D (&dhead->E,          1, digit)) return -1;
    if (0 >= dig__fread_port_D (&dhead->S,          1, digit)) return -1;
    if (0 >= dig__fread_port_D (&dhead->N,          1, digit)) return -1;
    if (0 >= dig__fread_port_D (&dhead->map_thresh, 1, digit)) return -1;

    return(0) ;
}

/*DEBUG*/
#ifdef DEBUG
debug_dump_port_info (fhead)
    struct dig_head *fhead;
{
    int i;
    fprintf (stderr, "Portable: %d  Long: ", fhead->portable);
    for (i = 0 ; i < LNG_SIZ ; i++)
	fprintf (stderr, "%02d",fhead->lng_cnvrt[i]);
    fprintf (stderr, "\n");
}
#endif
