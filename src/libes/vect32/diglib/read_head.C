
/*
**  Modified by Dave Gerdes  9/1988    portable
**  US Army Construction Engineering Research Lab
**
**  Modified by Dave Gerdes 1/91   more portable
*/
#include "gis.h"
#include "digit.h"
#include "dig_head.h"
#include <stdio.h>

static unsigned char Dig_Code[3] = { 5, 22, 62 };
static unsigned char Vers_Code[2] = { VERSION_MAJOR, VERSION_MINOR };
static unsigned char Back_Vers_Code[2] = { VERSION_MAJOR, VERSION_MINOR };

main (argc, argv)
    char *argv[];
{
    FILE *fp;

    G_gisinit ("read_head");
    if (argc != 2)
	fprintf (stderr, "Usage: read_head file\n"), exit (1);

    fp = G_fopen_vector_old (argv[1], G_mapset());
    if (NULL == fp)
	fprintf (stderr, "cant open file '%s'\n", argv[1]), exit (2);

    read_head (fp);
}

read_head(digit)
	FILE *digit ;
{
    long ltmp;
    unsigned char code[3], vers[2], back_vers[2];
    int Old = 0;
    char port_flag;
    struct dig_head *fhead;
    long data_offset;
    register int i;

    fseek (digit, 0L, 0) ;


    if (0 >= fread (code, 1, 3, digit)) return -1;

    printf ("Code: ");
    for (i = 0 ; i < 3 ; i++)
    {
	printf (" %d", code[i]);
	if (Dig_Code[i] != code[i])
	    Old = 1;
    }
    printf ("\n");

    if (Old)
	return old_read_head_binary (digit);
	

    if (0 >= fread (vers, 1, 2, digit)) return -1;
    if (0 >= fread (back_vers, 1, 2, digit)) return -1;

    printf ( "Version:  %d.%d\n", vers[0], vers[1]);

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

    if (0 >= fread (&port_flag,  1, 1, digit)) return -1;
    printf ( "File is %sin portable format.\n", port_flag ? "" : "NOT ");

    if (port_flag)
    {
    if (0 >= fread (fhead->dbl_cnvrt, 1,  DBL_SIZ, digit)) return -1;
    if (0 >= fread (fhead->flt_cnvrt, 1,  FLT_SIZ, digit)) return -1;
    if (0 >= fread (fhead->lng_cnvrt, 1,  LNG_SIZ, digit)) return -1;
    if (0 >= fread (fhead->shrt_cnvrt,1, SHRT_SIZ, digit)) return -1;

	/* set up quick flags */
	printf ( "Double format: ");
	fhead->dbl_quick=1;
	for (i = 0 ; i < DBL_SIZ ; i++)
	{
	    printf ( "%d ", fhead->dbl_cnvrt[i]);
	    if (fhead->dbl_cnvrt[i] != i)
		fhead->dbl_quick=0;
	}
	printf ("  %s\n", fhead->dbl_quick ? "QUICK" : "NOT quick");
	printf ( "Float format : ");

	fhead->flt_quick=1;
	for (i = 0 ; i < FLT_SIZ ; i++)
	{
	    printf ( "%d ", fhead->flt_cnvrt[i]);
	    if (fhead->flt_cnvrt[i] != i)
		fhead->flt_quick=0;
	}

	printf ("  %s\n", fhead->flt_quick ? "QUICK" : "NOT quick");
	printf ( "Long format  : ");

	fhead->lng_quick=1;
	for (i = 0 ; i < LNG_SIZ ; i++)
	{
	    printf ( "%d ", fhead->lng_cnvrt[i]);
	    if (fhead->lng_cnvrt[i] != i)
		fhead->lng_quick=0;
	}

	printf ("  %s\n", fhead->lng_quick ? "QUICK" : "NOT quick");
	printf ( "Short format : ");

	fhead->shrt_quick=1;
	for (i = 0 ; i < SHRT_SIZ ; i++)
	{
	    printf ( "%d ", fhead->shrt_cnvrt[i]);
	    if (fhead->shrt_cnvrt[i] != i)
		fhead->shrt_quick=0;
	}

	printf ("  %s\n", fhead->shrt_quick ? "QUICK" : "NOT quick");
    }
    /* Portable info now installed (if fhead == fhead) */

    if (0 >= fread (&data_offset, sizeof (long), 1, digit)) return -1;
    printf ( "Data_offset = %ld\n", data_offset);


    if (0 >= fread (fhead->organization, 1, sizeof(fhead->organization), digit)) return -1;
    if (0 >= fread(fhead->date,       1, sizeof(fhead->date       ),digit)) return -1;
    if (0 >= fread(fhead->your_name,  1, sizeof(fhead->your_name  ),digit)) return -1;
    if (0 >= fread(fhead->map_name,   1, sizeof(fhead->map_name   ),digit)) return -1;
    if (0 >= fread(fhead->source_date,1, sizeof(fhead->source_date),digit)) return -1;
    if (0 >= fread(fhead->line_3,     1, sizeof(fhead->line_3     ),digit)) return -1;

    /*
    if (0 >= dig__fread_port_L (&fhead->orig_scale, 1, digit)) return -1;
    if (0 >= dig__fread_port_I (&fhead->plani_zone, 1, digit)) return -1;

    if (0 >= dig__fread_port_D (&fhead->W,          1, digit)) return -1;
    if (0 >= dig__fread_port_D (&fhead->E,          1, digit)) return -1;
    if (0 >= dig__fread_port_D (&fhead->S,          1, digit)) return -1;
    if (0 >= dig__fread_port_D (&fhead->N,          1, digit)) return -1;
    if (0 >= dig__fread_port_D (&fhead->map_thresh, 1, digit)) return -1;

    fseek (digit, data_offset, 0);
    */

    return(0) ;
}


/*
**  This is similar to above, in that fhead and fhead can be different
**   fhead is the one that gets read into, except that fhead gets
**   initialized w/ default settings in case this is the first time
**   through.
*/
old_read_head_binary (digit, dhead)
	FILE *digit ;
	struct dig_head *dhead;
{ 
#ifndef FOO
    fprintf (stderr, "IN OLD_read_head\n");
    exit (1);
#else
    long ltmp;
    struct dig_head *Defhd, *fhead;

    Defhd = dig__get_default_in_head ();	/* get default setup */
    fhead = dig__get_head (digit);

    dig_struct_copy (Defhd, fhead, sizeof (struct dig_head));	/* set the file head to default */

    dig__set_cur_in_head (fhead);	/* and make that the current */

    fseek (digit, 0L, 0) ;		/* Back to the beginning */

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

    /* mark file as old */
    fhead->Back_Major = fhead->Version_Major = 3;
    fhead->Back_Minor = fhead->Version_Minor = 0;

    return(0) ;
#endif
}
