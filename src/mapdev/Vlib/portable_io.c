#include "V_.h"

/*
**
**  Possible scenarios

	Pre-4.0: 	In only
			Can potentially be anything
			Native,   Portable, or Native to another machine
			Default will have to be compiled in at GISGEN time,
			(Native will be the distributed default) and can
			be overridden with ENV variable OR  hopefully
			there will be a program to help convert files.

	4.0:		Out or In
			Can write out either Portable (DEFAULT) or Native 
			Will read in using control structures in dig file
			if they are enabled, else will read in native mode.
**
*/

/* for dig_globs.h */
#define CONTROL		/* for Memory_io stuff */
#include "digit.h"


static int Portable_In;
static int Portable_Out;


#define FIRST  { if (First_Time) Vect_init (); }
/*
#define FIRST  { if (First_Time) G_fatal_error ("PIO: Programmer did not call dig_init ()"); }
*/

static int First_Time = 1;


    /* dig__Init_V () */
Vect_init ()	/* init vector system */
{
    if (First_Time)
    {
	Init_Portable_structs ();
	First_Time = 0;
    }
}

/*
** 
**  Portable_Out :   Determines for ALL output files if they should be written
**		     in 4.0 portable or 4.0 native mode.
** 
**  Portable_In  :   If a file is found that is 3.0 format, then this flag
**		     specifies how it should be read in.
*/

static
Init_Portable_structs ()
{
    /*  Defaults */
    Portable_Out = 1;
#ifdef PORTABLE_3	/* GISGEN can define this to override */
    Portable_In = 1;
#else
    Portable_In = 0;
#endif

#ifdef NO_PORTABLE
    Portable_Out = 0;	/* GISGEN can define this to create non-portable */
			/*  4.0 files */
#endif

    /* DIG */
    dig__Init_portable_code (Portable_In || Portable_Out);
}




/*****************************************************************************/
/***************************** rw_binary.c ***********************************/
/*****************************************************************************/

/* routines to read and write DIGIT header */

/* programmers should no longer call this.  Close will take care
** of it.
*/
Vect__write_head_binary (Map, head)
    struct Map_info *Map; 
    struct dig_head *head;
{
    struct dig_head *hp;

    FIRST;

    if (head == NULL)
	hp = &(Map->head);
    else
	hp = head;

    return Vect_x_write_head_binary (Map, hp);
}

Vect__read_head_binary (Map, head)
    struct Map_info *Map;
    struct dig_head *head;
{
    struct dig_head *hp;

    /*
    if (NULL == dig__get_head (digit))
	G_fatal_error ("Programmer did not call dig_[P_]init()");
    */

    FIRST;

    if (head == NULL)
	hp = &(Map->head);
    else
	hp = head;

    return Vect_x_read_head_binary (Map, hp);
}






/*****************************************************************************/
/***************************** point_io.c ************************************/
/*****************************************************************************/

/*  Read_line ()
**     read line info from digit file into line_points structure 
**
**  Returns     (int)  type  or
**	 -2  End of file
**	-1 Out of memory

**  if Line_In_Memory is TRUE, then offset is taken off of Mem_Line_Ptr
**  in memory.
*/


int
Vect__Read_line (Map, p, offset)
    struct Map_info *Map; 
    struct line_pnts *p;
    long offset;
{
    FIRST;
    return Vect_x__Read_line (Map, p, offset);
}

/* write line info to DIGIT file */
/*  returns offset into file */
long
Vect__Write_line (Map, type, points) 
    struct Map_info *Map; 
    char type;
    struct line_pnts *points;
{
    FIRST;
    return Vect_x__Write_line (Map, type, points);
}

/* write line info to DIGIT file */
/*  at the given offset */
/*  obviously the number of points must NOT have changed */
/*  from when line was read in */

Vect__Rewrite_line (Map, offset, type, points) 
    struct Map_info *Map; 
    long offset;
    char type;
    struct line_pnts *points;
{
    FIRST;
    return Vect_x__Rewrite_line (Map, offset, type, points);
}


/*****************************************************************************/
/**************************** struct_io.c ************************************/
/*****************************************************************************/

dig_Rd_P_node (map, ptr, fp)
    struct Map_info *map;
    FILE *fp;
    struct P_node *ptr;
{
    FIRST;
    return dig_x_Rd_P_node (map, ptr, fp);
}

dig_Wr_P_node (map, ptr, fp)
    struct Map_info *map;
    FILE *fp;
    struct P_node *ptr;
{
    FIRST;
    return dig_x_Wr_P_node (map, ptr, fp);
}

dig_Rd_P_line (map, ptr, fp)
    struct Map_info *map;
    FILE *fp;
    struct P_line *ptr;
{
    FIRST;
    return dig_x_Rd_P_line (map, ptr, fp);
}

dig_Wr_P_line (map, ptr, fp)
    struct Map_info *map;
    FILE *fp;
    struct P_line *ptr;
{
    FIRST;
    return dig_x_Wr_P_line (map, ptr, fp);
}

dig_Rd_P_area (map, ptr, fp)
    struct Map_info *map;
    FILE *fp;
    struct P_area *ptr;
{
    FIRST;
    return dig_x_Rd_P_area (map, ptr, fp);
}

dig_Wr_P_area (map, ptr, fp)
    struct Map_info *map;
    FILE *fp;
    struct P_area *ptr;
{
    FIRST;
    return dig_x_Wr_P_area (map, ptr, fp);
}

/* island stuff */
dig_Rd_P_isle (map, ptr, fp)
    struct Map_info *map;
    FILE *fp;
    struct P_isle *ptr;
{
    FIRST;
    return dig_x_Rd_P_isle (map, ptr, fp);
}

dig_Wr_P_isle (map, ptr, fp)
    struct Map_info *map;
    FILE *fp;
    struct P_isle *ptr;
{
    FIRST;
    return dig_x_Wr_P_isle (map, ptr, fp);
}

dig_Rd_P_att (map, ptr, fp)
    struct Map_info *map;
    FILE *fp;
    struct P_att *ptr;
{
    FIRST;
    return dig_x_Rd_P_att (map, ptr, fp);
}

dig_Wr_P_att (map, ptr, fp)
    struct Map_info *map;
    FILE *fp;
    struct P_att *ptr;
{
    FIRST;
    return dig_x_Wr_P_att (map, ptr, fp);
}

dig_Rd_Plus_head (map, ptr, fp)
    struct Map_info *map;
    struct Plus_head *ptr;
    FILE *fp;
{
/*
    if (NULL == dig__get_head (map->digit))
	G_fatal_error ("Programmer did not call dig_P_init()");
*/
    FIRST;
    return dig_x_Rd_Plus_head (map, ptr, fp);
}

dig_Wr_Plus_head (map, ptr, fp)
    struct Map_info *map;
    struct Plus_head *ptr;
    FILE *fp;
{
    FIRST;
    return dig_x_Wr_Plus_head (map, ptr, fp);
}



/****************************************************************************/
/********************** Init default head structs ***************************/
/****************************************************************************/

static struct dig_head Def_In_Head;
static struct dig_head Def_Port_Head;
static struct dig_head Def_Out_Head;


static
Vect__init_default_heads (do_port)
{
    struct dig_head *head;

    /* old 4.0
    do_port = do_port || Portable_In || Portable_Out;
    */
    do_port = Portable_In || Portable_Out;
    if (do_port)
	dig__Init_portable_code (1);

    /* OUT */

    head = &Def_Out_Head;

    strcpy (head->organization, "Default Out Head");
/*  head->organization[0] = 0; */
    head->date[0] = 0;
    head->your_name[0] = 0;
    head->map_name[0] = 0;
    head->source_date[0] = 0;
    head->line_3[0] = 0;
    head->orig_scale = 0;
    head->plani_zone = 0;
    head->W = 0;
    head->E = 0;
    head->S = 0;
    head->N = 0;
    head->digit_thresh = 0.;
    head->map_thresh = 0.;

    /******/
    head->Version_Major = VERSION_MAJOR;
    head->Version_Minor = VERSION_MINOR;
    head->Back_Major = EARLIEST_MAJOR;
    head->Back_Minor = EARLIEST_MINOR;

    if (Portable_Out)
    {
	head->portable = 1;
	dig__fill_head_portable (head);
    }
    else
	head->portable = 0;

#ifdef DEBUG
    /*DEBUG*/ fprintf (stderr, "init_def_heads: Portable_Out = %d\n", Portable_Out);
    printf ( "Dumpflags: \n");
    /*DEBUG*/ dumpflags (head);
#endif

    dig__set_cur_out_head (head);	/* set Default OUTPUT header */


    /* IN */
    head = &Def_In_Head;

    strcpy (head->organization, "Default In Head");
/*  head->organization[0] = "Default In Head"; */
    head->date[0] = 0;
    head->your_name[0] = 0;
    head->map_name[0] = 0;
    head->source_date[0] = 0;
    head->line_3[0] = 0;
    head->orig_scale = 0;
    head->plani_zone = 0;
    head->W = 0;
    head->E = 0;
    head->S = 0;
    head->N = 0;
    head->digit_thresh = 0.;
    head->map_thresh = 0.;

    /******/
    head->Version_Major = 3;
    head->Version_Minor = 0;
    head->Back_Major = 3;
    head->Back_Minor = 0;

    if (Portable_In)
    {
	head->portable = 1;
	dig__fill_head_portable (head);
    }
    else
	head->portable = 0;

    /* IN Portable */
    if (do_port)
    {
/*	dig__Init_portable_code (1); handled above */

	head = &Def_Port_Head;

	strcpy (head->organization, "Default Portable Head");
/*	head->organization[0] = "Default Portable Head"; */
	head->date[0] = 0;
	head->your_name[0] = 0;
	head->map_name[0] = 0;
	head->source_date[0] = 0;
	head->line_3[0] = 0;
	head->orig_scale = 0;
	head->plani_zone = 0;
	head->W = 0;
	head->E = 0;
	head->S = 0;
	head->N = 0;
	head->digit_thresh = 0.;
	head->map_thresh = 0.;

	/******/
	head->Version_Major = 3;
	head->Version_Minor = 0;
	head->Back_Major = 3;
	head->Back_Minor = 0;

	head->portable = 1;
	dig__fill_head_portable (head);
    }
}

struct dig_head *
Vect__get_default_out_head ()
{
    static int first = 1;

    if (first)
    {
	Vect__init_default_heads (0);
	first = 0;
    }

    return (&Def_Out_Head);
}


struct dig_head *
Vect__get_default_in_head ()
{
    static int first = 1;

    if (first)
    {
	Vect__init_default_heads (0);
	first = 0;
    }

    return (&Def_In_Head);
}

struct dig_head *
Vect__get_default_port_head ()
{
    static int first = 1;

    if (first)
    {
	Vect__init_default_heads (1);
	first = 0;
    }

    return (&Def_Port_Head);
}

dumpflags (fhead)
    struct dig_head *fhead;
{
    int i;

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
