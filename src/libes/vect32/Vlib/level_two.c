/*
**  Written by: Dave Gerdes 5 1988
**  US Army Construction Engineering Research Lab
**
**  Modified by Dave Gerdes  1/1991  for  dig_head/fileno stuff
*/

/*
**
**INTERFACE LEVEL II
**==================
**
*/

#include "gis.h"
#include "Vect.h"
#include <stdlib.h>
#include "V_.h"


static int Writable = 0;	/* Open Plus file for WRITE/READONLY */
static char *RW_str = "r";
			

int Vect_P_init (
    char *name,
    char *mapset, 
    struct Map_info *map)
{
    char *error;

    if (NULL != (error = Vect__P_init (map, name, mapset)))
    {
	fprintf (stderr, "%s\n", error);
	exit (-1);
    }
    return (0);
}

/***************************************************************************/
/*     Undocumented Feature:  I.E.  use at your own risk! 
*/

/*
**  Simply ensure that files are opened as read/write.  everything
**  else is up to programmer.  Note that dig_att file is not openned
**  by P_init ().
**
**  This is used buy Vcadlabel and Vclean to modify an existing
**    dig_plus file.  You have to know what is going on in the background
**    to safely use this.
** 
**   The standard method of creating and modifying dig files
**    is to use only the level_one I/O calls to read and write dig files
**    and att files only.
*/
int Vect__P_writeable ( int x)
{
    Writable = x;
    if (x)
	RW_str = "r+";
    else
	RW_str = "r";

    return 0;
}

/***************************************************************************/



/* 
**  Returns NULL   or a pointer to an error message
*/
char *Vect__P_init ( struct Map_info *map, char *name, char *mapset) 
{
    FILE *plusfp;
    int have_old, have_plus, have_attr;
    char file[1024];

    Vect_init ();

    /* need dig open,  need to open plus for load_plus, and att for verify */


    have_old = have_plus = have_attr = 0;
    G__file_name (file, "dig", name, mapset);
    map->digit_file = G_store (file);
    if ( (map->dig_fp = fopen(file, RW_str)) != NULL )
	have_old = 1;
    else
	return ("Cannot open digit file");



    G__file_name (file, "dig_plus", name, mapset);
    map->plus_file = G_store (file);


    if ((plusfp = fopen (file, RW_str)) != NULL)
    {
	fclose (plusfp);
	have_plus = 1;
    }
    else
    {
	fclose(map->dig_fp);   
	map->dig_fp=NULL;    
	return ("Cannot open dig_plus file");
    }


    G__file_name (file, "dig_att", name, mapset);
    map->att_file = G_store (file);

    /*  set up level_one access to head structure */
    /*dig__set_head (map->dig_fp, &(map->head));*/

    Vect__read_head_binary(map, &(map->head));



    map->Line = NULL;
    map->Area = NULL;
    map->Isle = NULL;
    map->Att = NULL;
    map->Node = NULL;

    if (0 > dig_load_plus (map, map->dig_fp, 1))
	return ("Error reading dig_plus file");

    /* OK */
    return (NULL);
}

/*
**  This is to support	V2_init_for_create ()
**  Other than that it is totally UNSUPPORTED
*/
char *
Vect__P_init_new_plus ( struct Map_info *map, char *name)
{
    FILE *plusfp;
    int have_old, have_plus, have_attr;
    char file[1024];

    Vect_init ();

    /* need dig open,  need to open plus for load_plus, and att for verify */


    have_old = have_plus = have_attr = 0;
    G__file_name (file, "dig", name, G_mapset());
    map->digit_file = G_store (file);
    if ( (map->dig_fp = fopen(file, RW_str)) != NULL )
	have_old = 1;
    else
	return ("Cannot open digit file");



    G__file_name (file, "dig_plus", name, G_mapset());
    map->plus_file = G_store (file);


    if ((plusfp = fopen (file, RW_str)) != NULL)
    {
	fclose (plusfp);
	have_plus = 1;
    }
    else
    {
	have_plus = 0;
	/*
	return ("Cannot open dig_plus file");
	*/
    }


    G__file_name (file, "dig_att", name, G_mapset());
    map->att_file = G_store (file);

    /*  set up level_one access to head structure */
    /*dig__set_head (map->dig_fp, &(map->head));*/

    Vect__read_head_binary(map, &(map->head));


/*  If file is 3.x format, need to first update it to 4.0 before
**   editing it.
*/
        if (map->head.Version_Major < 4)
        {
            char buf[200];
            int ret;

/*DEBUG*/ fprintf (stderr, "Converting %s from 3.0 to 4.0\n", name);
            /* call  etc/v.from.3 to update file */

            /* unset references to dig_head */
            fclose (map->dig_fp);

            sprintf (buf, "%s/etc/v.from.3 -p %s", G_gisbase(), name);
            ret = system (buf);
                if (ret & 0xff00) G_fatal_error ("File conversion failed. Possibly Disk Full.\n");


            /* and get back to where we were */
            if ( (map->dig_fp = fopen(map->digit_file, "r+"))  ==  NULL)
            {
                fprintf(stderr,"Can't open vector file for update: %s\n",
                    map->digit_file);
                fprintf(stderr,"Contact your GRASS system administrator\n");
                exit(-1) ;
            }
	    Vect__read_head_binary(map, &(map->head));
        }




    map->Line = NULL;
    map->Area = NULL;
    map->Isle = NULL;
    map->Att = NULL;
    map->Node = NULL;

    /*
    if (0 > dig_load_plus (map, map->dig_fp, 1))
	return ("Error reading dig_plus file");
    */

    /* OK */
    return (NULL);
}





int V2_num_lines ( struct Map_info *map)
{
    return (map->n_lines);
}

int V2_num_areas ( struct Map_info *map)
{
    return (map->n_areas);
}

/* added 1/2002 MN */
int V2_num_islands ( struct Map_info *map)
{
    return (map->n_isles);
}


/* returns category of line */
/* or 0 on any error */
int V2_line_att ( struct Map_info *map, int line)
{
    P_LINE *Line;

    Line = &(map->Line[line]);

    if (line <= 0 || line > map->n_lines || !LINE_ALIVE (Line) || ! Line->att)
	return (0);
    return (map->Att[Line->att].cat);
}

int V2_area_att ( struct Map_info *map, int area)
{
    P_AREA *Area;

    if (area <= 0 || area > map->n_areas)
	return (0);

    Area = &(map->Area[area]);
    if (!AREA_ALIVE (Area) || ! Area->att)
	return (0);
    return (map->Att[Area->att].cat);
}

/* returns -1 on error */
/* note all areas may not be labeled */
/* use get_area_att  > 0 for test of labelled */

int V2_get_area ( struct Map_info *map, int num, P_AREA **Area)
{
    if (num <= 0 || num > map->n_areas)
	return (-1);
    if (!AREA_ALIVE (&(map->Area[num])))
	return (-1);
    *Area = &(map->Area[num]);
    return (0);
}

/* get Area bounding box info in NSEW */
int V2_get_area_bbox ( struct Map_info *map, int area,
    double *N,double *S,double *E,double *W)
{
    P_AREA *Area;

    if (area <= 0 || area > map->n_areas)
	return (-1);
    if (!AREA_ALIVE (&(map->Area[area])))
	return (-1);
    Area = &(map->Area[area]);
    *N = Area->N;
    *E = Area->E;
    *W = Area->W;
    *S = Area->S;
    return (0);
}

/* get Line bounding box info in NSEW */
int V2_get_line_bbox (
    struct Map_info *map, int line,
    double *N,double *S,double *E,double *W)
{
    P_LINE *Line;

    if (line <= 0 || line > map->n_lines)
	return (-1);
    if (!LINE_ALIVE (&(map->Line[line])))
	return (-1);
    Line = &(map->Line[line]);
    *N = Line->N;
    *E = Line->E;
    *W = Line->W;
    *S = Line->S;
    return (0);
}
