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

#include "V_.h"
#include "digit.h"
#include "dig_head.h"
#include "gis.h"


static struct line_pnts Points;
static int Writable = 0;	/* Open Plus file for WRITE/READONLY */
static char *RW_str = "r";
			


static char name_buf[1024];

int
V2_open_old (Map, name, mapset)
    struct Map_info  *Map;
    char *name, *mapset;
{
    Vect_init ();	/* init vector system */

    if (NULL != Vect__P_init (Map, name, mapset))
    {
	return -1;
    }
    Map->open = VECT_OPEN_CODE;
    Map->level = LEVEL_2;
    Map->mode = MODE_READ;

    G__file_name (name_buf, "dig", name, mapset);
    Map->digit_file = G_store (name_buf); 

    Map->name = G_store (name);
    Map->mapset = G_store (mapset);
    Map->Constraint_region_flag = 0;
    Map->Constraint_type_flag   = 0;
    Map->next_line = 1;

    return 0;
}


int
V2__open_new_1 (Map, name)		/* for digit  (unsupported) */
    struct Map_info  *Map;
    char *name;
{
    Vect_init ();	/* init vector system */

    if (0 > V1_open_new (Map, name))	/* open dig for write */
	return -1;

    Vect__P_writeable (0);

    Map->open = VECT_OPEN_CODE;
    Map->level = LEVEL_2;
    Map->mode = MODE_RW;

    G__file_name (name_buf, "dig", name, G_mapset());
    Map->digit_file = G_store (name_buf);

    Map->name = G_store (name);
    Map->mapset = G_store (G_mapset());
    Map->Constraint_region_flag = 0;
    Map->Constraint_type_flag   = 0;
    Map->next_line = 1;

    return 0;
}

/* TODO */
#ifdef FOO
/* can use dig__P_writeable () ?? */
int
V2_open_update (Map, name)		/* for digit, and v.cad.label */
    struct Map_info  *Map;
    char *name;
{
}

int
V2_open_new_plus (Map, name)		/* for v.build */
    struct Map_info  *Map;
    char *name;
{
}
#endif


/*
**  use this routine only at your own risk. 
**   It is not supported
**
**  It was desined to aide   v.build  which needs to open 
**    a dig file for RW, and create a dig_plus file anew.
*/
int
V2__init_for_create_plus (Map, name)	
    struct Map_info  *Map;
    char *name;
{
    Vect_init ();	/* init vector system */

    Vect__P_writeable (1);
    if (NULL != Vect__P_init_new_plus (Map, name))
	return -1;

    Vect__P_writeable (0);

    Map->open = VECT_OPEN_CODE;
    Map->level = LEVEL_2;
    Map->mode = MODE_RW;

    G__file_name (name_buf, "dig", name, G_mapset());
    Map->digit_file = G_store (name_buf);

    Map->name = G_store (name);
    Map->mapset = G_store (G_mapset());
    Map->Constraint_region_flag = 0;
    Map->Constraint_type_flag   = 0;
    Map->next_line = 1;

    return 0;
}


/*
**  use this routine only at your own risk. 
**   It is not supported
**
**  It was desined to aide   v.from.3  
*/
int
V2__open_update_1 (Map, name)
    struct Map_info  *Map;
    char *name;
{
    Vect_init ();	/* init vector system */

    Vect__P_writeable (1);
    if (NULL != (Vect__P_init (Map, name, G_mapset())))
	return -1;

    Vect__P_writeable (0);

    Map->open = VECT_OPEN_CODE;
    Map->level = LEVEL_2;
    Map->mode = MODE_RW;

    G__file_name (name_buf, "dig", name, G_mapset());
    Map->digit_file = G_store (name_buf);

    Map->name = G_store (name);
    Map->mapset = G_store (G_mapset());
    Map->Constraint_region_flag = 0;
    Map->Constraint_type_flag   = 0;
    Map->next_line = 1;

    return 0;
}

int
V2__setup_for_digit (Map, name)
    struct Map_info  *Map;
    char *name;
{
    Vect_init ();	/* init vector system */

    Map->open = VECT_OPEN_CODE;
    Map->level = LEVEL_2;
    Map->mode = MODE_RW;
    Map->name = G_store (name);
    Map->mapset = G_store (G_mapset());
    Map->Constraint_region_flag = 0;
    Map->Constraint_type_flag   = 0;
    Map->next_line = 1;

    return 0;
}
