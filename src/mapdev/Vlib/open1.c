/*
**  Written by:  Dave Gerdes   1-2/1991 
**  US Army Construction Engineering Research Lab
**
**  Overhauling Library interface
*/

/*  
 *******************************************************************
 *  #include "head.h"
 *
 *  dig_init (fd)
 *      FILE *fd ;
 *
 *
 * returns:  -1 on error
 *            0 on completion
 */

#include "V_.h"
#include "gis.h"

#include <sys/types.h>
#include <sys/stat.h>

static char name_buf[1024];

int
V1_open_old (Map, name, mapset)
    struct Map_info *Map;
    char *name;
    char *mapset;
{
    FILE *fp;

    Vect_init ();	/* init vector system */

/*DEBUG debugf ("Openning file %s in %s level 1 READ\n", name, mapset);*/
    if (NULL == (fp = G_fopen_old ("dig", name, mapset)))
	return -1;

    Map->dig_fp = fp;


    G__file_name (name_buf, "dig", name, mapset);

    Map->digit_file = G_store (name_buf); /*need?*/
    Map->name = G_store (name);
    Map->mapset = G_store (mapset);
    Map->open = VECT_OPEN_CODE;
    Map->level = LEVEL_1;
    Map->mode = MODE_READ;
    Map->Constraint_region_flag = 0;
    Map->Constraint_type_flag   = 0;

    return (Vect__read_head_binary(Map, &(Map->head)));	/* note dig_ => dig__ */
}

int
V1_open_new (Map, name)
    struct Map_info *Map;
    char *name;
{
    FILE *fp;

    Vect_init ();	/* init vector system */

/*DEBUG debugf ("Openning file %s level 1 WRITE\n", name);*/
    if (NULL == (fp = G_fopen_new ("dig", name)))
	return -1;

    Map->dig_fp = fp;

    {     /* added Sep 22, 1992  -dpg */
	/* check to see if dig_plus file exists and if so, remove it */

	struct stat info;

	G__file_name (name_buf, "dig_plus", name, G_mapset());
	if (stat (name_buf, &info) == 0)	/* file exists? */
	{
	    unlink (name_buf);
	}
    }


    G__file_name (name_buf, "dig", name, G_mapset());
    Map->digit_file = G_store (name_buf); /*need?*/

    Map->open  = VECT_OPEN_CODE;
    Map->level = LEVEL_1;
    Map->mode  = MODE_WRITE;
    Map->name = G_store (name);
    Map->mapset = G_store (G_mapset());
    Map->Constraint_region_flag = 0;	/* these do not apply to to write, but*/
    Map->Constraint_type_flag   = 0;    /* init them anyway                   */

    Vect__init_head (&(Map->head));
    Vect__write_head_binary (Map, &(Map->head)); /* note dig_ => dig__*/

    return 0;
}

/*
**  Not supported do not use
*/

int
V1__open_update_1 (Map, name)
    struct Map_info *Map;
    char *name;
{
    FILE *fp;

    Vect_init ();	/* init vector system */

/*DEBUG debugf ("Openning file %s level 1 UPDATE\n", name);*/
    if (NULL == (fp = G_fopen_modify ("dig", name)))
	return -1;

    Map->dig_fp = fp;

    G__file_name (name_buf, "dig", name, G_mapset());
    Map->digit_file = G_store (name_buf); 

    Map->open = VECT_OPEN_CODE;
    Map->level = LEVEL_1;
    Map->mode = MODE_RW;
    Map->name = G_store (name);
    Map->mapset = G_store (G_mapset());
    Map->Constraint_region_flag = 0;
    Map->Constraint_type_flag   = 0;

    return (Vect__read_head_binary(Map, &(Map->head)));	/* note dig_ => dig__ */
}

