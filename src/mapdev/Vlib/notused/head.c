/*
**  Written by Dave Gerdes 1/91
*/


#include "gis.h"
#include "V_.h"
#include "dig_head.h"


Vect_get_head (name, mapset, head)
    char *name, *mapset;
    struct dig_head *head;
{
    FILE *fp;

    fp = G_fopen_old ("dig", name, mapset);
    if (fp == NULL)
    {
	char buf[300];

	sprintf (buf, "Can't open vector header for file [%s in %s]", name,
	    mapset);
	G_warning (buf);
	return -1;
    }

    V__read_head_file (fp, head);

    fclose (fp);

    return 0;
}


Vect_put_head (name, head)
    char *name;
    struct dig_head *head;
{
    FILE *fp;
    struct dig_head tmp_head, old_head;
    struct dig_head *ptmp_head;

    dig_struct_copy (head, &tmp_head, sizeof (struct dig_head));
    fp = G_fopen_old ("dig", name, G_mapset());
    if (NULL != fp)
    {
	/* header exists.  first check for old portable stuff so we do not 
	** overwrite it.
	*/

	V__read_head_file (fp, &old_head);
	V__copy_portable_info (&old_head, &tmp_head); 	/* from -> to */
	fclose (fp);
    }
    else
    {
	/* creating NEW file, make sure we get default output info
	**  stuffed in there.  Who knows where the user found this struct.
	*/
	ptmp_head = Vect__get_default_out_head ();
	V__copy_portable_info (&ptmp_head, &tmp_head); 	/* from -> to */
    }

    return V__put_head (name, &tmp_head);
}


/* dig_close should call this instead of V_put_head ()
** cuz if writing new, then do not want to preserve old info
*/
Vect__put_head (name, head)
    char *name;
    struct dig_head *head;
{
    FILE *fp;

    fp = G_fopen_new ("dig_hd", name);

    V__write_head_file (fp, head);
}

Vect_init_head (head)
    struct dig_head *head;
{
    /* TODO or something like this... */
    dig_struct_copy (Vect__get_def_out_head (), head, sizeof (struct dig_head));
}

