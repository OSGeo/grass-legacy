/*
**
**   This file is one of two that can be loaded.  The flag CERL_PORTABLE
**   should *NOT* be defined unless you know what you are doing
**   See the file README_386 in this directory for more information.
*/
#ifdef CERL_PORTABLE
#include "./xstruct_io.c"
#else


/* %W% %G% */
/*
** Written by: Dave Gerdes 5 1988
** US Army Construction Engineering Research Lab
*/

#include "digit.h"

/* routines for reading and writing Dig+ structures. */
/* return 0 on success, -1 on failure of whatever kind */
/* if you dont want it written out, then dont call these routines */
/* ie  check for deleted status before calling a write routine */
/*  in as much as it would be nice to hide that code in here,  */
/*  this is a library routine and we chose to make it dependant on */
/*  as few external files as possible */

/*  these routines assume ptr->alloc_lines  is valid */
/*  Make sure it is initialized before calling */

/*  Internally, my default variables for lines/areas/nodes/isles  are type
**  plus_t  which is typedefed as short.  This limits the current version
**  to no more than 32K lines, nodes etc. (excluding points)
**  All in the name of future expansion, I have converted these values to 
**  longs in the dig_plus data file.

**  NOTE: 3.10 changes plus_t to  ints.
**    This assumes that any reasonable machine will use 4 bytes to
**    store an int.  The mapdev code is not guaranteed to work if
**    plus_t is changed to a type that is larger than an int.
*/

dig_Rd_P_node (ptr, fp)
    FILE *fp;
    struct P_node *ptr;
{
    long conv;
    register int i;

    if (0 >= fread (&(ptr->x      ), sizeof (ptr->x      ), 1, fp)) return (-1);
    if (0 >= fread (&(ptr->y      ), sizeof (ptr->y      ), 1, fp)) return (-1);
    if (0 >= fread (&(conv        ), sizeof (long        ), 1, fp)) return (-1);
    ptr->n_lines = conv;

    dig_node_alloc_line (ptr, (int) ptr->n_lines);
    if (ptr->n_lines)	/* Not guaranteed what fread does w/ 0 */
    {
     for (i = 0 ; i < ptr->n_lines ; i++)
     {
	 if (0>=fread(&conv, sizeof(long), 1,fp)) return(-1);
	 ptr->lines[i] = conv;
     }
     if(0>=fread(ptr->angles,sizeof(float),(int) ptr->n_lines,fp))return(-1);
    }
    ptr->alive = 1;
    return (0);
}
dig_Wr_P_node (ptr, fp)
    FILE *fp;
    struct P_node *ptr;
{
    long conv;
    register int i;

    if (0 >=
    fwrite (&(ptr->x      ), sizeof (ptr->x      ), 1, fp)) return (-1);
    fwrite (&(ptr->y      ), sizeof (ptr->y      ), 1, fp);
    conv = ptr->n_lines;
    fwrite (&(conv), sizeof (long), 1, fp);

    if (ptr->n_lines)
    {
	for (i = 0 ; i < ptr->n_lines ; i++)
	{
	    conv = ptr->lines[i];
	    fwrite (&conv,  sizeof (long),  1, fp);
	}
	fwrite (ptr->angles, sizeof (float), (int) ptr->n_lines, fp);
    }
    return (0);
}

dig_Rd_P_line (ptr, fp)
    FILE *fp;
    struct P_line *ptr;
{
    long conv;

    if (0 >= fread (&(conv       ), sizeof (long       ), 1, fp)) return (-1);
    ptr->N1 = conv;
    if (0 >= fread (&(conv       ), sizeof (long       ), 1, fp)) return (-1);
    ptr->N2 = conv;
    if (0 >= fread (&(conv       ), sizeof (long       ), 1, fp)) return (-1);
    ptr->left = conv;
    if (0 >= fread (&(conv       ), sizeof (long       ), 1, fp)) return (-1);
    ptr->right = conv;

    if (0 >= fread (&(ptr->N     ), sizeof (ptr->N     ), 1, fp)) return (-1);
    if (0 >= fread (&(ptr->S     ), sizeof (ptr->S     ), 1, fp)) return (-1);
    if (0 >= fread (&(ptr->E     ), sizeof (ptr->E     ), 1, fp)) return (-1);
    if (0 >= fread (&(ptr->W     ), sizeof (ptr->W     ), 1, fp)) return (-1);
    if (0 >= fread (&(ptr->offset), sizeof (ptr->offset), 1, fp)) return (-1);

    if (0 >= fread (&(conv       ), sizeof (long       ), 1, fp)) return (-1);
    ptr->att = conv;

    if (0 >= fread (&(ptr->type  ), sizeof (ptr->type  ), 1, fp)) return (-1);
    return (0);
}
dig_Wr_P_line (ptr, fp)
    FILE *fp;
    struct P_line *ptr;
{
    long conv;

    conv = ptr->N1;
    if (0 >= 
    fwrite (&(conv       ), sizeof (long       ), 1, fp)) return (-1);
    conv = ptr->N2;
    fwrite (&(conv       ), sizeof (long       ), 1, fp);
    conv = ptr->left;
    fwrite (&(conv       ), sizeof (long       ), 1, fp);
    conv = ptr->right;
    fwrite (&(conv       ), sizeof (long       ), 1, fp);

    fwrite (&(ptr->N     ), sizeof (ptr->N     ), 1, fp);
    fwrite (&(ptr->S     ), sizeof (ptr->S     ), 1, fp);
    fwrite (&(ptr->E     ), sizeof (ptr->E     ), 1, fp);
    fwrite (&(ptr->W     ), sizeof (ptr->W     ), 1, fp);
    fwrite (&(ptr->offset), sizeof (ptr->offset), 1, fp);

    conv = ptr->att;
    fwrite (&(conv       ), sizeof (long       ), 1, fp);
    fwrite (&(ptr->type  ), sizeof (ptr->type  ), 1, fp);
    return (0);
}

dig_Rd_P_area (ptr, fp)
    FILE *fp;
    struct P_area *ptr;
{
    register int i;
    long conv;

    if (0 >= fread (&(ptr->N), sizeof (ptr->N), 1, fp)) return (-1);
    if (0 >= fread (&(ptr->S), sizeof (ptr->S), 1, fp)) return (-1);
    if (0 >= fread (&(ptr->E), sizeof (ptr->E), 1, fp)) return (-1);
    if (0 >= fread (&(ptr->W), sizeof (ptr->W), 1, fp)) return (-1);

    if (0 >= fread (&(conv  ), sizeof (long  ), 1, fp)) return (-1);
    ptr->att = conv;
    if (0 >= fread (&(conv  ), sizeof (long  ), 1, fp)) return (-1);
    ptr->n_lines = conv;
    if (0 >= fread (&(conv  ), sizeof (long  ), 1, fp)) return (-1);
    ptr->n_isles = conv;


    ptr->lines = (plus_t *) dig_falloc (sizeof (plus_t), (int) ptr->n_lines);
    if (ptr->n_lines)
    {
	for (i = 0 ; i < ptr->n_lines ; i++)
	{
	    if (0 >= fread (&conv, sizeof (long), 1, fp)) return (-1);
	    ptr->lines[i] = conv;
	}
    }
    ptr->alloc_lines = ptr->n_lines;

    /* island stuff */
    ptr->isles = (plus_t *) dig_falloc (sizeof (plus_t), (int) ptr->n_isles);
    if (ptr->n_isles)
    {
	for (i = 0 ; i < ptr->n_isles ; i++)
	{
	    if (0 >= fread (&conv, sizeof (long), 1, fp)) return (-1);
	    ptr->isles[i] = conv;
	}
    }
    ptr->alloc_isles = ptr->n_isles;

    ptr->alive = 1;
    return (0);
}
dig_Wr_P_area (ptr, fp)
    FILE *fp;
    struct P_area *ptr;
{
    register int i;
    long conv;

    if (0 >= 
    fwrite (&(ptr->N), sizeof (ptr->N), 1, fp)) return (-1);
    fwrite (&(ptr->S), sizeof (ptr->S), 1, fp);
    fwrite (&(ptr->E), sizeof (ptr->E), 1, fp);
    fwrite (&(ptr->W), sizeof (ptr->W), 1, fp);

    conv = ptr->att;
    fwrite (&(conv  ), sizeof (long   ), 1, fp);
    conv = ptr->n_lines;
    fwrite (&(conv  ), sizeof (long  ), 1, fp);
    conv = ptr->n_isles;
    fwrite (&(conv  ), sizeof (long  ), 1, fp);

    if (ptr->n_lines)
    {
	for ( i = 0 ; i < ptr->n_lines ; i++)
	{
	    conv = ptr->lines[i];
	    fwrite (&conv, sizeof (long), 1, fp);
	}
    }
    /* island stuff */
    if (ptr->n_isles)
    {
	for ( i = 0 ; i < ptr->n_isles ; i++)
	{
	    conv = ptr->isles[i];
	    fwrite (&conv, sizeof (long), 1, fp);
	}
    }
    return (0);
}

/* island stuff */
dig_Rd_P_isle (ptr, fp)
    FILE *fp;
    struct P_isle *ptr;
{
    register int i;
    long conv;

    if (0 >= fread (&(ptr->N), sizeof (ptr->N), 1, fp)) return (-1);
    if (0 >= fread (&(ptr->S), sizeof (ptr->S), 1, fp)) return (-1);
    if (0 >= fread (&(ptr->E), sizeof (ptr->E), 1, fp)) return (-1);
    if (0 >= fread (&(ptr->W), sizeof (ptr->W), 1, fp)) return (-1);

    if (0 >= fread (&(conv  ), sizeof (long  ), 1, fp)) return (-1);
    ptr->area = conv;
    if (0 >= fread (&(conv  ), sizeof (long  ), 1, fp)) return (-1);
    ptr->n_lines = conv;

    ptr->lines = (plus_t *) dig_falloc (sizeof (plus_t), (int) ptr->n_lines);
    if (ptr->n_lines)
    {
	for (i = 0 ; i < ptr->n_lines ; i++)
	{
	    if (0 >= fread (&conv, sizeof (long),  1, fp)) return (-1);
	    ptr->lines[i] = conv;
	}
    }
    ptr->alloc_lines = ptr->n_lines;

    ptr->alive = 1;
    return (0);
}
dig_Wr_P_isle (ptr, fp)
    FILE *fp;
    struct P_isle *ptr;
{
    register int i;
    long conv;

    if (0 >= 
    fwrite (&(ptr->N      ), sizeof (ptr->N      ), 1, fp)) return (-1);
    fwrite (&(ptr->S      ), sizeof (ptr->S      ), 1, fp);
    fwrite (&(ptr->E      ), sizeof (ptr->E      ), 1, fp);
    fwrite (&(ptr->W      ), sizeof (ptr->W      ), 1, fp);

    conv = ptr->area;
    fwrite (&(conv        ), sizeof (long        ), 1, fp);
    conv = ptr->n_lines;
    fwrite (&(conv        ), sizeof (long        ), 1, fp);
    if (ptr->n_lines)
    {
	for (i = 0 ; i < ptr->n_lines ; i++)
	{
	    conv = ptr->lines[i];
	    fwrite (&conv, sizeof (long), 1, fp);
	}
    }
    return (0);
}

dig_Rd_P_att (ptr, fp)
    FILE *fp;
    struct P_att *ptr;
{
    long conv;

    if (0 >= fread (&(ptr->x     ), sizeof (ptr->x     ), 1, fp)) return (-1);
    if (0 >= fread (&(ptr->y     ), sizeof (ptr->y     ), 1, fp)) return (-1);
    if (0 >= fread (&(ptr->offset), sizeof (ptr->offset), 1, fp)) return (-1);
    if (0 >= fread (&(ptr->cat   ), sizeof (ptr->cat   ), 1, fp)) return (-1);

    if (0 >= fread (&(conv       ), sizeof (long       ), 1, fp)) return (-1);
    ptr->index = conv;

    if (0 >= fread (&(ptr->type  ), sizeof (ptr->type  ), 1, fp)) return (-1);
    return (0);
}
dig_Wr_P_att (ptr, fp)
    FILE *fp;
    struct P_att *ptr;
{
    long conv;

    if (0 >= 
    fwrite (&(ptr->x     ), sizeof (ptr->x     ), 1, fp)) return (-1);
    fwrite (&(ptr->y     ), sizeof (ptr->y     ), 1, fp);
    fwrite (&(ptr->offset), sizeof (ptr->offset), 1, fp);
    fwrite (&(ptr->cat   ), sizeof (ptr->cat   ), 1, fp);

    conv = ptr->index;
    fwrite (&(conv       ), sizeof (long       ), 1, fp);

    fwrite (&(ptr->type  ), sizeof (ptr->type  ), 1, fp);
    return (0);
}

dig_Rd_Plus_head (ptr, fp)
    struct Plus_head *ptr;
    FILE *fp;
{
    long conv;

rewind (fp);
if(0>=fread (&(conv), sizeof(long), 1, fp))return(-1);
    ptr->Major = conv;
if(0>=fread (&(conv), sizeof(long), 1, fp))return(-1);
    ptr->Minor = conv;
if(0>=fread (&(conv), sizeof(long), 1, fp))return(-1);
    ptr->n_nodes = conv;
if(0>=fread (&(conv), sizeof(long), 1, fp))return(-1);
    ptr->n_lines = conv;
if(0>=fread (&(conv), sizeof(long), 1, fp))return(-1);
    ptr->n_areas = conv;
if(0>=fread (&(conv), sizeof(long), 1, fp))return(-1);
    ptr->n_atts = conv;
if(0>=fread (&(conv), sizeof(long), 1, fp))return(-1);
    ptr->n_isles = conv;
if(0>=fread (&(conv), sizeof(long), 1, fp))return(-1);
    ptr->n_llines = conv;
if(0>=fread (&(conv), sizeof(long), 1, fp))return(-1);
    ptr->n_alines = conv;
if(0>=fread (&(conv), sizeof(long), 1, fp))return(-1);
    ptr->n_plines = conv;

if(0>=fread (&(conv), sizeof(long), 1, fp))return(-1);
    ptr->n_points = conv;
if(0>=fread (&(ptr->Node_offset ), sizeof(ptr->Node_offset ), 1, fp))return(-1);
if(0>=fread (&(ptr->Line_offset ), sizeof(ptr->Line_offset ), 1, fp))return(-1);
if(0>=fread (&(ptr->Area_offset ), sizeof(ptr->Area_offset ), 1, fp))return(-1);
if(0>=fread (&(ptr->Att_offset  ), sizeof(ptr->Att_offset  ), 1, fp))return(-1);
if(0>=fread (&(ptr->Isle_offset ), sizeof(ptr->Isle_offset ), 1, fp))return(-1);
if(0>=fread (&(ptr->Dig_size    ), sizeof(ptr->Dig_size    ), 1, fp))return(-1);
if(0>=fread (&(ptr->Att_size    ), sizeof(ptr->Att_size    ), 1, fp))return(-1);
if(0>=fread (&(ptr->Dig_code    ), sizeof(ptr->Dig_code    ), 1, fp))return(-1);
if(0>=fread (&(ptr->Att_code    ), sizeof(ptr->Att_code    ), 1, fp))return(-1);
if(0>=fread (&(conv             ), sizeof(long             ), 1, fp))return(-1);
    ptr->all_areas = conv;
if(0>=fread (&(conv             ), sizeof(long             ), 1, fp))return(-1);
    ptr->all_isles = conv;
if(0>=fread (&(ptr->snap_thresh ), sizeof(ptr->snap_thresh ), 1, fp))return(-1);
if(0>=fread (&(ptr->prune_thresh), sizeof(ptr->prune_thresh), 1, fp))return(-1);
if(0>=fread (&(ptr->Back_Major  ), sizeof(ptr->Back_Major  ), 1, fp))return(-1);
if(0>=fread (&(ptr->Back_Minor  ), sizeof(ptr->Back_Minor  ), 1, fp))return(-1);
if(0>=fread (&(ptr->future3     ), sizeof(ptr->future3     ), 1, fp))return(-1);
if(0>=fread (&(ptr->future4     ), sizeof(ptr->future4     ), 1, fp))return(-1);
if(0>=fread (&(ptr->F1          ), sizeof(ptr->F1          ), 1, fp))return(-1);
if(0>=fread (&(ptr->F2          ), sizeof(ptr->F2          ), 1, fp))return(-1);
if(0>=fread (&(ptr->F3          ), sizeof(ptr->F3          ), 1, fp))return(-1);
if(0>=fread (&(ptr->F4          ), sizeof(ptr->F4          ), 1, fp))return(-1);

if(0 >= fread (ptr->Dig_name, HEADSTR, 1, fp)) return (-1);
if(0 >= fread (ptr->filler,   HEADSTR, 1, fp)) return (-1);

/* check version numbers */
if(ptr->Major != VERSION_MAJOR || (ptr->Major==VERSION_MAJOR && ptr->Minor > VERSION_MAJOR + 5))
{
    if (VERSION_MAJOR < ptr->Back_Major || (VERSION_MAJOR == ptr->Back_Major &&
	VERSION_MINOR < ptr->Back_Minor))
    {
	fprintf (stderr, "Vector format version (%d.%d) is not known by this release.  EXITING\n",
	    ptr->Major, ptr->Minor);
	fprintf(stderr, "Try running support.vect to reformat the dig_plus file\n");
	exit (-1);
    }
}

return (0);
}
dig_Wr_Plus_head (ptr, fp)
    struct Plus_head *ptr;
    FILE *fp;
{
    long conv;
    
    /* is there a better place for this? */
    ptr->Major = VERSION_MAJOR;
    ptr->Minor = VERSION_MINOR;

    rewind (fp);
    conv = ptr->Major;
    if (0 >= 
    fwrite (&(conv ), sizeof (long), 1, fp)) return(-1);
    conv = ptr->Minor;
    fwrite (&(conv ), sizeof (long), 1, fp);


    conv = ptr->n_nodes;
    fwrite (&(conv ), sizeof (long), 1, fp);
    conv = ptr->n_lines;
    fwrite (&(conv ), sizeof (long), 1, fp);
    conv = ptr->n_areas;
    fwrite (&(conv ), sizeof (long), 1, fp);
    conv = ptr->n_atts;
    fwrite (&(conv ), sizeof (long), 1, fp);
    conv = ptr->n_isles;
    fwrite (&(conv ), sizeof (long), 1, fp);
    conv = ptr->n_llines;
    fwrite (&(conv ), sizeof (long), 1, fp);
    conv = ptr->n_alines;
    fwrite (&(conv ), sizeof (long), 1, fp);
    conv = ptr->n_plines;
    fwrite (&(conv ), sizeof (long), 1, fp);

    conv = ptr->n_points;
    fwrite (&(conv             ), sizeof (long             ), 1, fp);
    fwrite (&(ptr->Node_offset ), sizeof (ptr->Node_offset ), 1, fp);
    fwrite (&(ptr->Line_offset ), sizeof (ptr->Line_offset ), 1, fp);
    fwrite (&(ptr->Area_offset ), sizeof (ptr->Area_offset ), 1, fp);
    fwrite (&(ptr->Att_offset  ), sizeof (ptr->Att_offset  ), 1, fp);
    fwrite (&(ptr->Isle_offset ), sizeof (ptr->Isle_offset ), 1, fp);
    fwrite (&(ptr->Dig_size    ), sizeof (ptr->Dig_size    ), 1, fp);
    fwrite (&(ptr->Att_size    ), sizeof (ptr->Att_size    ), 1, fp);
    fwrite (&(ptr->Dig_code    ), sizeof (ptr->Dig_code    ), 1, fp);
    fwrite (&(ptr->Att_code    ), sizeof (ptr->Att_code    ), 1, fp);
    conv = ptr->all_areas;
    fwrite (&(conv             ), sizeof (long             ), 1, fp);
    conv = ptr->all_isles;
    fwrite (&(conv             ), sizeof (long             ), 1, fp);
    fwrite (&(ptr->snap_thresh ), sizeof (ptr->snap_thresh ), 1, fp);
    fwrite (&(ptr->prune_thresh), sizeof (ptr->prune_thresh), 1, fp);
    ptr->Back_Major = EARLIEST_MAJOR;
    fwrite (&(ptr->Back_Major  ), sizeof (ptr->Back_Major  ), 1, fp);
    ptr->Back_Minor = EARLIEST_MINOR;
    fwrite (&(ptr->Back_Minor  ), sizeof (ptr->Back_Minor  ), 1, fp);
    fwrite (&(ptr->future3     ), sizeof (ptr->future3     ), 1, fp);
    fwrite (&(ptr->future4     ), sizeof (ptr->future4     ), 1, fp);
    fwrite (&(ptr->F1          ), sizeof (ptr->F1          ), 1, fp);
    fwrite (&(ptr->F2          ), sizeof (ptr->F2          ), 1, fp);
    fwrite (&(ptr->F3          ), sizeof (ptr->F3          ), 1, fp);
    fwrite (&(ptr->F4          ), sizeof (ptr->F4          ), 1, fp);

    fwrite (ptr->Dig_name, HEADSTR, 1, fp);
    fwrite (ptr->filler, HEADSTR, 1, fp);
    return (0);
}


#endif   /* CERL_PORTABLE */
