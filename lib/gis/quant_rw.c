#include "gis.h"
#include <string.h>

/*********************************************************************
*
*   G_quantize_fp_map(name, mapset, min, max)
*   char *name, *mapset;   name of the map
*   CELL min, max;         resulting int range
*
*   Writes necessary quant rules for map <name> so that
*   a floating range of <name> is mapped into integer range (min, max)
*
**********************************************************************
* 
*   G_quantize_fp_map_range(name, mapset, d_min, d_max, min, max)
*   char *name, *mapset;   name of the map
*   CELL min, max;         resulting int range
*   DCELL d_min, d_max;    floating point range
*
*   Make a rule for map <name> that maps floating range (d_min, d_max)
*   into integer range (min, max)
*   This function is useful when the quant rule doesn't depend of the
*   range of produced float data, for example the slope map whould
*   want to have a quant rule: 0.0, 90.0 -> 0 , 90
*   no matter what the min and max slope of this map is.
*
**********************************************************************
* 
*   G_write_quant(name, mapset, quant)
*        char *name, *mapset;
*        struct Quant *quant;
*   writes the quant rule table for the map <name>
*
**********************************************************************
* 
*   G_read_quant(name, mapset, quant)
*        char *name, *mapset;
*
*   reads the quant table for name@mapset
*
**********************************************************************
*
*   G_truncate_fp_map(name, mapset)
*        char *name, *mapset;
*        struct Quant *quant;
*
*   writes the quant rules which indicate that all floating numbers
*   should be truncated instead of applying any quant rules from
*   floats to integers
*
**********************************************************************
*
*   G_round_fp_map(name, mapset)
*        char *name, *mapset;
*        struct Quant *quant;
*
*   writes the quant rules which indicate that all floating numbers
*   should be rounded instead of applying any quant rules from
*   floats to integers
*
**********************************************************************/

int G_truncate_fp_map(char *name,char *mapset)
{
    char buf[300];
    struct Quant quant;

    G_quant_init(&quant);
    G_quant_truncate(&quant);
    /* quantize the map */
    if(G_write_quant (name, mapset, &quant) < 0)
    {
        sprintf(buf, "G_truncate_fp_map: can't write quant rules for map %s", name);        G_warning(buf);
        return -1;
    }
    return 1;
}

int G_round_fp_map(char *name,char *mapset)
{
    char buf[300];
    struct Quant quant;

    G_quant_init(&quant);
    G_quant_round(&quant);
    /* round the map */
    if(G_write_quant (name, mapset, &quant) < 0)
    {
        sprintf(buf, "G_truncate_fp_map: can't write quant rules for map %s", name);        G_warning(buf);
        return -1;
    }
    return 1;
}

int G_quantize_fp_map(
    char *name,char *mapset,
    CELL min,CELL max)
{
    char buf[300];
    DCELL d_min, d_max;
    struct FPRange fp_range;

    if(G_read_fp_range(name, mapset, &fp_range) < 0)
    {
        sprintf(buf, "G_quantize_fp_map: can't read fp range for map %s", name);
        G_warning(buf);
        return -1;
    }
    G_get_fp_range_min_max(&fp_range, &d_min,  &d_max);
    if(G_is_d_null_value(&d_min) || G_is_d_null_value(&d_max))
    {
	 sprintf(buf, "G_quantize_fp_map: raster map %s is empty", name);
	 G_warning(buf);
	 return -1;
    }
    return G_quantize_fp_map_range(name, mapset, d_min, d_max, min, max);
}

/*-------------------------------------------------------------------------*/

int G_quantize_fp_map_range(
    char *name,char *mapset,
    DCELL d_min,DCELL d_max,
    CELL min,CELL max)
{
    char buf[300];
    struct Quant quant;

    G_quant_init(&quant);
    G_quant_add_rule(&quant, d_min, d_max, min, max);
    /* quantize the map */
    if(G_write_quant (name, mapset, &quant) < 0)
    {
        sprintf(buf, "G_quantize_fp_map_range: can't write quant rules for map %s", name);        G_warning(buf);
        return -1;
    }
    return 1;
}

/*-------------------------------------------------------------------------*/


int G_write_quant(
     char *name,char *mapset,
     struct Quant *quant)
{
     CELL cell_min, cell_max;
     DCELL d_min, d_max;
     char buf[300];

     if (G_raster_map_type (name, mapset) == CELL_TYPE)
     {
            sprintf(buf, "Cannot write quant rules: map %s is integer", name);
            G_warning(buf);
            return -1;
     }

     G_quant_get_limits (quant, &d_min, &d_max, &cell_min, &cell_max); 

     /* first actually write the rules */
     if( G__quant_export (name, mapset, quant) < 0)
     {
            sprintf(buf, "Cannot write quant rules for map %s", name);
            G_warning(buf);
            return -1;
     }

     return 1;
}

/*-------------------------------------------------------------------------*/

int G_read_quant(
     char *name,char *mapset,
     struct Quant *quant)
{
     G_quant_init (quant);
     return G__quant_import(name, mapset, quant);
}
