/*
*  Update a history file.  Some of the digit file information  is placed in
*  the hist file.
*  returns  0  -  successful creation of history file
*          -1  -  error
*/

#include <string.h>
#include <stdio.h>
#include <grass/gis.h>
#include <grass/dbmi.h>
#include <grass/Vect.h>
#include <grass/glocale.h>

int update_hist (char *raster_name, char *vector_name,
    char *vector_mapset, long scale)
{
    char  *mapset ;
    struct History hist ;

    if(raster_name == NULL)
        return(-1) ;

    mapset = G_mapset() ;

    if (G_read_history(raster_name, mapset, &hist) < 0)
	return -1;


    strcpy(hist.title, raster_name) ;

/*  store information from digit file into history  */
    sprintf(hist.datsrc_1, "Vector Map: %s in mapset %s", vector_name, vector_mapset);
    sprintf(hist.datsrc_2, "Original Scale from Vector Map: 1:%ld",
        scale) ;  /* 4.0 */

    /***  copying to the  second page of history instead of 1st page
    sprintf(hist.edhist[hist.edlinecnt++], "Original Map Scale: 1:%s",
        dlg_struct->head.orig_scale) ;
    ***/

    /* store command line options */
    G_command_history(&hist);

    return (G_write_history(raster_name, &hist)) ;
}

int 
update_colors (char *raster_name)
{
    struct Range range;
    struct Colors colors;
    CELL min,max;

    G_read_range (raster_name, G_mapset(), &range);
    G_get_range_min_max (&range, &min, &max);
    G_make_rainbow_colors (&colors, min, max);
    G_write_colors (raster_name, G_mapset(), &colors);

    return 0;
}


int
update_fcolors (char *raster_name)
{
    struct FPRange range;
    struct Colors colors;
    DCELL min,max;

    G_read_fp_range(raster_name, G_mapset(), &range);
    G_get_fp_range_min_max(&range, &min, &max);
    G_make_rainbow_colors(&colors, (CELL)min, (CELL)max);
    G_write_colors(raster_name, G_mapset(), &colors);

    return 0;
}


int 
update_cats (char *raster_name, char *vector_name, char *vector_mapset)
{
    /* TODO: maybe attribute transfer from vector map? 
       Use G_set_raster_cat() somewhere*/
    
    struct Categories cats;

    G_strip(raster_name);
    G_init_cats ((CELL) 0, raster_name, &cats);
    G_write_cats (raster_name, &cats);

    return 0;
}

int update_dbcolors(char *rast_name, char *vector_map, int field, char *rgb_column, int is_fp, char *attr_column)
{
    int i;
    int format;

    /* Map */
    struct Map_info Map;
    char *vector_mapset;

    /* Attributes */
    int nrec;
    struct field_info *Fi;
    dbDriver *Driver;
    dbCatValArray cvarr;
    dbValue value;

    /* colors */
    int cat;
    struct Colors colors;
    char colorstring[12];

    struct My_color_rule {
        int red;
        int green;
        int blue;
        double d;
        int i;
    } *my_color_rules;

    int colors_n_values = 0;
    int red;
    int grn;
    int blu;

    /* init colors structure */
    G_init_colors(&colors);

    /* open vector map and database driver */
    vector_mapset = G_find_vector2 (vector_map, "");
    Vect_open_old (&Map, vector_map, vector_mapset);
    db_CatValArray_init ( &cvarr );
    Fi = Vect_get_field( &Map,field);

    if ( Fi == NULL ) {
        G_fatal_error ("Cannot get layer info for vector map");
    }

    Driver = db_start_driver_open_database ( Fi->driver, Fi->database );

    if (Driver == NULL)
        G_fatal_error("Cannot open database %s by driver %s", Fi->database, Fi->driver);

    /* get number of records in attr_column */
    nrec = db_select_CatValArray ( Driver, Fi->table, Fi->key, attr_column , NULL, &cvarr );
    G_debug (3, "nrec = %d", nrec );

    if ( nrec < 1 ) 
        G_fatal_error ("Cannot select data from table");

    /* allocate space for color rules */
    my_color_rules = (struct My_color_rule *)G_malloc(sizeof(struct My_color_rule)*nrec);

    /* for each attribute */
    for ( i = 0; i < cvarr.n_values; i++ ) {

        /* selecect color attribute and category */
        cat = cvarr.value[i].cat;
        if (db_select_value (Driver,  Fi->table, Fi->key, cat, rgb_column, &value) < 0) {
            G_warning(_("No records selected"));
            continue;
        } 
        sprintf (colorstring, "%s", value.s);

        /* convert color string to three color integers */
        if (*colorstring != '\0') {
            G_debug (3, "element colorstring: %s", colorstring);
        
            if ( G_str_to_color(colorstring, &red, &grn, &blu) == 1) {
                G_debug (3, "cat %d r:%d g:%d b:%d", cat, red, grn, blu);
            } 
            else { 
                G_warning(_("Error in color definition column (%s) "
                "with cat %d: colorstring [%s]"), rgb_column, cat, colorstring);
                G_warning(_("Color set to [200:200:200]"));
                red = grn = blu = 200;
            }
        }
        else {
            G_warning (_("Error in color definition column (%s), with cat %d"),
                rgb_column, cat);
        }

        /* append color rules to my_color_rules array, they will be set
         * later all togheter */
        colors_n_values++;
        my_color_rules[i].red = red;
        my_color_rules[i].green = grn;
        my_color_rules[i].blue = blu;
        if (is_fp) {
            my_color_rules[i].d = cvarr.value[i].val.d;
            G_debug(2,"val: %f rgb: %s", cvarr.value[i].val.d, colorstring);
        }
        else {
            my_color_rules[i].i = cvarr.value[i].val.i;
            G_debug(2,"val: %d rgb: %s", cvarr.value[i].val.i, colorstring);
        }

    } /* /for each value in database */

    /* close the database driver */
    db_close_database_shutdown_driver(Driver);

    /* set the color rules: for each rule*/
    for ( i = 0; i < colors_n_values -1; i++ ) {
        if (is_fp) { /* add floating point color rule */
            G_add_d_raster_color_rule (
                 &my_color_rules[i].d , my_color_rules[i].red, my_color_rules[i].green, my_color_rules[i].blue,
                 &my_color_rules[i+1].d , my_color_rules[i+1].red, my_color_rules[i+1].green, my_color_rules[i+1].blue,
                 &colors);

        }
        else { /* add CELL color rule */
             G_add_color_rule (
                 (CELL) my_color_rules[i].i , my_color_rules[i].red, my_color_rules[i].green, my_color_rules[i].blue,
                 (CELL) my_color_rules[i+1].i , my_color_rules[i+1].red, my_color_rules[i+1].green, my_color_rules[i+1].blue,
                 &colors);
        }
    }

    /* write the rules */
    G_write_colors(rast_name, G_mapset(), &colors);
    
    return 1;
}

int update_labels(char *rast_name, char *vector_map, int field, char *label_column, int is_fp, char *attr_column)
{
    int i;
    int format;

    /* Map */
    struct Map_info Map;
    char *vector_mapset;

    /* Attributes */
    int nrec;
    struct field_info *Fi;
    dbDriver *Driver;
    dbCatValArray cvarr;
    dbValue value;

    /* labels */
    int cat;
    char *labelstring;
    struct Categories rast_cats;
    int labels_n_values = 0;
    struct My_labels_rule {
        char label[256];
        double d;
        int i;
    } *my_labels_rules;

    /* init raster categories */
    G_init_cats((CELL)0, "", &rast_cats);

    /* open vector map and database driver */
    vector_mapset = G_find_vector2 (vector_map, "");

    Vect_open_old (&Map, vector_map, vector_mapset);
    db_CatValArray_init ( &cvarr );
    Fi = Vect_get_field( &Map,field);

    if ( Fi == NULL ) {
        G_fatal_error ("Cannot get layer info for vector map");
    }

    Driver = db_start_driver_open_database ( Fi->driver, Fi->database );

    if (Driver == NULL)
        G_fatal_error("Cannot open database %s by driver %s", Fi->database, Fi->driver);

    /* get number of records in attr_column */
    nrec = db_select_CatValArray ( Driver, Fi->table, Fi->key, attr_column , NULL, &cvarr );
    G_debug (3, "nrec = %d", nrec );

    if ( nrec < 1 ) 
        G_fatal_error ("Cannot select data from table");

    my_labels_rules = (struct My_labels_rule *)G_malloc(sizeof(struct My_labels_rule)*nrec);

    /* for each attribute */
    for ( i = 0; i < cvarr.n_values; i++ ) {

        /* selecect attribute and category */
        cat = cvarr.value[i].cat;
        if (db_select_value (Driver,  Fi->table, Fi->key, cat, label_column, &value) < 0) {
            G_warning(_("No records selected"));
            continue;
        } 

        labels_n_values++;

        sprintf(my_labels_rules[i].label,"%s",value.s);
        
        if (is_fp) {
            my_labels_rules[i].d = cvarr.value[i].val.d;
        }
        else {
            my_labels_rules[i].i = cvarr.value[i].val.i;
        }

    } /* /for each value in database */


    /* close the database driver */
    db_close_database_shutdown_driver(Driver);

    /* set the color rules: for each rule*/
    if (is_fp) { /* add floating point color rule */
        for ( i = 0; i < labels_n_values -1; i++ ) {
            G_set_d_raster_cat(&my_labels_rules[i].d, &my_labels_rules[i+1].d,  my_labels_rules[i].label,&rast_cats);
        }
    }
    else {
        for ( i = 0; i < labels_n_values ; i++ ) {
            G_set_cat(my_labels_rules[i].i, my_labels_rules[i].label,&rast_cats);
        }
    }
    G_write_cats(rast_name, &rast_cats);

    return 1;
}

