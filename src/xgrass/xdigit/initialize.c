/*
**  Written by Dave Gerdes  5/1988
**  US Army Construction Engineering Research Lab
*/
#include "digit.h"
#include "color.h"


initialize ()
{
    init_graphics();
    set_default_display();
}

init_plus (map)
    struct Map_info *map;
{
    map->Node = (P_NODE *) dig_falloc (100, sizeof (P_NODE));
    map->Area = (P_AREA *) dig_falloc (100, sizeof (P_AREA));
    map->Line = (P_LINE *) dig_falloc (100, sizeof (P_LINE));
    map->Att  = (P_ATT  *) dig_falloc (100, sizeof (P_ATT));
    /*
    map->Isle = (P_LINE *) dig_falloc (100, sizeof (P_LINE));
    */
    map->n_nodes = 0;
    map->n_areas = 0;
    map->n_isles = 0;
    map->n_atts =  0;
    map->n_lines = 0;
    map->alloc_nodes = 100;
    map->alloc_areas = 100;
    map->alloc_lines = 100;
    map->alloc_isles = 0;
    map->alloc_atts  = 100;
    map->n_points = 0;
}

init_init (digit, att, plusfil, coor)
    FILE *digit, *att;
    char *plusfil;
    char *coor;
{
    int allocated;

    /*Overlay.head = &overlay_head; 4.0*/	/* name of overlay map */
    Overlay.dig_fp = NULL;
    Overlay.digit_file = NULL;
    N_overlay = "None";
    N_backdrop = "None";
    Contour_Interval = 5;


    /*
    CM = &Map1;
    */
    CM->Node = NULL;
    CM->Area = NULL;
    CM->Line = NULL;
    CM->Att = NULL;
    CM->n_nodes = CM->alloc_nodes = 0;
    CM->n_lines = CM->alloc_lines = 0;
    CM->n_areas = CM->alloc_areas = 0;
    CM->n_isles = CM->alloc_isles = 0;
    CM->n_atts  = CM->alloc_atts = 0;
    CM->n_points = 0;
    CM->dig_fp = digit;
    CM->att_fp = att;
    CM->plus_file = plusfil;
    CM->coor_file = coor;

    /* init allocate Gpoints arrays */
    allocated = 0;
    Gpoints.n_points = 0;
    Gpoints.x = (double *) dig_alloc_space (1000, &allocated, 1000, NULL, sizeof (double));
    allocated = 0;
    Gpoints.y = (double *) dig_alloc_space (1000, &allocated, 1000, NULL, sizeof (double));
    Gpoints.alloc_points = allocated;

    /* allocate arrays for Garea */
    allocated = 0;
    Garea.n_lines = 0;
    Garea.lines = (plus_t*)dig_alloc_space(50, &allocated, 50,NULL,sizeof (plus_t));
/*
    dcolors[WHITE] = D_translate_color ("white");
    dcolors[BLACK] = D_translate_color ("black");
    dcolors[YELLOW] = D_translate_color ("yellow");
    dcolors[BLUE] = D_translate_color ("blue");
    dcolors[RED] = D_translate_color ("red");
    dcolors[GREEN] = D_translate_color ("green");
    dcolors[ORANGE] = D_translate_color ("orange");
    dcolors[GREY] = D_translate_color ("grey");
    dcolors[MAGENTA] = D_translate_color ("magenta");
    dcolors[AQUA] = D_translate_color ("aqua");
    dcolors[INDIGO] = D_translate_color ("indigo");
    dcolors[VIOLET] = D_translate_color ("violet");
    dcolors[BROWN] = D_translate_color ("brown");
*/
#ifndef SCS_MODS 	/* see states.c */
    init_colors ();
#endif /* SCS_MODS */

}


init_colors ()
{
    CLR_LINE = XD_BLUE;
    CLR_AREA = XD_GREY;
    CLR_SITE = XD_GREEN;
    CLR_LSITE = XD_AQUA;
    CLR_LLINE = XD_MAGENTA;
    CLR_LAREA = XD_ORANGE;
    CLR_AMARK = XD_ORANGE;
    CLR_ALABEL = XD_ORANGE;
    CLR_LLABEL = XD_MAGENTA;
    CLR_HIGHLIGHT = XD_YELLOW;
    CLR_ERASE = XD_BLACK;
    CLR_UNKNOWN = XD_WHITE;
    CLR_OVERLAY = XD_WHITE;
    CLR_0_NODE = CLR_SITE;
    CLR_1_NODE = XD_GREEN;
    CLR_2_NODE = XD_RED;
}
