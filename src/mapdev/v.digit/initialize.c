/*
**  Written by Dave Gerdes  5/1988
**  US Army Construction Engineering Research Lab
*/
#include "display.h"
#include "digit.h"
#include "dig_curses.h"
#include "Map_proto.h"
#include "local_proto.h"
#include "glocale.h"

int initialize (
    FILE *digit,FILE *att,
    char *plusfil,
    char *coor)
{
    init_init (digit, att, plusfil, coor);
    Init_curses();

    init_graphics();
    init_states ();
    set_default_display();

    return 0;
}

int init_plus (struct Map_info *map)
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

    return 0;
}

int init_init (
    FILE *digit,FILE *att,
    char *plusfil,
    char *coor)
{
    int allocated;

    In_Debug = 0;
    /*Overlay.head = &overlay_head; 4.0*/	/* name of overlay map */
    Overlay.dig_fp = NULL;
    Overlay.digit_file = NULL;
    N_overlay = _("None");
    N_backdrop = _("None");
    Contour_Interval = 5;


    /*
    CMap = &Map1;
    */
    CMap->Node = NULL;
    CMap->Area = NULL;
    CMap->Line = NULL;
    CMap->Att = NULL;
    CMap->n_nodes = CMap->alloc_nodes = 0;
    CMap->n_lines = CMap->alloc_lines = 0;
    CMap->n_areas = CMap->alloc_areas = 0;
    CMap->n_isles = CMap->alloc_isles = 0;
    CMap->n_atts  = CMap->alloc_atts = 0;
    CMap->n_points = 0;
    CMap->dig_fp = digit;
    CMap->att_fp = att;
    CMap->plus_file = plusfil;
    CMap->coor_file = coor;

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

#ifndef SCS_MODS 	/* see states.c */
    init_colors ();
#endif /* SCS_MODS */

    return 0;
}


int init_colors (void)
{
    CLR_LINE = BLUE;
    CLR_AREA = GREY;
    CLR_SITE = GREEN;
    CLR_LSITE = AQUA;
    CLR_LLINE = MAGENTA;
    CLR_LAREA = ORANGE;
    CLR_AMARK = ORANGE;
    CLR_ALABEL = ORANGE;
    CLR_LLABEL = MAGENTA;
    CLR_HIGHLIGHT = YELLOW;
    CLR_ERASE = BLACK;
    CLR_UNKNOWN = WHITE;
    CLR_OVERLAY = WHITE;
    CLR_0_NODE = CLR_SITE;
    CLR_1_NODE = GREEN;
    CLR_2_NODE = RED;

    return 0;
}
