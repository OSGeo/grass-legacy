
/*
 *   main.c 
 *
 *   cell-file profiling.
 *
 */

#define DEBUG 
#define MAIN
#define USE_OLD_CODE  /* Frame set-up still needs old code ATM. */
#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include "profile.h"
#include "display.h"
#include "raster.h"
#include "math.h"
#include "D.h"

struct Profile profile;

void myDcell (char *name, char *mapset, int overlay);

int main (int argc, char **argv)
{
    char   *old_mapset, *old_mapname, *d_mapset, *d_mapname ;
    double cur_ux, cur_uy ;
    double ux, uy ;
    char   ltr[10];
    int    text_width, text_height;
    int    err, doplot;
    int    button ;
    int    cur_screen_x, cur_screen_y ;
    int    screen_x, screen_y ;
    struct Cell_head window ;
    int    t, b, l, r ;
    int    i,CurrentWin=0;
    long   min,max;
    struct Option *map, *dmap, *plotfile;
    struct GModule *module;

    /* Initialize the GIS calls */
    G_gisinit(argv[0]) ;

    /* Set description */
    module              = G_define_module();
    module->description = ""\
        "Interactive profile plotting utility with optional output.";
    
    /* set up command line */
    map              = G_define_option();
    map->key         = "rast";
    map->type        = TYPE_STRING;
    map->required    = YES;
    map->gisprompt   = "old,cell,raster" ;
    map->description = "Raster map to be profiled";

    dmap              = G_define_option();
    dmap->key         = "drast";
    dmap->type        = TYPE_STRING;
    dmap->required    = NO;
    dmap->gisprompt   = "old,cell,raster" ;
    dmap->description = "Optional display raster";
    
    plotfile              = G_define_option();
    plotfile->key         = "plotfile";
    plotfile->type        = TYPE_STRING;
    plotfile->required    = NO;
    plotfile->description = "Output profile data to file(s) with prefix 'name'";

    if (G_parser(argc, argv))
        exit(1);

    old_mapname = map->answer;

    old_mapset = G_find_cell2 (old_mapname, "") ;
    if (old_mapset == NULL)
    {
        char buf[256];
        sprintf(buf,"Raster map [%s] not available", old_mapname);
        G_fatal_error(buf) ;
    }

    if (plotfile->answer != NULL)
        doplot = 1;
    else
        doplot = 0;
    
    /* If the user wants to display a different raster */
    if (dmap->answer != NULL)
    {
        d_mapname = dmap->answer;
        d_mapset  = G_find_cell2 (d_mapname, "") ;
        if (d_mapset == NULL)
        {
            G_warning("Display raster [%s] not found. Using profile raster.",
                    d_mapname);
            d_mapname = old_mapname;
            d_mapset  = old_mapset;
        }
    }
    else
    {
        d_mapname = old_mapname;
        d_mapset  = old_mapset;
    }
    
    /* get cell-file range */
    WindowRange(old_mapname,old_mapset,&min,&max);

	/* the following code should not be used to get fp range correctly.
if (!quick_range(old_mapname,old_mapset,&min,&max))
   {
   if (!slow_range(old_mapname,old_mapset,&min,&max))
      G_fatal_error("Unable to read from cell-file");
   }
if (min > 0) min = 0;
if (max < 0) max = 0;
*/

    fprintf (stdout,"\n\nUse mouse to choose action\n");

    /* establish connection with graphics driver */
    if (R_open_driver() != 0)
        G_fatal_error ("No graphics device selected");

    /* Make sure screen is clear */
    R_standard_color(D_translate_color(DEFAULT_BG_COLOR));
    Dclearscreen();

    /* Establish windows on screen */
#ifdef USE_OLD_CODE
    Dnew(MOU.name, MOU.bot, MOU.top, MOU.left, MOU.right) ;
    Dnew(STA.name, STA.bot, STA.top, STA.left, STA.right) ;
    Dnew(MAP.name, MAP.bot, MAP.top, MAP.left, MAP.right) ;
    Dnew(ORIG.name, ORIG.bot, ORIG.top, ORIG.left, ORIG.right) ;
    for (i=0; i<=3; i++)
        Dnew(profiles[i].name,profiles[i].bot,profiles[i].top,
            profiles[i].left,profiles[i].right);
#else
    /* This operates different than above, expect real world coords ? */
    D_new_window(MOU.name, MOU.top, MOU.bot, MOU.left, MOU.right);
    D_new_window(STA.name, STA.top, STA.bot, STA.left, STA.right);
    D_new_window(MAP.name, MAP.top, MAP.bot, MAP.left, MAP.right);
    D_new_window(ORIG.name, ORIG.top, ORIG.bot, ORIG.left, ORIG.right);
    for (i = 0; i < 4; i++)
        D_new_window(profiles[i].name, profiles[i].top,
                profiles[i].bot, profiles[i].left, profiles[i].right);
#endif
    
    /* Plot cell-file in map window */
    D_set_cur_wind (MAP.name);
    myDcell(d_mapname, d_mapset, 1);
	

    /* loop until user wants to quit */
    for(;;)
    {
        /* display mouse-menu in mouse-menu window */
        D_set_cur_wind (MOU.name);
        R_standard_color(D_translate_color(DEFAULT_BG_COLOR));
        D_erase_window();
        R_standard_color(D_translate_color("red"));
        DrawText(25,1,1,"GRASS PROGRAM: profile");
        R_standard_color(D_translate_color(DEFAULT_FG_COLOR));
        DrawText(15,3,1,"MOUSE   | Left:   Where am I?");
#ifdef ANOTHER_BUTTON
        DrawText(15,4,1,"BUTTON  | Right:  Set FIRST point\n");
        DrawText(15,5,1,"MENU    | Middle: Quit this");
#else
        DrawText(15,4,1,"BUTTON  | Middle: Set FIRST point");
        DrawText(15,5,1,"MENU    | Right:  Quit this\n");
#endif
	R_stabilize();

        /* LOOP to get first point of line */
        do
        {

            /* choose map window and set up conversion factors */
            D_set_cur_wind (MAP.name);
            G_get_set_window(&window) ;
            D_get_screen_window(&t, &b, &l, &r) ;
            screen_y  = (t + b) / 2 ;
            screen_x  = (l + r) / 2 ;
            D_do_conversions(&window, t, b, l, r) ;

            /* get a point from the mouse */
            R_get_location_with_pointer(&screen_x, &screen_y, &button);

            /* exit if user hit left mouse button */
            if(button == RIGHTB)
            {
                D_set_cur_wind (ORIG.name);
                fprintf (stdout,"Use 'd.frame -e' to remove left over frames\n");
                return(0) ;
            }

            /* convert to (easting,northing) coordinates */
            cur_uy = D_d_to_u_row((double)screen_y);
            cur_ux = D_d_to_u_col((double)screen_x);

            if (cur_ux > window.east || cur_ux < window.west ||
                cur_uy > window.north || cur_uy < window.south)
            {
                D_set_cur_wind (STA.name);
                R_standard_color(D_translate_color(DEFAULT_BG_COLOR));
                D_erase_window();
                R_standard_color(D_translate_color("red"));
                DrawText(25,1,1,"OUTSIDE CURRENT WINDOW");
                R_stabilize();
                button = LEFTB;
            }
            else
            {
                /* print "earth" coords. and category info. in status window */
                D_set_cur_wind (STA.name);
                What(old_mapname,old_mapset,window,cur_ux,cur_uy);
            }

    }   while (button != MIDDLEB);

        /* display mouse-menu in mouse-menu window */
        D_set_cur_wind (MOU.name);
        R_standard_color(D_translate_color(DEFAULT_BG_COLOR));
        D_erase_window();
        R_standard_color(D_translate_color("red"));
        DrawText(25,1,1,"GRASS PROGRAM: profile");
        R_standard_color(D_translate_color(DEFAULT_FG_COLOR));
        DrawText(15,3,1,"MOUSE   | Left:   Where am I?");
#ifdef ANOTHER_BUTTON
        DrawText(15,4,1,"BUTTON  | Right:  Set SECOND point\n");
        DrawText(15,5,1,"MENU    | Middle: Quit this");
#else
        DrawText(15,4,1,"BUTTON  | Middle: Set SECOND point");
        DrawText(15,5,1,"MENU    | Right:  Quit this\n");
#endif
	R_stabilize();

        /* move graphics position to first point chosen */
        R_move_abs(screen_x, screen_y) ;
        cur_screen_x = screen_x ;
        cur_screen_y = screen_y ;

        /* LOOP to get second point of line */
        do
        {
            /* choose map window and set up conversion factors */
            D_set_cur_wind (MAP.name);
            G_get_window(&window) ;
            D_get_screen_window(&t, &b, &l, &r) ;
            D_do_conversions(&window, t, b, l, r) ;

            R_get_location_with_line(cur_screen_x,cur_screen_y,
                &screen_x, &screen_y, &button) ;
            uy = D_d_to_u_row((double)screen_y);
            ux = D_d_to_u_col((double)screen_x);
            if (ux > window.east || ux < window.west ||
                uy > window.north || uy < window.south)
            {
                D_set_cur_wind (STA.name);
                R_standard_color(D_translate_color(DEFAULT_BG_COLOR));
                D_erase_window();
                R_standard_color(D_translate_color("red"));
                DrawText(25,1,1,"OUTSIDE CURRENT WINDOW");
                button = LEFTB;
            }
            else
            {
                switch (button)
                {
                case LEFTB:
                    /* print "earth" coords. and category info. in status window */
                    D_set_cur_wind (STA.name);
                    What(old_mapname,old_mapset,window,ux,uy);
                    break;
                case MIDDLEB:
                    /* get profile data */
                    InitProfile(&profile,window,cur_uy,cur_ux,uy,ux);
                    if ( (err=ExtractProfile(&profile,old_mapname,old_mapset) )== -1 )
                    {
                        D_set_cur_wind (STA.name);
                        R_standard_color(D_translate_color(DEFAULT_BG_COLOR));
                        D_erase_window();
                        R_standard_color(D_translate_color("red"));
                        DrawText(25,1,1,"ERROR: end-point outside");
                        DrawText(25,2,1,"       of current window");
                    }
                    else if (err==-2)
                        G_fatal_error("Error opening cell-file");
                    else if (err==-3)
                        G_fatal_error("Error reading from cell-file");
                    else if (err==-4)
                        G_fatal_error("Mysterious window inconsistancy error");
                    else
                    {
                        /* draw profile line on cell-file */
                        black_and_white_line(screen_x,screen_y,cur_screen_x,cur_screen_y);

                        /* select letter for current profile label */
                        switch (CurrentWin)
                        {
                        case 0:
                                ltr[0]='A'; ltr[1]=0; break;
                        case 1:
                                ltr[0]='B'; ltr[1]=0; break;
                        case 2:
                                ltr[0]='C'; ltr[1]=0; break;
                        case 3:
                                ltr[0]='D'; ltr[1]=0; break;
                        default:
                                ltr[0]='?'; ltr[1]=0; break;
                        }

                        /* plot label in black */
                        text_height=(int)(0.03*(b-t));
                        text_width=(int)(0.03*(r-l));
                        D_set_cur_wind (MAP.name);
                        R_move_abs(screen_x,screen_y);
                        if (screen_x<=cur_screen_x && screen_y>=cur_screen_y)
                            R_move_rel(-(text_width+2),(text_height+2));
                        else if (screen_x<cur_screen_x && screen_y<=cur_screen_y)
                            R_move_rel(-(text_width+2),2);
                        else if (screen_x>cur_screen_x)
                            R_move_rel(3,0);
                        R_standard_color(D_translate_color(DEFAULT_BG_COLOR));
                        R_text_size(text_width,text_height);
                        R_text(ltr);
                        R_standard_color(D_translate_color(DEFAULT_FG_COLOR));

                        /* plot label in white */
                        R_move_abs(screen_x,screen_y);
                        if (screen_x<=cur_screen_x && screen_y>=cur_screen_y)
                            R_move_rel(-(text_width+2),(text_height+2));
                        else if (screen_x<cur_screen_x && screen_y<=cur_screen_y)
                            R_move_rel(-(text_width+2),2);
                        else if (screen_x>cur_screen_x)
                            R_move_rel(3,0);
                        R_move_rel(1,1);
                        R_text(ltr);
                        R_standard_color(D_translate_color(DEFAULT_BG_COLOR));

                        /*length = hypot(cur_ux - ux, cur_uy - uy);*/

                        /* tell user about profile being plotted */
                        D_set_cur_wind (STA.name);
                        R_standard_color(D_translate_color(DEFAULT_BG_COLOR));
                        D_erase_window();
                        R_standard_color(D_translate_color("red"));
                        DrawText(25,1,1,"PLOTTING PROFILE");

                        /* plot profile data in profile window */
                        D_set_cur_wind (profiles[CurrentWin++].name);
                        /* dump profile if requested */
                        if (doplot)
                            WriteProfile(old_mapname, old_mapset,
                                    plotfile->answer, ltr[0], &profile);
                        PlotProfile(profile,ltr,min,max);
                        if (CurrentWin > 3) CurrentWin = 0;

                        cur_screen_x = screen_x;
                        cur_screen_y = screen_y;
                        cur_ux = ux ;
                        cur_uy = uy ;
                        break;
                    }
                default:
                        break;
                }
            }
	    R_stabilize();
        }   while (button != RIGHTB && button != MIDDLEB);

        /* display mouse-menu in mouse-menu window */
        D_set_cur_wind (MOU.name);
        R_standard_color(D_translate_color(DEFAULT_BG_COLOR));
        D_erase_window();
        R_standard_color(D_translate_color("red"));
        DrawText(25,1,1,"GRASS PROGRAM: profile");
        R_standard_color(D_translate_color(DEFAULT_FG_COLOR));
        DrawText(15,3,1,"MOUSE   | Left:   DO ANOTHER");
#ifdef ANOTHER_BUTTON
        DrawText(15,4,1,"BUTTON  | Right:  CLEAR DISPLAY");
        DrawText(15,5,1,"MENU    | Middle: QUIT");
#else
        DrawText(15,4,1,"BUTTON  | Middle: CLEAR DISPLAY");
        DrawText(15,5,1,"MENU    | Right:  QUIT");
#endif
	R_stabilize();

        R_get_location_with_pointer(&screen_x, &screen_y, &button) ;
        if (button == RIGHTB)
        {
            D_set_cur_wind (ORIG.name);
            fprintf (stdout, "Use 'd.frame -e' to remove left over frames\n");
            return(0);
        }
        else if (button == MIDDLEB)
        {
            D_set_cur_wind (MAP.name);
            Derase(DEFAULT_BG_COLOR) ;
            myDcell (d_mapname, d_mapset, 1);
            for (i=0; i<=3; i++)
            {
                D_set_cur_wind (profiles[i].name);
                Derase(DEFAULT_BG_COLOR) ;
            }
            CurrentWin=0;
        }
        else
        {
            free(profile.ptr);
        }
    }

    return 0;
}

void myDcell (char *name, char *mapset, int overlay)
{
    int fd, i, t, b, l, r, code;
    CELL *cell;
    struct Colors clr;
    
    D_setup (!overlay);

    D_get_screen_window (&t, &b, &l, &r);

    D_cell_draw_setup (t, b, l, r);

    cell = G_allocate_c_raster_buf();

    if ((fd = G_open_cell_old (name, mapset)) < 0)
        G_fatal_error("%s: Couldn't open raster <%s@%s>",
                            G_program_name(), name, mapset);

    if (G_read_colors (name, mapset, &clr) < 0)
        G_fatal_error("%s: Couldn't read color table for <%s@%s>",
                            G_program_name(), name, mapset);
    D_set_colors (&clr);

    D_set_overlay_mode (overlay);

    for (i = 0; i >= 0; )
    {
        code = G_get_c_raster_row(fd,cell,i);
        if(code < 0)
            break;
        else if (code == 0) {
            i++;
            continue;
        }
        i = D_draw_cell(i, cell, &clr);
    }
    
    /* Only one cell, always set the name */
    D_set_cell_name (G_fully_qualified_name(name, mapset));

    G_close_cell (fd);
    G_free (cell);
}

/* vim: softtabstop=4 shiftwidth=4 expandtab: */
