#include "graphics.h"
#include "gis.h"

#define XSCALE  2.0
#define YSCALE  2.0

#define WRITE_STATUS \
    Write_status(cur_red, cur_grn, cur_blu, shift_incr, at_cat, hi_mode)

#define WRITE_CATS      Write_cats(categories, at_cat) 

interact(categories,colors,name,mapset) 
    struct Categories *categories ;
    struct Colors *colors ;
    char *name, *mapset;
{
    char buffer[128] ;
    int at_cat ;
    int hi_mode ;
    int hi_save_mode ;
    int shift_incr ;
    int cur_char ;
    int red_hi ;
    int grn_hi ;
    int blu_hi ;
    int cur_red ;
    int cur_grn ;
    int cur_blu ;
    int colors_changed;

    set_signals() ;
    red_hi = 0 ;
    grn_hi = 0 ;
    blu_hi = 0 ;
    hi_mode = 0 ;
    hi_save_mode = 0 ;

    colors_changed = 0;
    at_cat = 0 ;
    G_get_color (at_cat, &cur_red, &cur_grn, &cur_blu, colors) ;
    shift_incr = 10 ;

    Initialize_curses() ;

    WRITE_CATS ;

    Write_menu() ;

    WRITE_STATUS ;

/*  mark_category(at_cat, 1) ;*/

    while(1) 
    {
        R_flush() ;
        cur_char = getchar() & 0177 ;
        sprintf(buffer,"  %c",cur_char) ;
        Write_message(2, buffer) ;
        switch(cur_char)
        {
            case '*':
                Replot_screen();
                break;
            case 'Q':
                if (!colors_changed 
                || yes ("Colors changed but not saved","Ok to quit? (y/n)"))
                {
                     Close_curses() ;
                     return(0) ;
                }
                break;
            case 'D': case 'U':
            case 'd': case 'u':
                if (hi_mode && ! hi_save_mode)
                {
                    G_get_color (at_cat, &cur_red, &cur_grn, &cur_blu, colors) ;
                    D_reset_color( at_cat, cur_red, cur_grn, cur_blu);
                }
/*              mark_category(at_cat, 0) ;*/
                switch(cur_char)
                {
                    case 'd':
                        at_cat++ ;
                        break ;
                    case 'u':
                        at_cat += categories->num ;
                        break ;
                    case 'D':
                        at_cat += 10 ;
                        break ;
                    case 'U':
			if (categories->num > 10)
			    at_cat += categories->num - 9;
			else 
			    at_cat += categories->num - (categories->num - 1);
                        break ;
                }
                at_cat = at_cat % (categories->num + 1) ;

                if (hi_mode)
                {
                    cur_red = red_hi;
                    cur_grn = grn_hi;
                    cur_blu = blu_hi;
                    D_reset_color(at_cat, cur_red, cur_grn, cur_blu) ;
                    if (hi_save_mode)
                    {
                        G_set_color (at_cat, cur_red, cur_grn, cur_blu, colors) ;
                        colors_changed = 1;
                    }
                }
                else
                {
                    G_get_color (at_cat, &cur_red, &cur_grn, &cur_blu, colors) ;
                }

                WRITE_CATS ;
                WRITE_STATUS ;
/*              mark_category(at_cat, 1) ;*/
                break ;
            case 'r': case 'R': case 'g': case 'G': case 'b': case 'B':
                if (hi_mode)
                {
                    switch(cur_char)
                    {
                    case 'r': red_hi = shift_color(red_hi, -shift_incr) ; break ;
                    case 'R': red_hi = shift_color(red_hi,  shift_incr) ; break ;
                    case 'g': grn_hi = shift_color(grn_hi, -shift_incr) ; break ;
                    case 'G': grn_hi = shift_color(grn_hi,  shift_incr) ; break ;
                    case 'b': blu_hi = shift_color(blu_hi, -shift_incr) ; break ;
                    case 'B': blu_hi = shift_color(blu_hi,  shift_incr) ; break ;
                    }
                    cur_red = red_hi;
                    cur_grn = grn_hi;
                    cur_blu = blu_hi;
                    D_reset_color(at_cat, cur_red, cur_grn, cur_blu) ;
                    if (hi_save_mode)
                    {
                        G_set_color (at_cat, cur_red, cur_grn, cur_blu, colors) ;
                        colors_changed = 1;
                    }
                }
                else
                {
                    G_get_color (at_cat, &cur_red, &cur_grn, &cur_blu, colors) ;
                    switch(cur_char)
                    {
                    case 'r': cur_red = shift_color(cur_red, -shift_incr) ; break ;
                    case 'R': cur_red = shift_color(cur_red,  shift_incr) ; break ;
                    case 'g': cur_grn = shift_color(cur_grn, -shift_incr) ; break ;
                    case 'G': cur_grn = shift_color(cur_grn,  shift_incr) ; break ;
                    case 'b': cur_blu = shift_color(cur_blu, -shift_incr) ; break ;
                    case 'B': cur_blu = shift_color(cur_blu,  shift_incr) ; break ;
                    }
                    D_reset_color(at_cat, cur_red, cur_grn, cur_blu) ;
                    G_set_color (at_cat, cur_red, cur_grn, cur_blu, colors) ;
                    colors_changed = 1;
                }
                WRITE_STATUS ;
                break ;
            case 'i':
                shift_incr = shift_color(shift_incr, -1) ;
                WRITE_STATUS ;
                break ;
            case 'I':
                shift_incr = shift_color(shift_incr, 1) ;
                WRITE_STATUS ;
                break ;
            case '+':
		G_shift_colors (1, colors);
                if (hi_mode)
                {
                    cur_red = red_hi;
                    cur_grn = grn_hi;
                    cur_blu = blu_hi;
                    if (hi_save_mode)
                        G_set_color (at_cat, cur_red, cur_grn, cur_blu, colors) ;
                }
                D_set_colors(colors);
                colors_changed = 1;
                WRITE_STATUS ;
                break ;
            case '-':
		G_shift_colors (-1, colors);
                if (hi_mode)
                {
                    cur_red = red_hi;
                    cur_grn = grn_hi;
                    cur_blu = blu_hi;
                    if (hi_save_mode)
                        G_set_color (at_cat, cur_red, cur_grn, cur_blu, colors) ;
                }
                colors_changed = 1;
                D_set_colors(colors);
                WRITE_STATUS ;
                break ;
            case 'c':        /* Writeout color lookup table */
                colors_changed = 0;
                Clear_message() ;
                Write_message(2, "Writing color table      ") ;

                if (G_write_colors(name, mapset, colors) == -1)
                {
                    sleep(1);
                    Write_message(2, "Can't write color table  ") ;
                    sleep(2) ;
                }
    else
                Clear_message() ;
                break ;
            case 't':
                Clear_message() ;
                Write_message(1, "toggling new color table...") ;
                table_toggle(name, mapset, colors) ;
                /*Clear_message() ;*/
                if (hi_mode)
                {
                    cur_red = red_hi;
                    cur_grn = grn_hi;
                    cur_blu = blu_hi;
                    if (hi_save_mode)
                        G_set_color (at_cat, cur_red, cur_grn, cur_blu, colors) ;
                }
                D_set_colors(colors);
                colors_changed = 1;
                break ;
            case 'h': case 'H':
                if (hi_mode)
                {
                    G_get_color (at_cat, &cur_red, &cur_grn, &cur_blu, colors) ;
                    hi_mode = 0 ;
                    hi_save_mode = 0 ;
                }
                else
                {
                    cur_red = red_hi ;
                    cur_grn = grn_hi ;
                    cur_blu = blu_hi ;
                    hi_mode = 1 ;
                    if (cur_char == 'H')
                    {
                        G_set_color (at_cat, cur_red, cur_grn, cur_blu, colors) ;
                        hi_save_mode = 1 ;
                        colors_changed = 1;
                    }
                }
                D_reset_color(at_cat, cur_red, cur_grn, cur_blu) ;
                WRITE_STATUS ;

                break ;
            default:

                sprintf(buffer,"  %c - Unknown Command",cur_char) ;
                Write_message(2, buffer) ;
                break ;
        }
    }
}

shift_color(colr, shift)
{
    colr = colr + shift ;
    if (colr <   0) colr =   0 ;
    if (colr > 255) colr = 255 ;
    return(colr) ;
}

static
yes (msg1, msg2)
    char *msg1, *msg2;
{
    int c;

    Clear_message();
    Write_message (1, msg1);
    Write_message (2, msg2);

    while (1)
    {
        c = getchar() & 0177;
        switch (c)
        {
        case 'y':
        case 'Y':
            Clear_message();
            return 1;

        case 'n':
        case 'N':
            Clear_message();
            return 0;
        }
        putchar ('\7');
    }
}
