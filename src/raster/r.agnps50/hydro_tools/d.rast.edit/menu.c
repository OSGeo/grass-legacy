/*
   menus.c - popup menus
   
   Chris Rewerts, Agricultural Engineering, Purdue University
   May 1991

   Modified Dec 7, 1991  Chris Rewerts
   I changed the menu to add "d.vect" so that user can
   overlay a vector map on the file to be edited, this
   required the addition of a function in this file called
   "vect"

 */

#define GLOBAL
#include "edit.h"
char new_color[28];
char arrow_layer[128];

main_menu ()
{
    static char *options[] = {
	" MAIN MENU",
	"  edit",
	"  redraw",
	"  d.rast.zoom",
	"  d.rast.arrow",
	"  d.rast.num",
	"  d.vect",
	"  grid color",
	"  exit",
    NULL};
    int background_color;
    int text_color;
    int div_color;
    int answer;
    char line[256];

    background_color = D_translate_color ("indigo");
    text_color = D_translate_color ("white");
    div_color = D_translate_color ("blue");
    R_font ("romant");

    use_mouse ();

    for (;;)
    {
	answer = D_popup (
	    background_color,
	    text_color,
	    div_color,
	    80,
	    5,
	    3,
	    options
	    );
	switch (answer)
	{
	  case 1:
/* edit */
	    change_made = 0;
	    make_temporary_file ();
	    edit ();
	    if (change_made)
	    {
		make_new_cell_layer ();

		if (strcmp (current_name, orig_name) == 0)
		{
		    strcpy (current_name, new_name);
		    strcpy (current_mapset, user_mapset);
		}
		Dcell (current_name, current_mapset, 0);
	    }
	    use_mouse ();
	    break;
	  case 2:
/* redraw */
	    R_close_driver ();
	    R_open_driver ();
	    Dcell (current_name, current_mapset, 0);
	    use_mouse ();
	    break;
	  case 3:
/* zoom */
	    R_close_driver ();
	    G_system ("d.rast.zoom");
	    R_open_driver ();
	    use_mouse ();
	    break;
	  case 4:
/* arrow */
	    get_arrow_inputs ();
	    use_mouse ();
	    break;

	  case 5:
/* number */
	    R_close_driver ();
	    sprintf (line, "d.rast.num g=%s", grid_color_name);
	    G_system (line);
	    R_open_driver ();
	    use_mouse ();
	    break;

	  case 6:
/* vect */
	    R_close_driver ();
	    vect ();
	    R_open_driver ();
	    use_mouse ();
	    break;

	  case 7:
/* grid color */
	    color_menu ("SELECT GRID COLOR");
	    grid_color = D_translate_color (new_color);
	    strcpy (grid_color_name, new_color);
	    use_mouse ();
	    break;

	  case 8:
/* exit */
            if (change_made)
                Derase("black");
            else
	        Dcell (orig_name, orig_mapset, 0);
	    return (0);
	  default:
	    break;
	}
    }
}
/*---------------------------------------------------------------*/
vect()
{
    char line[120];
    char v_color[20];
    char *v_mapset;
    char map[80];
    int i;
    int ok = 1;
    int num;

    G_clear_screen();
    v_mapset=G_ask_vector_old("Enter name of vector map to be displayed", map);
    if(!strcmp(map, ""))
        return(0);
    if(!v_mapset)
    {
        printf("\n\nCould not access <%s>\n\n", map);
        sleep(2);
        return(0);
    }

    while(1)
    {
    G_clear_screen();
    printf("\n\n\nSelect color for displaying <%s>\n\n", map);
    printf("     1.  white          7. indigo\n");
    printf("     2.  red            8. violet\n");
    printf("     3.  orange         9. magenta\n");
    printf("     4.  yellow        10. brown\n");
    printf("     5.  green         11. gray\n");
    printf("     6.  blue          12. black\n\n");
    printf("     option -> ");
    ok = 1;
    G_gets(line);
    for( i = 0; i < strlen(line); i++)
    {
        if (!isdigit(line[i]))
            ok = 0;
    }
    if( !ok )
    {
        printf("\n\n\7enter a whole number between 1 and 12\n\n");
        sleep(2);
        continue;
    }
    sscanf(line, "%d", &num);
    if ((num < 1) || (num > 12))
    { 
        printf("\n\n\7enter a whole number between 1 and 12\n\n");
        sleep(2); 
        continue; 
    }
    break;
    }
    switch(num)
    {
        case 1:
            strcpy(v_color, "white");
            break;
        case 2:
            strcpy(v_color, "red");
            break;
        case 3:
            strcpy(v_color, "orange");
            break;
        case 4:
            strcpy(v_color, "yellow");
            break;
        case 5:
            strcpy(v_color, "green");
            break;
        case 6:
            strcpy(v_color, "blue");
            break;
        case 7:
            strcpy(v_color, "indigo");
            break;
        case 8:
            strcpy(v_color, "violet");
            break;
        case 9:
            strcpy(v_color, "magenta");
            break;
        case 10:
            strcpy(v_color, "brown");
            break;
        case 11:
            strcpy(v_color, "gray");
            break;
        case 12:
            strcpy(v_color, "black");
            break;
        default:
            strcpy(v_color, "white");
            break;
    }
    sprintf(line, "d.vect %s c=%s", map, v_color);
    G_system(line);
    return(0);
}
/*---------------------------------------------------------------*/
option_menu()
{
    static char *options[] = {
        " OPTIONS",
        "  grid color",
        "  exit",
        NULL };
    int background_color;
    int text_color;
    int div_color;
    int answer;

    background_color = D_translate_color("indigo");
    text_color = D_translate_color("white");
    div_color = D_translate_color("blue");
    R_font("romant");

   for(;;)
    {
        answer = D_popup(
               background_color,
               text_color,
               div_color,
               80,
               15,
               3,
               options
               );
    switch(answer)
    {
    case 1: 
/* grid color */
            color_menu("SELECT GRID COLOR");
            grid_color = D_translate_color(new_color);
            strcpy(grid_color_name, new_color);
            break;
    case 2:
/* exit */

           return(0);
    default:
           break;
    }
    return(0);
    }
}

/*---------------------------------------------------------------*/
color_menu(title)
char *title;
{

/* we vary the usage of the menu a bit to allow for a changeable
     title. We initialize with spaces then copy in the title later
  */ 
   static char *options[] = {
        "                    ",
        "      red",
        "      orange",
        "      yellow",
        "      green",
        "      blue",
        "      indigo",
        "      violet",
        "      gray",
        "      white",
        "      black",
        NULL };
    int background_color;
    int text_color;
    int div_color;
    int answer;

    if(strlen(title) > 20)
        error(1, "color_menu: title too long");
   
    strcpy(options[0], title);

    background_color = D_translate_color("indigo");
    text_color = D_translate_color("white");
    div_color = D_translate_color("blue");
    R_font("romant");

   for(;;)
    {
        answer = D_popup(
               background_color,
               text_color,
               div_color,
               80,
               25,
               3,
               options
               );
    switch(answer)
    {
    case 1: 
            strcpy(new_color, "red");
            break;
    case 2: 
            strcpy(new_color, "orange");
            break;
    case 3: 
            strcpy(new_color, "yellow");
            break;
    case 4: 
            strcpy(new_color, "green");
            break;
    case 5: 
            strcpy(new_color, "blue");
            break;
    case 6: 
            strcpy(new_color, "indigo");
            break;
    case 7: 
            strcpy(new_color, "violet");
            break;
    case 8: 
            strcpy(new_color, "gray");
            break;
    case 9: 
            strcpy(new_color, "white");
            break;
    case 10: 
            strcpy(new_color, "black");
            break;
    default:
           break;
    }
    return(0);
    }
}

/*---------------------------------------------------------------*/
map_type_menu()
{
    static char *options[] = {
        " ASPECT MAP TYPE",
        "  grass",
        "  agnps",
        "  answers",
        "  exit",
        NULL };
    int background_color;
    int text_color;
    int div_color;
    int answer;

    background_color = D_translate_color("indigo");
    text_color = D_translate_color("white");
    div_color = D_translate_color("blue");
    R_font("romant");

   for(;;)
    {
        answer = D_popup(
               background_color,
               text_color,
               div_color,
               80,
               15,
               3,
               options
               );
    switch(answer)
    {
    case 1: 
            strcpy(new_color, "grass");
            return(1);
    case 2:
            strcpy(new_color, "agnps");
            return(2);
    case 3:
            strcpy(new_color, "answers");
            return(3);
    case 4:
            return(-1);
    default:
            return(-1);
    }
    }
}
/*---------------------------------------------------------------*/
arrow_options()
{
    static char *options[] = {
        "SET PROGRAM OPTIONS?",
        "  NO:  use default options",
        "  YES: set options now",
        NULL };
    int background_color;
    int text_color;
    int div_color;
    int answer;

    background_color = D_translate_color("indigo");
    text_color = D_translate_color("white");
    div_color = D_translate_color("blue");
    R_font("romant");

   for(;;)
    {
        answer = D_popup(
               background_color,
               text_color,
               div_color,
               80,
               15,
               3,
               options
               );
    switch(answer)
    {
    case 1: 
            return(0);
    case 2:
            return(1);
    default:
           return(0);;
    }
    }
}
/*---------------------------------------------------------------*/

get_arrow_inputs()
{

    char line[198];
    char map_type[28], arrow_color[28], x_color[28], unknown_color[28];
    int m_type;

           m_type = map_type_menu();
           if(m_type < 1)
               return(0);
           strcpy(map_type, new_color);
           if(arrow_options())
               {
               arrow_map();
               strcpy(map_type, new_color);
               color_menu("COLOR FOR ARROWS");
               strcpy(arrow_color, new_color);
               color_menu("COLOR FOR X's");
               strcpy(x_color, new_color);
               color_menu("COLOR FOR ?'s");
               strcpy(unknown_color, new_color);
               }
           else
/* set defaults */
               {
               strcpy(arrow_layer, current_name);
               switch(m_type)
                   {
                   case 1:
                       strcpy(arrow_color, "green");
                       strcpy(x_color, "white");
                       strcpy(unknown_color, "red");
                       break;
                   case 2:
                       strcpy(arrow_color, "black");
                       strcpy(x_color, "white");
                       strcpy(unknown_color, "red");
                       break;
                   case 3:
                       strcpy(arrow_color, "green");
                       strcpy(x_color, "black");
                       strcpy(unknown_color, "white");
                       break;
                   default:
                       strcpy(arrow_color, "green");
                       strcpy(x_color, "white");
                       strcpy(unknown_color, "red");
                       break;
                  }
               }
           sprintf(line, 
           "d.rast.arrow map=%s type=%s grid_color=%s arrow=%s x=%s unk=%s",
           arrow_layer, map_type, grid_color_name, arrow_color, x_color, 
           unknown_color);
           R_close_driver();
           G_system(line);
           R_open_driver();
           return(0);

}

/*---------------------------------------------------------------*/
arrow_map()
{
    static char *options[] = {
        "USE DISPLAYED MAP AS INPUT?",
        "  NO:  enter other name now",
        "  YES: use displayed map",
        NULL };
    int background_color;
    int text_color;
    int div_color;
    int answer;

    background_color = D_translate_color("indigo");
    text_color = D_translate_color("white");
    div_color = D_translate_color("blue");
    R_font("romant");

   for(;;)
    {
        answer = D_popup(
               background_color,
               text_color,
               div_color,
               80,
               15,
               3,
               options
               );
    switch(answer)
    {
    case 1: 
            G_clear_screen();
            fprintf(stderr,
             "\n\n     +-------------------------------------------+\n");
            fprintf(stderr,
            "     |            Text input needed              |\n");
            fprintf(stderr, 
            "     +-------------------------------------------+\n\n");

            if 
            (G_ask_cell_old("Enter name of aspect map to use for arrows",
            arrow_layer) == NULL)
            {
                error(0, "no name given. using current layer.");
                strcpy(arrow_layer, current_name);
            }
            return(0);
    case 2:
            strcpy(arrow_layer, current_name);
            return(0);
    default:
           return(0);;
    }
    }
}
