/* Program: ps.icon
**
** This is a quick-and-dirty program used to create icons for use in the
** ps.map program.
**
** Author: Paul W. Carlson	May 1992
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "raster.h"
#include "display.h"
#include "gis.h"
#define MAIN
#include "ps_icon.h"

int 
main (int argc, char **argv)
{
    char window_name[64], buf[8];
    int t, b, l, r;
    struct Option *input_file;
    char *infile;
    
    /* initialize the GIS calls */
    G_gisinit (argv[0]);

    input_file = G_define_option();
    input_file->key = "input";
    input_file->type = TYPE_STRING;
    input_file->description = "icon filename";
    input_file->required = YES;

    if (G_parser(argc,argv))
                exit(1);
                
        
    infile = input_file->answer;

    /* open the icon file */
    open_icon_file(infile);

    /* set up display */
    R_open_driver();
    if (D_get_cur_wind(window_name)) G_fatal_error("No current frame");
    if (D_set_cur_wind(window_name)) G_fatal_error("Can't set current frame.");
    D_get_screen_window(&t, &b, &l, &r);
    R_set_window(t, b, l, r);
    R_close_driver();

    /* draw grid */
    draw_grid(t, b, l, r);

    /* if icon exists, show it */
    if (icon.file_exists)
    {
	show_icon();

  	/* do what with icon? */
	*buf = 0;
	while (*buf < '1' || *buf > '2')
	{
    	    G_clear_screen();
    	    fprintf (stdout,"\n\n\n\n\n\n");
	    fprintf (stdout,"\t\t\t1 - Exit without changing icon.\n\n");
	    fprintf (stdout,"\t\t\t2 - Delete this icon and make a new one.\n\n\n");
	    fprintf (stdout,"\t\t\tEnter selection (1 or 2) [1] >");
	    fgets(buf,8,stdin);
	    if (*buf == 0) *buf = '1';
	    if (*buf < '1' || *buf > '2') fprintf (stdout,"\007");
	}
	if (*buf == '1') 
	{
    	    G_clear_screen();
	    exit(0);
	}
	if ((icon.fp = G_fopen_new("ps_icons", infile)) == NULL)
	{
	    fprintf(stderr, "Can't overwrite \"%s\" ps_icon file.\n", infile);
	    exit(-1);
	}
        draw_grid(t, b, l, r);
    }

    /* make an icon */
    while (1)
    {
    	make_icon();

  	/* do what with icon? */
	*buf = 0;
	while (*buf < '1' || *buf > '3')
	{
    	    G_clear_screen();
    	    fprintf (stdout,"\n\n\n\n\n\n");
	    fprintf (stdout,"\t\t\t1 - Save icon and exit.\n\n");
	    fprintf (stdout,"\t\t\t2 - Exit without saving icon.\n\n");
	    fprintf (stdout,"\t\t\t3 - Start this icon again.\n\n\n");
	    fprintf (stdout,"\t\t\tEnter selection (1, 2 or 3) [1] >");
	    fgets(buf,8,stdin);
	    if (*buf == 0) *buf = '1';
	    if (*buf < '1' || *buf > '3') fprintf (stdout,"\007");
	}
	if (*buf == '1')
	{
	    save_icon();
	    break;
	}
	if (*buf == '2')
	{
	    fclose(icon.fp);
    	    G_remove("ps_icons", infile);
	    break;
	}
    	draw_grid(t, b, l, r);
    }
    G_clear_screen();

    exit(0);
}
