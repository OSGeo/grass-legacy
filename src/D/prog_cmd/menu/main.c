/*
 *   Dmenu - uses lines in stdin as options to prompt user.
 *
 *   Usage:  Dmenu [backcolor] [textcolor] [dividercolor] [size]
 *           Dmenu [backcolor=name] [textcolor=name] [dividercolor=name] [size=num]
 *
 *   Lines beginning with:
 *    #      are comments and ignored
 *    .B     contain the background color
 *    .C     contain the text color
 *    .D     contain the line divider color
 *    .S     contain the text size (in pixles)
 *    .T     contain the panel's top edge
 *    .L     contain the panel's left edge
 *
 *   Of the remaining lines, the first is the menu name; the rest
 *   are the menu options.
 *
 *   Returns option number chosen.
 *
 */

#include <stdio.h>
#define MAIN
#include "options.h"
#define USAGE	"[backcolor=name] [textcolor=name] [dividercolor=name] [size=num]"

main(argc, argv) char **argv ;
{
    char buff[128] ;
    char *gets() ;
    char *cmd_ptr ;
    char *tmp ;
    int i ;
    int len ;
    extern int stash_away() ;
    char *malloc() ;

/* Initialize the GIS calls */
    G_gisinit(argv[0]) ;

/* Check command line */
    R_open_driver();
    set_default_options() ;

    if (D_parse_command(argc, argv, variables, n_variables, stash_away))
    {
	fprintf(stderr,"Usage: %s %s\n", argv[0], USAGE) ;
	R_close_driver() ;
	exit(-1) ;
    }

/* Read the options */
    i = 0 ;
    while (gets(buff) != NULL)
    {
	if (*buff == '#')
		continue ;
	if (*buff == '.')
	{
	    for(cmd_ptr=buff+2; *cmd_ptr==' ' && *cmd_ptr != NULL; cmd_ptr++) ;
	    switch (buff[1] & 0x7F)
	    {
	    case 'F':   /* font */
		    R_font(cmd_ptr) ;
		    break ;
	    case 'B':   /* background color */
		    backcolor = D_translate_color(cmd_ptr) ;
		    break ;
	    case 'C':   /* text color */
		    textcolor = D_translate_color(cmd_ptr) ;
		    break ;
	    case 'S':   /* size */
		    sscanf(cmd_ptr, "%d", &size) ;
		    break ;
	    case 'T':   /* top edge */
		    sscanf(cmd_ptr, "%d", &top) ;
		    top = 100 - top ;
		    break ;
	    case 'L':   /* left edge */
		    sscanf(cmd_ptr, "%d", &left) ;
		    break ;
	    default:
		    break ;
	    }
	}
	else
	{
	    len = strlen(buff) ;
	    tmp = malloc(len+1) ;
	    strcpy(tmp, buff) ;
	    options[i++] = tmp ;
	}
    }

    options[i] = NULL ;
    if (i<2)
    {
	fprintf(stderr,"ERROR: Must be a title and at least one option\n") ;
	R_close_driver();
	exit(-1) ;
    }

    i = D_popup(
	    backcolor ,
	    textcolor ,
	    dividercolor ,
	    top,      /* The col of the top left corner */
	    left,     /* The row of the top left corner */
	    size,     /* The size of the characters in pixles */
	    options) ;/* The text */
    R_close_driver();

/* Provide the result to standard output */
    printf("%d\n", i) ;
}
