/*
**  Written by Dave Gerdes  5/1988
**  US Army Construction Engineering Research Lab
*/
/*  Last modified by R.L. Glenn  12/1991
**  USDA Tech. Infor. Sys. Division
*/

#include "digit.h"
#include "menus.i"
#include "popup.h"

Help (Menu)
    struct Menu_head *Menu;
{
    char filename[1024];
    char file[1024];
    char buf[BUFSIZ];
    char nohelp[BUFSIZ];
    int line, got_help=1;
    FILE *fp;

    if (Menu == &M_main)
	Menu = &M_global;
    strcpy (nohelp, help_file_name ("nohelp"));
    strcpy (filename, help_file_name (Menu->name));

    if ((fp = fopen (filename, "r")) == NULL)
	if ((fp = fopen (nohelp, "r")) == NULL)
	{
            sprintf(buf," No help available\n");
            message[0] = (char *) malloc (strlen (buf) + 1);
            sprintf(message[0],"%s", buf);
            message[1] = '\0';

            Dchoose(MEN.name) ;
            popup_messg( "info_help", 1) ;

	    sleep (3);
	    got_help = 0;
	    goto QUIT_IT;
	}

    G_clear_screen ();
    sprintf(buf,"See terminal for information ");
    message[0] = (char *) malloc (strlen (buf) + 1);
    sprintf(message[0],"%s", buf);
    message[1] = '\0';

    Dchoose(MEN.name) ;
    popup_messg( "info_help", 1) ;

    for (line = 1; fgets (buf, BUFSIZ, fp) != NULL && line < 13 ; line++)
    {
	dig_rmcr (buf);
	fprintf(stderr,"%s\n", buf);
    }
    fprintf(stderr, "  Select option for more help, Quit to end\n");
    fclose (fp);

QUIT_IT:
    ready(); 
    G_clear_screen ();
    return(got_help);
}

Help_item (Menu, key)
    struct Menu_head *Menu;
    int *key;
{
    char filename[1024];
    char file[1024];
    char buf[BUFSIZ];
    int line;
    FILE *fp;

    G_clear_screen ();
    if (Menu == &M_main)
	Menu = &M_global;
    strcpy (filename, help_file_name (Menu->name));

    sprintf (file, "%s.%d", filename, key);
    if ((fp = fopen (file, "r")) == NULL)
	{
	G_clear_screen ();
	fprintf(stderr, " No help available\n");
	}
    else
	{
	for (line = 1; fgets (buf, BUFSIZ, fp) != NULL && line < 5; line++)
	  {
	  dig_rmcr (buf);
	  fprintf(stderr,"%s\n", buf);
	  }
	fclose (fp);
	}
    ready();
    G_clear_screen ();
}

#define PATH "txt/SDIGIT"

char *
help_file_name (str)
    char *str;
{
    static char name[BUFSIZ];

    sprintf (name, "%s/%s/%s", G_gisbase(), PATH, str);
    return (name);
}

help_valid_key (key, Menu)
    int key;
    struct Menu_head *Menu;
{
    register int i;

    for (i = 0 ; Menu->item[i].text != NULL ; i++)
    {
	if (Menu->item[i].command == key)
	    return (1);
    }
    return (0);
}
/* these are defunct for now.  maybe later they can be used for extended help */
Help_main () {}
Help_global () {}
Help_digitize () {}
Help_edit () {}
Help_label () {}
Help_window () {}
Help_custom () {}
Help_tool () {}
Help_display() {}
Help_debug() {}
Help_color() {}
