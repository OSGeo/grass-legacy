/*
**  Written by Dave Gerdes  5/1988
**  US Army Construction Engineering Research Lab
*/

#include "digit.h"

Help (Menu)
    struct Menu_head *Menu;
{
    char filename[1024];
    char file[1024];
    char buf[BUFSIZ];
    char nohelp[BUFSIZ];
    int line, key;
    FILE *fp;

    if (Menu == &M_main)
	Menu = &M_global;
    strcpy (nohelp, help_file_name ("nohelp"));
    strcpy (filename, help_file_name (Menu->name));

    if ((fp = fopen (filename, "r")) == NULL)
	if ((fp = fopen (nohelp, "r")) == NULL)
	{
	    _Clear_help ();
	    _Write_help (2, " No help available\n");
	    Show_help ();
	    sleep (3);
	    goto leave;
	}

    _Clear_help ();
    for (line = 1; fgets (buf, BUFSIZ, fp) != NULL && line < 13 ; line++)
    {
	dig_rmcr (buf);
	_Write_help (line, buf);
    }
    _Write_help (14, "  Press option for more help. <ESC> to end");
    fclose (fp);

    Show_help ();
    Clear_info ();
    {
	register int i, cnt;
	char buf[BUFSIZ], tmp[10];

	strcpy (buf, "Press one of: ");
	cnt = 0;
	for (i = 0 ; NULL !=  Menu->item[i].text ; i++)
	{
	    if (Menu->item[i].enabled)
	    {
		cnt++;
		sprintf (tmp, "%c ", (char) Menu->item[i].command);
		strcat (buf, tmp);
	    }
	}
	if (!cnt)
	    Write_info (2, "Only Global Menu options area available from this screen");
	else
	{
	    Write_info (2, buf);
	    Write_info (4, "   for more information.  Press the <ESC> key to leave Help");
	}
    }

    while ((key = help_get_key ()) != ESC)
    {
	_Clear_info ();
	if (!help_valid_key (key, Menu))
	{
	    sprintf (buf, "  '%c' is not a menu item", (char) key);
	    Write_info (2, buf);
	}
	else
	{
	    switch (key) {     /* nobody likes a space in a file name */
		case ' ': key = '_'; break;
		case '*': key = '@'; break;
	    }
	    sprintf (file, "%s._%c", filename, (char)key);
	    if ((fp = fopen (file, "r")) == NULL)
	    {
		    _Clear_info ();
		    Write_info (2, " No help available\n");
	    }
	    else
	    {
	    for (line = 1; fgets (buf, BUFSIZ, fp) != NULL && line < 5; line++)
		{
		    dig_rmcr (buf);
		    _Write_info (line, buf);
		}
	    fclose (fp);
	    }
	    _Info_refresh ();
	    Show_help ();
	}
    }
leave:
    Hide_help ();
    Clear_info ();
}

#define PATH "txt/DIGIT"

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
