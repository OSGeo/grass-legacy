#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include "gis.h"
#include "icon.h"
#include "local_proto.h"

int edit_icons (void)
{
    char name[40];
    char editor[400];
    char buf[30];
    ICON icon;
    FILE *fd;
    static char *tempfile = 0;
    int new ;
    int n;

    if (!tempfile) tempfile = G_tempfile ();

    while(ask_icon_any ("enter icon file to be created or modified", name,0))
    {
	new = G_find_file ("icons", name, G_mapset()) == 0 ;
	if (new || !get_icon (name, G_mapset(), &icon))
		icon.nrows = icon.ncols = 0;


	do fprintf (stdout,"which editor would you like to use? ");
	while (!G_gets(editor));

	G_strip (editor);
	if (*editor == 0) continue;
	strcat (editor, " ");
	strcat (editor, tempfile);
EDIT:
	fd = fopen (tempfile, "w");
	if (!fd)
	{
	    perror ("cant open tempfile");
	    sleep(5);
	    return 1;
	}
	write_icon (fd, &icon, 1);
	fclose (fd);

	if(system (editor)) sleep(5);

	fd = fopen (tempfile, "r");
	if (!fd)
	{
	    perror ("oops, can't read tempfile");
	    continue;
	}
	read_icon (fd, &icon);
	fclose (fd);

DISPOSE:
	G_clear_screen ();
	fprintf (stdout,"ICON: %s\n\n", name);
	divider (icon.ncols);
	write_icon (stdout, &icon, 0);
	divider (icon.ncols);

	fprintf (stdout,"1  looks ok - %s\n", new?"create":"update");
	fprintf (stdout,"2  looks bad - edit again\n");
	fprintf (stdout,"3  cancel\n");

	do
	{
	    fprintf (stdout,"> ");
	    if (!G_gets(buf)) goto DISPOSE;
	    n = atoi (buf);
	}
	while (n < 1 || n > 3) ;

	if (n == 3) continue;	/* cancel */
	if (n == 2) goto EDIT;

	put_icon (name, &icon);
	fprintf (stdout,"ICON %s %s\n", name, new?"created":"updated");
    }

    return 0;
}
