/* %W% %G% */
/* program to grow image one cell */

/* fake changes to demonstrate SCCS system */

#include "gis.h"

char *intro[] =
{
"GROW ",
" ",
"The grow command accepts an input map layer and creates an",
"output map layer by enlarging all continguous areas by one",
"cell (around the perimeter).  All added cells are assigned a",
"category value of one.",
" ",
"An option is available for complete binary output; otherwise,",
"nonzero category values for all cells in the input map layer",
"will be preserved.",
0
};

main()
{

    char command[1024];

	char name[40];
	char *mapset;
	int fd;

	char buf[100];
	char ans[2];

	char new_name[40];
	char *new_mapset;
	int new_fd;
	int i;

	G_gisinit("GROW");

	G_clear_screen();
	for (i=0; intro[i]; i++)
	  printf("%s\n",intro[i]);


	mapset = G_ask_cell_old("Name of cell file to be grown",name);
	if (!mapset) exit(2);

	new_mapset = G_ask_cell_new("Name of result cell file",new_name);
	if (!new_mapset) exit(2);

    while (1)
	{

		do
		{
            printf("\nBinary output map? [y/n]  ");
        }
		while (!G_gets(buf));

		if (sscanf(buf,"%1s",ans) != 1)
			continue;

        if (*ans == 'Y') *ans = 'y';
        if (*ans == 'N') *ans = 'n';

        if (*ans != 'y' && *ans != 'n')
		{
			printf(" \n");
			printf("Please answer y or n \n");
			continue;
        }
        break;
    }

    if (*ans == 'y')
        sprintf(command, "Ggrow -b %s %s", name, new_name);
    else
        sprintf(command, "Ggrow %s %s", name, new_name);

	system(command);

}
