#include "imagery.h"
#include "local_proto.h"
#include "vask.h"
#include <string.h>

#define NCOLS 3
#define NROWS 10

static int more ;

int ask_newfiles (FILE *fd, struct Ref *ref, char *group)
{
    char mapset[30];
    while (new_mapset (fd, mapset))
	if(!ask_newfiles_in_mapset (fd, mapset, ref, group))
	    return 0;
    return 1;
}

int new_mapset (FILE *fd, char *mapset)
{
    char buf[1024];
    if (fgets(buf, sizeof buf, fd) == NULL)
	return 0;
    return sscanf (buf, "#%s", mapset) == 1;
}

int ask_newfiles_in_mapset (FILE *fd, char *mapset, struct Ref *ref, char *group)
{
    char use[NROWS*NCOLS][3];
    char name[NROWS*NCOLS][30];
    int nfiles;
    int i;
    int repeat;
    char title[80];
    char instructions[80];
    char mp[80];
    char next[20];
    char next_line[80];
    int row, col, width;

    nfiles = NROWS * NCOLS;
    width = (79+NCOLS)/NCOLS;

    sprintf (instructions, "GROUP: %s", group);
    I_location_info (title, instructions);
    sprintf (instructions, "Please mark an 'x' by the files to be added in group [%s]", group);

    repeat = 1;
    while(repeat)
    {
	V_clear();
	V_line (0, title);
	V_line (2, instructions);
	sprintf (mp, "MAPSET: %s", mapset);
	V_line (4, mp);
	V_const (mapset, 's', 4, 8, 30);
	for (i=0; i < nfiles; i++)
	    use[i][0] = 0;
	for (i=0; i < nfiles && (repeat = get_name(fd,name[i])); i++)
	{
	    col = (i/NROWS)*width;
	    row = i%NROWS + 6;
	    V_const (name[i], 's', row, col+3, width-4);
	    V_ques (use[i], 's', row, col, 2);
	}
	if (i==0) break;
	if (more)
	{
	    if (repeat)
		strcat (mp, "  (continued on next page)");
	    else
		strcat (mp, "  (next mapset on next page)");
	}

	*next = 0;
	*next_line = 0;
	sprintf (next, "%s","___");
	sprintf (next_line, "%*s%*s ", 1, "Enter 'end' to end the add query mode: ",3,"");
	V_line (19, next_line);
	V_ques (next, 's', 19, 39, 3);

	V_intrpt_ok();
	if(!V_call())
	    return 0;

	for (i=0; i < nfiles; i++)
	    if (use[i][0])
		I_add_file_to_group_ref (name[i], mapset, ref);

	if (strcmp (next, "end") == 0)
	  break;
	else
	  *next = 0;
    }
    return 1;
}

int get_name (FILE *fd, char *name)
{
    char buf[200];
    long offset;

    more = 1;
    offset = ftell (fd);
    if (fgets(buf, sizeof buf, fd) == NULL)
    {
	more = 0;
	return 0;
    }
    if (*buf == '#')
    {
	fseek (fd, offset, 0);
	return 0;
    }
    return sscanf (buf, "%s", name) == 1;
}
