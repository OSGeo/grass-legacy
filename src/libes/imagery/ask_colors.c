#include <string.h>
#include "gis.h"
#include "imagery.h"
#include "vask.h"
static int show_color(char *,int,struct Ref *);

/******************************************************************************
* I_ask_ref_colors (title, ref)
*     char *title
*     struct Ref *ref
*
* this routine prompts the user for color-file assignment
*
* parms:
*  title:      title to appear on first line of screen
*  ref:        group (or subgroup) reference list
******************************************************************/

#define NFILES 12
#define MAX_DISPLAY 34

int I_ask_ref_colors (
    char *title,
    struct Ref *ref)
{
    char display_name[NFILES][MAX_DISPLAY+1];
    char colors[NFILES][5];
    char err_line[80];
    char msg1[80];
    char msg2[80];
    int r,g,b;
    int line;
    int offset;
    int i;
    char *c;
    int nfiles;

    static char *used = "<<< r,g,b can only be specified once >>>";
    static char *huh =  "<<< please specify r,g,b only >>>";
    static char *divider =
"----------------------------------------------------------------------------";

    nfiles = ref->nfiles;
/* only use first NFILES files */
    if (nfiles > NFILES)
	nfiles = NFILES ;

/**************************************************************
 * step 1 - setup
 *   erase colors beyond NFILES
 *   then get the current color assignments
 *   if none, set up the defaults
 */
    if (ref->red.n >= NFILES)
	ref->red.n = -1;
    if (ref->grn.n >= NFILES)
	ref->grn.n = -1;
    if (ref->blu.n >= NFILES)
	ref->blu.n = -1;

    I_init_ref_color_nums (ref);

    r = ref->red.n;
    g = ref->grn.n;
    b = ref->blu.n;


/**************************************************************
 * step 2 - get the file names to be displayed to user
 *          and set up the colors
 */

    for (i = 0; i < nfiles; i++)
    {
	sprintf (msg1, "%s in %s", ref->file[i].name, ref->file[i].mapset);
	sprintf (display_name[i], "%-.*s", MAX_DISPLAY, msg1);
	colors[i][0] = 0;
    }
    for (i = 0; i < nfiles; i++)
    {
	if (i == r) strcat (colors[i], "r");
	if (i == g) strcat (colors[i], "g");
	if (i == b) strcat (colors[i], "b");
    }

/**************************************************************
 * step 3 - prompt for files names for r,g,b color components
 */

    V_clear();

    line = 1;
    V_line (line++, title);
    line++;

V_line (line++,
"Please indicate which files to use for red, green, and blue colors.");
V_line (line++,
"You may leave any color out. You may specify more than one color per file.");
V_line (line++,
"However, each color may only be specifed once.");
    line++;
V_line (line++,
"For example, to get a full color image, specify r,g,b for 3 different files.");
V_line (line++,
"To get a grey scale image, specify rgb for a single file.");

    V_line (line++, divider);
    line++;

    offset = (1 - (nfiles-1)/6) * 20 + 1;
    for (i = 0; i < nfiles; i++)
    {
	int row,col;

	row = line + i%6;
	col = (i/6)*40+offset;
	V_ques (colors[i], 's', row, col,   4);
	V_const (display_name[i], 's', row, col+5, 34);
    }
    line += 6;

    *err_line = 0;
    *msg1 = 0;
    *msg2 = 0;
    V_line (line++,err_line);
    V_line (line++,msg1);
    V_line (line++,msg2);

ASK:
    do
    {
	r = g = b = -1;

	if (*msg1 || *msg2)
	    strcpy (err_line, divider);
	else
	    *err_line = 0;
	V_intrpt_ok();
	if (!V_call())
	    return 0;

	*msg1 = 0;
	*msg2 = 0;

	for (i = 0; i < nfiles; i++)
	{
	    for (c = colors[i]; *c; c++)
	    {
		switch (*c)
		{
		case ' ': break;
		case 'r':
		case 'R':
			if (r >= 0)
			    strcpy (msg1, used);
			else
			    r = i;
			break;
		case 'g':
		case 'G':
			if (g >= 0)
			    strcpy (msg1, used);
			else
			    g = i;
			break;
		case 'b':
		case 'B':
			if (b >= 0)
			    strcpy (msg1, used);
			else
			    b = i;
			break;
		default:
			strcpy (msg2,huh);
		}
	    }
	}
    }
    while (*msg1 || *msg2) ;

/**************************************************************
 * step 4 - verify
 */
    fprintf (stderr,"Colors assigned as follows:\n\n");
    show_color ("RED:    ", r, ref);
    show_color ("GREEN:  ", g, ref);
    show_color ("BLUE:   ", b, ref);
    if (!G_yes ("\nLook ok? ", 1))
	goto ASK;

/**************************************************************
 * step 5 - finish
 *  set the colors
 */
    ref->red.n = r;
    ref->grn.n = g;
    ref->blu.n = b;
  
    return 1;
}

static int show_color(char *label,int n,struct Ref *ref)
{
    fprintf (stderr,"%s", label);
    if (n < 0)
	fprintf (stderr,"none\n");
    else
	fprintf (stderr,"%s in %s\n", ref->file[n].name, ref->file[n].mapset);

	return 0;
}
