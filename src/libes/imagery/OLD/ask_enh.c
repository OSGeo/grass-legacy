#include "imagery.h"

/******************************************************************************
* I_ask_enhancement (title, nfiles, ncolors, r,g,b, histo, get_name, get_histo)
*     char *title
*     int nfiles
*     int *r, *g, *b
*     sturct Histogram *histo[3]
*     int get_name()
*     int get_histo()
*
* this routine prompts the user for color-file assignment
* as well as color level and histogram configuration.
*
* parms:
*  title:      title to appear on first line of screen
*  nfiles:     number of files user can choose from
*  r,g,b       will be set to the file nummber for red,grn,blu respectively.
*  histo       will contain the (modfified) histograms for the red, green, blue
*              files respectively
*
*  get_name:   a routine to return a file name to display to the user
*              should copy the name for file n into display_name
*
*              get_name (n, display_name)
*                int n;
*                char display_name[];
*
*              n will be in the range 1 - nfiles
*           
*  get_histo:  a routine to return the histogram for file n
*
*              get_histo (n, histo)
*                int n;
*                struct Histogram *histo;
*          
*              n will be in the range 1 - nfiles
*
******************************************************************/

#define NFILES 12
#define MAX_DISPLAY 34

I_ask_enhancement (title, nfiles, ncolors, r,g,b, histo, get_name, get_histo)
    char *title;
    int *r, *g, *b;
    struct Histogram *histo[3];
    int (*get_name)();
    int (*get_histo)();
{
    char display_name[NFILES][MAX_DISPLAY+1];
    char colors[NFILES][5];
    char index[NFILES];
    char text[3][200];
    char err_line[80];
    char msg1[80];
    char msg2[80];
    long min[3];
    long max[3];
    int levels[3];
    int rgb[3];
    char contrast_stretch[3];
    int line;
    int repeat;
    int offset;
    int i, f;
    char *c;
    int true_nfiles;

    static char *label[] = {"RED", "GREEN", "BLUE"};
    static char *used = "<<< r,g,b can only be specified once >>>";
    static char *huh =  "<<< please specify r,g,b only >>>";
    static char *divider =
"----------------------------------------------------------------------------";

    if (nfiles > NFILES)
    {
	nfiles = NFILES ;
	sprintf (msg1,"More than %d files. Will only use first %d",
	    NFILES, NFILES);
	G_warning (msg1);
    }

/**************************************************************
 * step 1 - setup
 *   set r,g,b to NONE
 *   intialize color levels
 */
    *r = *b = *g = -1;
    if (ncolors < 8)
	G_fatal_error ("Minimum of 8 colors required");
    I_init_color_levels (ncolors);
    I_get_color_levels (&levels[0], &levels[1], &levels[2]);

/**************************************************************
 * step 2 - get the file names to be displayed to user
 */

    f = 0;
    for (i = 0; i < nfiles; i++)
    {
	if ((*get_name)(i, msg1)) 
	{
	    sprintf (display_name[f], "%-.*s", MAX_DISPLAY, msg1);
	    index[f++] = i;
	}
    }
    if ((nfiles = f) <= 0) return 0;

/**************************************************************
 * step 3 - prompt for files names for r,g,b color componenets
 */


    V_clear();

    line = 1;
    V_line (line++, title);
    line++;

V_line (line++,
"Please indicate which files to use for the red, green, and blue components");
V_line (line++,
"of the image. You may leave any color out. You may specify more than one");
V_line (line++,
"color per file. However, each color may only be specifed once.");
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

	colors[i][0] = 0;
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

    switch (nfiles)
    {
    case 1:
	strcpy (colors[0], "rgb");
	break;
    case 2:
	strcpy (colors[0], "b");
	strcpy (colors[1], "g");
	break;
    case 3:
	strcpy (colors[0], "b");
	strcpy (colors[1], "g");
	strcpy (colors[2], "r");
	break;
    case 4:
	strcpy (colors[0], "b");
	strcpy (colors[1], "g");
	strcpy (colors[3], "r");
	break;
    default:
	strcpy (colors[1], "b");
	strcpy (colors[2], "g");
	strcpy (colors[4], "r");
	break;
    }

    do
    {
	rgb[0] = rgb[1] = rgb[2] = -1;

	if (*msg1 || *msg2)
	    strcpy (err_line, divider);
	else
	    *err_line = 0;
	I_v_exec();

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
			if (rgb[0] >= 0)
			    strcpy (msg1, used);
			else
			    rgb[0] = i;
			break;
		case 'g':
		case 'G':
			if (rgb[1] >= 0)
			    strcpy (msg1, used);
			else
			    rgb[1] = i;
			break;
		case 'b':
		case 'B':
			if (rgb[2] >= 0)
			    strcpy (msg1, used);
			else
			    rgb[2] = i;
			break;
		default:
			strcpy (msg2, huh);
		}
	    }
	}
    }
    while (*msg1 || *msg2) ;

/**************************************************************
 * step 4 - histograms
 *  get the histograms
 *  find the min,max
 */

    for (f = 0; f < 3; f++)
    {
	int n;
	min[f] = 0;
	max[f] = 0;
	if (rgb[f] == -1)
	    continue;

	(*get_histo) (index[rgb[f]], histo[f]);
	n = G_histogram_num (histo[f]);
	min[f] = G_get_histogram_cat (0, histo[f]);
	max[f] = G_get_histogram_cat (n-1, histo[f]);
    }

/**************************************************************
 * step 5 - prompt user for color levels and histogram configuration
 */

    *msg1 = *msg2 = 0;
    *err_line = 0;
    V_clear();
    line = 1;

    V_line (line++, title);
    line++;

    V_line (line++,"Please set the number of levels of intensity for each color");
    if (rgb[0] >= 0 || rgb[1] >= 0 || rgb[2] >= 0)
    V_line (line++,"and set the min,max display range from the histograms");
    line++;

    V_line (line++,
"       number         histogram ranges");
    V_line (line++,
"      of color        new         current");
    V_line (line++,
"       levels      min   max     (min,max)    file name");
    line++;

    for (f = 0; f < 3; f++)
	V_ques (&levels[f], 'i', line+f, 8, 4);

    for (f = 0; f < 3; f++)
    {
	char temp[80];
	char temp2[30];

	sprintf (temp,"%s:", label[f]);
	sprintf (text[f],"%-15s",temp);
	if (rgb[f] >= 0)
	{
	    sprintf (temp2,"(%ld,%ld)",min[f],max[f]);
	    sprintf (temp,"%18s %-10s  %s","",temp2,display_name[rgb[f]]);
	    strcat (text[f],temp);
	    if (min[f] == 0 && max[f] > 0)
		min[f] = 1;
	    V_ques (&min[f],    'l', line, 19, 4);
	    V_ques (&max[f],    'l', line, 25, 4);
	}
	V_line (line, text[f]);
	line++;
    }

    if (rgb[0] >= 0 || rgb[1] >= 0 || rgb[2] >= 0)
    {
	line++;
	V_line (line,"Use histogram contrast stretching (y/n): ___");
	V_ques (contrast_stretch, 's', line, 42, 1);
	V_const (msg2, 's', line, 45, 30);
    }
    strcpy (contrast_stretch,"y");

    line++;
    V_line (line++, err_line);
    V_line (line++, msg1);

    do
    {
	repeat = 0;
	if (*contrast_stretch == 'y' || *contrast_stretch == 'n')
	    *msg2 = 0;
	else
	{
	    strcpy (contrast_stretch,"n");
	    strcpy (msg2,"<-- changed to 'n'");
	    repeat = 1;
	}
	if (*msg1)
	    strcpy (err_line, divider);
	else
	    *err_line = 0;
	I_v_exec ();

	*msg1 = 0;
	if (levels[0] < 2 || levels[1] < 2 || levels[2] < 2)
	{
	    strcpy (msg1, "<<< intensity levels must be 2 or larger >>>");
	    repeat = 1;
	}
    }
    while (repeat);

/**************************************************************
 * adjust the histograms according to the request
 * zero the histograms below the min and above the max
 * set others to 1 if contrast stretching not desired
 */
    for (f = 0; f < 3; f++)
    {
	CELL cat;
	int n;
	if (rgb[f] == -1)
	    continue;

	n = G_get_histogram_num (histo[f]);
	for (i = 0; i < n; i++)
	{
	    cat = G_get_histogram_cat (i, histo[f]);
	    if (cat < min[f] || cat > max[f])
		G_set_histogram_count (cat, (long) 0, histo[f]);
	    else if (*contrast_stretch == 'n')
		G_set_histogram_count (cat, (long) 1, histo[f]);
	}
    }

/**************************************************************
 * final step
 */

    if (rgb[0] >= 0)
	*r = index[rgb[0]];
    if (rgb[1] >= 0)
	*g = index[rgb[1]];
    if (rgb[2] >= 0)
	*b = index[rgb[2]];

    I_set_color_levels (levels[0], levels[1], levels[2]);
}
