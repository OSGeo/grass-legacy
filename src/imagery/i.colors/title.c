#include "globals.h"

display_title()
{
    char line[100];
    char title[80];

    R_text_size (VIEW_TITLE->nrows-4, VIEW_TITLE->nrows-4);
    R_standard_color (WHITE);

    sprintf (line, "GROUP %s", group.name);
    I_get_group_title (group.name, title, sizeof title);
    if (*title)
    {
	strcat (line, " (");
	strcat (line, title);
	strcat (line, ")");
    }

    Text (line, VIEW_TITLE->top, VIEW_TITLE->bottom,
	VIEW_TITLE->left, VIEW_TITLE->right, 2, 1);
}
