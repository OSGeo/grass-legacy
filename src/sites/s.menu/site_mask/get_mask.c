/* get_mask given map layer - returns 1 ok, 0 error */

#include "gis.h"

get_mask (layer, mapset, mask, cats)
    char *layer;
    char *mapset;
    char mask[];
    struct Categories *cats;
{
    int  i;
    char buf[100];
    char answer[4];
    int print_prompt;

    mask[0] = 0;
    for (i = 1; i <= cats->num; i++)
	mask[i] = 1;
    print_prompt = 1;
    while (1)
    {
	if (print_prompt)
	{
	    new_mask_screen ();
	    printf("You have the option of specifying which categories in the\n");
	    printf("mask layer <%s> ", layer);
	    printf("will form the mask. The alternative is\n");
	    printf("to mask all categories except no data.\n");
	    printf("\nDo you wish to specify the categories? ");
	}
	printf("(yes/no) ");
	print_prompt = 1;
	if (!G_gets(buf))
	    continue;
	print_prompt = 0;
	if (sscanf(buf,"%1s",answer) != 1)
	    continue;
	if (*answer == 'n' || *answer == 'N')
	    return 1;
	if (*answer == 'y' || *answer == 'Y')
	    break;
    }

    for(i = 0; i <= cats->num; i++)
	mask[i] = 0;
    return makemask (cats, mask);
}
