/* get_mask given map layer - returns 1 ok, 0 error */

#include "gis.h"
#include "site.h"
#include "local_proto.h"

int get_mask (char *layer, char *mapset, char mask[], struct Categories *cats)
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
	    fprintf (stdout,"You have the option of specifying which categories in the\n");
	    fprintf (stdout,"mask layer <%s> ", layer);
	    fprintf (stdout,"will form the mask. The alternative is\n");
	    fprintf (stdout,"to mask all categories except no data.\n");
	    fprintf (stdout,"\nDo you wish to specify the categories? ");
	}
	fprintf (stdout,"(yes/no) ");
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
