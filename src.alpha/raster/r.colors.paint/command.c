#include "stdio.h"
#include "options.h"

stash_away(pos, option)
int pos;
char *option;
{
	switch(pos)
	{
	case RESCALE:	if(!sscanf(option,"%s",do_rescale))
				return (-1);
			break;
	case OLDMAP:	if(!sscanf(option,"%s",old_name))
				return (-1);
			break;
	case NEWMAP:	if(!sscanf(option,"%s",new_name))
				return (-1);
			break;
	case DEV:	if(!sscanf(option,"%s",outdev_name))
				return (-1);
			break;
	case CTBL:	if(!sscanf(option,"%s",colortbl))
				return (-1);
			break;
	default:	fprintf(stderr,"Unknown option\n");
			return (-1);
			break;
	}
	return (0);
}
