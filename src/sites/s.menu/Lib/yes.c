#include "gis.h"

int yes (char *question)
{
	char answer[100];
	int ask;

	ask = 1;
	while (1)
	{
		if (ask)
		{
			fprintf (stdout,"\n%s ", question);
			ask = 0;
		}
		fprintf (stdout,"(y/n) ");
		if (!G_gets(answer))
		{
			ask = 1;
			continue;
		}
		if (strcmp(answer,"y") == 0)
			return 1;
		if (strcmp(answer,"yes") == 0)
			return 1;
		if (strcmp(answer,"Y") == 0)
			return 1;
		if (strcmp(answer,"YES") == 0)
			return 1;
		if (strcmp(answer,"n") == 0)
			return 0;
		if (strcmp(answer,"no") == 0)
			return 0;
		if (strcmp(answer,"N") == 0)
			return 0;
		if (strcmp(answer,"NO") == 0)
			return 0;
	}
}
