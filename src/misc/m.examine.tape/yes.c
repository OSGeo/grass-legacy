#include <stdio.h>
#include "local_proto.h"

int yes (void)
{
    char answer[300];

    while(1)
    {
	ask ("(y/n) ", answer, NULL);

	switch (*answer)
	{
	case 'y': case 'Y': return (1);
	case 'n': case 'N': return (0);
	}
    }
}
