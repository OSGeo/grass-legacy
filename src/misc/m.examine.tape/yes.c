#include <stdio.h>
yes()
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
