#include <unistd.h>
#include "lock.h"

int unlock_file ( char *file)
{
    if (access (file,0) != 0)
	return 0;
    remove ( file );
    if (access (file,0) != 0)
	return 1;
    return -1;
}
