#include <stdlib.h>
#include "flag.h"

int flag_destroy (FLAG *flags)
{
	free(flags->array[0]);
	free(flags->array);
	free(flags);

	return 0;
}
