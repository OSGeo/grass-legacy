#include "flag.h"

flag_destroy(flags)
FLAG *flags;
{
	free(flags->array[0]);
	free(flags->array);
	free(flags);
}
