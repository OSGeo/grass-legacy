
#include "pngdriver.h"

void
Client_Open(void)
{
}

void
Client_Close(void)
{
	if (auto_write)
		write_image();
}

