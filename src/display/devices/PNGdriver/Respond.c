
#include "pngdriver.h"

void
Respond(void)
{
	if (auto_write)
		write_image();
}

