#include "cairodriver.h"

void Cairo_Client_Close(void)
{
	G_debug(1, "Cairo_Client_Close");
	write_image();
}
