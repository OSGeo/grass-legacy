
#include <stdio.h>
#include "pngdriver.h"

int main(int argc, char **argv)
{
	LIB_init(PNG_Driver(), argc, argv);

	return LIB_main(argc, argv);
}

