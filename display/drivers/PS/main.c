
#include <stdio.h>
#include "psdriver.h"

int main(int argc, char **argv)
{
	LIB_init(PS_Driver(), argc, argv);

	return LIB_main(argc, argv);
}

