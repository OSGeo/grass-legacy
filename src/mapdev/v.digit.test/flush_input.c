#include "keyboard.h"

int flush_keyboard (void)
{
    char buf[100];

    set_keyboard ();
    while (key_hit (buf))
	;
    unset_keyboard ();
    return 0;
}
