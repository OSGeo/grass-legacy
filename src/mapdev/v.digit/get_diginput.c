#include "ginput.h"
#include "keyboard.h"
#include "local_proto.h"

int get_diginput (int *x, int *y)
{
    char buf[10];
    int hit;
    int key;
    int button;

    hit = key = 0;
#ifndef USE_KEYS
    if (digdevice.buttons < 5) 
#endif
    {
	set_keyboard();
	
	if ((hit = key_hit (buf)))
	{
	    buf[1] = 0;

	    if ( (digdevice.buttonstart == 0) && (buf[0] == '0') )
	    {
		key = 1;
	    }
	    else
	    {
		sscanf (buf, "%x", &key);
		if (key < 1 || key > 9)
		    hit = 0;

		key =  key + (1 - digdevice.buttonstart);
	    }
	}
	unset_keyboard();
    }


    if ((button = dig_input(x, y)) < 0)
	D_ask_if_err();


    if (hit)
	button = key;
   
    return button;
}
