#include "glob.h"

begin_divider()
{
	free_text_list (&divider_text);
	initialize_text_list (&divider_text);
}

add_divider (buf)

	char *buf;
{
	add_text (&divider_text, buf);
}
