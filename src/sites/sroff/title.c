#include "glob.h"

begin_title()
{
	free_text_list (&title_text);
	initialize_text_list (&title_text);
}

add_title (buf)

	char *buf;
{
	add_text (&title_text, buf);
}
