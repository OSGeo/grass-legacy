#include "glob.h"

int begin_title (void)
{
	free_text_list (&title_text);
	initialize_text_list (&title_text);

	return 0;
}

int add_title (char *buf)
{
	add_text (&title_text, buf);

	return 0;
}
