#include "glob.h"

int begin_divider (void)
{
	free_text_list (&divider_text);
	initialize_text_list (&divider_text);

	return 0;
}

int add_divider (char *buf)
{
	add_text (&divider_text, buf);

	return 0;
}
