#include "glob.h"

static need_lead_divider = 1;

begin_block()
{
	initialize_text_list (&block_text);
}

add_block (buf)

	char *buf;
{
	add_text (&block_text, buf);
}

begin_block_title()
{
	initialize_text_list (&block_title_text);
}

add_block_title (buf)

	char *buf;
{
	add_text (&block_title_text, buf);
}

end_block ()
{
	int n;
	char *buf;

	n = count_text (&divider_text) * 2 + count_text(&block_text)
		+ count_text (&block_title_text);

	if (lineno + n > pagelen)
		eject();
	else
		output_text (&block_title_text);

	output_text(&block_text);
	output_text(&divider_text);
	free_text_list (&block_text);
	free_text_list (&block_title_text);
}
