#include "glob.h"
#include "local_proto.h"

int begin_block (void)
{
	initialize_text_list (&block_text);
	return 0;
}

int add_block (char *buf)
{
	add_text (&block_text, buf);
	return 0;
}

int begin_block_title (void)
{
	initialize_text_list (&block_title_text);
	return 0;
}

int add_block_title (char *buf)
{
	add_text (&block_title_text, buf);
	return 0;
}

int end_block (void)
{
	int n;

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
	return 0;
}
