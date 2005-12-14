#include <stdio.h>
#include "gis.h"
#include "glocale.h"

#include "local_proto.h"

int graphics (FILE *infile)
{
	char buff[128+1];
	int got_new;

	got_new = G_getl2(buff, 128, infile);
	G_strip(buff);

	while(got_new)
	{
		switch (*buff & 0177)
		{
		case 't':
			do_text(buff) ;
			got_new = G_getl2(buff, 128, infile);
			G_strip(buff);
			break;
		case 's':
			do_size(buff) ;
			got_new = G_getl2(buff, 128, infile);
			G_strip(buff);
			break;
		case 'r':
			do_text_rotate(buff);
			got_new = G_getl2(buff, 128, infile);
			G_strip(buff);
			break;
		case 'p':
			got_new = do_poly(buff, infile);
			G_strip(buff);
			break;
		case 'c':
			do_color(buff) ;
			got_new = G_getl2(buff, 128, infile);
			G_strip(buff);
			break;
		case 'm':
			do_move(buff) ;
			got_new = G_getl2(buff, 128, infile);
			G_strip(buff);
			break;
		case 'd':
			do_draw(buff) ;
			got_new = G_getl2(buff, 128, infile);
			G_strip(buff);
			break;
		case 'i':
			do_icon(buff) ;
			got_new = G_getl2(buff, 128, infile);
			G_strip(buff);
			break;
		case '#':
			got_new = G_getl2(buff, 128, infile);
			G_strip(buff);
			break;
		default:
			fprintf (stderr, _("Don't understand: [%s]\n"), buff);
			got_new = G_getl2(buff, 128, infile);
			G_strip(buff);
			break;
		}
	}

	return 0;
}
