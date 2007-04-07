#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <dirent.h>
#include <grass/gis.h>
#include <grass/display.h>
#include <grass/raster.h>
#include <grass/glocale.h>

static char **fonts;
static int max_fonts;
static int num_fonts;

static void read_stroke_fonts(void);
static void read_freetype_fonts(void);
static char *make_font_list(void);
static void print_font_list(FILE *fp);

int main( int argc , char **argv )
{
	struct GModule *module;
	struct Option *opt1, *opt2, *opt3;
	struct Flag *flag1;

	G_gisinit(argv[0]);

	module = G_define_module();
	module->keywords = _("display");
	module->description =
			_("Selects the font in which text will be displayed "
			"on the user's graphics monitor.");

	/* find out what fonts we have */
	read_stroke_fonts();
	read_freetype_fonts();

	opt1 = G_define_option();
	opt1->key	= "font";
	opt1->type	= TYPE_STRING;
	opt1->required	= NO;
	opt1->options	= make_font_list();
	opt1->answer	= "romans";
	opt1->description = _("Choose new current font");

	opt2 = G_define_option();
	opt2->key	= "path";
	opt2->type	= TYPE_STRING;
	opt2->required	= NO;
	opt2->description = _("Path to TrueType font including file name");
	opt2->gisprompt	= "old_file,file,font";

	opt3 = G_define_option();
	opt3->key	= "charset";
	opt3->type	= TYPE_STRING;
	opt3->required	= NO;
	opt3->answer	= "UTF-8";
	opt3->description = _("Character encoding");

	flag1 = G_define_flag();
	flag1->key	= 'l';
	flag1->description = _("List fonts");

	G_gisinit(argv[0]);

	if (G_parser(argc, argv))
		exit(EXIT_FAILURE);

	if (flag1->answer)
	{
		print_font_list(stdout);
		exit(EXIT_SUCCESS);
	}

	/* load the font */
	if (R_open_driver() != 0)
		G_fatal_error (_("No graphics device selected"));

	if (opt2->answer)
		R_font(opt2->answer);
	else
	if (opt1->answer)
		R_font(opt1->answer);

	if (opt3->answer)
		R_charset(opt3->answer);

	/* add this command to the list */
	D_add_to_list(G_recreate_command());
	R_close_driver();

	exit(EXIT_SUCCESS);
}

static void read_stroke_fonts(void)
{
	char buf[GPATH_MAX];
	DIR *dirp;

	sprintf (buf, "%s/fonts", G_gisbase());
	dirp = opendir(buf);

	if (!dirp)
		return;

	for (;;)
	{
		struct dirent *dp = readdir(dirp);
		char *name;

		if (!dp)
			break;

		if(dp->d_name[0] == '.')
			continue;

		if (!strstr(dp->d_name, ".hmp"))
			continue;

		name = G_store(dp->d_name);
		*(strstr(name, ".hmp")) = '\0';

		if (num_fonts >= max_fonts)
		{
			max_fonts += 20;
			fonts = G_realloc(fonts, max_fonts * sizeof(char *));
		}

		fonts[num_fonts++] = name;
	}

	closedir(dirp);
}

static void read_freetype_fonts(void)
{
	char *capfile;
	FILE *fp = NULL;

	capfile = getenv("GRASS_FT_CAP");
	if (capfile)
	{
		fp = fopen(capfile, "r");
		if(!fp)
			G_warning("%s: Unable to read FreeType definition file; use the default", capfile);
	}

	if (!fp)
	{
		const char *gisbase = G_gisbase();

		capfile = G_malloc(strlen(gisbase) + 16 + 1);
		sprintf(capfile, "%s/etc/freetypecap", gisbase);

		fp = fopen(capfile, "r");
		if (!fp)
			G_warning("Missing FreeType definition file: %s", capfile);
	}

	if (!fp)
		return;

	for (;;)
	{
		char buf[1024], ifont[256], ipath[1024];
		FILE *fontfp;

		if (!fgets(buf, sizeof(buf), fp))
			break;

		if (buf[0] == '#')
			continue;

		if(sscanf(buf, "%[^:]:%[^:]", ifont, ipath) != 2)
			continue;

		fontfp = fopen(ipath, "r");
		if (!fontfp)
			continue;
		fclose(fontfp);

		if (num_fonts >= max_fonts)
		{
			max_fonts += 20;
			fonts = G_realloc(fonts, max_fonts * sizeof(char *));
		}

		fonts[num_fonts++] = G_store(ifont);
	}

	fclose(fp);
}

static char *make_font_list(void)
{
	char *list, *p;
	int len = 0;
	int i;

	for (i = 0; i < num_fonts; i++)
		len += strlen(fonts[i]) + 1;

	list = G_malloc(len);
	p = list;

	for (i = 0; i < num_fonts; i++)
	{
		char *q;

		if (i > 0)
			*p++ = ',';

		for (q = fonts[i]; *q; )
			*p++ = *q++;
	}

	*p++ = '\0';

	return list;
}

static void print_font_list(FILE *fp)
{
	int i;

	for (i = 0; i < num_fonts; i++)
		fprintf(fp, "%s\n", fonts[i]);
}

