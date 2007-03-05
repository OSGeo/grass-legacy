#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <dirent.h>
#include <grass/gis.h>
#include <grass/display.h>
#include <grass/raster.h>
#include <grass/glocale.h>

static char *read_ftcap(void);

int main( int argc , char **argv )
{
        char *fonts, *ftfonts;
	int fonts_len;
	int found;
        char buf[1024];
	DIR *dirp;
	struct dirent *dp;
	struct GModule *module;
        struct Option *opt1, *opt2, *opt3;

	G_gisinit(argv[0]);

	module = G_define_module();
	module->keywords = _("display");
	module->description =
			_("Selects the font in which text will be displayed "
			"on the user's graphics monitor.");

        /* find out what fonts we have */
        fonts = NULL;
	fonts_len = 0;
        sprintf (buf, "%s/fonts", G_gisbase());
	if ((dirp = opendir(buf)) != NULL)
        {
		found = 0;
                while ((dp = readdir(dirp)) != NULL)
                {
			char name[16];

			if(dp->d_name[0] == '.')
				continue;

			if(!strstr(dp->d_name, ".hmp"))
				continue;

			strcpy(name, dp->d_name);
			*(strstr(name, ".hmp")) = '\0';

			fonts_len += strlen(name) + 1;
			fonts = (char *)G_realloc(fonts, fonts_len);
                        if (found)
			{
				strcat(fonts, ",");
                	        strcat(fonts, name);
			}
			else
				strcpy(fonts, name);
			found = 1;
                }
                closedir(dirp);
        }
	if ((ftfonts = read_ftcap()))
	{
		if(fonts)
			found = 1;
		else
			found = 0;
		fonts_len += strlen(ftfonts) + 1;
		fonts = (char *)G_realloc(fonts, fonts_len);
		if (found)
			strcat(fonts, ",");
		strcat(fonts, ftfonts);
		G_free(ftfonts);
	}

        opt1 = G_define_option();
        opt1->key        = "font";
        opt1->type       = TYPE_STRING;
        opt1->required   = NO;
	opt1->options    = fonts;
        opt1->answer     = "romans";
        opt1->description= _("Choose new current font");

	opt2 = G_define_option();
	opt2->key        = "path";
	opt2->type       = TYPE_STRING;
	opt2->required   = NO;
	opt2->description= _("Path to TrueType font including file name");
	opt2->gisprompt  = "old_file,file,font";

	opt3 = G_define_option();
	opt3->key        = "charset";
	opt3->type       = TYPE_STRING;
	opt3->required   = NO;
	opt3->answer     = "UTF-8";
	opt3->description= _("Character encoding");

        /* Initialize the GIS calls */
        G_gisinit(argv[0]);

        /* Check command line */
        if (G_parser(argc, argv))
                exit(EXIT_FAILURE);

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

	if (fonts)
		G_free(fonts);

        exit(EXIT_SUCCESS);
}

static char *
read_ftcap(void)
{
	char *capfile, file[4096];
	int fonts_count;
	int font_names_len;
	char *font_names;
	char buf[4096], ifont[128], ipath[4096];
	FILE *fp, *fp2;

	fp = NULL;
	if((capfile = getenv("GRASS_FT_CAP")))
	{
		if((fp = fopen(capfile, "r")) == NULL)
			G_warning("%s: Unable to read FreeType definition file; use the default", capfile);
	}
	if(fp == NULL)
	{
		sprintf(file, "%s/etc/freetypecap", G_gisbase());
		if((fp = fopen(file, "r")) == NULL)
		{
			G_warning("%s: No FreeType definition file", file);
			return NULL;
		}
	}

	font_names = NULL;
	font_names_len = 0;
	fonts_count = 0;

	while(fgets(buf, sizeof(buf), fp) && !feof(fp))
	{
		char *p;

		p = strchr(buf, '#');
		if(p)
			*p = 0;

		if(sscanf(buf, "%[^:]:%[^:]", ifont, ipath) != 2)
			continue;

		if((fp2 = fopen(ipath, "r")) == NULL)
			continue;
		fclose(fp2);

		font_names_len += strlen(ifont) + 1;
		font_names = (char *)G_realloc(font_names, font_names_len);

		if(fonts_count > 0)
		{
			strcat(font_names, ",");
			strcat(font_names, ifont);
		}
		else
			strcpy(font_names, ifont);

		fonts_count++;
	}

	fclose(fp);

	return font_names;
}
