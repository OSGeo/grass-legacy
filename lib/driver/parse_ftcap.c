#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <grass/gis.h>
#include <grass/glocale.h>
#include <grass/freetypecap.h>
#include "driverlib.h"

int font_exists(const char *name)
{
	char path[GPATH_MAX];
	char *p;
	FILE *fp;

	strcpy(path, name);

	p = strrchr(path, '|');
	if(p)
		*p = '\0';
	fp = fopen(path, "r");
	if (!fp)
		return 0;

	fclose(fp);
	return 1;
}

struct FT_CAP *parse_freetypecap(void)
{
	char *capfile, file[GPATH_MAX];
	char buf[GPATH_MAX], iname[128], ipath[GPATH_MAX];
	FILE *fp;
	int fonts_count = 0;
	struct FT_CAP *fonts = NULL;

	fp = NULL;
	if((capfile = getenv("GRASS_FT_CAP")))
	{
		if((fp = fopen(capfile, "r")) == NULL)
			G_warning(_("%s: Unable to read FreeType definition file; use the default"), capfile);
	}
	if(fp == NULL)
	{
		sprintf(file, "%s/etc/freetypecap", G_gisbase());
		if((fp = fopen(file, "r")) == NULL)
			G_warning(_("%s: No FreeType definition file"), file);
	}

	if(fp != NULL){
		while(fgets(buf, sizeof(buf), fp) && !feof(fp))
		{
			char *p;

			p = strchr(buf, '#');
			if(p)
				*p = 0;

			if(sscanf(buf, "%[^:]:%[^:]", iname, ipath) != 2)
				continue;

			if (!font_exists(ipath))
				continue;

			fonts = (struct FT_CAP *)G_realloc(fonts,
				(fonts_count + 1) * sizeof(struct FT_CAP));

			fonts[fonts_count].name = G_store(iname);
			fonts[fonts_count].path = G_store(ipath);

			fonts_count++;
		}
		fclose(fp);
	}

	fonts = (struct FT_CAP *)G_realloc(fonts, (fonts_count + 1) *
			sizeof(struct FT_CAP));
	fonts[fonts_count].name = NULL;
	fonts[fonts_count].path = NULL;

	return fonts;
}

void free_freetypecap(struct FT_CAP *ftcap)
{
	int i;

	if(ftcap == NULL)
		return;

	for(i=0; ftcap[i].name; i++)
	{
		G_free(ftcap[i].name);
		G_free(ftcap[i].path);
	}

	G_free(ftcap);

	return;
}
