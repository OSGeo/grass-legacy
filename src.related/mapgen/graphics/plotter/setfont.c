#ifndef lint
static char *SCCSID = "@(#)setfont.c	USGS v.4.1";
#endif
/* initialize and control character fonts.  */
# define PLOTTER
# include "plotter.h"
# include "graphics.h"

/* define standard locations of fonts and default font */

extern char *font_dir[];

extern int error;
	static FONT *
font_base = (FONT *)0;
	/* open character definition file */
	static char *
fontname(name) char *name; {
	static char lname[FONTNAME];
 
	if (*name == '-') {  /* font from standard selections */
		strcpy(lname, font_dir[0]);
		if (*(name+1) == '\0')	strcat(lname, font_dir[1]);
		else			strcat(lname, ++name);
		name = lname;
	}
	return (name);
}
	/* initialize font */
	FONT *
setfont(name) char *name; {
	char *malloc();
	FILE *file;
	unsigned short size;
	FONT *font = (FONT *)0;

	name = fontname(name);
		/* see if named font in memory */
	for (font = font_base; font; font = font->next)
		if (!strcmp(font->name, name))
			return(font);
		/* not in memory, need to load */
	if (!(file = fopen(name, "r")) || !fread(&size, sizeof(size), 1, file))
		error = E_FNOFIL;
	else if (!(font = (FONT *)malloc(size + FONTNAME + sizeof(FONT *) +
			sizeof(float))))
		error = E_FALLOC;
	else {
		fseek(file, 0L, 0); /* rewind */
		if (fread(&(font->size), size, 1, file))
			strcpy(font->name, name);
		else {  /* failed in length! */
			free(font);
			font = (FONT *)0;
			error = E_FOPEN;
		}
	}
	if (file) fclose(file);
	if (font) {
		font->rsize = 21. / font->vect[0];
		font->next = font_base;
		font_base = font;
	}
	return (font);
}
