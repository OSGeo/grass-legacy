#include "raster.h"
#include <stdio.h>
#include "gis.h"
#include "graph.h"

static int select_font(char *);
static int select_font_freetype(char *name);

/*!
 * \brief choose font
 *
 * Set current font to <b>font name</b>. Available fonts are:
 * 
 <table>
 <tr><td><b>Font Name</b></td><td><b>Description</b></td></tr>
 <tr><td>cyrilc </td><td> cyrillic</td></tr>
 <tr><td>gothgbt </td><td> Gothic Great Britain triplex</td></tr>
 <tr><td>gothgrt </td><td>  Gothic German triplex</td></tr>
 <tr><td>gothitt </td><td>  Gothic Italian triplex</td></tr>
 <tr><td>greekc </td><td> Greek complex</td></tr>
 <tr><td>greekcs </td><td> Greek complex script</td></tr>
 <tr><td>greekp </td><td> Greek plain</td></tr>
 <tr><td>greeks </td><td> Greek simplex</td></tr>
 <tr><td>italicc </td><td>  Italian complex</td></tr>
 <tr><td>italiccs </td><td> Italian complex small</td></tr>
 <tr><td>italict </td><td> Italian triplex</td></tr>
 <tr><td>romanc </td><td> Roman complex</td></tr>
 <tr><td>romancs </td><td> Roman complex small</td></tr>
 <tr><td>romand </td><td> Roman duplex</td></tr>
 <tr><td>romanp </td><td> Roman plain</td></tr>
 <tr><td>romans </td><td> Roman simplex</td></tr>
 <tr><td>romant </td><td> Roman triplex</td></tr>
 <tr><td>scriptc </td><td> Script complex</td></tr>
 <tr><td>scripts </td><td> Script simplex</td></tr>
 </table>
 *
 *  \param name
 *  \return int
 */

int R_font(char *name)
{
	if(!select_font (name))
	    select_font ("romand");

	return 0;
}

static int select_font(char *name)
{
	char filename[1024];
	int stat;

	sprintf (filename, "%s/fonts/%s", G_gisbase(), name);

	_send_ident(FONT) ;
	_send_text(filename) ;
	_get_int (&stat);

	return stat == 0;
}

int R_font_freetype(char *name) {
	return select_font_freetype(name);
}

static int select_font_freetype(char *name) {
	char filename[1024];
	int stat;

	_send_ident(FONT_FREETYPE) ;
	_send_text(name) ;
	_get_int (&stat);

	return stat == 0;
}

int R_charset(char *name) {
	int stat;

	_send_ident(CHARSET) ;
	_send_text(name) ;
	_get_int (&stat);

	return stat == 0;
}

int R_font_freetype_release() {
	int stat;

	_send_ident(FONT_FREETYPE_RELEASE) ;
	_get_int (&stat);

	return stat == 0;
}
