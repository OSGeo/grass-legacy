/**********************************************************/
/*                                                        */
/*   s.label                                              */
/*   ALPHA program for GRASS 4.1                          */
/*                                                        */
/*   Latest version: 23 September 1994                    */
/*                                                        */
/*   by: Philip Verhagen                                  */
/*   Stichting RAAP                                       */
/*   Plantage Muidergracht 14                             */
/*   1018 TV Amsterdam                                    */
/*   THE NETHERLANDS                                      */
/*   motte@xs4all.nl                                      */
/*                                                        */
/*   s.label makes a paint-label file from a site list    */
/*   using the description as text; the user can specify  */
/*   all options for the label file. Optionally the user  */
/*   make a site list from a paint-label file             */
/*                                                        */
/**********************************************************/

#include "gis.h"

main (argc, argv)
int argc;
char **argv;

{

int i;
double east, north;
float E, N;
char dummy[80], text[80];
char *desc, *font;
char err_msg[200];
char *Mapset;
char LINE[80];

FILE *labels;
FILE *sites;

struct Option *Labelfile;
struct Option *Sitesfile;
struct Flag *Reverse;
struct Option *Xoffset;
struct Option *Yoffset;
struct Option *Reference;
struct Option *Font;
struct Option *PS_Font;
struct Option *Color;
struct Option *Size;
struct Option *Width;
struct Option *Hcolor;
struct Option *Hwidth;
struct Option *Bcolor;
struct Option *Border;
struct Option *Opaque;

G_gisinit(argv[0]);

Sitesfile = G_define_option();
Sitesfile->key = "sites";
Sitesfile->description = "Name of a site list";
Sitesfile->type = TYPE_STRING;
Sitesfile->required = YES;

Labelfile = G_define_option();
Labelfile->key = "label";
Labelfile->description = "Name of a paint-label file";
Labelfile->type = TYPE_STRING;
Labelfile->required = YES;

Reverse = G_define_flag();
Reverse->key = 'r';
Reverse->description = "Make sites from label-file";

Xoffset = G_define_option();
Xoffset->key = "xoffset";
Xoffset->description = "Offset label in x-direction";
Xoffset->type = TYPE_INTEGER;
Xoffset->answer = "";

Yoffset = G_define_option();
Yoffset->key = "yoffset";
Yoffset->description = "Offset label in y-direction";
Yoffset->type = TYPE_INTEGER;
Yoffset->answer = "";

Reference = G_define_option();
Reference->key = "reference";
Reference->description = "Reference position";
Reference->type = TYPE_STRING;
Reference->answer = "center";
Reference->options = "center,left,right,upper,lower";

Font = G_define_option();
Font->key = "font";
Font->description = "Font";
Font->type = TYPE_STRING;
Font->answer = "standard";
Font->options = "standard,cyrilc,gothgbt,gothgrt,gothitt,greekc,greekcs, grrekp,greeks,italicc,italiccs,italict,romanc,romancs,romand,romanp,romans, scriptc,scripts";

PS_Font = G_define_option();
PS_Font->key = "ps_font";
PS_Font->description = "PostScript font (for use with ps.map); N.B. screen display will be in standard font!";
PS_Font->type = TYPE_STRING;
PS_Font->options = "Helvetica,Helvetica-Bold,Helvetica-Oblique,Helvetica-BoldOblique,Times-Roman,Times-Bold,Times-Italic,Times-BoldItalic,Courier,Courier-Bold,Courier-BoldOblique,Symbol,AvantGarde-Book,AvantGarde-BookOblique,AvantGarde-Demi,AvantGarde-DemiOblique,Bookman-Demi,Bookman-DemiItalic,Bookman-Light,Bookman-LightItalic,Helvetica-Narrow,Helvetica-Narrow-Bold,Helvetica-Narrow-Oblique,Helvetica-Narrow-BoldOblique,NewCenturySchlbk-Roman,NewCenturySchlbk-Italic,NewCenturySchlbk-Bold,NewCenturySchlbk-BoldItalic,Palatino-Roman,Palatino-Italic,Palatino-BoldItalic,ZapfChancery-MediumItalic";

Size = G_define_option();
Size->key = "size";
Size->description = "Label size (in map-units)";
Size->type = TYPE_INTEGER;
Size->answer = "100";
Size->options = "1-1000";

Color = G_define_option();
Color->key = "color";
Color->description = "Text color";
Color->type = TYPE_STRING;
Color->answer = "black";
Color->options = "aqua,black,blue,brown,cyan,gray,green,grey,indigo,magenta, orange,purple,red,violet,white,yellow";

Width = G_define_option();
Width->key = "width";
Width->description = "Line width of text (only for p.map output)";
Width->type = TYPE_INTEGER;
Width->answer = "1";
Width->options = "1-100";

Hcolor = G_define_option();
Hcolor->key = "hcolor";
Hcolor->description = "Highlight color for text (only for p.map output)";
Hcolor->type = TYPE_STRING;
Hcolor->answer = "none";
Hcolor->options = "none,aqua,black,blue,brown,cyan,gray,green,grey,indigo,magenta, orange,purple,red,violet,white,yellow";

Hwidth = G_define_option();
Hwidth->key = "hwidth";
Hwidth->description = "Line width of highlight color (only for p.map output)";
Hwidth->type = TYPE_INTEGER;
Hwidth->answer = "0";
Hwidth->options = "0-100";

Bcolor = G_define_option();
Bcolor->key = "background";
Bcolor->description = "Background color";
Bcolor->type = TYPE_STRING;
Bcolor->answer = "none";
Bcolor->options = "none,aqua,black,blue,brown,cyan,gray,green,grey,indigo,magenta, orange,purple,red,violet,white,yellow";

Border = G_define_option();
Border->key = "border";
Border->description = "Border color";
Border->type = TYPE_STRING;
Border->answer = "none";
Border->options = "none,aqua,black,blue,brown,cyan,gray,green,grey,indigo,magenta, orange,purple,red,violet,white,yellow";

Opaque = G_define_option();
Opaque->key = "opaque";
Opaque->description = "Opaque to vector (only relevant if background color is selected)";
Opaque->type = TYPE_STRING;
Opaque->answer = "yes";
Opaque->options = "yes,no";

if (G_parser (argc, argv ) ) exit (-1 );

if (Reverse->answer != NULL) {

/* make paint-label file from site list */

	Mapset = G_find_file ("paint/labels", Labelfile->answer, "");
	if (Mapset == NULL) {
		sprintf (err_msg, "Paint-label file %s not found!\n", Labelfile->answer);
		G_fatal_error (err_msg );
		}

	labels = G_fopen_old ("paint/labels", Labelfile->answer, Mapset);
	sites = G_fopen_sites_new (Sitesfile->answer, G_mapset());

	i = 1;
	while (fgets (LINE, 80, labels) != NULL) {
		switch (i) {
		case 1:

/* sscanf does not accept type double for input ! */
/* only each first, second and 16th line are of interest */

			sscanf (LINE, "%s %f", dummy, &E);
			east = (double)E;
			i++;
			break;
		case 2:
			sscanf (LINE, "%s %f", dummy, &N);
			north = (double)N;
			i++;
			break;
		case 16:
			sscanf (LINE, "%s %s", dummy, text);
			i++;
			break;
		case 17:

/* after 17th line, write site and set count to 1 */

			G_put_site (sites, east, north, text);
			i = 1;
			break;
		default:
			i++;
		}
	}
			
	return (0);

}

else {

	Mapset = G_find_file ("site_lists", Sitesfile->answer, "");
	if (Mapset == NULL) {
		sprintf (err_msg, "Site list %s not found!\n", Sitesfile->answer);
		G_fatal_error (err_msg );
		}

	sites = G_fopen_sites_old (Sitesfile->answer, Mapset);
	labels = G_fopen_new ("paint/labels", Labelfile->answer);

/* check if font should be PostScript font */

	if (PS_Font->answer != NULL) G_strcpy (&font,PS_Font->answer);
	else G_strcpy (&font,Font->answer);

/* write label for each site using command-line specified options */

	while (G_get_site (sites, &east, &north, &desc) > 0 ) {
		fprintf (labels, "east: %f\n", east);
		fprintf (labels, "north: %f\n", north);
		fprintf (labels, "xoffset: %s\n", Xoffset->answer);
		fprintf (labels, "yoffset: %s\n", Yoffset->answer);
		fprintf (labels, "ref: %s\n", Reference->answer);
		fprintf (labels, "font: %s\n", &font);
		fprintf (labels, "color: %s\n", Color->answer);
		fprintf (labels, "size: %s\n", Size->answer);
		fprintf (labels, "width: %s\n", Width->answer);
		fprintf (labels, "hcolor: %s\n", Hcolor->answer);
		fprintf (labels, "hwidth: %s\n", Hwidth->answer);
		fprintf (labels, "background: %s\n", Bcolor->answer);
		fprintf (labels, "border: %s\n", Border->answer);
		fprintf (labels, "opaque: %s\n\n", Opaque->answer);
		fprintf (labels, "text: %s\n\n", desc);
		}
return (0);
	}
}
