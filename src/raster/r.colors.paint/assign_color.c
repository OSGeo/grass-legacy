#include "gis.h"
#include "table.h"

assign_color(mapname,mapset,min_cat,max_cat,tble,outdev)
char *mapname;
char *mapset;
int min_cat, max_cat;
struct ctable *tble;
char outdev[];
{
	char buf[256];
	char *pnt;
	char *pnter;
	char *paintdrsh;
	char *pntdr;
	int R,G,B;
	int i,j;
	float red,grn,blu;
	char *Ppainter_name();
	struct ctable clist;
	struct Colors color;
	extern char *outdev_name ;
	
/** SET THE DEVICE ENVIROMENT VARAIBLE ************************/

	sprintf(buf,"%s/etc/paint",G_gisbase());
	pnt = buf;
	G_setenv("PAINT",pnt);
	pnt = G_getenv("PAINT");

	pnter = outdev_name;
	G_setenv("PAINTER",pnter);
	pnter = G_getenv("PAINTER");

	sprintf(buf,"%s/driver.sh/%s",G_getenv("PAINT"),G_getenv("PAINTER"));
	paintdrsh = buf;
	G_setenv("PAINT_DRIVER_SHELL",paintdrsh);
	paintdrsh = G_getenv("PAINT_DRIVER_SHELL");

	sprintf(buf,"%s/driver/%s",G_getenv("PAINT"),G_getenv("PAINTER"));
	pntdr = buf;
	G_setenv("PAINT_DRIVER",pntdr);
        pntdr = G_getenv("PAINT_DRIVER");
	
	Pconnect();

/** INIT THE COLOR STRUCTURE **********************************/

	G_init_colors(&color);

/** SET COLOR FOR CATEGORY 0 **********************************/

	Pcolorvalue(tble->color_nums[0],&red,&grn,&blu);
	R = red * 256;
	G = grn * 256;
	B = blu * 256;
	G_set_color ((CELL) 0,R,G,B,&color);

/** SET COLORS FOR CATEGORIES MIN_CAT TO MAX_CAT **************/

	j = 1;
	for ( i = min_cat; i <= max_cat; i++)
	{
		Pcolorvalue (tble->color_nums[j],&red,&grn,&blu);
		R = red * 256;
		G = grn * 256;
		B = blu * 256;
		G_set_color((CELL) i, R, G, B, &color);
		j++;
	}
	Pdisconnect();

/** WRITE COLOR STRUCTURE TO COLOR TABLE FILE *****************/

	G_write_colors(mapname,mapset,&color);
}
