#ifndef lint
static char *SCCSID = "@(#)devlist.c	USGS v.4.4";
#endif
/* Device list module */
#include "plotter.h"

	/* list of currently supported devices */
	XY
*D4014(), *Dingena(), *Dhp7586(), *Dkong(), *Dc970(), *Ddmp60(), *Dpsqms(),
*Dhp7475(), *Dc906(), *Dgerber(), *Dranger(), *Dextdev(),
*Ddebug(), *Dbumdev(), *DGRASS(), *Dhpgl(), *Dc1077(), *Dcalhpgl();

	struct DEV_LIST
dev_list[] = {
		/* should leave these here */
	"debug",	Ddebug,	0, /* debug device mode */
	"exdebug",	Dextdev, 0, /* debug external device mode */
	"ranger",	Dranger, 0, /* range determination routine */
		/* selected for local system needs */
	"ps",		Dpsqms, 0, /* PostScript printer */
	"7475a",	Dhp7475, 0, /* Hewlett Parkard 7475 desktop A paper */
	"7475b",	Dhp7475, 1, /* Hewlett Parkard 7475 desktop B paper */
	"7475c",	Dhp7475, 1, /* Hewlett Parkard 7475 desktop C paper */
	"7475d",	Dhp7475, 1, /* Hewlett Parkard 7475 desktop D paper */
	"7475e",	Dhp7475, 1, /* Hewlett Parkard 7475 desktop E paper */
	"7475",		Dhp7475, 2, /* Hewlett Parkard 7475 desktop A paper */
	"hpgla",	Dhpgl, 0, /* Houston Inst. HPGL mode A paper */
	"hpglb",	Dhpgl, 1, /* Houston Inst. HPGL mode B paper */
	"hpglc",	Dhpgl, 2, /* Houston Inst. HPGL mode C paper */
	"hpgld",	Dhpgl, 3, /* Houston Inst. HPGL mode D paper */
	"hpgle",	Dhpgl, 4, /* Houston Inst. HPGL mode E paper */
	"hpgl",		Dhpgl, 5, /* Houston Inst. HPGL mode A paper */
	"calhpgla",	Dcalhpgl, 0, /* Houston Inst. HPGL mode A paper */
	"calhpglb",	Dcalhpgl, 1, /* Houston Inst. HPGL mode B paper */
	"calhpglc",	Dcalhpgl, 2, /* Houston Inst. HPGL mode C paper */
	"calhpgld",	Dcalhpgl, 3, /* Houston Inst. HPGL mode D paper */
	"calhpgle",	Dcalhpgl, 4, /* Houston Inst. HPGL mode E paper */
	"calhpgl",	Dcalhpgl, 5, /* Houston Inst. HPGL mode A paper */
	"dmp60a",	Ddmp60, 0, /* Houston Inst. HPGL mode A paper */
	"dmp60b",	Ddmp60, 1, /* Houston Inst. HPGL mode B paper */
	"dmp60c",	Ddmp60, 2, /* Houston Inst. HPGL mode C paper */
	"dmp60d",	Ddmp60, 3, /* Houston Inst. HPGL mode D paper */
	"dmp60e",	Ddmp60, 4, /* Houston Inst. HPGL mode E paper */
	"dmp60",	Ddmp60, 5, /* Houston Inst. HPGL mode A paper */
	"gerber",	Dgerber, 0, /* Gerber high-res photohead */
	"c1077",	Dc1077,	0, /* Calcomp controller with page advance */
	"c1077n",	Dc1077,1, /*Calcomp with out page advance */
	"xzoom",	Dextdev, 7, /* xzoom - zoom */
	"sunzoom",	Dextdev, 7, /* SunCore - zoom */
#ifdef GRASS
	"GRASS",	DGRASS, 0, /* GRASS monitor program Mdisplay */
#endif
#if 0			/* look under directory old */
	"PS",		Dpstscr, 0, /* PostScript printer */
	"c970",		Dc970,	0, /* Calcomp 970 (200 c/cm res) */
	"kong",		Dkong,	0, /* Kongsburg - photohead */
	"skong",	Dkong,	2, /* Kongsburg - swabbed output */
	"tek4014sm",	D4014,	1, /* Tektronic 4014 */
	"tk4010",	D4014,	0, /* Tektronic 4010 */
	"4027",		Dingena, 2, /* tektronic 4027 (color) */
	"4025",		Dingena, 2, /* tektronic 4027 (color) */
	"hp2647",	Dingena, 0, /* HP 2647 */
	"c906",		Dc906,	0, /* damn Calcomp controller */
	"hp2393a",	Dingena, 1, /* HP 2393A */
	"7586",		Dhp7586, 4, /* Hewlett Parkard 7586 */
	"7586a",	Dhp7586, 0, /* A paper */
	"7586b",	Dhp7586, 1, /* B paper */
	"7586c",	Dhp7586, 2, /* C paper */
	"7586d",	Dhp7586, 3, /* D paper */
	"7586e",	Dhp7586, 4, /* E paper */
	"dp8",		Dextdev, 2, /* Houston DP8 */
	"apple",	Dextdev, 3, /* Apple laser */
	"imogen",	Dextdev, 4, /* Imogen laser */
	"go140",	D4014,	2, /* GraphOn 140 */
	"c5800",	Dextdev, 5, /* Calcomp 5800 electrostatic */
	"sun",		Dextdev, 6, /* SunCore - batch */
# endif
	"bumdev",	Dbumdev, 0,
	0,		0,	0,
};
