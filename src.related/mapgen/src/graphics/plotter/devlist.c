#ifndef lint
static char *SCCSID = "@(#)devlist.c	OEMG v.3.3";
#endif
/* Device list module */
#include "plotter.h"
	/* list of currently supported devices */
	XY
*Dc936(), *D4014(), *Dingena(), *Dhp7586(), *Dkong(), *Dc970(),
*Dhp7475(), *Dc906(), *Dpstscr(), *Dgerber(), *Dranger(), *Dpsqms(),
*Dextdev(), *Ddebug(),  *DGRASS(), *Ddmp60(), *Dhpgl(), *Dc1077();

/* list of older, but devices not of current interest
*ge(), *rasdev(), *lips10(), *opt(), *dega()
*/

	struct DEV_LIST
dev_list[] = {
	"ranger",	Dranger, 0, /* range determination routine */
	"QMS",		Dpsqms, 0, /* QMS PostScript printer */
	"qms",		Dpsqms, 0, /* QMS PostScript printer */
	"PS",		Dpstscr, 0, /* PostScript printer */
	"ps",		Dpstscr, 0, /* PostScript printer */
	"exdebug",	Dextdev, 0, /* debug external device mode */
	"debug",	Ddebug,	0, /* debug device mode */
	"7475a",	Dhp7475, 0, /* Hewlett Parkard 7475 desktop A paper */
	"7475b",	Dhp7475, 1, /* Hewlett Parkard 7475 desktop B paper */
	"7475c",	Dhp7475, 1, /* Hewlett Parkard 7475 desktop C paper */
	"7475d",	Dhp7475, 1, /* Hewlett Parkard 7475 desktop D paper */
	"7475e",	Dhp7475, 1, /* Hewlett Parkard 7475 desktop E paper */
	"7475",		Dhp7475, 2, /* Hewlett Parkard 7475 desktop A paper */
	"dmp60a",	Ddmp60, 0, /* Houston Inst. HPGL mode A paper */
	"dmp60b",	Ddmp60, 1, /* Houston Inst. HPGL mode B paper */
	"dmp60c",	Ddmp60, 2, /* Houston Inst. HPGL mode C paper */
	"dmp60d",	Ddmp60, 3, /* Houston Inst. HPGL mode D paper */
	"dmp60e",	Ddmp60, 4, /* Houston Inst. HPGL mode E paper */
	"dmp60",	Ddmp60, 5, /* Houston Inst. HPGL mode A paper */
	"hpgla",	Dhpgl, 0, /* Houston Inst. HPGL mode A paper */
	"hpglb",	Dhpgl, 1, /* Houston Inst. HPGL mode B paper */
	"hpglc",	Dhpgl, 2, /* Houston Inst. HPGL mode C paper */
	"hpgld",	Dhpgl, 3, /* Houston Inst. HPGL mode D paper */
	"hpgle",	Dhpgl, 4, /* Houston Inst. HPGL mode E paper */
	"hpgl",		Dhpgl, 5, /* Houston Inst. HPGL mode A paper */
	"gerber",	Dgerber, 0, /* Gerber high-res photohead */
	"c1077",	Dc1077,	0, /* Calcomp controller with page advance */
	"c1077n",	Dc1077,1, /*Calcomp with out page advance */
	"GRASS",	DGRASS, 0, /* GRASS monitor program Mdisplay */
	"sun",		Dextdev, 6, /* SunCore - batch */
	"sunzoom",	Dextdev, 7, /* SunCore - zoom */
#ifdef IGNORE
	"sunview",	Dsunview, 0, /* Sunview canvas */
	"orchid",	Dorchid, 0, /* Orchid card */
	"7586",		Dhp7586, 4, /* Hewlett Parkard 7586 */
	"7586a",	Dhp7586, 0, /* A paper */
	"7586b",	Dhp7586, 1, /* B paper */
	"7586c",	Dhp7586, 2, /* C paper */
	"7586d",	Dhp7586, 3, /* D paper */
	"7586e",	Dhp7586, 4, /* E paper */
	"go140",	D4014,	2, /* GraphOn 140 */
	"c5800",	Dextdev, 5, /* Calcomp 5800 electrostatic */
	"ega",		Dgfx,	0, /* uPort gfx */
	"AT386",	Dgfx,	0, /* uPort gfx */
	"at386",	Dgfx,	0, /* uPort gfx */
	"kong",		Dkong,	0, /* Kongsburg - photohead */
	"skong",	Dkong,	2, /* Kongsburg - swabbed output */
	"gerbers",	Dgerber, 3, /* Gerber, newline-noswab */
	"ega",		dega,	0, /* EGA color */
	"tek4014sm",	D4014,	1, /* Tektronic 4014 */
	"tk4010",	D4014,	0, /* Tektronic 4010 */
	"c906",		Dc906,	0, /* damn Calcomp controller */
	"c970",		Dc970,	0, /* Calcomp 970 (200 c/cm res) */
	"c936",		c936,	0, /* Calcomp 936 */
	"4027",		Dingena, 2, /* tektronic 4027 (color) */
	"4025",		Dingena, 2, /* tektronic 4027 (color) */
	"hp2647",	Dingena, 0, /* HP 2647 */
	"lips10",	lips10,	0, /* CEI LIPS 10 laser printer */
	"ge",		ge,	0, /* standard GE raster printer */
	"hp2393a",	Dingena, 1, /* HP 2393A */
	"dp8",		Dextdev, 2, /* Houston DP8 */
	"apple",	Dextdev, 3, /* Apple laser */
	"imogen",	Dextdev, 4, /* Imogen laser */
	"rasdev",	rasdev,	0, /* formated for raster converter */
	"opt",		opt,	0, /* Optronic 680*540 slide output */
	"bumdev",	Dbumdev, 0,
# endif
	0,		0,	0,
};
