static char *SCCSID = "@(#)prjlst.c	AMG v.1.2";
/* Projection System: List of projections */
# include "projects.h"

extern	aea(), aeqd(), bipc(), bonne(), cea(), eck4(), eck6(), eqc(),
	eqdc(), gnom(), laea(), lcc(), merc(), mill(), moll(),
	nsper(), omerc(), ortho(), poly(), sinu(), stere(),
	tmerc(), ups(), utm(), vandg(), cass(), tpers();

struct PLIST plist[] = {
	"aea",		aea,	/* Albers Egual Area */
	"aeqd",		aeqd,	/* Azimuthal equidistant */
	"bipc",		bipc,	/* Bipolar Conic */
	"bonne",	bonne,	/* Bonne */
	"cass",		cass,	/* Cassini */
	"cea",		cea,	/* Cylindrical Equal Area */
	"eck4",		eck4,	/* Eckert # 4 */
	"eck6",		eck6,	/* Eckert # 6 */
	"eqc",		eqc,	/* Equidistant Cylindrical */
	"eqdc",		eqdc,	/* Equidistant Conic */
	"gnom",		gnom,	/* Gnomonic */
	"laea",		laea,	/* Lambert Azimuthal Equal Area */
	"lcc",		lcc,	/* Lambert Conformal Conic */
	"merc",		merc,	/* Mercator */
	"mill",		mill,	/* Miller */
	"moll",		moll,	/* Mollweides */
	"nsper",	nsper,	/* General Vertical Persepective */
	"omerc",	omerc,	/* Oblique Mercator */
	"ortho",	ortho,	/* Orthographic */
	"poly",		poly,	/* Polyconic */
	"sinu",		sinu,	/* Sinusoidal */
	"stere",	stere,	/* Stereographic */
	"tmerc",	tmerc,	/* Transverse Mercator */
	"tpers",	tpers,	/* Tilted perspective */
	"ups",		ups,	/* Universal Polar Stereographic */
	"utm",		utm,	/* Universal Trensverse Mercator */
	"vandg",	vandg,	/* Van der Grinten */
	0,0			/* list terminator */
};
