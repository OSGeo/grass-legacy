#ifndef lint
static char *SCCSID = "@(#)SetProj.c	USGS v.3.4";
#endif
/* Projection System: List of projections
** When adding a projection both of the following statements
** need to be updated.
*/
# include "projects.h"

UV	(*aea())(), (*aeqd())(), (*bipc())(), (*bonne())(), (*cea())(),
	(*eck4())(), (*eck6())(), (*eqc())(), (*eqdc())(), (*gnom())(),
	(*laea())(), (*lcc())(), (*merc())(), (*mill())(), (*moll())(),
	(*nsper())(), (*omerc())(), (*ortho())(), (*poly())(), (*sinu())(),
	(*stere())(), (*tmerc())(), (*ups())(), (*utm())(), (*vandg())(),
	(*cass())(), (*tpers())(), (*tcea())(), (*ocea())(), (*parab())(),
	(*gall())(), (*eck5())(), (*wink1())(), (*hammer())(), (*august())(),
	(*hataea())(), (*mbtfps())(), (*mbtfpq())(), (*putp2())(), (*eck3())(),
	(*mbtfpp())(), (*putp5())(), (*quau())(), (*dense())(), (*robin())(),
	(*aitoff())(), (*wintri())(), (*eck1())(), (*eck2())(), (*boggs())(),
	(*pconic())(), (*rpoly())(), (*airy())(), (*bacon())(), (*eisen())(),
	(*fourn())(), (*lagrng())(), (*nicol())(), (*ortel())(), (*vandg2())(),
	(*vandg3())(), (*vandg4())(), (*wag7())(), (*leac())(), (*loxim())(),
	(*apian())(), (*cc())(), (*tcc())(), (*collg())(), (*goode())();
	static
struct {
	char	*id;	/* +proj=identifier */
	UV (*(*proj)())(); /* projection initialization entry point */
	char	*name;	/* basic projection full name */
} *p, plist[] = {
	"airy",		airy,	"Airy-F",
	"aea",		aea,	"Albers Egual Area-FI",
	"apian",	apian,	"Apian Globular I-F",
	"aeqd",		aeqd,	"Azimuthal equidistant-FI",
	"aitoff",	aitoff,	"Aitoff-F",
	"august",	august,	"August Epicycloidal-F",
	"bacon",	bacon,	"Bacon Globular-F",
	"bipc",		bipc,	"Bipolar Conic-FI",
	"boggs",	boggs,	"Boggs Eumorphic-F",
	"bonne",	bonne,	"Bonne-FI",
	"cass",		cass,	"Cassini-FI",
	"cc",		cc,	"Central Cylindrical-FI",
	"cea",		cea,	"Cylindrical Equal Area-FI",
	"collg",	collg,	"Collignon-FI",
	"dense",	dense,	"Denoyer Semi-Elliptical-F",
	"eck1",		eck1,	"Eckert I-FI",
	"eck2",		eck2,	"Eckert II-FI",
	"eck3",		eck3,	"Eckert III-FI",
	"eck4",		eck4,	"Eckert IV-FI",
	"eck5",		eck5,	"Eckert V-FI",
	"eck6",		eck6,	"Eckert VI-FI",
	"eisen",	eisen,	"Eisenlohr-F",
	"eqc",		eqc,	"Equidistant Cylindrical-FI",
	"eqdc",		eqdc,	"Equidistant Conic-FI",
	"fourn",	fourn,	"Fournier Globular-F",
	"gall",		gall,	"Gall (Stereographic)-FI",
	"goode",	goode,	"Goode Homolosine-F",
	"gnom",		gnom,	"Gnomonic-FI",
	"hammer",	hammer,	"Hammer (Elliptical)-F",
	"hataea",	hataea,	"Hatano Asymmetrical Equal Area-FI",
	"lagrng",	lagrng,	"Lagrange-F",
	"laea",		laea,	"Lambert Azimuthal Equal Area-FI",
	"leac",		leac,	"Lambert Equal Area Conic-FI",
	"lcc",		lcc,	"Lambert Conformal Conic-FI",
	"loxim",	loxim,	"Loximuthal-FI",
	"mbtfpp",	mbtfpp,	"McBryde-Thomas Flat-Polar Parabolic-FI",
	"mbtfps",	mbtfps,	"McBryde-Thomas Flat-Polar Sinusoidal-FI",
	"mbtfpq",	mbtfpq,	"McBryde-Thomas Flat-Polar Quartic-FI",
	"merc",		merc,	"Mercator-FI",
	"mill",		mill,	"Miller-FI",
	"moll",		moll,	"Mollweides-FI",
	"nicol",	nicol,	"Nicolosi Globular-F",
	"nsper",	nsper,	"General Vertical Persepective-FI",
	"ocea",		ocea,	"Oblique Cylindrical Equal Area-FI",
	"omerc",	omerc,	"Oblique Mercator-FI",
	"ortel",	ortel,	"Ortelius-F",
	"ortho",	ortho,	"Orthographic-FI",
	"parab",	parab,	"Caster Parabolic-FI",
	"pconic",	pconic,	"Perspective Conic-F",
	"poly",		poly,	"Polyconic (American)-FI",
	"putp2",	putp2,	"Putnins P2'-FI",
	"putp5",	putp5,	"Putnins P5-FI",
	"rpoly",	rpoly,	"Rectangular Polyconic-F",
	"quau",		quau,	"Quartic Authalic-FI",
	"robin",	robin,	"Robinson-FI",
	"sinu",		sinu,	"Sinusoidal-FI",
	"stere",	stere,	"Stereographic-FI",
	"tcc",		tcc,	"Transverse Central Cylindrical-FI",
	"tcea",		tcea,	"Transverse Cylindrical Equal Area-FI",
	"tmerc",	tmerc,	"Transverse Mercator-FI",
	"tpers",	tpers,	"Tilted perspective-FI",
	"ups",		ups,	"Universal Polar Stereographic-FI",
	"utm",		utm,	"Universal Transverse Mercator-FI",
	"vandg",	vandg,	"Van der Grinten-FI",
	"vandg2",	vandg2,	"Van der Grinten II-F",
	"vandg3",	vandg3,	"Van der Grinten III-F",
	"vandg4",	vandg4,	"Van der Grinten IV-F",
	"wag7",		wag7,	"Wagner VII-F",
	"wink1",	wink1,	"Winkel I-FI",
	"wintri",	wintri,	"Winkel Tripel-F",
	0,		0,	"list terminator"
};
	void
proj_list() {
	for ( p = plist; p->id; ++p)
		printf("[+]proj=%-8s -> %s\n", p->id,p->name);
}
	UV
(*SetProj(name, inverse))() char *name; {
	int i;

	if (! strcmp(name, "list")) {
		for (i = 0; plist[i].id; ++i)
			printf("%s -> %s\n",plist[i].id,plist[i].name);
		exit(0);
	} else {
		for (i = 0; plist[i].id; ++i)
			if (! strcmp(name, plist[i].id))
				return ((*plist[i].proj)(inverse));
	}
	return ((UV (*)()) 0);
}
