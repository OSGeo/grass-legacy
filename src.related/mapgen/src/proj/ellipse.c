static char *SCCSID = "@(#)ellipse.c	AMG v.1.1";
/* output standard ellipsoid constants */
# include <stdio.h>
# include <math.h>

struct AE2 {
	double a, es;
};

static struct {
	char *name;	/* id tag */
	double a, b;	/* major and minor axis in meters */
	char *id;	/* identification */
} *s, ellipses[] = {
"airy",		6377563.396,	6356256.91,	"Airy",
"aust_ntl",	6378160.0,	6356774.719,	"Australian National",
"bessel",	6377397.155,	6356078.96284,	"Bessel",
"cl_66",	6378206.4,	6356583.8,	"Clarke 1866",
"cl_80",	6378249.145,	6356514.86955,	"Clarke 1880",
"everest",	6377276.3452,	6356075.4133,	"Everest",
"grs_80",	6378137.0,	6356752.31414,	"GRS 1980",
"hough",	6378270.0,	6356794.343479,	"Hough",
"intl",		6378388.0,	6356911.94613,	"International 1909 (Hayford)",
"krass",	6378245.0,	6356863.0188,	"Krassovsky",
"mercury",	6378166.0,	6356784.283666,	"Mercury 1960",
"mod_airy",	6377341.89,	6356036.143,	"Modified Airy",
"mod_ever",	6377304.063,	6356103.039,	"Modified Everest",
"mod_merc",	6378150.0,	6356768.337303,	"Modified Merc 1968",
"new_intl",	6378157.5,	6356772.2,	"New International 1967",
"s_e_asia",	6378155.0,	6356773.3205,	"Southeast Asia",
"walbeck",	6376896.0,	6355834.8467,	"Walbeck",
"wgs_66",	6378145.0,	6356759.769356,	"WGS 66",
"wgs_72",	6378135.0,	6356750.519915,	"WGS 72",
"sphere",	6370997.0,	0.0,		"Sphere of 6370997 m",
0, 0., 0., 0};

	static struct AE2
ellips(name) char *name; {
	struct AE2 ae;

	for (s = ellipses; s->name ; ++s )
		if (!strcmp(name, s->name)) {
			ae.a = s->a;
			if (s->b) {
				ae.es = ae.a * ae.a;
				ae.es = 1. - (s->b * s->b) / ae.es;
			} else
				ae.es = 0.;
			return ae;
		}
	ae.a = ae.es = HUGE; /* name not in list */
	return ae;
}

main(argc, argv) char **argv; {
	struct AE2 v;
	char *name;
	int n;

	if (argc < 2) { /* assume user wants list */
		printf("Accronym  : description\n_________ : ___________\n");
		for (s = ellipses; s->name ; ++s) {
			fputs(s->name, stdout);
			for (n = 10 - strlen(s->name); n--; )
				putchar(' ');
			fputs(": ", stdout);
			fputs(s->id, stdout);
			putchar('\n');
		}
	} else {
		name = argv[1];
		v = ellips(name);
		if (v.a == HUGE) {
			fprintf(stderr, "invalid ellipsoid name\n");
			exit(1);
		} else
			printf("+a=%.3f +es=%.10f\n",v.a,v.es);
	}
	exit(0);
}
