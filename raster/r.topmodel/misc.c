#include "global.h"


int run(char *cmd)
{
    int retval;


    if (G_system(cmd)) {
	G_warning("Failed");
	retval = 1;
    }
    else {
	G_message("OK");
	retval = 0;
    }

    return retval;
}


void gregion(void)
{
    char *hdmap;


    hdmap = NULL;
    if (!flg.input) {
	hdmap = map.elev;
    }
    else {
	if (map.belev)
	    hdmap = map.belev;
	else if (map.topidx)
	    hdmap = map.topidx;
    }

    if (hdmap) {
	sprintf(buf, "g.region rast=%s --quiet", hdmap);
	G_message("%s ...", buf);

	if (run(buf))
	    exit(1);
    }
}


void depressionless(void)
{
    sprintf(buf, "r.fill.dir input=%s elev=%s dir=%s type=grass --quiet",
	    map.elev, map.fill, map.dir);
    G_message("%s ...", buf);

    if (run(buf))
	exit(1);

    map.elev = map.fill;

    return;
}


void basin_elevation(void)
{
    /* be quiet */
/*    G_putenv("GRASS_VERBOSE", "0");   how to unset in a cross-platform way afterwards? */
    sprintf(buf, "r.mapcalc \"%s = if(%s == 0 || isnull(%s), null(), %s)\"",
	    map.belev, map.basin, map.basin, map.elev);

    G_message("%s ...", buf);

    if (run(buf))
	exit(1);

    return;
}


void top_index(void)
{
    if (map.belev) {
	sprintf(buf, "r.topidx input=%s output=%s --quiet",
		map.belev, map.topidx);
	G_message("%s ...", buf);

	if (run(buf))
	    exit(1);
    }

    if (map.topidx) {
	sprintf(buf, "r.stats -Anc input=%s nsteps=%d output=\"%s\"",
		map.topidx, misc.nidxclass, file.idxstats);
	G_message("%s ...", buf);

	if (run(buf))
	    exit(1);
    }

    return;
}
