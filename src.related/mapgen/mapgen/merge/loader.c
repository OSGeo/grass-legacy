#ifndef lint
static char *SCCSID = "@(#)loader.c	OEMG v.1.1";
#endif
#include "gen.h"
#include <stdio.h>
	extern long
x_board, y_board;
	extern int
cts_cm;
#define MAX_LINE 250
#define MAXC 20
	static int
loaddef(fid) FILE *fid; {
	char line[MAX_LINE], *s, *argv[MAXC], *getline();
	int scalein();

	while (s = getline(line, MAX_LINE, fid)) {
		++File_line;
		if (*s == '#') ++s;
		if (! words(s, MAXC, argv, scalein))
			return(0);
	}
	return(1);
}
	int
loader(master) char *master; {
	FILE *fid;
	long tag;
	extern char povert[], povero[];

	if (!(fid = fopen(master, "r")))
		emess(2, master, "no def - no plot");
	if (fread(&tag, sizeof(long), 1, fid) != 1)
		emess(1,"failure to read at least %d def bytes from file %s",
			sizeof(long),master);
	rewind(fid);
	if (tag == MAGIC) {	/* MAPGEN */
		void drawline(), mgdraw(), mgproj();

		if (fread(&m_def, sizeof(m_def), 1, fid) != 1)
			emess(1,"mapdef file (%s) not equal to %d bytes",
				master, sizeof(m_def));
		x_board = m_def.B.x_max;
		y_board = m_def.B.y_max;
		cts_cm = m_def.cts_cm;
		base_lim.min.x = m_def.D.x_min;
		base_lim.min.y = m_def.D.y_min;
		base_lim.max.x = m_def.D.x_max;
		base_lim.max.y = m_def.D.y_max;
		base_lim.clip = drawline;
		draw = mgdraw;
		geog_lim.min.x = DI_CON * m_def.l_lon;
		geog_lim.min.y = DI_CON * m_def.b_lat;
		geog_lim.max.x = DI_CON * m_def.r_lon;
		geog_lim.max.y = DI_CON * m_def.t_lat;
		icm = (geog_lim.min.x + geog_lim.max.x) / 2;
		icp = (geog_lim.min.y + geog_lim.max.y) / 2;
		geog_lim.clip = mgproj;
		geog_lim.next = &base_lim;
		data_type = MAPGEN;
	} else {	/* PLOTGEN */
		void drawline(), pgdraw();

		if (loaddef(fid))
			emess(1, "plotdef file <%s> failed to scale", master);
		x_board = p_def.x.board;
		y_board = p_def.y.board;
		cts_cm = p_def.cts;
		base_lim.min.x = p_def.x.min;
		base_lim.min.y = p_def.y.min;
		base_lim.max.x = p_def.x.max;
		base_lim.max.y = p_def.y.max;
		base_lim.clip = drawline;
		draw = pgdraw;
		data_type = PLOTGEN;
	}
	fclose(fid);
}
