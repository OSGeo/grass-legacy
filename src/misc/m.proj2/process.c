#include <string.h>
#include "geo.h"
#include "gis.h"
#include "projects.h"
#include "local_proto.h"

/* returns 1 if projection info changed, 0 otherwise */
int process(int in, char *parms, char *proj_name, char *proj_title,
    char *ellps_name, double *radius, struct used_opt *USED, char *units)
{
	int proj_changed = 0;
	int proj_index = 0;
	int sph = 0, index_prev = -1;
	double a, es;
	double unit_fact;
	int i, npr, stat;
	char io[16];
	char old_units[100];


	sprintf(old_units, "%s", units);
	G_strip(old_units);

	if (in == 1)
		sprintf(io, "INPUT ");
	else if (in == 2)
		sprintf(io, "OUTPUT ");
	else
		io[0] = '\0';
	
	proj_changed = 0;

	if (strlen(proj_name) == 0 || strncmp(proj_name, "None", 4) == 0) {
		proj_changed = 1;
		fprintf(stderr, "\n\nInitializing %sPROJECTION:", io);
		if (G_ask_proj_name(proj_name, proj_title) < 0)
			exit(0);
	} else {
		fprintf(stderr, "\n Last %sprojection used was \"%s\", do you want to change it (y/[n]) ? ", io, proj_name);
		G_gets(answer);
		if ((strlen(answer) != 0) && *answer != 'N' && *answer != 'n') {
			proj_changed = 1;
			fprintf(stderr, "\n\nInitializing %sPROJECTION:", io);
			if (G_ask_proj_name(proj_name, proj_title) < 0)
				exit(0);
		}
	}
	if (index_prev != -1)
		index_prev = proj_index;
	proj_index = get_proj_index(proj_name);
	if (proj_index < 0) {
		sprintf(buff, "projection %s is not specified in the table", proj_name);
		G_fatal_error(buff);
	}
	parms[0] = '\0';
	if ((strncmp(proj_name, "stp", 3) != 0) && (strncmp(proj_name, "ll", 2) != 0)) {
		if ((proj_index == ALSK) || (proj_index == GS48) || (proj_index == GS50)) {
			sprintf(ellps_name, "%s", "clark66");
			sph = 1;
		} else if ((proj_index == LABRD) || (proj_index == NZMG)) {
			sprintf(ellps_name, "%s", "international");
			sph = 1;
		} else if (proj_index == SOMERC) {
			sprintf(ellps_name, "%s", "bessel");
			sph = 1;
		} else if (proj_index == OB_TRAN) {
			sprintf(parms, "%s", " +o_proj=eqc");
			/* Hard coded to use "Equidistant Cylincrical"
			   as the second projection o_proj */
			sph = 2;
			get_sphere_radius(radius);
		} else {
			if (strncmp(ellps_name, "None", 4) == 0 || strlen(ellps_name) == 0) {
				proj_changed = 1;
				/* get_ellps_opts(in); */
				fprintf(stderr, "\nInitializing %sprojection ELLIPSOID:", io);
				sph = G_ask_ellipse_name(ellps_name);
				if (sph < 0)
					exit(0);
				if (sph == 2)
					get_sphere_radius(radius);
			} else {
				fprintf(stderr, "\n Last %sspheroid used was \"%s\", do you want to change it (y/[n]) ? ", io, ellps_name);
				G_gets(answer);
				if ((strlen(answer) != 0) && *answer != 'N' && *answer != 'n') {
					/* get_ellps_opts(io); */
					proj_changed = 1;
					fprintf(stderr, "\nInitializing %sprojection ELLIPSOID:", io);
					sph = G_ask_ellipse_name(ellps_name);
					if (sph < 0)
						exit(0);
				} else if (strncmp(ellps_name, "sphere", 6) == 0)
					sph = 2;
				if (sph == 2) {
					if (!*radius)
						*radius = RADIUS_DEF;
					fprintf(stderr, "\nRadius for the sphere was %.10f, do you want to change it (y/[n]) ? ", *radius);
					G_gets(answer);
					if ((strlen(answer) != 0) && *answer != 'N' && *answer != 'n')
						get_sphere_radius(radius);
				}
			}
		}
	}
	if (proj_index == STP)
		sprintf(units, "feet");
	else
		sprintf(units, "meters");

	switch (proj_index) {
	case STP:
		fprintf(stderr, "\n%sProjection:\n ", io);
		get_stp_proj(parms);
		proj_changed = 1;
		break;

	case LL:
		sprintf(parms, "+proj=%s", proj_name);
		break;

	default:
		if (sph != 2) {
			G_strip(ellps_name);
			if (G_get_ellipsoid_by_name(ellps_name, &a, &es) == 0) {
				sprintf(buff, "Invalid %sellipsoid %s", io, ellps_name);
				G_fatal_error(buff);
			}
			sprintf(buff, " +proj=%s +a=%.10f +es=%.10f", proj_name, a, es);
			strcat(parms, buff);
		} else {
			sprintf(buff, " +proj=%s +a=%.10f +es=0.0", proj_name, *radius);
			strcat(parms, buff);
		}
		/* get_proj_opts */
		for (i = 0; i < NOPTIONS; i++) {
			if (TABLE[proj_index][i].ask == 1) {
				if ((i > BOOLOW) && (i < BOOHIGH)) {
					fprintf(stderr, "\n%sProjection: Would you like to use %s (y/[n]) ? ", io, DESC[i]);
					G_gets(answer);
					proj_changed = 1;
				} else {
					if ((USED[i].was == 1) && (index_prev != proj_index)) {
						if ((i > INTLOW) && (i < INTHIGH)) {
							fprintf(stderr, "\n%sProjection: Last %s used was %d\n\t\tdo you want to change it (y/[n]) ? ", io, DESC[i], (int) (USED[i].val));
						} else {
							fprintf(stderr, "\n%sProjection: Last %s used was %.10f\n\t\tdo you want to change it (y/[n]) ? ", io, DESC[i], USED[i].val);
						}
						G_gets(answer);
					} else
						*answer = 'y';
				}
				if ((strlen(answer) != 0) && *answer != 'N' && *answer != 'n') {
					proj_changed = 1;
					set_proj_bool_opts(i, parms, buff);
					/* get_proj_non_bool_opts */
					for (;;) {
						if (i < LATHIGH)
							if (get_LL_stuff(in, &USED[i].val, 1, proj_index, i))
								break;
						if ((i > LONLOW) && (i < LONHIGH))
							if (get_LL_stuff(in, &USED[i].val, 0, proj_index, i))
								break;
						if ((i > FLOLOW) && (i < INTHIGH))
							if (get_float(in, &USED[i].val, proj_index, i))
								break;
						if (i > INTHIGH)
							break;
					}	/* for */
					USED[i].was = 1;
				}
				set_proj_nonbool_opts(i, parms, buff, USED[i].val);
				/* set_proj_non_bool_opts_defaults */
			} else if (TABLE[proj_index][i].def_exists == 1) {	/* don't ask, use the default */

				if (i < BOOLOW) {
					set_proj_nonbool_opts(i, parms, buff, TABLE[proj_index][i].deflt);
				} else if (i < BOOHIGH) {
					set_proj_bool_opts(i, parms, buff);
				}
			}
		}		/* for */
	}			/* switch index */
	/* get_proj_units */
	if (proj_index != LL) {
		fprintf(stderr, "Enter plural for %sunits [%s]: ", io, units);
		G_gets(answer);
		if (strlen(answer) != 0) {
			sprintf(units, "%s", answer);
		}
		G_strip(units);
		if (strcmp(old_units, units) != 0)
			proj_changed = 1;
		npr = strlen(units);
		for (i = 0; i < NUNITS; i++) {
			stat = min1(npr, strlen(UNITS[i].units));
			if (strncmp(UNITS[i].units, units, stat) == 0) {
				unit_fact = UNITS[i].fact;
				break;
			}
		}
		if (i >= NUNITS) {
			for (;;) {
				do {
					fprintf(stderr, "Enter conversion factor from %s to meters: ", units);
				} while (!G_gets(answer));
				if (strlen(answer) != 0) {
					if (sscanf(answer, "%lf", &unit_fact) != 1)
						fprintf(stderr, "\nInvalid Entry\n");
					else
						break;
				}
			}
		}
		sprintf(buff, " +unfact=%.10f ", unit_fact);
		strcat(parms, buff);
		fprintf(stderr, "\nParameters used in %sprojection: \n\t%s\n\n", io, parms);
	}
	return proj_changed;
}

int set_proj_bool_opts(int index, char *parms, char *buffer)
{
	switch (index) {
	case SOUTH:
		sprintf(buffer, " +south");
		strcat(parms, buffer);
		break;
	case NOSKEW:
		sprintf(buffer, " +ns");
		strcat(parms, buffer);
		break;
	case NOCUT:
		sprintf(buffer, " +no_cut");
		strcat(parms, buffer);
		break;
	case NOROT:
		sprintf(buffer, " +no_conv");
		strcat(parms, buffer);
		break;
	case NOUOFF:
		sprintf(buffer, " +no_uoff");
		strcat(parms, buffer);
		break;
	case ROTCONV:
		sprintf(buffer, " +rot_conv");
		strcat(parms, buffer);
		break;
	case GUAM:
		sprintf(buffer, " +guam");
		strcat(parms, buffer);
		break;
	case LOTSA:
		sprintf(buffer, " +lotsa");
		strcat(parms, buffer);
		break;
	case NODEFS:
		sprintf(buffer, " +no_defs");
		strcat(parms, buffer);
		break;
	default:
		break;
	}

	return 0;
}

int set_proj_nonbool_opts(int index, char *parms, char *buffer, double value)
{
	switch (index) {
	case LAT0:
		sprintf(buffer, " +lat_0=%.10f", value);
		strcat(parms, buffer);
		break;
	case LAT1:
		sprintf(buffer, " +lat_1=%.10f", value);
		strcat(parms, buffer);
		break;
	case LAT2:
		sprintf(buffer, " +lat_2=%.10f", value);
		strcat(parms, buffer);
		break;
	case LAT3:
		sprintf(buffer, " +lat_3=%.10f", value);
		strcat(parms, buffer);
		break;
	case LATTS:
		sprintf(buffer, " +lat_ts=%.10f", value);
		strcat(parms, buffer);
		break;
	case LATB:
		sprintf(buffer, " +lat_b=%.10f", value);
		strcat(parms, buffer);
		break;
	case OLATP:
		sprintf(buffer, " +o_lat_p=%.10f", value);
		strcat(parms, buffer);
		break;
	case OLAT1:
		sprintf(buffer, " +o_lat_1=%.10f", value);
		strcat(parms, buffer);
		break;
	case OLAT2:
		sprintf(buffer, " +o_lat_2=%.10f", value);
		strcat(parms, buffer);
		break;
	case LON0:
		sprintf(buffer, " +lon_0=%.10f", value);
		strcat(parms, buffer);
		break;
	case LON1:
		sprintf(buffer, " +lon_1=%.10f", value);
		strcat(parms, buffer);
		break;
	case LON2:
		sprintf(buffer, " +lon_2=%.10f", value);
		strcat(parms, buffer);
		break;
	case LON3:
		sprintf(buffer, " +lon_3=%.10f", value);
		strcat(parms, buffer);
		break;
	case LONC:
		sprintf(buffer, " +lon_c=%.10f", value);
		strcat(parms, buffer);
		break;
	case ALPHA:
		sprintf(buffer, " +alpha=%.10f", value);
		strcat(parms, buffer);
		break;
	case THETA:
		sprintf(buffer, " +theta=%.10f", value);
		strcat(parms, buffer);
		break;
	case OLONP:
		sprintf(buffer, " +o_lon_p=%.10f", value);
		strcat(parms, buffer);
		break;
	case OLON1:
		sprintf(buffer, " +o_lon_1=%.10f", value);
		strcat(parms, buffer);
		break;
	case OLON2:
		sprintf(buffer, " +o_lon_2=%.10f", value);
		strcat(parms, buffer);
		break;
	case OLONC:
		sprintf(buffer, " +o_lon_c=%.10f", value);
		strcat(parms, buffer);
		break;
	case OALPHA:
		sprintf(buffer, " +o_alpha=%.10f", value);
		strcat(parms, buffer);
		break;
	case ZONE:
		sprintf(buffer, " +zone=%d", (int) (value));
		strcat(parms, buffer);
		break;
	case SPATH:
		sprintf(buffer, " +path=%d", (int) (value));
		strcat(parms, buffer);
		break;
	case SNUM:
		sprintf(buffer, " +lsat=%d", (int) (value));
		strcat(parms, buffer);
		break;
	case AZIM:
		sprintf(buffer, " +azi=%.10f", value);
		strcat(parms, buffer);
		break;
	case TILT:
		sprintf(buffer, " +tilt=%.10f", value);
		strcat(parms, buffer);
		break;
	case HEIGH:
		sprintf(buffer, " +h=%.10f", value);
		strcat(parms, buffer);
		break;
	case KFACT:
		sprintf(buffer, " +k=%.10f", value);
		strcat(parms, buffer);
		break;
	case MFACT:
		sprintf(buffer, " +m=%.10f", value);
		strcat(parms, buffer);
		break;
	case MSFACT:
		sprintf(buffer, " +M=%.10f", value);
		strcat(parms, buffer);
		break;
	case NFACT:
		sprintf(buffer, " +n=%.10f", value);
		strcat(parms, buffer);
		break;
	case QFACT:
		sprintf(buffer, " +q=%.10f", value);
		strcat(parms, buffer);
		break;
	case WFACT:
		sprintf(buffer, " +W=%.10f", value);
		strcat(parms, buffer);
		break;
	case X0:
		sprintf(buffer, " +x_0=%.10f", value);
		strcat(parms, buffer);
		break;
	case Y0:
		sprintf(buffer, " +y_0=%.10f", value);
		strcat(parms, buffer);
		break;
	default:
		break;
	}

	return 0;
}

int get_sphere_radius(double *radius)
{
	for (;;) {
		fprintf(stderr, "\nEnter radius for the sphere in meters (%.10f):", RADIUS_DEF);
		G_gets(answer);
		if (strlen(answer) == 0) {
			*radius = RADIUS_DEF;
			break;
		}
		if (sscanf(answer, "%lf", radius) != 1)
			fprintf(stderr, "\nInvalid Entry\n");
		else
			break;
	}

	return 0;
}
