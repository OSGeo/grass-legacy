/*
 * $Id$
 *
 ****************************************************************************
 *
 * MODULE:       g.setproj 
 * AUTHOR(S):    M. L. Holko - Soil Conservation Service, USDA
 *               Morten Hulden - morten@ngb.se
 *               Andreas Lange - andreas.lange@rhein-main.de
 * PURPOSE:      Provides a means of creating a new projection information
 *               file (productivity tool).
 * COPYRIGHT:    (C) 2000 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *   	    	 License (>=v2). Read the file COPYING that comes with GRASS
 *   	    	 for details.
 *
 *****************************************************************************/

/* old log retained for information */
/* main.c    
   *    1.1   05/16/91  GRASS4.0
   *    Created by : M.L.Holko , Soil Conservation Service, USDA
   *    Purpose: Productivity tool
   *          Provides a means of creating a new projection
   *             information file
   *
   *  ------Rev 4.+ arguements --------------------------------------
   *    Input arguements:
   *             m.setproj set=mapset for output project info file
   *                    proj=projection of the output project info file
   *
   *   1.2 Changed by Morten Hulden 10/10/99 to add support for more projections        
   *                  morten@ngb.se
   *
   *   1.3 Changed by Andreas Lange 07/25/00 to add datum support
   *                  Andreas.Lange@Rhein-Main.de
 */

#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include "geo.h"
#include "gis.h"
#include "CC.h" /* for datum support */
#include "local_proto.h"
#define MAIN

int main(int argc, char *argv[])
{
	int Out_proj;
	int out_stat;
	int ret;
	int old_zone;
	int i;
	int stat;
	int set_datum = 0;
	char cmnd2[500], out_lon0[20], out_lat0[20];
	char proj_out[20], proj_name[50], set_name[20], file[1024],
	*value1, *a;
	char path[1024], epath[1024], buffa[1024], buffb[1024], answer[200],
	 answer1[200];
	char answer2[200], buff[1024];
	char tmp_buff[20], *buf;
	char datum[50], dat_ellps[50];

	struct Option *projopt;
	struct Key_Value *old_proj_keys, *out_proj_keys, *in_unit_keys;
	double aa, e2;
	double f, dx, dy, dz;
	FILE *ls, *out1, *FPROJ, *pj;
	int exist = 0;
	char *new_data, *key, *value, spheroid[50], *sph;
	int j, k, in_stat, ii, npr, sph_check;
	struct Cell_head cellhd;


	G_gisinit(argv[0]);

	init_table();
	init_unit_table();
	sprintf(set_name, "PERMANENT");
	G__file_name(path, "", PROJECTION_FILE, set_name);

	/* get the output projection parameters, if existing */
	/* Check for ownership here */
	stat = G__mapset_permissions(set_name);
	if (stat == 0) {
		G_fatal_error("PERMANENT: permission denied.");
	}
	G_get_default_window(&cellhd);
	if (-1 == G_set_window(&cellhd))
		G_fatal_error("set_window failed");

	if (G_get_set_window(&cellhd) == -1)
		G_fatal_error("get_set_window failed");

	Out_proj = cellhd.proj;
	old_zone = cellhd.zone;

	if (access(path, 0) == 0) {
		exist = 1;
		fprintf(stderr, "\n\nWARNING!  A projection file '%s' \n   already exists for this location\n", path);
		fprintf(stderr, "\nThis file contains all the parameters for the location's projection: %s\n", G__projection_name(Out_proj));
		fprintf(stderr, "\nOverriding this information implies that the old projection parameters\n");
		fprintf(stderr, "    were incorrect.  If you change the parameters, all existing data will be\n");
		fprintf(stderr, "    interpreted differently by the projection software.\n%c%c%c", 7, 7, 7);
		fprintf(stderr, "    GRASS will not re-project your data automatically\n\n");

		if (!G_yes("Would you still like to change some of the parameters ", 0)) {
			fprintf(stderr, "The projection information will not be updated\n");
			leave(SP_NOCHANGE);
		}
	}
	out_proj_keys = G_create_key_value();

	if (exist) {
		FPROJ = fopen(path, "r");
		old_proj_keys = G_fread_key_value(FPROJ);
		fclose(FPROJ);
		buf = G_find_key_value("zone", old_proj_keys);
		if (buf != NULL)
			sscanf(buf, "%d", &zone);
		if (zone != old_zone) {
			fprintf(stderr, "WARNING! Zone in default geographic region definition: %d\n is different from zone in PROJ_INFO file: %d\n", old_zone, zone);
			old_zone = zone;
		}
	}
	switch (Out_proj) {
	case 0:		/* No projection/units */
		/* leap frog over code, and just make sure we remove the file */
		fprintf(stderr, "XY-location cannot be projected.\n");
		goto write_file;
		break;

	case PROJECTION_UTM:
		sprintf(proj_name, "%s", G__projection_name(PROJECTION_UTM));
		sprintf(proj_out, "utm");
		break;
	case PROJECTION_SP:
		sprintf(proj_name, "%s", G__projection_name(PROJECTION_SP));
		sprintf(proj_out, "stp");
		break;
	case PROJECTION_LL:
		sprintf(proj_name, "%s", G__projection_name(PROJECTION_LL));
		sprintf(proj_out, "ll");
		break;
	case PROJECTION_OTHER:
		while (1) {
			if (G_ask_proj_name(proj_out, proj_name) < 0) {
				leave(SP_NOCHANGE);
			}
			Out_proj = 4;
			proj_index = get_proj_index(proj_out);
			if (proj_index == LL || proj_index == UTM)
				fprintf(stderr, "%c\nProjection 99 does not support UTM or LL\n\n", 7);
			else
				break;
		}
		break;
	default:
		G_fatal_error("Unknown projection\n");
	}

	proj_index = get_proj_index(proj_out);
	if (proj_index < 0) {
		sprintf(buff, "Projection %s is not specified in the table", proj_out);
		G_fatal_error(buff);
	}
	G_set_key_value("name", proj_name, out_proj_keys);

/*****************   GET spheroid  **************************/

	if (Out_proj != PROJECTION_SP) {	/* some projections have fixed spheroids */
		if ((proj_index == ALSK) || (proj_index == GS48) || (proj_index == GS50)) {
			sprintf(spheroid, "%s", "clark66");
			G_set_key_value("ellps", spheroid, out_proj_keys);
			sph_check = 1;
		} else if ((proj_index == LABRD) || (proj_index == NZMG)) {
			sprintf(spheroid, "%s", "international");
			G_set_key_value("ellps", spheroid, out_proj_keys);
			sph_check = 1;
		} else if (proj_index == SOMERC) {
			sprintf(spheroid, "%s", "bessel");
			G_set_key_value("ellps", spheroid, out_proj_keys);
			sph_check = 1;
		} else if (proj_index == OB_TRAN) {
			/* Hard coded to use "Equidistant Cylincrical"
			   until g.setproj has been changed to run
			   recurively, to allow input of options for
			   a second projection, MHu991010 */
			G_set_key_value("o_proj", "eqc", out_proj_keys);
			sph_check = 2;
		} else {
			if (exist) {
				buf = G_find_key_value("ellps", old_proj_keys);
				if (buf != NULL)
					strcpy(spheroid, buf);
				else
					sprintf(spheroid, "%s", "sphere");
				G_strip(spheroid);
				if ((G_get_spheroid_by_name(spheroid, &aa, &e2, &f)) || (strcmp(spheroid, "sphere") == 0)) {	/* if legal ellips. exist, ask wether or not to change it */
					fprintf(stderr, "The current ellipsoid is %s\n", spheroid);
					if (G_yes("Would you want to change ellipsoid parameter ", 0))
						sph_check = G_ask_ellipse_name(spheroid);
					else {
						fprintf(stderr, "The ellipse information is not changed\n");
						if (strcmp(spheroid, "sphere") == 0)
							sph_check = 2;
						else
							sph_check = 1;
					}
				}	/* the val is legal */
			} else
				sph_check = G_ask_ellipse_name(spheroid);
		}		/* if proj_index = ALSK ... OB_TRANS */

		if (sph_check < 0)
			leave(SP_NOCHANGE);

		if (sph_check == 2) {	/* ask radius */
			if (exist) {
				buf = G_find_key_value("a", old_proj_keys);
				if ((buf != NULL) && (sscanf(buf, "%lf", &radius) == 1)) {
					fprintf(stdout, "The radius right now is %f\n", radius);
					if (G_yes("Would you want to change the radius ", 0))
						radius = prompt_num_double("Enter radius for the sphere in meters", RADIUS_DEF, 1);
				}
			} else
				radius = prompt_num_double("Enter radius for the sphere in meters", RADIUS_DEF, 1);
		}		/* end ask radius */
	}
/*** END get spheroid  ***/

	/* ask for map datum, Andreas Lange, 7/2000 */
	if (ask_datum(datum))
	   set_datum = 1;

	/* create the PROJ_INFO & PROJ_UNITS files, if required */
	switch (proj_index) {
	case LL:
		break;

	case STP:
		if (Out_proj == PROJECTION_SP) {
			if (get_stp_code(old_zone, buffb) == 0) {
				sprintf(buff, "Invalid State Plane Zone : %d", old_zone);
				G_fatal_error(buff);
			}
		} else {
			get_stp_proj(buffb);
		}
		break;

	default:
		if (sph_check != 2) {
			G_strip(spheroid);
			if (G_get_spheroid_by_name(spheroid, &aa, &e2, &f) == 0)
				G_fatal_error("invalid input ellipsoid");
		}
		break;
	}

	/* datum support added by Andreas Lange 07/2000 */
	if (set_datum) {
	  if (!CC_get_datum_parameters(datum, dat_ellps, &dx, &dy, &dz)) {
	    sprintf(buff, "Error reading datum paramters!");
	    G_fatal_error(buff);
	  }

	  if(strcmp(dat_ellps, spheroid)!=0) {
	    sprintf(buff, "WARNING: ellipsoid defined by datum (%s) is not the same as choosen by user (%s)!\n", dat_ellps, spheroid);
	    G_warning(buff);
	    if (!G_yes("Continue anyway?", 0))
	      sprintf(buff, "Exiting, no update done!");
	      G_fatal_error(buff);
	  }
	  /* write out key/value pairs to out_proj_keys */
	  sprintf(tmp_buff, "%s", datum);
	  G_set_key_value("datum", tmp_buff, out_proj_keys);
	  sprintf(tmp_buff, "%lf", dx);
	  G_set_key_value("dx", tmp_buff, out_proj_keys);
	  sprintf(tmp_buff, "%lf", dy);
	  G_set_key_value("dy", tmp_buff, out_proj_keys);
	  sprintf(tmp_buff, "%lf", dz);
	  G_set_key_value("dz", tmp_buff, out_proj_keys);    
	}

      write_file:
	/*
	   **  NOTE   the program will (hopefully) never exit abnormally
	   **  after this point.  Thus we know the file will be completely
	   **  written out once it is opened for write 
	 */
	if (exist) {
		sprintf(buff, "mv %s %s~", path, path);
		system(buff);
	}
	if (Out_proj == 0)
		goto write_units;

	/*
	   **   Include MISC parameters for PROJ_INFO
	 */
	switch (proj_index) {
	case STP:
		for (i = 0; i < strlen(buffb); i++)
			if (buffb[i] == ' ')
				buffb[i] = '\t';
		sprintf(cmnd2, "%s\t\n", buffb);
		for (i = 0; i < strlen(cmnd2); i++) {
			j = k = 0;
			if (cmnd2[i] == '+') {
				while (cmnd2[++i] != '=')
					buffa[j++] = cmnd2[i];
				buffa[j] = 0;
				while (cmnd2[++i] != '\t' && cmnd2[i] != '\n' && cmnd2[i] != 0)
					buffb[k++] = cmnd2[i];
				buffb[k] = 0;
				G_set_key_value(buffa, buffb, out_proj_keys);
			}
		}
		break;
	case LL:
		G_set_key_value("proj", "ll", out_proj_keys);
		G_set_key_value("ellps", spheroid, out_proj_keys);
		break;

	default:
		if (sph_check != 2) {
			G_set_key_value("proj", proj_out, out_proj_keys);
			G_set_key_value("ellps", spheroid, out_proj_keys);
			sprintf(tmp_buff, "%.10f", aa);
			G_set_key_value("a", tmp_buff, out_proj_keys);
			sprintf(tmp_buff, "%.10f", e2);
			G_set_key_value("es", tmp_buff, out_proj_keys);
			sprintf(tmp_buff, "%.10f", f);
			G_set_key_value("f", tmp_buff, out_proj_keys);
		} else {
			G_set_key_value("proj", proj_out, out_proj_keys);
			/* G_set_key_value ("ellps", "sphere", out_proj_keys); */
			sprintf(tmp_buff, "%.10f", radius);
			G_set_key_value("a", tmp_buff, out_proj_keys);
			G_set_key_value("es", "0.0", out_proj_keys);
			G_set_key_value("f", "0.0", out_proj_keys);
		}

		for (i = 0; i < NOPTIONS; i++) {
			if (TABLE[proj_index][i].ask == 1) {
				if (i == SOUTH) {
					sprintf(buff, "\nIs this %s ", DESC[i]);
					if (G_yes(buff, 0))
						G_set_key_value("south", "defined", out_proj_keys);
				} else if (i == GUAM) {
					sprintf(buff, "\nDo You Want to set %s ", DESC[i]);
					if (G_yes(buff, 1))
						G_set_key_value("guam", "defined", out_proj_keys);
				} else if (i == NOROT) {
					sprintf(buff, "\nDo You Want to %s ", DESC[i]);
					if (G_yes(buff, 1))
						G_set_key_value("no_rot", "defined", out_proj_keys);
				} else if (i == NOCUT) {
					sprintf(buff, "\nDo You Want to Show %s ", DESC[i]);
					if (G_yes(buff, 1))
						G_set_key_value("no_cut", "defined", out_proj_keys);
				} else if (i == NOSKEW) {
					sprintf(buff, "\nDo You Want to %s ", DESC[i]);
					if (G_yes(buff, 1))
						G_set_key_value("ns", "defined", out_proj_keys);
				} else if (i == NOUOFF) {
					sprintf(buff, "\nDo You Want to %s ", DESC[i]);
					if (G_yes(buff, 1))
						G_set_key_value("no_uoff", "defined", out_proj_keys);
				} else if (i == ROTCONV) {
					sprintf(buff, "\nCalculations by %s ", DESC[i]);
					if (G_yes(buff, 1))
						G_set_key_value("rot_conv", "defined", out_proj_keys);
				} else if (i == LOTSA) {
					sprintf(buff, "\nDo you want to set %s ", DESC[i]);
					if (G_yes(buff, 1))
						G_set_key_value("lotsa", "defined", out_proj_keys);
				} else {
					for (;;) {
						/* G_clear_screen(); */
						if (i < LATHIGH)
							    if (get_LL_stuff(1, i))
								break;
						if ((i > LONLOW) && i < LONHIGH)
							    if (get_LL_stuff(0, i))
								break;
						if (i == ZONE) {
							if ((Out_proj == PROJECTION_UTM) && (old_zone != 0)) {
								fprintf(stderr, "The UTM zone is now set to %d\n", old_zone);
								if (!G_yes("Would you want to change UTM zone", 0)) {
									fprintf(stderr, "zone information will not be updated\n");
									zone = old_zone;
									break;
								} else {
									fprintf(stderr, "But if you change zone, all the existing data will be interpreted \n");
									fprintf(stderr, "by projection software. GRASS will not automatically\n");
									fprintf(stderr, "re-project or even change the headers for existing maps\n");
									if (!G_yes("Would you still want to change UTM zone?", 0)) {
										zone = old_zone;
										break;
									}
								}
							}	/* UTM */
							if (get_zone())
								break;
						}	/* zone */
						if (i == KFACT)
							if (get_KFACT(proj_index))
								break;
						if (i == MFACT)
							if (get_MFACT(proj_index))
								break;
						if (i == MSFACT)
							if (get_MSFACT(proj_index))
								break;
						if (i == NFACT)
							if (get_NFACT(proj_index))
								break;
						if (i == QFACT)
							if (get_QFACT(proj_index))
								break;
						if (i == WFACT)
							if (get_WFACT(proj_index))
								break;
						if (i == X0)
							if (get_x0(proj_index))
								break;
						if (i == Y0)
							if (get_y0(proj_index))
								break;
						if (i == HEIGH)
							if (get_HEIGH(proj_index))
								break;
						if (i == AZIM)
							if (get_AZIM(proj_index))
								break;
						if (i == TILT)
							if (get_TILT(proj_index))
								break;
						if (i == SNUM)
							if (get_SNUM(proj_index))
								break;
						if (i == SPATH)
							if (get_SPATH(proj_index))
								break;
					}	/* for */
				}
			} else if (TABLE[proj_index][i].def_exists == 1) {	/* don't ask, use the default */
				if (i < NLLSTUFF)
					LLSTUFF[i] = TABLE[proj_index][i].deflt;
				if (i == KFACT)
					kfact = TABLE[proj_index][i].deflt;
				if (i == MFACT)
					mfact = TABLE[proj_index][i].deflt;
				if (i == MSFACT)
					msfact = TABLE[proj_index][i].deflt;
				if (i == NFACT)
					nfact = TABLE[proj_index][i].deflt;
				if (i == QFACT)
					qfact = TABLE[proj_index][i].deflt;
				if (i == WFACT)
					wfact = TABLE[proj_index][i].deflt;
				if (i == X0)
					x_false = TABLE[proj_index][i].deflt;
				if (i == Y0)
					y_false = TABLE[proj_index][i].deflt;
				if (i == HEIGH)
					heigh = TABLE[proj_index][i].deflt;
				if (i == AZIM)
					azim = TABLE[proj_index][i].deflt;
				if (i == TILT)
					tilt = TABLE[proj_index][i].deflt;
				if (i == SNUM)
					snum = (int) TABLE[proj_index][i].deflt;
				if (i == SPATH)
					spath = (int) TABLE[proj_index][i].deflt;
			} else
				continue;
			
			/* set_nonbool_opts */
			switch (i) {
			case LAT0:
				sprintf(tmp_buff, "%.10f", LLSTUFF[i]);
				G_set_key_value("lat_0", tmp_buff, out_proj_keys);
				break;
			case LON0:
				sprintf(tmp_buff, "%.10f", LLSTUFF[i]);
				G_set_key_value("lon_0", tmp_buff, out_proj_keys);
				break;
			case LAT1:
				sprintf(tmp_buff, "%.10f", LLSTUFF[i]);
				G_set_key_value("lat_1", tmp_buff, out_proj_keys);
				strcat(cmnd2, buff);
				break;
			case LON1:
				sprintf(tmp_buff, "%.10f", LLSTUFF[i]);
				G_set_key_value("lon_1", tmp_buff, out_proj_keys);
				break;
			case LAT2:
				sprintf(tmp_buff, "%.10f", LLSTUFF[i]);
				G_set_key_value("lat_2", tmp_buff, out_proj_keys);
				break;
			case LON2:
				sprintf(tmp_buff, "%.10f", LLSTUFF[i]);
				G_set_key_value("lon_2", tmp_buff, out_proj_keys);
				break;
			case LAT3:
				sprintf(tmp_buff, "%.10f", LLSTUFF[i]);
				G_set_key_value("lat_2", tmp_buff, out_proj_keys);
				break;
			case LON3:
				sprintf(tmp_buff, "%.10f", LLSTUFF[i]);
				G_set_key_value("lon_3", tmp_buff, out_proj_keys);
				break;
			case LATTS:
				sprintf(tmp_buff, "%.10f", LLSTUFF[i]);
				G_set_key_value("lat_ts", tmp_buff, out_proj_keys);
				break;
			case LATB:
				sprintf(tmp_buff, "%.10f", LLSTUFF[i]);
				G_set_key_value("lat_b", tmp_buff, out_proj_keys);
				break;
			case LONC:
				sprintf(tmp_buff, "%.10f", LLSTUFF[i]);
				G_set_key_value("lon_c", tmp_buff, out_proj_keys);
				break;
			case ALPHA:
				sprintf(tmp_buff, "%.10f", LLSTUFF[i]);
				G_set_key_value("alpha", tmp_buff, out_proj_keys);
				break;
			case THETA:
				sprintf(tmp_buff, "%.10f", LLSTUFF[i]);
				G_set_key_value("theta", tmp_buff, out_proj_keys);
				break;
			case OLONP:
				sprintf(tmp_buff, "%.10f", LLSTUFF[i]);
				G_set_key_value("o_lon_p", tmp_buff, out_proj_keys);
				break;
			case OLATP:
				sprintf(tmp_buff, "%.10f", LLSTUFF[i]);
				G_set_key_value("o_lat_p", tmp_buff, out_proj_keys);
				break;
			case ZONE:
				/*
				   if (Out_proj == 1) 
				   {
				   sprintf(tmp_buff, "%d", G_zone());
				   G_set_key_value ("zone", tmp_buff, out_proj_keys);
				   } else */  {
					sprintf(tmp_buff, "%d", zone);
					G_set_key_value("zone", tmp_buff, out_proj_keys);
					cellhd.zone = zone;
				}
				break;
			case KFACT:
				sprintf(tmp_buff, "%.10f", kfact);
				G_set_key_value("k_0", tmp_buff, out_proj_keys);
				break;
			case MFACT:
				sprintf(tmp_buff, "%.10f", mfact);
				G_set_key_value("m", tmp_buff, out_proj_keys);
				break;
			case MSFACT:
				sprintf(tmp_buff, "%.10f", msfact);
				G_set_key_value("M", tmp_buff, out_proj_keys);
				break;
			case NFACT:
				sprintf(tmp_buff, "%.10f", nfact);
				G_set_key_value("n", tmp_buff, out_proj_keys);
				break;
			case QFACT:
				sprintf(tmp_buff, "%.10f", qfact);
				G_set_key_value("q", tmp_buff, out_proj_keys);
				break;
			case WFACT:
				sprintf(tmp_buff, "%.10f", wfact);
				G_set_key_value("W", tmp_buff, out_proj_keys);
				break;
			case X0:
				sprintf(tmp_buff, "%.10f", x_false);
				G_set_key_value("x_0", tmp_buff, out_proj_keys);
				break;
			case Y0:
				sprintf(tmp_buff, "%.10f", y_false);
				G_set_key_value("y_0", tmp_buff, out_proj_keys);
				break;
			case HEIGH:
				sprintf(tmp_buff, "%.10f", heigh);
				G_set_key_value("h", tmp_buff, out_proj_keys);
				break;
			case AZIM:
				sprintf(tmp_buff, "%.10f", azim);
				G_set_key_value("azi", tmp_buff, out_proj_keys);
				break;
			case TILT:
				sprintf(tmp_buff, "%.10f", tilt);
				G_set_key_value("tilt", tmp_buff, out_proj_keys);
				break;
			case SNUM:
				sprintf(tmp_buff, "%d", snum);
				G_set_key_value("lsat", tmp_buff, out_proj_keys);
				break;
			case SPATH:
				sprintf(tmp_buff, "%d", spath);
				G_set_key_value("path", tmp_buff, out_proj_keys);
				break;
			default:
				break;
			}	/* switch i */
		}		/* for OPTIONS */
		break;
	}			/* switch proj_index */


	/* create the PROJ_INFO & PROJ_UNITS files, if required */

	G_write_key_value_file(path, out_proj_keys, &out_stat);
	if (out_stat != 0) {
		sprintf(buffb, "Error writing PROJ_INFO file: %s\n", path);
		G_fatal_error(buffb);
	}
	G_free_key_value(out_proj_keys);
	if (exist)
		G_free_key_value(old_proj_keys);

      write_units:
	G__file_name(path, "", UNIT_FILE, set_name);

	/* if we got this far, the user
	   ** already affirmed to write over old info
	   ** so if units file is here, remove it.
	 */
	if (access(path, 0) == 0) {
		sprintf(buff, "mv -f %s %s~", path, path);
		system(buff);
	}
	if (Out_proj == 0)
		leave(0);

	{
		in_unit_keys = G_create_key_value();

		switch (Out_proj) {
		case PROJECTION_UTM:
			G_set_key_value("unit", "meter", in_unit_keys);
			G_set_key_value("units", "meters", in_unit_keys);
			G_set_key_value("meters", "1.0", in_unit_keys);
			break;
		case PROJECTION_SP:
			G_set_key_value("unit", "foot", in_unit_keys);
			G_set_key_value("units", "feet", in_unit_keys);
			G_set_key_value("meters", "0.30479999", in_unit_keys);
			break;
		case PROJECTION_LL:
			G_set_key_value("unit", "degree", in_unit_keys);
			G_set_key_value("units", "degrees", in_unit_keys);
			G_set_key_value("meters", "1.0", in_unit_keys);
			break;
		default:
			if (proj_index != LL) {
				/* G_clear_screen(); */
				fprintf(stderr, "Enter plural form of units [meters]: ");
				G_gets(answer);
				if (strlen(answer) == 0) {
					G_set_key_value("unit", "meter", in_unit_keys);
					G_set_key_value("units", "meters", in_unit_keys);
					G_set_key_value("meters", "1.0", in_unit_keys);
				} else {
					G_strip(answer);
					npr = strlen(answer);
					for (i = 0; i < NUNITS; i++) {
						in_stat = min1(npr, strlen(UNITS[i].units));
						if (strncmp(UNITS[i].units, answer, in_stat) == 0) {
							unit_fact = UNITS[i].fact;
							break;
						}
					}
					if (i < NUNITS) {
#ifdef FOO
						if (proj_index == STP && !strcmp(answer, "feet")) {
							fprintf(stderr, "%cPROJECTION 99 State Plane cannot be in FEET.\n", 7);
							sprintf(buff, "rm -f %s", path);
							system(buff);	/* remove file */
							leave(SP_FATAL);
						}
#endif
						G_set_key_value("unit", UNITS[i].unit, in_unit_keys);
						G_set_key_value("units", answer, in_unit_keys);
						sprintf(buffb, "%.10f", unit_fact);
						G_set_key_value("meters", buffb, in_unit_keys);
					} else {
						while (1) {
							fprintf(stderr, "Enter singular for unit: ");
							G_gets(answer1);
							G_strip(answer1);
							if (strlen(answer1) > 0)
								break;
						}
						while (1) {
							fprintf(stderr, "Enter conversion factor from %s to meters: ", answer);
							G_gets(answer2);
							G_strip(answer2);
							if (!(strlen(answer2) == 0 || (1 != sscanf(answer2, "%lf", &unit_fact))))
								break;
						}
						G_set_key_value("unit", answer1, in_unit_keys);
						G_set_key_value("units", answer, in_unit_keys);
						sprintf(buffb, "%.10f", unit_fact);
						G_set_key_value("meters", buffb, in_unit_keys);
					}
				}
			} else {
				G_set_key_value("unit", "degree", in_unit_keys);
				G_set_key_value("units", "degrees", in_unit_keys);
				G_set_key_value("meters", "1.0", in_unit_keys);
			}
		}		/* switch */

		G_write_key_value_file(path, in_unit_keys, &out_stat);
		if (out_stat != 0) {
			sprintf(buffb, "Error writing into UNITS output file: %s\n", path);
			G_fatal_error(buffb);
		}
		G_free_key_value(in_unit_keys);
	}			/* if */

	G__put_window(&cellhd, "", "DEFAULT_WIND");
	fprintf(stderr, "\nProjection information has been recorded for this location\n\n");
	if (old_zone != zone) {
		fprintf(stderr, "The geographic region information in WIND is now obsolete\n");
		fprintf(stderr, "Run g.region -d to update it.\n");
	}
	leave(0);
}

int min1(int a, int b) {
	if (a > b)
		return b;
	else
		return a;
}

int leave(int n) {
	exit(n);
}
