/* created by: Irina Kosinovsky 
   changed by: Morten Hulden <morten@ngb.se> October 1999, support for additional projections 
*/
#include <string.h>
#include <unistd.h>
#include "geo.h"
#include "gis.h"
#include "projects.h"
#define MAIN
#include "local_proto.h"

int main(int argc, char *argv[])
{
	struct pj_info info_in;
	struct pj_info info_out;
	int proj_changed_in = 0;
	int proj_changed_out = 0;
	char tmp_buf1[50], tmp_buf2[50];

	conv_typ = conv_way = 0;
	input_typ = output_typ = 1;
	G_clear_screen();

	/* Initialize all the globals here */
	sprintf(proj_name_in, "None");
	sprintf(proj_name_out, "None");
	sprintf(ellps_name_in, "None");
	sprintf(ellps_name_out, "None");

	init_table();
	init_unit_table();
	init_used_table();
	
	/*  user main menu */
	for (;;) {
		G_clear_screen();
		fprintf(stderr, "                   Coordinate Conversions\n\n\n");
		fprintf(stderr, "    1- Conversion       2- Input/Output Selection\n");
		fprintf(stderr, "    3- Quit\n\n");

		fprintf(stderr, "    Enter your selection [3] : ");
		G_gets(answer);

		if (strlen(answer) == 0)
			conv_typ = 3;
		else {
			if (*answer == '1' || *answer == '2' || *answer == '3')
				conv_typ = atoi(answer);
			else
				conv_typ = 0;
		}

		/*  actions   */
		switch (conv_typ) {
		case 1:
			parms_in[0] = '\0';
			proj_changed_in = process(1, parms_in, proj_name_in, proj_title_in, ellps_name_in, &radius_in, USED_in, units_in);
			if (proj_changed_in == 1) {
				pj_zero_proj(&info_in);
				if (pj_get_string(&info_in, parms_in) < 0)
					G_fatal_error("Cannot initialize proj_info_in");
			}
			parms_out[0] = '\0';
			proj_changed_out = process(2, parms_out, proj_name_out, proj_title_out, ellps_name_out, &radius_out, USED_out, units_out);
			if (proj_changed_out == 1) {
				pj_zero_proj(&info_out);
				if (pj_get_string(&info_out, parms_out) < 0)
					G_fatal_error("Cannot initialize proj_info_out");
			}
			proj_index_in = get_proj_index(proj_name_in);
			proj_index_out = get_proj_index(proj_name_out);

			if (input_typ == 1) {	/* keyboard input */
				for (;;) {
					if (proj_index_in != LL) {
						if (!get_enz())
							break;
					} else {
						if (!get_ll())
							break;
					}
					if (proj_index_in == LL) {
						X = LON;
						Y = LAT;
					} else {
						X = EAS;
						Y = NOR;
					}

					if ((proj_index_in == proj_index_out) && (proj_index_out == LL)) {
						LON_res = LON;
						LAT_res = LAT;
						cur_LAT = LAT;
						cur_LON = LON;
						/*DMS (1); */ /*  convert LAT/LON to dd mm ss.ss */
					} else if (pj_do_proj(&X, &Y, &info_in, &info_out) < 0)
						G_fatal_error("Error in pj_do_proj()");

					if (proj_index_out != LL) {
						EAS_res = X;
						NOR_res = Y;
						cur_LAT = LAT;	/* latitude printed out with results */
						cur_LON = LON;	/* longitude printed out with results */
						/*DMS (1); */ /*  convert LAT/LON to dd mm ss.ss */
					} else {
						LON_res = X;
						LAT_res = Y;
						cur_LAT = LAT_res;	/* latitude printed out with results */
						cur_LON = LON_res;	/* longitude printed out with results */
						/*DMS (0); */ /*  convert LAT_res/LON_res to dd mm ss.ss */
					}

					Write_results(0);
				}	/*end for loop */
			}
			if (input_typ == 2) {	/* file input */
				rec_cnt = 0;
				G_clear_screen();
				rec_cnt = 0;
				for (;;) {
					if (fgets(buff, 80, In_file) == NULL)
						break;
					rec_cnt++;

					if (proj_index_in != LL)
						sscanf(buff, "%lf%lf", &EAS, &NOR);
					else {
						if (sscanf(buff, "%s%s", tmp_buf1, tmp_buf2) == 2);
						{
							G_scan_easting(tmp_buf1, &LON, PROJECTION_LL);
							G_scan_northing(tmp_buf2, &LAT, PROJECTION_LL);
						}
					}
					if (proj_index_in == LL) {
						X = LON;
						Y = LAT;
					} else {
						X = EAS;
						Y = NOR;
					}

					if ((proj_index_in == proj_index_out) && (proj_index_out == LL)) {
						LON_res = LON;
						LAT_res = LAT;
						cur_LAT = LAT;
						cur_LON = LON;
						/*DMS (1); */ /*  convert LAT/LON to dd mm ss.ss */
					} else if (pj_do_proj(&X, &Y, &info_in, &info_out) < 0)
						G_fatal_error("Error in pj_do_proj()");

					if (proj_index_out != LL) {
						EAS_res = X;
						NOR_res = Y;
						cur_LAT = LAT;
						cur_LON = LON;
						/*DMS (1); */ /*  convert LAT/LON to dd mm ss.ss */
					} else {
						LON_res = X;
						LAT_res = Y;
						cur_LAT = LAT_res;
						cur_LON = LON_res;
						/*DMS (0); */ /*  convert LAT_res/LON_res to dd mm ss.ss */
					}
					Write_results(0);
				}	/* end for loop */

				Write_results(1);
				fclose(In_file);
				input_typ = output_typ = 1;
				fprintf(stderr, "\tresetting to default screen & keyboard\n");
				sleep(2);
			}
			break;

		case 2:	/*  input/output selection */
			conv_way = 0;
			while (conv_way != 3) {
				G_clear_screen();
				fprintf(stderr, "\n\n\t1- Input selection  2- Output selection 3- Main Menu\n\n");
				fprintf(stderr, "    Enter your selection [3] : ");
				G_gets(answer);
				if (strlen(answer) == 0)
					conv_way = 3;
				else if (*answer != '1' && *answer != '2' && *answer != '3')
					conv_way = 3;
				else
					conv_way = atoi(answer);

				/*  actions   */
				switch (conv_way) {
					/* --------------------- Input Selection ------------------------ */
				case 1:
					input_typ = 0;
					while (input_typ == 0) {
						G_clear_screen();
						fprintf(stderr, "\n\n\t1- Keyboard           2- File\n\n");
						fprintf(stderr, "\tEnter your selection [1] : ");
						G_gets(answer);
						if (strlen(answer) == 0)
							input_typ = 1;
						else if (*answer != '1' && *answer != '2')
							input_typ = 3;
						else
							input_typ = atoi(answer);

						/*  actions   */
						switch (input_typ) {
						case 1:	/*  Keyboard */
							break;

						case 2:	/*  File */
							get_file(1);
							break;

						default:	/* invalid option */
							fprintf(stderr, "  *** INVALID option *** \n");
							sleep(2);
							input_typ = 0;
							break;
						}
					}
					break;

					/* --------------------- Output Selection ------------------------ */
				case 2:
					output_typ = 0;
					while (output_typ == 0) {
						G_clear_screen();
						fprintf(stderr, "\n\n\t1- Screen             2- File\n\n");
						fprintf(stderr, "\tEnter your selection [1] : ");
						G_gets(answer);
						if (strlen(answer) == 0)
							output_typ = 1;
						else if (*answer != '1' && *answer != '2')
							output_typ = 3;
						else
							output_typ = atoi(answer);

						/*  actions   */
						switch (output_typ) {
						case 1:	/*  Screen */
							break;
						case 2:	/*  File */
							get_file(2);
							break;
						default:	/* invalid option */
							fprintf(stderr, "  *** INVALID option *** \n");
							sleep(2);
							output_typ = 0;
							break;
						}
					}
					break;

					/* --------------------- Return to Main Menu ------------------------ */
				case 3:
					break;

					/* --------------------- Invalid option ---------------------------- */
				default:	/* invalid option */
					fprintf(stderr, "  *** INVALID option *** \n");
					sleep(2);
					input_typ = 0;
					output_typ = 0;
					break;
				}
			}
			break;

			/*  quit  */
		case 3:
			G_clear_screen();
			/*
			   if (!get_mem(2))
			   {
			   fprintf(stderr,"\n Memory File could not be found/created ");
			   sleep(2);
			   G_clear_screen();
			   }
			 */
			exit(0);

			/* invalid option */
		default:
			fprintf(stderr, " *** INVALID option ***\n");
			sleep(2);
			break;
		}
	}
}

int min1(int a, int b)
{
	if (a < b)
		return a;
	else
		return b;
}
