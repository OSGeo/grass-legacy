#include <string.h>
#include <math.h>
#include "gis.h"
#include "geo.h"
#include "local_proto.h"

char answer[200];

int get_KFACT(int indx)
{
	sprintf(answer, "Enter %s ", DESC[KFACT]);
	kfact = prompt_num_double(answer, TABLE[indx][KFACT].deflt, 1);
	return (1);
}

int get_MFACT(int indx)
{
	sprintf(answer, "Enter %s ", DESC[MFACT]);
	mfact = prompt_num_double(answer, TABLE[indx][MFACT].deflt, 1);
	return (1);
}

int get_MSFACT(int indx)
{
	sprintf(answer, "Enter %s ", DESC[MSFACT]);
	msfact = prompt_num_double(answer, TABLE[indx][MSFACT].deflt, 1);
	return (1);
}

int get_NFACT(int indx)
{
	sprintf(answer, "Enter %s ", DESC[NFACT]);
	nfact = prompt_num_double(answer, TABLE[indx][NFACT].deflt, 1);
	return (1);
}

int get_QFACT(int indx)
{
	sprintf(answer, "Enter %s ", DESC[QFACT]);
	qfact = prompt_num_double(answer, TABLE[indx][QFACT].deflt, 1);
	return (1);
}

int get_WFACT(int indx)
{
	sprintf(answer, "Enter %s ", DESC[WFACT]);
	wfact = prompt_num_double(answer, TABLE[indx][WFACT].deflt, 1);
	return (1);
}


int get_x0(int indx)
{
	sprintf(answer, "Enter %s ", DESC[X0]);
	x_false = prompt_num_double(answer, TABLE[indx][X0].deflt, 1);
	return (1);
}

int get_y0(int indx)
{
	sprintf(answer, "Enter %s ", DESC[Y0]);
	y_false = prompt_num_double(answer, TABLE[indx][Y0].deflt, 1);
	return (1);
}

int get_HEIGH(int indx)
{
	sprintf(answer, "Enter %s ", DESC[HEIGH]);
	heigh = prompt_num_double(answer, TABLE[indx][HEIGH].deflt, 1);
	return (1);
}

int get_AZIM(int indx)
{
	sprintf(answer, "Enter %s ", DESC[AZIM]);
	azim = prompt_num_double(answer, TABLE[indx][AZIM].deflt, 1);
	return (1);
}

int get_TILT(int indx)
{
	sprintf(answer, "Enter %s ", DESC[TILT]);
	tilt = prompt_num_double(answer, TABLE[indx][TILT].deflt, 1);
	return (1);
}

int get_SNUM(int indx)
{
	sprintf(answer, "Enter %s ", DESC[SNUM]);
	snum = prompt_num_int(answer, (int) TABLE[indx][SNUM].deflt, 1);
	return (1);
}

int get_SPATH(int indx)
{
	sprintf(answer, "Enter %s ", DESC[SPATH]);
	spath = prompt_num_int(answer, (int) TABLE[indx][SPATH].deflt, 1);
	return (1);
}

int get_zone(void)
{
	int first_time = 1;

	zone = -1;
	while ((zone < 0) || (zone > 60)) {
		if (first_time)
			first_time = 0;
		else
			fprintf(stdout, "Invalid zone! Try Again:\n");
		sprintf(answer, "Enter Zone");
		zone = prompt_num_int(answer, 0, 0);
	}
	return (1);
}


/*
   *    Get the Prime Meridian value and std parallel value
   **** */
int get_LL_stuff(int lat, int index)
{
	char buff[256];

	/*  get LONCEN value arguements */
	if (TABLE[proj_index][index].def_exists == 1) {
		if (lat == 1) {
			G_format_northing(TABLE[proj_index][index].deflt, buff, PROJECTION_LL);
			fprintf(stderr, "\n    Enter %s (%s) :", DESC[index], buff);
		} else {
			G_format_easting((TABLE[proj_index][index].deflt), buff, PROJECTION_LL);
			fprintf(stderr, "\n    Enter %s (%s) :", DESC[index], buff);
		}
		G_gets(answer);
		if (strlen(answer) == 0) {
			LLSTUFF[index] = TABLE[proj_index][index].deflt;
			return (1);
		}
	} else {
		fprintf(stderr, "\n    Enter %s :", DESC[index]);
		G_gets(answer);
		if (strlen(answer) == 0) {
			LLSTUFF[index] = 0.0;
			return (0);
		}
	}
	if (lat == 1) {
		if (!get_deg(answer, 1)) {
			return (0);
		}
	} else {
		if (!get_deg(answer, 0)) {
			return (0);
		}
	}
	sscanf(answer, "%lf", &(LLSTUFF[index]));
	return (1);
}

double prompt_num_double(char *str, double deflt, int is_default)
{
	char answer[300];
	double tmp;

	while (1) {
		if (is_default)
			fprintf(stderr, "\n%s [%.10f]: ", str, deflt);
		else
			fprintf(stderr, "\n%s: ", str);

		G_gets(answer);
		G_strip(answer);
		if (strlen(answer) == 0 && is_default)
			return deflt;
		else if (sscanf(answer, "%lf", &tmp) == 1)
			break;
	}
	return tmp;
}


int prompt_num_int(char *str, int deflt, int is_default)
{
	char answer[300];
	int tmp;

	while (1) {
		if (is_default)
			fprintf(stderr, "\n%s [%d]: ", str, deflt);
		else
			fprintf(stderr, "\n%s: ", str);

		G_gets(answer);
		G_strip(answer);
		if (strlen(answer) == 0 && is_default)
			return deflt;
		else if (1 == sscanf(answer, "%d", &tmp))
			break;
	}
	return tmp;
}
