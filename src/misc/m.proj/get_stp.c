#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include "gis.h"

int get_stp_code(int, char[]);
int get_stp_num(void);
int ask_fips(FILE *, int *, int *, int *);

void get_stp_proj(char string[])
{
	int code;

	if ((code = get_stp_num()) == 0)
		exit(0);
	if (get_stp_code(code, string) == 0)
		G_fatal_error("This should not happen see your system admin");
}

int get_stp_code(int code, char string[])
{
	char nad27[256], buff[256], *p;
	int gotit = 0, stp;
	FILE *fp;


	sprintf(nad27, "%s/etc/state27", G_gisbase());
	fp = fopen(nad27, "r");
	if (fp == NULL) {
		sprintf(buff, "Can not open NAD27 file %s", nad27);
		G_fatal_error(buff);
	}
	while (!gotit) {
		if (fgets(buff, 200, fp) == NULL)
			break;
		if (buff[0] != '#') {
			sscanf(buff, "%d:", &stp);
			if (stp == code) {
				p = strtok(buff, ":");
				p = strtok(NULL, "\n");
				while (*p == ' ')
					p++;
				sprintf(string, "%s", p);
				gotit = 1;
			}
		}
	}
	fclose(fp);
	return (gotit);
}

int get_stp_num(void)
{
	FILE *fipsfile;
	char FIPSfile[256], buff[256];
	int NUM_ZON, sfips, cfips, SFIPS = 0, CFIPS = 0;
	int record, icode, reccnt, special_case;
	char STabbr[50], COname[50];

	sprintf(FIPSfile, "%s/etc/FIPS.code", G_gisbase());


	for (;;) {

		fipsfile = fopen(FIPSfile, "r");
		if (fipsfile == NULL) {
			G_fatal_error("Can not open FIPS code file");
		}
		ask_fips(fipsfile, &SFIPS, &CFIPS, &special_case);
		if (special_case == -1) {
			fclose(fipsfile);
			return (0);
		}
/* combine SFIPS and CFIPS to make lookup */
/*DEBUG fprintf(stderr,"FIPS = %d %d\n",SFIPS,CFIPS); */

		for (record = 0;; ++record) {
			icode = 0;
			reccnt++;
			if (fgets(buff, 80, fipsfile) == NULL)
				break;
			sscanf(buff, "%d%d%s%s%d", &sfips, &cfips, STabbr, COname, &NUM_ZON);
/* compare for match */
			if (SFIPS == sfips && CFIPS == cfips) {
				icode = 1;
				break;
			}
		}		/* end file search */
		if (icode != 0)
			break;
		else {		/* no match */
			fprintf(stderr, "\nNo match of fips state %d county %d \n", SFIPS, CFIPS);
			fclose(fipsfile);
		}
	}

/**** SPECIAL CASE FOR MICHIGAN ****, could be mercator or lambert */
	if (SFIPS == 26) {
		if (special_case == 2)
			NUM_ZON = NUM_ZON + 10;
	}
/**** SPECIAL CASE FOR ALASKA *****  */
	if (SFIPS == 2) {
		NUM_ZON = NUM_ZON + special_case;
	}
	/* all done, good-bye */
	fclose(fipsfile);
	return (NUM_ZON);
}

int ask_fips(FILE * fp, int *s, int *c, int *sc)
{
	int ii, FIPS = 0, NUM_ZON, sfips, cfips;
	char STabbr[50], STabbr_prev[50], COname[50], answer[50], buff[256];
	char *Tmp_file1, *Tmp_file2, *a, *b;
	FILE *Tmp_fd1 = NULL, *Tmp_fd2 = NULL;
	int in_stat;
	struct Key_Value *sf_keys, *cf_keys;

	*sc = 0;
	*s = 0;
	*c = 0;
	Tmp_file1 = G_tempfile();
	if (NULL == (Tmp_fd1 = fopen(Tmp_file1, "w")))
		G_fatal_error("Cannot open temp file");
	Tmp_file2 = G_tempfile();
	if (NULL == (Tmp_fd2 = fopen(Tmp_file2, "w")))
		G_fatal_error("Cannot open temp file");

	while (fgets(buff, 80, fp) != NULL) {
		sscanf(buff, "%d%d%s%s%d", &sfips, &cfips, STabbr, COname, &NUM_ZON);
		if (strncmp(STabbr, STabbr_prev, 2) != 0) {
			fprintf(Tmp_fd1, "%s -- %d\n", STabbr, sfips);
			fprintf(Tmp_fd2, "%d:%s\n", sfips, STabbr);
		}
		sprintf(STabbr_prev, "%s", STabbr);
	}
	fclose(Tmp_fd1);
	fclose(Tmp_fd2);

	sf_keys = G_read_key_value_file(Tmp_file2, &in_stat);
	if (in_stat != 0)
		G_fatal_error("reading sf key_value temp file");

	for (;;) {
		fprintf(stderr, "\nSpecify State FIPS code\n");
		fprintf(stderr, "Enter 'list' for the list of states with corresponding FIPS codes\n");
		fprintf(stderr, ">");
		while (!G_gets(answer)) {
			fprintf(stderr, "\nSpecify State FIPS code\n");
			fprintf(stderr, "Enter 'list' for the list of states with corresponding FIPS codes\n");
		}
		G_strip(answer);
		if (strlen(answer) == 0) {
			*sc = -1;
			return 1;
		}
		if (strncmp(answer, "list", 4) == 0) {
			if (isatty(1))
				sprintf(buff, "more %s", Tmp_file1);
			else
				sprintf(buff, "cat %s", Tmp_file1);
			system(buff);
		} else {
			a = G_find_key_value(answer, sf_keys);
			if (a == NULL)
				fprintf(stderr, "\nInvalid State FIPS code\n");
			else
				break;
		}
	}
	rewind(fp);

	sscanf(answer, "%d", s);

	FIPS = *s;
/**** SPECIAL CASE FOR MICHIGAN ****, could be mercator or lambert */
	if (FIPS == 26) {
		fprintf(stderr, "\nFor Michigan select- 1- East to West\n");
		fprintf(stderr, "                     2- North to South\n: ");
		ii = 0;
		for (;;) {
			fprintf(stderr, "\nFor Michigan select- 1- East to West\n");
			fprintf(stderr, "                     2- North to South\n: ");
			while (!G_gets(answer)) {
				fprintf(stderr, "\nFor Michigan select- 1- East to West\n");
				fprintf(stderr, "                     2- North to South\n: ");
			}
			G_strip(answer);
			if (strlen(answer) == 0) {
				*sc = -1;
				return 1;
			}
			sscanf(answer, "%d", &ii);
			if (ii != 1 && ii != 2)
				fprintf(stderr, "\n Invalid Entry\n ");
			else
				break;
		}
		*sc = ii;
	}
/**** SPECIAL CASE FOR ALASKA *****  */
	if (FIPS == 2) {
		ii = 0;
		for (;;) {
			fprintf(stderr, "\nFor Alaska enter the zone (1 through 9): ");
			while (!G_gets(answer)) {
				fprintf(stderr, "\nFor Alaska enter the zone (1 through 9): ");
			}
			G_strip(answer);
			if (strlen(answer) == 0) {
				*sc = -1;
				return 1;
			}
			sscanf(answer, "%d", &ii);
			if (ii < 1 || ii > 9)
				fprintf(stderr, "\n Invalid Entry\n ");
			else
				break;
		}
		*sc = ii;
	}
	unlink(Tmp_file1);
	unlink(Tmp_file2);

	Tmp_file1 = G_tempfile();
	if (NULL == (Tmp_fd1 = fopen(Tmp_file1, "w")))
		G_fatal_error("Cannot open temp file");
	Tmp_file2 = G_tempfile();
	if (NULL == (Tmp_fd2 = fopen(Tmp_file2, "w")))
		G_fatal_error("Cannot open temp file");

	while (fgets(buff, 80, fp) != NULL) {
		sscanf(buff, "%d%d%s%s%d", &sfips, &cfips, STabbr, COname, &NUM_ZON);
		if (sfips == *s) {
			fprintf(Tmp_fd1, "%s -- %d\n", COname, cfips);
			fprintf(Tmp_fd2, "%d:%s\n", cfips, COname);
		} /* ADDED THESE BRACKETS - BB 5/2000 */
	}
	fclose(Tmp_fd1);
	fclose(Tmp_fd2);

	cf_keys = G_read_key_value_file(Tmp_file2, &in_stat);
	if (in_stat != 0)
		G_fatal_error("reading cf key_value temp file");

	for (;;) {
		fprintf(stderr, "\nSpecify County FIPS code \n");
		fprintf(stderr, "Enter 'list' for the list of counties in %s with corresponding FIPS codes\n", a);
		fprintf(stderr, ">");
		while (!G_gets(answer)) {
			fprintf(stderr, "\nSpecify County FIPS code \n");
			fprintf(stderr, "Enter 'list' for the list of counties in %s with corresponding FIPS codes\n", a);
		}
		G_strip(answer);
		if (strlen(answer) == 0) {
			*sc = -1;
			return 1;
		}
		if (strncmp(answer, "list", 4) == 0) {
			if (isatty(1))
				sprintf(buff, "more %s", Tmp_file1);
			else
				sprintf(buff, "cat %s", Tmp_file1);
			system(buff);
		} else {
			b = G_find_key_value(answer, cf_keys);
			if (b == NULL)
				fprintf(stderr, "\nInvalid County FIPS code\n");
			else
				break;
		}
	}
	sscanf(answer, "%d", c);
	rewind(fp);
	unlink(Tmp_file1);
	unlink(Tmp_file2);
	return 0;
}
