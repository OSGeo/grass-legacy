#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <math.h>
#include <string.h>
#include  "gis.h"
#include "projects.h"
#define MAIN

#define PERMANENT "PERMANENT"
#define MAX_PARGS 100

int pj_get_kv(info, in_proj_keys, in_units_keys)
struct pj_info *info;
struct Key_Value *in_proj_keys, *in_units_keys;

{
	char *opt_in[MAX_PARGS];
	char *str;
	int  nopt1;
	int i, south;
	int nsize;
	char buffa[300], factbuff[50];
	char proj_in[50];
	PJ *pj;

        proj_in[0] = '\0';
	info->zone = 0;
	info->meters = 1.0;
	info->proj[0] = '\0';

	str = G_find_key_value("meters", in_units_keys);
	if (str != NULL) {
		strcpy(factbuff, str);
		if (strlen(factbuff) > 0)
			sscanf(factbuff, "%lf", &(info->meters));
	}
	str = G_find_key_value("name", in_proj_keys);
	if (str != NULL) {
		sprintf(proj_in, "%s", str);
	}
	str = G_find_key_value("proj", in_proj_keys);
	if (str != NULL) {
		sprintf(info->proj, "%s", str);
	}
	str = G_find_key_value("zone", in_proj_keys);
	if (str != NULL) {
		if (sscanf(str, "%d", &(info->zone)) != 1) {
			sprintf(buffa, "Invalid zone %s specified", str);
			G_fatal_error(buffa);
		}
	}
	if (strlen(info->proj) <= 0)
		sprintf(info->proj, "ll");

	if (strncmp(proj_in, "Lat", 3) == 0 
            || strcmp(info->proj,"ll") == 0) {
		return 1;
	}
	nopt1 = 0;
	south = 0;
	/* if zone is negative, write abs(zone) and define south */
	for (i = 0; i < in_proj_keys->nitems; i++) {
                /* the name parameter is just for grasses use */
                if( strncmp(in_proj_keys->key[i],"name",4) == 0 ) {
                    continue;

		/* Don't pass through any ellps= settings if we have the
                   underlying parameters.  */

		} else if( strncmp(in_proj_keys->key[i], "ellps", 5) == 0 ) {
                    if( G_find_key_value("a", in_proj_keys) != NULL
                        && (G_find_key_value("b", in_proj_keys) != NULL
                            || G_find_key_value("es", in_proj_keys) != NULL
                            || G_find_key_value("rf", in_proj_keys) != NULL))
                        continue;
                    else
                        sprintf(buffa, "%s=%s", 
                                in_proj_keys->key[i], in_proj_keys->value[i]);
                } else if (strncmp(in_proj_keys->key[i], "south", 5) == 0) {
			sprintf(buffa, "south");
			south = 1;
		} else if (strncmp(in_proj_keys->key[i], "zone", 4) == 0) {
			if (info->zone < 0) {
				info->zone = -info->zone;

				if (!south)
					sprintf(buffa, "south");
			}
			sprintf(buffa, "zone=%d", info->zone);
		} else if (strncmp(in_proj_keys->key[i], "ns", 2) == 0)
			sprintf(buffa, "ns");

		else if (strncmp(in_proj_keys->key[i], "no_rot", 6) == 0)
			sprintf(buffa, "no_rot");

		else if (strncmp(in_proj_keys->key[i], "no_uoff", 7) == 0)
			sprintf(buffa, "no_uoff");

		else if (strncmp(in_proj_keys->key[i], "rot_conv", 8) == 0)
			sprintf(buffa, "rot_conv");

		else if (strncmp(in_proj_keys->key[i], "no_cut", 6) == 0)
			sprintf(buffa, "no_cut");

		else if (strncmp(in_proj_keys->key[i], "guam", 4) == 0)
			sprintf(buffa, "guam");

		else if (strncmp(in_proj_keys->key[i], "lotsa", 5) == 0)
			sprintf(buffa, "lotsa");

		else if (strncmp(in_proj_keys->key[i], "no_defs", 7) == 0)
			sprintf(buffa, "no_defs");
		/* workaround because pj_ellps.c does not include sphere as valid ellipsoid */
		else 
			sprintf(buffa, "%s=%s", 
                                in_proj_keys->key[i], in_proj_keys->value[i]);

                nsize = strlen(buffa);
                if (!(opt_in[nopt1++] = (char *) malloc(nsize + 1)))
                    G_fatal_error("cannot allocate options\n");
                sprintf(opt_in[nopt1-1], buffa);
	}

        if (!(pj = pj_init(nopt1, opt_in))) {
            fprintf(stderr, "cannot initialize pj\ncause: ");
            fprintf(stderr,"%s\n",pj_strerrno(pj_errno));
            return -1;
        }
        info->pj = pj;

	return 1;
}


int pj_get_string(info, str)
struct pj_info *info;
char *str;
{
	char *opt_in[MAX_PARGS];
	char *s;
	int nopt = 0;
	int nsize;
	char zonebuff[50];
	PJ *pj;

	info->zone = 0;
	info->proj[0] = '\0';
	info->meters = 1.0;

	if ((str == NULL) || (str[0] == '\0')) {
		sprintf(info->proj, "ll");
		return 1;
	}
	s = str;
	while (s = strtok(s, " \t\n")) {
		if (strncmp(s, "+unfact=", 8) == 0) {
			s = s + 8;
			info->meters = atof(s);
		} else {
			if (strncmp(s, "+", 1) == 0)
				++s;
			if (nsize = strlen(s)) {
				if (nopt >= MAX_PARGS) {
					fprintf(stderr, "nopt = %d, s=%s\n", nopt, str);
					G_fatal_error("Option input overflowed option table");
				}
				if (!(opt_in[nopt] = (char *) malloc(nsize + 1)))
					G_fatal_error("Option input memory failure");
				sprintf(opt_in[nopt++], s);

				if (strncmp("proj=", s, 5) == 0) {
					sprintf(info->proj, "%s", s + 5);
					if (strncmp(info->proj, "ll", 2) == 0) {
						sprintf(info->proj, "ll");
						return 1;
					}
				}
				if (strncmp("zone=", s, 5) == 0) {
					sprintf(zonebuff, "%s", s + 5);
					sscanf(zonebuff, "%d", &(info->zone));
				}
			}
		}
		s = 0;
	}

	if (strncmp(info->proj, "ll", 2) != 0) {
		if (!(pj = pj_init(nopt, opt_in))) {
			fprintf(stderr, "cannot initialize pj\ncause: ");
			fprintf(stderr, "%s\n", pj_strerrno(pj_errno));
			return -1;
		}
		info->pj = pj;
	}
	return 1;
}


int pj_zero_proj(info)
struct pj_info *info;
{

	info->zone = 0;
	info->proj[0] = '\0';
	info->meters = 1.0;

	return 0;
}
