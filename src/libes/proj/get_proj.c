#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <math.h>
#include <string.h>
#include  "gis.h"
#include "projects.h"
#define MAIN

/* GRASS relative location of datum conversion lookup tables */
#define GRIDDIR "/etc/nad"
/* Finder function for datum conversion lookup tables */
#define FINDERFUNC set_proj_lib   
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

	nopt1 = 0;
	south = 0;
	/* if zone is negative, write abs(zone) and define south */
	for (i = 0; i < in_proj_keys->nitems; i++) {
                /* the name parameter is just for grasses use */
                if( strncmp(in_proj_keys->key[i],"name",4) == 0 ) {
                    continue;

		/* Don't need dx dy dz if datum= is specified; PROJ.4 will 
		 * get these from pj_datums.c file; otherwise dx dy dz used to
		 * form +towgs84 parameter after end of this loop PK */

                } else if( strncmp(in_proj_keys->key[i], "dx", 2) == 0
			   || strncmp(in_proj_keys->key[i], "dy", 2) == 0
		           || strncmp(in_proj_keys->key[i], "dz", 2) == 0 ) {
		    continue;
		
		   
                /* Don't pass through underlying parameters if we have ellps=
		 * settings; don't pass through ellps= settings if we have
		 * datum= settings. Changed from old logic by PK to use only
		 * top-level data sources if possible (i.e. within PROJ.4.)
		 * 
		 * N.B. This will be changed back to passing parameters when
		 * GRASS datum table format is updated
		 * 
		 * If there are many underlying ellipsoid parameters pass 
		 * them all through and let proj sort it out. */

                } else if( strncmp(in_proj_keys->key[i], "a", 1) == 0
			   || strncmp(in_proj_keys->key[i], "b", 1) == 0
			   || strncmp(in_proj_keys->key[i], "es", 2) == 0
			   || strncmp(in_proj_keys->key[i], "f", 1) == 0
		           || strncmp(in_proj_keys->key[i], "rf", 2) == 0 ) {
		    if( G_find_key_value("ellps", in_proj_keys) != NULL
		        || G_find_key_value("datum", in_proj_keys) != NULL)
		        continue;
                    else
                        sprintf(buffa, "%s=%s", 
                                in_proj_keys->key[i], in_proj_keys->value[i]);             	       		

		} else if( strncmp(in_proj_keys->key[i], "ellps", 5) == 0 ) {
                    if( G_find_key_value("datum", in_proj_keys) != NULL)
                        continue;
                    else
                        sprintf(buffa, "%s=%s", 
                                in_proj_keys->key[i], in_proj_keys->value[i]);

		/* PROJ.4 uses latlong instead of ll as 'projection name' */
		   
		} else if( strncmp(in_proj_keys->key[i], "proj", 4) == 0 ) {
		    if( strncmp(G_find_key_value("proj", in_proj_keys), "ll", 2) == 0)
                        sprintf(buffa, "%s=latlong", in_proj_keys->key[i]);
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

        /* If datum= is not specified but dx dy dz are, we can use them to
         * form the towgs84 parameter for datum shift PK */
        if( G_find_key_value("datum", in_proj_keys) == NULL
	    && G_find_key_value("dx", in_proj_keys) != NULL
	    && G_find_key_value("dy", in_proj_keys) != NULL
	    && G_find_key_value("dz", in_proj_keys) != NULL ) {
	        sprintf(buffa, "towgs84=%s,%s,%s",
			G_find_key_value("dx", in_proj_keys),
			G_find_key_value("dy", in_proj_keys),
			G_find_key_value("dz", in_proj_keys) );      
                nsize = strlen(buffa);
                if (!(opt_in[nopt1++] = (char *) malloc(nsize + 1)))
                    G_fatal_error("cannot allocate options\n");
                sprintf(opt_in[nopt1-1], buffa);
        }
   
        /* Set finder function for locating datum conversion tables PK */
        pj_set_finder( FINDERFUNC ); 

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
    char zonebuff[50], buffa[300];
    PJ *pj;

    info->zone = 0;
    info->proj[0] = '\0';
    info->meters = 1.0;

    if ((str == NULL) || (str[0] == '\0')) {
        /* Null Pointer or empty string is supplied for parameters, 
         * implying latlong projection; just need to set proj 
         * parameter and call pj_init PK */
        sprintf(info->proj, "ll");
        sprintf(buffa, "proj=latlong");
        nsize = strlen(buffa);
        if (!(opt_in[nopt] = (char *) malloc(nsize + 1)))
            G_fatal_error("Option input memory failure");
        sprintf(opt_in[nopt++], buffa);
    } else {
        /* Parameters have been provided; parse through them but don't
         * bother with most of the checks in pj_get_kv; assume the
         * programmer knows what he / she is doing when using this 
         * function rather than reading a PROJ_INFO file       PK */
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

                    if (strncmp("zone=", s, 5) == 0) {
                        sprintf(zonebuff, "%s", s + 5);
                        sscanf(zonebuff, "%d", &(info->zone));
                    }
                   
                    if (strncmp("proj=", s, 5) == 0) {
                        sprintf(info->proj, "%s", s + 5);
                        if (strncmp(info->proj, "ll", 2) == 0)
                            sprintf(buffa, "proj=latlong");
                        else
                            sprintf(buffa, s);
                    } else {
                        sprintf(buffa, s);
                    }
                    nsize = strlen(buffa);                                
                    if (!(opt_in[nopt] = (char *) malloc(nsize + 1)))
                        G_fatal_error("Option input memory failure");
                    sprintf(opt_in[nopt++], buffa);
                }
            }
            s = 0;
        }
    }

    /* Set finder function for locating datum conversion tables PK */
    pj_set_finder( FINDERFUNC );
   
    if (!(pj = pj_init(nopt, opt_in))) {
        fprintf(stderr, "cannot initialize pj\ncause: ");
        fprintf(stderr, "%s\n", pj_strerrno(pj_errno));
        return -1;
    }
    info->pj = pj;

    return 1;
}

/* This function is a bit useless now */

int pj_zero_proj(info)
struct pj_info *info;
{

	info->zone = 0;
	info->proj[0] = '\0';
	info->meters = 1.0;

	return 0;
}

/* set_proj_lib()
 * 'finder function' for use with PROJ.4 pj_set_finder() function */

const char * set_proj_lib (const char *name)
{
    const char *gisbase = G_gisbase();
    static char *buf = NULL;
    static int   buf_len;
    size_t len = strlen(gisbase) + sizeof(GRIDDIR) + strlen(name) + 1;

    if( buf_len < len )
    {
        if( buf != NULL )
            G_free( buf );
        buf_len = len + 20;
        buf = G_malloc(buf_len);
    }
   
    sprintf (buf, "%s%s/%s", gisbase, GRIDDIR, name);
   
    return buf; 
}
