
/*  @(#)a_b_main.c	2.1  6/26/87  */
#include <stdio.h>
#include <Vect.h>
#include "gis.h"

#define MAIN
#include "dlg.h"
#include "dig_head.h"

#define         B_DIG           "dig"
#define         A_DIG           "dig_ascii"
#define         A_DLG           "dlg"
#define         ATT             "dig_att"
#define         PLUS            "dig_plus"

struct dlg dlgstr;

doit2 (dig_name, a_dlg_name, add_att_name, off_att, force_lines, attopt, lattopt, subjopt, discard_univ, old_attr)
char * dig_name, * a_dlg_name, * add_att_name;
int off_att, force_lines, discard_univ, old_attr;
char *attopt, *lattopt, *subjopt;
{
	char *mapset;
	char *mymapset;
	char att_name[300];
	char a_dlgfile[300];
	struct Map_info Map;

	int i,x,y,z,match;
	FILE *dlg, *att;
	FILE *attfile, *lattfile, *subjfile;
	struct dig_head head;
	FILE *fopen();
	char filename[300];
	char  *rindex();
	struct Categories attcat, lattcat, subjcat;
	char buff[1024], label[1024], *cat_label;
	long num;
	CELL cat, lastcat;

	/* Print warning */
	if ((mapset = G_find_file2 (A_DLG, a_dlg_name, "")) == NULL)
	{
		fprintf (stderr, "Ascii DLG file <%s> not found.\n", a_dlg_name);
		exit(-1);
	}
	G__file_name (a_dlgfile, A_DLG, a_dlg_name, mapset);
	mymapset = G_mapset();

	G__file_name (att_name, ATT, dig_name, mymapset);
	G__make_mapset_element(ATT);


	if ( (dlg = fopen(a_dlgfile, "r")) == NULL)
	{
		printf("Can't find %s\n", "dlg");
		exit(-1);
	}

	/* Read the header section */
	if (read_dlg_head(dlg,&dlgstr) == -1)
		G_fatal_error ("Error reading DLG header");


	if (0 > Vect_open_new (&Map, dig_name))
		G_fatal_error ("Not able to create vector file <%s>");

	if (! (att = fopen(att_name, "w")) )
		G_fatal_error ("Not able to create att file <%s>");

	hd_dlg_to_dig (&dlgstr, &head);

	Vect_copy_head_data (&head, &Map.head);

	/* check for attribute files and then subject file */
	G_init_cats((CELL)0, "", &attcat);
	G_init_cats((CELL)0, "", &lattcat);


	/* start with last cat as 0 */
	lastcat = 0;
	/* check if user gave a subject file name */
	if (subjopt != NULL){
		/* check if it will be a new subject file */
		if (G_find_file("SUBJ", subjopt, G_mapset()) == NULL){
			subjfile = G_fopen_new("SUBJ", subjopt);
			G_init_cats((CELL)0, "", &subjcat);
			G_set_cat(0,"No data", &subjcat);
		} else{ /* read an existing subject file */
			if(G__read_cats("SUBJ", subjopt, G_mapset(), &subjcat, 1) < 0)
				G_fatal_error("Error reading SUBJ file");
			lastcat = subjcat.num;
		}
	} else{ /* no subject file given so just use cats struct */
		G_init_cats((CELL)0, "", &subjcat);
		G_set_cat(0,"No data", &subjcat);
	}
	/* process any attribute files into subjcat structure */
	/* Read all category names */

	if (attopt != NULL){
		if((attfile = G_fopen_old("dlg", attopt, G_mapset())) == NULL)
			G_fatal_error("Can't open att file.");
		if (discard_univ)
			G_getl(buff, sizeof buff, attfile);
		for (x=0;;x++) {
			match = 0; /* don't expect to find a match in subjcat */
			if (G_getl(buff, sizeof buff, attfile) == 0)
				break;
			*label = 0;
			if (old_attr){
				if (sscanf (buff, "%ld %[^\n]", &num, label) < 1)
					G_fatal_error("Error reading attribute file");
			}else{	
				if (sscanf (buff, "%ld %d %d %[^\n]", &num, &y, &z, label) < 1)
					G_fatal_error("Error reading attribute file");
			}
			G_strip(label);
			if (strlen(label) == 0)
				continue;
			for (cat=1;cat<=subjcat.num;cat++){
				cat_label = G_get_cat(cat,&subjcat);
				G_strip(cat_label);
				if ((strncmp(label, cat_label, 1024) != 0)){
					continue;
				}else{
					match = 1;
					break;
				}
			}
			if (!match){
				lastcat++;
				G_set_cat ((CELL)lastcat, label, &subjcat);
			}
		}
		/* set back to start for reading of dlg */
		rewind(attfile);
	}

	if (lattopt != NULL){
		if ((lattfile = G_fopen_old("dlg", lattopt, G_mapset())) == NULL)
			G_fatal_error("Can't open line att file");
		if (discard_univ)
			G_getl(buff, sizeof buff, lattfile);
		for (x=0;;x++) {
			match = 0; /* do't expect to find a match in subjcat */
			if (G_getl(buff, sizeof buff, lattfile) == 0)
				break;
			*label = 0;
			if (old_attr){
				if (sscanf (buff, "%ld %[^\n]", &num, label) < 1)
					G_fatal_error("Error reading attribute file");
			}else{
				if (sscanf (buff, "%ld %d %d %[^\n]", &num, &y, &z, label) < 1)
					G_fatal_error("Error reading attribute file");
			}
			G_strip(label);
			if (strlen(label) == 0)
				continue;
			for (cat=1;cat<=subjcat.num;cat++){
				cat_label = G_get_cat(cat,&subjcat);
				G_strip(cat_label);
				if ((strncmp(label, cat_label, 1024) != 0)){
					continue;
                                }else{
                                        match = 1;
                                        break;
                                }
                        }
                        if (!match){
				lastcat++;
				G_set_cat ((CELL)lastcat, label, &subjcat);
			}
		}
		/* set back to start for reading of dlg */
		rewind(lattfile);
	}

	if (subjopt != NULL)
		G__write_cats("SUBJ", subjopt, &subjcat);


	/* Read and write the main body */
	if (dlg_to_dig2 (dlg, &Map, att, add_att_name, off_att, force_lines, attopt, lattopt, &subjcat, old_attr) == -1) {
		printf("Error in translating header\n");
		exit (-1);
	}

	fclose (dlg);
	Vect_close (&Map);
/*	exit(0);*/
}




