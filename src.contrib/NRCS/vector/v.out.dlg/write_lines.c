/*
**  Written by Dave Gerdes  6/89
**  US Army Construction Engineering Research Lab
*/

#include "digit.h"
#include "export_dlg.h"
#include "gis.h"

#define LINE_FMT "L%5d%6d%6d%6d%6d            %6d%6d%6d\n"

write_dlg_lines (map, fp)
struct Map_info *map;
FILE *fp;
{
	P_LINE *Line;
	P_AREA *Area;
	int line, left, right;
	int n_points, n_atts;
	static struct line_pnts *Gpoints;
	static int first = 1;
	FILE *specfp, *adhocfp;
	char *LOCATION, specfeat[20], adhoc[20],  tmpbuf[256];
	char buf[250],dlgprefix2[2];
	FILE *dlg_attfile;
	int specmajor, specminor;
	int adhocmajor, adhocminor;
	struct Categories cats;

	/*
    **  SEt up so that we can call this repetitively in the future
    */
	if (first)
	{
		first = 0;
		Gpoints = Vect_new_line_struct ();
	}

	/* open special features and adhoc files for later lookup */
	if (dlgtype == 6 || dlgtype == 7 || dlgtype == 8){
		LOCATION = getenv ("LOCATION");
 		sprintf (tmpbuf,"%s/SPECFEAT.code",LOCATION);
		/* look under current location for file first */
		if ((specfp = fopen(tmpbuf,"r")) != NULL)
		{
		fprintf(stderr, "\tUsing local mapset SPECFEAT.code file\n");
        	}else{
		G_strcpy(tmpbuf, G_gisbase());
		G_strcat(tmpbuf, "/etc/SPECFEAT.code");
		specfp = fopen(tmpbuf, "r");
		fprintf(stderr, "\tUsing default SPECFEAT.code file\n");
		}

 		sprintf (tmpbuf,"%s/ADHOC.code",LOCATION);
		/* look under current location for file first */
		if ((adhocfp = fopen(tmpbuf, "r")) != NULL)
		{
		fprintf(stderr, "\tUsing local mapset ADHOC.code file\n");
		}else{
		G_strcpy(tmpbuf, G_gisbase());
		G_strcat(tmpbuf, "/etc/ADHOC.code");
		adhocfp = fopen(tmpbuf, "r");
		fprintf(stderr, "\tUsing default ADHOC.code file\n");
		}
	}
	if (dlgtype){
	sprintf (dlgprefix2,".%d",dlgext);
		G_strcpy(buf, dlgprefix);
 		strcat(buf, dlgprefix2);
		switch (dlgtype){
		case 6: /* special features */
			G_strcat(buf, "sa");
			dlg_attfile = fopen(buf, "w");
			break;
		case 7: /* hydro features */
			G_strcat(buf, "ha");
			dlg_attfile = fopen(buf, "w");
			break;
		case 8: /* culture features */
			G_strcat(buf, "ca");
			dlg_attfile = fopen(buf, "w");
			break;
		case 1:
		        G_strcpy(buf, dlgprefix);
			G_strcat(buf, ".latt");
			dlg_attfile = fopen(buf, "w");
			break;
		}
	}
	if (G_read_vector_cats( map->name, map->mapset, &cats) < 0){
		G_free_cats(&cats);
		G_init_cats((CELL)0, "", &cats);
	}


	for (line = 1 ; line <= map->n_lines ; line++)
	{
		Line = &(map->Line[line]);

		/* if reach the DOTs, then we are done */
		/*if (Line->type == DOT)
	    break;
	*/
		if (Line->left < 0)
		{
			left = abs(map->Isle[abs (Line->left)].area);
		}
		else
			left = Line->left;

		if (Line->right < 0)
		{
			right = abs(map->Isle[abs (Line->right)].area);
		}
		else
			right = Line->right;

		/* Note ++   
	**   This should more correctly be done by creating an Xarray/Yarray
	**   and then calling Vect_copy_xy_to_points ()  As dig_alloc_points
	**   is not currently a documented function.
	*/
		if (line == 1 && dlgtype < 5)
		{
			Area = &(map->Area[1]);
			dig_alloc_points (Gpoints, 5);	/*Note ++  */
			Gpoints->n_points = n_points = 5;
			Gpoints->x[0] = Area->W;
			Gpoints->y[0] = Area->N;
			Gpoints->x[1] = Area->E;
			Gpoints->y[1] = Area->N;
			Gpoints->x[2] = Area->E;
			Gpoints->y[2] = Area->S;
			Gpoints->x[3] = Area->W;
			Gpoints->y[3] = Area->S;
			Gpoints->x[4] = Area->W;
			Gpoints->y[4] = Area->N;
		}
		else
		{
			if (0 > Vect__Read_line (map, Gpoints, Line->offset))
			{
				fprintf (stderr, "ERROR reading line %d from file\n", line);
				exit (-1);
			}
		}

		if (Line->att)
			n_atts = 1;
		else 
			n_atts = 0;


		fprintf (fp, LINE_FMT, 
		    line,				/* index of element */
		Line->N1,			/* start node */
		Line->N2,			/* end node */
		left,				/* left area */
		right,				/* right area */
		Gpoints->n_points,		/* # of coords */
		n_atts,				/* # of atts */
		0);				/* unused */
		start_coords ();
		write_coords (fp, Gpoints->n_points, Gpoints->x, Gpoints->y);
		end_coords (fp);

		if (n_atts) {
			start_att ();
			if (dlgtype == 6 || dlgtype == 7 || dlgtype == 8){
				rewind(specfp);
				while (fgets(tmpbuf, 100, specfp) != NULL){
					if (*tmpbuf == '#' || *tmpbuf == ' ')
						continue;
					if(sscanf(tmpbuf, "%s%d%d%*s", specfeat, &specmajor, &specminor) < 3)
						continue;
/*DEBUG*/
					/*printf("%s %d %d\n", specfeat, specmajor, specminor);*/
/*DEBUG*/
					if (strcmp(G_get_cat(map->Att[Line->att].cat,
					    &cats), specfeat) == 0){
						write_dlg_att (fp, specmajor, specminor);
						break;
					}
				}
				rewind(adhocfp);
				while (fgets(tmpbuf, 100, adhocfp) != NULL){
					if (*tmpbuf == '#' || *tmpbuf == ' ')
						continue;
					if(sscanf(tmpbuf, "%s%d%d%*s", adhoc, &adhocmajor, &adhocminor) < 3)
						continue;
					/*printf("%s %d %d\n", adhoc, adhocmajor, adhocminor);*/
					if (strcmp(G_get_cat(map->Att[Line->att].cat,
					    &cats), adhoc) == 0){
						write_dlg_att (fp, adhocmajor, adhocminor);
						G_strcpy(specfeat,  adhoc);
						specminor = adhocminor;
						specmajor = adhocmajor;
						break;
					}
				}
			} else
				write_dlg_att (fp, DEF_MAJOR, map->Att[Line->att].cat);
			end_att (fp);

			/* write out ascii attribute file for dlg .sa or .latt */
			switch (dlgtype){
			case 6: /* special features */
				/*DEBUG*/
				/*printf( "%d\t%d\t%d\t%s\n",
				    line, specmajor, specminor, specfeat);*/
				/*DEBUG*/
				fprintf( dlg_attfile, "%d\t%d\t%d\t%s\n",
				    line, specmajor, specminor, specfeat);
				break;
			case 7: /* hydro features */
				fprintf( dlg_attfile, "%d\t%d\t%d\t%s\n",
				    line, specmajor, specminor, specfeat);
				break;
			case 8: /* culture features */
				fprintf( dlg_attfile, "%d\t%d\t%d\t%s\n",
				    line, specmajor, specminor, specfeat);
				break;
			case 1:
				fprintf( dlg_attfile, "%d\t%d\t%d\t%s\n",
				    line, 999, map->Att[Line->att].cat,
				    G_get_cat(map->Att[Line->att].cat, &cats));
				break;
			default:
				break;
			}

		} else{
			switch (dlgtype){
			case 1:
			case 6:
				fprintf( dlg_attfile, "%d\t\t\t\n", line);
				break;
			case 7:
				fprintf( dlg_attfile, "%d\t\t\t\n", line);
				break;
			case 8:
				fprintf( dlg_attfile, "%d\t\t\t\n", line);
				break;
			default:
				break;
			}
		}
	}
}
