#include "table.h"

print_cats_report(stats,mapname,mapset)
    FILE *stats;
    char *mapname;
    char *mapset;
{
    int		p,i,j;
    char buf1[50], buf2[50];


    printf ("\n\n\n");
    printf ("%s\n",topline);
    printf ("|%26.26sMAP LAYER CATEGORY REPORT%26.26s|\n", "","");
    printf ("%s\n",midline);
    sprintf (buf1,"[%s] in mapset [%s]", mapname, mapset);
    printf ("| Layer:    %-40.38sDate: %-20.20s|\n",buf1,G_date());
    printf ("| Location: %-66.64s|\n", G_location());
    printf ("| Title:    %-66.64s|\n",cats.title);
    printf ("| Mask:     %-66.64s|\n",G_mask_info());
    printf ("%s\n",midline);

    G_format_northing (window.north, buf1, window.proj);
    G_format_easting  (window.east,  buf2, window.proj);
    printf ("| %-10s north: %11s       east: %11s %22s|\n", "", buf1, buf2, "");

    G_format_northing (window.south, buf1, window.proj);
    G_format_easting  (window.west,  buf2, window.proj);
    printf ("| %-10s south: %11s       west: %11s %22s|\n", "Window:",buf1,buf2,"");

    G_format_resolution (window.ns_res, buf1, window.proj);
    G_format_resolution (window.ew_res, buf2, window.proj);
    printf ("| %-10s res:   %11s       res:  %11s %22s|\n", "", buf1, buf2, "");

    lines = 15;
    printf ("%s\n\n",topline);

    p = 0;
    for(i = 0; i < tables; i++){
	lines += 3;
	if((lines+3) >= threshold){
	    printf ("\n");
	    printf ("%37.37sPage %d\n\n",fill,++page);
	    lines = 3;
	}
	printf ("%s\n",topline);

	if((cols = (pnum-i*3)) > 3) cols = 3;

	for(j = 0; j < cols; j++){
	    while(!param[p][0]) p++;
	    col[j] = p;
	    p++;
	}

	print_cats_tables(stats);

	printf ("%s\n\n",topline);
    }
}
