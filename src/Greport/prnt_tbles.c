#include "table.h"

print_cats_tables (stats)
    FILE *stats;
{
    long x,n;
    char *G_get_cat();
    char *conv();
    double total;
    double square_meters;

    ncells = 0;
    count0 = 0;
    total0 = 0.0;
    total  = 0.0;

    fseek (stats, 0L, 0);
    while (next_stat (stats, &x, &square_meters, &n))
    {
	ncells += n;
	if (x == 0)
	{
	    count0 = n;
	    total0 = square_meters;
	}
    }


    fseek (stats, 0L, 0);
    switch (cols) 
    {
    case 0: 
	    printf ("| cat#%14.14sName%54.45s|\n",fill,fill);
	    printf ("%s\n",midline);
	    while (next_stat (stats, &x, &square_meters, &n))
	    {
		if(++lines >= threshold)
		{
		    printf ("%s\n",topline);
		    printf ("\n");
		    printf ("%36.36sPage %d\n\n",fill,++page);
		    printf ("%s\n",topline);
		    printf ("| cat#%14.14sName%54.54s|\n", fill,fill);
		    printf ("%s\n",midline);
		    lines = 5;
		}
		printf ("|%4ld | %-70.69s|\n",x,G_get_cat((CELL)x, &cats));
	    }
	    break;
    case 1: 
	    printf ("| cat#%14.14sName%43.43s|%10s|\n", fill,fill,
			    paramname[col[0]]);
	    printf ("%s\n",midline);
	    while (next_stat (stats, &x, &square_meters, &n))
	    {
		if(++lines >= threshold)
		{
		    printf ("%s\n",topline);
		    printf ("\n");
		    printf ("%36.36sPage %d\n\n",fill,++page);
		    printf ("%s\n",topline);
		    printf ("| cat#%14.14sName%43.43s|%10s|\n",
			    fill,fill,paramname[col[0]]);
		    printf ("%s\n",midline);
		    lines = 5;
		}
		printf ("|%4ld | %-59.58s|%11s\n",x,G_get_cat((CELL)x,&cats),
			conv(col[0],n,x,square_meters));
		total += square_meters;
	    }
	    lines += 3;
	    printf ("%s\n",midline);
	    printf ("|       totals%-53.53s|%11s\n",fill,
		    conv(col[0],ncells,x=0,total));
	    break;
    case 2: 
	    printf ("| cat#%14.14sName%32.32s|%10s|%10s|\n", fill,fill,
			    paramname[col[0]], paramname[col[1]]);
	    printf ("%s\n",midline);
	    while (next_stat (stats, &x, &square_meters, &n))
	    {
		if(++lines >= threshold)
		{
		    printf ("%s\n",topline);
		    printf ("\n");
		    printf ("%36.36sPage %d\n\n",fill,++page);
		    printf ("%s\n",topline);
		    printf ( "| cat#%14.14sName%32.32s|%10s|%10s|\n",
			    fill,fill,paramname[col[0]],paramname[col[1]]);
		    printf ("%s\n",midline);
		    lines = 5;
		}
		printf ("|%4ld | %-48.47s|",x,G_get_cat((CELL)x,&cats));
		printf ("%11s", conv(col[0],n,x,square_meters));
		printf ("%11s\n", conv(col[1],n,x,square_meters));
		total += square_meters;
	    }
	    lines += 3;
	    printf ("%s\n",midline);

	    printf ("|       totals%-42.42s|",fill);
	    printf ("%11s", conv(col[0],ncells,x=0,total));
	    printf ("%11s\n", conv(col[1],ncells,x=0,total));
	    break;
    case 3: 
	    printf ("| cat#%14.14sName%21.21s|%10s|%10s|%10s|",
		    fill,fill,
		    paramname[col[0]], paramname[col[1]], paramname[col[2]]);
	    printf ("\n%s\n",midline);
	    while (next_stat (stats, &x, &square_meters, &n))
	    {
		if(++lines >= threshold)
		{
		    printf ("%s\n",topline);
		    printf ("\n");
		    printf ("%36.36sPage %d\n\n",fill,++page);
		    printf ("%s\n",topline);
		    printf (
			    "| cat#%14.14sName%21.21s|%10s|%10s|%10s|\n",
			    fill,fill,paramname[col[0]],paramname[col[1]],
			    paramname[col[2]]);
		    printf ("%s\n",midline);
		    lines = 5;
		}
		printf ("|%4ld | %-37.36s|",x,G_get_cat((CELL)x,&cats));
		printf ("%11s", conv(col[0],n,x,square_meters));
		printf ("%11s", conv(col[1],n,x,square_meters));
		printf ("%11s\n", conv(col[2],n,x,square_meters));
		total += square_meters;
	    }
	    lines += 3;
	    printf ("%s\n",midline);
	    printf ("|       totals%-31.31s|",fill);
	    printf ("%11s", conv(col[0],ncells,x=0,total));
	    printf ("%11s", conv(col[1],ncells,x=0,total));
	    printf ("%11s\n", conv(col[2],ncells,x=0,total));
	    break;
    default:
	    printf ("ERROR CONDITION 1\n");
	    break;
    }
}

next_stat (fd, cat, square_meters, count)
    FILE *fd;
    long *cat;
    double *square_meters;
    long *count;
{
    return (fscanf (fd, "%ld:%lf:%ld", cat, square_meters, count) == 3);
}
