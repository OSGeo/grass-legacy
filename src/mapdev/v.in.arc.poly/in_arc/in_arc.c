/* Begin program in_arc.c */
/* usage: in_arc file.ply outfile */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main (argc,argv) char *argv[];
{

FILE*   infile;
FILE*   plyfile;
char    out_ply [50];
FILE*   pntfile;
char    out_pnt [50];
FILE*   txtfile;
char    out_txt [50];
int     cols;
char    buffer[256];
char    as [20], bs [20], cs [20];
char    value [256];
int     i = 1;
int     k = 1;
int     ID;


    if ((infile = fopen (argv [1], "r")) == NULL)
    {
      printf ("can't find input file %s\n", argv [1]);
      exit (1);
    }


    strcpy (out_ply, argv [2]);
    strcat (out_ply, ".ply");

    if ((plyfile = fopen (out_ply, "w")) == NULL)
    {
      printf ("can't open polygon file %s\n", out_ply);
      exit (1);
    }


    strcpy (out_pnt, argv [2]);
    strcat (out_pnt, ".pnt");

    if ((pntfile = fopen (out_pnt, "w")) == NULL)
    {
      printf ("can't open label point file %s\n", out_pnt);
      exit (1);
    }


    strcpy (out_txt, argv [2]);
    strcat (out_txt, ".txt");

    if ((txtfile = fopen (out_txt, "w")) == NULL)
    {
      printf ("can't open label text file %s\n", out_txt);
      exit (1);
    }


    while (!feof (infile))
    {
      fgets(buffer, 256, infile);
      if (strstr (buffer, "END") != NULL)
      {
	fprintf (plyfile, "END\n");
	i++;
      }
      else
      {
	cols = sscanf (buffer, "%s %s %s", &as, &bs, &cs);
	sscanf (as, "%i", &ID);

	if (ID == -99999)
	{
	  while (strstr (buffer, "END") == NULL) fgets(buffer, 256, infile);
	  k++;
	}
	else if ((cols == 1) && (ID >= 0))
	{
	  fprintf (plyfile, "    %i\n", i);
	  fprintf (txtfile, "    %i  %s  %s\n", i, as, as);
	}

        else if (cols == 3) 
	{
	  fprintf (plyfile, "    %i\n", i);
	  fprintf (pntfile, "    %i  %s  %s\n", i, bs, cs);
	  fprintf (txtfile, "    %i  %s  %s\n", i, as, as);
	}
	else if (cols == 2)
	{
	  fprintf (plyfile, "    %s  %s\n", as, bs);
	}
      }
    }

    fprintf (txtfile, "END");
    fprintf (pntfile, "END");

    fclose (infile);
    fclose (plyfile);
    fclose (txtfile);
    fclose (pntfile);
}

/* end in_arc.c */
