/* Begin program permut.c */
/* usage: permut file.ply tempfile */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define         MAXDOUBLES        5000
#define         MAXPAIRS         20000
#define         MAXLINES         40000
#define         BUFLENGTH          256

typedef struct    {
                    int   line;
                    int   closed;
                    float x,y;
                  } kenn;



int main (argc,argv) char *argv[];
{

FILE*   infile;
FILE*   outfile;
char    out_ply [250];
char    in_ply [250];
char    comstring [250];
char    buffer[BUFLENGTH];
int     i, j, cols, k, iteration, l, step;
int     ID, ident_nodes;
float   node_x, node_y, minimum;
float   x,y;
float   valx [MAXPAIRS], valy [MAXPAIRS];
int     doubles [MAXDOUBLES];
kenn    matrix [MAXLINES];
kenn    temp;


        strcpy (in_ply, argv [1]);
        strcpy (out_ply, argv [2]);


/*      read input-file                                                      */

        if ((infile = fopen (argv [1], "r")) == NULL)
        {
          printf ("can't find inputfile %s\n", argv [1]);
          exit (1);
        }

        while (!feof (infile))
        {
          if (strstr (fgets (buffer, BUFLENGTH, infile), "END") != NULL) break;
          cols = sscanf (buffer, "%i", &ID);
          cols = fscanf (infile, "%g %g", &node_x, &node_y);
      
          (matrix [ID]).line      = ID;
          (matrix [ID]).x         = node_x;
          (matrix [ID]).y         = node_y;
          (matrix [ID]).closed    = 0;

          i = 1;
          while (strstr (fgets (buffer, BUFLENGTH, infile), "END") == NULL)
          {
            cols = sscanf (buffer, "%g %g", &x, &y);
          }

          if ((x == (matrix [ID]).x) && (y ==  (matrix [ID]).y)) 
	    (matrix [ID]).closed = 1;
          else
          {
            printf ("polygon %i not closed\n", ID);
          }

        } /* while !feof */

        fclose (infile);
        printf ("read file with %i polygons\n", ID);


/*      Iteration loop to find and eliminate double nodes                    */

        ident_nodes = 1;
        iteration = 0;
        while (ident_nodes)
        {


/*      sort matrix to increasing x-values                                      */

        j = 0;
        iteration++;
        while (++j < ID)
        {
          minimum = (matrix [j]).x;
          i = j;

          while (++i <= ID)
          {
            if ((matrix [i]).x < minimum)
            {
               minimum     = (matrix [i]).x;
               temp        = matrix [i];
               matrix [i]  = matrix [j];
               matrix [j]  = temp;
            }
          }
        }


/*      find identical nodes                                                        */

        ident_nodes = 0;
        i = 1;
        k = 0;
	doubles [0] = 0;

        while (i < ID)
        {
          while ((matrix [i]).x != (matrix [i+1]).x) i++;
          j = 1;

          while (((matrix [i]).x == (matrix [i+j]).x) && (i+j <= ID))
          {
            if ((matrix [i]).y == (matrix [i+j]).y)
            {
              if ((matrix [i]).closed)
              {
                k++;
                doubles [k] = (matrix [i]).line;
                doubles [0]++;
              }
              else if ((matrix [i+j]).closed)
              {
                k++;
                doubles [k] = (matrix [i+j]).line;
                doubles [0]++;
              }
              else
              {
                printf ("polygons %i and %i with identical nodes not closed\n",
                         (matrix [i].line), (matrix [i+j]).line);
              }

              ident_nodes++;
            } /* if matrix [i] */

            j++;

          } /* while matrix [i] */

          i++;
        
        } /* while i<ID */

        if (!ident_nodes) 
	{
	  printf ("all double nodes removed. Program finished\n");
	  exit (0);
	}

       
        printf ("found %i identical nodes in iteration %i\n",
                  ident_nodes, iteration);


/*      sort double nodes to increasing ID's                                 */

        j = 0;
        while (++j < doubles [0])
        {
          minimum = (float) doubles [j];
          i = j;

          while (++i <= doubles [0])
          {
            if (doubles [i] < minimum)
            {
              minimum    = (float) doubles [i];
              doubles [i] = doubles [j];
              doubles [j] = (int) minimum;
            }
          }
        }


/*      copy input file until polygon ID                                          */



/*        open output file                                                           */

        if ((outfile = fopen (out_ply, "w")) == NULL)
        {
          printf ("can't open tempfile %s\n", out_ply);
          exit (1);
        }


/*  read input file                                                                    */

        if ((infile = fopen (argv [1], "r")) == NULL)
        {
          printf ("can't find input file %s\n", argv [1]);
          exit (1);
        }

        k = 1;
        while (!feof (infile))
        {


/*      read whole polygon                                                        */

          if (strstr (fgets (buffer, BUFLENGTH, infile), "END") != NULL) break;

          cols = sscanf (buffer, "%i", &ID);
          (matrix [ID]).line      = ID;

          j = 1;
          while (strstr (fgets (buffer, BUFLENGTH, infile), "END") == NULL)
          {
            cols = sscanf (buffer, "%g %g", &x, &y);
            valx [j] = x;
            valy [j] = y;
            j++;
          }

          valx [0] = j - 1;
     

/*      perform permutation if necessary                                        */

          if (ID == doubles [k])
          {
            step = (int) (valx [0] / 10 + 1);

            for (l = 2; l <= step; l++)
            {
              valx [(int) valx [0] + l - 1] = valx [l];
              valy [(int) valx [0] + l - 1] = valy [l];
            }

            for (l = 1; l <= valx [0]; l++)
            {
              valx [l] = valx [l + step];
              valy [l] = valy [l + step];
            } /* for */

            valx [(int) valx [0]] = valx [1];
            valy [(int) valx [0]] = valy [1];
            k++;
          }


/*      generate new matrix                                                    */

          (matrix [ID]).x       = valx [1];
          (matrix [ID]).y       = valy [1];


/*      write polygon                                                          */

          fprintf (outfile, "%5i\n", ID);

          for (l = 1; l <= valx [0]; l++)
          {
            fprintf (outfile, "%15.6f %15.6f\n", valx [l], valy [l]);
          }

          fprintf (outfile, "END\n");

        } /* while !feof */

        fprintf (outfile, "END");
        fclose (infile);
        fclose (outfile);

        sprintf (comstring, "cp %s %s", out_ply, in_ply);
        fprintf (stderr, "iteration %i finished ...", iteration);
        system (comstring);
        fprintf (stderr, "   ... tempfile copied\n");

     } /* while ident_nodes */

     exit (0);

} /* main */
