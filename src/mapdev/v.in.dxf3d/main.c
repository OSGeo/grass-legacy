/* ---------------------------------------------v.in.dxf3--------------------
	Programa para importar las coordenadas Z de los ficheros DXFs a los
ficheros de soporte "dig_att" de los mapas vectoriales de GRASS, generados por
v.in.dxf. Este programa es una reconversion a C de un programa creado en
1993 en GWBASIC para MS-DOS.
 ------------------------------------------------------ E. QUIROGA, 1995 -- */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "gis.h" 
# define MAXFILE 256
# define MAXLINE 80

#define NUMLINES 256


int main(int argc, char *argv[])
{
	char nfout[MAXFILE]; 
	FILE *dxf_fp;
	FILE *fout[NUMLINES];
	char basename[100];
	char linein[MAXLINE];
	char layer[MAXLINE];
	char linelayer[MAXLINE];
	double xcoord;
	double ycoord;
	int zcoord;
	int nvert;
	char *p;
	int i;
	
/* Interfaz de commandos de GRASS  */
		
	struct Option *dxf_opt, *line_opt;
	G_gisinit(argv[0]);
	
	dxf_opt = G_define_option();
    	dxf_opt->key                        = "dxf";
        dxf_opt->type                       = TYPE_STRING;
        dxf_opt->required           = YES;
        dxf_opt->multiple           = NO;
        dxf_opt->description                = "DXF input file";
                            
        line_opt = G_define_option();
        line_opt->key                       = "lines";
        line_opt->type                      = TYPE_STRING;
        line_opt->required          = NO;
        line_opt->multiple          = YES;
        line_opt->description       = "DXF layers with line data";
                                                    

	if (G_parser (argc, argv))
     	{
            /* 	extra_help (); */
                exit (-1);
        }
                         
/* Abrir el fichero DXF especificado */

	if ( dxf_opt->answer == NULL ) {
		fprintf (stderr, "%s: Command line error.\n\n", argv[0]);
	        G_usage();
	        exit (-1);
        }
	                                
	if ((dxf_fp = fopen (dxf_opt->answer, "r")) == NULL) {
	        fprintf (stderr, "\ncannot open [%s] for dxf file\n", dxf_opt->answer);
	        exit (-2);
	}
	      
	      	      
	                                                                    
/* Coger como prefijo el nombre del fichero DXF */

	p = G_rindex (dxf_opt->answer,'/');
	if (p == NULL) p = dxf_opt->answer;
	else p++;
	strcpy (basename, p);
	if (NULL != (p = G_rindex (basename, '.')))
		if (p != basename) *p = '\0';
	


    /*	printf("%s\n",basename); */
	

/* Abrir los ficheros de salida de las distintas Layers */

	if (line_opt->answers != NULL) {
		i = -1;
		while (line_opt->answers[++i]) {
			sprintf(nfout,"%s.%s", basename,line_opt->answers[i]);

			fout[i] = G_fopen_new("dig_att",nfout);
		/*	fout[i] = fopen(nfout,"w"); */
		}
	}	
	
	


/* Parte principal del programa */

	while (fgets(linein, MAXLINE, dxf_fp)) {
		if (strcmp(linein,"ENTITIES\n")==0) {   		/* Buscando la seccion ENTITIES */
			while (fgets(linein,MAXLINE,dxf_fp)) {
				if (strcmp(linein,"POLYLINE\n")==0) {     	/* Buscando la seccion POLYLINE */
					while (strcmp(linein,"VERTEX\n")!=0) {
						fgets(linein,MAXLINE,dxf_fp);
						if (strcmp(linein,"  8\n")==0) {
							fgets(linein,MAXLINE,dxf_fp);
							strcpy(layer,linein);
						}
					}
					nvert = 1;
					while (strcmp(linein,"SEQEND\n")!=0) {
						fgets(linein,MAXLINE,dxf_fp);
						if (strcmp(linein,"VERTEX\n")==0) nvert = nvert + 1;
						if (nvert == 2) {			/* Utilizando el segundo VERTICE para tomar las coordenadas */
							fgets(linein,MAXLINE,dxf_fp);
							while (strcmp(linein,"  0\n")!=0) {          /* Captura de las coordenadas X, Y y Z */
								if (strcmp(linein," 10\n")==0) {
									fgets(linein,MAXLINE,dxf_fp);
									xcoord = atof(linein);
								}
								if (strcmp(linein," 20\n")==0) {
									fgets(linein,MAXLINE,dxf_fp);
									ycoord = atof(linein);
								}
								if (strcmp(linein," 30\n")==0) {
									fgets(linein,MAXLINE,dxf_fp);
									zcoord = atoi(linein);
								}
								fgets(linein,MAXLINE,dxf_fp);
							}		
							
						/*  Impresion de las coordenadas la fichero dig_att */
							if (line_opt->answers != NULL) {
								i = -1 ;
								while (line_opt->answers[++i]) {
									sprintf(linelayer,"%s\n",line_opt->answers[i]);						
									if (strcmp(layer,linelayer)==0) fprintf(fout[i],"L %f %f %d \n", xcoord, ycoord, zcoord);
							  	}
							}
							
							nvert = 3;
						}
						else 	continue;
					}
				}
				else  continue;						
			}
		}
		else	continue;
	}
	fclose(dxf_fp);
	if (line_opt->answers != NULL) {
		i = -1;
		while (line_opt->answers[++i]) {
			fclose(fout[i]);
		}
	}
	return 0;	
}
