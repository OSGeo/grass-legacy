/* ---------------------------------------------v.in.dxf3--------------------
	Programa para importar las coordenadas Z de los ficheros DXFs a los
ficheros de soporte "dig_att" de los mapas vectoriales de GRASS, generados por
v.in.dxf. Este programa es una reconversion a C de un programa creado en
1993 en GWBASIC para MS-DOS.
 ------------------------------------------------------ E. QUIROGA, 1995 -- */

#include <stdio.h>
#include "gis.h" 
# define MAXFILE 256
# define MAXLINE 80


main(int argc, char *argv[])
{

	FILE *dxf_file; 
	char nfout[MAXFILE]; 
	FILE *dxf_fp;
	FILE *fout[3];
	char basename[100];
	char linein[MAXLINE];
	char layer[MAXLINE];
	char linelayer[MAXLINE];
	char xcoord[MAXLINE];
	char ycoord[MAXLINE];
	char zcoord[MAXLINE];
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

	dxf_file = dxf_opt->answer;
 		
	if ( dxf_file == NULL ) {
		fprintf (stderr, "%s: Command line error.\n\n", argv[0]);
	        G_usage();
	        exit (-1);
        }
	                                
	if ((dxf_fp = fopen (dxf_file, "r")) == NULL) {
	        fprintf (stderr, "\ncannot open [%s] for dxf file\n", dxf_file);
	        exit (-2);
	}
	      
	      	      
	                                                                    
/* Coger como prefijo el nombre del fichero DXF */

	p = G_rindex (dxf_file,'/');
	if (p == NULL) p = dxf_file;
	else p++;
	strcpy (basename, p);
	if (NULL != (p = G_rindex (basename, '.')))
		if (p != basename) *p = '\0';
	


    /*	printf("%s\n",basename); */
	

/* Abrir los ficheros de salida de las distintas Layers */

	if (line_opt->answers != NULL) {
		i = 0;
		while (line_opt->answers[i]) {
			sprintf(nfout,"%s.%s", basename,line_opt->answers[i++]);

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
							while (strcmp(linein,"VERTEX\n")!=0) {          /* Captura de las coordenadas X, Y y Z */
								if (strcmp(linein," 10\n")==0) {
									fgets(linein,MAXLINE,dxf_fp);
									strncpy(xcoord,linein,(strlen(linein) - 1));
								}
								if (strcmp(linein," 20\n")==0) {
									fgets(linein,MAXLINE,dxf_fp);
									strncpy(ycoord,linein,(strlen(linein) - 1));
								}
								if (strcmp(linein," 30\n")==0) {
									fgets(linein,MAXLINE,dxf_fp);
									strncpy(zcoord,linein,(strlen(linein) - 1));
								}
								fgets(linein,MAXLINE,dxf_fp);
							}		
							
						/*  Impresion de las coordenadas la fichero dig_att */
							if (line_opt->answers != NULL) {
								i = 0 ;
								while (line_opt->answers[i]) {
									sprintf(linelayer,"%s\n",line_opt->answers[i++]);						
									if (strcmp(layer,linelayer)==0) fprintf(fout[i],"L %s %s %s \n", xcoord, ycoord, zcoord);
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
	fclose(fout);
}
