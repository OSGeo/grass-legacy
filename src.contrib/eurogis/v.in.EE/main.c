/********************************************************************************
*   
*   v.in.EE input=name_file output=name_file
*  
*   Este comando genera un fichero vectorial ascii formato GRASS a partir de un 
*   fichero vectorial GENAMAP (ZF25).   
*  
*   INPUT:   name_file. Fichero ascii formato genamap, este nombre deberia 
*   tener la extension .EE, por ejemplo ~deltam/itge/ZF25/name_file.EE 
*
*   OUTPUT:  name_file. Mapa vectorial ascii grass, genera los dos ficheros dig_ascii
*   y dig_att.
*
*   Anabel Donadios Algarra. Junio 1992
*
******************************************************************************/ 



#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include <string.h>
#include "Vect.h"
#include "gis.h"
#include "vtoc.h"




struct head_record {   /*  El registro de cabecera de un fichero EE */
	char  	id_record[6];  /*  Formato de 5 caracteres  */
        char 	type_feature[11]; /* if LINE type_feature=1, if POINT type_feature=2
                               */
	char 	tag_feature[33];
	int	count;      /*  Formato de 5 caracteres  */
};


double maxx,minx,maxy,miny;
int zona,scale;
int FIRSTIME=1;

main(argc, argv)
	int argc;
	char *argv[];
{
        char *string;

        FILE *fd;
        FILE *fd_asc, *fd_att;
        FILE *fd_cab;
        char *gena_name;
	struct Option *old, *new;
	char command[200];

	struct Map_info Map;
        char *dig_name;
	struct Cell_head header;
	/* nombre completo o path absoluto */
        char location[1024],fasc[1024],fcab[1024],fatt[1024],fascii[1024];
	long timer;
        int option;

        /* needed to turn off buffering to make clock stuff work right */
        setbuf (stdout, NULL);


/* Initialize gis library */
	G_gisinit(argv[0]) ;

	old = G_define_option();
	old->key		= "input";
	old->type		= TYPE_STRING;
	old->required		= YES;
	old->multiple		= NO;
/* 	old->gisprompt		= "old,dig,vector"; */
	old->description	= "EE input file. i.e.:/home/deltam/file.EE";
/*	old->description	= "EE input file. P.e.:/home/deltam/file.EE";*/
	
	new = G_define_option();
	new->key		= "output";
	new->type		= TYPE_STRING;
	new->required		= YES;
	new->multiple		= NO;
	new->description	= "GRASS4.0 format vector file. i.e:name_file";
/*	new->description	= "Fichero vectorial formato GRASS4.0. P.e:name_file";*/


	if (G_parser (argc, argv))
	    exit (-1);

	dig_name = new->answer;
	gena_name = old->answer;

                  
        sprintf (location,"%s/%s",G_location_path(),G_mapset());  
        fprintf (stdout," LOCATION es  %s \n",location);

        sprintf (fasc,"%s/dig_ascii/fasc",location);  
        sprintf (fcab,"%s/dig_ascii/fcab",location);  
        sprintf (fatt,"%s/dig_att/%s",location,dig_name);  
        sprintf (fascii,"%s/dig_ascii/%s",location,dig_name);  

        if ((fd = fopen(gena_name,"r"))==NULL){
               fprintf (stdout,"ERROR: El fichero [%s] no se puede abrir\n",gena_name);
               exit(1);
        }

        if ((fd_asc = fopen(fasc,"w"))==NULL){
              fprintf (stdout,"ERROR: El fichero [%s] no se puede abrir\n",fascii);
               exit(1);
        }
        if ((fd_att = fopen(fatt,"w"))==NULL){
               fprintf (stdout,"ERROR: El fichero [%s] no se puede abrir\n",fatt); 
               exit(1);
        }

        if ((fd_cab = fopen(fcab,"w")) == NULL){
               fprintf (stdout,"ERROR: El fichero [%s] no se puede abrir\n",fcab); 
               exit(1);
        }
	/* this will be over all time */ 
        start_clock (&timer);

    

/* Do initial read of DIGIT file */
/* 	fprintf (stdout,"\n   Leyendo informacion vectorial %s \n",dig_name);  */
        start_clock(NULL);
        
	/*  Proceso de eleccion de funciones  */
        fprintf (stdout,"\nSeleccione que opcion describe el tipo de entidades que se van a procesar: \n");
	fprintf (stdout,"\n\t 1) Lineas  \n");
	fprintf (stdout,"\t 2) Areas \n");
	fprintf (stdout,"\t 3) Lineas en general. Las entidades GRASS seran tipo A y no L \n");
        fprintf (stdout,"\nTeclee el numero correspondiente a la opcion deseada\n");
        scanf("%d",&option);
        while (option!=1 && option!=2 && option!=3) {
               fprintf (stdout,"Opcion incorrecta \n");
               fprintf (stdout,"\n\t 1) Lineas  \n");
               fprintf (stdout,"\t 2) Areas \n");
	       fprintf (stdout,"\t 3) Lineas en general. Las entidades GRASS seran tipo A y no L \n");
		fprintf (stdout,"\nTeclee el numero correspondiente a la opcion deseada\n");
               scanf("%d",&option);
        }
	/*
		OTROS PARAMETROS
	*/
	fprintf (stdout,"?A que zona UTM pertenece el mapa?\n");
	fprintf (stdout,"(Introduzca un 0 si el mapa no esta en UTM\n");
	while (scanf("%d",&zona)!=1 || zona<0) {
		fprintf (stdout,"\nZona UTM incorrecta\n Intentelo de nuevo\n");
	}
	fprintf (stdout,"?Cual fue la resolucion a la que fue digitalizado?\n");

	while (scanf("%d",&scale)!=1 || scale<=0) {
		fprintf (stdout,"\nResolucion incorrecta\n Intentelo de nuevo\n");
	}

        if (option==1) lineas_puntos(fd,fd_att,fd_asc);
	else if (option==2 || option==3) areas_puntos(fd,fd_att,fd_asc);
        
        cabecera(dig_name,fd_cab);

        fclose (fd);
        fclose (fd_asc);
        fclose (fd_att);
        fclose (fd_cab);

        sprintf(command,"cat '%s' '%s' > '%s'",fcab,fasc,fascii);
        system(command);
        sprintf(command,"rm '%s' '%s'",fcab,fasc);
	system(command); 
	sprintf(command,"v.in.ascii i='%s' o='%s'",dig_name,dig_name);
        system(command);
       

        stop_clock(NULL);
        fprintf (stdout,"\n");

	fprintf (stdout, "                  Total time:          ");
	stop_clock (&timer);
	fprintf (stdout, "\n");
	exit(0);
}

lineas_puntos(fd,fd_att,fd_asc)
FILE *fd,*fd_att,*fd_asc;
{
        
        struct head_record record;
	int categ,cont,num1,err;
	double coordx,coordy,nousado,firstx,firsty;
        char *letraE;

	minx=miny=maxx=maxy=0;

        letraE = (char *)G_malloc(sizeof(char));
        letraE = "E";
        fprintf (stdout," Antes de while \n");

	while (fscanf(fd,"%5c%10c%32c%d",record.id_record,record.type_feature,record.tag_feature,&record.count) == 4){

/*   Considero que si la etiqueta es numerica debe ser distinta de cero. Si la
    etiqueta es no numerica se perdera */

                getc(fd);
                if ((err=sscanf(record.tag_feature,"%d",&categ)) != 1){
                        fprintf (stdout," record.tag_feature es %d, sscanf es %d\n",num1,err);
                        categ=0;
                };
                if (*letraE == record.type_feature[3]) {
                        fprintf (stdout,"LINE o EDGE\n");
                        cont=1;
                        fprintf (fd_asc,"L %d\n",record.count);
                }
		else  {
                    fprintf (stdout,"POINT \n");
                    fprintf (fd_asc,"P %d\n",record.count);
		    fscanf(fd,"%lf%lf%lf",&coordx,&coordy,&nousado);
		    limites_window(coordx,coordy);
		    fprintf(fd_asc,"%13.2f%13.2f\n",coordy,coordx);
		    fprintf(fd_asc,"%13.2f%13.2f\n",coordy,coordx);
                    if (categ)
                        fprintf(fd_att,"P%15.2f%15.2f  %d \n",coordx,coordy,categ);
		    cont=2;
		}
                while (cont<=record.count) {
                     fscanf(fd,"%lf%lf%lf",&coordx,&coordy,&nousado);
		     limites_window(coordx,coordy);
                     fprintf(fd_asc,"%13.2f%13.2f\n",coordy,coordx);
                     if (cont==2 && record.count==2 && categ)
                          fprintf(fd_att,"L%15.2f%15.2f   %d\n",(coordx+firstx)/2,(coordy+firsty)/2,categ);
                     else if (cont==2 && categ)
                          fprintf(fd_att,"L%15.2f%15.2f   %d\n",coordx,coordy,categ);
                     if (record.count==2){ 
        		firstx=coordx;
			firsty=coordy;
		     }
		     cont++;
                     
		}
                while (getc(fd) !='\n'); 
	} 
}

areas_puntos (fd,fd_att,fd_asc)
FILE *fd,*fd_att,*fd_asc;
{           

	struct head_record record;
        int categ,cont,err;
    	double coordx,coordy,nousado,firstx,firsty;
        char *letraE;


        minx=miny=maxx=maxy=0;
        letraE = (char *)G_malloc(sizeof(char));
        letraE = "E";

	while (fscanf(fd,"%5c%10c%32c%5d\n",record.id_record,record.type_feature,record.tag_feature,&record.count)==4)  {

                getc(fd);
		if ((err=sscanf(record.tag_feature,"%d",&categ)) != 1){
                        fprintf (stdout," record.tag_feature es %d, sscanf es %d\n",categ,err);
                        categ=0;
                }
		if (record.type_feature[3] == *letraE ) {
			cont=1;
			fprintf (fd_asc,"A %d\n",record.count);
		}
                else if (record.type_feature[2]== *letraE ) {
                     /*  AREATAG */
                        fscanf(fd,"%lf%lf%lf",&coordx,&coordy,&nousado);
                        limites_window(coordx,coordy);
                        if (categ)
                        	fprintf (fd_att,"A%15.2f%15.2f%7d\n",coordx,coordy,categ);
			else fprintf (stdout,"Error en areatag, no deberias ver este mensaje \n");
                        cont=2;
                }
		else  {
                        fprintf (stdout," PUNTO \n");
			fprintf (fd_asc,"P %d\n",record.count);
			fscanf(fd,"%lf%lf%lf",&coordx,&coordy,&nousado);
		        limites_window(coordx,coordy);
			fprintf(fd_asc,"%13.2f%13.2f\n",coordy,coordx);
			fprintf(fd_asc,"%13.2f%13.2f\n",coordy,coordx);
			if (categ) fprintf(fd_att,"P%15.2f%15.2f%7d\n",coordx,coordy,categ);
			cont=2; 
                }
                while (cont<=record.count) {
                     fscanf(fd,"%lf%lf%lf",&coordx,&coordy,&nousado);
                     limites_window(coordx,coordy);
                     fprintf(fd_asc,"%13.2f%13.2f\n",coordy,coordx);
                     if (cont==2 && record.count==2 && categ)
			fprintf(fd_att,"A%15.2f%15.2f  %d\n",(coordx+firstx)/2,(coordy+firsty)/2,categ);
		     else if (cont==2 && categ)
			fprintf(fd_att,"A%15.2f%15.2f   %d\n",coordx,coordy,categ);
                     if (record.count==2){
			firstx=coordx;
			firsty=coordy;
		     }
                     cont++;
		}
                while (getc(fd) != '\n');
	} 
}
	 
limites_window(coordx,coordy)
double coordx,coordy;


{

        if (FIRSTIME) {
           minx=maxx=coordx;
           miny=maxy=coordy;
           FIRSTIME=0;
        }
	else {
           if (coordx > maxx)
		maxx = coordx;
       	   else if (coordx < minx)
		minx = coordx;
	   else if (coordy > maxy)
		maxy = coordy;
	   else if (coordy < miny)
		miny = coordy;
        }
}

	
cabecera(name,fd_cab)
char *name;
FILE *fd_cab;

{
       double thresh;

        thresh=20.0;
	fprintf(fd_cab,"ORGANIZATION: EUROGIS\nDIGIT DATE:   nov 1992\nDIGIT NAME:   grass4\nMAP NAME:     %s\nMAP DATE:     nov 1992\nOTHER INFO:     \nMAP SCALE:    %d\nZONE:         %d\nWEST EDGE:    %f\nEAST EDGE:    %f\nSOUTH EDGE:   %f\nNORTH EDGE:   %f\nMAP THRESH:   %f\nVERTI:                      \n",name,scale,zona,minx,maxx,miny,maxy,thresh);
}
