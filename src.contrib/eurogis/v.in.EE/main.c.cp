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
#include "Vect.h"
#include "gis.h"
#include <string.h>
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
        char *gena_name,option;
	struct Option *old, *new;
	char errmsg[200];
	char command[100];

	struct Map_info Map;
        char *dig_name;
	struct Cell_head header;
	int stat;
	char *mapset;
	int area;
	long timer;

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
	old->description	= "EE input file. P.e.:/home/deltam/work_area/ZF25/file.EE";
	
	new = G_define_option();
	new->key		= "output";
	new->type		= TYPE_STRING;
	new->required		= YES;
	new->multiple		= NO;
	new->description	= "Fichero vectorial formato GRASS4.0. P.e:name_file";


	if (G_parser (argc, argv))
	    exit (-1);

	dig_name = new->answer;
	gena_name = old->answer;

	if (!*dig_name  || !*gena_name )
	{
	    fprintf (stderr, "%s: Error en la linea de comandos: v.in.EE input=name output=name\n\n", argv[0]);
	    G_usage();
	    exit (-1);
	}

        
                  

        if ((fd = fopen(gena_name,"r"))==NULL){
               printf("ERROR: El fichero [%s] no se puede abrir\n",gena_name);
               exit(1);
        }

        if ((fd_asc = fopen("file.ascii","w"))==NULL){
            /*   printf("ERROR: El fichero [%s] no se puede abrir\n",header_name); */
               printf("ERROR: El fichero dig_ascii  no se puede abrir\n"); 
               exit(1);
        }
        if ((fd_att = fopen("file.att","w"))==NULL){
             /*  printf("ERROR: El fichero [%s] no se puede abrir\n",header_name); */
               printf("ERROR: El fichero dig_att no se puede abrir\n");
               exit(1);
        }

        if ((fd_cab = fopen("file.cab","w")) == NULL){
             /*  printf("ERROR: El fichero [%s] no se puede abrir\n",header_name); */
               printf("ERROR: El fichero dig_cab no se puede abrir\n");
               exit(1);
        }
	/* this will be over all time */ 
        start_clock (&timer);

    

/* Do initial read of DIGIT file */
/* 	printf ("\n   Leyendo informacion vectorial %s \n",dig_name);  */
        start_clock(NULL);
        


               
	/*  Proceso de eleccion de funciones  */
        printf ("\nSeleccione que opcion describe el tipo de entidades que se van a procesar: \n");
	printf ("\n\t 1) Lineas y puntos. \n");
	printf ("\t 2) Areas \n");
	printf ("\t 3) Lineas en general. Las entidades GRASS seran tipo A y no L \n");
        printf ("\nTeclee el numero correspondiente a la opcion deseada\n");
	option=getchar() ;
/*	if (option!=1 || option!=2 || option!=3) {
		printf ("Opcion incorrecta \n");
                exit (1);
	};
*/
        option=1;
	/*
		OTROS PARAMETROS
	*/
	printf("?A que zona UTM pertenece el mapa?\n");
	while (scanf("%d",&zona)!=1 || zona<0) {
		printf("\nZona UTM incorrecta\n Intentelo de nuevo\n");
	}
	printf("?Cual fue la resolucion a la que fue digitalizado?\n");
	while (scanf("%d",&scale)!=1 || scale<=0) {
		printf("\nResolucion incorrecta\n Intentelo de nuevo\n");
	}
/*
        if (option==1) lineas_puntos(fd,fd_att,fd_asc);
	else if (option==2) areas_puntos(fd,fd_att,fd_asc);
	else if (option==3) todo_tipo;
*/
        lineas_puntos(fd,fd_att,fd_asc);
        printf ("Antes de cabecera \n");
        cabecera(dig_name,fd_cab);
        printf ("Despues de cabecera \n");

        stop_clock(NULL);
        printf("\n");
       
        /*
        Proceso fichero vectorial
        */



        fclose (fd);
        fclose (fd_asc);
        fclose (fd_att);
        fclose (fd_cab);

	printf ( "                  Total time:          ");
	stop_clock (&timer);
	printf ( "\n");
	exit(0);
}

lineas_puntos(fd,fd_att,fd_asc)
FILE *fd,*fd_att,*fd_asc;
{
        
        struct head_record record;
	int categ,cont,num1,err;
	double coordx,coordy,nousado,firstx,firsty;
        char *letraE,car;

	minx=miny=maxx=maxy=0;

        letraE = (char *)malloc(sizeof(char));
        printf (" Antes de while \n");

	while (fscanf(fd,"%5c%10c%32c%d",record.id_record,record.type_feature,record.tag_feature,&record.count) == 4){

/*   Considero que si la etiqueta es numerica debe ser distinta de cero. Si la
    etiqueta es no numerica se perdera */

                car=getc(fd);
                if ((err=sscanf(record.tag_feature,"%d",&categ)) != 1){
                        printf (" record.tag_feature es %d, sscanf es %d\n",num1,err);
                        categ=0;
                };
          /*      else  { 
                      printf ("err es %d \n",err); 
                      categ = num1;
                }
          */
                          
                printf ("record.count %d\n",record.count);
                letraE = "E";
 
                if (*letraE == record.type_feature[3]) {
                        printf ("LINE o EDGE\n");
                        cont=1;
                        fprintf (fd_asc,"L %d\n",record.count);
                }
		else  {
                    printf ("POINT \n");
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

/*
areas_puntos (fd,fd_att,fd_asc)
FILE *fd,*fd_att,*fd_asc;
{           

	struct head_record record;
        int categ,cont;
    	double coordx,coordy,nousado;

	while (fscanf(fd,"%5d%-10s%-39s%5d\n",&record.id_record,record.type_feature,record.tag_feature,&record.count)==5)  {


		sscanf(record.tag_feature,"%d",&categ);
		if (categ==0) {
			if (record.tag_feature == "") categ = -1;
		} 
		if (record.type_feature == "LINE" || record.type_feature == "EDGE") {
			cont=1;
			fprintf (fd_asc,"A %d\n",record.count);
		}
		else if (record.type_feature == "POINT"){
			fprintf (fd_asc,"P %d\n",record.count);
			fscanf(fd,"%f%f%f",&coordx,&coordy,&nousado);
		        limites_window(coordx,coordy);
			fprintf (fd_asc,"%13.2f%13.2f\n",coordy,coordx);
			fprintf (fd_asc,"%13.2f%13.2f\n",coordy,coordx);
			if (categ != -1) fprintf(fd_att,"P%15.2f%15.2f%7d\n",coordx,coordy,categ);
			cont=2; 
                }
		else if (record.type_feature == "AREATAG") {
			fscanf(fd,"%lf%lf%lf",&coordx,&coordy,&nousado);
		        limites_window(coordx,coordy);
			fprintf (fd_att,"A%15.2f%15.2f%7d\n",coordx,coordy,categ);
			cont=2;
		}
                while (cont<=record.count) {
                     fscanf(fd,"%lf%lf%lf",&coordx,&coordy,&nousado);
                     fprintf(fd_asc,"%13.2f%13.2f\n",coordy,coordx);
		     cont++;
		}
	} 
}
	 
*/     
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
