/********************************************************************************
*   
*   v.out.DI input=name_file output=name_file
*  
*   Este comando genera un fichero vectorial formato GENAMAP a partir de un 
*   fichero vectorial GRASS.   
*  
*   INPUT:   name_file. Mapa vectorial de grass, si no se ha generado ya la 
*   topologia debe ejecutarse el comando v.support.
*
*   OUTPUT1:  name_file. Fichero ascii formato genamap, este nombre deberia
*   tener la extension .DI, por ejemplo ~deltam/itge/ZF19/name_file.DI 
*         
*   OUTPUT2:  name_file.head Fichero ascii con datos sobre el mapa, nombre,
*                            zona y coordenadas de la ventana.
*
*   Anabel Donadios Algarra. Junio 1992
*
******************************************************************************/ 



#include <stdio.h>
#include "Vect.h"
#include "gis.h"
#include <string.h>
#include "vtoc.h"



struct line_pnts *Points;

struct area_size {
    double size;
    int area;
};

struct head_record {   /*  El registro de cabecera de un fichero DI */
	int 	id_record;  /*  Formato de 5 caracteres  */
        char	type_feature[10];
	char	tag_feature[32];
	char	blanco[3];
	int	count;      /*  Formato de 5 caracteres  */
};


main(argc, argv)
	int argc;
	char *argv[];
{
        
        int type; /*  tipo de la entidad leida por Vect_read_next_line  */
        struct head_record record;
        char *string;
        int lines,areas; /* numero de lineas (L y P) y areas */
        int tag,cont_arc,ind;
        FILE *fd, *fd_hd;
        double *px,*py;
        double norte,sur,este,oeste;

	char *dig_name, *gena_name, header_name[80]; 
	struct Option *old, *new;
	char errmsg[200];
	char command[100];

	struct Map_info Map;
	struct Cell_head header;
	int stat;
	char *mapset;
	int area;
	long timer;
	long f_size;
	int bufsize;
	int level;

        /* needed to turn off buffering to make clock stuff work right */
        setbuf (stdout, NULL);


/* Initialize gis library */
	G_gisinit(argv[0]) ;

	old = G_define_option();
	old->key		= "input";
	old->type		= TYPE_STRING;
	old->required		= YES;
	old->multiple		= NO;
	old->gisprompt		= "old,dig,vector";
	old->description	= "vector input file";
	
	new = G_define_option();
	new->key		= "output";
	new->type		= TYPE_STRING;
	new->required		= YES;
	new->multiple		= NO;
	new->description	= "DI output file. Por ejemplo: /home/deltam/work_area/ZF19/name_file.DI";


	if (G_parser (argc, argv))
	    exit (-1);

	dig_name = old->answer;
	gena_name = new->answer;

	if (!*dig_name  || !*gena_name )
	{
	    fprintf (stderr, "%s: Error en la linea de comandos: v.out.DI input=name output=name\n\n", argv[0]);
	    G_usage();
	    exit (-1);
	}

        
                  

        if ((fd = fopen(gena_name,"w"))==NULL){
               fprintf (stdout,"ERROR: El fichero [%s] no se puede abrir\n",gena_name);
               fprintf (stdout,"Compruebe los permisos de escritura\n");
               exit(1);
        }
        strcpy(header_name,gena_name);
        strcat(header_name,".head");

        if ((fd_hd = fopen(header_name,"w"))==NULL){
               fprintf (stdout,"ERROR: El fichero [%s] no se puede abrir\n",header_name);
               exit(1);
        }

	/* this will be over all time */
        start_clock (&timer);

    
	if ((mapset = G_find_vector2 (dig_name, "")) == NULL)
	{
	    sprintf (errmsg, "No puedo encontrar el fichero vectorial <%s>\n", dig_name);
	    G_fatal_error (errmsg);
	}

/* Do initial read of DIGIT file */
	fprintf (stdout,"\n   Leyendo informacion vectorial %s \n",dig_name);
        start_clock(NULL);
        

        /*
        Necesito level=2 para obtener los tag de la entidades
        */
	level = Vect_open_old (&Map, dig_name, mapset);
	if (level < 2)
	    G_fatal_error ("You must run v.support before running this program");


        /*
        Creacion de un fichero dig_name.head con la siguiente informacion del fichero 
        vectorial:
		map_name
		escala original
		zona
                Oeste
		Sur
		Este
		Norte
        Los datos sobre la ventana los obtendre con la libreria Vect__get_window, el
        resto directamente de la estructura dig_head head de Map
        */
        norte=sur=este=oeste=0;
        if (Vect__get_window(&Map,&norte,&sur,&este,&oeste)<0)
          fprintf (stdout,"Las coordenadas de las ventanas del mapa no estan accesibles\n");    
                 
        fprintf(fd_hd,"\n Nombre del mapa:	 %s ",Map.head.map_name);
        fprintf(fd_hd,"\n Escala original:	 %ld",Map.head.orig_scale);
        fprintf(fd_hd,"\n Zona:			 %d",Map.head.plani_zone);
        fprintf(fd_hd,"\n Coordenadas de la ventana:	");
        fprintf(fd_hd,"\n Oeste:	%lf",oeste);
        fprintf(fd_hd,"\n Sur:  	%lf",sur);
        fprintf(fd_hd,"\n Este: 	%lf",este);
        fprintf(fd_hd,"\n Norte:	%lf",norte);
       

        stop_clock(NULL);
        fprintf (stdout,"\n");
       
        /*
        Proceso fichero vectorial
        */


	/* init Points struct */

	Points = Vect_new_line_struct ();

	fprintf (stdout, "\n   Creando fichero DI para genamap ...         \n");
	start_clock (NULL);
        
        /* 
        Numero de lineas (L) y puntos (P) del fichero dig_ascii (o dig), el numero total de lineas,
        bordes de areas  y puntos
        */       
        lines=V2_num_lines(&Map);
        /* 
        Numero de areas, y por lo tanto el numero de registros A del fichero dig_att,
        conteniendo los centroides y tag de las areas
        */
        areas=V2_num_areas(&Map);


        cont_arc=0;      /* indice que indica el arco que se esta procesando */ 
      
        while ((type=Vect_read_next_line (&Map,Points)) >0 ){
               cont_arc++; 
               record.id_record = cont_arc;
               /*  Type =1  L
                   Type =2  A
                   Type =4  P
               */
               switch(type){
                  case 1:
                       strcpy (record.type_feature,"LINE");
                       break;
		  case 2:
                       strcpy(record.type_feature,"EDGE");
                       break;
		  case 4: 
                       strcpy(record.type_feature,"POINT");
                       break;
                  default:   
                       fprintf (stdout,"tipo no esperado %d \n",type);
                       break;
               };
               if ((tag = V2_line_att(&Map,cont_arc)) != 0)
                /*  line isn't labeled  */
                    sprintf(record.tag_feature,"%d",tag);
               else
                    sprintf(record.tag_feature,"%s","N.T.");
               strcpy(record.blanco,"");
               record.count=Points->n_points; 
               
               /* Si es un PUNTO solo debo copiar uno de los dos registros, en 
               GRASS un punto viene definido por dos pares de coordenadas
               iguales, en GENAMAP solo con un par */
               if (type==4) record.count--;

               /*
                Escribo el record
               */
               fprintf(fd,"%5d%-10s%-32s%3s%5d\n",record.id_record,record.type_feature,record.tag_feature,record.blanco,record.count);

               /* 
		Escribo la estructura Points 
		*/
               
               px=Points->x;
               py=Points->y;
               while (record.count!=0){
                    fprintf(fd,"%lf %lf\n",*(px),*(py));
                    px++;
                    py++;
                    record.count--;
               }

        }

       /* El maximo numero de registros del fichero dig_att es lines + areas */

        lines=lines+areas;
        ind=1;
        while (lines-- && areas) {
             /*
             Preparo record AREATAG
             */
             if ((Map.Att+ind)->type == 2) {
                record.id_record = ++cont_arc;
                strcpy(record.type_feature,"AREATAG");
                sprintf(record.tag_feature,"%d",(Map.Att+ind)->cat);
                record.count=1;
                areas--;
                fprintf(fd,"%5d%-10s%-32s%3s%5d\n",record.id_record,record.type_feature,record.tag_feature,record.blanco,record.count);
                fprintf(fd,"%lf %lf\n",(Map.Att+ind)->x,(Map.Att+ind)->y);
             }
             ind++;

        }
        stop_clock(NULL);
        fprintf (stdout,"\n");
        



	Vect_destroy_line_struct (Points);	/* free it up when finished */
        Vect_close (&Map);
        fclose (fd);

	fprintf (stdout, "                  Total time:          ");
	stop_clock (&timer);
	fprintf (stdout, "\n");
	exit(0);
}

