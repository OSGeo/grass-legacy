/****************************************************************************/
/* PROGRAM: undxf  							    */
/*									    */
/* Description: Takes a .dxf file from AutoCAD or other similar programs    */
/* and converts the data into unwam11 format to be converted to a sagis     */
/* mapfile.								    */
/*									    */
/* The features from AutoCAD that are processed are:			    */
/*   lines 								    */
/*   polylines								    */
/*   polygons								    */
/*   sketches								    */
/*   points								    */
/*   donuts								    */
/*   circles								    */
/*   arcs								    */ 
/*									    */
/*   The features from AutoCAD that are not processed:			    */
/*   blocks								    */
/*   text								    */
/*									    */
/* Special notes:							    */
/* an id is created called 'FRAME' from the extents bounds in the .dxf file */
/* this id can be used to window on.					    */
/* 									    */
/* 									    */
/* Programmer: Tom Howard   National Park Service GIS division		    */
/* 									    */
/****************************************************************************/

#include <stdio.h>
#include <math.h>

/*  define constants */

#define  DEBUG          1
#define  IGNORE         0
#define  EOFC           1
#define  SECTION	2
#define  ENDSEC		3
#define  TABLES		4
#define  ENDTAB		5
#define  POLYLINE	6
#define  SEQEND  	7
#define  HEADER		8
#define  VIEW		9
#define  BLOCKS		10
#define  STYLE		11
#define  LAYER		12
#define  MIN		13
#define  MAX		14
#define  VERTEX		15
#define  X_COORDINATE	16
#define  Y_COORDINATE	17
#define  LABEL          18
#define  ENTITIES       19
#define  BLOCK          20
#define  CIRCLE         21
#define  CLOSED         22
#define  ARC            23
#define  DONUT          24
#define  IN_RADIUS      25
#define  OUT_RADIUS     26
#define  LINE           27
#define  POINT          28
#define  LINE3D         29
#define  FACE3D         30
#define  SOLID	        31
#define  FF             100000.0
#define  FFM            1000.0
 
/* global variables */

FILE *infil;
FILE *outfil;

/****************************************************************************/
/* Function: get_return_code						    */
/*									    */
/* Description: This function returns an integer value corresponding to the */
/* appropriate section label or input type.				    */
/* 									    */
/* Global variables : NONE						    */
/*									    */
/* Passed parameters: code     - Inputed numeric code.			    */
/*                    string   - Section or variable label.		    */
/*									    */
/* Local variables  : NONE  						    */
/*									    */
/* Programmer: Tom Howard   National Park Service GIS division		    */
/* 									    */
/* Creation Date: 2-16-89               Last Modification: 2-22-89	    */
/*									    */
/****************************************************************************/

int get_return_code(code,string)

int     code;
char    string[];

{
 if (strcmp("EOF",     string)==0) return (EOFC);
 if (strcmp("SECTION", string)==0) return (SECTION);
 if (strcmp("ENDSEC",  string)==0) return (ENDSEC);
 if (strcmp("TABLES",  string)==0) return (TABLES);
 if (strcmp("ENDTAB",  string)==0) return (ENDTAB);
 if (strcmp("POLYLINE",string)==0) return (POLYLINE);
 if (strcmp("SEQEND",  string)==0) return (SEQEND);
 if (strcmp("HEADER",  string)==0) return (HEADER);
 if (strcmp("VIEW",    string)==0) return (VIEW);
 if (strcmp("BLOCK",   string)==0) return (BLOCK);
 if (strcmp("STYLE",   string)==0) return (STYLE);
 if (strcmp("$EXTMIN", string)==0) return (MIN);
 if (strcmp("$EXTMAX", string)==0) return (MAX);
 if (strcmp("VERTEX",  string)==0) return (VERTEX);
 if (strcmp("CIRCLE",  string)==0) return (CIRCLE);
 if (strcmp("ENTITIES",string)==0) return (ENTITIES);
 if (strcmp("ARC",     string)==0) return (ARC);
 if (strcmp("LINE",    string)==0) return (LINE);
 if (strcmp("POINT",   string)==0) return (POINT);
 if (strcmp("SOLID",   string)==0) return (SOLID);
 if (strcmp("3DLINE",  string)==0) return (LINE3D);
 if (strcmp("3DFACE",  string)==0) return (FACE3D);


   switch (code)
      {
       case  0:   return (LABEL);
       case  8:   return (LAYER);
       case 10:   return (X_COORDINATE);
       case 11:   return (X_COORDINATE);
       case 12:   return (X_COORDINATE);
       case 13:   return (X_COORDINATE);
       case 14:   return (X_COORDINATE);
       case 15:   return (X_COORDINATE);
       case 16:   return (X_COORDINATE);
       case 17:   return (X_COORDINATE);
       case 18:   return (X_COORDINATE);
       case 20:   return (Y_COORDINATE);
       case 21:   return (Y_COORDINATE);
       case 22:   return (Y_COORDINATE);
       case 23:   return (Y_COORDINATE);
       case 24:   return (Y_COORDINATE);
       case 25:   return (Y_COORDINATE);
       case 26:   return (Y_COORDINATE);
       case 27:   return (Y_COORDINATE);
       case 28:   return (Y_COORDINATE);
       case 40:   return (IN_RADIUS);
       case 42:   return (OUT_RADIUS);
       case 70:   return (CLOSED); 
      }

/*   return (IGNORE);  */

}  /*  get_return_value */

/****************************************************************************/
/* Function: open_files							    */
/*									    */
/* Description: opens data file (name supplied by user) and a scratch file  */
/* called SCRATCH for output.				                    */
/* 									    */
/* Global variables : infil   - input file pointer.			    */
/* 		      outfil  - output file pointer.			    */
/*									    */
/* Passed parameters: input file name (supplied by user)		    */
/*									    */
/* Local variables  : outfile_name - output filename    		    */
/*                    tempin_file  - temporary file name                    */
/*									    */
/* Programmer: Tom Howard   National Park Service GIS division		    */
/* 									    */
/* Creation Date: 2-16-89               Last Modification: 2-16-89	    */
/*									    */
/****************************************************************************/

int open_files(infile_name)
char infile_name[];
{
   FILE *fopen();
   char outfile_name[30];
   char tempin_file[30];

   strcpy(tempin_file,infile_name);
   strcpy(outfile_name,infile_name);
   strcat(infile_name,".dxf");
   while (((infil=fopen(infile_name,"r+"))==NULL) &&
          (strcmp(infile_name,"q") != 0))
      {
       printf("Unable to read %s\nEnter new filename (q=quit) ",infile_name);
       scanf("%s",infile_name);
       strcpy(tempin_file,infile_name);
       strcpy(outfile_name,infile_name);
       strcat(infile_name,".dxf");
      }  /* end while */

   if (strcmp(infile_name,"q.dxf")==0)
      exit(2);
   else
      {
       printf("%s\n",tempin_file);
       outfil = fopen(outfile_name,"w+");
      }
}  /* open_files */

/****************************************************************************/
/* Function: read_pair							    */
/*									    */
/* Description: Reads a code label pair from input data file.               */
/* 									    */
/* Global variables : infil   - input file pointer.			    */
/*									    */
/* Passed parameters: string  - holds label value read from data file	    */
/*                    floating- read data read in (sometimes).		    */
/*									    */
/* Local variables  : code    - code read in from data file.	            */
/*                    junk    - just what it says                           */
/*									    */
/* Programmer: Tom Howard   National Park Service GIS division		    */
/* 									    */
/* Creation Date: 2-16-89               Last Modification: 2-26-89	    */
/*									    */
/****************************************************************************/

int read_pair(string,floating)
char string[];
double *floating;
{
 int  code;
 int  integer;
 char junk[100];
 

  fscanf(infil,"%d",&code);

  switch(code)
     {
      case 0: /* start of entity, table entry, or file separator */
      case 2: /* A name: attribute tag, block name, etc.         */
      case 8: /* Layer name                                      */
      case 9: /* Variable name identifier.			    */
    
              fscanf(infil,"%s",string);
              return(get_return_code(code,string));
              break;
      case 10: /*  10-18 x values                                  */
      case 11: 
      case 12: 
      case 13: 
      case 14:
      case 15: 
      case 16: 
      case 17: 
      case 18:
      case 20: /*  20-28  y values                                */
      case 21: 
      case 22: 
      case 23: 
      case 24:
      case 25: 
      case 26: 
      case 27: 
      case 28:
      case 40:
      case 42:
      case 50: /*  50-58 angles	        			    */
      case 51: 
      case 52: 
      case 53: 
      case 54: 
      case 55: 
      case 56: 
      case 57: 
      case 58:

         fscanf(infil,"%lf",floating);
         strcpy(string," ");
         return(get_return_code(code,string));
         break;

      case 70: 
         fscanf(infil,"%d",&integer);
         strcpy(string," ");
         return(get_return_code(code,string)); 
         break;

      default:
              fgets(junk,80,infil); 
             return(IGNORE);
    }  /* end switch (code)  */
} /* read_pair */

/****************************************************************************/
/* Function: frame							    */
/*									    */
/* Description: Creates an id called frame using the maximum and minimum    */
/* values taken from the data file header variables $EXTMIN and $EXTMAX     */
/* 									    */
/* Global variables : outfil - output file pointer			    */
/*									    */
/* Passed parameters: xmin   - minimum x coordinate value                   */
/*                    ymin   - minimum y coordinate value                   */
/*                    xmax   - maximum x coordinate value                   */
/*                    ymax   - maximum y coordinate value                   */
/*									    */
/* Local variables  : NONE						    */
/*									    */
/* Programmer: Tom Howard   National Park Service GIS division		    */
/* 									    */
/* Creation Date: 2-16-89               Last Modification: 2-28-89	    */
/*									    */
/****************************************************************************/

int frame(xmin,ymin,xmax,ymax)
double xmin;
double ymin;
double xmax;
double ymax;
{
   fprintf(outfil,"%15s%-10s%25s%5d\n"," ","FRAME"," ",5);
   fprintf(outfil,"%11.2lf%11.2lf\n",xmax,ymax);
   fprintf(outfil,"%11.2lf%11.2lf\n",xmax,ymin);
   fprintf(outfil,"%11.2lf%11.2lf\n",xmin,ymin);
   fprintf(outfil,"%11.2lf%11.2lf\n",xmin,ymax);
   fprintf(outfil,"%11.2lf%11.2lf\n",xmax,ymax);

}  /* frame */

/****************************************************************************/
/* Function: process_header						    */
/*									    */
/* Description: Read information from header section extracting the extents */
/* values used to make the id frame, and ignoreing the rest.                */
/* 									    */
/* Global variables : infil  - input file pointer 			    */
/*									    */
/* Passed parameters: NONE						    */
/*									    */
/* Local variables  : return_code  - code value returned from read_pair     */ 
/*                    junk1        - just what it says                      */
/*                    junk2        - ditto                                  */
/*                    xmin,        - minimum x-coordinate value             */
/*                    ymin         - minimum y-coordinate value             */
/*                    xmax         - maximum x-coordinate value             */
/*                    ymax         - maximum y-coordinate value             */
/*									    */
/* Programmer: Tom Howard   National Park Service GIS division		    */
/* 									    */
/* Creation Date: 2-16-89               Last Modification: 2-26-89	    */
/*									    */
/****************************************************************************/

int process_header()
{
   int     return_code;
   char    junk1[30];
   double  junk2;
   double  xmin;
   double  ymin;
   double  xmax;
   double  ymax;

   do /* process data until end of header section */
      {
       return_code = read_pair(junk1,&junk2);
       switch (return_code)
          {
           case MIN : return_code=read_pair(junk1,&xmin);
                      return_code=read_pair(junk1,&ymin);
                      break;
           case MAX : return_code=read_pair(junk1,&xmax);
                      return_code=read_pair(junk1,&ymax);
                      break;
          }  /* end switch (return_code) */
      } while (return_code != ENDSEC);

/*  create id 'FRAME' from min and max points    */
   frame(xmin*FFM+FF,ymin*FFM+FF,xmax*FFM+FF,ymax*FFM+FF);
}  /* process_header */ 

/****************************************************************************/
/* Function: process_ignores 						    */
/*									    */
/* Description: Reads through sections TABLES, STYLE, BLOCKS, LAYER, and    */
/* VIEW and just ignores the data.                                          */
/* 									    */
/* Global variables : NONE						    */
/*									    */
/* Passed parameters: NONE						    */
/*									    */
/* Local variables  : return_code  - code value returned from read_pair     */
/*                    junk1        - just what it says			    */
/*                    junk2        - ditto				    */
/*									    */
/* Programmer: Tom Howard   National Park Service GIS division		    */
/* 									    */
/* Creation Date: 2-16-89               Last Modification: 2-16-89	    */
/*									    */
/****************************************************************************/

int process_ignores()
{
   int    return_code;
   char   junk1[30];
   double junk2;

   do {  /* skip to end of section, ignoring all data */
       return_code = read_pair(junk1,&junk2);
      } while (return_code != ENDSEC);

}  /* process_ignores */

/****************************************************************************/
/* Function:  write_out							    */
/*									    */
/* Description: Writes arc data to output file				    */
/* 									    */
/* Global variables : outfil	      - output file pointer	            */
/*									    */
/* Passed parameters: x               - array containing x-coordinates      */
/*	              y  	      - array containing y-coordinates      */
/*		      pair_index      - index for coordinate arrays	    */ 
/*                    flag            - flag true if donut hole             */
/*									    */
/* Local variables  : index	      - index for arrays		    */
/*									    */
/* Programmer: Tom Howard   National Park Service GIS division		    */
/* 									    */
/* Creation Date: 2-22-89               Last Modification: 2-28-89	    */
/*									    */
/****************************************************************************/

int write_out(id_name,x,y,pair_index,flag)
char    id_name[];
double  x[];
double  y[];
int     pair_index;
int     flag;
{
 int index;

 if (pair_index > (-1))
    {
     if (flag == 0 )
        {
        fprintf(outfil,"%15s%-10s%25s%5d\n"," ",id_name," ",pair_index+1);
        for (index=0; index <= pair_index;index++)
           fprintf(outfil,"%11.2lf%11.2lf\n",x[index]*FFM+FF,y[index]*FFM+FF);
        }
     if (flag == 1)
        {
         x[0] = (-(x[0]*FFM+FF));
         fprintf(outfil,"%11.2lf%11.2lf\n",x[0],y[0]*FFM+FF);
         for (index=1; index <= pair_index;index++)
            fprintf(outfil,"%11.2lf%11.2lf\n",x[index]*FFM+FF,y[index]*FFM+FF);
        }
     if (flag == 2)
        {
         fprintf(outfil,"%15s%-10s%25s%5d\n"," ",id_name," ",pair_index*2+2);
         for (index=0; index <= pair_index;index++)
            fprintf(outfil,"%11.2lf%11.2lf\n",x[index]*FFM+FF,y[index]*FFM+FF);
        }
    }  /* end if */
}  /* write_out  */

/****************************************************************************/
/* Function: make_arc							    */
/*									    */
/* Description: Creates an arc using a center point, a radius and a start   */
/* and end angle                                                            */
/* 									    */
/* Global variables : outfil      - output file pointer                     */
/*									    */
/* Passed parameters: centerx     - x center point                          */
/*                    centery     - y center point                          */
/*                    radius      - radius of arc                           */
/*                    start_angle - starting angle for arc                  */
/*                    finish_angle- ending angle for arc                    */
/*		      x           - array containing x coordinates          */
/*                    y           - array containing y coordiantes          */
/*		      pair_index  - index for arrays			    */
/*		      flag        - 0 if outline, 1 if hole, 2 if arc       */
/*									    */
/* Local variables  : alpha       - angle in radians                        */
/*		      theta       - angle in degrees                        */
/*                    step        - increment step                          */
/*                    multiple    - multiplication factor                   */
/*                    pair_index  - index for x, y arrays                   */ 
/*                    index       - array index				    */
/*                    start_index - index value before arc creation         */
/*									    */
/* Programmer: Tom Howard   National Park Service GIS division		    */
/* 									    */
/* Creation Date: 2-21-89               Last Modification: 3-2-89	    */
/*									    */
/****************************************************************************/

int make_arc(centerx,centery,radius,start_angle,finish_angle,x,y,
             pair_index,flag)
double  centerx;
double  centery;
double  radius;
double  start_angle;
double  finish_angle;
double  x[];
double  y[];
int     *pair_index;
int     flag;

{
 double  alpha;
 double  theta;
 double  sin();
 double  cos();
 double  step;
 int     index;
 int     start_index;

 start_index = *pair_index+1;
 step = 10.0;
 if (flag == 0)   /* outline  has to go clockwise */
    {
     if (start_angle <= finish_angle)
       {
        theta = finish_angle;
        do {
            alpha = 3.141592654*theta/180.0;
            x[++*pair_index] = radius*cos(alpha)+centerx;
            y[*pair_index] = radius*sin(alpha)+centery;

            theta = theta - step;
            } while (theta >= start_angle);
   
       }
      else
        {
        theta = start_angle;
        do {
            alpha = 3.141592654*theta/180.0;
            x[++*pair_index] = radius*cos(alpha)+centerx;
            y[*pair_index] = radius*sin(alpha)+centery;

            theta = theta - step;
           } while (theta <= finish_angle);
       } 
    }
   else   
     if (flag == 1)  /* its a hole, has to go counter-clockwise */ 
        {
         if (start_angle <= finish_angle)
           {
            theta = finish_angle;
           do {
               alpha = 3.141592654*theta/180.0;
               x[++*pair_index] = radius*cos(alpha)+centerx;
               y[*pair_index] = radius*sin(alpha)+centery;

               theta = theta - step;
              } while (theta >= start_angle);
           }
         else
           {
            theta = finish_angle;
            do {
                alpha = 3.141592654*theta/180.0;
                x[++*pair_index] = radius*cos(alpha)+centerx;
                y[*pair_index] = radius*sin(alpha)+centery;

                theta = theta + step;
               } while (theta <= start_angle);
            }
       }
    else
       if (flag ==2 )   /* an arc */
          {
            if (start_angle > finish_angle)
               theta = finish_angle + 360.0;
            else 
               theta = finish_angle;
            do {
                alpha = 3.141592654*theta/180.0;
                x[++*pair_index] = radius*cos(alpha)+centerx;
                y[*pair_index] = radius*sin(alpha)+centery;

                theta = theta - step;
               } while (theta >= start_angle);
       }  /* if (flag == 2) */
}  /* make_arc */

/****************************************************************************/
/* Function: process_circle						    */
/*									    */
/* Description: Reads circle information and creates a circle.              */
/* 									    */
/* Global variables : NONE						    */
/*									    */
/* Passed parameters: NONE						    */
/*									    */
/* Local variables  : return_code   - value returned from read_pair	    */
/*		      label         - hold string value			    */
/* 		      id_name       - id (layer) name                       */
/*                    centerx       - x center point                        */
/*                    centery       - y center point                        */
/*                    radius        - radius of circle                      */
/*                    junk          - junk                                  */
/*		      x		    - x-coordinate array		    */
/*	 	      y		    - y-coordinate array		    */
/*		      pair_index    _ coordinate array index		    */
/*									    */
/* Programmer: Tom Howard   National Park Service GIS division		    */
/* 									    */
/* Creation Date: 2-16-89               Last Modification: 2-28-89	    */
/*									    */
/****************************************************************************/

int process_circle()
{
 int     return_code;
 int     id_name[30];
 int     pair_index;
 char    label[30];
 double  centerx;
 double  centery; 
 double radius;
 double  junk;
 double  x[3000];
 double  y[3000];

 return_code = read_pair(label,&junk);
 if (strcmp(label,"0")==0)
   strcpy(id_name,"UNKNOWN");
 else
   strcpy(id_name,label);

 do {  /* ignore attribute codes  */ 
     return_code = read_pair(label,&junk);
    } while (return_code != X_COORDINATE);

 centerx = junk;
 return_code = read_pair(label,&centery);
 return_code = read_pair(label,&radius);
pair_index = (-1); 
make_arc(centerx,centery,radius,0.0,360.0,x,y,&pair_index,0);
write_out(id_name,x,y,pair_index,0);
} /*   process_circle  */

/****************************************************************************/
/* Function: process_arc						    */
/*									    */
/* Description: Reads arc information and creates an arc                    */
/* 									    */
/* Global variables : NONE						    */
/*									    */
/* Passed parameters: NONE						    */
/*									    */
/* Local variables  : return_code   - value returned from read_pair	    */
/*		      label         - hold string value			    */
/* 		      id_name       - id (layer) name                       */
/*                    centerx       - x center point                        */
/*                    centery       - y center point                        */
/*                    radius        - radius of circle                      */
/*                    start_angle   _ beginning angle                       */
/*                    finish_angle  - finish angle                          */
/*                    junk          - junk                                  */
/*		      x		    - x-coordinate array		    */
/*		      y 	    - y-coordinate array		    */
/*		      pair_index    - coordiante arrays index		    */
/*									    */
/* Programmer: Tom Howard   National Park Service GIS division		    */
/* 									    */
/* Creation Date: 2-16-89               Last Modification: 3-2-89	    */
/*									    */
/****************************************************************************/

int process_arc()
{
 int     return_code;
 int     id_name[30];
 int     pair_index;
 char    label[30];
 double  centerx;
 double  centery; 
 double  radius;
 double  start_angle;
 double  finish_angle;
 double  junk;
 double  x[3000];
 double  y[3000];

 return_code = read_pair(label,&junk);
 if (strcmp(label,"0")==0)
   strcpy(id_name,"UNKNOWN");
 else
   strcpy(id_name,label);
 
 return_code = read_pair(label,&centerx);
 return_code = read_pair(label,&centery);
 return_code = read_pair(label,&radius);
 return_code = read_pair(label,&start_angle);
 return_code = read_pair(label,&finish_angle);

 pair_index = (-1); 
 make_arc(centerx,centery,radius,start_angle,finish_angle,x,y,
          &pair_index,2);
 write_out(id_name,x,y,pair_index,0);

} /*   process_arc  */

/****************************************************************************/
/* Function: process_line						    */
/*									    */
/* Description:  reads line data from input file and creates a line on the  */
/* output file.								    */
/* 									    */
/* Global variables : outfil        - output file pointer.		    */
/*									    */
/* Passed parameters: NONE						    */
/*									    */
/* Local variables  : begin_x       - beginning x coordinate                */
/*                    begin_y       - beginning y coordinate		    */
/*                    end_x         - ending x coordinate		    */
/*		      end_y         - ending y coordinate		    */
/* 		      junk	    - just what it says			    */ 
/*		      id_name       - id (layer) name                       */
/*		      label	    - string				    */
/*	              return_code   - value returned from read_pair         */
/*									    */
/* Programmer: Tom Howard   National Park Service GIS division		    */
/* 									    */
/* Creation Date: 2-23-89               Last Modification: 2-28-89	    */
/*									    */
/****************************************************************************/

int process_line()
{
 double  begin_x;
 double  begin_y;
 double  end_x;
 double  end_y;
 double  junk;
 char    id_name[30];
 char    label[30];
 int     return_code;

 return_code = read_pair(label,&junk);

 if (strcmp(label,"0")==0)
   strcpy(id_name,"UNKNOWN");
 else
   strcpy(id_name,label);

/*  ignore attribute codes  */
 while ((read_pair(label,&junk) != X_COORDINATE)); 
       
 begin_x = junk;
 return_code = read_pair(label,&begin_y);
 return_code = read_pair(label,&end_x);
 return_code = read_pair(label,&end_y);

 fprintf(outfil,"%15s%-10s%25s%5d\n"," ",id_name," ",2);
 fprintf(outfil,"%11.2lf%11.2lf\n",begin_x*FFM+FF,begin_y*FFM+FF);
 fprintf(outfil,"%11.2lf%11.2lf\n",end_x*FFM+FF,end_y*FFM+FF);
} /* process_line */

/****************************************************************************/
/* Function: process_point						    */
/*									    */
/* Description:  creates a point on the output data file		    */
/* 									    */
/* Global variables : outfil    - output file pointer			    */
/*									    */
/* Local variables  : x          - x-coordinate				    */ 
/*		      y          - y-coordinate				    */
/*		      junk       - just what it says			    */
/*                    id_name    - id (layer) name			    */
/*    		      label      - character string			    */
/*		      return_code- value returned from read_pair            */
/*									    */
/* Programmer: Tom Howard   National Park Service GIS division		    */
/* 									    */
/* Creation Date: 2-24-89               Last Modification: 2-28-89	    */
/*									    */
/****************************************************************************/

int process_point()
{
 double  x;
 double  y;
 double  junk;
 char    id_name[30];
 char    label[30];
 int     return_code;

 return_code = read_pair(label,&junk);

 if (strcmp(label,"0")==0)
   strcpy(id_name,"UNKNOWN");
 else
   strcpy(id_name,label);
 
 return_code = read_pair(label,&x);
 return_code = read_pair(label,&y);

 fprintf(outfil,"%15s%-10s%25s%5d\n"," ",id_name," ",1);
 fprintf(outfil,"%11.2lf%11.2lf\n",x*FFM+FF,y*FFM+FF);
} /* process_point */

/****************************************************************************/
/* Function: process_solid						    */
/*									    */
/* Description: Processes either a SOLID or a FACE3D form the .dxf file     */
/* 									    */
/* Global variables : outfil             - output file pointer              */
/*									    */
/* Passed parameters: label              - new entity label                 */
/*                    return_value       - code value		            */
/*									    */
/* Local variables  : return_code        - code returned by read_pair       */
/*	              pair_index         - x,y array index                  */
/*                    x[5]               - x-coordinate array		    */
/*	              y[5]               - y-coordinate array               */
/*                    junk               - junk				    */
/*                    id_name[30]        - id (layer) name		    */
/*                    label[30]          - character string                 */
/*		      index              - array index                      */
/*									    */
/* Programmer: Tom Howard   National Park Service GIS division		    */
/* 									    */
/* Creation Date: 3-2-89                Last Modification: 3-2-89	    */
/*									    */
/****************************************************************************/

int process_solid()
{
int    return_code;
int    pair_index ;
int    index;
double x[5];
double y[5];
double junk;
char   id_name[30];
char   label[30];

 return_code = read_pair(label,&junk);
 if (strcmp(label,"0")==0)
   strcpy(id_name,"UNKNOWN");
 else
   strcpy(id_name,label);
 
 for (pair_index = 0; pair_index <4;)                     
    {
     /*  get only x and y values, ignore the z's  */
     return_code = read_pair(label,&junk);
     switch (return_code)
        {
         case X_COORDINATE   : x[pair_index] = junk;
                               break;

         case Y_COORDINATE   : y[pair_index] = junk; 
                               pair_index += 1;
                               break;
        } 
    }  /* for */

 fprintf(outfil,"%15s%-10s%25s%5d\n"," ",id_name," ",pair_index);

 for (index = 0; index < pair_index; index++)
  {
    fprintf(outfil,"%11.2lf%11.2lf\n",
(x[index]*(double)FFM+(double)FF), (y[index]*(double)FFM+(double)FF) );
  }

}  /* process_solid */

/****************************************************************************/
/* Function: process_polyline						    */
/*									    */
/* Description: Reads in layer (id) name and x,y pairs from data file and   */
/* outputs a header and a polyline to the output data file		    */
/*								            */
/* Donuts, ellipses, polygons, and polylines are all concidered polylines   */
/* in .dxf files so they are all handled in this section                    */
/* 									    */
/* Global variables : outfil  - output data to file pointer		    */
/*									    */
/* Passed parameters: NONE						    */
/*									    */
/* Local variables  : index            - array index			    */
/*		      return_code      - code value returned fro read_pair  */
/*		      pair_index       - array index                        */
/*	 	      label	       - label read in from input file      */
/*                    id_name	       - name of id			    */
/*		      x		       - x-coordinate array		    */
/*		      y		       - y-coordinate array		    */
/*		      value	       - real value inputted from data file */
/*		      flag             - closed polygon flag		    */
/*		      donut_flag       - flag set to true fo donut	    */
/*                    inner_radius     - donut inner radius                 */
/*                    outer_radius     - donut outer radius                 */
/*                    x_average        - average of x center points         */
/*									    */
/* Programmer: Tom Howard   National Park Service GIS division		    */
/* 									    */
/* Creation Date: 2-16-89               Last Modification: 3-1-89	    */
/*									    */
/****************************************************************************/

int process_polyline()
{
 int       index;
 int       return_code;
 int       pair_index;
 int       flag=0;
 int       donut_flag = (-1) ;
 char      label[30];
 char      id_name[30];
 double	   x[3000];
 double    y[3000];
 double    value;
 double    inner_radius;
 double    outer_radius;
 double    x_average;

 pair_index = (-1);
 do {   /* process until SEQEND is reached */
     return_code = read_pair(label,&value);
     switch(return_code) 
        {
         case LAYER          :if(strcmp(label,"0")==0)
                                 strcpy(id_name,"UNKNOWN");
                               else
                                 strcpy(id_name,label);
 			       break;

         case X_COORDINATE   :x[++pair_index] = value;
                              break;

         case Y_COORDINATE   :y[pair_index] = value;
                              break;

         case IN_RADIUS      :inner_radius = value;
			      donut_flag = 1;
                              break;

         case OUT_RADIUS     :outer_radius = value;
			      break;

         case CLOSED	     :flag = 1;
       }  
   } while (return_code != SEQEND);

/* output polyline to file  */
  if (donut_flag==1)
     {
      pair_index = (-1);
      x_average = (x[0] + x[1])/2;
      make_arc(x_average,y[0],outer_radius/2,0.0,360.0,x,y,&pair_index,0);
      write_out(id_name,x,y,pair_index,2);
      pair_index = (-1);
      make_arc(x_average,y[0],inner_radius,360.0,0.0,x,y,&pair_index,1);
      write_out(id_name,x,y,pair_index,1);
     }    /* donut creation */
  else
     {
      if (flag)
         {
          pair_index++;
          x[pair_index] = x[0];
          y[pair_index] = y[0];
         }
      if (pair_index > (-1)) 
         {
          fprintf(outfil,"%15s%-10s%25s%5d\n"," ",id_name," ",pair_index+1);
          for (index = 0;index <=pair_index;index++)
             fprintf(outfil,"%11.2lf%11.2lf\n",x[index]*FFM+FF,y[index]*FFM+FF);
         }
     }
}   /*   process_polyline */

/****************************************************************************/
/* Function: process_entities					            */
/*									    */
/* Description: processes ENTITIES section of DXF file                      */ 
/* 									    */
/* Global variables : NONE						    */
/*									    */
/* Passed parameters: NONE						    */
/*									    */
/* Local variables  : return_code    - values returned by read_pair         */
/*		      label          - string value			    */
/*		      junk	     - just what it says		    */
/*		      id_name        - id (layer) name                      */
/*									    */
/* Programmer: Tom Howard   National Park Service GIS division		    */
/* 									    */
/* Creation Date: 2-24-89               Last Modification: 2-28-89	    */
/*									    */
/****************************************************************************/

int process_entities()
{
 char    id_name[30];
 int     return_code;
 double  junk;
 char    label[30];

 do {    /* process until ENDSEC is reached */
     return_code = read_pair(label,&junk);
     strcpy(id_name," ");
     switch(return_code)
        {
         case POLYLINE     :process_polyline();
                            break;

         case CIRCLE       :process_circle();
			    break;

         case ARC	   :process_arc();
			    break;

         case POINT	   :process_point();
			    break;

         case LINE3D       :
         case LINE	   :process_line();
                            break;

         case SOLID        :
         case FACE3D       :process_solid();
                            break;
        
         case EOFC         :return;
                            break;

        }  /* switch  */
    }   while (return_code != ENDSEC);
}   /* process_entities  */

/****************************************************************************/
/* Function: process						            */
/*									    */
/* Description:  calls appropriate process to handle sections		    */
/* 									    */
/* Global variables : NONE						    */
/*									    */
/* Passed parameters: NONE						    */
/*									    */
/* Local variables  : return_code    - value returned by read_code	    */
/*		      label          - code label			    */
/*		      junk	     - just what it says		    */
/*									    */
/* Programmer: Tom Howard   National Park Service GIS division		    */
/* 									    */
/* Creation Date: 2-16-89               Last Modification: 2-28-89	    */
/*									    */
/****************************************************************************/

int process()
{
 int      return_code;
 char     label[30];
 double   junk;

 do {
    return_code = read_pair(label,&junk);
    switch(return_code)
      {
 
       case HEADER 	   :process_header();
                            break;

       case TABLES         :
       case BLOCKS         :process_ignores();
                            break;

       case ENTITIES       :process_entities();
                            break;

      }
   } while(return_code != EOFC);
}  /* process */

/****************************************************************************/
/* Function: close_up_shop					            */
/*									    */
/* Description: closes all open files					    */
/* 									    */
/* Global variables : infil       - input file pointer			    */
/*		      outfil      - output file pointer			    */
/*									    */
/* Passed parameters: NONE					            */
/*									    */
/* Local variables  : NONE						    */
/*									    */
/* Programmer: Tom Howard   National Park Service GIS division		    */
/* 									    */
/* Creation Date: 2-16-89               Last Modification: 2-16-89	    */
/*									    */
/****************************************************************************/

int close_up_shop()
{
 fclose(infil);
 fclose(outfil);
}   /* close_up_shop  */

/****************************************************************************/
/* Function: main							    */
/*									    */
/* Description: Controls flow of program				    */
/* 									    */
/* Global variables : infile_name       - input file name		    */
/*									    */
/* Passed parameters: argc              - argument count		    */
/*		      argv              - argument vector                   */
/*									    */
/* Local variables  : infile_name       - input file string                 */
/*									    */
/* Programmer: Tom Howard   National Park Service GIS division		    */
/* 									    */
/* Creation Date: 2-16-89               Last Modification: 2-28-89	    */
/*									    */
/****************************************************************************/

main(argc,argv)
int   argc;
char  *argv[];
{
char  infile_name[30];
/* determine if filename is passed to the program */
if (argc == 2)
   strcpy(infile_name,argv[1]);
else
   {
    printf("Input filename: ");
    scanf("%s",infile_name);
   }
open_files(infile_name);
process();
close_up_shop();
}  /* main  */
