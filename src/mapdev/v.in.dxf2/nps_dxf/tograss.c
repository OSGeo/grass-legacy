#include <stdio.h>
#include <math.h>
#include <ctype.h>

#define  TRUE                      1
#define  FALSE			   0
#define  FIRST			   0
#define  MAX_ORGANIZATION          30
#define  MAX_DIGIT_DATE		   20
#define  MAX_MAP_NAME		   40
#define  MAX_MAP_DATE		   11
#define  MAX_OTHER_INFO		   73
#define  MAX_CATEGORIES		   999
#define  MAX_CATEGORY_LENGTH	   30
#define  MAX_FILE_NAME		   30
#define  STRING			   1
#define  INTEGER		   2
#define  REAL			   3
#define  STRING_MAX		   80
#define  MAX_INTEGER_LENGTH	   40
#define  MAX_LENGTH		   40
#define  MAX_POINTS                3000
#define  MAX_DIGIT_NAME            20

struct category {
                  int  category_count;
		  char *categories[MAX_CATEGORIES];
                };

struct header   {
                 double    north;
                 double    south;
                 double    east;
                 double    west;
                 char      organization[MAX_ORGANIZATION];
                 char      digit_date[MAX_DIGIT_DATE];
                 char      map_name[MAX_MAP_NAME];
                 char      map_date[MAX_MAP_DATE];
                 int       scale;
                 char      other_info[MAX_OTHER_INFO];
                 int       zone;
                 double    thresh;
                 char      digit_name[MAX_DIGIT_NAME];
                };

/* global variables */

FILE  *infp;
FILE  *scratchfp;
FILE  *digfp;
FILE  *sitefp;
FILE  *attfp;
FILE  *catfp;
char  digit_filename[MAX_LENGTH];

/***************************************************************************/
double min(value1,value2)
double value1;
double value2;
{
  if (value1 <= value2)
     return(value1);
  else
     return(value2);
}  /*  min  */

/***************************************************************************/
int  get_string(string,max_length,type)
char   string[];
int    max_length;
int    type;
{
  int   index;
  int   c;
  char  temp_string[STRING_MAX];
  int   error_flag;
  int   decimal_flag;

      decimal_flag = FALSE;
      error_flag = FALSE;
      index = 0;

      while (((c= getchar()) != '\n') && (index < max_length))
         {

           switch(type)
             {
              case STRING   :temp_string[index++] = c;
                             error_flag = FALSE;
                             break;

              case INTEGER  :if (isdigit(c) != FALSE)
                                temp_string[index++] = c;
                             else
                                error_flag = TRUE;
                             break;

              case REAL     : if ((isdigit(c) != FALSE ) || ((c == '.') &&
                                   (decimal_flag == FALSE)))
                                  {
                                    temp_string[index++] = c;
                                    decimal_flag = TRUE;
                                   }
                              else
                                 error_flag = TRUE;
             }  /* switch */
         }  /* while */
   temp_string[index] = '\0';
   strcpy(string,temp_string);
   return (error_flag);
}  /* get_string */

/***************************************************************************/
void get_date(year)
char year[];
{
long *t,tt;
char *p,*ctime();
register int j;

tt=0;
t=(&tt);
time(t);
p=ctime(t);

year[0] = p[20];
year[1] = p[21];
year[2] = p[22];
year[3] = p[23];
year[4] = '\0';
}  /* get_date */

/***************************************************************************/
void  min_max(easting,northing,category_length,head)
double        easting[];
double        northing[];
struct header *head;
int           category_length;
{
 int  index;

 for (index = FIRST; index < category_length; index++)
   {
    if (easting[index] <= head->west)
       head->west = easting[index];
    if (easting[index] >= head->east)
       head->east = easting[index];
    if (northing[index] <= head->south)
       head->south= northing[index];
    if (northing[index] >= head->north)
       head->north= northing[index];
   }
}  /* min_max */

/***************************************************************************/
int get_utm_zone()
{
 char   c;
 char   zone_string[MAX_INTEGER_LENGTH];
 int    utm_zone;
 int    index;
 int    flag;

 flag = TRUE;
 while (flag == TRUE)
   {
    flag = FALSE;
    index = FIRST;
    printf("\nUTM zone (? - help)     __\b\b");
    while ((c = getchar()) != '\n')
       {
        if (c == '?')
           system("utm");
        if (isdigit(c) != FALSE)
           zone_string[index++] = c;
        else
           flag = TRUE;
       }
    }
  zone_string[index] = '\0';
  utm_zone = atoi(zone_string);
  return(utm_zone);
}  /* get_utm_zone */

/***************************************************************************/
void setup(head,cat)
struct header      *head;
struct category    *cat;
{
 char   c;
 int    index;
 int    error_flag;
 char   date_string[MAX_DIGIT_DATE];
 char   string[STRING_MAX];

 head->north = (-999999999.99);
 head->south =  999999999.99;
 head->east  = (-999999999.99);
 head->west  =  999999999.99;

 strcpy(head->organization,"GIS Division, NPS");
 get_date(date_string);
 strcpy(head->digit_date,date_string);
 strcpy(head->map_date,date_string);

 printf("\nmap name                ________________________________________\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b");
 get_string(string,MAX_MAP_NAME,STRING);
 strcpy(head->map_name,string);

 do {
     printf("\nmap scale               ____________\b\b\b\b\b\b\b\b\b\b\b\b");
     error_flag = get_string(string,MAX_INTEGER_LENGTH,INTEGER);
     head->scale = atoi(string);
     } while (error_flag == TRUE);

 head->zone = get_utm_zone();
 strcpy(head->other_info," ");
 head->thresh = 0.0;

 cat->category_count = (-1);

 printf("\nsites name              ________________________________________\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b");
 get_string(string,STRING_MAX,STRING);
 fprintf(sitefp,"name|%-s\n",string);

 printf("\nsites description       ________________________________________\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b");
 get_string(string,STRING_MAX,STRING);
 fprintf(sitefp,"desc|%-s\n",string);

 printf("\ndigit name              ________________________________________\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b");
 get_string(string,STRING_MAX,STRING);
 strcpy(head->digit_name,string);

}  /* set_up */

/***************************************************************************/
void find_in_point (easting,northing,easting_in,northing_in,category_length)
double   easting[];
double   northing[];
double   *easting_in;
double   *northing_in;
int      category_length;
{
 int    index=0;
 int    found;
 double slope;
 double xm;
 double ym;
 double b;

 do {
     if (abs(easting[index] - easting[index+1]) > 1.0)
        {
        if (northing[index] == northing[index+1])
           slope = 0.0;
	else
           {
	    slope = (northing[index] - northing[index+1])/
                    (easting[index]  - easting[index+1] );

            xm = (abs(northing[index] - northing[index+1]))/2 +
                  min(northing[index],northing[index+1]);

            ym = (abs(easting[index] - easting[index+1]))/2 +
                 min(easting[index],easting[index+1]);

            b = ym - slope*xm;

            *easting_in = xm + 5;

            *northing_in = (*easting_in*slope)+b;
            found = TRUE;
           }
        }
     else
        {
          index++;
          found=FALSE;
        }
   } while (found == FALSE);
}  /* find_in_point  */

/***************************************************************************/
void inatt(easting, northing,type, category)
double  easting;
double  northing;
char    type;
int     category;
{
 fprintf(attfp,"%c  %11.2lf  %11.2lf  %7d\n",type,easting,northing,category);
}  /* inatt  */

/***************************************************************************/
void find_on_point(easting,northing,easting_on,northing_on,category_length)
double  easting[];
double  northing[];
int     category_length;
double  *easting_on;
double  *northing_on;

{
 int middle;

 middle = (FIRST + category_length)/2;
 *easting_on = easting[middle];
 *northing_on = northing[middle];
}  /* find_on_point */

/***************************************************************************/
void indig (easting,northing,category_length,type)
double  easting[];
double  northing[];
int     category_length;
char    type;
{
 int   index;

 fprintf(digfp,"%c%4d\n",type,category_length);
 for (index = FIRST; index <category_length; index++)
    fprintf(digfp,"%13.2lf%13.2lf\n",northing[index],easting[index]);
}  /* indig */

/***************************************************************************/
void insite (easting,northing,name,site_number)
double  easting;
double  northing;
char    name[];
int     *site_number;
{
 fprintf(sitefp,"%8d|%8d|%-3d%-s\n",(int)easting,(int)northing,
                                     *site_number++,name);
}  /* insite  */

/***************************************************************************/
int incat(category_name,cat)
char            category_name[];
struct category *cat;
{
 int index=0;
 int position = (-1);

 do {
     if (strcmp(category_name,cat->categories[index]) == 0)
        position = index;
     index++;
    } while ((index <= cat->category_count) && (position == (-1)));
 printf("*** position: %d\n",position);
 if (position == (-1))
   {
    cat ->category_count++;
    position = cat->category_count;
    strcpy(cat->categories[position],category_name);
   }
  return( position );
}  /* incat  */

/***************************************************************************/
void open_files()
{
  char filename[MAX_FILE_NAME];
  char sites_filename[MAX_FILE_NAME];
  char attribute_filename[MAX_FILE_NAME];
  char categories_filename[MAX_FILE_NAME];
  FILE *fopen();

  printf("\ninput filename (q-quit) _______________\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b");
  get_string(filename,MAX_FILE_NAME,STRING);

  while (((infp = fopen(filename,"r+")) == NULL) &&
          (strcmp(filename,"q") != 0))
     {
      printf("unable to read %s\n",filename);
      printf("\ninput filename (q-quit) _______________\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b");
      get_string(filename,MAX_FILE_NAME,STRING);
     }
  if (strcmp(filename,"q") == 0)
     exit(1);
  else
    {
      strcpy(digit_filename,     filename);
      strcat(digit_filename,     ".dig");
      strcpy(sites_filename,     filename);
      strcat(sites_filename,     ".sit");
      strcpy(attribute_filename, filename);
      strcat(attribute_filename, ".att");
      strcpy(categories_filename,filename);
      strcat(categories_filename,".cat");
     }

  digfp     = fopen(digit_filename,     "w+");
  sitefp    = fopen(sites_filename,     "w+");
  attfp     = fopen(attribute_filename, "w+");
  catfp     = fopen(categories_filename,"w+");
  scratchfp = fopen("SCRATCH",          "w+");
}  /* open_files */

/***************************************************************************/
void write_categories(cat)
struct category *cat;
{
 int index;

 fprintf(catfp,"#%3d categories \n\n\n",cat->category_count);
 fprintf(catfp,"0.00 0.00 0.00 0.00\n");
 fprintf(catfp,"0:no data\n");

 for (index = FIRST; index < cat->category_count; index++)
   fprintf(catfp,"%d:%-s\n",index+1,cat->categories[index]);
}  /* write_categories */

/***************************************************************************/
void write_header(head)
struct header *head;
{
 fprintf(scratchfp,"%-14s%-s\n",      "ORGANIZATION:",  head->organization);
 fprintf(scratchfp,"%-14s%-s\n",      "DIGIT DATE:",    head->digit_date);
 fprintf(scratchfp,"%-14s%-s\n",      "DIGIT NAME:",    head->digit_name);
 fprintf(scratchfp,"%-14s%-s\n",      "MAP NAME:",      head->map_name);
 fprintf(scratchfp,"%-14s%-s\n",      "MAP DATE:",      head->map_date);
 fprintf(scratchfp,"%-14s%-d\n",      "MAP SCALE:",     head->scale);
 fprintf(scratchfp,"%-14s%-s\n",      "OTHER INFO:",    head->other_info);
 fprintf(scratchfp,"%-14s%-d\n",      "ZONE:",          head->zone);
 fprintf(scratchfp,"%-14s%11.2lf\n",  "WEST EDGE:",     head->west);
 fprintf(scratchfp,"%-14s%11.2lf\n",  "EAST EDGE:",     head->east);
 fprintf(scratchfp,"%-14s%11.2lf\n",  "SOUTH EDGE:",    head->south);
 fprintf(scratchfp,"%-14s%11.2lf\n",  "NORTH EDGE:",    head->north);
 fprintf(scratchfp,"%-14s%\n",        "VERTI:");
}  /* write_header */

/***************************************************************************/
void close_files()
{
 fclose(scratchfp);
 fclose(digfp);
 fclose(sitefp);
 fclose(attfp);
 fclose(catfp);
 }  /* close_files */

/***************************************************************************/
void process_points(easting,northing,first_index,last_index,category_name,
                   category_length,site,cat,head)
double   easting[];
double   northing[];
int      first_index;
int      last_index;
int      *site;
char     category_name[];
int      category_length;
struct category   *cat;
struct header     *head;
{
 double            easting_in;
 double            northing_in;
 double            easting_on;
 double            northing_on;
 int               category_number;
 int               index;
 int               length;

   length = last_index - first_index;

	 /* data is an area */

    if ((easting[first_index] == easting[last_index-1]) &&
        (northing[first_index] == northing[last_index-1]) &&
        (last_index-1 != first_index))
       {
        category_number = incat(category_name,cat);
        find_in_point(easting,northing,&easting_in,&northing_in,length+1);
        indig(easting,northing,length,'A');
        inatt(easting_in,northing_in,'A',category_number+1);
       }

           /* data is a line */

    if (((easting[first_index] != easting[last_index-1]) ||
         (northing[first_index] != northing[last_index-1])) &&
         (last_index-1 != first_index))
        {
         category_number = incat(category_name,cat);
         find_on_point(easting,northing,&easting_on,&northing_on,length+1);
         inatt(easting_on,northing_on,'L',category_number+1);
         indig(easting,northing,length,'L');
        }

         /* data is a point */

    if (last_index-1 == first_index)
	insite(easting[first_index],northing[first_index],category_name,site);

   min_max(easting,northing,length,head);

}  /*  process_points  */

/***************************************************************************/
void process()
{
 char              category_name[STRING_MAX];
 int               category_length;
 double            easting[MAX_POINTS];
 double            northing[MAX_POINTS];
 double            temp_easting[MAX_POINTS];
 double            temp_northing[MAX_POINTS];
 int               last;
 int               last_index;
 int               first_index;
 int               index;
 int               site=1;
 struct category   cat;
 struct header     head;
 char              temp_string[STRING_MAX];

 setup(&head,&cat);

 while ((fscanf(infp,"%s%d",category_name,&category_length) != EOF))
   {
    for (last = FIRST; last <category_length;last++)
       fscanf(infp,"%lf%lf",&temp_easting[last],&temp_northing[last]);

    index = 0;
    first_index = FIRST;

    while ((temp_easting[index] >= 0.0) && (index < last))
      {
       easting[index] = temp_easting[index];
       northing[index] = temp_northing[index];
       index++;
      }
     last_index = index;

    process_points(easting,northing,first_index,last_index,category_name,
                   category_length,&site,&cat,&head);

        first_index = FIRST;
        last_index = last - last_index;

    if (index < last)   /* hole  */
       {
        easting[first_index] = (-temp_easting[first_index+last_index]);
        northing[first_index] = temp_northing[first_index+last_index];
        for (index = first_index+1 ; index < last-last_index;index++)
          {
           easting[index] = temp_easting[index+last_index];
           northing[index] = temp_northing[index+last_index];
          }

        last_index = index;
        process_points(easting,northing,first_index,last_index,category_name,
                       category_length,&site,&cat,&head);
       }

   }  /*  while  */

   write_categories(&cat);
   write_header(&head);
   close_files();
   system("cat SCRATCH > SCRATCH2");
   strcpy(temp_string,"cat ");
   strcat(temp_string,digit_filename);
   strcat(temp_string," >> SCRATCH2");
   system(temp_string);
   strcpy(temp_string,"cat  SCRATCH2 >");
   strcat(temp_string,digit_filename);
   system(temp_string);
   system("rm SCRATCH SCRATCH2 ");
}   /* process */

/***************************************************************************/
main()
{
   open_files();
   process();
} /* main */
