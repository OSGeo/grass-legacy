
/****************************************************************
  1 April 1993
  Program for registering USGS 7.5' Quad Maps

  Phillip L. Chaney     Center For Advanced Spatial Technologies
  Research Specialist   12 Ozark Hall
                        University of Arkansas
                        Fayetteville, Ark. 72701
                        Phone: (501) 575-6159
                        E-mail: phil@cast.uark.edu
  Special Thanks to Ravi Kiran & Bill Johnston
*****************************************************************/ 

     
#include <stdio.h>
#include <strings.h>

int count;     /* Increment Counter */

int test1;     /* Check for valid Latitude Degrees */
int test2;     /* Check for valid Latidude Minutes:Seconds */
int test3;     /* Check for valid Longitude Degrees */
int test4;     /* Check for valid Longitude Minutes:Seconds */

int latd;      /* Degrees of Latitude */
int lond;      /* Degrees of Longitude */ 
int latitude;  /* Accept or Reject Latitude D:M:S */
int longitude; /* Accept or Reject Longitude D:M:S */

int opt1;      /* Switch Option for Latitude Minutes:Seconds */
int opt2;      /* Switch Option for Longitude Minutes:Seconds */
int opt3;      /* Switch option for Datum Selection */

int zone;      /* UTM Zone # */
int zonetest;  /* Check for valid UTM Zone */

int datumtest; /* Check for valid Datum Option */
int res;       /* Resolution of Geographic Region */

int qdbuf=200; /* Buffer Zone around Quad Map */

char fname[40];        /* Registration File Name */
char regname[80];      /* fname in the $LOCATION/reg directory */
char *mll2u ;          /* Grass Command "m.ll2u" */
char sphere[10];       /* Sphereoid/Ellipsoid Selection */

char regionname[40];   /* Geographic Region File Name */
char region[80];       /* Grass Command "g.region" to create region */
char mkregdir[80];     /* Unix command "mkdir $LOCATION/reg */
char regtmp[80];       /* Tmp file = $LOCATION/reg/reg.tmp */
char location[80];     /* Current GRASS LOCATION*/

struct DMS     /* Degrees:Minutes:Seconds */
{
 int deg;
 char ms[6];
};

struct DMS lat[8]; /* Latitude for 8 Registration Points */
struct DMS lon[8]; /* Longitude for 8 Registration Points */

struct UTM         /* UTM Coordinates in Easting and Northing */
{
 double e;
 double n;
};

struct UTM utm[4]; /* UTM Coordinates for 4 Exterior Corners of Quad */

struct UTM1        /* UTM Coordinates in Easting and Northing as integers */
{
 long e;
 long n;
};

struct UTM1 utm1[4]; /* UTM Coordinates for 4 Exterior Corners of Quad */

long x,y;          /* Geographic Region Variable Buffer Distances (0-200 M) */
long window[4];    /* Geographic Region Limits */

/*****************************************************/
/*****************************************************/

void getlatdeg()           /* Enter Degrees of Latitude */
{ char buffer[10];
  latd = 91;

  printf("\nEnter Degrees of Latitude (0-90): ");
  gets(buffer);
  sscanf(buffer,"%d",&latd);

    if(latd >= 0 && latd <= 90)
      test1= 1;
    else
      {
      printf("\nError!\n");
      test1= 0;
      }
}

/*****************************************************/

void getlondeg()          /* Enter Degrees of Longitude */
{ char buffer[10];
  lond = 181;

  printf("\nEnter Degrees of Longitude (0-180): ");
  gets(buffer);
  sscanf(buffer,"%d",&lond);
    if(lond >= 0 && lond <= 180)
      test3= 1;
    else
      {
      printf("\nError!\n");
      test3= 0;
      }
}

/*****************************************************/

void getlatms()    /* Enter Latitude Minute:Second Option */
{ char buffer[10];
  opt1 = 0;

  printf("\n\nSelect Option (1-8): ");
  gets(buffer);
  sscanf(buffer,"%d",&opt1);

  switch(opt1)
  {  case  1  : strcpy(lat[0].ms, "00:00");
		strcpy(lat[2].ms, "07:30");
		strcpy(lat[4].ms, "02:30");
		strcpy(lat[6].ms, "05:00");
		test2=1;
		break;

     case  2  : strcpy(lat[0].ms, "07:30");
		strcpy(lat[2].ms, "15:00");
		strcpy(lat[4].ms, "10:00");
		strcpy(lat[6].ms, "12:30");
		test2=1;
		break;

     case  3  : strcpy(lat[0].ms, "15:00");
		strcpy(lat[2].ms, "22:30");
		strcpy(lat[4].ms, "17:30");
		strcpy(lat[6].ms, "20:00");
		test2=1;
		break;

     case  4  : strcpy(lat[0].ms, "22:30");
		strcpy(lat[2].ms, "30:00");
		strcpy(lat[4].ms, "25:00");
		strcpy(lat[6].ms, "27:30");
		test2=1;
		break;

     case  5  : strcpy(lat[0].ms, "30:00");
		strcpy(lat[2].ms, "37:30");
		strcpy(lat[4].ms, "32:30");
		strcpy(lat[6].ms, "35:00");
		test2=1;
		break;

     case  6  : strcpy(lat[0].ms, "37:30");
		strcpy(lat[2].ms, "45:00");
		strcpy(lat[4].ms, "40:00");
		strcpy(lat[6].ms, "42:30");
		test2=1;
		break;

     case  7  : strcpy(lat[0].ms, "45:00");
		strcpy(lat[2].ms, "52:30");
		strcpy(lat[4].ms, "47:30");
		strcpy(lat[6].ms, "50:00");
		test2=1;
		break;

     case  8  : strcpy(lat[0].ms, "52:30");
		strcpy(lat[2].ms, "00:00");
		strcpy(lat[4].ms, "55:00");
		strcpy(lat[6].ms, "57:30");
		test2=1;
		break;

     default  : printf("\nError!");
		test2=0;
   }
}

/*****************************************************/

void getlonms()     /* Enter Longitude Minute:Second Option */
{ char buffer[10];
  opt2 = 0;

  printf("\n\nSelect Option (1-8): ");
  gets(buffer);
  sscanf(buffer,"%d",&opt2);

  switch(opt2)
  {  case  1  : strcpy(lon[0].ms, "00:00");
		strcpy(lon[1].ms, "52:30");
		strcpy(lon[4].ms, "57:30");
		strcpy(lon[5].ms, "55:00");
		test4=1;
		break;

     case  2  : strcpy(lon[0].ms, "07:30");
		strcpy(lon[1].ms, "00:00");
		strcpy(lon[4].ms, "05:00");
		strcpy(lon[5].ms, "02:30");
		test4=1;
		break;

     case  3  : strcpy(lon[0].ms, "15:00");
		strcpy(lon[1].ms, "07:30");
		strcpy(lon[4].ms, "12:30");
		strcpy(lon[5].ms, "10:00");
		test4=1;
		break;

     case  4  : strcpy(lon[0].ms, "22:30");
		strcpy(lon[1].ms, "15:00");
		strcpy(lon[4].ms, "20:00");
		strcpy(lon[5].ms, "17:30");
		test4=1;
		break;

     case  5  : strcpy(lon[0].ms, "30:00");
		strcpy(lon[1].ms, "22:30");
		strcpy(lon[4].ms, "27:30");
		strcpy(lon[5].ms, "25:00");
		test4=1;
		break;

     case  6  : strcpy(lon[0].ms, "37:30");
		strcpy(lon[1].ms, "30:00");
		strcpy(lon[4].ms, "35:00");
		strcpy(lon[5].ms, "32:30");
		test4=1;
		break;

     case  7  : strcpy(lon[0].ms, "45:00");
		strcpy(lon[1].ms, "37:30");
		strcpy(lon[4].ms, "42:30");
		strcpy(lon[5].ms, "40:00");
		test4=1;
		break;

     case  8  : strcpy(lon[0].ms, "52:30");
		strcpy(lon[1].ms, "45:00");
		strcpy(lon[4].ms, "50:00");
		strcpy(lon[5].ms, "47:30");
		test4=1;
		break;

     default  : printf("\nError!");
		test4=0;
   }
}

/*****************************************************/

void menu()   /* Minute:Second Options Menu */
{
 printf("\n+--------------------------+");
 printf("\n|  Minute:Second  Options  |");
 printf("\n+--------------------------+");
 printf("\n|  1. 00:00      5. 30:00  |");
 printf("\n|  2. 07:30      6. 37:30  |");
 printf("\n|  3. 15:00      7. 45:00  |");
 printf("\n|  4. 22:30      8. 52:30  |");
 printf("\n+--------------------------+");
} 

/*****************************************************/

void getlocation()    /*Get Current GRASS LOCATION*/
{
 FILE *fd;

   fd=popen("echo $LOCATION","rw");
   fscanf(fd,"%s",location);
   pclose(fd);
}

/*****************************************************/

void registername()  /*Enter Registration File Name */
{char buffer[40];

 FILE *fp;
 
 printf("\nEnter file name: ");
 gets(buffer);
 sscanf(buffer,"%s",fname);

 getlocation();

 sprintf(regname,"%s/reg/%s",location,fname);
 sprintf(regtmp,"%s/reg/reg.tmp",location);

 if((fp=fopen(regtmp, "w")) == NULL)
 {
   printf("\n\nError! Can't open file \"%s\" !\n",regtmp);
   exit();
 }
 else
 {

  for(count=0; count < 8; count++)
  fprintf(fp,"%d:%sW %d:%sN\n", lon[count].deg, lon[count].ms,
	                        lat[count].deg, lat[count].ms);
  fclose(fp);
 }
}

/*****************************************************/

void utmzone()     /* Enter UTM Zone */
{char buffer[10];
 zone = 100;

 printf("\nEnter UTM Zone (1-30): ");
 gets(buffer);
 sscanf(buffer,"%d",&zone);

 if(zone >= 1 && zone <= 30)
    zonetest=1;
 else
   {
    printf("\nError!\n");
    zonetest=0;
   }
}

/*****************************************************/

void datumopt()
{
 printf("\n\n   Datum      Ellipsoid\n");
 printf("\n1. NAD27       clark66");
 printf("\n2. NAD83        grs80");
}

/*****************************************************/


void datum()
{
 char buffer[10];
 opt3 = 0;

 printf("\n\nSelect option (1-2) [1] ");
 gets(buffer);
 sscanf(buffer,"%d",&opt3);

  switch(opt3)
  {  case  0  :     sprintf(sphere,"clark66");
                    datumtest = 1;
                    printf("\nspheroid = %s\n",sphere);
                    break;

     case  1  :     sprintf(sphere,"clark66");
                    datumtest = 1;
                    printf("\nspheroid = %s\n",sphere);
                    break;

     case  2  :     sprintf(sphere,"grs80");
                    datumtest = 1;
                    printf("\nspheroid = %s\n",sphere);
                    break;

     default  :     printf("\nError!");
		    datumtest=0;
 }
}

/*****************************************************/

void registration()      /* Create Registration File for Digitizing */
{char buffer[10];

 printf("\n\n\nDo you wish to create a registration point file? (y/n) [y] ");
 gets(buffer);

 if(buffer[0] == 'y' || buffer[0] == NULL)
   {
	char temp[100];

    datumopt();
    do datum();
    while((datumtest) !=1);

    do utmzone();
    while((zonetest) !=1);
 
    registername();


    printf("\n\nm.ll2u -z spheroid=%s zone=%d input=reg.tmp output=%s",sphere,zone,fname);
    sprintf(temp,"m.ll2u -z spheroid=%s zone=%d input=%s output=%s",sphere,zone,regtmp,regname);

	mll2u = ( char * ) malloc ( sizeof ( char ) * ( strlen( temp ) + 1) ) ;  
	strcpy ( mll2u, temp); 
    system(mll2u);


    printf("\n\n\n[%s] is now ready for registration !",fname);
   }
 else
   {
    printf("\nEXIT !\n\n");
    exit();
   }
}

/*****************************************************/

void copyms()  /* Copy Latitude and Longitude M:S to Correct Positions */
{  strcpy(lat[1].ms, lat[0].ms);
   strcpy(lat[3].ms, lat[2].ms);
   strcpy(lat[5].ms, lat[4].ms);
   strcpy(lat[7].ms, lat[6].ms);
   strcpy(lon[2].ms, lon[0].ms);
   strcpy(lon[3].ms, lon[1].ms);
   strcpy(lon[6].ms, lon[4].ms);
   strcpy(lon[7].ms, lon[5].ms);
} 

/*****************************************************/

void quadmap()  /* Display USGS 7.5' Quad Map */
{
 printf("\n\n\n                 USGS 7.5' Quad Map");
 printf("\n\n  %2d:%s   3 +--------------------+ 4  ",lat[2].deg,lat[2].ms);
 printf("     Registration Points");
 printf("\n               |                    |");
 printf("         Longitude  Latitude");
 printf("\n               |                    |");
 printf("           (West)   (North) ");
 printf("\n               |                    |");
 printf("\n               |                    |  ");
 printf("    1. %3d:%s  %2d:%s",lon[0].deg,lon[0].ms,lat[0].deg,lat[0].ms);
 printf("\n     %s     |     7+      +8     |  ",lat[6].ms); 
 printf("    2. %3d:%s  %2d:%s",lon[1].deg,lon[1].ms,lat[1].deg,lat[1].ms);
 printf("\n               |                    |  ");
 printf("    3. %3d:%s  %2d:%s",lon[2].deg,lon[2].ms,lat[2].deg,lat[2].ms);
 printf("\n               |                    |  ");
 printf("    4. %3d:%s  %2d:%s",lon[3].deg,lon[3].ms,lat[3].deg,lat[3].ms);
 printf("\n               |                    |");
 printf("\n               |                    |  ");
 printf("    5. %3d:%s  %2d:%s",lon[4].deg,lon[4].ms,lat[4].deg,lat[4].ms);
 printf("\n     %s     |     5+      +6     |  ",lat[4].ms);
 printf("    6. %3d:%s  %2d:%s",lon[5].deg,lon[5].ms,lat[5].deg,lat[5].ms);
 printf("\n               |                    |  ");
 printf("    7. %3d:%s  %2d:%s",lon[6].deg,lon[6].ms,lat[6].deg,lat[6].ms);
 printf("\n               |                    |  ");
 printf("    8. %3d:%s  %2d:%s",lon[7].deg,lon[7].ms,lat[7].deg,lat[7].ms);
 printf("\n               |                    |");
 printf("\n               |                    |");
 printf("\n  %2d:%s   1 +--------------------+ 2",lat[0].deg,lat[0].ms);
 printf("\n\n       %3d:%s    %s  %s    %3d:%s",lon[0].deg,lon[0].ms,lon[4].ms,lon[5].ms,lon[1].deg,lon[1].ms);
}

/*****************************************************/

void latchk()             /* Accept or Reject Latitude */
{char buffer[10];

  do getlatdeg();
  while((test1) !=1);

  menu();

  do getlatms();
  while((test2) !=1);

  printf("\nLatitude of Lower Left Quad Corner: %d:%s", latd, lat[0].ms);

  printf("\n\nDo you accept Latitude? (y/n) [y] ");
  gets(buffer);

    if(buffer[0] == 'y' || buffer[0] == NULL)  
      latitude = 1;
    else
     { 
      latitude = 0;
     }
}

/*****************************************************/

void lonchk()    /* Accept or Reject Longitude */
{char buffer[10];

  do getlondeg();
  while((test3) !=1);

  menu();

  do getlonms();
  while((test4) !=1);

  printf("\nLongitude of Lower Left Quad Corner: %d:%s", lond, lon[0].ms);

  printf("\n\nDo you accept Longitude? (y/n) [y] ");
  gets(buffer);

    if(buffer[0] == 'y' || buffer[0] == NULL)  
      longitude = 1;
    else
      longitude = 0;
}

/*****************************************************/

void getutm()  /* Retrieve UTM Coordinates */
{
 FILE *fp; 

 if((fp=fopen(regname, "r")) == NULL)
   {
    printf("Error, Can't open file:\"%s\" !",regname);
    exit();
   }

 for(count=0;count<4;count++)
   fscanf(fp, "%F %F",&(utm[count].e),&(utm[count].n));

 for(count=0;count<4;count++)
  {
    utm1[count].e=utm[count].e;
    utm1[count].n=utm[count].n;
  }

 fclose(fp);
}

/*****************************************************/

void calcwindow()        /* Calculate Buffer Around Quad Map */
{ 
  if(utm1[0].e >= 500000)
     
     {
      x = utm1[2].e % qdbuf;
      if(x <= 100)
         x = x + 100;
      window[0] = utm1[2].e - x;

      x = utm1[1].e % qdbuf;
      y = qdbuf - x;   
      if(y <= 100)
         y = y + 100;
      window[1] = utm1[1].e + y;

      x = utm1[0].n % qdbuf;
      if(x <= 100)
         x = x + 100;
      window[2] = utm1[0].n - x;

      x = utm1[3].n % qdbuf;
      y = qdbuf - x;
      if(y <= 100)
         y = y + 100;
      window[3] = utm1[3].n + y;

     }
 
   else   

     {
      x = utm1[0].e % qdbuf;
      if(x <= 100)
         x = x + 100;
      window[0] = utm1[0].e - x;

      x = utm1[3].e % qdbuf;
      y = qdbuf - x;   
      if(y <= 100)
         y = y + 100;
      window[1] = utm1[3].e + y;

      x = utm1[1].n % qdbuf;
      if(x <= 100)
         x = x + 100;
      window[2] = utm1[1].n - x;

      x = utm1[2].n % qdbuf;
      y = qdbuf - x;
      if(y <= 100)
         y = y + 100;
      window[3] = utm1[2].n + y;
     }
}

/*****************************************************/

void regionmap()
{

 printf("\n**************************** UTM Coordinates ***************************");
 printf("\n\n         Geographic Region                    Registration Points");
 printf("\n\n               North                          East          North"); 
 printf("\n              %D",window[3]); 
 printf("\n         +---------------+");
 printf("\n         |3             4|             1. %f %f",utm[0].e,utm[0].n);
 printf("\n         |               |             2. %f %f",utm[1].e,utm[1].n);
 printf("\n  West   |               |  East");
 printf("\n %D  |               | %D",window[0],window[1]);
 printf("\n         |               |             3. %f %f",utm[2].e,utm[2].n);
 printf("\n         |1             2|             4. %f %f",utm[3].e,utm[3].n);
 printf("\n         +---------------+");
 printf("\n               South"); 
 printf("\n              %D\n",window[2]); 

 printf("\ng.region n=%D s=%D e=%D w=%D res=%d save=%s",
                window[3],window[2],window[1],window[0],res,regionname);

 sprintf(region,"g.region n=%D s=%D e=%D w=%D res=%d save=%s",
                window[3],window[2],window[1],window[0],res,regionname);
 system(region);

 printf("\n\n************************************************************************\n");
}

/*****************************************************/

void resolution()  /* Enter Grid Resolution for Geographic Region File */
{char buffer[10];

  printf("\nEnter grid resolution in meters: [30] ");
  gets(buffer);
  sscanf(buffer,"%d",&res);

  if(buffer[0] == NULL)  
     res = 30;

}

/*****************************************************/

void getregionname() /* Enter Geographic Region File Name */
{char buffer[40];
 
 printf("\nEnter USGS 7.5' Quad name: [%s] ",fname);
 gets(buffer);

 if(buffer[0] == NULL)
   strcpy(regionname,fname);
 else
   sscanf(buffer,"%s",regionname);
}

/*****************************************************/

void getregion()         /* Create Geographic Region File */
{char buffer[10];

 printf("\n\nDo you wish to create a geographic region file? (y/n) [y] ");
 gets(buffer);

 if(buffer[0] == 'y' || buffer[0] == NULL)
   {
    getregionname();
    resolution();
    getutm();
    calcwindow();
    regionmap();
   }
 else
   {
    printf("\nEXIT !\n\n");
    exit();
   }
}

/***************************************************************************/
/***************************************************************************/

main()
{
  sprintf(mkregdir,"mkdir $LOCATION/reg");
  system(mkregdir);

  printf("\n\n");
  printf("*************************************************************");
  printf("\n* Enter data for \"Lower Left Corner\" of USGS 7.5' Quad Map! *\n");
  printf("*************************************************************\n");

  do lonchk();
  while((longitude) != 1);

  for(count=0;count < 8; count++)
    lon[count].deg = lond;

  if(opt2 == 1)
    {
    lon[1].deg = lond-1;
    lon[3].deg = lond-1;
    lon[4].deg = lond-1;
    lon[5].deg = lond-1;
    lon[6].deg = lond-1;
    lon[7].deg = lond-1;
    }

  do latchk();
  while((latitude) != 1);

  for(count=0;count < 8; count++)
      lat[count].deg = latd;

  if(opt1 == 8)
    {
    lat[2].deg = latd+1;
    lat[3].deg = latd+1;
    }

  copyms();
  quadmap();
  registration();
  getregion();
}


