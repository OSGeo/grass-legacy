#include "geo.h" 

int init_table() 

{ 
 int i,j;

 for (j=0; j < NOPTIONS; j++) {
   for (i=0; i < NPROJES; i++) {

      TABLE[i][j].ask = 0;
      TABLE[i][j].def_exists = 0;
   }
   USED_in[j].was = 0;
   USED_out[j].was = 0;
 }

 USED_out[KFACT].was = 1;
 USED_out[KFACT].val = 0.;
 
 USED_in[KFACT].was = 1;
 USED_in[KFACT].val = 0.;

 USED_out[ZONE].val = 0.;
 USED_in[ZONE].val = 0.;

 TABLE[UTM][ZONE].ask = 1;
 TABLE[UTM][SOUTH].ask = 1;
 
 TABLE[AEA][LAT0].ask = 1;
 TABLE[AEA][LAT0].def_exists = 1;
 TABLE[AEA][LAT0].deflt = 23.0;

 TABLE[AEA][LON0].ask = 1;
 TABLE[AEA][LON0].def_exists = 1;
 TABLE[AEA][LON0].deflt = -96.0;

 TABLE[AEA][LAT1].ask = 1;
 TABLE[AEA][LAT1].def_exists = 1;
 TABLE[AEA][LAT1].deflt = 29.5;

 TABLE[AEA][LAT2].ask = 1;
 TABLE[AEA][LAT2].def_exists = 1;
 TABLE[AEA][LAT2].deflt = 45.5;

 TABLE[LCC][LAT0].ask = 1;
 TABLE[LCC][LAT0].def_exists = 1;
 TABLE[LCC][LAT0].deflt = 23.0;

 TABLE[LCC][LON0].ask = 1;
 TABLE[LCC][LON0].def_exists = 1;
 TABLE[LCC][LON0].deflt = -96.0;

 TABLE[LCC][LAT1].ask = 1;
 TABLE[LCC][LAT1].def_exists = 1;
 TABLE[LCC][LAT1].deflt = 33.0;

 TABLE[LCC][LAT2].ask = 1;
 TABLE[LCC][LAT2].def_exists = 1;
 TABLE[LCC][LAT2].deflt = 45.0;

 TABLE[MERC][LON0].ask = 1;
 TABLE[MERC][LON0].def_exists = 1;
 TABLE[MERC][LON0].deflt = -96.0;

 TABLE[MERC][LATTS].ask = 1;
 TABLE[MERC][LATTS].def_exists = 1;
 TABLE[MERC][LATTS].deflt = 0.;

 TABLE[TMERC][LAT0].ask = 1;
 TABLE[TMERC][LAT0].def_exists = 1;
 TABLE[TMERC][LAT0].deflt = 23.0;

 TABLE[TMERC][LON0].ask = 1;
 TABLE[TMERC][LON0].def_exists = 1;
 TABLE[TMERC][LON0].deflt = -96.0;

 TABLE[TMERC][KFACT].ask = 1;
 TABLE[TMERC][KFACT].deflt = 1.0;

 sprintf(DESC[LAT0],"Central Parallel\0");
 sprintf(DESC[LON0],"Central Meridian\0");
 sprintf(DESC[LAT1],"First Standard Parallel\0");
 sprintf(DESC[LAT2],"Second Standard Parallel\0");
 sprintf(DESC[LATTS],"Latitude of True Scale\0");
 sprintf(DESC[ZONE],"Projection Zone\0");
 sprintf(DESC[SOUTH],"Southern Hemisphere\0");
 sprintf(DESC[KFACT],"Scale Factor at the Central Meridian\0");

 return 1;
}

int get_proj_index(str)
 char *str;
{
   if ((strncmp(str,"LL",2) == 0)    
     ||(strncmp(str,"ll",2) == 0))     return LL;
   if ((strncmp(str,"UTM",3) == 0)    
     ||(strncmp(str,"utm",3) == 0))     return UTM;
   if ((strncmp(str,"STP",3) == 0)   
     ||(strncmp(str,"stp",3) == 0))    return STP;
   if ((strncmp(str,"AEA",3) == 0)   
     ||(strncmp(str,"aea",3) == 0))    return AEA;
   if ((strncmp(str,"LCC",3) == 0)   
     ||(strncmp(str,"lcc",3) == 0))    return LCC;
   if ((strncmp(str,"MERC",4) == 0)   
     ||(strncmp(str,"merc",4) == 0))   return MERC;
   if ((strncmp(str,"TMERC",5) == 0)   
     ||(strncmp(str,"tmerc",5) == 0))   return TMERC;
} 



int init_unit_table()
{
  sprintf(UNITS[0].units,"meters");
  sprintf(UNITS[0].unit,"meter");
  UNITS[0].fact = 1.0;

  sprintf(UNITS[1].units,"feet");
  sprintf(UNITS[1].unit,"foot");
  UNITS[1].fact = 0.3048;

  sprintf(UNITS[3].units,"inches");
  sprintf(UNITS[3].unit,"inch");
  UNITS[3].fact = 2.540000e-02;

  sprintf(UNITS[2].units,"miles");
  sprintf(UNITS[2].unit,"mile");
  UNITS[2].fact = 1609.344;

  sprintf(UNITS[4].units,"centimeters");
  sprintf(UNITS[4].unit,"centimeter");
  UNITS[4].fact = 0.01;

  sprintf(UNITS[5].units,"nanometers");
  sprintf(UNITS[5].unit,"nanometer");
  UNITS[5].fact = 1.000000e-09;

  sprintf(UNITS[6].units,"microns");
  sprintf(UNITS[6].unit,"micron");
  UNITS[6].fact = 1.000000e-06;

  sprintf(UNITS[7].units,"angstroms");
  sprintf(UNITS[7].unit,"angstrom");
  UNITS[7].fact = 1.000000e-10;

  sprintf(UNITS[8].units,"decinanometers");
  sprintf(UNITS[8].unit,"decinanometer");
  UNITS[8].fact = 1.000000e-10;

  sprintf(UNITS[9].units,"yards");
  sprintf(UNITS[9].unit,"yard");
  UNITS[9].fact = 0.9144;

  sprintf(UNITS[10].units,"rods");
  sprintf(UNITS[10].unit,"rod");
  UNITS[10].fact = 5.0292;

  sprintf(UNITS[11].units,"lightyears");
  sprintf(UNITS[11].unit,"lightyear");
  UNITS[11].fact = 9.460530e+15; 

return 1;
}

/*


british			1200|3937 m/ft
nmile			1852m
arpentlin		191.835 ft
barleycorn		1|3 in
bolt			40 yd
bottommeasure		1|40 in
cable			720 ft
caliber			1-2 in
chain			66 ft
cordfoot		cord
cubit			18 in
ell			45 in
engineerschain		100 ft
engineerslink		100|100 ft
fathom			6 ft
fermi			1-15 m
finger			7|8 in
furlong			220 yd
geodeticfoot		british-ft
geographicalmile	1852 m
gunterschain		22 yd
hand			4 in
league			3 mi
line			1|12 in
link			66|100 ft
marineleague		3 nmile
mil			1-3 in
nauticalmile		nmile
pace			36 in
palm			3 in
parasang		3.5 mi
pica			1|6 in
point			1|72 in
quarter			9 in
rope			20 ft
skein			120 yd
span			9 in
spindle			14400 yd
surveyfoot		british-ft
surveyorschain		66 ft
surveyorslink		66|100 ft

*/
