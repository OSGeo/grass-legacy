#include "gis.h"
#include "display.h"
#include "Vect.h"
#include "infx.h"

char* getCat (Map,x,y,i)

  struct Map_info *Map;
  float x,y;
  int *i;
  

{
  
    static char buf[32]="";
    double east, north ;
    

    plus_t line, area;
    int dbCat;
    
	dbCat = -1;
        east  = (double) x ;
        north = (double) y;




	line = dig_point_to_line (Map, east, north, -1);
        area = dig_point_to_area (Map, east, north) ;


        if (line + area == 0) {
		sprintf(buf,"Nothing found.\n") ;
		dbCat=-1;
		*i=dbCat;
		return buf;
		
	}

        if ( (line > 0 ) && (area == 0)) { 
	
	  if(dbCat = V2_line_att(Map,line)){		
	 		
		snprintf(buf,32,"Line category:\n");		
	  }else 
	  snprintf(buf,32,"Line category not found\n");	
	} else if (area > 0) {
       
	 	if(dbCat= V2_area_att(Map, area)) {
	
			snprintf(buf,32,"Area category:\n");	
	  	}else{
	  		snprintf(buf,32,"Area category not found\n");
			if (line > 0) {
				if(dbCat = V2_line_att(Map,line)){
					snprintf(buf,32,"Line category:\n");
				} else 
	  				snprintf(buf,32,"Line category not found\n");
			}
		}
	} 
	
	*i=dbCat;
return buf;
}

int fillSQLstruct(tp,x,y,dist)
  struct Sql *tp;
  float x,y;
  int dist;
{
	double east, north ;
	int ret=0;


	east = (double) x;
	north = (double) y;
	tp->centX = east;
	tp->centY = north;
	tp->distance = (double) dist;
	tp->permX = east + tp->distance;
	tp->permY = north + tp->distance;
	tp->maxX = east + tp->distance;
	tp->maxY = north + tp->distance;
	tp->minX = east - tp->distance;
	tp->minY = north - tp->distance;
	

	tp->rad2 = ((tp->permX - tp->centX) * (tp->permX - tp->centX) +
       			(tp->permY - tp->centY) * (tp->permY - tp->centY) );


	return(ret) ;

}
