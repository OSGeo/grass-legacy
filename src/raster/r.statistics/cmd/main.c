#include <string.h>
#include "gis.h"

#define MAIN
#include "method.h"

#define MDEBUG(a) fprintf(stderr,"%s\n",a);


int 
is_ok (char *method, char *map)
{
   if(map == NULL)
   {
     fprintf(stderr,"Sorry, with method '%s' you have to define an outputmap.\n",
                     method);
     exit(1);
    }

    return 0;
}



int 
main (int argc, char **argv)
{
 char *mapset,*me;
 int usecats;
 int o_method;
 struct GModule *module;
 struct Option *method, *basemap, *covermap, *outputmap;
 struct Flag *flag_c;
 struct Categories cats;
         
  
    G_gisinit(me=argv[0]);

    module = G_define_module();
    module->description =
		"Category or object oriented statistics.";
					        
    basemap = G_define_option();
    basemap->key        = "base";
    basemap->type       = TYPE_STRING ;
    basemap->required   = YES ;
    basemap->gisprompt  = "old,cell,raster" ;
    basemap->description = "base raster map";

    covermap = G_define_option();
    covermap->key       = "cover";
    covermap->type      = TYPE_STRING;
    covermap->required  = YES ;
    covermap->gisprompt  = "old,cell,raster" ;
    covermap->description = "cover raster map";

    method = G_define_option();
    method->key          = "method";
    method->type         = TYPE_STRING;
    method->required     = YES;
    method->options      = G_malloc(1024);
    for (o_method = 0; menu[o_method].name; o_method++)
    {
       if (o_method)
	  strcat (method->options, ",");
       else
      	  *(method->options) = 0;
	  strcat (method->options, menu[o_method].name);
    }
    method->description  = "method of object-based statistic";
    
    outputmap = G_define_option();
    outputmap->key       = "output";
    outputmap->type      = TYPE_STRING;
    outputmap->required  = NO ;
    outputmap->gisprompt  = "new,cell,raster" ;
    outputmap->description = "resultant raster map (not used with 'distribution')";
    
    
    flag_c = G_define_flag();
    flag_c->key = 'c';
    flag_c->description = "cover values extracted from the category labels of the cover map";

    if (G_parser(argc,argv))
	exit(1);

    usecats = flag_c->answer;
    
    mapset = G_find_cell2 (covermap->answer, "");
    if (G_read_cats (covermap->answer, mapset, &cats) < 0)
    {
       fprintf (stderr, "%s: ERROR reading category file for %s\n",
        	me, covermap->answer);
       exit(1);
    }
    
    for (o_method = 0; menu[o_method].name; o_method++)
       if (strcmp(menu[o_method].name, method->answer) == 0)
          break;

    if (!menu[o_method].name)
    {
       fprintf (stderr, "<%s=%s> unknown %s\n",
          method->key, method->answer, method->key);
       G_usage();
       exit(1);
    }                                                                                                                                                      
 
 
   switch(menu[o_method].val){
        case DISTRIB:
             if(outputmap->answer != NULL)
               fprintf(stderr,"Outputmap '%s' ignored!\n", outputmap->answer);
	     
	     o_distrib(basemap->answer, covermap->answer, 
	                  outputmap->answer,usecats); /* ,&cats);  */
	     break;
	case AVERAGE:
	     is_ok(method->answer, outputmap->answer);
	     o_average(basemap->answer, covermap->answer, 
	                  outputmap->answer,usecats,&cats); 
	     break;
        case MODE:
             is_ok(method->answer, outputmap->answer);
	     o_mode(basemap->answer, covermap->answer, 
	                  outputmap->answer,usecats,&cats); 
	     break;
        case ADEV:
             is_ok(method->answer, outputmap->answer);
             o_adev(basemap->answer, covermap->answer, 
                    outputmap->answer,usecats,&cats);
             break;
        case SDEV:
             is_ok(method->answer, outputmap->answer);
             o_sdev(basemap->answer, covermap->answer,
                    outputmap->answer,usecats,&cats);
             break;
        case VARIANC:
             is_ok(method->answer, outputmap->answer);
             o_var(basemap->answer, covermap->answer,
                    outputmap->answer,usecats,&cats); 
             break;       
        case SKEWNES:
             is_ok(method->answer, outputmap->answer);
             o_skew(basemap->answer, covermap->answer,
                    outputmap->answer,usecats,&cats); 
             break;       
        case KURTOSI:
             is_ok(method->answer, outputmap->answer);
             o_kurt(basemap->answer, covermap->answer,
                    outputmap->answer,usecats,&cats); 
             break;       
        case MEDIAN:
             is_ok(method->answer, outputmap->answer);
             o_median(basemap->answer, covermap->answer,
                    outputmap->answer,usecats,&cats); 
             break;       
        case MIN:
             is_ok(method->answer, outputmap->answer);
             o_min(basemap->answer, covermap->answer,
                    outputmap->answer,usecats,&cats);
             break;       
        case MAX:
             is_ok(method->answer, outputmap->answer);
             o_max(basemap->answer, covermap->answer,
                    outputmap->answer,usecats,&cats); 
             break;
        case SUM:
             is_ok(method->answer, outputmap->answer);
             o_sum(basemap->answer, covermap->answer,
                    outputmap->answer,usecats,&cats); 
             break;	            
	default:
          printf("Not yet implemented!\n"); 
    }    		

    return 0;
}
