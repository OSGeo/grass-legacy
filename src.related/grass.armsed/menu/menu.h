#include "gis.h"

#define step1 "Partition into left/right half planes"
#define step2 "Collect statistics for armsed"
#define step3 "Provide final armsed input"
#define step4 "Run armsed"

EXTERN struct Cell_head window;
EXTERN char proj_name[41], *proj_mapset;
EXTERN char elev_name[41], *elev_mapset;
EXTERN char extthin_name[41], *extthin_mapset;
EXTERN char basin_name[41], *basin_mapset;
EXTERN char part_name[31], *part_mapset;
EXTERN char soils_name[31], *soils_mapset;
EXTERN char cover_name[31], *cover_mapset;
EXTERN char sim_title[41], *sim_mapset;
EXTERN int complete[6];
