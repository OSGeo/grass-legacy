#ifndef PARTS_STRUCTS_H_
#define PARTS_STRUCTS_H_

struct _partDescript;
struct _pntDescript;
struct _pntRepos;

typedef struct _partDescript partDescript;
typedef struct _pntDescript pntDescript;
typedef struct _pntRepos pntRepos;


/* The description of a polygon part */

struct _partDescript {
  int numPoints;
  int allocPoints;
  pntDescript *linepnts;
};


/* Description of an actual point */

struct _pntDescript {
  int duff;
  int isnode;
  double xPosn;
  double yPosn;
  double zVal;     
  double mVal;
  pntDescript **linkverts;  /* Only initialise and use if required        */
  int linknum;		/* links outwards        */
  double *linkdirect;      /* directions of links (math format)          */
};


struct _pntRepos {

  int n_parts;
  int alloc_parts;
  partDescript *parts;
};

#endif    /* PARTS_STRUCTS_H_ */
