#define         SHORT           short
#define         TRUE    1
#define         FALSE   0
 
#define MELEMENT        struct Melement
MELEMENT {
    short       x, y;   /* grid coordinates */
    int         value;
    MELEMENT    *next, *prior;  /* next and prior element in row list */
    };

#define NEIGHBOR        struct neighbor
NEIGHBOR {
    double      distance;
    MELEMENT    *Mptr,          /* pointer to data in linked lists of input */
                **searchptr;    /* row search pointer that identified this
                                   neighbor */
    NEIGHBOR    *next;
    };

/* structure for search pointers which access a row list of MELEMENTs */
/* if latitude-longitude, ealive and walive prevent search collisions on a
   circular, doubly-linked list; else, list is linear (NULL terminated) and
   pointers to MELEMENT are set NULL to indicate end of search in a direction */
#define EW              struct ew
EW {
    MELEMENT    *east,  /* next eastward search in this row */
                *west,  /* next westward search in this row */
                *start; /* starting point of east and west search in this row */
    short       ealive, walive; /* used only for latitude-longitude,
                                   TRUE if search is active in this direction */
    EW          *next;
    };
 
#ifdef MAIN
 
struct Cell_head        window ;
CELL                    *cell, *mask;
double                  *rowlook, *collook,
			*lat_diff,	/* distances between latitudes */
                        ew2;

short                   ll;     /* TRUE if latitude-longitude projection */

/* function pointers for LL function substitutes */

int                     first_west (), first_west_LL ();
int                     (*init_row_search) ();  /* function pointer */
 
int                     completed_row (), completed_row_LL ();
int                     (*comp_row_search) ();  /* function pointer */

int                     find_neighbors (), find_neighbors_LL ();
int                     (*locate_neighbors) ();  /* function pointer */

int                     exhaust_search (), exhaust_search_LL ();
int                     (*exhaust_row) ();  /* function pointer */

double                  offset_distance (), offset_distance_LL ();
double                  (*check_offset) ();  /* function pointer */

#endif
