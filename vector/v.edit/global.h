#ifndef _V_EDIT_
# define _V_EDIT_

# ifdef MAIN
#  define global
# else
#  define global extern
# endif

# include <stdio.h>
# include <stdlib.h>
# include <grass/gis.h>
# include <grass/Vect.h>
# include <grass/glocale.h>

enum mode {
    MODE_CREATE,
    MODE_ADD,
    MODE_DEL,
    MODE_MOVE,
    MODE_VERTEX,
    MODE_STRAIGHTEN,
    MODE_SPLIT,
    MODE_BREAK,
    MODE_MERGE,
    MODE_CONTOURS,
    MODE_SELECT,
    MODE_CATADD,
    MODE_CATDEL,
    MODE_COPY,
    MODE_SNAP,
    MODE_FLIP,
};

/* options */
global struct Option *map_opt, *in_opt, *maxdist_opt, *tool_opt,
  *coord_opt, *cat_opt, *move_opt, *bbox_opt, *fld_opt,
  *poly_opt, *type_opt, *id_opt, *where_opt;
global struct Flag *n_flg, *t_flg, *i_flg, *b_flg, *c_flg;

/* global variables */
global struct GModule *module;
global struct Map_info Map;
global char *mapset;
global enum mode action_mode;
global FILE *ascii;

/* args.c */
int parser(int argc, char*argv[]);

/* delete.c */
int do_del(struct Map_info *Map);

/* a2b.c */
int asc_to_bin(FILE *, struct Map_info *);
int read_head ( FILE * dascii, struct Map_info *Map );

/* move.c */
int do_move(struct Map_info *Map);

/* vertex.c */
int do_move_vertex(struct Map_info *Map);
int do_remove_vertex(struct Map_info *Map);
int do_break(struct Map_info *Map);
int do_split(struct Map_info *Map);

/* merge.c */
int do_merge(struct Map_info *Map);

/* snap.c */
int do_snap(struct Map_info *Map);

/* select.c */
int do_print_selected(struct Map_info *Map);
struct ilist* select_lines(struct Map_info *Map);
int sel_by_cat(struct Map_info *Map, struct cat_list* cl_orig, struct ilist *List);
int sel_by_coordinates(struct Map_info *Map, struct ilist *List);
int sel_by_bbox(struct Map_info *Map, struct ilist *List);
int sel_by_polygon(struct Map_info *Map, struct ilist *List);
int sel_by_id(struct Map_info *Map, struct ilist *List);
int sel_by_where(struct Map_info *Map, struct ilist *List);
int merge_lists (struct ilist* alist, struct ilist* blist);

/* max_distance.c */
double max_distance(double maxdistance);

/* cats.c */
int cats (struct Map_info *Map, int del);

/* copy.c */
int do_copy (struct Map_info *Map);

/* flip.c */
int do_flip (struct Map_info *Map);

#endif
