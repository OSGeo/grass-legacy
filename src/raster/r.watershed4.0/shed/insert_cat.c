#include "watershed.h"

insert_cat (cat, value, num_cells)
	CAT	*cat;
	CELL	value;
	int	num_cells;
{
  	CAT	*temp_cat, *new_cat();

    for (;;) {
	if (value > cat->cat_val) {
		if (cat->nxt == NULL) {
			cat->nxt = new_cat (value, num_cells);
			cat = cat->nxt;
			cat->nxt = NULL;
			return;
		} 
		else	cat = cat->nxt;
	} else if (value == cat->cat_val) {
		cat->num_cat = cat->num_cat + num_cells;
		return;
	} else {
		temp_cat = new_cat (cat->cat_val, cat->num_cat);
		temp_cat->nxt = cat->nxt;
		cat->cat_val = value;
		cat->num_cat = num_cells;
		cat->nxt = temp_cat;
		return;
	}
    }
}

CAT *
new_cat (value, num_cat)
	CELL	value;
	int	num_cat;
{
	CAT	*new;

	new = (CAT *) G_malloc (sizeof (CAT));
	new->num_cat = num_cat;
	new->cat_val = value;
	return (new);
}
