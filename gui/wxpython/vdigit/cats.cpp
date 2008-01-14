/**
   \file cats.cpp

   \brief Category management

   This program is free software under the GNU General Public
   License (>=v2). Read the file COPYING that comes with GRASS
   for details.

   (C) 2008 by The GRASS development team

   \author Martin Landa <landa.martin gmail.com>

   \date 2008 
*/

#include "driver.h"
#include "digit.h"

/**
   \brief Initialize cats structure.

   \return 0 on success
   \return -1 on error
*/
int Digit::InitCats()
{
    int nfields, field, index, ncats, max_cat;
    int cat, type, id; 

    struct lcat lc;

    G_debug(2, "vdigit.InitCats()");

    if (!cats.empty()) {	
	cats.clear();
    }

    if (!display->mapInfo) {
	return -1;
    }

    nfields = Vect_cidx_get_num_fields (display->mapInfo);
    
    for (int i = 0; i < nfields; i++ ) {
	field = Vect_cidx_get_field_number(display->mapInfo, i);
	ncats = Vect_cidx_get_num_cats_by_index(display->mapInfo, index);
	for (int j = 0; j < ncats; j++) {
	    Vect_cidx_get_cat_by_index (display->mapInfo, index, j, &cat, &type, &id);
	    if (cat > max_cat)
		max_cat = cat;
	}

	lc.layer = Vect_cidx_get_field_number(display->mapInfo, i);
	lc.cat = max_cat;

	cats.push_back(lc);
	G_debug(3, "vdigit.InitCats(): layer=%d, cat=%d", lc.layer, lc.cat);
    }

    return 0;
}

/**
   \brief Get max category number for layer

   \param layer layer number

   \return category number (1 if no category found)
   \return -1 on error
*/
int Digit::GetCategory(int layer)
{
    for(std::vector<struct lcat>::const_iterator i = cats.begin(), e = cats.end();
	i != e; ++i) {
	if (layer == (*i).layer) {
	    G_debug(3, "v.digit.GetCategory(): layer=%d, cat=%d", (*i).layer, (*i).cat);
	    return (*i).cat;
	}
    }

    return -1;
}

/**
   \brief Set max category number for layer

   \param layer layer number
   \param cats  category number to be set

   \return previosly set category
*/
int Digit::SetCategory(int layer, int cat)
{
    int old_cat;

    for(std::vector<struct lcat>::iterator i = cats.begin(), e = cats.end();
	i != e; ++i) {
	if (layer == (*i).layer) {
	    old_cat = (*i).cat;
	    (*i).cat = cat;
	    G_debug(3, "vdigit.SetCategory(): layer=%d, cat=%d", layer, cat);
	}
    }

    return old_cat;
}

/**
   \brief Copy categories from one vector feature to other

   \param cats  list of layer/category to be copied			       
   \param ids   list of line ids where to copy categories

   \return number of modified features
   \return -1 on error
*/
int Digit::CopyCats(std::vector<std::vector<int> > cats, std::vector<int> ids)
{
  /* TODO */

  return 0;
}
