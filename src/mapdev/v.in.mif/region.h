#ifndef REGION_SUPP_H_
#define REGION_SUPP_H_

struct pnt_bank_;

typedef struct pnt_bank_ pnt_bank;


struct pnt_bank_ {

  int n_pnt_d;
  int alloc_pnt_d;

  pntDescript *pnt_ds;
}


/* Prototypes */

int add_pnt_to_bank(double xpos, double ypos, )

#endif /* REGION_SUPP_H_  */
