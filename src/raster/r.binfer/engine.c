/*
**                               
** Filename: engine.c
**                                 
**                                  
** Author: Kurt A. Buehler           
** Date: Fri Feb 10 08:41:42 EST 1989
** Version: 1.0                        
**                                      
*/

#include <stdlib.h>
#include <stdio.h>
#include "symtab.h"
#include "local_proto.h"

char errmsg[80];
extern int verbose;
extern int probabilitymaps;
extern int combinedmap;
extern int colortable;
struct symtab *index_to_att();
struct symtab *index_to_val();

int engine (struct symtab *ptr, char *result)
{
    struct prob *list, *get_product_list();
    int row,col;
    int nrows,ncols;
    double sum;

    set_context_flags(ptr);
    set_subjective_flags(ptr);
    open_files(ptr,result);

    nrows = G_window_rows();
    ncols = G_window_cols();

    fprintf(stderr,"percent complete: ");
    for ( row = 0; row < nrows; row++ ) {
        G_percent(row, nrows, 5);
        get_maprows(ptr,row);
        for ( col = 0; col <= ncols; col++ ) {
            set_layer_flags(ptr);
            list = get_product_list(ptr,&sum);
            store_final_prob(ptr,list,sum,col);
            free_product_list(list);
        }
        write_maprow(ptr);
    }
    G_percent(row, nrows, 5);
    close_files(ptr,result);
    fprintf(stderr,"Writing support.\n");
    write_support(ptr,result);

    return 0;
}

int 
open_files (struct symtab *ptr, char *result)
{
    struct symtab *local = ptr;
    while ( local != NULL ) {
        switch(local->s_type) {
        case HeadSymbol:
            break;
        case AttributeSymbol:
            switch(local->element.att->a_type) {
            case LayerAttribute:
                if ( (local->element.att->mapset = 
			 G_find_cell(local->reclassname, "")) == NULL)
		{
                    sprintf(errmsg,"Can't find cell file [%s]\n",local->reclassname);
                    fatal(errmsg);
                }
                if ((local->element.att->fildes = G_open_cell_old(local->reclassname,
                    local->element.att->mapset)) < 0 ) {
                    sprintf(errmsg,"Can't open cell file [%s in %s]\n",local->reclassname,
                        local->element.att->mapset);
                    fatal(errmsg);
                }
                local->element.att->cellbuf = G_allocate_cell_buf();
                local->element.att->cellptr = local->element.att->cellbuf;
                break;
            case InferredAttribute:
                if ( combinedmap ) {
                    if ((local->element.att->fildes = G_open_cell_new(result)) < 0 ) {
                        sprintf(errmsg,"Can't open new cell file [%s in %s]\n",local->name,
                            G_mapset());
                        fatal(errmsg);
                    }
                }
                local->element.att->cellbuf = G_allocate_cell_buf();
                local->element.att->cellptr = local->element.att->cellbuf;
                break;
            case ContextAttribute:
            case SubjectiveAttribute:
                break;
            }
            break;
        case ValueSymbol:
            if ( local->element.val->v_type == InferredAttribute ) {
                if ( probabilitymaps ) {
                    if ( (local->element.val->desc.infr->fildes = G_open_cell_new(local->name)) < 0 ) {
                        sprintf(errmsg,"Can't open new cell file [%s in %s]\n",local->name,
                            G_mapset());
                        fatal(errmsg);
                    }
                }
                local->element.val->desc.infr->cellbuf = G_allocate_cell_buf();
                local->element.val->desc.infr->cellptr = local->element.val->desc.infr->cellbuf;
            }
            break;
        }
        local = local->next;
    }

    return 0;
}


int 
get_maprows (struct symtab *ptr, int row)
{
    struct symtab *local = ptr;
    int init = 0;
    int retval;

    while((local = index_to_att(local,LayerAttribute,init)) != NULL ) {
        init = 1;
        if ((retval = G_get_map_row(local->element.att->fildes,
            local->element.att->cellbuf,row)) <= 0 ) {
            if ( retval < 0 ) {
                sprintf(errmsg,"Couldn't get row %d of [%s in %s]\n",row,local->reclassname,
                    local->element.att->mapset);
                fatal(errmsg);
            } else {
                sprintf(errmsg,"Row %d of [%s in %s] outside of window\n",row,local->reclassname,
                    local->element.att->mapset);
                fatal(errmsg);
            }
        }
        local->element.att->cellptr = local->element.att->cellbuf;
        local = local->next;
    }

    return 0;
}

struct prob *
get_product_list (struct symtab *ptr, double *sum)
{
    struct symtab *local = ptr;
    struct symtab *inferred_att;
    struct prob *list;
    struct prob *list_head;
    struct prob *tmp;
    struct prob *tmpl;
    int first = 1;
    int i;

    *sum = 0.0;
    inferred_att = index_to_att(local,InferredAttribute,0);
    list_head = list = (struct prob *)malloc(sizeof(struct prob));
    for ( i = 0; i < inferred_att->element.att->num_vals; i++ ) {
        list->probability = 1.0;
        list->next = (struct prob *)malloc(sizeof(struct prob));
        list = list->next;
    }
    list->next = (struct prob *)0;
    local = ptr;
    while ( local != NULL ) {
        switch(local->s_type) {
        case HeadSymbol:
            break;
        case AttributeSymbol:
            break;
        case ValueSymbol:
            switch(local->element.val->v_type) {
            case LayerAttribute:
                if ( local->element.val->desc.layer->truthflag ) {
                    tmpl = local->element.val->desc.layer->list->next;
                    for ( i = 0,tmp = list_head; i < inferred_att->element.att->num_vals;
                        i++, tmp = tmp->next) {
                        tmp->probability *= tmpl->probability;
                        tmpl = tmpl->next;
                    }
                }
                break;
            case ContextAttribute:
                if ( local->element.val->desc.cont->truthflag ) {
                    tmpl = local->element.val->desc.cont->list->next;
                    for ( i = 0,tmp = list_head; i < inferred_att->element.att->num_vals;
                        i++, tmp = tmp->next) {
                        tmp->probability *= tmpl->probability;
                        tmpl = tmpl->next;
                    }
                }
                break;
            case SubjectiveAttribute:
                if ( local->element.val->desc.subj->truthflag ) {
                    tmpl = local->element.val->desc.subj->list->next;
                    for ( i = 0,tmp = list_head; i < inferred_att->element.att->num_vals;
                        i++, tmp = tmp->next) {
                        tmp->probability *= tmpl->probability;
                        tmpl = tmpl->next;
                    }
                }
                break;
            case InferredAttribute:
                if ( first ) {
                    tmp = list_head;
                    first = 0;
                }
                tmp->probability *= local->element.val->desc.infr->prior_prob;
                tmp = tmp->next;
                break;
            }
            break;
        }
        local = local->next;
    }
    for ( i = 0,tmp = list_head; i < inferred_att->element.att->num_vals;
        i++, tmp = tmp->next) {
        *sum += tmp->probability;
    }
    return(list_head);
}


int 
free_product_list (struct prob *ptr)
{
    struct prob *local = ptr;

    while (local != NULL) {
        local = local->next;
        free(ptr);
        ptr = local;
    }

    return 0;
}

int 
store_final_prob (struct symtab *ptr, struct prob *list, double sum, int col)
{
    struct symtab *inferred_att = index_to_att(ptr,InferredAttribute,0);
    struct symtab *local = inferred_att->next;
    struct prob *tmpl = list;
    int init = 0;
    CELL temp;
    int maxvalue = 0, max = 0, count = 1;

    while((local = index_to_val(local,init)) != NULL ) {
        init = 1;
        if ( sum < 0.00000001 ) {
            temp = 0;
        } else {
            temp = (CELL)((tmpl->probability/sum) * 100.0 + 0.5);
        }
        local->element.val->desc.infr->cellptr[col] = temp;

        if ( temp > max ) {
            max = temp;
            maxvalue = count;
        }
        count++;
        local = local->next;
        tmpl = tmpl->next;
    }
    inferred_att->element.att->cellptr[col] = maxvalue;

    return 0;

}

int 
write_maprow (struct symtab *ptr)
{
    struct symtab *inferred_att = index_to_att(ptr,InferredAttribute,0);
    struct symtab *local = inferred_att->next;
    int init = 0;

    while((local = index_to_val(local,init)) != NULL ) {
        init = 1;
        if ( probabilitymaps ) {
            G_put_map_row(local->element.val->desc.infr->fildes,
                local->element.val->desc.infr->cellbuf);
        }
        local->element.val->desc.infr->cellptr = 
            local->element.val->desc.infr->cellbuf;
        local = local->next;
    }
    if ( combinedmap ) {
        G_put_map_row(inferred_att->element.att->fildes,
            inferred_att->element.att->cellbuf);
    }
    inferred_att->element.att->cellptr = 
        inferred_att->element.att->cellbuf;

    return 0;
}

int 
close_files (struct symtab *ptr, char *result)
{
    struct symtab *local = ptr;
    while ( local != NULL ) {
        switch(local->s_type) {
        case HeadSymbol:
            break;
        case AttributeSymbol:
            switch(local->element.att->a_type) {
            case LayerAttribute:
                if ( G_close_cell(local->element.att->fildes) == -1 ) {
                    fprintf(stderr,"Error on G_close_cell()\n");
                }
                break;
            case InferredAttribute:
                if ( combinedmap ) {
                    if ( G_close_cell(local->element.att->fildes) == -1 ) {
                        fprintf(stderr,"Error on G_close_cell()\n");
                    }
                }
                break;
            case ContextAttribute:
            case SubjectiveAttribute:
                break;
            }
            break;
        case ValueSymbol:
            if ( local->element.val->v_type == InferredAttribute 
                && probabilitymaps ) {
                if ( G_close_cell(local->element.val->desc.infr->fildes) == -1 ) {
                    fprintf(stderr,"Error on G_close_cell()\n");
                }
            }
            break;
        }
        local = local->next;
    }

    return 0;
}

int 
write_support (struct symtab *ptr, char *result)
{
    struct symtab *local = ptr;
    struct Range range;
    struct Colors color;
    CELL min, max;

    do_cats(ptr,result);
    while ( local != NULL ) {
        switch(local->s_type) {
        case HeadSymbol:
            break;
        case AttributeSymbol:
            switch(local->element.att->a_type) {
            case LayerAttribute:
                break;
            case InferredAttribute:
                if ( combinedmap ) {
                    G_read_range(result,G_mapset(),&range);
		    G_get_range_min_max(&range, &min, &max);

                    switch ( colortable ) {
                    case AspectColors:
                        G_make_aspect_colors(&color,min,max);
                        break;
                    case GreyScale:
                        G_make_grey_scale_colors(&color,min,max);
                        break;
                    case HistoGreyScale:
                        G_make_grey_scale_colors(&color,min,max);
                        break;
                    case Rainbow:
                        G_make_rainbow_colors(&color,min,max);
                        break;
                    case Ramp:
                        G_make_ramp_colors(&color,min,max);
                        break;
                    case Random:
                        G_make_random_colors(&color,min,max);
                        break;
                    case RYG:
                        G_make_ryg_colors(&color,min,max);
                        break;
                    case Wave:
                        G_make_wave_colors(&color,min,max);
                        break;
                    }
                    G_write_colors(result,G_mapset(),&color);
                    G_free_colors(&color);
                }
                break;
            case ContextAttribute:
            case SubjectiveAttribute:
                break;
            }
            break;
        case ValueSymbol:
            if ( local->element.val->v_type == InferredAttribute 
                && probabilitymaps ) {
                G_read_range(local->name,G_mapset(),&range);
		G_get_range_min_max(&range, &min, &max);

                switch ( local->colortable ) {
                case AspectColors:
                    G_make_aspect_colors(&color,min,max);
                    break;
                case GreyScale:
                    G_make_grey_scale_colors(&color,min,max);
                    break;
                case HistoGreyScale:
                    G_make_grey_scale_colors(&color,min,max);
                    break;
                case Rainbow:
                    G_make_rainbow_colors(&color,min,max);
                    break;
                case Ramp:
                    G_make_ramp_colors(&color,min,max);
                    break;
                case Random:
                    G_make_random_colors(&color,min,max);
                    break;
                case RYG:
                    G_make_ryg_colors(&color,min,max);
                    break;
                case Wave:
                    G_make_wave_colors(&color,min,max);
                    break;
                }
                G_write_colors(local->name,G_mapset(),&color);
                G_free_colors(&color);
            }
            break;
        }
        local = local->next;
    }

    return 0;
}


int do_cats(struct symtab *ptr,char *result)
{
    struct Categories cats;
    struct symtab *inferred_att = index_to_att(ptr,InferredAttribute,0);
    struct symtab *local = inferred_att->next;
    int i = 0;
    char title[256];

    G_read_cats(result,G_mapset(),&cats);
    sprintf(title,"Combined map. Created by r.binfer: %s",G_date());
    G_set_cats_title(title,&cats);
    sprintf(title,"no data");
    G_set_cat((CELL)i++,title,&cats);
    inferred_att = inferred_att->next;
    while(inferred_att != (struct symtab *)0 ) {
        sprintf(title,"attribute value: %s",inferred_att->name);
        G_set_cat((CELL)i++,title,&cats);
        inferred_att = inferred_att->next;
    }
    G_write_cats(result,&cats);
    G_free_cats(&cats);

    while(local != (struct symtab *)0 ) {
        G_read_cats(local->name,G_mapset(),&cats);
        sprintf(title,"Probability map. Created by r.binfer: %s",G_date());
        G_set_cats_title(title,&cats);
        sprintf(title,"no data");
        G_set_cat((CELL)0,title,&cats);
        for(i = 1; i <= 100; i++ ) {
            sprintf(title,"probability of %.2f",(double)i/100.0);
            G_set_cat((CELL)i,title,&cats);
        }
        G_write_cats(local->name,&cats);
        G_free_cats(&cats);
        local = local->next;
    }

    return 0;
}
