#include "labels.h"
#ifndef M_E
# define M_E 2.7182818284590452353602874713526625L
#endif

#define TEMP_DECS 50

static double calc_label_overlap(label_t *label, int cc, int nc);
static void do_label_overlap(label_t *label, int cc, int nc);

void simulate_annealing(label_t *labels, int n_labels, struct params *p)
{
	double T, dE;
	T = -1.0/log(1.0/3.0);
	unsigned int t, tot_better=0, tot_worse=0, tot_ign=0;

	fprintf(stderr, "Optimizing label positions: ...");
	for(t=0;t<TEMP_DECS;t++) {
		int i;
		unsigned int successes=0;
		for(i=0; i < (n_labels * 20); i++) {
			int l,c,cc, r;
			label_t *lp;
			/* pick a random label*/
			r = rand();
			l = (int)((double)(n_labels) * (r / (RAND_MAX + 1.0)));
			lp = &labels[l];
			/*, and a random new candidate place */
			c = (int)((double)(lp->n_candidates) * 
					  (rand() / (RAND_MAX + 1.0)));
			cc = lp->current_candidate;
			if(c == cc) {
				if(c==0) c++;
				else c--;
			}
			/* calc dE */
			dE = lp->current_score - lp->candidates[cc].score +
				lp->candidates[c].score;
			dE += calc_label_overlap(lp, cc, c);
/*			printf("%s:\n"
				   "\tPoint at (%lf,%lf), candidate %d at (%lf,%lf)\n"
				   "\tbb (NEWS)=(%lf,%lf,%lf,%lf)\n", lp->text,
				   lp->shape->x[0],lp->shape->y[0], c,
				   lp->candidates[c].point.x, lp->candidates[c].point.y,
				   lp->bb.N, lp->bb.E, lp->bb.W, lp->bb.S);*/
/*			fprintf(stderr, "T=%lf dE=%lf, i=%d l=%d cc=%d c=%d\r",
					T,dE,i,l,cc,c);*/
			
			/* if dE < 0 accept */
			if(dE < 0) {
				do_label_overlap(lp, cc, c);
				lp->current_score += lp->candidates[c].score;
				lp->current_candidate=c;
				successes++;
				tot_better++;
			}
			/* else revert with probability p=1-e^(-dE/T) */
			else {
				double p,r;
				p = 1.0 - pow(M_E, -dE/T);
				r = rand() / (RAND_MAX + 1.0);
				if(p < r) {
/*					printf("p<r: dE=%lf, p=%lf, T=%lf, r=%lf\n",
						dE, p, T, r);*/
					do_label_overlap(lp, cc, c);
					lp->current_score += lp->candidates[c].score;
					lp->current_candidate=c;
					successes++;
					tot_worse++;
				}
				else {
					tot_ign++;
				}
			}
			/* decrease immediately */
			if(successes > (5*n_labels)) {
				break;
			}
		}
/*		if(t % 10000 == 0)
		{
			char *tmp;
			FILE *labelf;
			tmp = G_tempfile();
			fprintf(stderr, "\nWriting labels to file %s: ...", tmp);
			labelf = fopen(tmp, "w");
			for(i=0; i < n_labels;i++) {
				if(labels[i].n_candidates > 0)
					print_label(labelf, &labels[i], p);
				G_percent(i, (n_labels-1), 1);
			}
			fclose(labelf);
		}
*/
		G_percent(t, TEMP_DECS, 1);
		/* we have found an optimal solution */
		if(successes == 0) {
			break;
		}
		T -= 0.1 * T;
	}
	G_percent(TEMP_DECS, TEMP_DECS, 1);
	fprintf(stderr, "%u moves improving placing %u moves worsening placing %d ignored moves in %d rounds\n",
			tot_better, tot_worse, tot_ign, t);
}

static double calc_label_overlap(label_t *label, int cc, int nc)
{
	int i, old_overlaps=0, new_overlaps=0;

	label->current_score = 0.0;
	for(i=0;i<label->candidates[cc].n_intersections;i++) {
		label_t *ol;
		int oc;
		
		ol = label->candidates[cc].intersections[i].label;
		oc = label->candidates[cc].intersections[i].candidate;
		if(ol->current_candidate == oc) {
			old_overlaps++;
		}
	}
	for(i=0;i<label->candidates[nc].n_intersections;i++) {
		label_t *ol;
		int oc;
		
		ol = label->candidates[nc].intersections[i].label;
		oc = label->candidates[nc].intersections[i].candidate;
		if(ol->current_candidate == oc) {
			new_overlaps++;
		}
	}
	return (double)(new_overlaps - old_overlaps) * 40.0;
}

static void do_label_overlap(label_t *label, int cc, int nc)
{
	int i;

	label->current_score = 0.0;
	for(i=0;i<label->candidates[cc].n_intersections;i++) {
		label_t *ol;
		int oc;
		
		ol = label->candidates[cc].intersections[i].label;
		oc = label->candidates[cc].intersections[i].candidate;
		if(ol->current_candidate == oc) {
			ol->current_score -= 40.0;
		}
	}
	for(i=0;i<label->candidates[nc].n_intersections;i++) {
		label_t *ol;
		int oc;
		
		ol = label->candidates[nc].intersections[i].label;
		oc = label->candidates[nc].intersections[i].candidate;
		if(ol->current_candidate == oc) {
			label->current_score += 40.0;
			ol->current_score += 40.0;
		}
	}
}
