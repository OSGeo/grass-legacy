#include "labels.h"
#ifndef M_E

/**
 * This is simply Euler's number
 */
# define M_E 2.7182818284590452353602874713526625L
#endif

/**
 * How many times to decrease the Temperature T.
 */
#define TEMP_DECS 50

static double calc_label_overlap(label_t * label, int cc, int nc);
static void do_label_overlap(label_t * label, int cc, int nc);

static unsigned int overlaps_created = 0, overlaps_removed = 0;

void simulate_annealing(label_t * labels, int n_labels, struct params *p)
{
    /* The temperature of the system */
    double T;
    /* The change in energy */
    double dE;
    T = -1.0 / log(1.0 / 3.0);
    unsigned int t, tot_better = 0, tot_worse = 0, tot_ign = 0;

    fprintf(stderr, "Optimizing label positions: ...");
    for (t = 0; t < TEMP_DECS; t++) {
	int i;
	unsigned int successes = 0;
	for (i = 0; i < (n_labels * 30); i++) {
	    int l, c, cc, r;
	    label_t *lp;

	    /* pick a random label */
	    r = rand();
	    l = (int)((double)(n_labels) * (r / (RAND_MAX + 1.0)));
	    lp = &labels[l];
	    /* skip labels without sufficient number of candidates */
	    if (lp->n_candidates < 2)
		continue;

	    cc = lp->current_candidate;
	    /*, and a random new candidate place */
	    c = (int)((double)(lp->n_candidates) * (rand() / (RAND_MAX + 1.0)));
	    if (c == cc) {
		if (c == 0)
		    c++;
		else
		    c--;
	    }
	    /* calc dE */
	    dE = lp->candidates[c].score - lp->candidates[cc].score;
	    dE += calc_label_overlap(lp, cc, c);

	    /* if dE < 0 accept */
	    if (dE < 0.0) {
		do_label_overlap(lp, cc, c);
		lp->current_score += lp->candidates[c].score;
		lp->current_candidate = c;
		successes++;
		tot_better++;
	    }
	    /* else keep with probability p=e^(-dE/T) */
	    else {
		double p, r;
		p = pow(M_E, -dE / T);
		r = rand() / (RAND_MAX + 1.0);
		if (r < p) {
		    do_label_overlap(lp, cc, c);
		    lp->current_score += lp->candidates[c].score;
		    lp->current_candidate = c;
		    successes++;
		    tot_worse++;
		}
		else {
		    tot_ign++;
		}
	    }
	    /* decrease immediately */
	    if (successes > (5 * n_labels)) {
		break;
	    }
	}
	G_percent(t, TEMP_DECS, 1);
	/* we have found an optimal solution */
	if (successes == 0) {
	    break;
	}
	T -= 0.1 * T;
    }
    G_percent(TEMP_DECS, TEMP_DECS, 1);
}

/**
 * This function calculates the change in E (dE) if the given label would
 * be moved to the new place. Contrary to the original algorithm this function
 * requires twice as much energy to overlap two labels then is released by
 * resolving an overlap. I don't have and scientific fact but it seems to
 * improve the result.
 * @param label The label in question
 * @param cc The current candidate
 * @param nc The new potential candidate location
 * @return The dE value.
 */
static double calc_label_overlap(label_t * label, int cc, int nc)
{
    int i, old_overlaps = 0, new_overlaps = 0;

    label->current_score = 0.0;
    for (i = 0; i < label->candidates[cc].n_intersections; i++) {
	label_t *ol;
	int oc;

	ol = label->candidates[cc].intersections[i].label;
	oc = label->candidates[cc].intersections[i].candidate;
	if (ol->current_candidate == oc) {
	    old_overlaps++;
	}
    }
    for (i = 0; i < label->candidates[nc].n_intersections; i++) {
	label_t *ol;
	int oc;

	ol = label->candidates[nc].intersections[i].label;
	oc = label->candidates[nc].intersections[i].candidate;
	if (ol->current_candidate == oc) {
	    new_overlaps++;
	}
    }
    return (double)((new_overlaps) - old_overlaps) * 80.0;
}

/**
 * This function commits the label change to the new location.
 * @param label The label to move
 * @param cc The current candidate
 * @param nc The new potential candidate location
 */
static void do_label_overlap(label_t * label, int cc, int nc)
{
    int i;

    label->current_score = 0.0;
    for (i = 0; i < label->candidates[cc].n_intersections; i++) {
	label_t *ol;
	int oc;

	ol = label->candidates[cc].intersections[i].label;
	oc = label->candidates[cc].intersections[i].candidate;
	if (ol->current_candidate == oc) {
	    ol->current_score -= 80.0;
	    overlaps_removed++;
	}
    }
    for (i = 0; i < label->candidates[nc].n_intersections; i++) {
	label_t *ol;
	int oc;

	ol = label->candidates[nc].intersections[i].label;
	oc = label->candidates[nc].intersections[i].candidate;
	if (ol->current_candidate == oc) {
	    label->current_score += 80.0;
	    ol->current_score += 80.0;
	    overlaps_created++;
	}
    }
}
