/* ppmmerge.c - wrapper program for PPM
**
** Copyright (C) 1991 by Jef Poskanzer.
**
** Permission to use, copy, modify, and distribute this software and its
** documentation for any purpose and without fee is hereby granted, provided
** that the above copyright notice appear in all copies and that both that
** copyright notice and this permission notice appear in supporting
** documentation.  This software is provided "as is" without express or
** implied warranty.
*/

#include <stdio.h>
#include "ppm.h"

void main( argc, argv )
int argc;
char *argv[];
    {
    register char *cp;

    if ( argv[0] == NULL )
	cp = "<nil>";
    else if ( ( cp = rindex( argv[0], '/' ) ) != NULL )
	++cp;
    else
	cp = argv[0];

#define TRY(s,m) { if ( strcmp( cp, s ) == 0 ) m( argc, argv ); }

    TRY("giftoppm", giftoppm_main);
    TRY("gouldtoppm", gouldtoppm_main);
    TRY("ilbmtoppm", ilbmtoppm_main);
    TRY("imgtoppm", imgtoppm_main);
    TRY("mtvtoppm", mtvtoppm_main);
    TRY("pcxtoppm", pcxtoppm_main);
    TRY("pgmtoppm", pgmtoppm_main);
    TRY("pi1toppm", pi1toppm_main);
    TRY("picttoppm", picttoppm_main);
    TRY("ppmhist", ppmhist_main);
    TRY("ppmmake", ppmmake_main);
    TRY("ppmpat", ppmpat_main);
    TRY("ppmquant", ppmquant_main);
    TRY("ppmrelief", ppmrelief_main);
    TRY("ppmtogif", ppmtogif_main);
    TRY("ppmtoilbm", ppmtoilbm_main);
    TRY("ppmtopcx", ppmtopcx_main);
    TRY("ppmtopgm", ppmtopgm_main);
    TRY("ppmtopi1", ppmtopi1_main);
    TRY("ppmtopict", ppmtopict_main);
    TRY("ppmtops", ppmtops_main);
    TRY("ppmtopuzz", ppmtopuzz_main);
    TRY("ppmtorgb3", ppmtorgb3_main);
    TRY("ppmtouil", ppmtouil_main);
    TRY("ppmtoxpm", ppmtoxpm_main);
    TRY("qrttoppm", qrttoppm_main);
    TRY("rawtoppm", rawtoppm_main);
    TRY("rgb3toppm", rgb3toppm_main);
    TRY("spctoppm", spctoppm_main);
    TRY("sputoppm", sputoppm_main);
    TRY("tgatoppm", tgatoppm_main);
    TRY("ximtoppm", ximtoppm_main);
    TRY("xpmtoppm", xpmtoppm_main);

    (void) fprintf(
	stderr, "ppmmerge: \"%s\" is an unknown PPM program!\n", cp );
    exit( 1 );
    }
