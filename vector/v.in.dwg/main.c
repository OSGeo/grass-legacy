/* ***************************************************************
 * *
 * * MODULE:       v.in.dwg
 * * 
 * * AUTHOR(S):    Radim Blazek
 * *               
 * * PURPOSE:      Import OGR vectors
 * *               
 * * COPYRIGHT:    (C) 2001 by the GRASS Development Team
 * *
 * *               This program is free software under the 
 * *               GNU General Public License (>=v2). 
 * *               Read the file COPYING that comes with GRASS
 * *               for details.
 * *
 * **************************************************************/
#define AD_PROTOTYPES
#define AD_VM_PC
#define OD_GENERIC_READ

#define MAIN

#include <stdio.h>
#include <stdlib.h> 
#include <string.h> 
#include <math.h>
#include <fcntl.h>
#include <unistd.h> 
#include "gis.h"
#include "dbmi.h"
#include "Vect.h"
#include "ad2.h"
#include "odio.h"
#include "global.h"

int 
main (int argc, char *argv[])
{
    struct GModule *module;
    struct Option *out_opt, *in_opt;
    char   buf[2000];
    dbHandle handle;

    /* DWG */
    char   path[2000];
    short initerror, entset, retval;
    AD_OBJHANDLE pspace, mspace;
    PAD_ENT_HDR adenhd;
    PAD_ENT aden;
    AD_VMADDR entlist;
    
    G_gisinit(argv[0]);
    
    module = G_define_module();
    module->description = "Convert DWG/DXF to GRASS vector";

    out_opt = G_define_standard_option(G_OPT_V_OUTPUT);
    out_opt->required = NO;

    in_opt = G_define_option();
    in_opt->key = "input";
    in_opt->type =  TYPE_STRING;
    in_opt->required = YES;
    in_opt->description = "DWG or DXF file.";

    if (G_parser (argc, argv)) exit(-1); 

    db_init_string (&sql);
    db_init_string (&str);
    adenhd=(PAD_ENT_HDR)malloc(sizeof(AD_ENT_HDR));
    aden=(PAD_ENT)malloc(sizeof(AD_ENT));
    Layer=(PAD_LAY)malloc(sizeof(AD_LAY));
    Points = Vect_new_line_struct ();
    Cats = Vect_new_cats_struct ();
    Block = NULL;

    atrans = 20; /* nested, recursive levels */
    Trans = (TRANS *) G_malloc ( atrans * sizeof(TRANS) );
    
    /* Init OpenDWG */
    sprintf ( path, "%s/etc/adinit.dat", G_gisbase() ); 
    if (!adInitAd2( path, &initerror)) {
        sprintf(buf, "Unable to initialize OpenDWG Toolkit, error: %d: %s.", initerror,adErrorStr(initerror));
        if (initerror==AD_UNABLE_TO_OPEN_INIT_FILE) sprintf ( buf, "%s Cannot open %s", buf, path);
        G_fatal_error ( buf );
    }
    adSetupDwgRead();
    adSetupDxfRead();
    
    /* Open input file */
    if ((dwghandle=adLoadFile(in_opt->answer,AD_PRELOAD_ALL,1))==NULL) {
	G_fatal_error ("Cannot open input file. Error %d: %s", adError(),adErrorStr(adError()) );
    }

    /* open output vector */
    Vect_open_new (&Map, out_opt->answer, 0 ); 

    /* Add DB link */
    Fi = Vect_default_field_info ( Map.name, 1, NULL, GV_1TABLE );
    Vect_map_add_dblink ( &Map, 1, NULL, Fi->table, "cat", Fi->database, Fi->driver);

    /* Create table */
    sprintf ( buf, "create table %s ( cat integer, entity_name varchar(20), color int, weight int, "
	           "block varchar(100), layer varchar(100), txt varchar(100) )", Fi->table );
    db_set_string ( &sql, buf);
    G_debug ( 3, db_get_string ( &sql ) );

    driver = db_start_driver( Fi->driver );
    if (driver == NULL) G_fatal_error ( "Cannot open driver %s", Fi->driver );
    db_init_handle (&handle);
    db_set_handle (&handle, Vect_subst_var(Fi->database,Map.name,G_mapset()), NULL);
    if (db_open_database(driver, &handle) != DB_OK) {
	db_shutdown_driver(driver);
	G_fatal_error ( "Cannot open database %s", Fi->database );
    }
    if (db_execute_immediate (driver, &sql) != DB_OK ) {
	db_close_database(driver);
	db_shutdown_driver(driver);
	G_fatal_error ( "Cannot create table: %s", db_get_string ( &sql )  );
    }

    cat = 1;
    /* Write each entity. Some entities may be composed by other entities (like INSERT or BLOCK) */
    /* Set transformation for first (index 0) level */
    Trans[0].dx = Trans[0].dy = Trans[0].dz = 0;
    Trans[0].xscale = Trans[0].yscale = Trans[0].zscale = 1;
    Trans[0].rotang = 0;
    if (adGetBlockHandle(dwghandle, pspace, AD_PAPERSPACE_HANDLE)) {
	entlist=adEntityList(dwghandle,pspace);
	adStartEntityGet(entlist);
	for (entset=0; entset<2; entset++) {
	    do {
	        if (!(retval=adGetEntity(entlist,adenhd,aden))) continue;
	        wrentity(adenhd,aden, 0, entlist);
	    } while (retval==1);
	    if (entset==0) {
	        if (adGetBlockHandle(dwghandle, mspace, AD_MODELSPACE_HANDLE)) {
	            entlist=adEntityList(dwghandle,mspace);
	            adStartEntityGet(entlist);
	        }
	    }
	}
    }

    free(aden);
    free(adenhd);
	
    adCloseFile(dwghandle);
    adCloseAd2();
    
    db_close_database(driver);
    db_shutdown_driver(driver);
    
    Vect_build ( &Map, stdout );
    Vect_close ( &Map );

    exit(0) ;
}

