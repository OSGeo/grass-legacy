static char rcsid[] = "@(#)XGRASS $Id: xc.verify.c,v 0.0.0.1 1992/05/05 14:59:03 kurt Exp kurt $";
/*
 * File:
 *
 * Desc:
 *
 * Auth:
 *
 * Date:
 *
 * Modification History:
 *
 *
 */
#include "xc.xclip.h"
#ifdef USE_GRASS
#include "gis.h"
#endif /* USE_GRASS */

VerifyDBElement(p)
    XCParmData *p;
{
    char *temp, *group, *subgroup, *mapset, *element;
    char *strtok();

    if (p->selecttype == XC_SELECT_UNDETERMINED ) {
        /* KAB handle selectable types... */
        ;
    }
    if ( !p->isInput ) return;
    switch(p->value.dbval.type) {
#ifdef USE_GRASS
        case XC_DB_TYPE_RASTER:
	    if ( G_find_file("cell",p->value.dbval.desc,G_mapset()) == NULL ) {
		sprintf(errorbuf,"no access to raster file \"%s\"",
		    p->value.dbval.desc);
		XCFatalError("verify database element",errorbuf);
	    }
	    break;
	case XC_DB_TYPE_ASCII_DLG:
	    if ( G_find_file("dlg",p->value.dbval.desc,G_mapset()) == NULL ) {
		sprintf(errorbuf,"no access to ASCII DLG file \"%s\"",
		    p->value.dbval.desc);
		XCFatalError("verify database element",errorbuf);
	    }
	    break;
	case XC_DB_TYPE_DLG:
	    if ( G_find_file("bdlg",p->value.dbval.desc,G_mapset()) == NULL ) {
		sprintf(errorbuf,"no access to binary dlg file \"%s\"",
		    p->value.dbval.desc);
		XCFatalError("verify database element",errorbuf);
	    }
	    break;
	case XC_DB_TYPE_ASCII_VECTOR:
	    if ( G_find_file("dig_ascii",p->value.dbval.desc,G_mapset()) == NULL ) {
		sprintf(errorbuf,"no access to ASCII vector file \"%s\"",
		    p->value.dbval.desc);
		XCFatalError("verify database element",errorbuf);
	    }
	    break;
	case XC_DB_TYPE_VECTOR:
	    if ( G_find_file("dig",p->value.dbval.desc,G_mapset()) == NULL ) {
		sprintf(errorbuf,"no access to vector file \"%s\"",
		    p->value.dbval.desc);
		XCFatalError("verify database element",errorbuf);
	    }
	    break;
	case XC_DB_TYPE_SITES:
	    if ( G_find_file("site_lists",p->value.dbval.desc,G_mapset()) 
		 == NULL ) {
		sprintf(errorbuf,"no access to sites file \"%s\"",
		    p->value.dbval.desc);
		XCFatalError("verify database element",errorbuf);
	    }
	    break;
	case XC_DB_TYPE_REGION:
	    if ( G_find_file("windows",p->value.dbval.desc,G_mapset()) 
		 == NULL ) {
		sprintf(errorbuf,"no access to region file \"%s\"",
		    p->value.dbval.desc);
		XCFatalError("verify database element",errorbuf);
	    }
	    break;
	case XC_DB_TYPE_ICON:
	    if ( G_find_file("icons",p->value.dbval.desc,G_mapset()) 
		 == NULL ) {
		sprintf(errorbuf,"no access to icon file \"%s\"",
		    p->value.dbval.desc);
		XCFatalError("verify database element",errorbuf);
	    }
	    break;
	case XC_DB_TYPE_LABEL:
	    if ( G_find_file("paint",p->value.dbval.desc,G_mapset()) 
		 == NULL ) {
		sprintf(errorbuf,"no access to label file \"%s\"",
		    p->value.dbval.desc);
		XCFatalError("verify database element",errorbuf);
	    }
	    break;
	case XC_DB_TYPE_IMAGE_GROUP:
	    if ( G_find_file("group",p->value.dbval.desc,G_mapset()) == NULL ) {
		sprintf(errorbuf,"no access to site list \"%s\"",
		    p->value.dbval.desc);
		XCFatalError("verify database element",errorbuf);
	    }
	    break;
	case XC_DB_TYPE_IMAGE_SUBGROUP:
            /* expect group,subgroup@mapset */
	    temp = XtMalloc(strlen(p->value.dbval.desc) + 1 );
	    strcpy(temp,p->value.dbval.desc);
            group = strtok(temp,",");
	    if ( group == NULL ) {
		sprintf(errorbuf,"invalid group in \"%s\"",p->value.dbval.desc);
		XCFatalError("verify database element",errorbuf);
	    }
	    subgroup = strtok(NULL,"@");
	    if ( subgroup == NULL ) {
		sprintf(errorbuf,"invalid subgroup in \"%s\"",
		     p->value.dbval.desc);
		XCFatalError("verify database element",errorbuf);
	    }
	    mapset = strtok(NULL,"");
	    if ( mapset == NULL ) {
		mapset = G_mapset();
	    } 
	    sprintf(element,"group/%s/subgroup",group);
	    if ( G_find_file(element,subgroup,mapset) == NULL ) {
		sprintf(errorbuf,"no access to subgroup \"%s\"",
		    p->value.dbval.desc);
		XCFatalError("verify database element",errorbuf);
	    }
	    break;
#endif /* USE_GRASS */
        case XC_DB_TYPE_USER_DEFINED:
	/* KAB deal with this */
	    break;
	default:
	    XCWarning("verify database element","cannot verify unknown type");
	    break;
    }
}
    
