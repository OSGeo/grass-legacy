#ifndef _Reclass_h
#define _Reclass_h

#include <Xm/Xm.h>

#ifndef XmRECLASS_BIT
#define XmRECLASS_BIT (54)
#endif

/* Class record constants */

externalref WidgetClass reclassWidgetClass;

typedef struct _ReclassClassRec * ReclassWidgetClass;
typedef struct _ReclassRec      * ReclassWidget;


#ifndef XgIsReclass
#define XgIsReclass(w) (_XmIsFastSubclass(XtClass(w),XmRECLASS_BIT))
#endif

#define XmNoriginalMap "originalMap"
#define XmCOriginalMap "OriginalMap"

#define XmNoriginalMapset "originalMapset"
#define XmCOriginalMapset "OriginalMapset"

#define XmNoriginalMapLabelString "originalMapLabelString"
#define XmCOriginalMapLabelString "OriginalMapLabelString"

#define XmNoriginalMapListLabelString "originalMapListLabelString"
#define XmCOriginalMapListLabelString "OriginalMapListLabelString"

#define XmNnewMap "newMap"
#define XmCNewMap "NewMap"

#define XmNnewMapTitle "newMapTitle"
#define XmCNewMapTitle "NewMapTitle"

#define XmNnewMapset "newMapset"
#define XmCNewMapset "NewMapset"

#define XmNnewMapTitleLabelString "newMapTitleLabelString"
#define XmCNewMapTitleLabelString "NewMapTitleLabelString"

#define XmNnewMapLabelString "newMapLabelString"
#define XmCNewMapLabelString "NewMapLabelString"

#define XmNnewMapListLabelString "newMapListLabelString"
#define XmCNewMapListLabelString "NewMapListLabelString"

#define XmNcatValueLabelString "catValueLabelString"
#define XmCCatValueLabelString "CatValueLabelString"

#define XmNcatNameTextLabelString "catNameTextLabelString"
#define XmCCatNameTextLabelString "CatNameTextLabelString"

#define XmNreclassOnOk "reclassOnOk"
#define XmCReclassOnOk "ReclassOnOk"

#define XmNreclassOnApply "reclassOnApply"
#define XmCReclassOnApply "ReclassOnApply"

/*  Creation entry points:
*/
#ifdef _NO_PROTO

extern Widget XgReclassGetChild() ;
extern Widget XgCreateReclass() ;
extern Widget XgCreateReclassDialog() ;

#else /* _NO_PROTO */

#if defined(__cplusplus) || defined(c_plusplus)
extern "C" {
#endif


extern Widget XgReclassGetChild( Widget fs, unsigned char which) ;
extern Widget XgCreateReclass( Widget p, String name, ArgList args,
                                                                  Cardinal n) ;
extern Widget XgCreateReclassDialog( Widget ds_p, String name,
                                            ArgList fsb_args, Cardinal fsb_n) ;
#endif /* _NO_PROTO */

#if defined(__cplusplus) || defined(c_plusplus)
}
#endif


#endif /* _Reclass_h */
/* DON'T ADD ANYTHING AFTER THIS #endif */
