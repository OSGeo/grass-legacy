#include "xgrass_lib.h"
#include "xgbitmaps.h"
#include "Interact.h"
#include "Region.h"
#include "Browser.h"
#include "Caption.h"

#include "gis.h"
#include <ctype.h>


typedef struct _edit_cellhd_data {
    struct Cell_head *cellhd;
    Widget north, south, east, west;
} EditCellhdData;

EditCellhdData *XgEditCellhd();
