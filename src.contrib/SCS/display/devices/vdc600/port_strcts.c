/* Function: init_port_structs
**
** Author: Paul W. Carlson		Jan. 1990
*/

#include <sys/types.h>
#include <sys/at_ansi.h>
#include <sys/kd.h>
#include "vdc600.h"

void init_port_structs()
{
    /* initialize the port structure for updating the video segment */
    set_seg.args[0].dir  = OUT_ON_PORT;
    set_seg.args[0].port = GDC_IND_SEL;
    set_seg.args[0].data = 0x0F;
    set_seg.args[1].dir  = OUT_ON_PORT;
    set_seg.args[1].port = GDC_IND_REG;
    set_seg.args[1].data = 0x05;
    set_seg.args[2].dir  = OUT_ON_PORT;
    set_seg.args[2].port = GDC_IND_SEL;
    set_seg.args[2].data = 0x09;
    set_seg.args[3].dir  = OUT_ON_PORT;
    set_seg.args[3].port = GDC_IND_REG;
    set_seg.args[3].data = 0x00;

    /* initialize the port structure for selecting a GDC register */
    sel_gdc.args[0].dir  = OUT_ON_PORT;
    sel_gdc.args[0].port = GDC_IND_SEL;
    sel_gdc.args[0].data = 0x00;
    sel_gdc.args[1].dir  = OUT_ON_PORT;
    sel_gdc.args[1].port = 0x00;
    sel_gdc.args[1].data = 0x00;

    /* initialize the port structure for outputting a GDC byte */
    out_gdc.args[0].dir  = OUT_ON_PORT;
    out_gdc.args[0].port = GDC_IND_REG;
    out_gdc.args[0].data = 0x00;
    out_gdc.args[1].dir  = OUT_ON_PORT;
    out_gdc.args[1].port = 0x00;
    out_gdc.args[1].data = 0x00;

    /* initialize the port structure for selecting a TS register */
    sel_ts.args[0].dir  = OUT_ON_PORT;
    sel_ts.args[0].port = TS_IND_SEL;
    sel_ts.args[0].data = 0x00;
    sel_ts.args[1].dir  = OUT_ON_PORT;
    sel_ts.args[1].port = 0x00;
    sel_ts.args[1].data = 0x00;

    /* initialize the port structure for outputting a TS byte */
    out_ts.args[0].dir  = OUT_ON_PORT;
    out_ts.args[0].port = TS_IND_REG;
    out_ts.args[0].data = 0x00;
    out_ts.args[1].dir  = OUT_ON_PORT;
    out_ts.args[1].port = 0x00;
    out_ts.args[1].data = 0x00;

    /* initialize the port structure to wait for vertical retrace */
    vwait.args[0].dir = IN_ON_PORT;
    vwait.args[0].port = INP_STAT_1;
    vwait.args[0].data = 0x00;
    vwait.args[1].dir = OUT_ON_PORT;
    vwait.args[1].port = 0x00;
    vwait.args[1].data = 0x00;
}
