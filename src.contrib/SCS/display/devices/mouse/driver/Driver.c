/* Logitech Bus Mouse device driver
 *
 * Author: Paul W. Carlson	Dec. 1988
 */
#include <sys/types.h>
#include <sys/signal.h>
#include <sys/systm.h>
#include <sys/param.h>
#include <sys/dir.h>
#include <sys/user.h>
#include <sys/errno.h>
#include <sys/file.h>

#define DATA_PORT	0x23c
#define CONT_PORT	0x23e
#define CONF_PORT	0x23f
#define CONF_BYTE	0x91
#define R_BUTTON	0x20
#define M_BUTTON	0x40
#define L_BUTTON	0x80
#define HI_NYBLE	0x20
#define LOW_NYBLE	0x00
#define READ_X		0x00
#define READ_Y		0x40
#define LATCH_DATA	0x80
#define ENABLE_MOUSE	0x00
#define DISABLE_MOUSE	0x10
#define OPEN		0x02

#define MOUSE_INIT	(('M' << 8) | 81)
#define GET_MOUSE	(('M' << 8) | 82)

static int Status;
static int *mous_argp;
static int ints_enabled;
static struct mous_struct {
    int x;
    int y;
    int button;
    int stat;
} mous_io;

/************** Function: mousopen ********************************
 */
mousopen(dev, flag, id)
int dev, flag, id;
{

    /* check if already open */
    if (Status & OPEN) u.u_error = EBUSY;
    else
    {	
	/* open driver and disable mouse interrupts */
	Status |= OPEN;
	mous_io.button = 0;
	outb(CONF_PORT, CONF_BYTE);
    	outb(CONT_PORT, DISABLE_MOUSE);	
    }
}


/************** Function: mousclose ********************************
 */
mousclose(dev, flag, id)
int dev, flag, id;
{
    outb(CONT_PORT, DISABLE_MOUSE);
    outb(CONF_PORT, 0);
    Status = ~OPEN;
}


/************** Function: mousintr ********************************
 */
mousintr(vect)
int vect;
{
    char delta_x, delta_y;
    unsigned char status;

    outb(CONT_PORT, DISABLE_MOUSE);	
    if ((Status & OPEN) == 0) return;

    /* capture the info */
    outb(CONT_PORT, LATCH_DATA);

    /* read delta X */
    outb(CONT_PORT, LATCH_DATA | READ_X | HI_NYBLE);
    status = inb(DATA_PORT);
    delta_x = (status << 4) & 0xf0 ;
    outb(CONT_PORT, LATCH_DATA | READ_X | LOW_NYBLE);
    delta_x |= (inb(DATA_PORT) & 0x0f);
	
    /* read delta Y */
    outb(CONT_PORT, LATCH_DATA | READ_Y | HI_NYBLE);
    delta_y = (inb(DATA_PORT) & 0x0f) << 4;
    outb(CONT_PORT, LATCH_DATA | READ_Y | LOW_NYBLE);
    delta_y |= (inb(DATA_PORT) & 0x0f);

    /* if either x or y has changed, update coordinates */
    if (delta_x | delta_y)
    {	mous_io.x += delta_x;
	if (mous_io.x < 0) mous_io.x = 0;
	mous_io.y += delta_y;
	if (mous_io.y < 0) mous_io.y = 0;
    }

    /* if a button was pushed, update button status */
    status &= 0xE0;
    mous_io.button = 0;
    if (!(status & L_BUTTON)) mous_io.button = 1;
    if (!(status & M_BUTTON)) mous_io.button = 2;
    if (!(status & R_BUTTON)) mous_io.button = 3;
}


/************** Function: mousioctl ********************************
 */
mousioctl(dev, cmd, argp, mode)
dev_t dev;
int cmd, *argp, mode;
{
    int s;

    s = spl1();
    switch (cmd)
    {	case MOUSE_INIT:
	    mous_argp = argp;
	    copyin(mous_argp, &mous_io, sizeof(struct mous_struct));
	    mous_io.stat = 0;
	    break;

	case GET_MOUSE:
	    mous_io.stat = 0;
      	    outb(CONT_PORT, ENABLE_MOUSE);	
	    mous_io.stat = 1;
	    copyout(&mous_io, mous_argp, sizeof(struct mous_struct));
	    break;

	default:
	    u.u_error = EINVAL;
	    break;
    }
    splx(s);
}
