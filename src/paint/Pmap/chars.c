/*
 * HI plotter character set
 *
 * a 7*14 dot character fits in a 8(0:7)*18(0:17) pixel cell.
 * y==6 is the baseline; y<6 are descenders.
 * (0,6) is the home position;
 *
 *	17 + *  *  *  *  *  *  *  =      -- top-most row
 *	16 + *  *  *  *  *  *  *  =
 *	15 + *  *  *  *  *  *  *  =      H and *: character matrix
 *	14 + *  *  *  *  *  *  *  =	 =: inter-character spacing
 *	13 + *  *  *  *  *  *  *  =
 *	12 + *  *  *  *  *  *  *  =
 *	11 + *  *  *  *  *  *  *  =
 *	10 + *  *  *  *  *  *  *  =
 *	 9 + *  *  *  *  *  *  *  =
 *	 8 + *  *  *  *  *  *  *  =
 *	 7 + *  *  *  *  *  *  *  =
 *	 6 +-H--*--*--*--*--*--*-----H'  -- baseline
 *	 5 + *  *  *  *  *  *  *  =
 *	 4 + *  *  *  *  *  *  *  =      -- bottom-most row
 *	 3 + =  =  =  =  =  =  =  =      <| inter-line spacing
 *	 2 + =  =  =  =  =  =  =  =      <|
 *	 1 + =  =  =  =  =  =  =  =      <|
 *	 0 + =  =  =  =  =  =  =  =      <|
 *       +  +  +  +  +  +  +  +  +
 *       0  1  2  3  4  5  6  7  8
 *       ^                 ^
 *       left-most col     right-most col
 *
 * H: home of current cell; H': home of next cell
 *
 * UNLEAD: move to (0,6) in the current cell
 * LEAD:   move to (0,6) of the next cell [(8,6) relative to the current cell]
 * M(x,y): move to (x,y) of the current cell
 * D(x,y): draw to (x,y) of the current cell
 *
 * the first byte is the octal character code (they don't really have
 * to be continuous or consequtive, but it looks better that way).
 * what follows is a list of move/draw operations encoded into 16 bits:
 *
 *	 1 1 1 1 1 1
 *   5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
 *	| | | | | | | | | | | | | | | | |
 *	|*|      x      |       y       |
 *
 * * bit 15, if 1, means draw while moving; otherwise just move.
 * the next-to-last operation is a move (a "lead") to the home position
 * for the next character; for "zero-length" characters, such as underscore,
 * the lead is to the home position in the current cell, so "overstrike"
 * is possible.
 * the last operation is a stop code to terminate the sequence.
 *
 * last modification: sometime in antiquity, j w hamilton
 *
 */

# define M(x,y) (x << 8 | (y & 0xFF))		/* Moveto(x,y) */
# define D(x,y) (x << 8 | (y & 0xFF) | 0x8000)	/* Drawto(x,y) */
# define LEAD   (8 << 8 | (6 & 0xFF))		/* Leadto(8,6) */
# define UNLEAD (0 << 8 | (6 & 0xFF))		/* Leadto(0,6) */
# define STOP   -1				/* STOPdrawing */

short int Chars[] =
{
/* null (zero-width) */
	 000, STOP,

/* downward-arrow */
	 001, M( 0, 9), D( 3, 6), D( 3,14), M( 3, 6), D( 6, 9),
	      LEAD, STOP,

/* Alpha */
	 002, M( 6, 6), D( 3, 9), D( 1, 9), D( 0, 8),
	      D( 0, 7), D( 1, 6), D( 3, 6), D( 6, 9),
	      LEAD, STOP,

/* Beta */
	 003, D( 2, 7), D( 2,12), D( 3,13), D( 5,13),
	      D( 6,12), D( 6,11), D( 5,10), D( 2,10),
	      M( 5,10), D( 6, 9), D( 6, 8), D( 5, 7),
	      D( 3, 7), D( 2, 8),
	      LEAD, STOP,

/* carat (full-width) */
	 004, M( 0, 8), D( 3,11), D( 6, 8),
	      LEAD, STOP,

/* "not" (bar with rhs dropped down) */
	 005, M( 1,10), D( 5,10), D( 5, 8),
	      LEAD, STOP,

/* Epsilon */
	 006, M( 3, 9), D( 2,10), D( 1,10), D( 0, 9),
	      D( 0, 7), D( 1, 6), D( 2, 6), D( 3, 7),
	      M( 2, 8), D( 0, 8),
	      LEAD, STOP,

/* Pi */
	 007, M( 0,10), D( 6,10), M( 4,10), D( 4, 6),
	      M( 2, 6), D( 2,10),
	      LEAD, STOP,

/* Lambda */
	 010, D( 3, 9), M( 0,11), D( 1,11), D( 6, 6),
	      LEAD, STOP,

/* Gamma */
	 011, M( 0,11), D( 1,12), D( 2,12), D( 5, 9),
	      D( 5, 7), D( 4, 6), D( 3, 6), D( 2, 7),
	      D( 2, 8), D( 6,12),
	      LEAD, STOP,

/* circle-with-a-handle (approx. Fo) */
	 012, M( 2,10), D( 1,10), D( 0, 9), D( 0, 7),
	      D( 1, 6), D( 3, 6), D( 4, 7), D( 4, 9),
	      D( 3,10), D( 2,10), D( 2,12), D( 4,12),
	      LEAD, STOP,

/* Integral Symbol */
	 013, M( 0, 7), D( 1, 6), D( 2, 6), D( 3, 7),
	      D( 3,12), D( 4,13), D( 5,13), D( 6,12),
	      LEAD, STOP,

/* Plus-or-Minus (understruck plus: _+) */
	 014, M( 0, 9), D( 4, 9), M( 2,11), D( 2, 7),
	      M( 0, 7), D( 4, 7),
	      LEAD, STOP,

/* Target (Circle-around-Crosshairs) */
	 015, M( 0, 8), D( 0, 7), D( 1, 6), D( 3, 6),
	      D( 4, 7), D( 4, 9), D( 3,10), D( 1,10),
	      D( 0, 9), D( 0, 8), D( 4, 8), M( 2,10), D( 2, 6),
	      LEAD, STOP,

/* Infinity Symbol */
	 016, M( 0,10), D( 1, 9), D( 2, 9), D( 4,11),
	      D( 5,11), D( 6,10), D( 5, 9), D( 4, 9),
	      D( 2,11), D( 1,11), D( 0,10),
	      LEAD, STOP,

/* Sigma (small) */
	 017, M( 4, 8), D( 3, 9), D( 1, 9), D( 0, 8),
	      D( 0, 7), D( 1, 6), D( 3, 6), D( 4, 7),
	      D( 4,10), D( 2,12), D( 1,12),
	      LEAD, STOP,

/* "Contains" (~C) */
	 020, M( 3,11), D( 1,11), D( 0,10), D( 0, 8),
	      D( 1, 7), D( 3, 7),
	      LEAD, STOP,

/* "Is Contained By" (~backward C) */
	 021, M( 0,11), D( 2,11), D( 3,10), D( 3, 8),
	      D( 2, 7), D( 0, 7),
	      LEAD, STOP,

/* "Union" (~U) */
	 022, M( 0,10), D( 0, 8), D( 1, 7), D( 3, 7),
	      D( 4, 8), D( 4,10),
	      LEAD, STOP,

/* "Intersection" (~upside-down U) */
	 023, M( 0, 7), D( 0, 9), D( 1,10), D( 3,10),
	      D( 4, 9), D( 4, 7),
	      LEAD, STOP,

/* "For All" (V-) */
	 024, M( 0,14), D( 3, 6), D( 6,14), M( 1,11), D( 5,11),
	      LEAD, STOP,

/* "There exists" (Backwards "E") */
	 025, D( 6, 6), D( 6,14), D( 0,14), M( 2,10), D( 6,10),
	      LEAD, STOP,

/* Modified Target (Circle-with-Crosshairs on 45 degrees) */
	 026, D( 4,10), M( 0,10), D( 4, 6), M( 3, 6),
	      D( 1, 6), D( 0, 7), D( 0, 9), D( 1,10),
	      D( 3,10), D( 4, 9), D( 4, 7), D( 3, 6),
	      LEAD, STOP,

/* Two-Headed Arrow (pts both left and right) */
	 027, M( 2, 8), D( 0,10), D( 2,12), M( 0,10),
	      D( 6,10), M( 4,12), D( 6,10), D( 4, 8),
	      LEAD, STOP,

/* underscore (zero-width) */
	 030, M( 0, 5), D( 4, 5),
	      UNLEAD, STOP,

/* right arrow */
	 031, M( 0,10), D( 6,10), M( 3,13), D( 6,10), D( 3, 7),
	      LEAD, STOP,

/* carat '^' (zero-width) */
	 032, M( 2,16), D( 3,17), D( 4,16),
	      UNLEAD, STOP,

/* not-equal (slashed equal sign: =/) */
	 033, D( 6,12), M( 0,10), D( 6,10), M( 0, 8), D( 6, 8),
	      LEAD, STOP,

/* less-than-or-equal (underscored less-than: _<) */
	 034, D( 3, 6), M( 3, 7), D( 0,10), D( 3,13),
	      LEAD, STOP,

/* greater-than-or-equal (underscored greater-than: _>) */
	 035, D( 3, 6), M( 0, 7), D( 3,10), D( 0,13),
	      LEAD, STOP,

/* identically-equal-to (triple-bar '=') */
	 036, M( 0, 7), D( 6, 7), M( 6, 9), D( 0, 9),
	      M( 0,11), D( 6,11),
	      LEAD, STOP,

/* (inverted carat, or super-script 'v') */
	 037, M( 0,11), D( 3, 8), D( 6,11),
	      LEAD, STOP,

/* [040]- blank space */
	 040, LEAD, STOP,

/* [041]- exclamation (!)*/
	 041, M( 2, 6), D( 2, 7), M( 2, 8), D( 2,13),
	      LEAD, STOP,

/* [042]- Double-Quote (")*/
	 042, M( 2,12), D( 2,14), M( 4,14), D( 4,12),
	      LEAD, STOP,

/* [043]- sharp (#) */
	 043, M( 2, 7), D( 2,13), M( 4,13), D( 4, 7),
	      M( 6, 9), D( 0, 9), M( 0,11), D( 6,11),
	      LEAD, STOP,

/* [044]- dollar sign ($) */
	 044, M( 0, 8), D( 2, 6), D( 4, 6), D( 6, 8),
	      D( 4,10), D( 2,10), D( 0,12), D( 2,14),
	      D( 4,14), D( 6,12), M( 4,14), D( 4, 6),
	      M( 2, 6), D( 2,14),
	      LEAD, STOP,

/* [045]- percent: '%' */
	 045, D( 6,12), D( 1,12), D( 0,11), D( 0,10),
	      D( 1, 9), D( 2, 9), D( 3,10), D( 3,11),
	      D( 2,12), M( 4, 9), D( 3, 8), D( 3, 7),
	      D( 4, 6), D( 5, 6), D( 6, 7), D( 6, 8),
	      D( 5, 9), D( 4, 9),
	      LEAD, STOP,

/* [046]- Ampersand: '&' */
	 046, M( 6, 6), D( 1,11), D( 1,13), D( 2,14),
	      D( 3,14), D( 4,13), D( 0, 9), D( 0, 7),
	      D( 1, 6), D( 3, 6), D( 5, 8),
	      LEAD, STOP,

/* [047]- rt single quote (apostrophe: ') */
	 047, M( 2,12), D( 4,14),
	      LEAD, STOP,

/* [050]- '(' */
	 050, M( 2, 6), D( 0, 8), D( 0,12), D( 2,14),
	      LEAD, STOP,

/* [051]- ')' */
	 051, D( 2, 8), D( 2,12), D( 0,14),
	      LEAD, STOP,

/* [052]- asterisk: '*' */
	 052, M( 0, 8), D( 4,12), M( 2,12), D( 2, 8),
	      M( 4, 8), D( 0,12), M( 0,10), D( 4,10),
	      LEAD, STOP,

/* [053]- '+' */
	 053, M( 2, 7), D( 2,11), M( 0, 9), D( 4, 9),
	      LEAD, STOP,

/* [054]- ',' */
	 054, M( 3, 6), D( 3, 7), D( 2, 7), D( 2, 6),
	      D( 3, 6), D( 2, 5),
	      LEAD, STOP,

/* [055]- '-' */
	 055, M( 0, 9), D( 4, 9),
	      LEAD, STOP,

/* [056]- '.' */
	 056, M( 2, 6), D( 3, 6), D( 3, 7), D( 2, 7), D( 2, 6),
	      LEAD, STOP,

/* [057]- slash: '/' */
	 057, D( 6,12),
	      LEAD, STOP,

/* [060]- '0' */
	 060, M( 0, 7), D( 6,13), M( 6,12), D( 4,14),
	      D( 2,14), D( 0,12), D( 0, 8), D( 2, 6),
	      D( 4, 6), D( 6, 8), D( 6,12),
	      LEAD, STOP,

/* [061]- '1' */
	 061, M( 1,12), D( 3,14), D( 3, 6),
	      LEAD, STOP,

/* [062]- '2' */
	 062, M( 0,12), D( 0,13), D( 1,14), D( 5,14),
	      D( 6,13), D( 6,11), D( 0, 7), D( 0, 6), D( 6, 6),
	      LEAD, STOP,

/* [063]- '3' */
	 063, M( 0,13), D( 1,14), D( 5,14), D( 6,13),
	      D( 6,12), D( 4,10), D( 6, 8), D( 6, 7),
	      D( 5, 6), D( 1, 6), D( 0, 7),
	      LEAD, STOP,

/* [064]- '4' */
	 064, M( 5, 6), D( 5,14), D( 0, 9), D( 6, 9),
	      LEAD, STOP,

/* [065]- '5' */
	 065, M( 0, 7), D( 1, 6), D( 4, 6), D( 6, 8),
	      D( 6, 9), D( 5,10), D( 1,10), D( 0, 9),
	      D( 0,14), D( 6,14),
	      LEAD, STOP,

/* [066]- '6' */
	 066, M( 0, 9), D( 1,10), D( 5,10), D( 6, 9),
	      D( 6, 7), D( 5, 6), D( 1, 6), D( 0, 7),
	      D( 0,10), D( 4,14),
	      LEAD, STOP,

/* [067]- '7' */
	 067, M( 0,14), D( 6,14), D( 1, 6),
	      LEAD, STOP,

/* [070]- '8' */
	 070, M( 1,10), D( 0, 9), D( 0, 7), D( 1, 6),
	      D( 5, 6), D( 6, 7), D( 6, 9), D( 5,10),
	      D( 6,11), D( 6,13), D( 5,14), D( 1,14),
	      D( 0,13), D( 0,11), D( 1,10), D( 5,10),
	      LEAD, STOP,

/* [071]- '9' */
	 071, M( 2, 6), D( 6,10), D( 6,13), D( 5,14),
	      D( 1,14), D( 0,13), D( 0,11), D( 1,10),
	      D( 5,10), D( 6,11),
	      LEAD, STOP,

/* [072]- ':' */
	 072, M( 2, 6), D( 3, 6), D( 3, 7), D( 2, 7),
	      D( 2, 6), M( 2,10), D( 3,10), D( 3,11),
	      D( 2,11), D( 2,10),
	      LEAD, STOP,

/* [073]- ';' */
	 073, M( 3, 6), D( 3, 7), D( 2, 7), D( 2, 6),
	      D( 3, 6), D( 2, 5), M( 2,10), D( 3,10),
	      D( 3,11), D( 2,11), D( 2,10),
	      LEAD, STOP,

/* [074]- '<' */
	 074, M( 3, 7), D( 0,10), D( 3,13),
	      LEAD, STOP,

/* [075]- Equals: '=' */
	 075, M( 0, 8), D( 6, 8), M( 6,10), D( 0,10),
	      LEAD, STOP,

/* [076]- '>' */
	 076, M( 0, 7), D( 3,10), D( 0,13),
	      LEAD, STOP,

/* [077]- '?' */
	 077, M( 1,13), D( 2,14), D( 4,14), D( 5,13),
	      D( 5,12), D( 3,10), D( 3, 8), M( 3, 7), D( 3, 6),
	      LEAD, STOP,

/* [0100]- '@' */
	0100, M( 1, 6), D( 0, 7), D( 0,11), D( 1,12),
	      D( 5,12), D( 6,11), D( 6, 8), D( 5, 7),
	      D( 4, 8), D( 4,11), M( 4,10), D( 3,11),
	      D( 2,11), D( 1,10), D( 1, 9), D( 2, 8),
	      D( 3, 8), D( 4, 9),
	      LEAD, STOP,

/* [0101]- 'A' */
	0101, D( 3,14), D( 6, 6), M( 1, 9), D( 5, 9),
	      LEAD, STOP,

/* [0102]- 'B' */
	0102, D( 0,14), D( 5,14), D( 6,13), D( 6,11),
	      D( 5,10), D( 0,10), M( 5,10), D( 6, 9),
	      D( 6, 7), D( 5, 6), D( 0, 6),
	      LEAD, STOP,

/* [0103]- 'C' */
	0103, M( 6,13), D( 5,14), D( 2,14), D( 0,12),
	      D( 0, 8), D( 2, 6), D( 5, 6), D( 6, 7),
	      LEAD, STOP,

/* [0104]- 'D' */
	0104, D( 0,14), D( 4,14), D( 6,12), D( 6, 8),
	      D( 4, 6), D( 0, 6),
	      LEAD, STOP,

/* [0105]- 'E' */
	0105, D( 0,14), D( 6,14), M( 4,10), D( 0,10),
	      M( 0, 6), D( 6, 6),
	      LEAD, STOP,

/* [0106]- 'F' */
	0106, D( 0,14), D( 6,14), M( 4,10), D( 0,10),
	      LEAD, STOP,

/* [0107]- 'G' */
	0107, M( 6,13), D( 5,14), D( 2,14), D( 0,12),
	      D( 0, 8), D( 2, 6), D( 4, 6), D( 6, 8),
	      D( 6,10), D( 4,10),
	      LEAD, STOP,

/* [0110]- 'H' */
	0110, D( 0,14), M( 6,14), D( 6, 6), M( 0,10), D( 6,10),
	      LEAD, STOP,

/* [0111]- 'I' */
	0111, M( 1, 6), D( 5, 6), M( 3, 6), D( 3,14),
	      M( 1,14), D( 5,14),
	      LEAD, STOP,

/* [0112]- 'J' */
	0112, M( 1, 7), D( 2, 6), D( 3, 6), D( 4, 7),
	      D( 4,14), M( 2,14), D( 6,14),
	      LEAD, STOP,

/* [0113]- 'K' */
	0113, M( 1, 6), D( 1,14), M( 1,10), D( 5,14),
	      M( 1,10), D( 5, 6),
	      LEAD, STOP,

/* [0114]- 'L' */
	0114, D( 0,14), M( 0, 6), D( 6, 6),
	      LEAD, STOP,

/* [0115]- 'M' */
	0115, D( 0,14), D( 3,11), D( 6,14), D( 6, 6),
	      LEAD, STOP,

/* [0116]- 'N' */
	0116, D( 0,14), D( 6, 6), D( 6,14),
	      LEAD, STOP,

/* [0117]- 'O' */
	0117, M( 0, 8), D( 0,12), D( 2,14), D( 4,14),
	      D( 6,12), D( 6, 8), D( 4, 6), D( 2, 6), D( 0, 8),
	      LEAD, STOP,

/* [0120]- 'P' */
	0120, D( 0,14), D( 5,14), D( 6,13), D( 6,11),
	      D( 5,10), D( 0,10),
	      LEAD, STOP,

/* [0121]- 'Q' */
	0121, M( 0, 8), D( 0,12), D( 2,14), D( 4,14),
	      D( 6,12), D( 6, 8), D( 4, 6), D( 2, 6),
	      D( 0, 8), M( 4, 8), D( 6, 6),
	      LEAD, STOP,

/* [0122]- 'R' */
	0122, D( 0,14), D( 5,14), D( 6,13), D( 6,11),
	      D( 5,10), D( 0,10), M( 2,10), D( 6, 6),
	      LEAD, STOP,

/* [0123]- 'S' */
	0123, M( 0, 8), D( 2, 6), D( 4, 6), D( 6, 8),
	      D( 4,10), D( 2,10), D( 0,12), D( 2,14),
	      D( 4,14), D( 6,12),
	      LEAD, STOP,

/* [0124]- 'T' */
	0124, M( 3, 6), D( 3,14), M( 0,14), D( 6,14),
	      LEAD, STOP,

/* [0125]- 'U' */
	0125, M( 0,14), D( 0, 7), D( 1, 6), D( 5, 6),
	      D( 6, 7), D( 6,14),
	      LEAD, STOP,

/* [0126]- 'V' */
	0126, M( 0,14), D( 3, 6), D( 6,14),
	      LEAD, STOP,

/* [0127]- 'W' */
	0127, M( 0,14), D( 0, 6), D( 3, 9), D( 6, 6), D( 6,14),
	      LEAD, STOP,

/* [0130]- 'X' */
	0130, M( 6,14), D( 0, 6), M( 0,14), D( 6, 6),
	      LEAD, STOP,

/* [0131]- 'Y' */
	0131, M( 0,14), D( 3,11), D( 6,14), M( 3,11), D( 3, 6),
	      LEAD, STOP,

/* [0132]- 'Z' */
	0132, M( 0,14), D( 6,14), D( 0, 6), D( 6, 6),
	      LEAD, STOP,

/* [0133]- '[' */
	0133, M( 3, 4), D( 0, 4), D( 0,14), D( 3,14),
	      LEAD, STOP,

/* [0134]- '\' */
	0134, M( 0,12), D( 6, 6),
	      LEAD, STOP,

/* [0135]- ']' */
	0135, M( 0, 4), D( 3, 4), D( 3,14), D( 0,14),
	      LEAD, STOP,

/* [0136]- up-arrow '^' */
	0136, M( 0,11), D( 3,14), D( 6,11), M( 3,14), D( 3, 6),
	      LEAD, STOP,

/* [0137]- underline '_' */
	0137, D( 7, 6),
	      LEAD, STOP,

/* [0140]- '`' */
	0140, M( 2,14), D( 4,12),
	      LEAD, STOP,

/* [0141]- 'a' */
	0141, M( 4, 9), D( 3,10), D( 1,10), D( 0, 9),
	      D( 0, 7), D( 1, 6), D( 3, 6), D( 4, 7),
	      M( 4, 6), D( 4, 10),
	      LEAD, STOP,

/* [0142]- 'b' */
	0142, D( 0,13), M( 0, 9), D( 1,10), D( 3,10),
	      D( 4, 9), D( 4, 7), D( 3, 6), D( 1, 6), D( 0, 7),
	      LEAD, STOP,

/* [0143]- 'c' */
	0143, M( 4, 9), D( 3,10), D( 1,10), D( 0, 9),
	      D( 0, 7), D( 1, 6), D( 3, 6), D( 4, 7),
	      LEAD, STOP,

/* [0144]- 'd' */
	0144, M( 0, 7), D( 0, 9), D( 1,10), D( 3,10),
	      D( 4, 9), D( 4, 7), D( 3, 6), D( 1, 6),
	      D( 0, 7), M( 4, 6), D( 4,13),
	      LEAD, STOP,

/* [0145]- 'e' */
	0145, M( 4, 7), D( 3, 6), D( 1, 6), D( 0, 7),
	      D( 0, 9), D( 1,10), D( 3,10), D( 4, 9),
	      D( 4, 8), D( 0, 8),
	      LEAD, STOP,

/* [0146]- 'f' */
	0146, M( 2, 6), D( 2,12), D( 3,13), D( 4,13),
	      D( 5,12), M( 0,11), D( 4,11),
	      LEAD, STOP,

/* [0147]- 'g' */
	0147, M( 4, 9), D( 3,10), D( 1,10), D( 0, 9),
	      D( 0, 7), D( 1, 6), D( 3, 6), D( 4, 7),
	      M( 4,10), D( 4, 5), D( 3, 4), D( 1, 4), D( 0, 5),
	      LEAD, STOP,

/* [0150]- 'h' */
	0150, D( 0,13), M( 0, 9), D( 1,10), D( 3,10),
	      D( 4, 9), D( 4, 6),
	      LEAD, STOP,

/* [0151]- 'i' */
	0151, M( 3,12), D( 3,11), M( 3,10), D( 3, 7),
	      /*D( 4, 6), D( 5, 6),*/
	      D( 3,6),
	      LEAD, STOP,

/* [0152]- 'j' */
	0152, M( 3,12), D( 3,11), M( 3,10), D( 3, 5),
	      D( 2, 4), D( 1, 4),
	      LEAD, STOP,

/* [0153]- 'k' */
	0153, D( 0,13), M( 0, 8), D( 2,10), M( 0, 8), D( 2, 6),
	      LEAD, STOP,

/* [0154]- 'l' */
	0154, M( 2, 6), D( 2,13),
	      LEAD, STOP,

/* [0155]- 'm' */
	0155, D( 0,10), M( 0, 9), D( 1,10), D( 2,10),
	      D( 3, 9), D( 3, 6), M( 3, 9), D( 4,10),
	      D( 5,10), D( 6, 9), D( 6, 6),
	      LEAD, STOP,

/* [0156]- 'n' */
	0156, D( 0,10), M( 0, 9), D( 1,10), D( 2,10),
	      D( 3, 9), D( 3, 6),
	      LEAD, STOP,

/* [0157]- 'o' */
	0157, M( 0, 7), D( 0, 9), D( 1,10), D( 3,10),
	      D( 4, 9), D( 4, 7), D( 3, 6), D( 1, 6), D( 0, 7),
	      LEAD, STOP,

/* [0160]- 'p' */
	0160, M( 0, 4), D( 0,10), M( 0, 9), D( 1,10),
	      D( 3,10), D( 4, 9), D( 4, 7), D( 3, 6),
	      D( 1, 6), D( 0, 7),
	      LEAD, STOP,

/* [0161]- 'q' */
	0161, M( 4, 9), D( 3,10), D( 1,10), D( 0, 9),
	      D( 0, 7), D( 1, 6), D( 3, 6), D( 4, 7),
	      M( 4,10), D( 4, 4),
	      LEAD, STOP,

/* [0162]- 'r' */
	0162, D( 0,10), M( 0, 9), D( 1,10), D( 3,10), D( 4, 9),
	      LEAD, STOP,

/* [0163]- 's' */
	0163, M( 0, 7), D( 1, 6), D( 3, 6), D( 4, 7),
	      D( 3, 8), D( 1, 8), D( 0, 9), D( 1,10),
	      D( 3,10), D( 4, 9),
	      LEAD, STOP,

/* [0164]- 't' */
	0164, M( 2,13), D( 2, 7), D( 3, 6), D( 4, 6),
	      D( 5, 7), M( 1,11), D( 3,11),
	      LEAD, STOP,

/* [0165]- 'u' */
	0165, M( 0,10), D( 0, 7), D( 1, 6), D( 3, 6),
	      D( 4, 7), D( 4,10), D( 4, 6),
	      LEAD, STOP,

/* [0166]- 'v' */
	0166, M( 0,10), D( 2, 6), D( 4,10),
	      LEAD, STOP,

/* [0167]- 'w' */
	0167, M( 0,10), D( 0, 6), D( 2, 8), D( 4, 6), D( 4,10),
	      LEAD, STOP,

/* [0170]- 'x' */
	0170, D( 4,10), M( 0,10), D( 4, 6),
	      LEAD, STOP,

/* [0171]- 'y' */
	0171, M( 0,10), D( 2, 6), M( 4,10), D( 1, 4), D( 0, 4),
	      LEAD, STOP,

/* [0172]- 'z' */
	0172, M( 0,10), D( 4,10), D( 0, 6), D( 4, 6),
	      LEAD, STOP,

/* [0173]- '{' */
	0173, M( 3,14), D( 2,13), D( 2,11), D( 0, 9),
	      D( 2, 7), D( 2, 5), D( 3, 4),
	      LEAD, STOP,

/* [0174]- '|' */
	0174, M( 2, 4), D( 2,14),
	      LEAD, STOP,

/* [0175]- '}' */
	0175, M( 0,14), D( 1,13), D( 1,11), D( 3, 9),
	      D( 1, 7), D( 1, 5), D( 0, 4),
	      LEAD, STOP,

/* [0176]- '~' (zero-width) */
	0176, M( 2,16), D( 3,17), D( 4,16), D( 5,17),
	      UNLEAD, STOP,

/* [0177]- (elevated NOT sign) (zero-width) */
	0177, M( 1,16), D( 4,16), D( 4,15),
	      UNLEAD, STOP,

/* end of list */
	STOP
} ;
