/*  @(#)menus.h	1.1  5/4/87  */
#define MAIN_MENU		0
#define PLOT_MENU		1
#define NODE_MENU		2
#define LINE_MENU		3
#define AREA_MENU		4
#define LABEL_MENU		5

#define N_MENU_LINES	14

static char *menus[6][N_MENU_LINES] = {
/*	"<--Room for text-->" */
	{
	"MAIN MENU  (DLG LABEL)",
	"  ",
	"  P - Plot (menu)",
	"  L - Label (menu)",
	"  ",
	"  ",
	"  ",						/**   "  n - Node (menu)",  **/
	"  ",						/**   "  l - Line (menu)",  **/
	"  ",						/**   "  a - Area (menu)",  **/
	"  ",
	"  R - Report any errors",
	"  W - Write out this session",
	"  * - Replot text",
	"  Q - Quit" } ,
	{
	"PLOT MENU",
	"_ ",
	"  a - Show area centroids",
	"  l - Show lines",
	"_ n - Show nodes",
	"  A - Show area labels",
	"_ L - Show labeled lines ",
	"  W - Define new window",
	"  P - Previous window",
	"_ C - Clear window",
	"  w - Where am I",
	"  s - Display scale",
	"_ * - Replot Text",
	"  q - Return to Main Menu" } ,
	{
	"EDIT-NODE",
	"  ",
	"  m - Move node",
	"  s - Split node",
	"  c - Combine 2+ nodes",
	"  r - Remove node",
	"  e - Edit category codes",
	"  ",
	"  ",
	"  ",
	"  ",
	"  ",
	"  * - Replot Text",
	"  q - Return to Main Menu" } ,
	{
	"EDIT-LINE",
	"  ",
	"  n - Change nodes",
	"  a - Change areas",
	"  s - Split line",
	"  r - Redraw line",
	"  x - Remove line",
	"  e - Edit category codes",
	"  ",
	"  ",
	"  ",
	"  ",
	"  * - Replot Text",
	"  q - Return to Main Menu" } ,
	{
	"EDIT-AREA",
	"  ",
	"  l - Link lines around area",
	"  i - Inspect perimeter lines",
	"  m - Move area center",
	"  e - Edit category codes",
	"  ",
	"  ",
	"  ",
	"  ",
	"  ",
	"  ",
	"  * - Replot Text",
	"  q - Return to Main Menu" } ,
	{
	"LABEL MENU",
	" _",
	"  l - label lines",
	" _a - label areas",
	"  L - unlabel lines",
	"  A - unlabel areas",
	"  ",
	"  s - show lines of cat. #",
	"  w - Where am I",
	"  ",
	"  ",
	"  ",
	"  * - Replot Text",
	"  q - Return to Main Menu" } ,
} ;
