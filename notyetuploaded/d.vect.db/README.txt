d.vect.db specification

The  new  proposed program d.what.db will perform following func­
tions: ll.  Editing attributes for map's objects  (composite  and
primitive).   Creating  composite objects, adding/deleting compo­
nents.  Creating new Themes for the map.

The idea behind d.vect.db is to get complete tool  working  which
will  allow  editing  DBMS  tables and composite tables for GRASS
vector maps.  I will demonstrate that after such tool is complet­
ed,  it will be very easy to complete the d.rast.db (similar tool
for raster maps) and after the digitizer is  chosen,  to  provide
the  digitizer with ability to edit attributes and to create/edit
composite objects.

The program will present user with a menu: to edit attributes  or
to  create/edit  composite  object.   he  will be asked to choose
Theme (Theme Selection widget) If he chose to create/edit compos­
ite  object,  the  theme can be cosen to be "all" And if the user
chooses to create new composite object, he will be asked for  new
cid  and  presented with Composite Widget.  Otherwise for editing
attributes, and editing existing Composite  objects  he  he  will
have  to  choose the object to edit with a mouse.  (see Mouse_Se­
lect_ID())

function needed: does fid or cid appear in the map?  Edit Widget

Arguments: name, mapset, id, theme

(Please refer to the enclosed picture sheet)

The purpose of the edit Widget is to provide interactive menu  to
edit  category  description  and  table attributes and to jump to
other attribute tables connected with the current table by refer­
ence  edges by clicking on foreign/primary key buttons.  if there
are more than one table with reference edge to primary  key  then
present  a  scrolled widget to let user choose which one to edit.
If the key is a primary key and there is more than one record  in
the table connected with ref.edge which matches this key, present
user with a scrolled widget showing all the rows in the table and
letting  user  chose one row(record) to edit.  (in which case the
Edit Widget for the chosen table will pop up)

Also if the object is composite, Edit widget has a push-button to
edit the composition (i.e add/delete components) of composite ob­
ject. If button is pressed, Composite Widget for this object pops
up  if not already there.  (we keep the list of (id, theme)'s for
which Composite and Edit widgets are realized)

QUESTION: Do the reference edges have direction? i.e. is one  key
foreign  and  another primary (and I should treat the later as an
attribute, or can I click on first key to go to the second  table
and then click on the second key in the second window to go back?

NOTE: If edges are not directed, check if the widget we are about
to  pop  up  is  already realized, if it is, just move cursor and
keybord control there.

NOTE: when adding new id, don't forget to add it to the  list  of
ids in the theme file.

Composite Widget

Arguments: name, mapset, cid, theme

The  purpose  of  composite  Widget is to choose components which
make up composite object and to invoke Edit Widget  to  edit  at­
tributes for components or for object itself.

There  is  a  button  for  each  component and when the button is
pressed the objects with this id are highlighted on  the  monitor
and the the Edit Widget pops up.  (When the cosen theme is "all",
all the components are displayed for each theme and  (id,  theme)
is shown on each component button.  This way before the Edit wid­
get pops up, the theme for the object being edited  is  set.   If
specific  theme is chosen, only components paired with this theme
and components with no theme specified are shown.

When "Add/Delete Components button is  pressed,  programs  enters
the  loop  in which User is asked to click on the component. (See
Mouse_Select_ID() function) and then asks to  delete  or  to  add
chosen  component  and  if theme is "all" and component chosen is
primitive, asks if the theme should be restricted for this compo­
nent (Theme Selection Widget).

Or should we make 2 buttons for each component: add and delete?

There  is  also  a button to edit attributes for composite object
itself.  When theme selected is "all", this button should be many
buttons  (1 for each theme) because cid doesn't need to be paired
with theme.  When one of the buttons id pressed, If  Edit  widget
for  this  object  and  chosen  theme is realized, the control is
transfered there, if, not pop up the new one.  (we keep the  list
of  (id,  theme)'s for which Composite and Edit widgets are real­
ized)

Needed function: List all the (id, theme) pairs for primitive ob­
jects  and  all cids for composite objects particibating in given
cid.  (for this composite file needs to be opened,  and  all  the
entries  for  given cid read. If some fid's appear withour theme,
add (fid,theme) for all existing themes for which fid is  in  the
list of theme's ids, to the list.

NOTE:  when fid's theme is not specified, but (fid,theme) was re­
moved for one of the themes, write into the list (fid,theme1) for
all  the  theme1's  which contain this fid if this theme actually
contained this fid; and don't change anything if  the  theme  did
not contain this fid..

Theme  Selection  Widget  This widget is not drawn, it is easy to
imagine.  It will present user with a srollable list of available
themes and ask to make selection. If selected theme is not in the
list, it will present a scrollable list of available  Data  Bases
to  choose  from  and also pop up the menu to ask for other theme
parameters: i.e. name, number of columns   in  the  liink  table,
reference  edges  to  other  data  base  tables (here the list of
available database will pop up to help choose dbms for  a  refer­
ence  edge  but  the  table name has to be entered by user on his
own, and we have to check if it exists) No list of associated ids
for the new Theme is created at this point.

Needed  API functions: List available themes for map, List avail­
able Data Bases, Check if the table  exists  in  the  given  Data
Base.

Error/Message  Widget  Just  a message widget with "OK" button is
it's an error, "Dismiss " button is it is a message.

Mouse_Select_ID () function

int Mouse_Collect_ID(Map)
   struct Map_info *Map;

Presents user with a message:  center  tab(;);  ll.   Right  But­
ton:,select  Left  Button:,done  Center Button:,Search for parent
composite object

When Right button is clicked, the object is  highlighted  and  if
done  function is pressed immediately afterwards, id is returned.

If Center button is pressed, id of the object is found, and  then
get_cids(list, id) is called to get a list of all cid's for which
id is one of the components for current theme, and the first such
composite object is highleghted.

Then  the  new  message  pops  up: center tab(;); ll.  Right But­
ton:,Show next composite in  the  list  Left  Button:,Select/Done
Center Button:,Search for parent composite object

This is another mode of the loop where user can view all the com­
posite objects on this level by clicking the right  button  until
Left Button is pressed (at which point chosen cid is returned) or
until Center button calls get_cids(list, id, theme) for the  cur­
rent cid and the same is repeated for the next level of composite
objects.

So in order to select a primitive object, the user  just  has  to
click  on  it  with  the right button and then click left button:
done.

If he wants to select composite object, he has  to  click  Center
button  on  the  primitive component, and then click Right button
until needed composite object is highlighted, or even go  further
up  the  tree  by  clicking center button again and search on the
higher level, until the right composite object is found, and then
press the Left button is pressed to make a selection.

PROBLEM1:  Right now there is no function to find the closest ob­
ject: There are only functions to find closest site, closest area
or closest line. They will need to be modified to return the dis­
tance, so that the 3 distances can be compared,  and  the  object
with smallest distance chosen.

PROBLEM2: Suppose more than one object in the map has category n.
Then in Mouse_Select_ID() do I highlight all  the  objectts  with
such category number or only the one on which we clicked?

Need  to  write Highlight(Map, id) function  which highlights all
objects with cat=id/2.

Need to add ordered array into Map_Info which stores list of  ob­
jects for each category value.

function  needed: get_cids(name, mapset, list, id, theme) Returns
all composite objects which contain either id, or (id, theme) and
return thenm in the list.

Things  to  do  when  hooking up to digitizer Only Highlite() and
Mouse_Select_ID() need to be modified.

Things to do when  writing  d.rast.db  Highlite()  and  Mouse_Se­
lect_ID()  need to be modified.  Function which checks if fid ap­
pears in the map needs to be modified, For  floating  point  maps
fid is not (cat+1)*2 , but (category index + 1) * 2 where catego­
ry index is  the  number  of  fp  range  in  Category  file;  Use
G_set_quant_rules(fd,  &cats.q);  and  then G_get_c_map_row() and
the cell value will be category indexes.

Summary of needed additional functions  function  determining  if
fid or cid appears in given map.

Highlight(Map,  id)  function   which highlights all objects with
cat=id/2.

get_cids(name, mapset, list, id, theme) Returns all composite ob­
jects which contain either id, or (id, theme) and return thenm in
the list.

List available themes for  map.

List available  Data Bases.

Check if the table exists in the given Data Base.


DE

