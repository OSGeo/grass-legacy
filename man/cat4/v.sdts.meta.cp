NAME





v.sdts.meta.cp
 -
installs
supplementary
metadata
file
preparatory
to
creation
of
an
SDTS
export
dataset.
(GRASS
Vector
Data
Export/Processing
Program)
SYNOPSIS





v.sdts.meta.cp
v.sdts.meta.cp
help
v.sdts.meta.cp
[-f]
metafile=name
map=name
DESCRIPTION





The
program
provides
assistance
for
the
preparation
of
supplemental
metadata
for
an
SDTS
export
dataset.
The
user
specifies
a
map
layer
in
his
current
mapset,
and
then
a
file
of
metadata
information
pertaining
to
the
named
map.
The
program
copies
the
specified
metadata
file
to
a
standard
location
(in
the
user's
mapset,
under
the
dig_misc
directory).
Later,
when
v.out.sdts
is
run
for
the
same
map
layer,
the
items
in
the
metadata
file
will
be
incorporated
in
various
places
in
the
export
dataset.
While
the
metadata
file
can
be
prepared
by
the
user,
it's
format
and
contents
are
strictly
defined.
An
alternative
is
to
use
the
interface
program,
v.sdts.meta,
with
which
the
user
can
prepare
and
install
a
correctly
formatted
metadata
file.
COMMAND
LINE
OPTIONS





Flags:
-f		force
		overwriting
		of
		pre-
		exisiting
		metadata
		file.
Parameters:
map=name	name
		of
		vector
		map
		layer
		to
		which
		the
		specified
		metadata
		file
		applies.
metafile=name	name
		of
		file
		to
		be
		installed
		as
		a
		metadata
		file
		for
		the
		specified
		map.
NOTES





The
format
of
the
metadata
source
file
is
rather
highly
specified.
Following
is
a
list
of
the
items
that
can
be
included
in
the
file.
Note
that
each
item
is
preceded
by
a
particular
code.
The
code,
and
the
following
':'
must
be
entered
intact
for
each
included
metadata
item.
The
colon
is
then
immediately
followed
by
the
user-
supplied
information:

IDEN_MPDT:(creation
date
of
original
source
map;
YYYY
or
YYMMDD
format)
IDEN_TITL:(general
title
for
contents
of
transfer,
for
TITL
field
in
IDEN
module.
If
not
specified,
vector
header
"map_name"
will
be
used)
IDEN_COMT:(general
comment
on
transfer,
for
IDEN
module's
COMT
field)
XREF_HDAT:(name
of
geodetic
datum
to
which
export
data
are
referenced,
for
HDAT
fieid
in
XREF
module.)
DDSH_ENT_NAME:(for
Dictionary/Schema
module;
name
of
kind
of
entity
that
dig_att
and
dig_cats
values
represent.
If
not
specified,
map
name
will
be
used.)
DDDF_GRASS_ENT:(definition
for
entity
in
"DDSH_ENT_NAME",
for
Dictionary/
Definition
module.
if
not
supplied,
simple
default
is
used.)
DDDF_ATTR_NUM:(definition
for
dig_att
values,
for
Dictionary/Definition
module;
if
not
specified,
simple
default
is
used.)
DDDF_ATTR_LABEL:(definition
for
dig_cats
values,
for
Dictionary/Definition
module;
if
not
specified,
simple
default
is
used.)
As
noted
above,
the
metadata
file
can
be
prepared,
and
installed,
without
the
user
having
to
worry
about
format
details,
with
v.sdts.meta.

SEE
ALSO





The
GRASS-
SDTS
User
Guide
v.sdts.meta,
v.sdts.dq.cp,
v.out.sdts,
v.in.sdts.
AUTHORS





David
Stigberg,
U.S.
Army
Construction
Engineering
Research
Laboratory
Tin
Qian,
University
of
Illinois

NOTICE





This
program
is
part
of
the
contrib
section
of
the
GRASS
distribution.
As
such,
it
is
externally
contributed
code
that
has
not
been
examined
or
tested
by
the
Office
of
GRASS
Integration.

































