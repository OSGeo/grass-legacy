NAME





v.sdts.dq.cp
 -
installs
SDTS
data
quality
reports.
(GRASS
Vector
Data
Export/Processing
Program)
SYNOPSIS





v.sdts.dq.cp
v.sdts.dq.cp
help
v.sdts.dq.cp
[-f]
map=name
[H=name]
[PA=name]
[AA=name]
[LC=name]
[CG=name]
DESCRIPTION





The
program
provides
assistance
for
the
preparation
of
the
five
data
quality
report
modules
(Lineage,
Positional
Accuracy,
Attribute
Accuracy,
Logical
Consistency,
and
Completeness)
required
in
an
SDTS
transfer
dataset.
The
program
has
one
simple
function:
the
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
one
or
more
files
to
be
used
for
SDTS
data
quality
reports
for
this
map
layer.
The
program
copies
the
specified
files
to
a
standard
location
(in
the
user's
current
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
data
quality
reports
will
be
incorporated
into
an
SDTS
export
dataset.

COMMAND
LINE
OPTIONS





-f		force
		overwriting
		of
		pre-
		exisiting
		data
		quality
		report(s).
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
		data
		quality
		report
		files
		apply.
		HL=name
		name
		of
		file
		to
		be
		used
		for
		the
		SDTS
		Lineage
		(HL)
		data
		quality
		report.
		PA=name
		name
		of
		file
		to
		be
		used
		for
		the
		SDTS
		Positional
		Accuracy
		(PA)
		data
		quality
		report.
		AA=name
		name
		of
		file
		to
		be
		used
		for
		the
		SDTS
		Attribute
		Accuracy
		(AA)
		data
		quality
		report.
		LC=name
		name
		of
		file
		to
		be
		used
		for
		the
		SDTS
		Logical
		Consistency
		(LC)
		data
		quality
		report.
		CG=name
		name
		of
		file
		to
		be
		used
		for
		the
		SDTS
		Completeness
		(CG)
		data
		quality
		report.
NOTES





Data
Quality
report
files
should
be
simple
narrative
text
files.
After
the
files
have
been
installed
with
v.sdts.dq.cp,
v.out.sdts
will
convert
the
installed
copy
of
each
report
to
SDTS
ISO
8211
format.
Each
paragraph
in
the
original
file
will
become
a
separate
record
in
the
SDTS
data
quality
module.
Parameter
names--HL,
PA,
AA,
LC,
CG--are
SDTS
codes
for
different
data
quality
modules
(HL=Lineage,
PA=Positional
Accuracy,
etc.).
Data
quality
files
to
be
installed
can
be
created
as
well
as
installed
with
v.sdts.meta.

SEE
ALSO





GRASS-
SDTS
User
Guide
v.sdts.meta,
v.sdts.meta.cp,
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














































