From grass-lists-owner@max.cecer.army.mil Mon Jun 14 15:11:42 1993
Received: from amber.cecer.army.mil by zorro.cecer.army.mil with SMTP id AA03701
  (5.67a/IDA-1.4.4); Mon, 14 Jun 1993 15:11:32 -0500
Received: from zorro.cecer.army.mil by amber.cecer.army.mil (4.1/SMI-4.1)
	id AA17109; Mon, 14 Jun 93 15:07:18 CDT
Received: from amber.cecer.army.mil by zorro.cecer.army.mil with SMTP id AA03480
  (5.67a/IDA-1.4.4 for <grassu-people@amber>); Mon, 14 Jun 1993 15:07:17 -0500
Return-Path: <mccauley@ecn.purdue.edu>
Received: from max.cecer.army.mil by amber.cecer.army.mil (4.1/SMI-4.1)
	id AA17103; Mon, 14 Jun 93 15:07:13 CDT
Received: from cocklebur.ecn.purdue.edu by max.cecer.army.mil with SMTP id AA07132
  (5.67a/IDA-1.5 for <grassu-list@max.cecer.army.mil>); Mon, 14 Jun 1993 15:07:14 -0500
Received: by cocklebur.ecn.purdue.edu (5.65/1.32jrs)
	id AA08631; Mon, 14 Jun 93 15:07:12 -0500
Date: Mon, 14 Jun 93 15:07:12 -0500
Message-Id: <9306142007.AA08631@cocklebur.ecn.purdue.edu>
From: McCauley Technical Services <darrellmy@ids.net>
Sender: mccauley@ecn.purdue.edu
Reply-To: darrellmy@ids.net
Sender: lists-owner@max.cecer.army.mil
Reply-To: grassu-list@max.cecer.army.mil
Precedence: Bulk
To: grassu-list@max.cecer.army.mil
Subject: gnuplot3.4 (g.gnuplot) official release
Status: RO


Dear GRASS Users:
  For those of you writing statistical/mathematical applications for
grass OR for those of you who want alternatives to p.map or
d.linegraph, g.gnuplot, a scientific plotting program specifically
adapted for GRASS, is exactly what you are looking for.

The README file for the package and a few installation notes are
appended.  I'll just highlight what may be of interest to GRASS users.
In addition to plotting several types of XY data, you can also plot:
  surfaces                
  contours               
  surfaces w/ contours on them   
  surfaces w/ contours under them
  vectors files
  sites lists
This latest release of gnuplot provides for hidden line removal.
Additionally, you can label and annotate these plots. 

There are numerous output devices available, including X11, LaTeX,
troff (gpic), PostScript, and ***THE GRASS GRAPHICS MONITOR***. The
latter makes GNUPLOT attractive for
 o  programmers who want their applications to have graphing
    capabilities (e.g., the upcoming release of s.semivar
    (yes, I'm still working, be patient))
 o  user's who would like to display graphs next to their
    other graphic output from GRASS (see d.frame)
So, the GNUPLOT input file that you use to to draw graphs in the GRASS
monitor could be easily modified to produce publication-quality
graphs.  I would envision programmers who incorporate g.gnuplot
in their software providing the option of writing a GNUPLOT input file
(so that the user could write PostScript, METAFONT, or whatever at
a later time).

I have put the following in moon.cecer.army.mil:grass/incoming
   gnuplot3.4.tar.Z - the GNUPLOT source
   g.gnuplot.ps.Z - a short tutorial on g.gnuplot

I would like to thank those who helped with beta testing,
especially Bill at Oakridge for his comments (and 
encouragement).

If, after using this a little, you think that this is something
that should be included in src.related, let someone at OGI 
know.

The fact that the programming required for this little project 
was pretty easy says a lot for OGI, the programming manual,
and the design of GRASS monitor communications (even if it
isn't the best for X). Thanks folks!

James Darrell McCauley                           phone: 317.497.4757
McCauley Technical Services              internet: darrellmy@ids.net
P.O. Box 2485                West Lafayette, Indiana 47906-0485, USA
                  *** GRASS Programmer for Hire ***

-----------
INSTALLATION NOTES:

  % cd gnuplot 
  % ln -s makefile.g Gmakefile
  % gmake4.0 
  or
  % gmake4.1

Then, test it out, perhaps on some of the demo scripts:
  % d.mon start=whatever
  % cd demo
  % ../g.gnuplot all.dem
(a shorter, more interesting demo for GRASS users
may be hidden.dem instead of all.dem)

When you are satisfied, edit Gmakefile and change the lines
that tell gmake4.x to install it with your other GRASS binaries.
That is, change

#$(BIN_MAIN_CMD)/g.gnuplot: $(OFILES) $(DISPLAYLIB) $(RASTERLIB) $(GISLIB) 
g.gnuplot: $(OFILES) $(DISPLAYLIB) $(RASTERLIB) $(GISLIB) 

to

#g.gnuplot: $(OFILES) $(DISPLAYLIB) $(RASTERLIB) $(GISLIB) 
$(BIN_MAIN_CMD)/g.gnuplot: $(OFILES) $(DISPLAYLIB) $(RASTERLIB) $(GISLIB) 

Then, run gmake4.x again.
  % gmake4.1
  % pushd $GISBASE/src/CMD ; MAKELINKS ; popd

This installs the binary. Next step is to install the on-line help:

  % mkdir $(GISBASE)/man/helpp
  % cp docs/gnuplot.gih $(GISBASE)/man/helpp

Now, go to the docs directory and prepare a copy of the manual
for your users. There is a README file in that directory to help you.

Print the g.gnuplot tutorial, and you're done.

--------
0README:

Gnuplot is a command-line driven interactive function plotting utility
for UNIX, MSDOS, and VMS platforms.  The software is copyrighted but
freely distributed (i.e., you don't have to pay for it).  It was
originally intended as graphical program which would allow scientists
and students to visualize mathematical functions and data.  Gnuplot
supports many different types of terminals, plotters, and printers
(including many color devices, and pseudo-devices like LaTeX) and is
easily extensible to include new devices.  [ The "GNU" in gnuplot is
NOT related to the Free Software Foundation, the naming is just a
coincidence (and a long story). Thus gnuplot is not covered by the Gnu
copyleft, but rather by its own copyright statement, included in all
source code files.]

Gnuplot handles both curves (2 dimensions) and surfaces (3
dimensions). Surfaces can be plotted as a mesh fitting the specified
function, floating in the 3-d coordinate space, or as a contour plot
on the x-y plane. For 2-d plots, there are also many plot styles,
including lines, points, lines with points, error bars, and impulses
(crude bar graphs). Graphs may be labeled with arbitrary labels and
arrows, axes labels, a title, date and time, and a key.  The interface
includes command-line editing and history on most platforms.

The new gnuplot user should begin by reading the general information
available by typing `help` after running gnuplot. Then read about the
`plot` command (type `help plot`).  The manual for gnuplot (which is a
nicely formatted version of the on-line help information) can be
printed either with TeX, troff or nroff.  Look at the docs/Makefile
for the appropriate option.

                      Help and Bug Reports

Additional help can be obtained from the USENET newsgroup
        comp.graphics.gnuplot.
This newsgroup is the first place to ask for routine help.  It is is
gatewayed to a mailing list info-gnuplot@dartmouth.edu.  If you cannot
obtain a USENET feed and wish to join the above mailing list (or get
yourself off), mail to
        info-gnuplot-request@dartmouth.edu.
Please do not ask to sign up if you can read comp.graphics.gnuplot.

Note that since gnuplot has nothing to do with the GNU project, please
don't ask them for help or information about gnuplot; also, please
don't ask us about GNU stuff.

There is another list specifically for documented bug reports
and the submissions of fixes and modifications, bug-gnuplot@dartmouth.edu.
To join this list send mail to bug-gnuplot-request@dartmouth.edu.

"bug-gnuplot" is NOT an appropriate place to ask questions on how to
solve a gnuplot problem or even to report a bug that you haven't
investigated personally.  It is far more likely you'll get the help
you need for this kind of problem from comp.graphics.gnuplot.

"bug-gnuplot" is appropriate for turning in a formal bug report
that does not require timely action.  In other words, if you spend
time and investigate a bug, and especially if you fix a bug, then
send it to bug-report and your fix will be considered for the next
release of gnuplot.  Fixes should be in ``diff -c'' format done
against the most current official version of gnuplot or the latest
alpha or beta release of the next version.  All major modifications
should include documentation and a demo file.  Finally, it is
much easier to integrate smaller stepwise modifications rather
than one gigantic diff file which represented months of changes.
All messages to bug-gnuplot should include the machine you are using, the
operating system and it's version, plotting devices, and the version
of gnuplot that you are running.

                   Where to get updates to GNUPLOT

Congratulations on getting this version of GNUPLOT! Unfortunately, it
may not be the most recent version ("you never know where this version
has been!"). You can be sure that it IS the most recent version by
checking one of the official distribution sites, guaranteed to be kept
up to date (of course, if you got this file from one of those sites,
you don't need to check!).

To hear automatically about future releases (and other GNUPLOT news),
read the newsgroup; see above.

At the time of this writing, the following are the official
distribution sites and transfer instructions. Note that
prep.ai.mit.edu is NOT an official site, and may not be up to date.
Also, comp.sources.misc is usually a month or so behind us.

Date: Fri Jun 04 01:03:20 PDT 1993

Version: 3.4

In general, GNUPLOT 3.4 is available as the file gnuplot3.4.tar.Z.
There are no patches that bring GNUPLOT 2.02 up to 3.0, so you must
obtain the whole new release. There are, however, patches to
modify 3.0 to 3.1 and 3.1 to 3.2. There are no patches to bring
3.2 to 3.4.  (Version 3.3 was skipped because of possible confusion
with the numerous BETA releases.)  It will be made available to simtel20 
and its mirrors in ZIP format, along with a DOS, MS-Windows 3.1 and
OS/2 2.0 executables.

In addition, along with version 3.4, there is now a
gpcontrb.tar.z which contains additional modifications,
supplemental programs and demonstration files.  None of
these has been extensively tested by the authors but are
made available for possible future modifications and use.
Also, some sites will have gpdoc_ps.zip which contains
PostScript versions of the manuals and tutorials.

Please obtain gnuplot from the site
nearest you.

USENET users:

    GNUPLOT 3.4 was posted to comp.sources.misc.


NORTH AMERICA:

     Anonymous ftp to ftp.dartmouth.edu (129.170.16.4)
     Fetch
        pub/gnuplot/gnuplot3.4.tar.Z
     in binary mode.

     Users without ftp capability can obtain it through a mail ftp
     server. Send a mail message saying 'help' to
     BITFTP@pucc.princeton.edu for instructions. For a uuencoded
     copy of the gnuplot sources (compressed tar file), send this
     message to BITFTP@pucc.princeton.edu:
         FTP DARTMOUTH.EDU UUENCODE
         USER ANONYMOUS
         CD pub/gnuplot
         BINARY
         GET gnuplot3.4.tar.Z
         QUIT


AUSTRALIA:

     Anonymous ftp to monu1.cc.monash.edu.au (130.194.1.101).
     Fetch pub/gnuplot3.4.tar.Z in binary mode.


EUROPE:

     Anonymous ftp to irisa.irisa.fr (131.254.2.3).
     Fetch pub/gnuplot3.4.tar.Z in binary mode.

----

     DISCLAIMER - This product is not related in any way to
     Pixar or any other commercial venture.

----

                                        -Thomas Williams-
                                        -Alex Woo-


