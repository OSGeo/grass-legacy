#!/bin/sh  -- #perl, to stop looping
eval 'exec $GRASS_PERL -S $0 ${1+"$@"}'
    if 0;

# ##############################################################
# 
# $Id$
# grass help system to html converter
# Andreas Lange, andreas.lange@rhein-main.de
#
# 
#       
# ##############################################################

# use CGI::Pretty;
# use strict;

# ##############################################################
#
# globals 
#
# ##############################################################

my ($DEBUG) = 0;
my ($VERB)  = 0;

my ($GISBASE) = $ARGV[0];
my ($GRASS_DOCS) = "documents/html";
my ($GRASS_HELP) = "documents/html/help";
my ($GRASS_BASE) = "./help";
my ($GRASS_DOC_HOME) = "$GRASS_DOCS/index.html";
my ($GRASS_HOME) = "http://grass.itc.it/index2.html";
my ($GRASS_LOGO) = "grass.smlogo.gif";
my ($GRASS_NOTFND) ="$GRASS_HELP/notfound.html";

# ##############################################################
#
# tools
#
# ##############################################################


# ##############################################################
#
# escape html entities
#
# ##############################################################
sub html_escape {
  my (@str) = @_;

  foreach (@str) {
    s/\&/\&amp;/g;
    s/\%/\&percent;/g;
    s/</\&lt;/g;
    s/>/\&gt;/g;
    s/\"/&quot;/g;
  }
  return wantarray ? @str : $str[0];
}


# ##############################################################
#
# create horizontal line in grass style
#
# ##############################################################
sub html_hl {
  return ("    <HR width=100%% align=center size=6 noshade></HR>\n");
}


# ##############################################################
#
# create document head in grass style
#
# ##############################################################
sub html_head {
  my ($heading, $title, $logo) = @_;

  return "
<HTML>
  <HEAD>
    <TITLE>
      $title
    </TITLE>
  </HEAD>
  <BODY bgcolor=\"white\">
    <TABLE>
      <TR>
        <TD><IMG SRC=\"$logo\" ALT=\"GRASS Logo\" ALIGN=\"center\"></TD>
        <TD ALIGN=\"center\" VALIGN=\"middle\"><H1>$heading</H1></TD>
      </TR>
    </TABLE>
    <P>\n" . html_hl . "</P>"; 
}


# ##############################################################
#
# create list of navigation targets
#
# ##############################################################
sub html_home {
  my ($str, $url) = ("","");
  my ($turl) = shift;
  my ($gurl) = shift;

  while (defined ($turl)) {
    $url = html_url($turl, $gurl);
    $str .= "
    <P>
      $url
      <BR>\n";
    $turl = shift;
    $gurl = shift;
  }

  return $str;
}


# ##############################################################
#
# create footer for document in grass style
#
# ##############################################################
sub html_foot{
  my($date);

  $date=localtime(time);

  return "
    <P>\n  " . html_hl . "    <B>Please use your browser buttons to navigate this documentation!</B>
      <BR>
      <I>Automatically created on: $date</I>
    </P>
  </BODY>
</HTML>\n";
}


# ##############################################################
#
# cascading style sheet
#
# ##############################################################
sub html_css {
  # not implemented. 
  return; 
}


# ##############################################################
#
# format unified resource locator
#
# ##############################################################
sub html_url {
  my ($text, $url) = @_;

  return "<A HREF=\"$url\">$text</A>";
}


# ##############################################################
#
# preformatted text
#
# ##############################################################
sub html_pre {
  my ($text) = @_;

  return "  <PRE>$text</PRE>\n";
}


# ##############################################################
#
# format single line centered
#
# ##############################################################
sub html_center {
  my ($line) = @_;

  $line = trim($line);
  return "
  <CENTER>
    $line
  </CENTER>\n";
}


# ##############################################################
#
# format single heading level n
#
# ##############################################################
sub html_h {
  my ($level) = shift;
  my ($line) = @_;

  $line = trim($line);
  return "
    <H$level>
      $line
    </H$level>\n";
}


# ##############################################################
#
# trim whitespace
#
# ##############################################################
sub trim {
  my (@str) = @_;

  foreach (@str) {
    s/^\s+//;
    s/\s+$//;
  }
  return wantarray ? @str : $str[0];
}


# ##############################################################
#
# do the grass help system specific formatting
#
# ##############################################################
sub html_format {
  my (@text) = @_;
  my (@str);
  my ($line);

  foreach $line (@text) {
    if ($line =~ /\|/) {
        push(@str, html_pre($line));
      } elsif ($line =~ /(---)|(===)/) {
        push(@str, html_pre($line));
      } elsif ($line =~ /^\s+[^\s]+/) {
        if ($line !~ /[a-z]+/) {
          push(@str, html_center(html_h(2, $line)));
        } else {
          push(@str, html_center($line));
        } 
      } elsif ($line =~ /^\s*$/) {
        push(@str, "    <P>");
    } else {
      push(@str, $line);
    }
  }
  return @str;
}


# ##############################################################
#
# print to file descriptor
#
# ##############################################################
sub html_print {
  my ($FH) = shift;
  my (@text) = @_;
  my ($line);

  foreach $line (@text) {
    printf($FH $line);
  }
  return;
}


# ##############################################################
#
# re-work grass help system links to html urls
#
# ##############################################################
sub html_links {
  my ($h) = shift;
  my ($level, @text) = @_;
  my (@result, $hlevel, $line);
  my ($m, $t, $url); 

  $hlevel = "./";
  $hlevel .= ("../" x $level);
  foreach $line (@text) {
    $line = html_escape($line);
    print $line if $DEBUG;
    while ($line =~ m/(\\[^\\]+\\)/g ) { 
      # equivalent: m/(\\(?:(?!\\).)*\\)/g
      $m = $1;
      $t = $m;
      $t =~ s/\\//g; 
      $t = trim($t);
      if (defined ($h->{$t}) ) {
        # create url
        print "found: $h->{$t}\n" if $DEBUG;
        $url = html_url ($t, "$hlevel$h->{$t}");
        # check if link exists here!
      } else {
        # create link to notfound.html
        $url = html_url ($t, "$hlevel$GRASS_NOTFND");
      }
      $line =~ s/\Q$m\E/$url/;
    }
    push @result, $line;
  }
  return @result; 
}


mkdir "$GISBASE/$GRASS_HELP", 0777;


# ##############################################################
#
# read in and process LOOKUP database
#
# ##############################################################

open LOOKUP, "< ./$GRASS_BASE/LOOKUP" || die "ERROR: LOOKUP can not be opened";

my ($key) = 1;
my ($key_str) = "";
my ($line);
my (%hash);

while (defined($line = <LOOKUP>)) {
  chomp($line);
  $line = html_escape($line);

  if ( $key ) {
    $key = 0;
    $key_str = trim($line);
    next;
  } else {
    $key = 1;
  
    if ( $line =~ /^\.+/s ) {
      # link to manual page
      $line =~ s!^\.\./\.\./man/man\d/!$GRASS_DOCS/!g;
      $line =~ s!\.\d$!!g;
    }
    if ( $line =~ /^\d+/s ) {
      if ( $line =~ /17\.manual\/Help\.pages/s ) {
        # old one-line help pages, not useful any more
        $line =~ s!17\.manual/Help\.pages/!$GRASS_DOCS/!g;
      } else {
        # link to page
        $line = "$GRASS_HELP/$line";
      }
    }
    if ( $line =~ /^Commands/s ) {
      # link to Commands.def
      $line = "$GRASS_HELP/$line";
    }
    print("$key_str -> $line.html \n") if $DEBUG;
    $hash{$key_str} = "$line.html";
  }
}

close(LOOKUP);
  
if ( $VERB ) {
  print "List of Manual entries found: \n";
  foreach $key (sort (keys %hash)) {
    print "  $key -- $hash{$key} \n"; 
  }
}


# ##############################################################
#
# create notfound.html page
#
# ##############################################################

open FILE, "> $GISBASE/$GRASS_NOTFND" || die "ERROR: $GRASS_NOTFND could not be created";

printf(FILE html_head("File not found", "Not found", "./../$GRASS_LOGO"));
printf(FILE "
    <H2>This file could not be found.</H2>
    <P>\n");
printf(FILE html_hl);
printf(FILE html_home("GRASS Help", "./index.html", "GRASS Documentation", "./../../../$GRASS_DOC_HOME", "GRASS WWW Home", $GRASS_HOME));
printf(FILE html_foot);

close FILE;

# ##############################################################
#
# create index.html page
#
# ##############################################################

my(@text);

open INDEX, "< ./$GRASS_BASE/INDEX" || die "ERROR: INDEX could not be opened";
@text = <INDEX>;
close INDEX;

open FILE, "> $GISBASE/$GRASS_HELP/index.html" || die "ERROR: index.html could not be created";
printf(FILE html_head("Grass Help System", "Help Index", "./../$GRASS_LOGO"));
html_print(*FILE, html_format(html_links (\%hash, 3, @text)));
printf(FILE html_hl);
printf(FILE html_home("GRASS Help", "./index.html", "GRASS Documentation", "./../../../$GRASS_DOC_HOME", "GRASS WWW Home", $GRASS_HOME));
printf(FILE html_foot);
close FILE;


# ##############################################################
#
# create individual html pages for help system
#
# ##############################################################

my ($dir, $file); 

opendir(DIR, "$GRASS_BASE") || die "ERROR: Could not open $GRASS_BASE";

while (defined($dir = readdir(DIR))) {
  next if $dir =~ /^\.\.?/;
  next if $dir =~ /CVS/;
  next if ( -f "$GRASS_BASE/$dir" );
  mkdir "$GISBASE/$GRASS_HELP/$dir", 0777;
  print "dir: $dir\n" if $DEBUG;

  opendir (SUBDIR, "$GRASS_BASE/$dir") || die "ERROR: $dir";
  while (defined($file = readdir(SUBDIR))) {
    next if $file =~ /^\.\.?/;
    next if $file =~ /CVS/;
    next if (! -f "$GRASS_BASE/$dir/$file");
    print "  $file\n" if $DEBUG;

    open (INFILE, "$GRASS_BASE/$dir/$file") || die "ERROR: $file";
    @text = <INFILE>;
    close INFILE;
    open (FILE, ">$GISBASE/$GRASS_HELP/$dir/$file.html") || die "ERROR: $file.html";
    printf(FILE html_head("GRASS Help System", "$file", "./../../$GRASS_LOGO"));
    html_print(*FILE, html_format(html_links(\%hash, 4, @text)));
    printf(FILE html_hl);
    printf(FILE html_home("GRASS Help", "./../index.html", "GRASS Documentation", "./../../../../$GRASS_DOC_HOME", "GRASS WWW Home", $GRASS_HOME));
    printf(FILE html_foot);
    close FILE;
  }
  closedir(SUBDIR);
}

closedir(DIR);

__END__
