#ident	"$Header: stdcshrc,v 1.4 90/04/08 16:27:39 integ Exp $"
#	This is the default standard profile provided to a user.
#	They are expected to edit it to meet their own needs.

set history=40
umask 022

set path=(. /usr/X11/bin /usr/X11/contrib/bin /usr/X11/demos/bin /usr/net /bin /usr/bin /usr/bin/X11 /usr/ucb /usr/local/bin /usr/grass)

if ( $?prompt ) then
	/bin/stty line 2 erase '^H' kill '^U' intr '^C' echoe ctlecho
	eval `tset -s -Q`

	# list directories in columns
	alias ls 'ls -C'
endif
