--  Created by William Kyngesburye on 2006-12-12.
--  GRASS Applescript startup
--  COPYRIGHT: (C) 2006-2007 by the GRASS Development Team
-- 	This program is free software under the GPL (>=v2)
--	Read the file COPYING that comes with GRASS for details.

on launched theObject
	set grass_path to (posix path of (path to me as string)) & "Contents/Resources/"
	set grass_startup to (quoted form of (grass_path & "grass.sh"))
	set grassRun to grass_startup & "; exit"
	
	set TerminalRunning to false
	try
		if ((do shell script "ps -axc | grep '\\bTerminal\\b'") is not null) then
			set TerminalRunning to true
		end if
	end try
	tell application "Terminal"
		activate
		if TerminalRunning then
			do script (grassRun)
		else
			do script (grassRun) in window 1
		end if
	end tell
	
	tell me to quit
end launched
