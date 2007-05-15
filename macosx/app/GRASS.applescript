--  Created by William Kyngesburye on 2006-12-12.
--  GRASS Applescript startup
--  COPYRIGHT: (C) 2006-2007 by the GRASS Development Team
-- 	This program is free software under the GPL (>=v2)
--	Read the file COPYING that comes with GRASS for details.

on launched theObject
	set grass_path to (path to me as string) & "Contents:Resources:"
	set grass_startup to grass_path & "grass.sh"
	
	tell application "Finder" to open file grass_startup using (application file "Terminal.app" in folder "Utilities" in folder "Applications" in startup disk)
	tell me to quit
end launched
