--  Created by William Kyngesburye on 2006-12-12.

on launched theObject
	set grass_path to (path to me as string) & "Contents:Resources:"
	set grass_startup to grass_path & "grass.sh"
	
	tell application "Finder" to open file grass_startup using (application file "Terminal.app" in folder "Utilities" in folder "Applications" in startup disk)
	tell me to quit
end launched
