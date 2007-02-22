import os

env={}

class NotInGRASSSession(Exception):
    def __str__(self):
        return "You must be in running GRASS session"

if not os.getenv("GISBASE"):
    raise NotInGRASSSession()
else:
    env["GISBASE"] = os.getenv("GISBASE")
    env["GIS_LOCK"] = os.getenv("GIS_LOCK")
    env["GISRC"] = os.getenv("GISRC")

for key in os.environ.keys():
    if key.find("GRASS") > -1:
        env[key] = os.getenv(key)

for line in os.popen("g.gisenv").readlines():
    key,val = line.strip().split("=")
    env[key] = val


