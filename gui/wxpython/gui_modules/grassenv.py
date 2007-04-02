import os

env={}

class NotInGRASSSession(Exception):
    def __str__(self):
        return "You must be in running GRASS session"

class CouldNotStartMonitor(Exception):
    def __init__(self,monitor):
        self.monitor = monitor

    def __str__(self):
        return "Could not start GRASS monitor <%s>" % self.monitor

class CouldNotStopMonitor(Exception):
    def __init__(self,monitor):
        self.monitor = monitor

    def __str__(self):
        return "Could not start GRASS monitor <%s>" % self.monitor

class CouldNotExecute(Exception):
    def __init__(self,command):
        self.command = command
    def __str__(self):
        return "Could not execute GRASS command: %s" % self.command


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
    val = val.replace("'","")
    val = val.replace(";","")
    env[key] = val
