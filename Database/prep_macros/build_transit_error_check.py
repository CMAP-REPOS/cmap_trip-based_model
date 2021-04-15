# BUILD_TRANSIT_ERROR_CHECK.PY
# Nick Ferguson, 07/09/2015

# Checks report for input errors that may have occurred during transit
# network batchin. Creates an empty file to signal that an error was
# found.

# Revisions ------------------------------------------------------------
# 12/12/2017 NRF: Adjusted file paths for call from
#                 build_transit_error_check.bat

import sys, os, string

scen = sys.argv[1]
rpt_name = 'build_' + str(scen) + 'transit.rpt'
rpt = os.path.dirname(os.getcwd()) + '\\report\\' + rpt_name

with open(rpt) as r:
    for line in r:
        split_line = string.split(line, 'error count=')
        if len(split_line) > 1:
            count = int(string.strip(split_line[1]))
            if count > 0:
                f = open(
                        os.path.dirname(os.getcwd())
                        + '\\report\\build_transit.error', 'w')
                f.close()
                sys.exit()