# Updating transit networks

To update transit networks in the MHN and MRN:
1. Review configuration settings in \Scripts\transit_update.toml and
update them if necessary.
2. Execute `uv run tbmtools update` to run
[](./tbmtools.md#tbmtoolstransit).
3. Review the verification results in
\output\transit_line_verification_report.csv.
4. Investigate the results further using the Verify Transit Network
Toolbox in \VerifyTransitNetwork\VerifyTransitNetwork.aprx. Notes can be
added to \route_verification_notes.yml. These notes will show up in the
report the next time it's executed.
5. Repeat steps 1 through 4 until the verification results are
acceptable.

Line and segment data tables for bus and rail are written to
\output. Use MHN and MRN tools to load these tables into the
geodatabases.
* bus_lines_current.csv
* bus_segments_current.csv
* rail_lines_current.csv
* rail_segments_current.csv
