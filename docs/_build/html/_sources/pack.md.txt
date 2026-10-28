# Packaging model data

To package model data for sharing:
1. Review configuration settings in
\src\tbmtools\src\tbmtools\config.yaml and update them if necessary.
2. Execute `uv run tbmtools pack` to run
[](./tbmtools.md#tbmtoolsdata_package).

Data read from the containing trip-based model project directory will be
written to \packaged_data.

Templates for the Data User Guide in
\src\tbmtools\src\tbmtools\templates may be edited as needed.
* data_user_guide_md.txt - Markdown template used for content
* data_user_guide_html.txt - HTML template used for styling