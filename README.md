# Common Covid Tasks

CCT combines a number of commonly requested covid related tasks into an easy(ish) to use package.
- Create a node network hierarchy from the [pangolin](https://github.com/cov-lineages/pango-designation) lineages and aliases
- Cache that data locally in case of remote data or access problems
- Query that network for ancestors and descendants of a lineage
- Parse [constellations](https://github.com/cov-lineages/constellations/) into a easily used table
- Parse the [covariants.org](https://covariants.org/) variant table
- Expand all the variants in that table into their children
- Sort lineages by ancestry instead of alnum

## Installation Instructions

*Clone Git Repository and Install Locally*

`install.packages("cct", repos = NULL, type="source")`

or

`remotes::install_local("cct")`

*Install from Github*

`remotes::install_github("TheZetner/cct")`

