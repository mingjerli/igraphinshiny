# igraphinshiny 0.2.0

## Major Changes

* Updated minimum R version requirement to R >= 4.1.0 for compatibility with
  current R releases.

* Replaced all deprecated igraph functions with their modern equivalents:
    - `graph.full()` → `make_full_graph()`
    - `graph.empty()` → `make_empty_graph()`
    - `graph.lattice()` → `make_lattice()`
    - `graph.ring()` → `make_ring()`
    - `erdos.renyi.game()` → `sample_gnp()`
    - `watts.strogatz.game()` → `sample_smallworld()`
    - `barabasi.game()` → `sample_pa()`
    - Layout functions updated to use `layout_*()` naming convention
    - Centrality functions updated to use underscore naming convention

## Minor Changes

* Added explicit namespace prefixes for all igraph function calls in the
  Shiny application.

* Updated NAMESPACE to use explicit exports and imports.

* Improved DESCRIPTION with better description text, URL, and BugReports fields.

* Added UTF-8 encoding specification.

* Fixed typo in UI: "Graphout" corrected to "Graphopt".

* Renamed centrality output columns for clarity:
    - "Bon" → "Power" (power centrality)
    - "Evcent" → "Eigenvector" (eigenvector centrality)
    - "Kleinberg" → "Authority" (authority score)

# igraphinshiny 0.1.1

* Added Excel file input support using readxl package.

* Fixed adjacency matrix input handling.

* Added adjustable arrow size control.

# igraphinshiny 0.1.0

* Initial CRAN release.
