#' Run igraph Plot Demo in Shiny
#'
#' Launches an interactive Shiny web application that demonstrates igraph
#' plotting functionality and graph theory concepts. Users can visualize
#' various graph types, view adjacency matrices, and compute centrality measures.
#'
#' @return This function does not return a value. It launches a Shiny application
#'   in the user's default web browser.
#'
#' @examples
#' \dontrun{
#' plotDemo()
#' }
#'
#' @export
plotDemo <- function() {
  appDir <- system.file("igraphPlotDemo", package = "igraphinshiny")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `igraphinshiny`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
