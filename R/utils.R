#' Evaluate quietly
#' @details
#' Function to hide undesired prints. Copied from:
#' https://r.789695.n4.nabble.com/Suppressing-output-e-g-from-cat-td859876.html
#' @param x A function to evaluate.
#' @examples
#' # quiet(print("Nothing is shown"))
quiet = function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}
