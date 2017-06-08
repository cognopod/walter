#' waltplot
#' 
#' A function to grid all the waltplots.
#' 
#' @param vars A vector of column names to be plotted.
#' @param smoothed A parameter of whether to use histogram (0) or violinplots (1) in the waltplot grid.
#' @param data The data frame.
#' @export
#' 
waltplot <- function (vars, smoothed, data) {
  p = NULL
  for (i in 1:length(vars)) {
    if (smoothed[i] == 0) {
      p[[i]] <- suppressWarnings(walt.histogram(data, vars[i]))
    } else {
      p[[i]] <- walt.violinplot(data, vars[i])
    }
  }
  suppressWarnings(cowplot::plot_grid(plotlist = p, 
                             labels = c(LETTERS[1:length(vars)]), 
                             ncol = round(sqrt(length(vars)), digits = 0), 
                             nrow = ceiling(length(vars)/round(sqrt(length(vars))))
  ))
}