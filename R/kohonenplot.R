##-----------------------------------------------------------------------------
#' Convert a SOM into a heat map
#' 
#' @param som A kohonen Self-Organising Map
#' @param scaled Scale the size of the groups to the range [-1, 1]; Default = TRUE
#' 
#' @examples
#' heat.map(som(as.matrix(mtcars), somgrid(2, 2, topo = 'hexagonal'), alpha = c(0.05, 0.01), keep.data = TRUE, rlen = 1000, n.hood = 'circular'))
#' @export
heat_map <- function(som, scaled = T) {
  df <- som_to_dataframe(som, scaled)
  ggplot2::ggplot(df, aes(Group, Parameter, fill = Value)) + ggplot2::geom_raster() +
    ggplot2::scale_fill_gradient2() + 
    ggplot2::geom_text(data = dplyr::filter(df, Parameter == 'Size'), aes(Group, Parameter, label = round(Value, 2)))
}


##-----------------------------------------------------------------------------
#' Convert a SOM to a data frame ready to be plotted
#' 
#' Takes a SOM and create a data frame with a row for each tile in the SOM and 
#' a column for each column of the original matrix. The content of the data 
#' frame is equivalent to the codes matrix. Then the data frame is molten to 
#' ease the plotting of the data frame
#' 
#' @param som A kohonen Self-Organising Map
#' @param scaled Scale the size of groups to the range [-1, 1]; Default = TRUE
som_to_dataframe <- function(som, scaled = T) {
  num_fields <- som$grid$xdim * som$grid$ydim
  
  size <- data.frame(dplyr::select(dplyr::tally(dplyr::group_by(data.frame(Class = som$unit.classif), Class)), n))
  colnames(size) <- c('Size')
  if (scaled) {
    size <- scale(size)
  } 
  
  df <- reshape2::melt(
    data.frame(Group = 1:num_fields, Size = size, som$codes), id.vars = c('Group'))
  colnames(df) <- c('Group', 'Parameter', 'Value')
  df
}



