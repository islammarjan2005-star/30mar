#' Applies a standardised theme to a plotly chart.
#'
#' This theme has been designed using charting best practice principles.
#'
#' @param p A plotly object. 
#' @param legend Choose the position of the legend. Options are 'bottom' (default), 'top', 'right', 'left', 'none'.
#' @param gridline Choose which axis you want major gridlines on. Options are 'y' (default), 'x', 'none'.
#' @param void Removes all axis details equivalent to theme_void(). Options are FALSE (default) or TRUE.
#'
#' @export
#' @import plotly


theme_dbt_plotly <- function(p, legend = 'bottom', gridline = 'y', void = FALSE){
  
  # Theme only affects plotly layout
  layout(p,
         
         # This sets the position and font size of the title
         title = list(font = list(size = 18),
                      x = 0.01, y = 0.97,
                      xanchor = 'left'),
         
         # This sets the x axis font size and removes gridlines.
         xaxis = list(title = list(font = list(size = 16)),
                      showgrid = F),
         
         # This sets the y axis font size.
         yaxis = list(title = list(font = list(size = 16))),
         
         # Default to putting Legend under the chart. Other options are 'none', 'top' 'right', 'left'.
         legend = list(orientation = 'h',
                       xanchor = "center",
                       x = 0.5)
  )
}
