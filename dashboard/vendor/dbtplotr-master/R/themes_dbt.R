#' Applies a standardised theme to a ggplot .
#'
#' This theme has been designed using charting best practice principles.
#'
#' @param legend Choose the position of the legend. Options are 'bottom' (default), 'top', 'right', 'left', 'none'.
#' @param gridline Choose which axis you want major gridlines on. Options are 'y' (default), 'x', 'none'.
#' @param void Removes all axis details equivalent to theme_void(). Options are FALSE (default) or TRUE.
#'
#' @export
#' @import ggplot2

theme_dbt <- function(legend = 'bottom', gridline = 'y', void = FALSE){
  
  font <- "Arial"   #assign font family up front
  
  # Assign a grid line style (same as used in theme_minimal)
  if(gridline == 'y') {
    grid_y = element_line(color = "grey90", linewidth = 0.2)
    grid_x = element_blank()
  } else if(gridline == 'x') {
    grid_y = element_blank()
    grid_x = element_line(color = "grey90", linewidth = 0.2)
  } else {
    grid_y = element_blank()
    grid_x = element_blank()
  }
  
  
  # Changes the default geom styles eg: GeomBar$default_aes
  update_geom_defaults("line", list(linewidth = 0.8))  # Original default is 0.5
  update_geom_defaults("bar", list(fill = "#00285f")) # Original default is grey35, replaced by DBT analysis blue. 
  
  
  # Construct theme
  theme_minimal() +    # Based on minimal with some elements changed
    
    # Global components for all charts - Title & Legend position and size
    theme(
      # This increases the top margin so there is more space for the Titles (ggplot default is 5.5pt)
      plot.margin = margin(t=15, r=5.5, b=5.5, l=5.5, "pt"),
      
      # This sets the position of the title, subtitle and caption with reference to the entire plot
      plot.title.position = "plot",      
      plot.caption.position = "plot",       
      
      # This defines the chart title  font and size
      plot.title = element_text(             
        family = font,            
        size = 14,                
        face = 'bold',            # Bold typeface
        hjust = 0,
        vjust = 5),               # Raises title further above plot area
      
      # This defines the subtitle font and size
      plot.subtitle = element_text(          
        family = font,            
        size = 12,
        hjust = 0,
        vjust = 6),               # Raises subtitle further above plot area
      
      # This defines the caption/footnote font and size
      plot.caption = element_text(           
        family = font,            
        size = 8,                 
        hjust = 0),               # Left align
      
      # Default to putting Legend under the chart. Other options are 'none', 'top' 'right', 'left'.
      legend.position = legend
    ) +
    
    if (void == FALSE) {
      
      # Panel and axis components for most charts
      theme(
        # Gridline style - Major gridlines based on 'gridline' parameter. All minor gridlines removed.
        panel.grid.major.y = grid_y,
        panel.grid.major.x = grid_x,
        panel.grid.minor = element_blank(),
        
        
        # This removes Axis tick marks
        axis.ticks = element_blank(),
        
        # This sets the axis text font and size
        axis.title = element_text(family = font,
                                  size = 12),
        
        axis.text = element_text(family = font,
                                 size = 10),
        
        # This sets the Margin for x and y axis tick labels
        axis.text.x = element_text(margin = margin(b = 6)),
        
        axis.text.y = element_text(margin = margin(l = 6))
      )
      
    } else if (void == TRUE) {
      # Panel and axis components for for Pie charts
      theme(
        # Removes all panel and axis text and lines
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank()
      )
    }
}
