label_cleaner <- function(variable){
  label <- tools::toTitleCase(gsub('_', ' ', variable))
  
  label
}


#' Creates a standardised linechart using ggplot
#'
#' @param df dataframe containing columns to plot
#' @param x_axis name of column containing x-axis values, specified as string eg. 'year'
#' @param y_axis name of column containing y-axis values, specified as string eg. 'value'
#' @param group optional, name of column to group by when applying colour palette (default: 'none') eg. 'country'
#' @param range_zero 
#'
#' @import ggplot2 
linechart <- function(df, x_axis, y_axis, group='none', range_zero = F){
 
  p <- 
    if(group == 'none'){
      p <- ggplot(data = df,
                  mapping = aes(x = .data[[x_axis]],
                                y = .data[[y_axis]]))
      }else{
        p <- ggplot(data = df,
                    mapping = aes(x = .data[[x_axis]],
                                  y = .data[[y_axis]],
                                  colour = .data[[group]]))
        }
  
  
  p <- p + geom_line() + theme_dbt() + scale_color_manual(values = dbtplotr:::dbt_palettes$gaf)
  
  if(is.numeric(data[[y_axis]])){
    if(range_zero){
      p <- p + scale_y_continuous(labels = scales::comma, 
                                  limits = c(0, max(df[[y_axis]])))
    }else{
      p <- p + scale_y_continuous(labels = scales::comma)
    }
  }
  
  p <- p + labs(x = label_cleaner(x_axis), y = label_cleaner(y_axis), color = label_cleaner(group))

  p
}


#' Creates a standardised barchart using ggplot
#'
#' @param df dataframe containing columns to plot
#' @param x_axis name of column containing x-axis values, specified as string eg. 'year'
#' @param y_axis name of column containing y-axis values, specified as string eg. 'value'
#' @param group optional: name of column to group by when applying colour palette eg. 'country'  ('none' by default)
#' @param flip optional invert axes (TRUE) or maintain (FALSE, default)
#'
#' @import ggplot2 scales
barchart <- function(df, x_axis, y_axis, group='none',flip = FALSE){
  
  if(class(df[[x_axis]])%in%c("Date","factor")){

    p <- 
      if(group == 'none'){
        p <- ggplot(data = df,
                    mapping = aes(x = .data[[x_axis]],
                                  y = .data[[y_axis]]))
      
      }else{
        df[[group]] <- factor(df[[group]])
        p <-
          ggplot(data = df,
                 mapping = aes(x = .data[[x_axis]],
                               y = .data[[y_axis]],
                               fill = .data[[group]]))
    }
  }else{
    p <- 
      if(group == 'none'){
        p <- ggplot(data = df,
                    mapping = aes(x = reorder(.data[[x_axis]], .data[[y_axis]]),
                                  y = .data[[y_axis]]))
      }else{
        df[[group]] <- factor(df[[group]])
        p <-
          ggplot(data = df,
                 mapping = aes(x = reorder(.data[[x_axis]],.data[[y_axis]]),
                               y = .data[[y_axis]],
                               fill = .data[[group]]))
      }
    
    
  }
  
  if(flip){
    p <- p + coord_flip()
  }
  
  p <- p + geom_col(position = 'dodge') + theme_dbt()
  
  p <- p + scale_fill_manual(values = dbtplotr::dbt_palettes$gaf)
  
  if(is.numeric(data[[y_axis]])){
    p <- p + scale_y_continuous(labels = scales::comma)
  }
  
  p <- p + labs(x = label_cleaner(x_axis), y = label_cleaner(y_axis), fill = label_cleaner(group))
  
  p
}
