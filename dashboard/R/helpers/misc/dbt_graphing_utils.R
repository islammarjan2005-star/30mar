
DIT_Colours <- c('#CF102D', '#00285F', '#004D44', '#4814A0', '#0063BE', '#E24912','#CF102D', '#00285F', '#004D44', '#4814A0', '#0063BE', '#E24912')


theme_DIT <- function(){ 
  font <- "Arial"   #assign font family up front
  
  theme_minimal() %+replace%    #replace elements we want to change
    
    theme(
      
      #grid elements
      panel.grid.major = element_blank(),    #strip major gridlines
      panel.grid.minor = element_blank(),    #strip minor gridlines
      axis.ticks = element_blank(),          #strip axis ticks
      
      strip.background = element_rect(fill = '#CF102D'),
      strip.text = element_text(colour = 'White', size = 12),
      
      #since theme_minimal() already strips axis lines, 
      #we don't need to do that again
      
      #text elements
      plot.title = element_text(             #title
        family = font,            #set font family
        size = 16,                #set font size
        face = 'bold',            #bold typeface
        hjust = 0,                #left align
        vjust = 2),               #raise slightly
      
      plot.subtitle = element_text(          #subtitle
        family = font,            #font family
        size = 12),               #font size
      
      plot.caption = element_text(           #caption
        family = font,            #font family
        size = 9,                 #font size
        hjust = 1),               #right align
      
      axis.title = element_text(             #axis titles
        family = font,            #font family
        size = 10),               #font size
      
      axis.text = element_text(              #axis text
        family = font,            #axis famuly
        size = 9),                #font size
      
      axis.text.x = element_text(            #margin for axis text
        margin=margin(5, b = 10))
      
      #since the legend often requires manual tweaking 
      #based on plot content, don't define it here
      
      
    )
}






# DBT TIME SERIES PLOT — ONS-styled with chart_type ("line" | "stacked_area")

# =============================================================================
# Registry utilities (dispatcher)
# =============================================================================
.dbt_chart_builders <- new.env(parent = emptyenv())

dbt_register_chart <- function(name, fn) {
  stopifnot(is.character(name), length(name) == 1, is.function(fn))
  assign(name, fn, envir = .dbt_chart_builders)
}

dbt_get_chart_builder <- function(name) {
  fn <- get0(name, envir = .dbt_chart_builders, inherits = FALSE)
  if (is.null(fn)) stop("Unsupported chart_type: '", name,
                        "'. Register a builder via dbt_register_chart().")
  fn
}

# =============================================================================
# Small internal helpers used by builders
# =============================================================================

# Period utilities (for bars)
.dbt_period_floor <- function(d, interval) {
  y <- as.integer(format(d, "%Y")); m <- as.integer(format(d, "%m"))
  if (interval == "daily") return(d)
  if (interval == "weekly") return(d - (as.integer(format(d, "%u")) - 1L))  # ISO Monday
  if (interval == "monthly") return(as.Date(format(d, "%Y-%m-01")))
  if (interval == "quarterly") {
    qstart_m <- c(1,4,7,10)[ ((m - 1) %/% 3) + 1L ]
    return(as.Date(sprintf("%04d-%02d-01", y, qstart_m)))
  }
  if (interval == "annually") return(as.Date(sprintf("%04d-01-01", y)))
  if (interval == "5year")   return(as.Date(sprintf("%04d-01-01", y - (y %% 5))))
  if (interval == "decade")  return(as.Date(sprintf("%04d-01-01", y - (y %% 10))))
  d
}
.dbt_by_string <- function(interval) switch(
  interval,
  daily = "1 day", weekly = "7 days", monthly = "1 month",
  quarterly = "3 months", annually = "1 year",
  `5year` = "5 years", decade = "10 years", "1 month"
)
