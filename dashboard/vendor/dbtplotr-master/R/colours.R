#' Colour palettes for DBT
#'
#' Returns a named list containing various package colour schemes.
#'
#' @noRd
#' @export
dbt_palettes <- list(
  
  # Analysis community colours as of June 2024 
  # https://workspace.trade.gov.uk/working-at-dbt/policies-and-guidance/guidance/analysis-briefing-guidance-and-templates/
  dbt2 = list(
    default = c("#00285f", "#739AB0"),
    blue_berry1 = c("#00285f","#C78094"),
    blue_berry2 = c("#7D2057","#739AB0"),
    blue_berry3 = c("#224E59","#C78094"),
    berry = c("#7D2057","#C78094"),
    blue1 = c("#00285f", "#739AB0"),
    blue2 = c("#224E59", "#739AB0")
  ),
  
  
  dbt = list(
    default = c("#00285f", "#739AB0","#224E59", "#C78094","#7D2057"),
    reverse = c("#7D2057", "#C78094","#224E59", "#739AB0", "#00285f"),
    option2 = c("#00285f", "#C78094","#224E59", "#739AB0","#7D2057"),
    extended = c("#00285f", "#739AB0","#224E59", "#C78094","#7D2057", "#9F80EA", "#590094"),
    extended_reverse = c("#590094", "#9F80EA", "#7D2057", "#C78094","#224E59", "#739AB0", "#00285f")
  ),
  
  sequential = list(
    default = c("#b0c9e1", "#6c98c7", "#4174a3", "#1e4f7f", "#00285f", "#001f4c", "#001442", "#000d3a", "#00062f"),
    rag = c("#DB4325","#EDA247","#E6E1BC","#57C4AD","#006164"),
    blue = c("#b0c9e1", "#6c98c7", "#4174a3", "#1e4f7f", "#00285f", "#001f4c", "#001442", "#000d3a", "#00062f"),
    berry = c("#f1c6d1","#e79db5","#d2749b","#9c0075","#7a005e","#59004a","#3a0035","#1c0022","#0d0014")
  ),
  
  
  # https://analysisfunction.civilservice.gov.uk/policy-store/codes-for-accessible-colours/
  gaf = c("#12436D", "#F46A25", "#801650","#28A197","#3D3D3D","#A285D1"),
  gaf2 = c("#12436D", "#F46A25"),
  gaf3 = c("#12436D", "#F46A25", "#801650"),
  
  blues = c("#12436D", "#0965A0", "#2493D9","#7BC7F8","#bdd7e7"),
  
  # https://colorbrewer2.org/
  reds = c("#a50f15", "#de2d26", "#fb6a4a","#fcae91","#fee5d9"),
  
  highlight = c("#12436D", "#BFBFBF"),
  
  # https://workspace.trade.gov.uk/working-at-dbt/policies-and-guidance/guidance/choose-the-right-dit-brand/
  corporate = c("#cf102d", "#00285F", "#004d44", "#4F0B7B","#0063BE", "#E24912", "#A90083")
)




#' Applies a colour palette to a ggplot2 chart
#' 
#' Generates a discrete scale function for an chart colour palette and a given
#' aesthetic, using `ggplot2::scale_discrete_manual()`.
#' 
#' @param palette Select the name of a DBT colour palette. 
#' The ONS palette is used by default. Other options can be viewed at https://gitlab.data.trade.gov.uk/dbt-data-visualisation-library/dbtplotr#colour-palettes'.
#' You can also provide a custom hex-colour string eg. `'#FF0000, #FFFF00, #0000FF, #800080'`
#' @param type Specify whether the chosen colours will apply to a line or an area.
#' Options are 'line' (default), 'bar', 'pie', 'donut', 'colour', 'fill'.
#'
#' @export
colours_dbt <- function(palette = 'dbt', option = NULL, type = 'line'){
  
  # # Check if palette is defined
  if(all(grepl("^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$", palette))){
    
    choice <- palette #%>% gsub(" ", "", .) %>% strsplit(",") %>% unlist()  # User may enter a custom palette
    
  } else if (is.character(palette) && length(palette) == 1 && palette %in% names(dbt_palettes)){
    
    choice <- dbt_palettes[[palette]]
    
    if (is.list(choice)){
      
      if (is.null(option)) {option <- "default"}
      
      if (!option %in% names(choice)){
        
        stop(sprintf("Invalid palette choice. Select from: %s", paste(names(choice), collapse = ", ")))
      } 
      else {choice <- choice[[option]]}
    } 
  } 
  
  else {
    
    stop(sprintf("Invalid palette choice. Input a valid list of hex colours or choose from: %s", 
                 paste(names(dbt_palettes), collapse = ", ")))
    
  }
  
  # Depending on chart type the colours will either apply to lines or to fill an area
  if(type == 'line'){
    aes <- 'colour'
  } else if(type %in% c('bar','pie','donut')){
    aes <- 'fill' 
  } else {
    aes <- type
  }
  
  
  ggplot2::scale_discrete_manual(values = choice, aesthetics = aes)
}
