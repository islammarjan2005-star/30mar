# dbtplotr

`dbtplotr` is an R package to provide standardised colours and chart formats for 
ggplot2. It includes theme functions which meet Government Statistical
Service (GSS) best practice guidance. It also includes colour palettes
which meet WCAG 2.0 accessibility standards, and are distinguishable in greyscale.

## Installation

The package can be installed directly from GitLab by putting the following code at the top of your scripts:

```r
devtools::install_git("git@gitlab.data.trade.gov.uk:dbt-data-visualisation-library/dbtplotr.git", upgrade = 'never')
library(dbtplotr)
```

## General Good Practice
All charts should be properly labelled, with a title and a source or other notes.  
In ggplot this can be done using the `labs()` component (see example in the code below).  
More examples of charts that iuse this package are at: https://gitlab.data.trade.gov.uk/dbt-data-visualisation-library/library/-/blob/master/r/README.md  


## DBT ggplot theme
The DBT ggplot theme should be used to ensure a consistent chart style.  
It is used by adding `dbtplotr::theme_dbt()` at the end of the ggplot code (see example below).

```r
library(dplyr)
library(ggplot2)
devtools::install_git("git@gitlab.data.trade.gov.uk:dbt-data-visualisation-library/dbtplotr.git", upgrade = 'never')
library(dbtplotr)

ggplot(data = dbtplotr::sk_timeseries, aes(x = period, y = value, group = direction)) +
  geom_line(aes(color = direction)) +
  labs(title = "UK Trade with South Korea",
       subtitle = "Ipsum Lorem dolor sit amet",
       caption = "Source: ONS",
       x = "Year",
       y = "Value, £m",
       color = "Direction") +
  dbtplotr::theme_dbt()
```
A chart with the default ggplot theme:
&nbsp;  
<img src="/man/figures/without_dbtplotr.png"  width="75%" alt="Chart with default ggplot theme">

A chart using the DBT theme and analysis community colours:  
&nbsp;  
<img src="/man/figures/with_dbtplotr_DBT_colour.png"  width="75%" alt="Chart with DBT theme">  

### dbtplotr argument options 
**Legend**  
The legend will appear at the **bottom** of the chart by default.  
You can specify a different position (or remove it) using the legend argument eg: `dbtplotr::theme_dbt(legend = 'top')`.   

**Gridlines**
Only horizontal gridlines are included by default.  
You can remove or include other gridlines using the gridlines argument eg: `dbtplotr::theme_dbt(gridlines = 'none')`.  

**Void**
An option to remove all axis elements, equivalent to using theme_void(). `dbtplotr::theme_dbt(void = TRUE)`.  


### Adjusting the theme 
The dbtplotr theme should be suitable for most charts. However you can tweak the
theme by adding further `theme()` arguments **after** 'theme_dbt()`. For example:  
``` r
.... +  
dbtplotr::theme_dbt() +
theme(plot.title = element_text(colour = "#FF0000"))
```  


## Colour palettes
There are several different colour palettes available depending on your needs.  
These can be used by adding: `dbtplotr::colours_dbt(type = <chart-type>, palette = <palette-name>)` at the end of the ggplot code (see example below).

```r
ggplot(data = dbtplotr::sk_timeseries, aes(x = period, y = value, group = direction)) +
  geom_line(aes(color = direction)) +
  labs(title = "UK Trade with South Korea",
       subtitle = "Ipsum Lorem dolor sit amet",
       caption = "Source: ONS",
       x = "Year",
       y = "Value, £m",
       color = "Direction") +
  dbtplotr::theme_dbt() +
  dbtplotr::colours_dbt(type = 'line', palette = 'gaf')
```

The palettes that you can choose from are (palette names in quotes): 


### Contrasting

  **'dbt2'** : `#00285f`  `#a90083` 
  
  **'gaf'** : `#12436D`  `#F46A25` `#801650` `#28A197` `#3D3D3D` `#A285D1`
  
  **'gaf2'** : `#12436D` `#F46A25`
  
  **'gaf3'** : `#12436D` `#F46A25` `#801650`
    
Use this if your chart has different subjects (eg. multiple countries).       
Source: https://analysisfunction.civilservice.gov.uk/policy-store/codes-for-accessible-colours/

### Single colour scales
    
  **'dbtblue'**: `#00285f` `#a3abcc` `#777d96` `#a8adbe` `#d9ddea` `#d6dae1` `#06183b` `#0c0e21`
  
  **'dbtberry'**: `#a90083` `#c886b1` `#d979b7` `#7b005b` `#550137` `#d6dae1` `#f5dcec` `#dcb0cc`
  
  **'blues'** : `#12436D` `#0965A0` `#2493D9` `#7BC7F8` `#bdd7e7`
  
  **'reds'** : `#a50f15` `#de2d26` `#fb6a4a` `#fcae91` `#fee5d9`
    

Use this if your chart shows the same subject over time.  
Source: https://analysisfunction.civilservice.gov.uk/policy-store/codes-for-accessible-colours/ & https://colorbrewer2.org/

### Highlight one series
    
  **'highlight'** : `#12436D` `#BFBFBF`

Use this if your chart is busy and you want to highlight one series above others (eg. UK vs. other countries).   


### DBT corporate

  **'corporate'** : `#cf102d` `#00285F` `#004d44` `#4F0B7B` `#0063BE` `#E24912` `#A90083`

We advise against using this palette unless needed.   
Source: https://workspace.trade.gov.uk/working-at-dbt/policies-and-guidance/guidance/choose-the-right-dit-brand/    


### Custom colours
You can specify a custom colour string instead of a named palette. This must be comma separated and use hex. For example:  
```r
  dbtplotr::colours_dbt(type = 'line', palette = '#cf102d, #00285F, #004d44, #4F0B7B, #0063BE, #E24912, #A90083')
```


Here is a chart with the default **dbtplotr** theme and colours:  
&nbsp;  
<img src="/man/figures/with_dbtplotr_colour.png"  width="75%" alt="Chart with default dbtplotr theme and colours.">  
