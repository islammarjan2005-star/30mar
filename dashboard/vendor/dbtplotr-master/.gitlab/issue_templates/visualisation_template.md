# Visualisation checklist

Use this checklist to review or quality assure visualisations against accessibility and best practice guidelines. This checklist builds on Government Analysis function guidance on designing and publishing visualisations (https://analysisfunction.civilservice.gov.uk/policy-store/data-visualisation-charts/).

The DBT Data Visualisation packages are designed to implement these best practices and include colour palettes which meet WCAG 2.0 accessibility standards and are distinguishable in greyscale:
- [**dbtplotr** package for standardised colours and chart formats for ggplot2 and plotly](https://gitlab.data.trade.gov.uk/dbt-data-visualisation-library/dbtplotr)
- [**dbtplotpython** package for standardised colours and chart formats in Python](https://gitlab.data.trade.gov.uk/dbt-data-visualisation-library/dbtplotpython).

## General
- [ ] No shaded backgrounds, and unnecessary annotations, shapes, shadows, borders or patterns
- [ ] Suitable gridlines (light grey with a suggested maximum of 10)
- [ ] Maximum of 4 categories or series, where possible

## Text 
This applies to axes titles, annotations and labels.
- [ ] Text uses sans serif fonts
- [ ] Text is suitably large and legible (high contrast against background)
- [ ] Text is horizontal (axes are flipped if necessary to ensure horizontal text)

## Axes
- [ ] Y-axis values are right-aligned (units over units, tens over tens, hundreds over hundreds)
- [ ] Axes use appropriate units and formatting ie. commas to separate thousands
- [ ] Categorical data labels are aligned between tick marks
- [ ] Continuous data labels are aligned centrally on tick marks

## Colours
- [ ] Colours are only used where necessary to convey clearer message
- [ ] Consistent, variable-specific colours are used, where multiple related charts are used.
- [ ] Colours meet WCAG 2.0 accessibility standards (check [colour contrasts](https://webaim.org/resources/contrastchecker/))
- [ ] Colours are distinguishable in greyscale ie use hues instead of 
- [ ] Colours are well-suited to conveying the message (eg use highlight palette to emphasise category)
- [ ] Y-axis values are right-aligned (units over units, tens over tens, hundreds over hundreds)


Checklists for specific chart types are below:

## Bar charts
<details><summary>Click to expand</summary>
    
- [ ] Bars ranked in ascending or descending order, otherwise by sensible variable eg date.
- [ ] Gaps between bars are narrower than the width of a single bar
- [ ] Legends are presented in the same order and orientation as bars in the chart 
- [ ] Where a clustered bar chart is used, the gap between clusters is wider than a single bar.
</details>

## Line charts
<details><summary>Click to expand</summary>
    
- [ ] Bars ranked in ascending or descending order, otherwise by sensible variable eg date.
- [ ] Lines are suitably thick
- [ ] Where a broken axis is used, it is clearly marked
</details>

## Pie charts
<details><summary>Click to expand</summary>
    
- [ ] Categories displayed have notably different sizes (use bar charts if categories are similarly sized)
- [ ] Categories add up to a distinct 'whole', otherwise use a bar chart where elements are duplicated
- [ ] Maximum of 5 categories.
- [ ] Each colour has at least a 3:1 contrast ratio with the two other colours it isnext to.
</details>


