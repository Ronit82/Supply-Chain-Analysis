my_essential_packages<-c("tidyverse", "data.table", "dplyr", "readr", "skimr",
                         "tibble", "tidyr", "purrr", "stringr", "forcats", 
                         "janitor", "Tmisc", "class", "lubridate",
                         "forecast", "timetk", "zoo",  "ggplot2",  
                         "highcharter", "gganimate", "geonames", "ggmap", "maps", "plotly", 
                         "RColorBrewer" ,"leaflet", "htmltools", "htmlwidgets")

invisible(lapply(my_essential_packages, require, character.only = TRUE, quietly = TRUE))
install.packages("highcharter")
library(highcharter)

products<-fread("D:/downloads/archive (6)/supply_chain_data.csv", stringsAsFactors = FALSE)


  
  # Product type: Sales and Availability
  
  
product_type<-products %>% 
  select(`Product type`, Availability) %>% 
  group_by(`Product type`) %>% 
  summarise(across(everything(), sum, na.rm = TRUE), .groups = 'drop')

products_sold_per_product_type<-products %>% 
  select(`Product type`, `Number of products sold`) %>% 
  group_by(`Product type`) %>% 
  summarise(across(everything(), sum, na.rm=TRUE), .groups='drop')

products_sold_percent <- products_sold_per_product_type %>%
  group_by(`Product type`) %>%
  summarise(percentage = sum(`Number of products sold`) / sum(products_sold_per_product_type$`Number of products sold`) * 100) %>% 
  mutate(percentage = round(percentage, 2))



products_sold_per_product_type %>% 
  hchart(type = "pie", hcaes(x=`Product type`, y=`Number of products sold`),showInLegend=TRUE, legend= list(layout= "vertical", align= "right", verticalAlign= "middle", borderWidth= 0.5)) %>% 
  hc_title(text = 'Number of Products Sold') %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_chart(options3d=list(enabled=TRUE, alpha=45, beta=0)) %>% 
  hc_plotOptions(pie=list(innerSize= 100, 
                          depth= 45))

products_sold_percent %>% 
  hchart(type = "pie", hcaes(x=`Product type`, y=percentage), showInLegend=TRUE, legend= list(layout= "vertical", align= "right", verticalAlign= "middle", borderWidth= 0.5)) %>% 
  hc_title(text = 'Percentage of Products Sold') %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_chart(options3d=list(enabled=TRUE, alpha=45, beta=0)) %>% 
  hc_plotOptions(pie=list(innerSize= 100, 
                          depth= 45,
                          dataLabels=list(format='{point.percentage:.1f}%'),
                          tooltip=list(pointFormat='{point.y:.1f}%')))

product_type%>% 
  hchart(type = "bar", hcaes(x=`Product type`, y=`Availability`)) %>% 
  hc_title(text = 'Number of Products') %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_chart(options3d=list(enabled=TRUE, alpha=45, beta=0))


customer_demographics<-products %>% 
  select(`Customer demographics`) %>% 
  group_by(`Customer demographics`) %>% 
  summarise(Total=n()) %>% 
  arrange(desc(Total))


customer_demographics %>% 
  hchart(type = "column", hcaes(x=`Customer demographics`, y=Total)) %>% 
  hc_title(text = 'Customer Demographics') %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_chart(options3d=list(enabled=TRUE, alpha=25, beta=0))


  
  # Customer demographics and product type correlation
 
corr<-products %>% 
  select(`Customer demographics`, `Product type`, `Number of products sold`, ) %>% 
  group_by(`Customer demographics`, `Product type`) %>% 
  summarise(across(everything(), sum, na.rm = TRUE), .groups = 'drop')

hchart(corr, "heatmap", hcaes(x = `Customer demographics`, y = `Product type`, value = `Number of products sold`)) %>% 
  hc_colorAxis(
    stops = color_stops(5, c("#ffffd9", "#edf8b1", "#c7e9b4", "#7fcdbb", "#41b6c4", "#2c7fb8", "#253494")),
    labels = list(format = "{value}")
  ) %>% 
  hc_xAxis(title = list(text = "Customer Demographics")) %>% 
  hc_yAxis(title = list(text = "Product Type")) %>% 
  hc_title(text = "Heatmap of Number of Products Sold")
```

  
  # Revenue
  
revenue <- products %>% 
  select(`Product type`, `Revenue generated`) %>% 
  group_by(`Product type`) %>% 
  summarise(across(everything(), ~round(sum(., na.rm = TRUE), 2)), .groups = 'drop') %>% 
  arrange(desc(`Revenue generated`))


revenue %>% 
  hchart(type = "column", hcaes(x=`Product type`, y=`Revenue generated`)) %>% 
  hc_title(text = 'Total Revenue Generated') %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_chart(options3d=list(enabled=TRUE, alpha=25, beta=0))


  # SKU Performance
 
sku_revenue <- products %>%
  group_by(SKU) %>%
  summarize(Total.revenue = sum(`Revenue generated`)) %>% 
  arrange(desc(Total.revenue))


highchart() %>%
  hc_title(text = "Total Revenue by Product SKU") %>%
  hc_xAxis(categories = sku_revenue$SKU) %>%
  hc_yAxis(title = list(text = "Total Revenue")) %>%
  hc_add_series(name = "SKU", data = sku_revenue$Total.revenue) %>%
  hc_plotOptions(column = list(stacking = "normal")) %>%
  hc_colors(c("blue")) %>%
  hc_tooltip(formatter = JS("function(){return '<b>'+ this.x +'</b><br/>'+'Total Revenue: ₹'+ Highcharts.numberFormat(this.y, 0);}")) %>%
  hc_chart(type = "column")


  
  # Order quantity vs Revenue Generated
  
order_revenue<-products %>% 
  select(`Product type`, `Order quantities`, `Revenue generated`) %>% 
  group_by(`Product type`) %>% 
  summarise(across(everything(), ~round(sum(., na.rm = TRUE), 2)), .groups = 'drop')


order_revenue %>%
  hchart(type = "column", hcaes(x=`Product type`, y=`Revenue generated`, group=`Order quantities`)) %>%
  hc_title(text = 'Number of Orders and Their Revenue') %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_chart(options3d=list(enabled=TRUE, alpha=25, beta=0)) %>% 
  hc_tooltip(pointFormat = "Total Revenue: <b>₹ {point.y:,.2f}</b>")

  
  # City and Product Type
  
city_product<-products %>% 
  select(`Product type`, Location, `Number of products sold`) %>% 
  group_by(`Product type`, Location) %>% 
  summarise(across(everything(), sum, na.rm = TRUE), .groups = 'drop') %>% 
  arrange(desc(`Number of products sold`))

city_product %>% 
  hchart(type = "column", hcaes(x=`Product type`, y=`Number of products sold`, group=Location)) %>% 
  hc_title(text = 'Number of Products Sold Per City') %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_chart(options3d=list(enabled=TRUE, alpha=25, beta=0))

  # Manufacturing cost
  
manufacturing_cost<-products %>% 
  select(`Product type`, `Manufacturing costs`) %>% 
  group_by(`Product type`) %>% 
  summarise(across(everything(), ~round(sum(., na.rm = TRUE), 2)), .groups = 'drop')

manufacturing_cost %>% 
  hchart(type = "pie", hcaes(x=`Product type`, y=`Manufacturing costs`)) %>% 
  hc_title(text = 'Total Manufacturing Cost of Each Product Type') %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_chart(options3d=list(enabled=TRUE, alpha=45, beta=0)) %>% 
  hc_plotOptions(pie=list(innerSize= 100, 
                          depth= 45))

  
  # Correlation of Supply Chain Variables
  
## Subset the data to include only the relevant columns
supply_chain_data <- products %>% 
  select(`Lead times`, `Stock levels`, `Order quantities`)

## Calculate the correlation matrix
corr_matrix <- cor(supply_chain_data)

## Create a heatmap of the correlation matrix
hchart(corr_matrix, "heatmap", 
       colorAxis = list(minColor = "#FFFFFF", maxColor = "#0365C0"),
       yAxis = list(title = ""),
       xAxis = list(title = ""),
       tooltip = list(valueDecimals = 2)) %>% 
  hc_title(text = "Correlation Heatmap of Supply Chain Variables")
