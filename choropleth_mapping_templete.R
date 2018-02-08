library(rgeos)
library(rgdal)
library(raster)
library(ggplot2)
library(viridis)
library(dplyr)
library(gtable)
library(grid)
library(stringi)
library(ggmap)
library(readr)
library(tidyr)
library(broom)

# Raw data import and initial crime filter for our relevant time period.
raw_data <- read_rds("lou_crime_geocoded_df.rds.gzip")

create_date_variables <- function(df){
  require(lubridate)
  # Uses the POSIXct date_occured variable to create useful date related
  # subvariables
  df$year <- year(df$date_occured)
  df$month <- month(df$date_occured)
  df$day <- day(df$date_occured)
  df$hour <- hour(df$date_occured)
  df$year_month <- paste(df$year, df$month, sep = '-')
  df$day_of_week <- wday(df$date_occured, label = TRUE, abbr = FALSE)
  df$weekday <- ifelse(df$day_of_week == "Saturday" | df$day_of_week == "Sunday", 
                       "Weekend", "Weekday")
  df$yday <- yday(df$date_occured)
  
  return(df)
}

crime_lou <- create_date_variables(raw_data)
# Filter out records with no lat/lng coords and those from desired time period
crime_lou <- crime_lou%>%
  filter(year <=2016 & year >=2005 & !is.na(lat)  & !is.na(lng))

# Heroin based filtering and tidying
heroin_offenses <- crime_lou%>%
  filter(stri_detect_regex(uor_desc, "heroin"))

heroin_zip <- heroin_offenses%>%
  group_by(zip_code, year)%>%
  summarise(value = n())%>%
  rename(region = zip_code)%>%
  spread(key = year, value = value)%>%
  gather(year, value, -region)

#Dealing with NA values
heroin_zip$value <- ifelse(is.na(heroin_zip$value) == TRUE, 0, heroin_zip$value)


# Creating map object
map <- readOGR("Jefferson_County_KY_ZIP_Codes.shp")

map@data$id <- rownames(map@data)
map_df <- tidy(map, region = 'id')
map_df <- left_join(map_df, map@data, by = 'id')

# Testing heroin by zip ggplot
heroin_map_df <- full_join(map_df, heroin_zip, by= c('ZIPCODE' = 'region'))


theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Ubuntu Regular", color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#f5f5f2", color = NA), 
      panel.background = element_rect(fill = "#f5f5f2", color = NA), 
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.border = element_blank(),
      ...
    )
}

heroin_map_16 <- heroin_map_df%>%
  filter(year == 2016)

ggplot()+
  geom_polygon(data = heroin_map_16,
               aes(x = long, y = lat, group = group, fill = value))+
  geom_path(data = heroin_map_16, aes(x = long, y = lat, group = group),
            color = 'black', size = 0.1)+
  coord_equal()+
  theme_map()+
  scale_fill_viridis(option = 'magma', alpha = 0.5, direction = -1, name = "Heroin Offenses",
                     guide = guide_colorbar(
                       direction = 'horizontal',
                       barheight = unit(2, units = 'mm'),
                       barwidth = unit(50, units = 'mm'),
                       draw.ulim = F,
                       title.position = 'top',
                       title.hjust = 0.5,
                       label.hjust = 0.5
                     ))+
  theme(legend.position = 'bottom')+
  labs(title = "Louisville's Heroin Epidemic",
       subtitle = "Heroin Offenses by Zip Code, 2016")

no_classes <- 6
labels <- c()

quantiles <- quantile(heroin_map_16$value, 
                      probs = seq(0, 1, length.out = no_classes + 1))
for(idx in 1:length(quantiles)){
  labels <- c(labels, paste0(round(quantiles[idx], 2),
                             ' - ',
                             round(quantiles[idx + 1], 2)))
}

labels <- labels[1:length(labels)-1]

heroin_map_16$value_quantiles <- cut(heroin_map_16$value,
                                    breaks = quantiles,
                                    labels = labels, 
                                    include.lowest = TRUE)

ggplot()+
  geom_polygon(data = heroin_map_16, aes(fill = value_quantiles,
                                        x = long,
                                        y = lat, 
                                        group = group))+
  geom_path(data = heroin_map_16, aes(x = long,
                                     y = lat, 
                                     group = group),
            color = 'white', size = 0.1)+
  coord_equal()+
  theme_map()+
  labs(title = "Louisville's Heroin Epidemic",
       subtitle = "Heroin Offenses by Zip Code, 2016")+
  scale_fill_viridis(
    option = 'magma',
    name = "Heroin Offenses",
    discrete = TRUE,
    direction = -1,
    guide = guide_legend(
      keyheight = unit(5, units = 'mm'),
      title.position = 'top',
      reverse = TRUE
    )
  )

# Discrete breaks
discrete_breaks <- c(7, 16, 27, 39, 83)
minVal <- min(heroin_map_16$value, na.rm = T)  
maxVal <- max(heroin_map_16$value, na.rm = T)

#form labels
labels <- c()
brks <- c(minVal, discrete_breaks, maxVal)

for(idx in 1:length(brks)){
  labels <- c(labels,round(brks[idx + 1], 2))
}

labels <- labels[1:length(labels)-1]
# new variable in dataset based on these
heroin_map_16$brks <- cut(heroin_map_16$value,
                         breaks = brks,
                         include.lowest = TRUE,
                         labels = labels)
brks_scale <- levels(heroin_map_16$brks)
labels_scale <- rev(brks_scale)


q <- ggplot()+
  geom_polygon(data = heroin_map_16, aes(fill = brks,
                                        x = long, 
                                        y = lat, 
                                        group = group))+
  geom_path(data = heroin_map_16, aes(x = long,
                                     y = lat, 
                                     group = group),
            color = 'white', size = 0.1)+
  coord_equal()+
  theme_map()+
  theme(legend.position = 'bottom')+
  labs(title = "Louisville's Heroin Epidemic",
       subtitle = "Heroin Offenses by Zip Code, 2016")+
  scale_fill_manual(
    values = rev(magma(6)),
    breaks = rev(brks_scale),
    name = "Heroin Offenses",
    drop = FALSE,
    labels = labels_scale,
    guide = guide_legend(
      direction = 'horizontal',
      keyheight = unit(2, units = 'mm'),
      keywidth = unit(70/length(labels), units = 'mm'),
      title.position = 'top',
      title.hjust = 0.5,
      nrow = 1,
      byrow = T,
      label.hjust = 1,
      reverse = T,
      label.position = 'bottom'
    )
  )
library(gtable)
library(grid)
extendLegendWithExtremes <- function(p){
  p_grob <- ggplotGrob(p)
  legend <- gtable_filter(p_grob, "guide-box")
  legend_grobs <- legend$grobs[[1]]$grobs[[1]]
  # grab the first key of legend
  legend_first_key <- gtable_filter(legend_grobs, "key-3-1-1")
  legend_first_key$widths <- unit(2, units = "cm")
  # modify its width and x properties to make it longer
  legend_first_key$grobs[[1]]$width <- unit(2, units = "cm")
  legend_first_key$grobs[[1]]$x <- unit(0.15, units = "cm")
  
  # last key of legend
  legend_last_key <- gtable_filter(legend_grobs, "key-3-6-1")
  legend_last_key$widths <- unit(2, units = "cm")
  # analogous
  legend_last_key$grobs[[1]]$width <- unit(2, units = "cm")
  legend_last_key$grobs[[1]]$x <- unit(1.02, units = "cm")
  
  # grab the last label so we can also shift its position
  legend_last_label <- gtable_filter(legend_grobs, "label-5-6")
  legend_last_label$grobs[[1]]$x <- unit(2, units = "cm")
  
  # Insert new color legend back into the combined legend
  legend_grobs$grobs[legend_grobs$layout$name == "key-3-1-1"][[1]] <- 
    legend_first_key$grobs[[1]]
  legend_grobs$grobs[legend_grobs$layout$name == "key-3-6-1"][[1]] <- 
    legend_last_key$grobs[[1]]
  legend_grobs$grobs[legend_grobs$layout$name == "label-5-6"][[1]] <- 
    legend_last_label$grobs[[1]]
  
  # finally, I need to create a new label for the minimum value 
  new_first_label <- legend_last_label$grobs[[1]]
  new_first_label$label <- round(min(heroin_map_16$value, na.rm = T), 2)
  new_first_label$x <- unit(-0.15, units = "cm")
  new_first_label$hjust <- 1
  
  legend_grobs <- gtable_add_grob(legend_grobs, 
                                  new_first_label, 
                                  t = 6, 
                                  l = 2, 
                                  name = "label-5-0", 
                                  clip = "off")
  legend$grobs[[1]]$grobs[1][[1]] <- legend_grobs
  p_grob$grobs[p_grob$layout$name == "guide-box"][[1]] <- legend
  
  # the plot is now drawn using this grid function
  grid.newpage()
  grid.draw(p_grob)
}
extendLegendWithExtremes(q)


p <- ggplot()+
  geom_polygon(data = heroin_map_16, aes(fill = brks,
                                        x = long, 
                                        y = lat, 
                                        group = group))+
  geom_path(data = heroin_map_16, aes(x = long,
                                     y = lat, 
                                     group = group),
            color = 'white', size = 0.1)+
  coord_equal()+
  theme_map()+
  theme(
    legend.position = c(0.5, 0.03),
    legend.text.align = 0,
    legend.background = element_rect(fill = alpha('white', 0.0)),
    legend.text = element_text(size = 7, hjust = 0, color = "#4e4d47"),
    plot.title = element_text(hjust = 0.5, color = "#4e4d47"),
    plot.subtitle = element_text(hjust = 0.5, color = "#4e4d47", 
                                 margin = margin(b = -0.1, 
                                                 t = -0.1, 
                                                 l = 2, 
                                                 unit = "cm"), 
                                 debug = F),
    legend.title = element_text(size = 8),
    # plot.margin = unit(c(.5,.5,.2,.5), "cm"),
    panel.spacing = unit(c(-.1,0.2,.2,0.2), "cm"),
    panel.border = element_blank(),
    plot.caption = element_text(size = 6, 
                                hjust = 0.92, 
                                margin = margin(t = 0.2, 
                                                b = 0, 
                                                unit = "cm"), 
                                color = "#939184")
  ) +
  labs(title = "Louisville's Heroin Epidemic",
       subtitle = "Heroin Offenses by Zip Code, 2016")+
  scale_fill_manual(
  # magma with 8 classes
  values = rev(magma(8, alpha = 0.7)[2:7]),
  breaks = rev(brks_scale),
  name = "Heroin Offenses",
  drop = FALSE,
  labels = labels_scale,
  guide = guide_legend(
    direction = 'horizontal',
    keyheight = unit(2, units = 'mm'),
    keywidth = unit(70/length(labels), units = 'mm'),
    title.position = 'top',
    title.hjust = 0.5,
    label.hjust = 1,
    nrow = 1,
    byrow = TRUE,
    reverse = TRUE,
    label.position = 'bottom'
  )
)

extendLegendWithExtremes(p)

no_classes <- 6
labels <- c()

quantiles <- quantile(heroin_map_1516$value, 
                      probs = seq(0, 1, length.out = no_classes + 1))
# Discrete breaks
discrete_breaks <- c(7, 16, 30, 43, 81)
minVal <- min(heroin_map_16$value, na.rm = T)  
maxVal <- max(heroin_map_16$value, na.rm = T)

#form labels
labels <- c()
brks <- c(minVal, discrete_breaks, maxVal)

for(idx in 1:length(brks)){
  labels <- c(labels,round(brks[idx + 1], 2))
}

labels <- labels[1:length(labels)-1]
# new variable in dataset based on these
heroin_map_16$brks <- cut(heroin_map_16$value,
                          breaks = brks,
                          include.lowest = TRUE,
                          labels = labels)
brks_scale <- levels(heroin_map_16$brks)
labels_scale <- rev(brks_scale)



p <- ggplot()+
  geom_polygon(data = heroin_map_1516, aes(fill = brks,
                                         x = long, 
                                         y = lat, 
                                         group = group))+
  geom_path(data = heroin_map_1516, aes(x = long,
                                      y = lat, 
                                      group = group),
            color = 'white', size = 0.1)+
  coord_equal()+
  theme_map()+
  theme(
    legend.position = c(0.5, 0.03),
    legend.text.align = 0,
    legend.background = element_rect(fill = alpha('white', 0.0)),
    legend.text = element_text(size = 7, hjust = 0, color = "#4e4d47"),
    plot.title = element_text(hjust = 0.5, color = "#4e4d47"),
    plot.subtitle = element_text(hjust = 0.5, color = "#4e4d47", 
                                 margin = margin(b = -0.1, 
                                                 t = -0.1, 
                                                 l = 2, 
                                                 unit = "cm"), 
                                 debug = F),
    legend.title = element_text(size = 8),
    # plot.margin = unit(c(.5,.5,.2,.5), "cm"),
    panel.spacing = unit(c(-.1,0.2,.2,0.2), "cm"),
    panel.border = element_blank(),
    plot.caption = element_text(size = 6, 
                                hjust = 0.92, 
                                margin = margin(t = 0.2, 
                                                b = 0, 
                                                unit = "cm"), 
                                color = "#939184")
  ) +
  labs(title = "Louisville's Heroin Epidemic",
       subtitle = "Heroin Offenses by Zip Code, 2016")+
  scale_fill_manual(
    # magma with 8 classes
    values = rev(magma(8, alpha = 0.7)[2:7]),
    breaks = rev(brks_scale),
    name = "Heroin Offenses",
    drop = FALSE,
    labels = labels_scale,
    guide = guide_legend(
      direction = 'horizontal',
      keyheight = unit(2, units = 'mm'),
      keywidth = unit(70/length(labels), units = 'mm'),
      title.position = 'top',
      title.hjust = 0.5,
      label.hjust = 1,
      nrow = 1,
      byrow = TRUE,
      reverse = TRUE,
      label.position = 'bottom'
    )
  )

extendLegendWithExtremes <- function(p){
  p_grob <- ggplotGrob(p)
  legend <- gtable_filter(p_grob, "guide-box")
  legend_grobs <- legend$grobs[[1]]$grobs[[1]]
  # grab the first key of legend
  legend_first_key <- gtable_filter(legend_grobs, "key-3-1-1")
  legend_first_key$widths <- unit(2, units = "cm")
  # modify its width and x properties to make it longer
  legend_first_key$grobs[[1]]$width <- unit(2, units = "cm")
  legend_first_key$grobs[[1]]$x <- unit(0.15, units = "cm")
  
  # last key of legend
  legend_last_key <- gtable_filter(legend_grobs, "key-3-6-1")
  legend_last_key$widths <- unit(2, units = "cm")
  # analogous
  legend_last_key$grobs[[1]]$width <- unit(2, units = "cm")
  legend_last_key$grobs[[1]]$x <- unit(1.02, units = "cm")
  
  # grab the last label so we can also shift its position
  legend_last_label <- gtable_filter(legend_grobs, "label-5-6")
  legend_last_label$grobs[[1]]$x <- unit(2, units = "cm")
  
  # Insert new color legend back into the combined legend
  legend_grobs$grobs[legend_grobs$layout$name == "key-3-1-1"][[1]] <- 
    legend_first_key$grobs[[1]]
  legend_grobs$grobs[legend_grobs$layout$name == "key-3-6-1"][[1]] <- 
    legend_last_key$grobs[[1]]
  legend_grobs$grobs[legend_grobs$layout$name == "label-5-6"][[1]] <- 
    legend_last_label$grobs[[1]]
  
  # finally, I need to create a new label for the minimum value 
  new_first_label <- legend_last_label$grobs[[1]]
  new_first_label$label <- round(min(heroin_map_1516$value, na.rm = T), 2)
  new_first_label$x <- unit(-0.15, units = "cm")
  new_first_label$hjust <- 1
  
  legend_grobs <- gtable_add_grob(legend_grobs, 
                                  new_first_label, 
                                  t = 6, 
                                  l = 2, 
                                  name = "label-5-0", 
                                  clip = "off")
  legend$grobs[[1]]$grobs[1][[1]] <- legend_grobs
  p_grob$grobs[p_grob$layout$name == "guide-box"][[1]] <- legend
  
  # the plot is now drawn using this grid function
  grid.newpage()
  grid.draw(p_grob)
}
extendLegendWithExtremes(p)
