## Data Exploration and Statistics ##
## For SU Master's Thesis in Landscape Ecology
## Emma Gemal
## Last updated 10/2/23

#### Library ----
library(MASS)   # for Box-Cox transformation 
library(tidyverse)
library(plotrix)  # for 'std.error()'
library(GGally)  # for 'ggcorr()'
library(lmtest)
library(lme4)
library(vegan)
library(geosphere)  # for 'mantel()' test 
library(indicspecies)   # for indicator sp. test 

#### Data Import ----
sites <- read.csv("Data/site_conditions.csv")
plots <- read.csv("Data/plots_tidy.csv")
coverage <- read.csv("Data/coverage.csv")

# GIS variables
ndvi <- read.csv("Data/NDVI_plots.csv")
slope <- read.csv("Data/slope.csv")
soildepth <- read.csv("Data/soildepth.csv")
wetness <- read.csv("Data/wetness.csv")

?TukeyHSD()
#### Data Exploration & Cleaning ----
view(sites)
view(plots)
view(coverage)
view(ndvi)
view(slope)
view(soildepth)
view(wetness)

str(sites)   # change date to date, plot to cat, and maybe x and y coord to coordinates
str(plots)   # change date to date and check for spelling mistakes and duplicates
str(coverage)   # change date to date

str(ndvi)   # make plot_nr categorical and combine with others
str(slope)   # make plot_nr categorical and combine with others
str(soildepth)   # make plot_nr categorical and combine with others
str(wetness)   # make plot_nr categorical and combine with others


### Editing import format
# Main variables 
sites <- sites %>% 
            mutate(date = as.Date(date, format = "%d/%m/%y"),
                   plot = as.character(plot),
                   plot_nr = as.character(plot_nr),
                   site_nr = as.character(site_nr)) %>% 
            mutate(aspect = case_when(grepl("S", site) ~ "S",
                                      grepl("N", site) ~ "N"),
                   grazing_cat = substr(site, 1, 1)) %>%   # adding aspect
            mutate(grazing_m = grazing_s/60)    # grazing in minutes 
str(sites)            

plots <- plots %>% 
            mutate(date = as.Date(date, format = "%d/%m/%y"),
                   plot_nr = as.character(plot_nr),
                   site_nr = as.character(site_nr))
str(plots)

coverage <- coverage %>%  
              mutate(date = as.Date(date, format = "%d/%m/%y"),
                     plot_rep = as.character(plot_rep),
                     plot_nr = as.character(plot_nr),
                     site_nr = as.character(site_nr)) 
print(coverage %>% group_by(plot_nr) %>% summarize(sum(coverage_perc)), n = 105)  # all att 100%
str(coverage) 

# GIS variables
gisvar <- left_join(ndvi, slope)
gisvar <- left_join(gisvar, wetness)
gisvar <- left_join(gisvar, soildepth)

str(gisvar)
gisvar <- gisvar %>% 
            mutate(plot_nr = as.character(plot_nr))
str(gisvar)


### Checking species 
sort(unique(plots$sp_common))  # remove whitespace after words 
                               # "purple mountain heather" & "purple mountain-heath" 

sort(unique(plots$sp_latin))  # some don't have a latin name... not good 
                              # remove whitespace after words
                              # Euphrasia stricta var. tenuis & Euphrasia stricta var. tenius
                              # "Peltigera aphthosa" & "Peltigera apthosa" 
                              # "Pleurozium schereberi" & "Pleurozium schreberi"
                              # "Stereocaulon alpinus" & "Stereocaulon alpinum"
                              # "Stereocaulon sp," & "Stereocaulon sp."
                              # "Vaxinium uliginosum" & "Vaccinium uliginosum"
                              # "Pilocella sp." & "Pilocello sp." (should be Pilosella)

unique(coverage$sp_group)  # empty row is seen as own character 
coverage <- coverage %>% na.omit()   # remove empty row

print(coverage %>% group_by(plot_nr, sp_group) %>% summarise(l = length(sp_group)) %>% 
        filter(l > 1), n = 500)

unique(plots$sp_group)

### Editing species that had mistakes 
plots <- plots %>% 
            mutate(sp_common = ifelse(sp_common == "purple mountain-heath", 
                                      "purple mountain heather",
                                      sp_common)) %>% 
            mutate(sp_common = str_trim(sp_common, side = "right")) %>% 
            mutate(sp_latin = str_trim(sp_latin, side = "right")) %>% 
            mutate(sp_latin = ifelse(sp_latin == "Peltigera apthosa", "Peltigera aphthosa",
                                     sp_latin),
                   sp_latin = ifelse(sp_latin == "Euphrasia stricta var. tenius", 
                                     "Euphrasia stricta var. tenuis", sp_latin),
                   sp_latin = ifelse(sp_latin == "Pleurozium schereberi", "Pleurozium schreberi",
                                     sp_latin),
                   sp_latin = ifelse(sp_latin == "Stereocaulon alpinus", "Stereocaulon alpinum",
                                     sp_latin),
                   sp_latin = ifelse(sp_latin == "Stereocaulon sp,", "Stereocaulon sp.",
                                     sp_latin),
                   sp_latin = ifelse(sp_latin == "Vaxinium uliginosum", "Vaccinium uliginosum",
                                     sp_latin),
                   sp_latin = ifelse(sp_latin == "Pilocello sp.", "Pilosella sp.", sp_latin),
                   sp_latin = ifelse(sp_latin == "Pilocella sp.", "Pilosella sp.", sp_latin))

plots[(plots$sp_latin == ""), ]   # no latin name because they are "unknown"
plots <- plots %>%  
            mutate(sp_latin = ifelse(sp_latin == "", "unknown", sp_latin))

plots %>% group_by(plot_nr, sp_latin) %>% summarise(x = length(sp_latin)) %>% filter(x > 1)
 # no duplicate names 


# . ----
#### Variable Creation ----
### Species Richness & Proportion
plots <- plots %>% 
            group_by(plot_nr) %>% 
            mutate(richness_plot = length(unique(sp_latin))) %>%   # overall per plot
            ungroup() %>% 
            group_by(site_nr) %>% 
            mutate(richness_site = length(unique(sp_latin))) %>%   # overall per site (no reps)
            ungroup() %>% 
            group_by(plot_nr, sp_group) %>% 
            mutate(richness_group = length(unique(sp_latin))) %>%   # per group per plot 
            ungroup()

richness <- plots %>% 
              group_by(plot_nr, site_nr, sp_group) %>% 
              summarize(richness_plot = mean(richness_plot),
                        richness_site = mean(richness_site),
                        richness_group = mean(richness_group),
                        height_site = mean(avg_height)) %>% 
              ungroup() %>% 
              group_by(plot_nr) %>% 
              mutate(rich_group_prop = (richness_group/sum(richness_group))) %>%  # proportion
              ungroup() %>%
              group_by(site_nr) %>% 
              mutate(richness_siteT = mean(richness_plot)) %>%  # true richness per site (with reps)
              ungroup() %>% 
              group_by(sp_group, site_nr) %>% 
              mutate(rich_propT = mean(rich_group_prop)) %>% 
              mutate(rich_prop_perc = rich_propT*100)   

### GIS Standardization
gisvar <- gisvar %>% 
            mutate(across(c("ndvi":"soil_depth"), 
                           .fns = list(s = scale))) %>% 
            mutate(ndvi_s = as.vector(ndvi_s),
                   slope_deg_s = as.vector(slope_deg_s),
                   wetness_s = as.vector(wetness_s),
                   soil_depth_s = as.vector(soil_depth_s))


### Combining dataframes ----
## Combining 'richness' with 'sites' to add grazing data 
grazing <- sites %>% dplyr::select(site_nr, plot_nr, plot, aspect, grazing_m)
grazing <- grazing %>% 
              mutate(grazing_m_s = as.vector(scale(grazing_m)))  # standardizing grazing
              
str(grazing)

richness <- left_join(richness, grazing)
richness <- as.data.frame(richness)
str(richness)

## Adding grazing data to 'coverage' 
coverage <- coverage %>% rename(plot = plot_rep)
str(coverage)

coverage <- left_join(coverage, grazing)
str(coverage)

## Adding GIS variables to data
richness <- left_join(richness, gisvar)
coverage <- left_join(coverage, gisvar)

str(richness)
str(coverage)

## Only grazing and GIS variables 
plot_grazing <- left_join(sites, gisvar)
str(plot_grazing)

plot_grazing <- plot_grazing %>% 
                    dplyr::select(site_nr, plot_nr, grazing_m, aspect, ndvi, slope_deg, wetness,
                                  soil_depth, ndvi_s, wetness_s, slope_deg_s, soil_depth_s)


### Creating summary dataframes ----
coverage_site <- coverage %>% 
                    group_by(site_nr) %>% 
                    mutate(ndvi = mean(ndvi),
                           slope_deg = mean(slope_deg),
                           wetness = mean(wetness),
                           soil_depth = mean(soil_depth)) %>% 
                    group_by(site_nr, sp_group, aspect) %>% 
                    summarize(coverage_cm2 = mean(coverage_cm2),
                              coverage_perc = mean(coverage_perc),
                              coverage_prop = coverage_perc/100,
                              ndvi = mean(ndvi),
                              slope_deg = mean(slope_deg),
                              wetness = mean(wetness),
                              soil_depth = mean(soil_depth),
                              grazing_m = mean(grazing_m))
coverage_site <- as.data.frame(coverage_site)
str(coverage_site)


richness_site <- richness %>% 
                    group_by(site_nr) %>% 
                    mutate(ndvi = mean(ndvi),
                           slope_deg = mean(slope_deg),
                           wetness = mean(wetness),
                           soil_depth = mean(soil_depth),
                           ndvi_s = mean(ndvi_s),
                           slope_deg_s = mean(slope_deg_s),
                           wetness_s = mean(wetness_s),
                           soil_depth_s = mean(soil_depth_s),
                           grazing_m = mean(grazing_m),
                           grazing_m_s = mean(grazing_m_s)) %>% 
                    group_by(site_nr, sp_group, aspect) %>% 
                    summarise(richness_siteT = mean(richness_siteT),
                              height_site = mean(height_site),
                              richness_group = mean(richness_group),
                              rich_propT = mean(rich_propT),
                              rich_prop_perc = mean(rich_prop_perc),
                              ndvi = mean(ndvi),
                              slope_deg = mean(slope_deg),
                              wetness = mean(wetness),
                              soil_depth = mean(soil_depth),
                              ndvi_s = mean(ndvi_s),
                              slope_deg_s = mean(slope_deg_s),
                              wetness_s = mean(wetness_s),
                              soil_depth_s = mean(soil_depth_s),
                              grazing_m = mean(grazing_m),
                              grazing_m_s = mean(grazing_m_s))
  
# without sp group
richness_site2 <- richness_site %>% 
                    dplyr::select(!c(richness_group, rich_propT)) %>% 
                    group_by(site_nr, aspect) %>% 
                    summarise(richness_siteT = mean(richness_siteT),
                              height_site = mean(height_site),
                              ndvi = mean(ndvi),
                              slope_deg = mean(slope_deg),
                              wetness = mean(wetness),
                              soil_depth = mean(soil_depth),
                              ndvi_s = mean(ndvi_s),
                              slope_deg_s = mean(slope_deg_s),
                              wetness_s = mean(wetness_s),
                              soil_depth_s = mean(soil_depth_s),
                              grazing_m = mean(grazing_m),
                              grazing_m_s = mean(grazing_m_s))



# . ----
#### Initial Visualizations ----
## Richness
ggplot(richness, aes(x = grazing_m, y = richness_plot)) +   # richness with grazing 
  geom_point()

ggplot(richness, aes(x = grazing_m, y = richness_plot)) +   # N and S accounted for 
  geom_point(aes(color = aspect)) +
  stat_smooth(method = "lm", aes(color = aspect))

ggplot(richness_site, aes(x = grazing_m, y = richness_siteT)) +   # averaged across the plots
  geom_point()

# use this one for final 
ggplot(richness_site, aes(x = grazing_m, y = richness_siteT)) +   # averaged and aspect accounted for
  geom_point(aes(color = aspect)) +
  stat_smooth(method = "lm", aes(color = aspect))

ggplot(richness_site, aes(x = grazing_m, y = rich_propT)) +   # how proportions of groups changes
  geom_point(aes(color = sp_group)) +
  stat_smooth(method = "lm", aes(color = sp_group))

# use this one for final? or without aspect
ggplot(richness_site, aes(x = grazing_m, y = rich_propT)) +   # aspect accounted for 
  geom_point(aes(color = aspect)) +
  stat_smooth(method = "lm", aes(color = aspect, fill = aspect)) +   
  facet_wrap(~sp_group, scales = "free_y")

ggplot(richness, aes(x = aspect, y = rich_propT)) +   # relative species nr for each group 
  geom_boxplot(aes(fill = sp_group))                       # for N vs. S 

ggplot(richness_site, aes(x = aspect, y = richness_siteT)) +   # averaged richness N vs. S
  geom_boxplot()

ggplot(richness_site, aes(grazing_m, y = richness_group)) +  # nr of species for each group
  geom_point(aes(color = sp_group)) +                         # for N vs. S
  stat_smooth(method = "lm", aes(color = sp_group, fill = sp_group), se = F) +   
  facet_wrap(~aspect)

## Coverage
ggplot(coverage_site, aes(x = grazing_m, y = coverage_perc)) +  # coverage of groups with grazing 
  geom_point(aes(color = sp_group)) +
  stat_smooth(method = "lm", se = T, aes(color = sp_group, fill = sp_group), alpha = 0.3)

# use this one for final
ggplot(coverage_site, aes(x = grazing_m, y = coverage_perc)) +  # differences for N vs. S
  geom_point(aes(color = aspect)) +
  stat_smooth(method = "lm", aes(color = aspect, fill = aspect)) +   
  facet_wrap(~sp_group, scales = "free_y")

ggplot(coverage_site, aes(x = sp_group, y = coverage_perc)) +  # difference in coverage per group
  geom_boxplot()

# use this one for final
ggplot(coverage_site, aes(x = aspect, y = coverage_perc)) +   # coverage differences N vs. S
  geom_boxplot(aes(fill = sp_group))

## Veg height
ggplot(richness_site, aes(x = grazing_m, y = height_site)) +  # height with grazing 
  geom_point() +
  stat_smooth(method = "lm")

# use this one for final
ggplot(richness_site, aes(x = grazing_m, y = height_site)) +   # height of N vs. S plots 
  geom_point(aes(color = aspect)) +
  stat_smooth(method = "lm", aes(color = aspect, fill = aspect))

ggplot(richness_site, aes(x = aspect, y = height_site)) +
  geom_boxplot()

### GIS variable visualizations ----
## Aspect ---- 
ggplot(richness_site, aes(y = grazing_m, x = aspect)) +   # grazing by aspect 
  geom_boxplot()

## NDVI ----
ggplot(richness_site, aes(y = ndvi, x = aspect)) +  # ndvi by aspect 
  geom_boxplot()

ggplot(richness_site, aes(y = ndvi, x = wetness)) +  # ndvi with wetness 
  geom_point()

ggplot(richness_site, aes(x = ndvi, y = richness_siteT)) +  # richness 
  geom_point() +
  stat_smooth(method = "lm")

ggplot(richness_site, aes(x = ndvi, y = richness_siteT)) +  # richness + aspect
  geom_point(aes(color = aspect)) +
  stat_smooth(method = "lm", aes(color = aspect))

ggplot(richness_site, aes(x = ndvi, y = rich_propT)) +   # rich prop + sp group
  geom_point(aes(color = sp_group)) +
  stat_smooth(method = "lm", aes(color = sp_group))

ggplot(richness_site, aes(x = ndvi, y = rich_propT)) +   # rich prop + sp group + aspect 
  geom_point(aes(color = sp_group)) +
  stat_smooth(method = "lm", aes(color = sp_group)) +   
  facet_wrap(~aspect)

ggplot(richness_site, aes(x = ndvi, y = height_site)) +  # height 
  geom_point() +
  stat_smooth(method = "lm")

ggplot(richness_site, aes(x = ndvi, y = height_site)) +  # height + aspect
  geom_point(aes(color = aspect)) +
  stat_smooth(method = "lm", aes(color = aspect))

ggplot(coverage_site, aes(x = ndvi, y = coverage_perc)) +  # coverage 
  geom_point() +
  stat_smooth(method = "lm")

ggplot(coverage_site, aes(x = ndvi, y = coverage_perc)) +  # coverage + aspect
  geom_point(aes(color = aspect)) +
  stat_smooth(method = "lm", aes(color = aspect))

ggplot(coverage_site, aes(x = ndvi, y = coverage_perc)) +  # coverage + sp group 
  geom_point(aes(color = sp_group)) +
  stat_smooth(method = "lm", se = T, aes(color = sp_group, fill = sp_group))

ggplot(coverage_site, aes(x = ndvi, y = coverage_perc)) +  # coverage + sp group + aspect
  geom_point(aes(color = sp_group)) +
  stat_smooth(method = "lm", se = T, aes(color = sp_group, fill = sp_group)) +
  facet_wrap(~aspect)

## NDVI + Grazing ----
ggplot(richness, aes(x = grazing_m, y = ndvi)) +  # NDVI ~ grazing 
  geom_point() +
  stat_smooth(method = "lm") 

ggplot(richness, aes(x = grazing_m, y = ndvi)) +  # NDVI ~ grazing + aspect 
  geom_point(aes(color = aspect)) +
  stat_smooth(method = "lm", aes(color = aspect)) +
  facet_wrap(~aspect)


ggplot(richness_site, aes(x = grazing_m, y = richness_siteT)) +   # richness + ndvi + aspect
  geom_point(aes(color = ndvi)) +
  facet_wrap(~aspect)

ggplot(richness_site, aes(x = ndvi, y = rich_propT)) +   # rich prop + ndvi + aspect + sp group
  geom_point(aes(color = aspect)) +
  stat_smooth(method = "lm", aes(color = aspect)) +
  facet_wrap(~sp_group)

ggplot(richness_site, aes(x = grazing_m, y = height_site)) +   # height + ndvi + aspect
  geom_point(aes(color = ndvi)) +
  facet_wrap(~aspect)

ggplot(coverage_site, aes(x = grazing_m, y = coverage_perc)) +   # coverage + ndvi + aspect
  geom_point(aes(color = ndvi)) +
  facet_wrap(~aspect)

ggplot(coverage_site, aes(x = grazing_m, y = coverage_perc)) +   # coverage + ndvi + aspect + sp group
  geom_point(aes(color = ndvi)) +
  facet_wrap(~aspect + sp_group)

## Slope ----
ggplot(richness_site, aes(y = slope_deg, x = aspect)) +  # slope by aspect 
  geom_boxplot()

ggplot(richness_site, aes(x = slope_deg, y = richness_siteT)) +  # richness + aspect
  geom_point(aes(color = aspect)) +
  stat_smooth(method = "lm", aes(color = aspect))

ggplot(richness_site, aes(x = slope_deg, y = rich_propT)) +   # rich prop + sp group
  geom_point(aes(color = sp_group)) +
  stat_smooth(method = "lm", aes(color = sp_group))

ggplot(richness_site, aes(x = slope_deg, y = rich_propT)) +   # rich prop + sp group + aspect 
  geom_point(aes(color = sp_group)) +
  stat_smooth(method = "lm", aes(color = sp_group)) +   
  facet_wrap(~aspect)

ggplot(richness_site, aes(x = slope_deg, y = height_site)) +  # height + aspect
  geom_point(aes(color = aspect)) +
  stat_smooth(method = "lm", aes(color = aspect))

ggplot(coverage_site, aes(x = slope_deg, y = coverage_perc)) +  # coverage + aspect
  geom_point(aes(color = aspect)) +
  stat_smooth(method = "lm", aes(color = aspect))

ggplot(coverage_site, aes(x = slope_deg, y = coverage_perc)) +  # coverage + sp group 
  geom_point(aes(color = sp_group)) +
  stat_smooth(method = "lm", se = T, aes(color = sp_group, fill = sp_group))

ggplot(coverage_site, aes(x = slope_deg, y = coverage_perc)) +  # coverage + sp group + aspect
  geom_point(aes(color = sp_group)) +
  stat_smooth(method = "lm", se = T, aes(color = sp_group, fill = sp_group)) +
  facet_wrap(~aspect, scales = "free_x")

## Slope + Grazing ----
ggplot(richness, aes(x = grazing_m, y = slope_deg)) +  # slope ~ grazing 
  geom_point() +
  stat_smooth(method = "lm") 

ggplot(richness, aes(x = grazing_m, y = slope_deg)) +  # slope ~ grazing + aspect 
  geom_point(aes(color = aspect)) +
  stat_smooth(method = "lm", aes(color = aspect)) +
  facet_wrap(~aspect)


ggplot(richness_site, aes(x = grazing_m, y = richness_siteT)) +   # richness + slope + aspect
  geom_point(aes(color = slope_deg)) +
  facet_wrap(~aspect)

ggplot(richness_site, aes(x = grazing_m, y = rich_propT)) +   # rich prop + slope + aspect + sp group
  geom_point(aes(color = slope_deg)) +
  facet_wrap(~aspect + sp_group)

ggplot(richness_site, aes(x = grazing_m, y = height_site)) +   # height + slope + aspect
  geom_point(aes(color = slope_deg)) +
  facet_wrap(~aspect)

ggplot(coverage_site, aes(x = grazing_m, y = coverage_perc)) +   # coverage + slope + aspect
  geom_point(aes(color = slope_deg)) +
  facet_wrap(~aspect)

ggplot(coverage_site, aes(x = grazing_m, y = coverage_perc)) +   # coverage + slope + aspect + sp group
  geom_point(aes(color = slope_deg)) +
  facet_wrap(~aspect + sp_group)

## Soil Depth ----
ggplot(richness_site, aes(y = soil_depth, x = aspect)) +  # slope by aspect 
  geom_boxplot()

ggplot(richness_site, aes(x = soil_depth)) +
  geom_histogram()

## Soil Depth + Grazing ----
ggplot(richness_site, aes(x = grazing_m, y = richness_siteT)) +   # richness + depth + aspect
  geom_point(aes(color = soil_depth)) +
  facet_wrap(~aspect)

ggplot(richness_site, aes(x = grazing_m, y = rich_propT)) +   # rich prop + depth + aspect + sp group
  geom_point(aes(color = soil_depth)) +
  facet_wrap(~aspect + sp_group)

ggplot(richness_site, aes(x = grazing_m, y = height_site)) +   # height + depth + aspect
  geom_point(aes(color = soil_depth)) +
  facet_wrap(~aspect)

ggplot(coverage_site, aes(x = grazing_m, y = coverage_perc)) +   # coverage + depth + aspect
  geom_point(aes(color = soil_depth)) +
  facet_wrap(~aspect)

ggplot(coverage_site, aes(x = grazing_m, y = coverage_perc)) +   # coverage + depth + aspect + sp group
  geom_point(aes(color = soil_depth)) +
  facet_wrap(~aspect + sp_group)

ggplot(richness_site, aes(x = grazing_m, y = soil_depth)) +   # grazing vs. depth  
  geom_point(aes(color = aspect)) +
  stat_smooth(method = "lm", aes(color = aspect))

## Wetness ----
ggplot(richness_site, aes(y = wetness, x = aspect)) +  # wetness by aspect 
  geom_boxplot()

ggplot(richness_site, aes(x = wetness)) +  # wetness by aspect 
  geom_histogram()

ggplot(richness_site, aes(x = wetness, y = richness_siteT)) +  # richness + aspect
  geom_point(aes(color = aspect)) +
  stat_smooth(method = "lm", aes(color = aspect))

ggplot(richness_site, aes(x = wetness, y = rich_propT)) +   # rich prop + sp group
  geom_point(aes(color = sp_group)) +
  stat_smooth(method = "lm", aes(color = sp_group))

ggplot(richness_site, aes(x = wetness, y = rich_propT)) +   # rich prop + sp group + aspect 
  geom_point(aes(color = sp_group)) +
  stat_smooth(method = "lm", aes(color = sp_group)) +   
  facet_wrap(~aspect, scales = "free_x")

ggplot(richness_site, aes(x = wetness, y = height_site)) +  # height 
  geom_point() +
  stat_smooth(method = "lm")

ggplot(richness_site, aes(x = wetness, y = height_site)) +  # height + aspect
  geom_point(aes(color = aspect)) +
  stat_smooth(method = "lm", aes(color = aspect))

ggplot(coverage_site, aes(x = wetness, y = coverage_perc)) +  # coverage + aspect
  geom_point(aes(color = aspect)) +
  stat_smooth(method = "lm", aes(color = aspect))

ggplot(coverage_site, aes(x = wetness, y = coverage_perc)) +  # coverage + sp group 
  geom_point(aes(color = sp_group)) +
  stat_smooth(method = "lm", se = T, aes(color = sp_group, fill = sp_group))

ggplot(coverage_site, aes(x = wetness, y = coverage_perc)) +  # coverage + sp group + aspect
  geom_point(aes(color = sp_group)) +
  stat_smooth(method = "lm", se = T, aes(color = sp_group, fill = sp_group)) +
  facet_wrap(~aspect, scales = "free_x")

## Wetness + Grazing ----
ggplot(richness, aes(x = grazing_m, y = wetness)) +  # wetness ~ grazing 
  geom_point(aes(color = richness_siteT)) +
  stat_smooth(method = "loess") 

ggplot(richness, aes(x = wetness, y = grazing_m)) +  # wetness ~ grazing + aspect 
  geom_point(aes(color = aspect)) +
  stat_smooth(method = "lm", aes(color = aspect)) +
  facet_wrap(~aspect)


ggplot(richness_site, aes(x = grazing_m, y = richness_siteT)) +   # richness + wetness + aspect
  geom_point(aes(color = wetness)) +
  facet_wrap(~aspect)

ggplot(richness_site, aes(x = grazing_m, y = rich_propT)) +   # rich prop + wetness + aspect + sp group
  geom_point(aes(color = wetness)) +
  facet_wrap(~aspect + sp_group)

ggplot(richness_site, aes(x = grazing_m, y = height_site)) +   # height + wetness + aspect
  geom_point(aes(color = wetness)) +
  facet_wrap(~aspect)

ggplot(coverage_site, aes(x = grazing_m, y = coverage_perc)) +   # coverage + wetness + aspect
  geom_point(aes(color = wetness)) +
  facet_wrap(~aspect)

ggplot(coverage_site, aes(x = grazing_m, y = coverage_perc)) +   # coverage + wetness + aspect + sp group
  geom_point(aes(color = wetness)) +
  facet_wrap(~aspect + sp_group)

# . ----
#### Vegetation Models ----
ggcorr(richness_site, label = T)  
  # slope  and soil depth = quite well negatively correlated (-0.6)
    # soil depth is a very low resolution, so maybe exclude this from models
  # slope and wetness = correlate -0.5, but is less than the threshold for correlation (usually 0.7)

### Richness ~ Grazing (Site) ----
# only have 34 datapoints due to averaging across plots per site = max 3-4 parameters per model

# checking assumptions
hist(richness_site2$richness_siteT)  # maybe bimodal  

rg1 <- lm(richness_siteT ~ grazing_m, data = richness_site2)
plot(rg1)
shapiro.test(resid(rg1))   # p < 0.05 = normal
bptest(rg1)  # p > 0.05 = and no heteroskedasticity 

rga1 <- lm(richness_siteT ~ grazing_m + aspect, data = richness_site2)
plot(rga1)
shapiro.test(resid(rga1))   # p < 0.05 = normal
bptest(rga1)  # p > 0.05 = no heteroskedasticity

AIC(rg1, rga1)  # best is 'rga1' (with aspect)


### Models with GIS variables
## NDVI
rgan <- lm(richness_siteT ~ grazing_m_s + aspect + ndvi_s, data = richness_site2)
rgan2 <- lm(richness_siteT ~ grazing_m_s * aspect + ndvi_s, data = richness_site2)
rgan3 <- lm(richness_siteT ~ grazing_m_s + aspect * ndvi_s, data = richness_site2)
rgan4 <- lm(richness_siteT ~ grazing_m_s * ndvi_s + aspect, data = richness_site2)
rgan5 <- lm(richness_siteT ~ grazing_m_s * aspect * ndvi_s, data = richness_site2)
rgan6 <- lm(richness_siteT ~ grazing_m_s*aspect + ndvi_s*aspect, data = richness_site2)
rgan7 <- lm(richness_siteT ~ aspect + ndvi_s, data = richness_site2)
rgan8 <- lm(richness_siteT ~ aspect*ndvi_s, data = richness_site2)

AIC(rga1, rgan, rgan2, rgan3, rgan4, rgan5, rgan6, rgan7, rgan8)  
  # 'rgan3' is a bit better but not by 2 (second best = 'rga1')

## Slope
rgas <- lm(richness_siteT ~ grazing_m_s + aspect + slope_deg_s, data = richness_site2)
rgas2 <- lm(richness_siteT ~ grazing_m_s * aspect + slope_deg_s, data = richness_site2)
rgas3 <- lm(richness_siteT ~ grazing_m_s + aspect * slope_deg_s, data = richness_site2)
rgas4 <- lm(richness_siteT ~ grazing_m_s * slope_deg_s + aspect, data = richness_site2)
rgas5 <- lm(richness_siteT ~ grazing_m_s * aspect * slope_deg_s, data = richness_site2)
rgas6 <- lm(richness_siteT ~ grazing_m_s*aspect + slope_deg_s*aspect, data = richness_site2)

AIC(rga1, rgan3, rgas, rgas2, rgas3, rgas4, rgas5, rgas6)  # 'rgas' is best and simplest 

## Wetness
rgaw <- lm(richness_siteT ~ grazing_m_s + aspect + wetness_s, data = richness_site2)
rgaw2 <- lm(richness_siteT ~ grazing_m_s * aspect + wetness_s, data = richness_site2)
rgaw3 <- lm(richness_siteT ~ grazing_m_s + aspect * wetness_s, data = richness_site2)
rgaw4 <- lm(richness_siteT ~ grazing_m_s * wetness_s + aspect, data = richness_site2)
rgaw5 <- lm(richness_siteT ~ grazing_m_s * aspect * wetness_s, data = richness_site2)
rgaw6 <- lm(richness_siteT ~ grazing_m_s*aspect + wetness_s*aspect, data = richness_site2)

AIC(rga1, rgas, rgaw, rgaw2, rgaw3, rgaw4, rgaw5, rgaw6)  
  # 'rgaw3' is best but not by 2 (second best = 'rgaw')


## Adding 1 other variable, no interaction
rgawn <- lm(richness_siteT ~ grazing_m_s + aspect + wetness_s + ndvi_s, data = richness_site2)
rgaws <- lm(richness_siteT ~ grazing_m_s + aspect + wetness_s + slope_deg_s, data = richness_site2)

AIC(rgaw3, rgawn, rgaws)  # 'rgawn' is better (same DF, but lower AIC)


## Choosing the best model 
r_null <- lm(richness_siteT ~ 1, data = richness_site2)
AIC(r_null, rga1, rgawn)  # 'rgawn' is definitely best  

hist(resid(rgawn))


## Richness Model Results ## ----
# lm(richness_siteT ~ grazing_m_s + aspect + wetness_s + ndvi_s)
summary(rgawn)     ## USE THIS MODEL'S RESULTS ##
  # aspect NORTH (intercept): 17.5252 ± 0.4909
  # aspect SOUTH: -3.2477 ± 0.7079, p = 7.96e-05 (SIGNIFICANT)
  # grazing_s: 0.1901 ± 0.3655, p = 0.6069 (not significant)
  # wetness_s: 2.0208 ± 0.3815, p = 1.11e-05 (SIGNIFICANT)
  # ndvi_s: 0.7114 ± 0.3849, p = 0.0748 (not significant)
# adjusted R2 = 0.5573

# checking residuals 
plot(residuals(rgawn) ~ predict(rgawn, type = "response"))  # looks random = good

## Interpreting my model
# grazing
  0.1901/sd(richness_site2$grazing_m)   # = 0.02256703
# wetness
  2.0208/sd(richness_site2$wetness)   # = 0.1139502
# ndvi
  0.7114/sd(richness_site2$ndvi)   # = 13.11307

# comparing to OG model (not scaled)
rgawn_og <- lm(richness_siteT ~ grazing_m + aspect + wetness + ndvi, richness_site)
summary(rgawn_og)
  # grazing = 0.02247
  # wetness = 0.10946
  # NDVI = 12.13148
# similar 


### Richness ~ Grazing (Sp Group Proportions) ----
hist(richness_site$rich_propT)   # a bit skewed 

### Lichens ----
lichen <- richness_site %>% filter(sp_group == "lichen")
hist(lichen$rich_propT)   # a bit skewed 

lg1 <- lm(rich_propT ~ grazing_m, data = lichen)
plot(lg1)
shapiro.test(resid(lg1))   # p < 0.05 = not normal
bptest(lg1)  # p > 0.05 = no heteroskedasticity though 

lga <- lm(rich_propT ~ grazing_m + aspect, data = lichen)
shapiro.test(resid(lga))   # p < 0.05 = not normal
bptest(lga)  # p > 0.05 = no heteroskedasticity though 

lga2 <- lm(rich_propT ~ grazing_m * aspect, data = lichen)
shapiro.test(resid(lga2))   # p < 0.05 = not normal
bptest(lga2)  # p > 0.05 = no heteroskedasticity though 

## Transformations 
# log
lg_log <- lm(log(rich_propT) ~ grazing_m, data = lichen)
shapiro.test(resid(lg_log))   # p > 0.05 = normal
bptest(lg_log)  # p > 0.05 = and no heteroskedasticity

lga_log <- lm(log(rich_propT) ~ grazing_m + aspect, data = lichen)
shapiro.test(resid(lga_log))   # p > 0.05 = normal
bptest(lga_log)  # p > 0.05 = and no heteroskedasticity

lga_log2 <- lm(log(rich_propT) ~ grazing_m * aspect, data = lichen)
shapiro.test(resid(lga_log2))   # p > 0.05 = normal
bptest(lga_log2)  # p > 0.05 = and no heteroskedasticity

AIC(lg_log, lga_log, lga_log2)  # 'lg_log' (only grazing) is best so far 

## Models with GIS variables 
# NDVI
lgn_log <- lm(log(rich_propT) ~ grazing_m_s + ndvi_s, data = lichen)
lgan_log <- lm(log(rich_propT) ~ grazing_m_s + aspect + ndvi_s, data = lichen)
lgan_log2 <- lm(log(rich_propT) ~ grazing_m_s + aspect*ndvi_s, data = lichen)
lgan_log3 <- lm(log(rich_propT) ~ grazing_m_s*aspect + ndvi_s, data = lichen)
lgan_log4 <- lm(log(rich_propT) ~ grazing_m_s*aspect + ndvi_s*aspect, data = lichen)

AIC(lg_log, lga_log, lgn_log, lgan_log, lgan_log2, lgan_log3, lgan_log4)
  # 'lgan_log3' = lowest, but not quite by 2 units (next best is 'lgn_log')

# Slope
lgs_log <- lm(log(rich_propT) ~ grazing_m_s + slope_deg_s, data = lichen)
lgas_log <- lm(log(rich_propT) ~ grazing_m_s + aspect + slope_deg_s, data = lichen)
lgas_log2 <- lm(log(rich_propT) ~ grazing_m_s + aspect*slope_deg_s, data = lichen)
lgas_log3 <- lm(log(rich_propT) ~ grazing_m_s*aspect + slope_deg_s, data = lichen)
lgas_log4 <- lm(log(rich_propT) ~ grazing_m_s*aspect + slope_deg_s*aspect, data = lichen)

AIC(lgan_log3, lgn_log, lg_log, lga_log, lgs_log, lgas_log, lgas_log2, lgas_log3, lgas_log4)
  # 'lgan_log3' = still best 

# Wetness
lgw_log <- lm(log(rich_propT) ~ grazing_m_s + wetness_s, data = lichen)
lgaw_log <- lm(log(rich_propT) ~ grazing_m_s + aspect + wetness_s, data = lichen)
lgaw_log2 <- lm(log(rich_propT) ~ grazing_m_s + aspect*wetness_s, data = lichen)
lgaw_log3 <- lm(log(rich_propT) ~ grazing_m_s*aspect + wetness_s, data = lichen)
lgaw_log4 <- lm(log(rich_propT) ~ grazing_m_s*aspect + wetness_s*aspect, data = lichen)

AIC(lgan_log3, lgn_log, lg_log, lga_log, lgw_log, lgaw_log, lgaw_log2, lgaw_log3, lgaw_log4)
  # 'lgan_log3' = still best

# Multiple variables 
lgns_log <- lm(log(rich_propT) ~ grazing_m_s + ndvi_s + slope_deg_s, data = lichen)  # standardized
lgans_log <- lm(log(rich_propT) ~ grazing_m_s + aspect + ndvi_s + slope_deg_s, data = lichen)  

lgnw_log <- lm(log(rich_propT) ~ grazing_m_s + ndvi_s + wetness_s, data = lichen)  # standardized
lgnw_log2 <- lm(log(rich_propT) ~ grazing_m_s + ndvi_s*wetness_s, data = lichen)
lganw_log <- lm(log(rich_propT) ~ grazing_m_s + aspect + ndvi_s + wetness_s, data = lichen)  

min(AIC(lgan_log3, lgn_log, lgns_log, lgans_log, lgnw_log, lgnw_log2, lganw_log)[,2])    
  # lowest == -8.187243

AIC(lgan_log3, lgn_log, lgns_log, lgans_log, lgnw_log, lgnw_log2, lganw_log)
  # 'lgan_log3' = still lowest, but not by 2 


## Choosing the best model 
l_null <- lm(log(rich_propT) ~ 1, data = lichen)
AIC(l_null, lgn_log, lgan_log3)   # 'lgan_log3' is definitely lower than the null 

shapiro.test(resid(lgan_log3))  # normal
bptest(lgan_log3)  # no heteroskedasticity 


### Mosses ----
moss <- richness_site %>% filter(sp_group == "moss")
hist(moss$rich_propT)   # looks good 

mg1 <- lm(rich_propT ~ grazing_m, data = moss)
plot(mg1)
shapiro.test(resid(mg1))   # p > 0.05 = normal
bptest(mg1)  # p > 0.05 = no heteroskedasticity either  

mga <- lm(rich_propT ~ grazing_m + aspect, data = moss)
shapiro.test(resid(mga))   # p > 0.05 = normal normal
bptest(mga)  # p > 0.05 = no heteroskedasticity  

mga2 <- lm(rich_propT ~ grazing_m * aspect, data = moss)
shapiro.test(resid(mga2))   # p > 0.05 = normal
bptest(mga2)  # p > 0.05 = no heteroskedasticity  

AIC(mg1, mga, mga2)  # so far, 'mga' is best but not by 2 

## Models with GIS variables 
# NDVI
mgn <- lm(rich_propT ~ grazing_m_s + ndvi_s, data = moss)
mgan <- lm(rich_propT ~ grazing_m_s + aspect + ndvi_s, data = moss)
mgan2 <- lm(rich_propT ~ grazing_m_s + aspect*ndvi_s, data = moss)
mgan3 <- lm(rich_propT ~ grazing_m_s*aspect + ndvi_s, data = moss)
mgan4 <- lm(rich_propT ~ grazing_m_s*aspect + ndvi_s*aspect, data = moss)

AIC(mg1, mga, mgn, mgan, mgan2, mgan3, mgan4)
  # 'mgan' = lowest, but not by 2 units (next best is 'mga' or 'mg1)

# Slope
mgs <- lm(rich_propT ~ grazing_m_s + slope_deg_s, data = moss)
mgas <- lm(rich_propT ~ grazing_m_s + aspect + slope_deg_s, data = moss)
mgas2 <- lm(rich_propT ~ grazing_m_s + aspect*slope_deg_s, data = moss)
mgas3 <- lm(rich_propT ~ grazing_m_s*aspect + slope_deg_s, data = moss)
mgas4 <- lm(rich_propT ~ grazing_m_s*aspect + slope_deg_s*aspect, data = moss)

AIC(mgan, mg1, mga, mgs, mgas, mgas2, mgas3, mgas4)
  # 'mgan' = still best 

# Wetness
mgw <- lm(rich_propT ~ grazing_m_s + wetness_s, data = moss)
mgaw <- lm(rich_propT ~ grazing_m_s + aspect + wetness_s, data = moss)
mgaw2 <- lm(rich_propT ~ grazing_m_s + aspect*wetness_s, data = moss)
mgaw3 <- lm(rich_propT ~ grazing_m_s*aspect + wetness_s, data = moss)
mgaw4 <- lm(rich_propT ~ grazing_m_s*aspect + wetness_s*aspect, data = moss)

AIC(mgan, mg1, mga, mgw, mgaw, mgaw2, mgaw3, mgaw4)
  # 'mgaw2' = lowest, but close to 'mgw' and 'mgan'

# Multiple GIS variables 
mgnw <- lm(rich_propT ~ grazing_m_s + ndvi_s + wetness_s, data = moss)  # standardized
mgnw2 <- lm(rich_propT ~ grazing_m_s + ndvi_s*wetness_s, data = moss)  # standardized
mganw <- lm(rich_propT ~ grazing_m_s + aspect + ndvi_s + wetness_s, data = moss)  

mgns <- lm(rich_propT ~ grazing_m_s + ndvi_s + slope_deg_s, data = moss)  # standardized
mgans <- lm(rich_propT ~ grazing_m_s + aspect + ndvi_s + slope_deg_s, data = moss)  

mgws <- lm(rich_propT ~ grazing_m_s + wetness_s + slope_deg_s, data = moss)  # standardized
mgws2 <- lm(rich_propT ~ grazing_m_s + wetness_s*slope_deg_s, data = moss)  # standardized
mgaws <- lm(rich_propT ~ grazing_m_s + aspect + wetness_s + slope_deg_s, data = moss)  

min(AIC(mgan, mgaw2, mgw, mgnw, mgnw2, mganw, mgns, mgans, mgws, mgws2, mgaws)[,2])    
  # lowest == -108.0255

AIC(mgan, mgaw2, mgw, mgnw, mgnw2, mganw, mgns, mgans, mgws, mgws2, mgaws)
  # 'mganw' = lowest with 6 DF, but not by 2 
  # lowest and simplest = 'mgw' 


## Choosing the best model 
m_null <- lm(rich_propT ~ 1, data = moss)
AIC(m_null, mganw, mgw)   # 'mgw' is not better than the null, so going with 'mganw'


### Shrubs ----
shrub <- richness_site %>% filter(sp_group == "shrub")
hist(shrub$rich_propT)   # looks good  

sg1 <- lm(rich_propT ~ grazing_m, data = shrub)
plot(sg1)
shapiro.test(resid(sg1))   # p > 0.05 = normal
bptest(sg1)  # p > 0.05 = no heteroskedasticity either  

sga <- lm(rich_propT ~ grazing_m + aspect, data = shrub)
shapiro.test(resid(sga))   # p > 0.05 = normal 
bptest(sga)  # p > 0.05 = no heteroskedasticity  

sga2 <- lm(rich_propT ~ grazing_m * aspect, data = shrub)
shapiro.test(resid(sga2))   # p > 0.05 = normal
bptest(sga2)  # p > 0.05 = no heteroskedasticity  

AIC(sg1, sga, sga2)  # so far, all are equal (sg1 = simplest)

## Models with GIS variables 
# NDVI
sgn <- lm(rich_propT ~ grazing_m_s + ndvi_s, data = shrub)
sgan <- lm(rich_propT ~ grazing_m_s + aspect + ndvi_s, data = shrub)
sgan2 <- lm(rich_propT ~ grazing_m_s + aspect*ndvi_s, data = shrub)
sgan3 <- lm(rich_propT ~ grazing_m_s*aspect + ndvi_s, data = shrub)
sgan4 <- lm(rich_propT ~ grazing_m_s*aspect + ndvi_s*aspect, data = shrub)

AIC(sg1, sga, sgn, sgan, sgan2, sgan3, sgan4)
  # 'sg1' = best because it's the simplest 

# Slope
sgs <- lm(rich_propT ~ grazing_m_s + slope_deg_s, data = shrub)
sgas <- lm(rich_propT ~ grazing_m_s + aspect + slope_deg_s, data = shrub)
sgas2 <- lm(rich_propT ~ grazing_m_s + aspect*slope_deg_s, data = shrub)
sgas3 <- lm(rich_propT ~ grazing_m_s*aspect + slope_deg_s, data = shrub)
sgas4 <- lm(rich_propT ~ grazing_m_s*aspect + slope_deg_s*aspect, data = shrub)

AIC(sg1, sga, sgs, sgas, sgas2, sgas3, sgas4)
# 'sgas' = simpler than others that low, but not lowest by 2 (second best = 'sgs')

# Wetness
sgw <- lm(rich_propT ~ grazing_m_s + wetness_s, data = shrub)
sgaw <- lm(rich_propT ~ grazing_m_s + aspect + wetness_s, data = shrub)
sgaw2 <- lm(rich_propT ~ grazing_m_s + aspect*wetness_s, data = shrub)
sgaw3 <- lm(rich_propT ~ grazing_m_s*aspect + wetness_s, data = shrub)
sgaw4 <- lm(rich_propT ~ grazing_m_s*aspect + wetness_s*aspect, data = shrub)

AIC(sgas, sgs, sg1, sga, sgw, sgaw, sgaw2, sgaw3, sgaw4)
# 'sgaw3' = lowest, but not quite by 2 (second lowest/simplest = 'sgaw')

# Multiple GIS var
sgnw <- lm(rich_propT ~ grazing_m_s + ndvi_s + wetness_s, data = shrub)  # standardized
sgnw2 <- lm(rich_propT ~ grazing_m_s + ndvi_s*wetness_s, data = shrub)  # standardized
sganw <- lm(rich_propT ~ grazing_m_s + aspect + ndvi_s + wetness_s, data = shrub)  

sgns <- lm(rich_propT ~ grazing_m_s + ndvi_s + slope_deg_s, data = shrub)  # standardized
sgans <- lm(rich_propT ~ grazing_m_s + aspect + ndvi_s + slope_deg_s, data = shrub)  

sgws <- lm(rich_propT ~ grazing_m_s + wetness_s + slope_deg_s, data = shrub)  # standardized
sgws2 <- lm(rich_propT ~ grazing_m_s + wetness_s*slope_deg_s, data = shrub)  # standardized
sgaws <- lm(rich_propT ~ grazing_m_s + aspect + wetness_s + slope_deg_s, data = shrub)  

min(AIC(sgaw3, sgaw, sgas, sgnw, sgnw2, sganw, sgns, sgans, sgws, sgws2, sgaws)[,2])    
# lowest == -95.05383

AIC(sgaw3, sgaw, sgas, sgnw, sgnw2, sganw, sgns, sgans, sgws, sgws2, sgaws)
# 'sgaw3' = lowest with 6 DF, but not by 2 (second = 'sgaw')


## Choosing the best model 
s_null <- lm(rich_propT ~ 1, data = shrub)
AIC(s_null, sgaw3, sgaw)   # 'sgaw3' is better than the null

# checking if interactions add anything
summary(sgaw)  # adjusted R2 = 0.196
summary(sgaw3)  # adjusted R2 = 0.2562 (quite a lot better), going with this one


### Grasses ----
grass <- richness_site %>% filter(sp_group == "grass")
hist(grass$rich_propT)   # looks pretty ok  

gg1 <- lm(rich_propT ~ grazing_m, data = grass)
plot(gg1)
shapiro.test(resid(gg1))   # p > 0.05 = normal
bptest(gg1)  # p > 0.05 = no heteroskedasticity either  

gga <- lm(rich_propT ~ grazing_m + aspect, data = grass)
shapiro.test(resid(gga))   # p > 0.05 = normal 
bptest(gga)  # p > 0.05 = no heteroskedasticity  

gga2 <- lm(rich_propT ~ grazing_m * aspect, data = grass)
shapiro.test(resid(gga2))   # p > 0.05 = normal
bptest(gga2)  # p > 0.05 = no heteroskedasticity  

AIC(gg1, gga, gga2)  # 'gg1' = the best 

## Models with GIS variables 
# NDVI
ggn <- lm(rich_propT ~ grazing_m_s + ndvi_s, data = grass)
ggan <- lm(rich_propT ~ grazing_m_s + aspect + ndvi_s, data = grass)
ggan2 <- lm(rich_propT ~ grazing_m_s + aspect*ndvi_s, data = grass)
ggan3 <- lm(rich_propT ~ grazing_m_s*aspect + ndvi_s, data = grass)
ggan4 <- lm(rich_propT ~ grazing_m_s*aspect + ndvi_s*aspect, data = grass)

AIC(gg1, ggn, ggan, ggan2, ggan3, ggan4)
# 'gg1' = best because it's the simplest 

# Slope
ggs <- lm(rich_propT ~ grazing_m_s + slope_deg_s, data = grass)
ggas <- lm(rich_propT ~ grazing_m_s + aspect + slope_deg_s, data = grass)
ggas2 <- lm(rich_propT ~ grazing_m_s + aspect*slope_deg_s, data = grass)
ggas3 <- lm(rich_propT ~ grazing_m_s*aspect + slope_deg_s, data = grass)
ggas4 <- lm(rich_propT ~ grazing_m_s*aspect + slope_deg_s*aspect, data = grass)

AIC(gg1, ggs, ggas, ggas2, ggas3, ggas4)
# 'gg1' = still best

# Wetness
ggw <- lm(rich_propT ~ grazing_m_s + wetness_s, data = grass)
ggaw <- lm(rich_propT ~ grazing_m_s + aspect + wetness_s, data = grass)
ggaw2 <- lm(rich_propT ~ grazing_m_s + aspect*wetness_s, data = grass)
ggaw3 <- lm(rich_propT ~ grazing_m_s*aspect + wetness_s, data = grass)
ggaw4 <- lm(rich_propT ~ grazing_m_s*aspect + wetness_s*aspect, data = grass)

AIC(gg1, gga, ggw, ggaw, ggaw2, ggaw3, ggaw4)
# 'ggaw4' = the best by far 

# Multiple GIS var 
ggnw <- lm(rich_propT ~ grazing_m_s + ndvi_s + wetness_s, data = grass)  # standardized
ggnw2 <- lm(rich_propT ~ grazing_m_s + ndvi_s*wetness_s, data = grass)  
gganw <- lm(rich_propT ~ grazing_m_s + aspect + ndvi_s + wetness_s, data = grass) 

ggns <- lm(rich_propT ~ grazing_m_s + ndvi_s + slope_deg_s, data = grass)  # standardized
ggans <- lm(rich_propT ~ grazing_m_s + aspect + ndvi_s + slope_deg_s, data = grass)  

ggws <- lm(rich_propT ~ grazing_m_s + wetness_s + slope_deg_s, data = grass)  # standardized
ggws2 <- lm(rich_propT ~ grazing_m_s + wetness_s*slope_deg_s, data = grass)  # standardized
ggaws <- lm(rich_propT ~ grazing_m_s + aspect + wetness_s + slope_deg_s, data = grass)  

min(AIC(ggaw4, gg1, ggnw, ggnw2, gganw, ggns, ggans, ggws, ggws2, ggaws)[,2])    
  # lowest == -121.2405

AIC(ggaw4, gg1, ggnw, ggnw2, gganw, ggns, ggans, ggws, ggws2, ggaws)
  # 'ggaw4' = best 


## Choosing the best model 
g_null <- lm(rich_propT ~ 1, data = grass)
AIC(g_null, ggaw4)   # 'ggaw4' is better than the null

shapiro.test(resid(ggaw4))  # normal
bptest(ggaw4)  # no heteroskedasticity


### Herbs ----
herb <- richness_site %>% filter(sp_group == "herb")
hist(herb$rich_propT)    

hg1 <- lm(rich_propT ~ grazing_m, data = herb)
plot(hg1)
shapiro.test(resid(hg1))   # p > 0.05 = normal
bptest(hg1)  # p > 0.05 = no heteroskedasticity either  

hga <- lm(rich_propT ~ grazing_m + aspect, data = herb)
shapiro.test(resid(hga))   # p > 0.05 = normal 
bptest(hga)  # p > 0.05 = no heteroskedasticity  

hga2 <- lm(rich_propT ~ grazing_m * aspect, data = herb)
shapiro.test(resid(hga2))   # p > 0.05 = normal
bptest(hga2)  # p > 0.05 = no heteroskedasticity  

AIC(hg1, hga, hga2)  # 'hg1' = the best 

## Models with GIS variables 
# NDVI
hgn <- lm(rich_propT ~ grazing_m_s + ndvi_s, data = herb)
hgan <- lm(rich_propT ~ grazing_m_s + aspect + ndvi_s, data = herb)
hgan2 <- lm(rich_propT ~ grazing_m_s + aspect*ndvi_s, data = herb)
hgan3 <- lm(rich_propT ~ grazing_m_s*aspect + ndvi_s, data = herb)
hgan4 <- lm(rich_propT ~ grazing_m_s*aspect + ndvi_s*aspect, data = herb)

AIC(hg1, hgn, hgan, hgan2, hgan3, hgan4)
  # 'hg1' = still best

# Slope
hgs <- lm(rich_propT ~ grazing_m_s + slope_deg_s, data = herb)
hgas <- lm(rich_propT ~ grazing_m_s + aspect + slope_deg_s, data = herb)
hgas2 <- lm(rich_propT ~ grazing_m_s + aspect*slope_deg_s, data = herb)
hgas3 <- lm(rich_propT ~ grazing_m_s*aspect + slope_deg_s, data = herb)
hgas4 <- lm(rich_propT ~ grazing_m_s*aspect + slope_deg_s*aspect, data = herb)

AIC(hg1, hgs, hgas, hgas2, hgas3, hgas4)
  # 'hgs' = best, but juuust under 2 units (otherwise 'hg1' is best still)

# Wetness
hgw <- lm(rich_propT ~ grazing_m_s + wetness_s, data = herb)
hgaw <- lm(rich_propT ~ grazing_m_s + aspect + wetness_s, data = herb)
hgaw2 <- lm(rich_propT ~ grazing_m_s + aspect*wetness_s, data = herb)
hgaw3 <- lm(rich_propT ~ grazing_m_s*aspect + wetness_s, data = herb)
hgaw4 <- lm(rich_propT ~ grazing_m_s*aspect + wetness_s*aspect, data = herb)

AIC(hg1, hgs, hgw, hgaw, hgaw2, hgaw3, hgaw4)
  # 'hgaw' = the best, but under 2 units (next best = 'hgw')

# Multiple GIS var (no interactions)
hgnw <- lm(rich_propT ~ grazing_m_s + ndvi_s + wetness_s, data = herb)  # standardized
hgnw2 <- lm(rich_propT ~ grazing_m_s + ndvi_s*wetness_s, data = herb)  
hganw <- lm(rich_propT ~ grazing_m_s + aspect + ndvi_s + wetness_s, data = herb)  

hgns <- lm(rich_propT ~ grazing_m_s + ndvi_s + slope_deg_s, data = herb)  # standardized
hgans <- lm(rich_propT ~ grazing_m_s + aspect + ndvi_s + slope_deg_s, data = herb)  

hgws <- lm(rich_propT ~ grazing_m_s + wetness_s + slope_deg_s, data = herb)  # standardized
hgws2 <- lm(rich_propT ~ grazing_m_s + wetness_s*slope_deg_s, data = herb)  
hgaws <- lm(rich_propT ~ grazing_m_s + aspect + wetness_s + slope_deg_s, data = herb)  

min(AIC(hgaw, hgw, hg1, hgnw, hgnw2, hganw, hgns, hgans, hgws, hgws2, hgaws)[,2])    
  # lowest == -97.06862

AIC(hgaw, hgw, hg1, hgnw, hgnw2, hganw, hgns, hgans, hgws, hgws2, hgaws)
  # 'hgaw' = still best and simplest 


## Choosing the best model 
h_null <- lm(rich_propT ~ 1, data = herb)
AIC(h_null, hgaw, hgw)   # 'hgaw' is better than the null

shapiro.test(resid(hgaw))  # normal
bptest(hgaw)  # no heteroskedasticity 


## Richness Proportion Model Results ## ----
# LICHEN = lm(log(rich_propT) ~ grazing_m_s*aspect + ndvi_s
summary(lgan_log3)      ## USE THIS MODEL'S RESULTS ##
  # aspect NORTH (intercept): exp(-1.27895) ± exp(0.04787)
  # aspect SOUTH: exp(0.09448) ± exp(0.06852), p = 0.1785 (not significant)
  # grazing_s NORTH: exp(0.01116) ± exp(0.04812), p = 0.8183 (not significant) 
  # grazing_s*aspect SOUTH: exp(-0.12673) ± exp(0.06815), p = 0.0731 (not significant)
  # ndvi_s: exp(-0.17443) ± exp(0.03781), p = 7.42e-05 (SIGNFICANT)    
# adjusted R2 = 0.3917 

# checking residuals 
plot(residuals(lgan_log3) ~ predict(lgan_log3, type = "response"))  # looks good 

## Interpreting my model
# grazing N
0.01116/sd(lichen$grazing_m)  # = 0.001324819
# grazing S
-0.12673/sd(lichen$grazing_m)  # = -0.01504429
# ndvi
-0.17443/sd(lichen$ndvi)    # = -3.215228

# comparing to OG model (not scaled)
lgan_log3_og <- lm(log(rich_propT) ~ grazing_m*aspect + ndvi, data = lichen)
summary(lgan_log3_og)
  # grazing N = 0.001319
  # grazing S = -0.014982
  # NDVI = -2.974657
# similar 


# MOSS = lm(rich_propT ~ grazing_m_s + aspect + ndvi_s + wetness_s)
summary(mganw)      ## USE THIS MODEL'S RESULTS ##
  # aspect NORTH (intercept): 0.1935644 ± 0.0111056
  # aspect SOUTH: -0.0266947 ± 0.0160144, p = 0.1063 (not significant)
  # grazing_s: -0.0005226 ± 0.0082679, p = 0.9500 (not significant)
  # ndvi_s: 0.0155851 ± 0.0087063, p = 0.0839 (not significant)
  # wetness_s: -0.0176485 ± 0.0086303, p = 0.0500 (not significant)
# adjusted R2 = 0.1584

# checking residuals 
plot(residuals(mganw) ~ predict(mganw, type = "response"))  # not fantastic but ok  

## Interpreting my model
# grazing 
-0.0005226/sd(moss$grazing_m)  # = -6.203857e-05
# ndvi
0.0155851/sd(richness_site2$ndvi)    # = 0.2872766
# wetness
-0.0176485/sd(richness_site2$wetness)    # = -0.0009951751


# comparing to OG model (not scaled)
mganw_og <- lm(rich_propT ~ grazing_m + aspect + ndvi + wetness, data = moss)
summary(mganw_og)
  # grazing = -6.178e-05
  # NDVI = 2.658e-01
  # wetness = -9.560e-04
# very similar 

summary(lm(rich_propT ~ grazing_m_s * aspect + wetness_s * aspect, data = shrub))

# SHRUB = lm(rich_propT ~ grazing_m_s*aspect + wetness_s)
summary(sgaw3)      ## USE THIS MODEL'S RESULTS ##
  # aspect NORTH (intercept): 0.22966 ± 0.01327
  # aspect SOUTH: 0.04475 ± 0.01894, p = 0.02507 (SIGNIFICANT)
  # grazing_s NORTH: 0.02146 ± 0.01343, p = 0.12088 (not significant)
  # grazing_s*aspect SOUTH: -0.03592 ± 0.01936, p = 0.07367 (not significant)
  # wetness_s: -0.03067 ± 0.01065, p = 0.00739 (SIGNIFICANT)
# adjusted R2 = 0.2566

## Interpreting my model
# grazing N
0.02146/sd(shrub$grazing_m)  # = 0.002547546
# grazing S
-0.03592/sd(shrub$grazing_m)  # = -0.004264113
# wetness
-0.03067/sd(shrub$wetness)    # = -0.00172944

# comparing to OG model (not scaled)
sgaw3_og <- lm(rich_propT ~ grazing_m*aspect + wetness, data = shrub)
summary(sgaw3_og)
  # grazing N = 0.0025373
  # grazing S = -0.0042468
  # wetness = -0.0016611
# very similar 


# GRASS = lm(rich_propT ~ grazing_m_s*aspect + wetness_s*aspect)
summary(ggaw4)      ## USE THIS MODEL'S RESULTS ##
  # aspect NORTH (intercept): 0.153703 ± 0.009261
  # aspect SOUTH: -0.003642 ± 0.012884, p = 0.77948 (not significant)
  # grazing NORTH: -0.010821 ± 0.009195, p = 0.24917 (not significant)
  # grazing*aspect SOUTH: 0.031877 ± 0.013407, p = 0.02449 (SIGNIFICANT)
  # wetness NORTH: 0.054734 ± 0.017524, p = 0.00413 (SIGNIFICANT)
  # wetness*aspect SOUTH: -0.039613 ± 0.019200, p = 0.04848 (SIGNIFICANT)
# adjusted R2 = 0.3075

## Interpreting my model
# grazing N
-0.010821/sd(grass$grazing_m)  # = -0.001284576
# grazing S
0.031877/sd(grass$grazing_m)  # = 0.003784163
# wetness N
0.054734/sd(grass$wetness)    # = 0.003086376
# wetness S
-0.039613/sd(grass$wetness)    # = -0.002233724

# comparing to OG model (not scaled)
ggaw4_og <- lm(rich_propT ~ grazing_m*aspect + wetness*aspect, data = grass)
summary(ggaw4_og)
  # grazing N = -0.0012792
  # grazing S = 0.0037685
  # wetness N = 0.0029648
  # wetness S = -0.0021458
# very similar 

# HERB = lm(rich_propT ~ grazing_m_s + aspect + wetness_s)
summary(hgaw)      ## USE THIS MODEL'S RESULTS ##
  # aspect NORTH (intercept): 0.162522 ± 0.011884
  # aspect SOUTH: -0.030283 ± 0.017697, p = 0.098100 (not significant)
  # grazing: 0.001077 ± 0.009160, p = 0.907262 (not significant)
  # wetness_s: 0.040812 ± 0.009536, p = 0.000198 (SIGNIFICANT)
# adjusted R2 = 0.3639

## Interpreting my model
# grazing 
0.001077/sd(herb$grazing_m)  # = 0.0001265944
# wetness 
0.040812/sd(herb$wetness)    # = 0.002248992

# comparing to OG model (not scaled)
hgaw_og <- lm(rich_propT ~ grazing_m + aspect + wetness, data = herb)
summary(hgaw_og)
  # grazing = 0.0001273
  # wetness = 0.0022107
# very similar 


# . ----
#### Landscape Models ----
### Grazing ~ Landscape ----
hist(plot_grazing$grazing_m)  # a bit skewed

# wetness
gw <- lm(grazing_m ~ wetness, data = plot_grazing)
hist(resid(gw))
plot(gw)  # looks like I have a bit of an outlier
shapiro.test(resid(gw))  # not normal
bptest(gw)  # no heteroskedasticity

# wetness + aspect
gwa <- lm(grazing_m ~ wetness + aspect, data = plot_grazing)
shapiro.test(resid(gwa))  # not normal
bptest(gwa)  # no heteroskedasticity

gwa2 <- lm(grazing_m ~ wetness * aspect, data = plot_grazing)
shapiro.test(resid(gwa2))  # not normal
bptest(gwa2)  # no heteroskedasticity

# NDVI
gn <- lm(grazing_m ~ ndvi, data = plot_grazing)
plot(gn)  
shapiro.test(resid(gn))  # not normal
bptest(gn)  # no heteroskedasticity though

# NDVI + aspect
gna <- lm(grazing_m ~ ndvi + aspect, data = plot_grazing)
shapiro.test(resid(gna))  # not normal
bptest(gna)  # no heteroskedasticity though

gna2 <- lm(grazing_m ~ ndvi * aspect, data = plot_grazing)
shapiro.test(resid(gna2))  # not normal
bptest(gna2)  # no heteroskedasticity though

# slope
gs <- lm(grazing_m ~ slope_deg, data = plot_grazing)
plot(gs)  
shapiro.test(resid(gs))  # not normal
bptest(gs)  # no heteroskedasticity

# slope + aspect
gsa <- lm(grazing_m ~ slope_deg + aspect, data = plot_grazing)
shapiro.test(resid(gsa))  # not normal
bptest(gsa)  # no heteroskedasticity

gsa2 <- lm(grazing_m ~ slope_deg * aspect, data = plot_grazing)
shapiro.test(resid(gsa2))  # not normal
bptest(gsa2)  # and heteroskedasticity

### Transformations
## log
# wetness
gw_log <- lm(log(grazing_m) ~ wetness, data = plot_grazing)
shapiro.test(resid(gw_log))  # not normal
bptest(gw_log)  # no heteroskedasticity though

gwa_log <- lm(log(grazing_m) ~ wetness + aspect, data = plot_grazing)
shapiro.test(resid(gwa_log))  # not normal
bptest(gwa_log)  # no heteroskedasticity though

gwa_log2 <- lm(log(grazing_m) ~ wetness * aspect, data = plot_grazing)
shapiro.test(resid(gwa_log2))  # not normal
bptest(gwa_log2)  # no heteroskedasticity though

# NDVI
gn_log <- lm(log(grazing_m) ~ ndvi, data = plot_grazing)
shapiro.test(resid(gn_log))  # not normal
bptest(gn_log)  # no heteroskedasticity though

gna_log <- lm(log(grazing_m) ~ ndvi + aspect, data = plot_grazing)
shapiro.test(resid(gna_log))  # not normal 
bptest(gna_log)  # no heteroskedasticity though

gna_log2 <- lm(log(grazing_m) ~ ndvi * aspect, data = plot_grazing)  
shapiro.test(resid(gna_log2))  # not normal
bptest(gna_log2)  # and heteroskedasticity

# slope
gs_log <- lm(log(grazing_m) ~ slope_deg, data = plot_grazing)
shapiro.test(resid(gs_log))  # not normal
bptest(gs_log)  # and heteroskedasticity

gsa_log <- lm(log(grazing_m) ~ slope_deg + aspect, data = plot_grazing)
shapiro.test(resid(gsa_log))  # not normal
bptest(gsa_log)  # and heteroskedasticity

gsa_log2 <- lm(log(grazing_m) ~ slope_deg * aspect, data = plot_grazing)
shapiro.test(resid(gsa_log2))  # not normal
bptest(gsa_log2)  # and heteroskedasticity

## sqrt
# wetness
gw_sqrt <- lm(sqrt(grazing_m) ~ wetness, data = plot_grazing)
shapiro.test(resid(gw_sqrt))  # not normal
bptest(gw_sqrt)  # no heteroskedasticity though

gwa_sqrt <- lm(sqrt(grazing_m) ~ wetness + aspect, data = plot_grazing)
shapiro.test(resid(gwa_sqrt))  # not normal
bptest(gwa_sqrt)  # no heteroskedasticity

gwa_sqrt2 <- lm(sqrt(grazing_m) ~ wetness * aspect, data = plot_grazing)
shapiro.test(resid(gwa_sqrt2))  # not normal
bptest(gwa_sqrt2)  # no heteroskedasticity

# NDVI
gn_sqrt <- lm(sqrt(grazing_m) ~ ndvi, data = plot_grazing)
shapiro.test(resid(gn_sqrt))  # not normal
bptest(gn_sqrt)  # no heteroskedasticity

gna_sqrt <- lm(sqrt(grazing_m) ~ ndvi + aspect, data = plot_grazing)
shapiro.test(resid(gna_sqrt))  # not normal 
bptest(gna_sqrt)  # no heteroskedasticity

gna_sqrt2 <- lm(sqrt(grazing_m) ~ ndvi * aspect, data = plot_grazing)  
shapiro.test(resid(gna_sqrt2))  # not normal
bptest(gna_sqrt2)  # and heteroskedasticity

# slope
gs_sqrt <- lm(sqrt(grazing_m) ~ slope_deg, data = plot_grazing)
shapiro.test(resid(gs_sqrt))  # not normal
bptest(gs_sqrt)  # and heteroskedasticity

gsa_sqrt <- lm(sqrt(grazing_m) ~ slope_deg + aspect, data = plot_grazing)
shapiro.test(resid(gsa_sqrt))  # not normal
bptest(gsa_sqrt)  # no heteroskedasticity

gsa_sqrt2 <- lm(sqrt(grazing_m) ~ slope_deg * aspect, data = plot_grazing)
shapiro.test(resid(gsa_sqrt2))  # not normal
bptest(gsa_sqrt2)  # and heteroskedasticity 

### GLM's 
# wetness
gw_glm <- glm(grazing_m ~ wetness, data = plot_grazing)
hist(resid(gw_glm))  # sort of normal-looking, maybe a bit skewed 

gw_glm2 <- glm(grazing_m ~ wetness, family = Gamma(link = "log"), data = plot_grazing)

summary(gw_glm)
summary(gw_glm2)  # way less/ not over-dispersed with Gamma (residual deviance < DF)

gwa_glm <- glm(grazing_m ~ wetness + aspect, family = Gamma(link = "log"), data = plot_grazing)
gwa_glm2 <- glm(grazing_m ~ wetness*aspect, family = Gamma(link = "log"), data = plot_grazing)

# NDVI
gn_glm <- glm(grazing_m ~ ndvi, family = Gamma(link = "log"), data = plot_grazing)
gna_glm <- glm(grazing_m ~ ndvi + aspect, family = Gamma(link = "log"), data = plot_grazing)
gna_glm2 <- glm(grazing_m ~ ndvi*aspect, family = Gamma(link = "log"), data = plot_grazing)

# slope
gs_glm <- glm(grazing_m ~ slope_deg, family = Gamma(link = "log"), data = plot_grazing)
gsa_glm <- glm(grazing_m ~ slope_deg + aspect, family = Gamma(link = "log"), data = plot_grazing)
gsa_glm2 <- glm(grazing_m ~ slope_deg*aspect, family = Gamma(link = "log"), data = plot_grazing)

# soil depth
gd_glm <- glm(grazing_m ~ soil_depth, family = Gamma(link = "log"), data = plot_grazing)
gda_glm <- glm(grazing_m ~ soil_depth + aspect, family = Gamma(link = "log"), data = plot_grazing)
gda_glm2 <- glm(grazing_m ~ soil_depth*aspect, family = Gamma(link = "log"), data = plot_grazing)


min(AIC(gw_glm, gw_glm2, gwa_glm, gwa_glm2, gn_glm, gna_glm, gna_glm2, gs_glm, gsa_glm, 
        gsa_glm2, gd_glm, gda_glm, gda_glm2)[,2])  # lowest = 670.6974
AIC(gw_glm, gw_glm2, gwa_glm, gwa_glm2, gn_glm, gna_glm, gna_glm2, gs_glm, gsa_glm, gsa_glm2,
    gd_glm, gda_glm, gda_glm2)
  # 'gw_glm2' is lowest 

### Multiple variables
# 2-3 variables
gwn_glm <- glm(grazing_m ~ wetness_s + ndvi_s, family = Gamma(link = "log"), data = plot_grazing)
gws_glm <- glm(grazing_m ~ wetness_s + slope_deg_s, family = Gamma(link = "log"), data = plot_grazing)
gwd_glm <- glm(grazing_m ~ wetness_s + soil_depth_s, family = Gamma(link = "log"), data = plot_grazing)
gwns_glm <- glm(grazing_m ~ wetness_s + ndvi_s + slope_deg_s, family = Gamma(link = "log"), 
                data = plot_grazing)
gwnd_glm <- glm(grazing_m ~ wetness_s + ndvi_s + soil_depth_s, family = Gamma(link = "log"), 
                data = plot_grazing)
gwsd_glm <- glm(grazing_m ~ wetness_s + soil_depth_s + slope_deg_s, family = Gamma(link = "log"), 
                data = plot_grazing)

gwn_glm2 <- glm(grazing_m ~ wetness_s*ndvi_s, family = Gamma(link = "log"), data = plot_grazing)
gws_glm2 <- glm(grazing_m ~ wetness_s*slope_deg_s, family = Gamma(link = "log"), data = plot_grazing)


# with aspect 
gwna_glm <- glm(grazing_m ~ wetness_s + ndvi_s + aspect, family = Gamma(link = "log"), 
                data = plot_grazing)
gwna_glm2 <- glm(grazing_m ~ wetness_s + ndvi_s*aspect, family = Gamma(link = "log"), 
                 data = plot_grazing)
gwna_glm3 <- glm(grazing_m ~ wetness_s*aspect + ndvi_s, family = Gamma(link = "log"), 
                 data = plot_grazing)
gwna_glm4 <- glm(grazing_m ~ wetness_s*aspect + ndvi_s*aspect, family = Gamma(link = "log"), 
                 data = plot_grazing)

gwsa_glm <- glm(grazing_m ~ wetness_s + slope_deg_s + aspect, family = Gamma(link = "log"), 
                data = plot_grazing)
gwsa_glm2 <- glm(grazing_m ~ wetness_s + slope_deg_s*aspect, family = Gamma(link = "log"), 
                 data = plot_grazing)
gwsa_glm3 <- glm(grazing_m ~ wetness_s*aspect + slope_deg_s, family = Gamma(link = "log"), 
                 data = plot_grazing)
gwsa_glm4 <- glm(grazing_m ~ wetness_s*aspect + slope_deg_s*aspect, family = Gamma(link = "log"), 
                 data = plot_grazing)

gwnsa_glm <- glm(grazing_m ~ wetness_s + ndvi_s + slope_deg_s + aspect, 
                 family = Gamma(link = "log"), data = plot_grazing)
gwnsa_glm2 <- glm(grazing_m ~ wetness_s*aspect + ndvi_s + slope_deg_s, family = Gamma(link = "log"), 
                  data = plot_grazing)
gwnsa_glm3 <- glm(grazing_m ~ wetness_s + ndvi_s*aspect + slope_deg_s, family = Gamma(link = "log"), 
                  data = plot_grazing)
gwnsa_glm4 <- glm(grazing_m ~ wetness_s + ndvi_s + slope_deg_s*aspect, family = Gamma(link = "log"), 
                  data = plot_grazing)
gwnsa_glm5 <- glm(grazing_m ~ wetness_s*aspect + ndvi_s + slope_deg_s*aspect, 
                  family = Gamma(link = "log"), data = plot_grazing)
gwnsa_glm6 <- glm(grazing_m ~ wetness_s + ndvi_s*aspect + slope_deg_s*aspect, 
                  family = Gamma(link = "log"), data = plot_grazing)
gwnsa_glm7 <- glm(grazing_m ~ wetness_s*aspect + ndvi_s*aspect + slope_deg_s, 
                  family = Gamma(link = "log"), data = plot_grazing)
gwnsa_glm8 <- glm(grazing_m ~ wetness_s*aspect + ndvi_s*aspect + slope_deg_s*aspect, 
                  family = Gamma(link = "log"), data = plot_grazing)


min(AIC(gw_glm2, gwn_glm, gws_glm, gwns_glm, gwna_glm, gwna_glm2, gwna_glm3, gwna_glm4,
        gwn_glm2, gws_glm2, gwsa_glm, gwsa_glm2, gwsa_glm3, gwsa_glm4, gwnsa_glm, gwnsa_glm2, 
        gwnsa_glm3, gwnsa_glm4, gwnsa_glm5, gwnsa_glm6, gwnsa_glm7, gwnsa_glm8)[,2])  
  # lowest = 670.6974
AIC(gw_glm2, gwn_glm, gws_glm, gwns_glm, gwna_glm, gwna_glm2, gwna_glm3, gwna_glm4,
    gwn_glm2, gws_glm2, gwsa_glm, gwsa_glm2, gwsa_glm3, gwsa_glm4, gwnsa_glm, gwnsa_glm2, 
    gwnsa_glm3, gwnsa_glm4, gwnsa_glm5, gwnsa_glm6, gwnsa_glm7, gwnsa_glm8)
  # 'gw_glm2' is best (wetness only, family = Gamma)

# null model
graze_null <- glm(grazing_m ~ 1, family = Gamma(link = "log"), data = plot_grazing)
AIC(graze_null, gw_glm2)  # better than the null


## Landscape Model Results ## ----
summary(gw_glm2)   ## USE THIS MODEL'S RESULTS ###
  # intercept = exp(2.553538) ± exp(0.104194)
  # wetness (NOT STANDARDIZED): exp(-0.020337) ± exp(0.004487), p = 1.62e-05

# calculating McFadden's R2 
with(summary(gw_glm2), 1 - deviance/null.deviance)  # R2 = 0.09118

# . ----
#### Other Basic Statistics ----
aspect <- sites %>% dplyr::select(plot_nr, aspect)
gisvar <- left_join(gisvar, aspect)

### Wetness ~ Aspect
t.test(wetness ~ aspect, data = gisvar)
  # t = -1.5389, df = 69.931, p-value = 0.1283
  # N = 11.41176, S = 17.00000 

# range
summary(ifelse(richness_site2$aspect == "N", richness_site2$wetness, NA))
  # min = 0.93, max = 38.33, mean = 11.42 
summary(ifelse(richness_site2$aspect == "S", richness_site2$wetness, NA))
  # min = 0.00, max = 88.08, mean = 17.05


### Slope angle ~ Aspect
t.test(slope_deg ~ aspect, data = gisvar)
  # t = -1.7695, df = 59.86, p-value = 0.0819
  # N = 5.543193, S = 7.058317 

# range
summary(ifelse(richness_site2$aspect == "N", richness_site2$slope_deg, NA))
  # min = 1.682, max = 8.354, mean = 5.542 
summary(ifelse(richness_site2$aspect == "S", richness_site2$slope_deg, NA))
  # min = 1.278, max = 20.671, mean = 7.058 

### NDVI ~ Aspect
t.test(ndvi ~ aspect, data = gisvar)
  # t = -1.9925, df = 99.937, p-value = 0.04904
  # N = 0.09076183, S = 0.11356689 

summary(ifelse(richness_site2$aspect == "N", richness_site2$ndvi, NA))
summary(ifelse(richness_site2$aspect == "S", richness_site2$ndvi, NA))


### Richness ~ Aspect
ra1 <- lm(richness_siteT ~ aspect, data = richness_site2)
shapiro.test(resid(ra1))  # normal 

t.test(richness_siteT ~ aspect, data = richness_site2)
  # t = 2.4577, DF = 31.866, p = 0.01961
  # N = 17.07, S = 14.73


### Richness Proportion ~ Sp Group
prop_lm <- lm(rich_propT ~ sp_group, data = richness_site)
shapiro.test(resid(prop_lm))  # not normal, use transformed model 

prop_lm2 <- lm(sqrt(rich_propT) ~ sp_group, data = richness_site)
shapiro.test(resid(prop_lm2))  # normal 

prop_aov <- aov(sqrt(rich_propT) ~ sp_group, data = richness_site)
summary(prop_aov)
  # p = <2e-16 ***, F = 41.76, DF = 4 

TukeyHSD(prop_aov)
  #                   diff        lower        upper       p 
  # herb-grass   -0.003650346 -0.048191544  0.04089085 0.9994153
  # lichen-grass  0.161196462  0.117335322  0.20505760 0.0000000
  # moss-grass    0.037537333 -0.006323807  0.08139847 0.1315350
  # shrub-grass   0.114726022  0.070864882  0.15858716 0.0000000
  # lichen-herb   0.164846808  0.120305609  0.20938801 0.0000000
  # moss-herb     0.041187679 -0.003353520  0.08572888 0.0846658
  # shrub-herb    0.118376368  0.073835169  0.16291757 0.0000000
  # moss-lichen  -0.123659129 -0.167520269 -0.07979799 0.0000000
  # shrub-lichen -0.046470440 -0.090331580 -0.00260930 0.0319417
  # shrub-moss    0.077188689  0.033327549  0.12104983 0.0000275

richness_site %>% group_by(sp_group) %>% summarize(prop = mean(rich_propT),
                                                   se = std.error(rich_propT))
  # grass: 0.149 ± 0.00752
  # herb: 0.149 ± 0.0108 
  # lichen: 0.300 ± 0.0135 
  # moss: 0.180 ± 0.00838
  # shrub: 0.251 ± 0.0108 

## N vs. S proportions per group
# lichen
t.test(rich_propT ~ aspect, data = lichen)  
  # t = -0.24378, df = 31.984, p-value = 0.809

# shrub
t.test(rich_propT ~ aspect, data = shrub)  
  # t = -1.6986, df = 27.656, p-value = 0.1006

# grass
t.test(rich_propT ~ aspect, data = grass)  
  # t = -0.47806, df = 31.857, p-value = 0.6359

# moss
t.test(rich_propT ~ aspect, data = moss)  
  # t = 1.5857, df = 30.661, p-value = 0.1231

# herb
t.test(rich_propT ~ aspect, data = herb)  
  # t = 0.66431, df = 26.431, p-value = 0.5122


### Coverage ~ Sp Group
cov_lm <- lm(coverage_perc ~ sp_group, data = coverage_site)
shapiro.test(resid(cov_lm))  # not normal, use transformed model 

cov_lm2 <- lm(sqrt(coverage_perc) ~ sp_group, data = coverage_site)
shapiro.test(resid(cov_lm2))  # almost normal but not quite

cov_lm3 <- lm(log(coverage_perc) ~ sp_group, data = coverage_site)
shapiro.test(resid(cov_lm3))  # not normal

# using non-parametric test instead
cov_kruskal <- kruskal.test(coverage_perc ~ sp_group, data = coverage_site)
cov_kruskal
  # p = < 2.2e-16, Kruskal-Wallis chi-squared = 117.78, DF = 4 

pairwise.wilcox.test(coverage_site$coverage_perc, coverage_site$sp_group)
  #        grass    herb     lichen  moss   
  # herb   7.8e-06  -        -        -      
  # lichen 1.7e-06  2.3e-14  -        -      
  # moss   0.0015   2.6e-12  0.0056   -      
  # shrub  3.2e-16  < 2e-16  7.0e-08  6.5e-15


# average coverage per group
coverage_site %>% group_by(sp_group) %>% summarize(cov = mean(coverage_perc),
                                                   se = std.error(coverage_perc))
  # grass: 6.73 ± 1.44
  # herb: 1.58 ± 0.340 
  # lichen: 23.7 ± 3.29 
  # moss: 11.5 ± 1.56
  # shrub: 57.0 ± 3.51 


## N vs. S coverage per group
# lichen
lichen_cov <- coverage_site %>% filter(sp_group == "lichen")
wilcox.test(coverage_perc ~ aspect, data = lichen_cov)  
# W = 139, p-value = 0.8651

# shrub
shrub_cov <- coverage_site %>% filter(sp_group == "shrub")
wilcox.test(coverage_perc ~ aspect, data = shrub_cov)  
# W = 116, p-value = 0.3394

# grass
grass_cov <- coverage_site %>% filter(sp_group == "grass")
wilcox.test(coverage_perc ~ aspect, data = grass_cov)  
# W = 159, p-value = 0.6339

# moss
moss_cov <- coverage_site %>% filter(sp_group == "moss")
wilcox.test(coverage_perc ~ aspect, data = moss_cov)  
# W = 199, p-value = 0.02291

# herb
herb_cov <- coverage_site %>% filter(sp_group == "herb")
wilcox.test(coverage_perc ~ aspect, data = herb_cov)  
# W = 154, p-value = 0.331


# average coverage per group by aspect
coverage_site %>% group_by(aspect, sp_group) %>% summarize(cov = mean(coverage_perc),
                                                           se = std.error(coverage_perc))
# grass: N = 7.36, S = 6.11
# herb: N = 1.49, S = 1.69
# lichen: N = 24.3, S = 23.1
# moss: N = 12.8, S = 10.1
# shrub: N = 54.2, S = 59.9

?summarize

### Summary of species 
## How many were found
rich_sum <- plots %>% 
              group_by(sp_group) %>% 
              summarize(richness = length(unique(sp_latin)))
rich_sum
  # moss = 10, grass = 14, shrub = 15, lichen = 20, herb = 28

sum(rich_sum$richness)  # found 87 species in total 

plots %>% 
  group_by(aspect) %>% 
  summarize(richness = length(unique(sp_latin)))

# S = 70, N = 61 

# on average per site
richness_site %>% 
  group_by(sp_group) %>% 
  summarize(richness = mean(richness_group),
            se = std.error(richness_group))
  # grass: 2.41 ± 0.174
  # herb: 2.56 ± 0.240
  # lichen: 4.63 ± 0.166
  # moss: 2.85 ± 0.168
  # shrub: 3.90 ± 0.161

richness_site %>% 
  group_by(aspect) %>% 
  summarize(richness = mean(richness_siteT),
            se = std.error(richness_siteT))

# N = 17.1 ± 0.284
# S = 14.7 ± 0.310

                              
## What was most prominent 
plots %>% group_by(sp_latin) %>% summarize(most = length(sp_latin)) %>% arrange(desc(most))

aspect <- sites %>% dplyr::select(plot_nr, aspect)
plots <- left_join(plots, aspect)

latin_sum <- plots %>% 
                group_by(sp_latin, aspect) %>% 
                summarize(most = length(sp_latin)) %>% 
                pivot_wider(names_from = "aspect", values_from = "most") 

# top 5 for S-slopes:
  # Cladonia rangiferina, Dicranum scoparium, Empetrum nigrum ssp. hermaphroditum,
  # Vaccinium vitis-idaea, Juncus trifidus
# top 5 for N-slopes:
  # Cladonia rangiferina, Dicranum scoparium, Vaccinium vitis-idaea, Cladonia bellidiflora,
  # Polytrichum hyperboreum

# . ----
#### NMDS Relative Abundance ----
### NMDS ~ Plot ----
## Dividing coverage across species
sp_cov2 <- left_join(plots, coverage)
sp_cov2 <- sp_cov2 %>% 
              dplyr::select(plot_nr, aspect, sp_latin, sp_group, coverage_perc) %>% 
              group_by(plot_nr, sp_group) %>% 
              mutate(rel_abund = coverage_perc/length(sp_group)) %>% 
              ungroup() %>% 
              na.omit() %>% 
              dplyr::select(!c(coverage_perc, sp_group))

print(sp_cov2 %>% group_by(plot_nr) %>% summarise(sum(rel_abund)), n = 105)  # all = 100%! 

## Making a matrix
sp_matrix3 <- sp_cov2 %>% 
                  distinct() %>% 
                  pivot_wider(names_from = "sp_latin", values_from = "rel_abund", 
                              values_fill = NA)

sp_matrix3[is.na(sp_matrix3)] <- 0  # making sure non-existent sp just have coverage of 0

# with only abundance data 
sp_matrix4 <- sp_matrix3 %>% 
                 dplyr::select(!c(plot_nr, aspect)) 

## Looking at how many axes to extract 
set.seed(9)
mds1b <- metaMDS(sp_matrix4, distance = "bray", k = 1, sratmax = 0.99999,    # no convergence
                 autotransform = F)   
mds2b <- metaMDS(sp_matrix4, distance = "bray", k = 2, sratmax = 0.99999,    # no convergence
                 autotransform = F)   
mds3b <- metaMDS(sp_matrix4, distance = "bray", k = 3, sratmax = 0.99999,    # no convergence
                 autotransform = F) 
mds4b <- metaMDS(sp_matrix4, distance = "bray", k = 4, sratmax = 0.99999,    # solution found 
                 autotransform = F)
  # stress = 0.1258
mds5b <- metaMDS(sp_matrix4, distance = "bray", k = 5, sratmax = 0.99999,    # solution found
                 autotransform = F)    
  # stress = 0.0972
mds6b <- metaMDS(sp_matrix4, distance = "bray", k = 6, sratmax = 0.99999,    # solution found 
                 autotransform = F) 
  # stress = 0.0794
mds7b <- metaMDS(sp_matrix4, distance = "bray", k = 7, sratmax = 0.99999,    # solution found 
                 autotransform = F) 
  # stress = 0.06616
mds8b <- metaMDS(sp_matrix4, distance = "bray", k = 8, sratmax = 0.99999,    # solution found
                 autotransform = F) 
  # stress = 0.05608
mds9b <- metaMDS(sp_matrix4, distance = "bray", k = 9, sratmax = 0.99999,    # no convergence
                 autotransform = F) 
mds10b <- metaMDS(sp_matrix4, distance = "bray", k = 10, sratmax = 0.99999,    # no convergence
                  autotransform = F)  

scree <- cbind(rbind(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 
               rbind(mds1b$stress, mds2b$stress, mds3b$stress, mds4b$stress, mds5b$stress, 
                     mds6b$stress, mds7b$stress, mds8b$stress, mds9b$stress, mds10b$stress))
plot(scree)
# ~6 or more dimensions = <0.1 stress (which is good)

stressplot(mds6b)   # non-metric fit = 0.994, linear fit = 0.934stressplot(mds7b)   # non-metric fit = 0.996, linear fit = 0.949
stressplot(mds8b)   # non-metric fit = 0.997, linear fit = 0.959
stressplot(mds9b)   # non-metric fit = 0.998, linear fit = 0.967

# 7 dimensions improves the linear fit quite a bit, then afterwards it tapers off 

# choosing k = 7

set.seed(7)
nmds7 <- metaMDS(sp_matrix4, distance = "bray", k = 7, autotransform = F, trymax = 500, 
                 sratmax = 0.99999)  
# note that autotransformation is off (default = T) 
?metaMDS
nmds7
# stress = ~0.06575
# 7 dimensions 


#### Effect of Env Variables ----
### For Sites
# removing unnecessary info from GIS data 
gisvar2 <- gisvar %>% 
              filter(!(plot_nr == 15)) %>% dplyr::select(!c(X, Y))

# making environmental variable vectors for the site 
gisvar_long <- left_join(gisvar2, sites)  

## Not standardized 
gisvar_sites <- gisvar_long %>% 
                  dplyr::select(site_nr, ndvi, slope_deg, wetness, soil_depth) %>% 
                  group_by(site_nr) %>% 
                  summarize(ndvi = mean(ndvi),
                            slope_deg = mean(slope_deg),
                            soil_depth = mean(soil_depth),
                            wetness = mean(wetness)) %>% 
                  ungroup() %>% 
                  dplyr::select(!site_nr)

gisfit <- envfit(nmds6, gisvar_sites, permu = 999)
gisfit   
# NDVI, slope and wetness correlate with the NMDS 
  # ndvi: R2 = 0.3565, p = 0.002 (SIGNIFICANT)
  # slope: R2 = 0.2565, p = 0.011 (SIGNIFICANT)
  # wetness: R2 = 0.2014, p = 0.031 (SIGNIFICANT)
  # soil depth: R2 = 0.0175, p = 0.754


## Standardized variables
gisvar_sites2 <- gisvar_long %>% 
                  dplyr::select(site_nr, ndvi_s, slope_deg_s, wetness_s, soil_depth_s) %>% 
                  group_by(site_nr) %>% 
                  summarize(ndvi = mean(ndvi_s),
                            slope_deg = mean(slope_deg_s),
                            soil_depth = mean(soil_depth_s),
                            wetness = mean(wetness_s)) %>% 
                  ungroup() %>% 
                  dplyr::select(!site_nr)

gisfit2 <- envfit(nmds6, gisvar_sites2, permu = 999)
gisfit2   
  # GIS variables DO have an effect
    # ndvi: R2 = 0.3552, p = 0.002 (SIGNIFICANT)
    # slope: R2 = 0.2560, p = 0.012 (SIGNIFICANT)
    # wetness: R2 = 0.2015, p = 0.025 (SIGNIFICANT)
    # soil depth: R2 = 0.0171, p = 0.776 


### For Plots 
## Not standardized 
gisvar_plots <- gisvar_long %>%  
                  dplyr::select(ndvi, slope_deg, wetness, soil_depth, grazing_m)

gisfit3 <- envfit(nmds7, gisvar_plots, permu = 999)
gisfit3   
  # ndvi: R2 = 0.2254, p = 0.001 (SIGNIFICANT)
  # slope: R2 = 0.2180, p = 0.001 (SIGNIFICANT)
  # wetness: R2 = 0.1994, p = 0.001 (SIGNIFICANT) 
  # soil depth: R2 = 0.0073, p = 0.684
  # grazing: R2 = 0.0037, p = 0.824

# splitting by aspect
sp_matrixN <- sp_matrix3 %>%   # could go back and rename objects if no site-level NMDS anymore
                filter(aspect == "N") %>% 
                dplyr::select(!c(aspect, site_nr, plot_nr, plot_rep))
sp_matrixS <- sp_matrix3 %>%   
                filter(aspect == "S") %>% 
                dplyr::select(!c(aspect, site_nr, plot_nr, plot_rep))

set.seed(7)
nmds7N <- metaMDS(sp_matrixN, distance = "bray", k = 7, autotransform = F, trymax = 500, 
                  sratmax = 0.99999)  
nmds7S <- metaMDS(sp_matrixS, distance = "bray", k = 7, autotransform = F, trymax = 500, 
                  sratmax = 0.99999)  

gisvarN <- gisvar_long %>%  
              filter(aspect == "N") %>% 
              dplyr::select(ndvi, slope_deg, wetness, soil_depth, grazing_m)
gisvarS <- gisvar_long %>%  
            filter(aspect == "S") %>% 
            dplyr::select(ndvi, slope_deg, wetness, soil_depth, grazing_m)

gisfitN <- envfit(nmds7N, gisvarN, permu = 999)
gisfitN

gisfitS <- envfit(nmds7S, gisvarS, permu = 999)
gisfitS


## Standardized variables 
gisvar_plots2 <- gisvar_long %>%  
                  dplyr::select(ndvi_s, slope_deg_s, wetness_s, soil_depth_s)

gisfit4 <- envfit(nmds7, gisvar_plots2, permu = 999)
gisfit4   
  # ndvi: R2 = 0.2254, p = 0.001 (SIGNIFICANT)
  # slope: R2 = 0.2180, p = 0.001 (SIGNIFICANT)
  # wetness: R2 = 0.1994, p = 0.001 (SIGNIFICANT) 
  # soil depth: R2 = 0.0073, p = 0.688  


#### Visualizing NMDS's ----
### Site
# not standardized 
plot(nmds6, type = "t", display = "sites") 
plot(gisfit, p.max = 0.05, col = "red")
dev.off()

# standardized 
plot(nmds6, type = "t", display = "sites") 
plot(gisfit2, p.max = 0.05, col = "red")
dev.off()


### Plot
plot(nmds7, type = "t", display = "sites") 
plot(gisfit, p.max = 0.05, col = "red")
dev.off()

# north
plot(nmds7N, type = "t", display = "sites") 
plot(gisfitN, p.max = 0.05, col = "red")
dev.off()

plot(nmds7S, type = "t", display = "sites") 
plot(gisfitS, p.max = 0.05, col = "red")
dev.off()


### GIS ~ Sp 
gisvar_plots <- gisvar_plots %>% 
                  mutate(plot_rep2 = case_when(plot == "1" ~ "A",
                                               plot == "2" ~ "B",
                                               plot == "3" ~ "C")) %>% 
                  mutate(site_plot = str_c(site_nr, "", plot_rep2))

plot(xlim = c(-1, 1), ylim = c(-1, 1), nmds4, type = "t", "species") 
text(nmds4, labels = gisvar_plots$site_plot, cex = 0.5, col = gisvar_plots$site_nr)
ordiellipse(nmds4, gisvar_long$aspect, kind = "sd", label = T)
# dev.off()

## envfit() for relative abundance 
spfit <- envfit(nmds4, sp_matrix4, permutations = 999) 
spfit
  # species found to be significantly associated with GIS variables:
    # Rubus chamaemorus: R2 = 0.186, p = 0.001
    # Betula nana: R2 = 0.0829, p = 0.014
    # Nephroma expallidum: R2 = 0.0697, p = 0.043
    # Vaccinium vitis-idaea: R2 = 0.1888, p = 0.001
    # Empetrum nigrum ssp. hermaphroditum: R2 = 0.1306, p = 0.001
    # Deschampsia flexuosa: R2 = 0.2082, p = 0.001
    # Arctostaphylos alpina: R2 = 0.1406, p = 0.004
    # Lysimachia europaea: R2 = 0.0973, p = 0.005
    # Deschampsia cespitosa: R2 = 0.1130, p = 0.008
    # Juncus trifidus: R2 = 0.1499, p = 0.002
    # Nephroma arcticum: R2 = 0.1723, p = 0.001
    # Flavocetraria nivalis: R2 = 0.0546, p = 0.05 ???
    # Harrimanella hypnoides: R2 = 0.0659, p = 0.035
    # Antennaria alpina: R2 = 0.0909, p = 0.014
    # Pilosella sp.: R2 = 0.0942, p = 0.012
    # Ranunculus recurvatus?: R2 = 0.1466, p = 0.001
    # Stereocaulon alpinum: R2 = 0.1561, p = 0.002
    # Bistorta vivipara: R2 = 0.0828, p = 0.017
    # Solidago virgaurea ssp. lapponica: R2 = 0.2470, p = 0.001
    # Bartsia alpina: R2 = 0.1170, p = 0.002
    # Anthoxanthum nipponicum: R2 = 0.1359, p = 0.002
    # Phyllodoce caerulea: R2 = 0.1063, p = 0.003
    # Parnassia palutris: R2 = 0.0754, p = 0.045
    # Festuca vivipara ssp. vivipara : R2 = 0.0754, p = 0.045
    # Tofieldia pusilla: R2 = 0.0754, p = 0.045
    # Viola biflora: R2 = 0.0754, p = 0.045
    # Huperzia selago ssp. arctica: R2 = 0.0668, p = 0.036
    # Juncus alpinoarticulatus ssp. alpestris: R2 = 0.1761, p = 0.002
    # Bryum pseudotriquetrum: R2 = 0.0697, p = 0.045
    # Nardus stricta: R2 = 0.0927, p = 0.008


# plot(nmds4, type = "n") 
# text(nmds4, labels = gisvar_plots$site_plot, cex = 0.5, col = gisvar_plots$site_nr)
# ordiellipse(nmds4, gisvar_long$aspect, kind = "sd", label = T)
plot(spfit, p.max = 0.001, cex = 0.8)  # adding arrows to the above plot §

# plot(gisfit2, p.max = 0.01, col = "red")
dev.off()

#### ANOSIM Tests ----
### By Site 
## Rel abundance ~ aspect 
ano_sp <- anosim(sp_matrix2, sp_matrix$aspect, distance = "bray", permutations = 9999)
ano_sp
  # ANOSIM R = 0.04279, p = 0.1118 (not significant)

### By Plot
## Rel abundance ~ aspect 
ano_sp2 <- anosim(sp_matrix4, sp_matri3a$aspect, distance = "bray", permutations = 9999)
ano_sp2
# ANOSIM R = 0.03782, p = 0.0134 (SIGNIFICANT)



#### Indicator Species Test ----
### By Site 
# sp_matrix2 = matrix with abundance

## With aspect 
aspect <- sp_matrix$aspect

indsp <- multipatt(sp_matrix2, aspect, func = "r.g", control = how(nperm = 9999))
summary(indsp)
  # those found significantly more in N-facing slopes:
    # Cladonia pyxidata: stat = 0.399, p = 0.0143 
    # Polytrichum commune: stat = 0.393, p = 0.0196
    # Barbilophozia kunzeana: stat = 0.376, p = 0.0469

  # those found significantly more in S-facing slopes:
    # Empetrum nigrum ssp. hermaphroditum: stat = 0.421, p = 0.0146

## With grazing categories
grazecat2 <- grazecat$grazecat

indsp2 <- multipatt(sp_matrix2, grazecat2, func = "r.g", control = how(nperm = 9999))
summary(indsp2)


### By Plot
# sp_matrix3 = matrix with abundance

## With aspect 
aspect <- sp_matrix3$aspect

indsp2 <- multipatt(sp_matrix4, aspect, func = "r.g", control = how(nperm = 9999))
summary(indsp2)
# those found significantly more in N-facing slopes:
  # Cladonia pyxidata: stat = 0.327, p = 0.0005 
  # Polytrichum commune: stat = 0.253, p = 0.0068

# those found significantly more in S-facing slopes:
  # Empetrum nigrum ssp. hermaphroditum: stat = 0.329, p = 0.0005
  # Arctostaphylos alpina: stat = 0.210, p = 0.0358
  # Cladonia deformis: stat = 0.203, p = 0.0411


# . ----
#### Autocorrelation Test ----
## Seeing if my sites that are closer together are more similar than those further apart
sites_m <- sites %>% filter(plot == 1) %>% dplyr::select(x_coord, y_coord)
            
# creating a geographic df with Haversine distance 
sites_d <- distm(sites_m, fun = distHaversine)
sites_dist <- as.dist(sites_d)

# creating relative abundance df with Bray-Curtis distance
sp_dist <- vegdist(sp_matrix2, method = "bray")


# rel abundance vs location 
mantel_sp <- mantel(sp_dist, sites_dist, method = "spearman", permutations = 9999, na.rm = TRUE)
mantel_sp
  # not spatially autocorrelated
  # used the Spearman method (non-parametric)
    # p = 0.2268 (not significant)
    # Mantel statistic r = 0.04687



# . ----
# . ----
#### OLD RESULTS #### ####
#### Richness ~ Grazing (Sp Group Proportions) ----
# do model with rich_group_prop ~ grazing_m + sp_group + aspect 
hist(richness_site$rich_propT)   # a bit skewed 

rpg1 <- lm(rich_propT ~ grazing_m, data = richness_site)
plot(rpg1)
shapiro.test(resid(rpg1))   # p < 0.05 = not normal
bptest(rpg1)  # p > 0.05 = no heteroskedasticity though 

rpg2 <- lm(rich_propT ~ grazing_m + sp_group, data = richness_site)
plot(rpg2)
shapiro.test(resid(rpg2))   # p < 0.05 = not normal
bptest(rpg2)  # p < 0.05 = and heteroskedasticity 

rpg3 <- lm(rich_propT ~ grazing_m*sp_group, data = richness_site)
plot(rpg3)
shapiro.test(resid(rpg3))   # p < 0.05 = not normal
bptest(rpg3)  # p > 0.05 = no heteroskedasticity 

## Transformations 
# log
rpg2_log <- lm(log(rich_propT) ~ grazing_m + sp_group, data = richness_site)
shapiro.test(resid(rpg2_log))   # p > 0.05 = normal
bptest(rpg2_log)  # p < 0.05 = but heteroskedasticity
hist(resid(rpg2_log))

# sqrt
rpg2_sqrt <- lm(sqrt(rich_propT) ~ grazing_m + sp_group, data = richness_site)
shapiro.test(resid(rpg2_sqrt))   # p > 0.05 = normal
bptest(rpg2_sqrt)  # p > 0.05 = and no heteroskedasticity   (USE THIS!!)
hist(resid(rpg2_sqrt))
summary(rpg2_sqrt)

rpg2_sqrt2 <- lm(sqrt(rich_propT) ~ grazing_m*sp_group, data = richness_site)

AIC(rpg2_sqrt, rpg2_sqrt2)  # without interaction is best 


### Model with aspect 
rpga1 <- lm(sqrt(rich_propT) ~ grazing_m + sp_group + aspect, data = richness_site)
shapiro.test(resid(rpga1))   # p > 0.05 = normal
bptest(rpga1)  # p > 0.05 = and no heteroskedasticity 

rpga2 <- lm(sqrt(rich_propT) ~ grazing_m*aspect + sp_group, data = richness_site)

AIC(rpg2_sqrt, rpga1, rpga2)  # 'rpg2_sqrt' is the best, almost 2 units   


### Models with GIS variables (+ aspect)
## NDVI
rpgn_sqrt <- lm(sqrt(rich_propT) ~ grazing_m + sp_group + ndvi, data = richness_site)
rpgna_sqrt <- lm(sqrt(rich_propT) ~ grazing_m + sp_group + aspect + ndvi, data = richness_site)
rpgna_sqrt2 <- lm(sqrt(rich_propT) ~ grazing_m + sp_group + aspect*ndvi, data = richness_site)
rpgna_sqrt3 <- lm(sqrt(rich_propT) ~ grazing_m*aspect + sp_group + ndvi, data = richness_site)


AIC(rpg2_sqrt, rpga1, rpga2, rpgn_sqrt, rpgna_sqrt, rpgna_sqrt2, rpgna_sqrt3)
# 'rpg2_sqrt' is best 

## Slope
rpgs_sqrt <- lm(sqrt(rich_propT) ~ grazing_m + sp_group + slope_deg, data = richness_site)
rpgsa_sqrt <- lm(sqrt(rich_propT) ~ grazing_m + sp_group + aspect + slope_deg, data = richness_site)
rpgsa_sqrt2 <- lm(sqrt(rich_propT) ~ grazing_m + sp_group + aspect*slope_deg, data = richness_site)
rpgsa_sqrt3 <- lm(sqrt(rich_propT) ~ grazing_m*aspect + sp_group + slope_deg, data = richness_site)

AIC(rpg2_sqrt, rpga1, rpga2, rpgs_sqrt, rpgsa_sqrt, rpgsa_sqrt2, rpgsa_sqrt3)  
# 'rpg2_sqrt' is best (simplest)

## Soil depth
rpgd_sqrt <- lm(sqrt(rich_propT) ~ grazing_m + sp_group + soil_depth, data = richness_site)
rpgda_sqrt <- lm(sqrt(rich_propT) ~ grazing_m + sp_group + aspect + soil_depth, data = richness_site)
rpgda_sqrt2 <- lm(sqrt(rich_propT) ~ grazing_m + sp_group + aspect*soil_depth, data = richness_site)
rpgda_sqrt3 <- lm(sqrt(rich_propT) ~ grazing_m*aspect + sp_group + soil_depth, data = richness_site)

AIC(rpg2_sqrt, rpga1, rpga2, rpgd_sqrt, rpgda_sqrt, rpgda_sqrt2, rpgda_sqrt3)  
# 'rpg2_sqrt' is best 

## Wetness
rpgw_sqrt <- lm(sqrt(rich_propT) ~ grazing_m + sp_group + wetness, data = richness_site)
rpgwa_sqrt <- lm(sqrt(rich_propT) ~ grazing_m + sp_group + aspect + wetness, data = richness_site)
rpgwa_sqrt2 <- lm(sqrt(rich_propT) ~ grazing_m + sp_group + aspect*wetness, data = richness_site)
rpgwa_sqrt3 <- lm(sqrt(rich_propT) ~ grazing_m*aspect + sp_group + wetness, data = richness_site)

AIC(rpg2_sqrt, rpga1, rpga2, rpgw_sqrt,rpgwa_sqrt, rpgwa_sqrt2, rpgwa_sqrt3)  
# 'rpg2_sqrt' is best 

# adding GIS variables doesn't add anything 

## Choosing the best model 
rp_null <- lm(sqrt(rich_propT) ~ 1, data = richness_site)
AIC(rp_null, rpg2_sqrt)   # 'rpg2_sqrt' is the best


## Richness Proportion Model Results ## 
# lm(sqrt(rich_propT) ~ grazing_m + sp_group)

summary(rpg2_sqrt)      ## USE THIS MODEL'S RESULTS ##
# grass (intercept): 0.3838287^2 ± 0.0128474^2
# grazing: -0.0001127^2 ± 0.0006102^2, p = 0.8536 (not significant) 
# herb: -0.00363^2 ± 0.016195^2, p = 0.8229 (not significant)    
# lichen: 0.16119^2 ± 0.015947^2, p = < 2e-16 (SIGNIFICANT)
# moss: 0.037537^2 ± 0.0159474^2, p = 0.0198 (SIGNIFICANT)
# shrub: 0.114726^2 ± 0.015947^2, p = 2.21e-11 (SIGNIFICANT)

# adjusted R2 = 0.491 

# checking residuals 
plot(residuals(rpg2_sqrt) ~ predict(rpg2_sqrt, type = "response")) 
### Height ~ Grazing ----
hist(richness_site2$height_site)  # right skewed (positive skew)
head(richness_site2$height_site)


# testing to use another distribution
richness_site2 <- richness_site2 %>% 
  mutate(height_int = round(height_site*1000))

hganw_poi <- glm(height_int ~ grazing_m + aspect + ndvi + wetness, family = "poisson", 
                 data = richness_site2)
hganw_nb <- glm.nb(height_int ~ grazing_m + aspect + ndvi + wetness, 
                   data = richness_site2)

plot(hganw_poi)
plot(hganw_nb)
summary(hganw_poi)  # residual deviance >> DF = likely overdispersed 
summary(hganw_nb)  # residual deviance and DF = very similar = not overdispersed = better
# intercept: exp(7.773465)   (DIVIDE BY 1000 TO BACKTRANSFORM!!)
# 

summary()



hg1 <- lm(height_site ~ grazing_m, data = richness_site2)
plot(hg1)
shapiro.test(resid(hg1))   # p < 0.05 = not normal (skewed)
bptest(hg1)  # p > 0.05 = no heteroskedasticity though 

## Transformations 
# log
hg2 <- lm(log(height_site) ~ grazing_m, data = richness_site2)
plot(hg2)
shapiro.test(resid(hg2))   # p < 0.05 = not normal
bptest(hg2)  # p > 0.05 = no heteroskedasticity 

# sqrt 
hg3 <- lm(sqrt(height_site) ~ grazing_m, data = richness_site2)
plot(hg3)
shapiro.test(resid(hg3))   # p < 0.05 = not normal, almost worse 
bptest(hg3)  # p > 0.05 = no heteroskedasticity 

# box-cox 
bc <- boxcox(hg1)
(lambda <- bc$x[which.max(bc$y)])  # extracting exact lambda for transformation 

hg4 <- lm(((height_site^lambda-1)/lambda) ~ grazing_m, data = richness_site2)
plot(hg4)
shapiro.test(resid(hg4))   # p < 0.05 = not normal, nothing works 
bptest(hg4)  # p > 0.05 = no heteroskedasticity 


## GLM    
hg_glm <- glm(height_site ~ grazing_m, family = Gamma(link = log), data = richness_site2)
hist(resid(hg_glm))  # without 'Gamma' distribution it wasn't super normal 


### Models with aspect 
hga1 <- lm(height_site ~ grazing_m + aspect, data = richness_site2)
plot(hga1)
shapiro.test(resid(hga1))   # p < 0.05 = not normal 
bptest(hga1)  # p < 0.05 = and heteroskedasticity 

hga_glm <- glm(height_site ~ grazing_m + aspect, family = Gamma(link = log), data = richness_site2)
hist(resid(hga_glm))

hga_glm2 <- glm(height_site ~ grazing_m*aspect, family = Gamma(link = log), data = richness_site2)
hist(resid(hga_glm2))

AIC(hg_glm, hga_glm, hga_glm2)  # 'hga_glm2' is best


### Models with GIS variables (+ aspect)
## NDVI
hgan <- glm(height_site ~ grazing_m + aspect + ndvi, family = Gamma(link = log), data = richness_site2)
hgan2 <- glm(height_site ~ grazing_m + aspect*ndvi, family = Gamma(link = log), data = richness_site2)
hgan3 <- glm(height_site ~ grazing_m*aspect + ndvi, family = Gamma(link = log), data = richness_site2)
hgan4 <- glm(height_site ~ grazing_m*aspect + ndvi*aspect, family = Gamma(link = log), 
             data = richness_site2)
hgan5 <- glm(height_site ~ grazing_m*aspect*ndvi, family = Gamma(link = log), data = richness_site2)

AIC(hga_glm2, hgan, hgan2, hgan3, hgan4, hgan5)  # 'hgan3' is best 

## Slope
hgas <- glm(height_site ~ grazing_m + aspect + slope_deg, family = Gamma(link = log), 
            data = richness_site2)
hgas2 <- glm(height_site ~ grazing_m + aspect*slope_deg, family = Gamma(link = log), 
             data = richness_site2)
hgas3 <- glm(height_site ~ grazing_m*aspect + slope_deg, family = Gamma(link = log), 
             data = richness_site2)
hgas4 <- glm(height_site ~ grazing_m*aspect + slope_deg*aspect, family = Gamma(link = log), 
             data = richness_site2)
hgas5 <- glm(height_site ~ grazing_m*aspect*slope_deg, family = Gamma(link = log), 
             data = richness_site2)

AIC(hga_glm2, hgan3, hgas, hgas2, hgas3, hgas4, hgas5)  # 'hgan3' is still best 

## Wetness
hgaw <- glm(height_site ~ grazing_m + aspect + wetness, family = Gamma(link = log), 
            data = richness_site2)
hgaw2 <- glm(height_site ~ grazing_m + aspect*wetness, family = Gamma(link = log), 
             data = richness_site2)
hgaw3 <- glm(height_site ~ grazing_m*aspect + wetness, family = Gamma(link = log), 
             data = richness_site2)
hgaw4 <- glm(height_site ~ grazing_m*aspect + wetness*aspect, family = Gamma(link = log), 
             data = richness_site2)
hgaw5 <- glm(height_site ~ grazing_m*aspect*wetness, family = Gamma(link = log), 
             data = richness_site2)

AIC(hga_glm2, hgan3, hgaw, hgaw2, hgaw3, hgaw4, hgaw5)  # 'hgan3' is still best 


## NDVI + 1 other
hgans <- glm(height_site ~ grazing_m + aspect + ndvi + slope_deg, family = Gamma(link = log), 
             data = richness_site2)
hganw <- glm(height_site ~ grazing_m + aspect + ndvi + wetness, family = Gamma(link = log), 
             data = richness_site2)

AIC(hgan3, hgans, hganw)  # 'hganw' = best 


## Choosing the best model 
h_null <- glm(height_site ~ 1, family = Gamma(link = log), data = richness_site2)

AIC(hgan3, hganw, h_null)   # 'hganw' is best


## Height Model Results ## ----
# height_site ~ grazing_m * aspect + ndvi + wetness, family = Gamma(link = log)

summary(hganw)        ## USE THIS MODEL'S RESULTS ##
# grazing NORTH: 2.7^(-0.007511) ± 2.7^(0.004852), p = 0.124 (not significant)
# grazing*aspect SOUTH: 2.7^(0.016494) ± 2.7^(0.007022), p = 0.0200 (SIGNIFICANT)
# ndvi: 2.7^(5.541403) ± 2.7^(0.549923), p = < 2e-16 (SIGNIFICANT)
# wetness: 2.7^(-0.009523) ± 2.7^(0.001767), p = 2.46e-07 (SIGNIFICANT)

# calculating McFadden's R2 
with(summary(hganw2), 1 - deviance/null.deviance)  # R2 = 0.5331 (a good model)

# checking residuals 
plot(residuals(hganwsd14) ~ predict(hganwsd14, type = "response")) 


# testing if any "outliers" effect the relationships
test <- richness_site %>% filter(wetness < 50)
ggplot(test, aes(x = wetness, y = height_site)) +  # height + aspect
  geom_point(aes(color = aspect)) +
  stat_smooth(method = "lm", aes(color = aspect))

hganwsd14t <- glm(height_site ~ grazing_m*aspect + ndvi*aspect + wetness*aspect + slope_deg*aspect + 
                    soil_depth*aspect, family = Gamma(link = log), data = test)
summary(hganwsd14t)  # now grazing S is significant, NDVI differs between N and S, etc. 


### Coverage ~ Grazing ----
ggplot(coverage_site, aes(x = coverage_perc)) +
  geom_histogram() +
  facet_wrap(~sp_group)   # zero-inflated/ lots of low values 

cg1 <- lm(coverage_perc ~ grazing_m + sp_group, data = coverage_site)
plot(cg1)
hist(resid(cg1))
shapiro.test(resid(cg1))   # p < 0.05 = not normal 
bptest(cg1)  # p < 0.05 = and heteroskedasticity   

cg2 <- lm(coverage_perc ~ grazing_m*sp_group, data = coverage_site)   # int. makes more sense 
plot(cg2)
shapiro.test(resid(cg2))   # p < 0.05 = not normal 
bptest(cg2)  # p < 0.05 = and heteroskedasticity   

## Transformations 
# log
cg3 <- lm(log(coverage_perc) ~ grazing_m*sp_group, data = coverage_site)
plot(cg3)
shapiro.test(resid(cg3))   # p < 0.05 = not normal
bptest(cg3)  # p < 0.05 = and heteroskedasticity 

# sqrt 
cg4 <- lm(sqrt(coverage_perc) ~ grazing_m*sp_group, data = coverage_site)
plot(cg4)
shapiro.test(resid(cg4))   # p > 0.05 = normal
bptest(cg4)  # p < 0.05 = but heteroskedasticity 

# box-cox 
bc2 <- boxcox(cg1)
(lambda2 <- bc2$x[which.max(bc2$y)])  # extracting exact lambda for transformation 

cg5 <- lm(((coverage_perc^lambda2-1)/lambda2) ~ grazing_m*sp_group, data = coverage_site)
plot(cg5)
shapiro.test(resid(cg5))   # p > 0.05 = normal
bptest(cg5)  # p < 0.05 = no heteroskedasticity 
summary(cg5)


## GLM
cg_glm <- glm(coverage_perc ~ grazing_m*sp_group, data = coverage_site)
hist(resid(cg_glm))  # looks pretty 'normal' (can use Gaussian)
summary(cg_glm)     


### Models with aspect
cga1 <- lm(coverage_perc ~ grazing_m*sp_group*aspect, data = coverage_site)
plot(cga1)
shapiro.test(resid(cga1))   # p < 0.05 = not normal, can't use it 
bptest(cga1)  # p < 0.05 = and heteroskedasticity 

cga_glm <- glm(coverage_perc ~ grazing_m + sp_group + aspect, data = coverage_site)
hist(resid(cga_glm))   # gaussian works 

cga_glm2 <- glm(coverage_perc ~ grazing_m*sp_group + aspect, data = coverage_site)
cga_glm3 <- glm(coverage_perc ~ grazing_m*aspect + sp_group, data = coverage_site)
cga_glm4 <- glm(coverage_perc ~ grazing_m + sp_group*aspect, data = coverage_site)
cga_glm5 <- glm(coverage_perc ~ grazing_m*sp_group*aspect, data = coverage_site)

AIC(cg_glm, cga_glm, cga_glm2, cga_glm3, cga_glm4, cga_glm5)   # best is 'cga_glm' (no interaction)


### Models with GIS variables (+ aspect)
# avoiding interactions with sp_group due to the small amount of data 
## NDVI
cgan <- glm(coverage_perc ~ grazing_m + sp_group + aspect + ndvi, data = coverage_site)
can <- glm(coverage_perc ~ grazing_m + sp_group + ndvi, data = coverage_site)
cgn <- glm(coverage_perc ~ grazing_m + aspect + ndvi, data = coverage_site)
cgn2 <- glm(coverage_perc ~ grazing_m + aspect*ndvi, data = coverage_site)
cgn3 <- glm(coverage_perc ~ grazing_m*aspect + ndvi, data = coverage_site)
cgn4 <- glm(coverage_perc ~ grazing_m*aspect + ndvi*aspect, data = coverage_site)
cgan2 <- glm(coverage_perc ~ grazing_m + sp_group + ndvi*aspect, data = coverage_site)
cgan3 <- glm(coverage_perc ~ grazing_m*aspect + sp_group + ndvi*aspect, data = coverage_site)
cgan4 <- glm(coverage_perc ~ grazing_m*aspect + sp_group + ndvi, data = coverage_site)

min(AIC(cga_glm, cgan, cgan2, cgan3, cgan4, can, cgn, cgn2, cgn3, cgn4)[,2])   # lowest == 1361.371 
AIC(cga_glm, cgan, cgan2, cgan3, cgan4, can, cgn, cgn2, cgn3, cgn4)  # 'cga_glm' or 'can' is best 

## Slope
cgas <- glm(coverage_perc ~ grazing_m + sp_group + aspect + slope_deg, data = coverage_site)
cas <- glm(coverage_perc ~ grazing_m + sp_group + slope_deg, data = coverage_site)
cgs <- glm(coverage_perc ~ grazing_m + aspect + slope_deg, data = coverage_site)
cgs2 <- glm(coverage_perc ~ grazing_m + aspect*slope_deg, data = coverage_site)
cgs3 <- glm(coverage_perc ~ grazing_m*aspect + slope_deg, data = coverage_site)
cgs4 <- glm(coverage_perc ~ grazing_m*aspect + slope_deg*aspect, data = coverage_site)
cgan2 <- glm(coverage_perc ~ grazing_m + sp_group + slope_deg*aspect, data = coverage_site)
cgas3 <- glm(coverage_perc ~ grazing_m*aspect + sp_group + slope_deg*aspect, data = coverage_site)
cgas4 <- glm(coverage_perc ~ grazing_m*aspect + sp_group + slope_deg, data = coverage_site)

min(AIC(cga_glm, cgas, cgas2, cgas3, cgas4, cas, cgs, cgs2, cgs3, cgs4)[,2])   # lowest == 1361.371 
AIC(cga_glm, cgas, cgas2, cgas3, cgas4, cas, cgs, cgs2, cgs3, cgs4)  # 'cga_glm' or 'cas' is best 

## Soil depth
cgad <- glm(coverage_perc ~ grazing_m + sp_group + aspect + soil_depth, data = coverage_site)
cad <- glm(coverage_perc ~ grazing_m + sp_group + soil_depth, data = coverage_site)
cgd <- glm(coverage_perc ~ grazing_m + aspect + soil_depth, data = coverage_site)
cgd2 <- glm(coverage_perc ~ grazing_m + aspect*soil_depth, data = coverage_site)
cgd3 <- glm(coverage_perc ~ grazing_m*aspect + soil_depth, data = coverage_site)
cgd4 <- glm(coverage_perc ~ grazing_m*aspect + soil_depth*aspect, data = coverage_site)
cgaw2 <- glm(coverage_perc ~ grazing_m + sp_group + soil_depth*aspect, data = coverage_site)
cgad3 <- glm(coverage_perc ~ grazing_m*aspect + sp_group + soil_depth*aspect, data = coverage_site)
cgad4 <- glm(coverage_perc ~ grazing_m*aspect + sp_group + soil_depth, data = coverage_site)

min(AIC(cga_glm, cgad, cgad2, cgad3, cgad4, cad, cgd, cgd2, cgd3, cgd4)[,2])   # lowest == 1361.371 
AIC(cga_glm, cgad, cgad2, cgad3, cgad4, cad, cgd, cgd2, cgd3, cgd4)  # 'cga_glm' or 'cad' is best

## Wetness
cgaw <- glm(coverage_perc ~ grazing_m + sp_group + aspect + wetness, data = coverage_site)
caw <- glm(coverage_perc ~ grazing_m + sp_group + wetness, data = coverage_site)
cgw <- glm(coverage_perc ~ grazing_m + aspect + wetness, data = coverage_site)
cgw2 <- glm(coverage_perc ~ grazing_m + aspect*wetness, data = coverage_site)
cgw3 <- glm(coverage_perc ~ grazing_m*aspect + wetness, data = coverage_site)
cgw4 <- glm(coverage_perc ~ grazing_m*aspect + wetness*aspect, data = coverage_site)
cgaw2 <- glm(coverage_perc ~ grazing_m + sp_group + wetness*aspect, data = coverage_site)
cgaw3 <- glm(coverage_perc ~ grazing_m*aspect + sp_group + wetness*aspect, data = coverage_site)
cgaw4 <- glm(coverage_perc ~ grazing_m*aspect + sp_group + wetness, data = coverage_site)

min(AIC(cga_glm, cgaw, cgaw2, cgaw3, cgaw4, caw, cgw, cgw2, cgw3, cgw4)[,2])   # lowest == 1361.303 
AIC(cga_glm, cgaw, cgaw2, cgaw3, cgaw4, caw, cgw, cgw2, cgw3, cgw4)  # 'cga_glm' or 'caw' is best 

# comparing to each variable
AIC(cga_glm, can, cas, cad, caw)  # all pretty much identical, none of GIS ones have 'aspect'


## Combination testing 
canw <- glm(coverage_perc ~ grazing_m + sp_group + ndvi + wetness, data = coverage_site)
cans <- glm(coverage_perc ~ grazing_m + sp_group + ndvi + slope_deg, data = coverage_site)
cand <- glm(coverage_perc ~ grazing_m + sp_group + ndvi + soil_depth, data = coverage_site)

canws <- glm(coverage_perc ~ grazing_m + sp_group + ndvi + wetness + slope_deg, data = coverage_site)
canwd <- glm(coverage_perc ~ grazing_m + sp_group + ndvi + wetness + soil_depth, data = coverage_site)

canwsd <- glm(coverage_perc ~ grazing_m + sp_group + ndvi + wetness + slope_deg + 
                soil_depth, data = coverage_site)

AIC(cga_glm, canw, cans, cand, canws, canwd, canwsd)  # 'cga_glm' is best, GIS var don't add  


## Choosing the best model
c_null <- glm(coverage_perc ~ 1, data = coverage_site)
AIC(cga_glm, c_null)

## Coverage Model Results ## ----
# glm(coverage_perc ~ grazing_m + sp_group + aspect)

summary(cga_glm)   ## USE THIS MODEL'S RESULTS ##
# 6.75333 ± 2.9027
# grazing: -0.01039 ± 0.1297, p = 0.9363    
# herb: -5.14391 ± 3.42615, p = 0.1352    
# lichen: 16.96416 ± 3.37309, p = 1.31e-06 (SIGNIFICANT)
# moss: 4.7241 ± 3.39892, p = 0.1665    
# shrub: 50.31285 ± 3.37309, p = < 2e-16 (SIGNIFICANT)
# aspect SOUTH: 0.16916 ± 2.15598, p = 0.9376    

# calculating McFadden's R2 
with(summary(cga_glm), 1 - deviance/null.deviance)  # R2 = 0.6819 (very good model)

# checking residuals 
plot(residuals(cga_glm) ~ predict(cga_glm, type = "response"))  # a pattern, but due to sp. groups



#### Basic Statistics ----
### Height ~ Aspect
ha1 <- lm(height_site ~ aspect, data = richness_site)
shapiro.test(resid(ha1))  # not normal 

wilcox.test(height_site ~ aspect, data = richness_site)
# W = 3233, p = 0.3507 (no difference in height between N and S)

# average height S vs. N
richness_site %>% group_by(aspect) %>% summarise(height = mean(height_site),
                                                 se = std.error(height_site))
# N = 3.39cm ± 0.168
# S = 4.27cm ± 0.286


### Coverage ~ Sp Group + Aspect
cov_lm <- lm(coverage_perc ~ sp_group, data = coverage_site)
shapiro.test(resid(cov_lm))  # not normal

# coverage ~ group (overall)
kruskal.test(coverage_perc ~ sp_group, data = coverage_site)
# chi-squared = 117.78, df = 4, p = < 2.2e-16 (SIGNIFICANT)

dunn.test::dunn.test(coverage_site$coverage_perc, coverage_site$sp_group, list = F, altp = T)
# diff = COLUMN minus ROW 
# grass-herb: diff = 2.8212, p = 0.0048 (SIGNIFICANT)
# grass-lichen: diff = -3.9275, p = 0.0001 (SIGNIFICANT)
# grass-moss: diff = -2.1323, p = 0.0330 (SIGNIFICANT)
# grass-shrub: diff = -7.3885, p = 0.0000 (SIGNIFICANT)
# herb-lichen: diff = -6.6886, p = 0.0000 (SIGNIFICANT)
# herb-moss: diff = -4.90089, p = 0.0000 (SIGNIFICANT)
# herb-shrub: diff = -10.0968, p = 0.0000 (SIGNIFICANT)
# lichen-moss: diff = 1.76574, p = 0.0774 (not significant)
# lichen-shrub: diff = -3.461, p = 0.0005 (SIGNIFICANT)
# moss-shrub: diff = -5.2008, p = 0.0000 (SIGNIFICANT)

# coverage ~ group (NORTH)
coverageN <- coverage_site %>% filter(aspect == "N")
kruskal.test(coverage_perc ~ sp_group, data = coverageN)
# chi-squared = 61.961, df = 4, p = 1.122e-12 (SIGNIFICANT)

dunn.test::dunn.test(coverageN$coverage_perc, coverageN$sp_group, list = F, altp = T)
# diff = COLUMN minus ROW 
# grass-herb: diff = 2.140144, p = 0.0323 (SIGNIFICANT)
# grass-lichen: diff = -2.682129, p = 0.0073 (SIGNIFICANT)
# herb-lichen: diff = -4.822274, p = 0.0000 (SIGNIFICANT)
# grass-moss: diff = -2.022020, p = 0.0432 (SIGNIFICANT)
# herb-moss: diff = -4.162165, p = 0.0000 (SIGNIFICANT)
# lichen-moss: diff = 0.660109, p = 0.5092 (not significant)
# grass-shrub: diff = -5.218340, p = 0.0000 (SIGNIFICANT)
# herb-shrub: diff = -7.358485, p = 0.0000 (SIGNIFICANT)
# lichen-shrub: diff = -2.536210, p = 0.0112 (SIGNIFICANT)
# moss-shrub: diff = -3.196320, p = 0.0014 (SIGNIFICANT)

# coverage ~ group (SOUTH)
coverageS <- coverage_site %>% filter(aspect == "S")
kruskal.test(coverage_perc ~ sp_group, data = coverageS)
# chi-squared = 55.836, df = 4, p = 2.17e-11 (SIGNIFICANT)

dunn.test::dunn.test(coverageS$coverage_perc, coverageS$sp_group, list = F, altp = T)
# diff = COLUMN minus ROW 
# grass-herb: diff = 1.861202, p = 0.0627 (not significant)
# grass-lichen: diff = -2.822872, p = 0.0048 (SIGNIFICANT)
# herb-lichen: diff = -4.594437, p = 0.0000 (SIGNIFICANT)
# grass-moss: diff = -1.003856, p = 0.3154 (not significant)
# herb-moss: diff = -2.807424, p = 0.0050 (SIGNIFICANT)
# lichen-moss: diff = 1.775916, p = 0.0757 (not significant)
# grass-shrub: diff = -5.156062, p = 0.0000 (SIGNIFICANT)
# herb-shrub: diff = -6.853539, p = 0.0000 (SIGNIFICANT)
# lichen-shrub: diff = -2.333190, p = 0.0196 (SIGNIFICANT)
# moss-shrub: diff = -4.073483, p = 0.0000 (SIGNIFICANT)


# shrub ~ aspect
s <- coverage_site %>% filter(sp_group == "shrub")
wilcox.test(coverage_perc ~ aspect, data = s)
# W = 116, p = 0.3394

# lichen ~ aspect
l <- coverage_site %>% filter(sp_group == "lichen")
wilcox.test(coverage_perc ~ aspect, data = l)
# W = 139, p = 0.8651

# moss ~ aspect
m <- coverage_site %>% filter(sp_group == "moss")
wilcox.test(coverage_perc ~ aspect, data = m)
# W = 199, p = 0.02291 (SIGNIFICANT)

# grass ~ aspect
g <- coverage_site %>% filter(sp_group == "grass")
wilcox.test(coverage_perc ~ aspect, data = g)
# W = 159, p = 0.6339

# herb
h <- coverage_site %>% filter(sp_group == "herb")
wilcox.test(coverage_perc ~ aspect, data = h)
# W = 154, p = 0.331 

#### NMDS (Coverage by Group) ----
### Making a matrix
cov_matrix <- coverage_site %>% 
  dplyr::select(site_nr, aspect, grazing_cat, sp_group, coverage_perc) %>% 
  pivot_wider(names_from = "sp_group", values_from = "coverage_perc") 

cov_matrix[is.na(cov_matrix)] <- 0  # making sure non-existent groups just have coverage of 0

coveragem <- coverage %>% 
  dplyr::select(plot_nr, aspect, grazing_cat, sp_group, coverage_perc)
coveragem <- coveragem %>% pivot_wider(names_from = "sp_group", values_from = "coverage_perc")

# making a matrix with only coverage data 
cov_matrix_sp <- cov_matrix %>% dplyr::select(!c(site_nr, aspect, grazing_cat)) 

### Looking at how many axes to extract 
mds1 <- metaMDS(cov_matrix_sp, distance = "bray", k = 1)
mds2 <- metaMDS(cov_matrix_sp, distance = "bray", k = 2)
mds3 <- metaMDS(cov_matrix_sp, distance = "bray", k = 3)
mds4 <- metaMDS(cov_matrix_sp, distance = "bray", k = 4)
mds5 <- metaMDS(cov_matrix_sp, distance = "bray", k = 5)
mds6 <- metaMDS(cov_matrix_sp, distance = "bray", k = 6)

screeC <- cbind(rbind(1,2,3,4,5,6),rbind(mds1$stress, mds2$stress, mds3$stress, mds4$stress,
                                         mds5$stress, mds6$stress))
plot(screeC)
# 3 or 4 axes seem reasonable

nmds <- metaMDS(cov_matrix_sp, distance = "bray", k = 3, autotransform = TRUE, trymax = 500, 
                sratmax = 0.999)  
nmds

nmds2 <- metaMDS(cov_matrix_sp, distance = "bray", k = 4, autotransform = TRUE, trymax = 500, 
                 sratmax = 0.999)  
nmds2

### Plotting the NMDS 
# extracting NMDS scores (x and y coordinates)
data.scores <- as.data.frame(scores(nmds))
species_scores <- as.data.frame(scores(nmds, "species"))
species_scores$species <- rownames(species_scores)

data.scores$grazing_cat <- cov_matrix$grazing_cat
data.scores$aspect <- cov_matrix$aspect

# function for ellipses
veganCovEllipse <- function (cov, center = c(0, 0), scale = 1, npoints = 100) 
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}  # run from here

# theme for plots 
theme_thesis <- theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title.x = 
          element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = 
          element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))


## Plotting by aspect
NMDSa <- data.frame(MDS1 = nmds$points[,1], MDS2 = nmds$points[,2], 
                    group = data.scores$aspect)

NMDSa$group <- as.factor(NMDSa$group)
NMDSa_mean <- aggregate(NMDSa[,1:2], list(group = NMDSa$group), "mean")

# making the ellipsoids
ord <- ordiellipse(nmds, data.scores$aspect, label = T, conf = 0.95)

df_ell <- data.frame()   # run from here (this side)

for(g in levels(NMDSa$group)){
  df_ell <- rbind(df_ell, 
                  cbind(as.data.frame(with(NMDSa[NMDSa$group==g,],
                                           veganCovEllipse(ord[[g]]$cov, ord[[g]]$center,
                                                           ord[[g]]$scale)))
                        ,group=g))
}  # run from here 


# making a theme
theme_thesis <- theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title.x = 
          element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = 
          element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))

# plot
(nmds_plot <- ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
    geom_polygon(data = df_ell, aes(x = NMDS1, y = NMDS2, group = group,
                                    color = group, fill = group), alpha = 0.2, 
                 size = 0.5, linetype = 1) +
    geom_point(aes(color = aspect, shape = aspect, fill = aspect), 
               size = 2, alpha = 0.6) +
    geom_text(data = species_scores, aes(x = NMDS1, y = NMDS2, label = species),
              alpha = 0.5, size = 2) +
    theme_thesis + 
    labs(x = "NMDS1", y = "NMDS2") +
    scale_color_manual(values = c("#458264", "#F4A460"),
                       labels = c("N", "S")) +
    scale_shape_manual(values = c(21, 22),
                       labels = c("N", "S")) +
    scale_fill_manual(values = c("#458264", "#F4A460"),
                      labels = c("N", "S")))

## Plotting by grazing category
NMDSg <- data.frame(MDS1 = nmds$points[,1], MDS2 = nmds$points[,2], 
                    group = data.scores$grazing_cat)

NMDSg$group <- as.factor(NMDSg$group)
NMDSg_mean <- aggregate(NMDSg[,1:2], list(group = NMDSg$group), "mean")

# making the ellipsoids
ord2 <- ordiellipse(nmds, data.scores$grazing_cat, label = T, conf = 0.95)

df_ell2 <- data.frame()   # run from here (this side)

for(g in levels(NMDSg$group)){
  df_ell2 <- rbind(df_ell2, 
                   cbind(as.data.frame(with(NMDSg[NMDSg$group==g,],
                                            veganCovEllipse(ord2[[g]]$cov, ord2[[g]]$center,
                                                            ord2[[g]]$scale)))
                         ,group=g))
}  # run from here 


# plot
(nmds_plot2 <- ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
    geom_polygon(data = df_ell2, aes(x = NMDS1, y = NMDS2, group = group,
                                     color = group, fill = group), alpha = 0.2, 
                 size = 0.5, linetype = 1) +
    geom_point(aes(color = grazing_cat, shape = grazing_cat, fill = grazing_cat), 
               size = 2, alpha = 0.6) +
    geom_text(data = species_scores, aes(x = NMDS1, y = NMDS2, label = species),
              alpha = 0.5, size = 2) +
    theme_thesis + 
    labs(x = "NMDS1", y = "NMDS2"))

## For sp group ~ aspect 
ano_a <- anosim(cov_matrix_sp, cov_matrix$aspect, distance = "bray", permutations = 9999)
ano_a
# not significantly different (R = 0.01435, p = 0.2577)

## For sp group ~ grazing category
ano_g <- anosim(cov_matrix_sp, cov_matrix$grazing_cat, distance = "bray", permutations = 9999)
ano_g
# not significantly different (R = -0.08811, p = 0.9864) 


### NMDS ~ Site ----
## Dividing coverage across species
sp_cov <- left_join(plots, coverage)
sp_cov <- sp_cov %>% 
  dplyr::select(site_nr, plot_nr, aspect, sp_latin, sp_group, coverage_perc) %>% 
  group_by(site_nr, plot_nr, sp_group) %>% 
  mutate(rel_abund = coverage_perc/length(sp_group)) %>% 
  ungroup() %>% 
  na.omit() %>% 
  group_by(site_nr, sp_latin, aspect) %>% 
  summarize(rel_abund2 = sum(rel_abund)) %>% 
  mutate(rel_abund2 = ifelse(site_nr == "5", rel_abund2/2, rel_abund2/3)) %>% 
  ungroup()

print(sp_cov %>% group_by(site_nr) %>% summarise(sum(rel_abund2)), n = 105)  # all = 100%! 

## Making a matrix
sp_matrix <- sp_cov %>% 
  distinct() %>% 
  pivot_wider(names_from = "sp_latin", values_from = "rel_abund2", 
              values_fill = NA)  # get a warning, but no duplicates found

sp_matrix[is.na(sp_matrix)] <- 0  # making sure non-existent sp just have coverage of 0

# with only coverage data 
sp_matrix2 <- sp_matrix %>% 
  dplyr::select(!c(site_nr, aspect)) 

## Looking at how many axes to extract 
set.seed(9)
mds1 <- metaMDS(sp_matrix2, distance = "bray", k = 1, sratmax = 0.99999,    # no convergence
                autotransform = F)   
mds2 <- metaMDS(sp_matrix2, distance = "bray", k = 2, sratmax = 0.99999,    # no convergence
                autotransform = F)   
mds3 <- metaMDS(sp_matrix2, distance = "bray", k = 3, sratmax = 0.99999,    # solution found
                autotransform = F) 
mds4 <- metaMDS(sp_matrix2, distance = "bray", k = 4, sratmax = 0.99999,    # solution found 
                autotransform = F)
# stress = 0.1256
mds5 <- metaMDS(sp_matrix2, distance = "bray", k = 5, sratmax = 0.99999,    # solution found?
                autotransform = F)    
# stress = 0.0976
mds6 <- metaMDS(sp_matrix2, distance = "bray", k = 6, sratmax = 0.99999,    # solution found 
                autotransform = F) 
# stress = 0.0798
mds7 <- metaMDS(sp_matrix2, distance = "bray", k = 7, sratmax = 0.99999,    # solution found 
                autotransform = F) 
# stress = 0.0654
mds8 <- metaMDS(sp_matrix2, distance = "bray", k = 8, sratmax = 0.99999,    # solution found
                autotransform = F) 
# stress = 0.0566
mds9 <- metaMDS(sp_matrix2, distance = "bray", k = 9, sratmax = 0.99999,    # solution found? 
                autotransform = F) 
mds10 <- metaMDS(sp_matrix2, distance = "bray", k = 10, sratmax = 0.99999,    # no convergence?
                 autotransform = F)  

scree <- cbind(rbind(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 
               rbind(mds1$stress, mds2$stress, mds3$stress, mds4$stress, mds5$stress, 
                     mds6$stress, mds7$stress, mds8$stress, mds9$stress, mds10$stress))
plot(scree)
# ~5 or more dimensions = <0.1 stress (which is good)

stressplot(mds5)   # non-metric fit = 0.996, linear fit = 0.957
stressplot(mds6)   # non-metric fit = 0.997, linear fit = 0.972
stressplot(mds7)   # non-metric fit = 0.998, linear fit = 0.98
stressplot(mds8)   # non-metric fit = 0.999, linear fit = 0.986
stressplot(mds9)   # non-metric fit = 0.999, linear fit = 0.99

# 6 dimensions improves the linear fit quite a bit, then afterwards it tapers off 

# choosing k = 6

set.seed(6)
nmds6 <- metaMDS(sp_matrix2, distance = "bray", k = 6, autotransform = F, trymax = 500, 
                 sratmax = 0.99999)  
# note that autotransformation is off (default = T) 

nmds6
# stress = 0.05118
# 6 dimensions 



### NMDS with GIS matrix ----
## Not standardized 
# making ndvi not negative
gisvar2b <- gisvar2 %>% 
  mutate(ndvi = ifelse(ndvi < 0, ndvi + 0.025, ndvi)) %>% 
  dplyr::select(!(plot_nr))

# NMDS 
set.seed(9)
mds1gis <- metaMDS(gisvar2b, distance = "bray", k = 1, sratmax = 0.99999,    # no convergence
                   autotransform = F)   
mds2gis <- metaMDS(gisvar2b, distance = "bray", k = 2, sratmax = 0.99999,    # solution found 
                   autotransform = F)   
# stress = 0.085912
mds3gis <- metaMDS(gisvar2b, distance = "bray", k = 3, sratmax = 0.99999,    # solution found 
                   autotransform = F) 
# stress = 0.061489
mds4gis <- metaMDS(gisvar2b, distance = "bray", k = 4, sratmax = 0.99999,    # solution found 
                   autotransform = F)
# stress = 0.04273
mds5gis <- metaMDS(gisvar2b, distance = "bray", k = 5, sratmax = 0.99999,    # solution found 
                   autotransform = F)   
# stress = 0.0337
mds6gis <- metaMDS(gisvar2b, distance = "bray", k = 6, sratmax = 0.99999,    # no convergence
                   autotransform = F) 
mds7gis <- metaMDS(gisvar2b, distance = "bray", k = 7, sratmax = 0.99999,    # no convergence 
                   autotransform = F) 
mds8gis <- metaMDS(gisvar2b, distance = "bray", k = 8, sratmax = 0.99999,    # no convergence
                   autotransform = F) 
mds9gis <- metaMDS(gisvar2b, distance = "bray", k = 9, sratmax = 0.99999,    # no convergence
                   autotransform = F) 
mds10gis <- metaMDS(gisvar2b, distance = "bray", k = 10, sratmax = 0.99999,    # no convergence
                    autotransform = F) 

scree2 <- cbind(rbind(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 
                rbind(mds1gis$stress, mds2gis$stress, mds3gis$stress, mds4gis$stress, mds5gis$stress, 
                      mds6gis$stress, mds7gis$stress, mds8gis$stress, mds9gis$stress, mds10gis$stress))
plot(scree2)
# ~2 or more dimensions = <0.1 stress (which is good)

stressplot(mds2gis)   # non-metric fit = 0.993, linear fit = 0.972
stressplot(mds3gis)   # non-metric fit = 0.996, linear fit = 0.984
stressplot(mds4gis)   # non-metric fit = 0.998, linear fit = 0.991
stressplot(mds5gis)   # non-metric fit = 0.999, linear fit = 0.994
# k = 4 is best 

set.seed(4)
nmds4 <- metaMDS(gisvar2b, distance = "bray", k = 4, autotransform = F, trymax = 500, 
                 sratmax = 0.99999)  
# note that autotransformation is off (default = T) 

nmds4
# stress = 0.041999
# 4 dimensions 


## Standardized variables
