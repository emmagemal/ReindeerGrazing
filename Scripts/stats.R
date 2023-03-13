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
                   grazing_cat = substr(site, 1, 1))   # adding aspect
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
                                     sp_latin))

plots[(plots$sp_latin == ""), ]   # no latin name because they are "unknown"
plots <- plots %>%  
            mutate(sp_latin = ifelse(sp_latin == "", "unknown", sp_latin))

plots %>% group_by(plot_nr, sp_latin) %>% summarise(x = length(sp_latin)) %>% filter(x > 1)
 # no duplicate names 


# . ----
#### Variable Creation ----
### Species richness
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
              mutate(rich_group_prop = (richness_group/sum(richness_group))) %>% 
              ungroup() %>%
              group_by(site_nr) %>% 
              mutate(richness_siteT = mean(richness_plot)) %>%  # true richness per site (with reps)
              ungroup() %>% 
              group_by(sp_group, site_nr) %>% 
              mutate(rich_propT = mean(rich_group_prop))

# creating a new categorical grazing variable with 3 categories 
summary(sites)
  # min grazing = 16.42s
  # max grazing = 1774.98s

sites <- sites %>% 
            mutate(grazing_cat2 = case_when(grazing_s < 300 ~ "1",     # low
                                            grazing_s >= 300 & grazing_s < 800 ~ "2",
                                            grazing_s >= 800 ~ "3"))   # high

sites %>% group_by(grazing_cat2) %>%  summarize(length(grazing_cat2))
  # made it as even as possible without making it too 'incorrect' for what is L, M, and H


### Combining dataframes ----
## Combining 'richness' with 'sites' to add grazing data 
grazing <- sites %>% dplyr::select(site_nr, plot_nr, plot, aspect, grazing_s, grazing_cat2)
grazing <- grazing %>% mutate(grazing_m = grazing_s/60)
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
                              grazing_s = mean(grazing_s),
                              grazing_m = mean(grazing_m))
coverage_site <- as.data.frame(coverage_site)
str(coverage_site)

richness_site <- richness %>% 
                    group_by(site_nr) %>% 
                    mutate(ndvi = mean(ndvi),
                           slope_deg = mean(slope_deg),
                           wetness = mean(wetness),
                           soil_depth = mean(soil_depth)) %>% 
                    group_by(site_nr, sp_group, aspect) %>% 
                    summarise(richness_siteT = mean(richness_siteT),
                              height_site = mean(height_site),
                              richness_group = mean(richness_group),
                              rich_propT = mean(rich_propT),
                              ndvi = mean(ndvi),
                              slope_deg = mean(slope_deg),
                              wetness = mean(wetness),
                              soil_depth = mean(soil_depth),
                              grazing_s = mean(grazing_s),
                              grazing_m = mean(grazing_m))

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
  geom_point(aes(color = sp_group)) +
  stat_smooth(method = "lm", aes(color = sp_group, fill = sp_group)) +   
  facet_wrap(~aspect)

ggplot(richness, aes(x = aspect, y = rich_propT)) +   # relative species nr for each group 
  geom_boxplot(aes(fill = sp_group))                       # for N vs. S 

ggplot(richness_site, aes(x = aspect, y = richness_siteT)) +   # averaged richness N vs. S
  geom_boxplot()

## Coverage
ggplot(coverage_site, aes(x = grazing_m, y = coverage_perc)) +  # coverage of groups with grazing 
  geom_point(aes(color = sp_group)) +
  stat_smooth(method = "lm", se = T, aes(color = sp_group, fill = sp_group), alpha = 0.3)

# use this one for final
ggplot(coverage_site, aes(x = grazing_m, y = coverage_perc)) +  # differences for N vs. S
  geom_point(aes(color = sp_group)) +
  stat_smooth(method = "lm", se = T, aes(color = sp_group, fill = sp_group)) +
  facet_wrap(~aspect)
 
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
## NDVI ----
ggplot(richness_site, aes(y = ndvi, x = aspect)) +  # ndvi by aspect 
  geom_boxplot()

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
ggplot(richness_site, aes(x = grazing_m, y = richness_siteT)) +   # richness + ndvi + aspect
  geom_point(aes(color = ndvi)) +
  facet_wrap(~aspect)

ggplot(richness_site, aes(x = grazing_m, y = rich_propT)) +   # rich prop + ndvi + aspect + sp group
  geom_point(aes(color = ndvi)) +
  facet_wrap(~aspect + sp_group)

ggplot(richness_site, aes(x = grazing_m, y = height_site)) +   # height + ndvi + aspect
  geom_point(aes(color = ndvi)) +
  facet_wrap(~aspect)

ggplot(coverage_site, aes(x = grazing_m, y = coverage_perc)) +   # coverage + ndvi + aspect
  geom_point(aes(color = ndvi)) +
  facet_wrap(~aspect)

ggplot(coverage_site, aes(x = grazing_m, y = coverage_perc)) +   # coverage + ndvi + aspect + sp group
  geom_point(aes(color = ndvi)) +
  facet_wrap(~aspect + sp_group)

ggplot(richness_site, aes(x = grazing_m, y = ndvi)) +   # grazing vs. ndvi
  geom_point(aes(color = aspect)) +
  stat_smooth(method = "lm", aes(color = aspect))

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

ggplot(richness_site, aes(x = grazing_m, y = slope_deg)) +   # grazing vs. slope 
  geom_point(aes(color = aspect)) +
  stat_smooth(method = "lm", aes(color = aspect))

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
ggplot(richness_site, aes(x = grazing_m, y = richness_siteT)) +   # richness + wetness + aspect
  geom_point(aes(wetness)) +
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

ggplot(richness_site, aes(x = grazing_m, y = wetness)) +   # grazing vs. wetness 
  geom_point(aes(color = aspect)) +
  stat_smooth(method = "lm", aes(color = aspect))

# . ----
#### Models ----
ggcorr(richness_site, label = T)  # nothing I have to exclude 

### Richness ~ Grazing (Site) ----
# checking assumptions
hist(richness_site$richness_siteT)  # maybe bimodal  
hist(richness_site$grazing_m)

rg1 <- lm(richness_siteT ~ grazing_m, data = richness_site)
plot(rg1)
shapiro.test(resid(rg1))   # p < 0.05 = not normal
bptest(rg1)  # p > 0.05 = no heteroskedasticity though 

## Transformations 
# log
rg1_log <- lm(log(richness_siteT) ~ grazing_m, data = richness_site)
shapiro.test(resid(rg1_log))   # p < 0.05 = not normal
bptest(rg1_log)  # p > 0.05 = no heteroskedasticity  

# sqrt
rg1_sqrt <- lm(sqrt(richness_siteT) ~ grazing_m, data = richness_site)
shapiro.test(resid(rg1_sqrt))   # p < 0.05 = not normal
bptest(rg1_sqrt)  # p > 0.05 = no heteroskedasticity  


## GLM 
rg_glm <- glm(richness_siteT ~ grazing_m, data = richness_site)
hist(resid(rg_glm), breaks = 10)  # gaussian seems ok, maybe bimodal 
summary(rg_glm)


### Models with aspect 
rga1 <- lm(richness_siteT ~ grazing_m + aspect, data = richness_site)
plot(rga1)
shapiro.test(resid(rga1))   # p < 0.05 = not normal
bptest(rga1)  # p > 0.05 = no heteroskedasticity though

rga_glm <- glm(richness_siteT ~ grazing_m + aspect, data = richness_site)
rga_glm2 <- glm(richness_siteT ~ grazing_m * aspect, data = richness_site)

AIC(rga_glm, rga_glm2)  # best is 'rga_glm'


### Models with aspect + GIS variables
## NDVI
rgan_glm <- glm(richness_siteT ~ grazing_m + aspect + ndvi, data = richness_site)
rgan_glm2 <- glm(richness_siteT ~ grazing_m * aspect + ndvi, data = richness_site)
rgan_glm3 <- glm(richness_siteT ~ grazing_m + aspect * ndvi, data = richness_site)
rgan_glm4 <- glm(richness_siteT ~ grazing_m * ndvi + aspect, data = richness_site)
rgan_glm5 <- glm(richness_siteT ~ grazing_m * aspect * ndvi, data = richness_site)

AIC(rga_glm, rgan_glm, rgan_glm2, rgan_glm3, rgan_glm4, rgan_glm5)  # 'rgan_glm3' is best 

## Slope
rgas_glm <- glm(richness_siteT ~ grazing_m + aspect + slope_deg, data = richness_site)
rgas_glm2 <- glm(richness_siteT ~ grazing_m * aspect + slope_deg, data = richness_site)
rgas_glm3 <- glm(richness_siteT ~ grazing_m + aspect * slope_deg, data = richness_site)
rgas_glm4 <- glm(richness_siteT ~ grazing_m * slope_deg + aspect, data = richness_site)
rgas_glm5 <- glm(richness_siteT ~ grazing_m * aspect * slope_deg, data = richness_site)

AIC(rga_glm, rgas_glm, rgas_glm2, rgas_glm3, rgas_glm4, rgas_glm5)  # 'rgas_glm5' is best 

## Soil depth
rgad_glm <- glm(richness_siteT ~ grazing_m + aspect + soil_depth, data = richness_site)
rgad_glm2 <- glm(richness_siteT ~ grazing_m * aspect + soil_depth, data = richness_site)
rgad_glm3 <- glm(richness_siteT ~ grazing_m + aspect * soil_depth, data = richness_site)
rgad_glm4 <- glm(richness_siteT ~ grazing_m * soil_depth + aspect, data = richness_site)
rgad_glm5 <- glm(richness_siteT ~ grazing_m * aspect * soil_depth, data = richness_site)

AIC(rga_glm, rgad_glm, rgad_glm2, rgad_glm3, rgad_glm4, rgad_glm5)  # 'rgad_glm4' is best 

## Wetness
rgaw_glm <- glm(richness_siteT ~ grazing_m + aspect + wetness, data = richness_site)
rgaw_glm2 <- glm(richness_siteT ~ grazing_m * aspect + wetness, data = richness_site)
rgaw_glm3 <- glm(richness_siteT ~ grazing_m + aspect * wetness, data = richness_site)
rgaw_glm4 <- glm(richness_siteT ~ grazing_m * wetness + aspect, data = richness_site)
rgaw_glm5 <- glm(richness_siteT ~ grazing_m * aspect * wetness, data = richness_site)

AIC(rga_glm, rgaw_glm, rgaw_glm2, rgaw_glm3, rgaw_glm4, rgaw_glm5)  # 'rgaw_glm3' is best 

# comparing to each variable
AIC(rga_glm, rgan_glm3, rgas_glm5, rgad_glm4, rgaw_glm3)  # 'rgaw_glm3' (wetness) is best 

## Wetness + 1 other
rgawn_glm <- glm(richness_siteT ~ grazing_m + aspect + wetness + ndvi, data = richness_site)
rgaws_glm <- glm(richness_siteT ~ grazing_m + aspect + wetness + slope_deg, data = richness_site)
rgawd_glm <- glm(richness_siteT ~ grazing_m + aspect + wetness + soil_depth, data = richness_site)

AIC(rga_glm, rgaw_glm, rgaw_glm3, rgawn_glm, rgaws_glm, rgawd_glm)  # 'rgawn_glm' = best 

## Wetness + NDVI + 1 other
rgawns_glm <- glm(richness_siteT ~ grazing_m + aspect + wetness + ndvi + slope_deg, 
                 data = richness_site)
rgawnd_glm <- glm(richness_siteT ~ grazing_m + aspect + wetness + ndvi + soil_depth, 
                 data = richness_site)

AIC(rga_glm, rgaw_glm, rgaw_glm3, rgawn_glm, rgawns_glm, rgawnd_glm)
  # 'rgawns_glm' is best

## Wetness + NDVI + slope + depth 
rgawnsd_glm <- glm(richness_siteT ~ grazing_m + aspect + wetness + ndvi + slope_deg + soil_depth, 
                  data = richness_site)

AIC(rga_glm, rgaw_glm3, rgan_glm3, rgas_glm5, rgad_glm4,rgawn_glm, rgawns_glm, rgawnsd_glm)  
# 'rgawnsd_glm' is best 

## Interaction possibilities  
rgawn_glm2 <- glm(richness_siteT ~ grazing_m * aspect + wetness + ndvi, data = richness_site)
rgawn_glm3 <- glm(richness_siteT ~ grazing_m*aspect + wetness*aspect + ndvi, data = richness_site)
rgawn_glm4 <- glm(richness_siteT ~ grazing_m*aspect + wetness*aspect + ndvi*aspect, 
                  data = richness_site)

rgawns_glm2 <- glm(richness_siteT ~ grazing_m * aspect + wetness + ndvi + slope_deg, 
                   data = richness_site)
rgawnsd_glm2 <- glm(richness_siteT ~ grazing_m * aspect + wetness + ndvi + slope_deg + soil_depth, 
                    data = richness_site)

min(AIC(rga_glm, rga_glm2, rgaw_glm3, rgawn_glm, rgawns_glm, rgawnsd_glm, rgawn_glm2, rgawn_glm3,
        rgawn_glm4, rgawns_glm2, rgawnsd_glm2)[,2])   # lowest == 634.6302
AIC(rga_glm, rga_glm2, rgaw_glm3, rgawn_glm, rgawns_glm, rgawnsd_glm, rgawn_glm2, rgawn_glm3,
    rgawn_glm4, rgawns_glm2, rgawnsd_glm2)    # best is 'rgawnsd_glm2' 

## Choosing the best model 
r_null <- glm(richness_siteT ~ 1, data = richness_site)
AIC(r_null, rga_glm, rgaw_glm3, rgawn_glm, rgawns_glm, rgawnsd_glm, rgawnsd_glm2)  
  # 'rgawnsd_glm2' is best, with 9 DF  

hist(resid(rgawnsd_glm2))

# testing if the additional variables actually make it better
with(summary(rg_glm), 1 - deviance/null.deviance)  # R2 = 0.02848
with(summary(rga_glm), 1 - deviance/null.deviance)  # R2 =  0.179
with(summary(rgan_glm), 1 - deviance/null.deviance)  # R2 =  0.23126 
with(summary(rgas_glm), 1 - deviance/null.deviance)  # R2 = 0.329, better than ndvi
with(summary(rgad_glm), 1 - deviance/null.deviance)   # R2 = 0.1831, worse 
with(summary(rgaw_glm), 1 - deviance/null.deviance)   # R2 = 0.5623, wetness makes it way better 

with(summary(rgaw_glm3), 1 - deviance/null.deviance)  # R2 = 0.598
with(summary(rgawn_glm), 1 - deviance/null.deviance)  # R2 = 0.61033, better 
with(summary(rgawn_glm2), 1 - deviance/null.deviance)  # R2 = 0.6291, int doesn't improve much
with(summary(rgawn_glm3), 1 - deviance/null.deviance)  # R2 = 0.640, int doesn't improve much
with(summary(rgawn_glm4), 1 - deviance/null.deviance)  # R2 = 0.6513, int doesn't improve much

with(summary(rgawns_glm), 1 - deviance/null.deviance)  # R2 = 0.6571, improves quite a bit  
with(summary(rgawnd_glm), 1 - deviance/null.deviance)  # R2 = 0.6142, not worth including soil depth
with(summary(rgawnsd_glm), 1 - deviance/null.deviance)  # R2 = 0.6951, maybe ok with all 4

with(summary(rgawns_glm2), 1 - deviance/null.deviance)  # R2 = 0.6822, still improving I guess
with(summary(rgawnsd_glm2), 1 - deviance/null.deviance)  # R2 = 0.7346, still improving by a lot 


## Richness Model Results ## ----
# glm(richness_siteT ~ grazing_m * aspect + wetness + ndvi + slope_deg + soil_depth)

summary(rgawnsd_glm2)     ## USE THIS MODEL'S RESULTS ##
  # grazing NORTH: -0.034401 ± 0.020483, p = 0.0981 (not significant)
  # grazing SOUTH: 0.147290 ± 0.030190, p = 2.56e-06 (SIGNIFICANT)
  # wetness: 0.086433 ± 0.008993, p = < 2e-16 (SIGNIFICANT)
  # ndvi: 20.240012 ± 2.678091, p = 2.99e-12 (SIGNIFICANT)
  # slope: -0.342851 ± 0.044528, p = 1.33e-12 (SIGNIFICANT)
  # soil_depth: -0.523787 ± 0.092250, p = 6.26e-08 (SIGNIFICANT)

# calculating McFadden's R2 
with(summary(rgawnsd_glm2), 1 - deviance/null.deviance)  # R2 = 0.734451 (a great model)

# checking residuals 
plot(residuals(rgawnsd_glm2) ~ predict(rgawnsd_glm2, type = "response"))  # looks random = good


### Richness ~ Grazing (Sp Group Proportions) ----
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


## Richness Proportion Model Results ## ----
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
hist(richness_site$height_site)  # right skewed (positive skew)

hg1 <- lm(height_site ~ grazing_m, data = richness_site)
plot(hg1)
shapiro.test(resid(hg1))   # p < 0.05 = not normal (skewed)
bptest(hg1)  # p > 0.05 = no heteroskedasticity though 

## Transformations 
# log
hg2 <- lm(log(height_site) ~ grazing_m, data = richness_site)
plot(hg2)
shapiro.test(resid(hg2))   # p < 0.05 = not normal
bptest(hg2)  # p > 0.05 = no heteroskedasticity 

# sqrt 
hg3 <- lm(sqrt(height_site) ~ grazing_m, data = richness_site)
plot(hg3)
shapiro.test(resid(hg3))   # p < 0.05 = not normal, almost worse 
bptest(hg3)  # p > 0.05 = no heteroskedasticity 

# box-cox 
bc <- boxcox(hg1)
(lambda <- bc$x[which.max(bc$y)])  # extracting exact lambda for transformation 

hg4 <- lm(((height_site^lambda-1)/lambda) ~ grazing_m, data = richness_site)
plot(hg4)
shapiro.test(resid(hg4))   # p < 0.05 = not normal, nothing works 
bptest(hg4)  # p > 0.05 = no heteroskedasticity 


## GLM    
hg_glm <- glm(height_site ~ grazing_m, family = Gamma(link = log), data = richness_site)
hist(resid(hg_glm))  # without 'Gamma' distribution it wasn't super normal 


### Models with aspect 
hga1 <- lm(height_site ~ grazing_m + aspect, data = richness_site)
plot(hga1)
shapiro.test(resid(hga1))   # p < 0.05 = not normal 
bptest(hga1)  # p < 0.05 = and heteroskedasticity 

hga_glm <- glm(height_site ~ grazing_m + aspect, family = Gamma(link = log), data = richness_site)
hist(resid(hga_glm))

hga_glm2 <- glm(height_site ~ grazing_m*aspect, family = Gamma(link = log), data = richness_site)
hist(resid(hga_glm2))

AIC(hg_glm, hga_glm, hga_glm2)  # 'hga_glm2' is best


### Models with GIS variables (+ aspect)
## NDVI
hgan <- glm(height_site ~ grazing_m + aspect + ndvi, family = Gamma(link = log), data = richness_site)
hgan2 <- glm(height_site ~ grazing_m + aspect*ndvi, family = Gamma(link = log), data = richness_site)
hgan3 <- glm(height_site ~ grazing_m*aspect + ndvi, family = Gamma(link = log), data = richness_site)
hgan4 <- glm(height_site ~ grazing_m*aspect + ndvi*aspect, family = Gamma(link = log), 
             data = richness_site)
hgan5 <- glm(height_site ~ grazing_m*aspect*ndvi, family = Gamma(link = log), data = richness_site)

AIC(hga_glm2, hgan, hgan2, hgan3, hgan4, hgan5)  # 'hgan4' is best (grazing*aspect + ndvi*aspect)

## Slope
hgas <- glm(height_site ~ grazing_m + aspect + slope_deg, family = Gamma(link = log), 
            data = richness_site)
hgas2 <- glm(height_site ~ grazing_m + aspect*slope_deg, family = Gamma(link = log), 
             data = richness_site)
hgas3 <- glm(height_site ~ grazing_m*aspect + slope_deg, family = Gamma(link = log), 
             data = richness_site)
hgas4 <- glm(height_site ~ grazing_m*aspect + slope_deg*aspect, family = Gamma(link = log), 
             data = richness_site)
hgas5 <- glm(height_site ~ grazing_m*aspect*slope_deg, family = Gamma(link = log), 
             data = richness_site)

AIC(hga_glm2, hgas, hgas2, hgas3, hgas4, hgas5)  # 'hgas5' is best (grazing*aspect*slope)

## Soil depth
hgad <- glm(height_site ~ grazing_m + aspect + soil_depth, family = Gamma(link = log), 
            data = richness_site)
hgad2 <- glm(height_site ~ grazing_m + aspect*soil_depth, family = Gamma(link = log), 
             data = richness_site)
hgad3 <- glm(height_site ~ grazing_m*aspect + soil_depth, family = Gamma(link = log), 
             data = richness_site)
hgad4 <- glm(height_site ~ grazing_m*aspect + soil_depth*aspect, family = Gamma(link = log), 
             data = richness_site)
hgad5 <- glm(height_site ~ grazing_m*aspect*soil_depth, family = Gamma(link = log), 
             data = richness_site)

AIC(hga_glm2, hgad, hgad2, hgad3, hgad4, hgad5)  # 'hgad4' or 'hgad5' is best (probably 'hgad4')

## Wetness
hgaw <- glm(height_site ~ grazing_m + aspect + wetness, family = Gamma(link = log), 
            data = richness_site)
hgaw2 <- glm(height_site ~ grazing_m + aspect*wetness, family = Gamma(link = log), 
             data = richness_site)
hgaw3 <- glm(height_site ~ grazing_m*aspect + wetness, family = Gamma(link = log), 
             data = richness_site)
hgaw4 <- glm(height_site ~ grazing_m*aspect + wetness*aspect, family = Gamma(link = log), 
             data = richness_site)
hgaw5 <- glm(height_site ~ grazing_m*aspect*wetness, family = Gamma(link = log), 
             data = richness_site)

AIC(hga_glm2, hgaw, hgaw2, hgaw3, hgaw4, hgaw5)  # 'hgaw5' is best 

# comparing to each variable
AIC(hgan4, hgad4, hgad5, hgaw5, hgas5)  # 'hgan4' (NDVI) is best 


## NDVI + 1 other
hgans <- glm(height_site ~ grazing_m + aspect + ndvi + slope_deg, family = Gamma(link = log), 
             data = richness_site)
hgand <- glm(height_site ~ grazing_m + aspect + ndvi + soil_depth, family = Gamma(link = log), 
             data = richness_site)
hganw <- glm(height_site ~ grazing_m + aspect + ndvi + wetness, family = Gamma(link = log), 
             data = richness_site)

AIC(hgan4, hgans, hgand, hganw)  # 'hganw' = best 

## NDVI + Wetness + 1-2 others
hganws <- glm(height_site ~ grazing_m + aspect + ndvi + wetness + slope_deg, 
              family = Gamma(link = log), data = richness_site)
hganwd <- glm(height_site ~ grazing_m + aspect + ndvi + wetness + soil_depth, 
              family = Gamma(link = log), data = richness_site)
hganwsd <- glm(height_site ~ grazing_m + aspect + ndvi + wetness + slope_deg + soil_depth, 
               family = Gamma(link = log), data = richness_site)

AIC(hganw, hganws, hganwd, hganwsd)   # 'hganw' or 'hganws' = identical, 'hganw' is simpler

## Interaction possibilities  
# NDVI and wetness  
hganw2 <- glm(height_site ~ grazing_m*aspect + ndvi + wetness, family = Gamma(link = log), 
             data = richness_site)
hganw3 <- glm(height_site ~ grazing_m + aspect*ndvi + wetness, family = Gamma(link = log), 
             data = richness_site)
hganw4 <- glm(height_site ~ grazing_m + aspect*wetness + ndvi, family = Gamma(link = log), 
             data = richness_site)
hganw5 <- glm(height_site ~ grazing_m*aspect + aspect*ndvi + wetness, family = Gamma(link = log), 
              data = richness_site)
hganw6 <- glm(height_site ~ grazing_m + aspect*ndvi + wetness*aspect, family = Gamma(link = log), 
              data = richness_site)
hganw7 <- glm(height_site ~ grazing_m*aspect + ndvi + aspect*wetness, family = Gamma(link = log), 
              data = richness_site)
hganw8 <- glm(height_site ~ grazing_m*aspect + ndvi*aspect + aspect*wetness, 
              family = Gamma(link = log), data = richness_site)

min(AIC(hgan4, hganw, hganws, hganw2, hganw3, hganw4, hganw5, hganw6, hganw7, hganw8)[,2])    
  # lowest == 541.9329
AIC(hgan4, hganw, hganws, hganw2, hganw3, hganw4, hganw5, hganw6, hganw7, hganw8) 
  # 'hganw2' is best, because it's simplest 

# interaction with 3 GIS variables 
hganws2 <- glm(height_site ~ grazing_m*aspect + ndvi + wetness + slope_deg, 
               family = Gamma(link = log), data = richness_site)
hganws3 <- glm(height_site ~ grazing_m*aspect + ndvi*aspect + wetness + slope_deg, 
               family = Gamma(link = log), data = richness_site)
hganws4 <- glm(height_site ~ grazing_m*aspect + ndvi*aspect + wetness*aspect + slope_deg, 
               family = Gamma(link = log), data = richness_site)
hganws5 <- glm(height_site ~ grazing_m*aspect + ndvi*aspect + wetness*aspect + slope_deg*aspect, 
               family = Gamma(link = log), data = richness_site)
hganws6 <- glm(height_site ~ grazing_m*aspect + ndvi + wetness*aspect + slope_deg, 
               family = Gamma(link = log), data = richness_site)
hganws7 <- glm(height_site ~ grazing_m*aspect + ndvi + wetness*aspect + slope_deg*aspect, 
               family = Gamma(link = log), data = richness_site)

hganwd2 <- glm(height_site ~ grazing_m*aspect + ndvi + wetness + soil_depth, 
               family = Gamma(link = log), data = richness_site)
hganwd3 <- glm(height_site ~ grazing_m*aspect + ndvi*aspect + wetness + soil_depth, 
               family = Gamma(link = log), data = richness_site)
hganwd4 <- glm(height_site ~ grazing_m*aspect + ndvi*aspect + wetness*aspect + soil_depth, 
               family = Gamma(link = log), data = richness_site)
hganwd5 <- glm(height_site ~ grazing_m*aspect + ndvi*aspect + wetness*aspect + soil_depth*aspect, 
               family = Gamma(link = log), data = richness_site)
hganwd6 <- glm(height_site ~ grazing_m*aspect + ndvi + wetness*aspect + soil_depth, 
               family = Gamma(link = log), data = richness_site)
hganwd7 <- glm(height_site ~ grazing_m*aspect + ndvi + wetness*aspect + soil_depth*aspect, 
               family = Gamma(link = log), data = richness_site)

min(AIC(hganw, hganw2, hganws2, hganws3, hganws4, hganws5, hganws6, hganws7, hganwd2, hganwd3, 
        hganwd4, hganwd5, hganwd6, hganwd7)[,2])   
  # lowest == 524.799
AIC(hganw, hganw2, hganws2, hganws3, hganws4, hganws5, hganws6, hganws7, hganwd2, hganwd3, 
    hganwd4, hganwd5, hganwd6, hganwd7)  # 'hganwd5' is the best 

# interaction with all 4 variables
hganwsd2 <- glm(height_site ~ grazing_m*aspect + ndvi + wetness + slope_deg + soil_depth, 
                family = Gamma(link = log), data = richness_site)
hganwsd3 <- glm(height_site ~ grazing_m*aspect + ndvi*aspect + wetness + slope_deg + soil_depth, 
                family = Gamma(link = log), data = richness_site)
hganwsd4 <- glm(height_site ~ grazing_m*aspect + ndvi + wetness*aspect + slope_deg + soil_depth, 
                family = Gamma(link = log), data = richness_site)
hganwsd5 <- glm(height_site ~ grazing_m*aspect + ndvi + wetness + slope_deg*aspect + soil_depth, 
                family = Gamma(link = log), data = richness_site)
hganwsd6 <- glm(height_site ~ grazing_m*aspect + ndvi + wetness + slope_deg + soil_depth*aspect, 
                family = Gamma(link = log), data = richness_site)
hganwsd7 <- glm(height_site ~ grazing_m*aspect + ndvi*aspect + wetness*aspect + slope_deg + soil_depth, 
                family = Gamma(link = log), data = richness_site)
hganwsd8 <- glm(height_site ~ grazing_m*aspect + ndvi*aspect + wetness + slope_deg*aspect + soil_depth, 
                family = Gamma(link = log), data = richness_site)
hganwsd9 <- glm(height_site ~ grazing_m*aspect + ndvi*aspect + wetness + slope_deg + 
                soil_depth*aspect, family = Gamma(link = log), data = richness_site)
hganwsd10 <- glm(height_site ~ grazing_m*aspect + ndvi + wetness*aspect + slope_deg*aspect + 
                  soil_depth, family = Gamma(link = log), data = richness_site)
hganwsd11 <- glm(height_site ~ grazing_m*aspect + ndvi + wetness*aspect + slope_deg + 
                  soil_depth*aspect, family = Gamma(link = log), data = richness_site)
hganwsd12 <- glm(height_site ~ grazing_m*aspect + ndvi + wetness + slope_deg*aspect + 
                  soil_depth*aspect, family = Gamma(link = log), data = richness_site)
hganwsd13 <- glm(height_site ~ grazing_m*aspect + ndvi*aspect + wetness*aspect + slope_deg*aspect + 
                soil_depth, family = Gamma(link = log), data = richness_site)
hganwsd14 <- glm(height_site ~ grazing_m*aspect + ndvi*aspect + wetness*aspect + slope_deg*aspect + 
                soil_depth*aspect, family = Gamma(link = log), data = richness_site)

min(AIC(hganws2, hganwsd, hganwsd2, hganwsd3, hganwsd4, hganwsd5, hganwsd6, hganwsd7, hganwsd8, hganwsd9, 
        hganwsd10, hganwsd11, hganwsd12, hganwsd13, hganwsd14)[,2])   # lowest == 506.4835
AIC(hganws2, hganwsd, hganwsd2, hganwsd3, hganwsd4, hganwsd5, hganwsd6, hganwsd7, hganwsd8, hganwsd9, 
    hganwsd10, hganwsd11, hganwsd12, hganwsd13, hganwsd14)   # 'hganwsd14' is the best, but 13 DF


## Choosing the best model 
h_null <- glm(height_site ~ 1, family = Gamma(link = log), data = richness_site)
AIC(hganwd5, hganwsd14, h_null)  

# testing if the additional variables actually make it better
with(summary(hga_glm2), 1 - deviance/null.deviance)  # R2 = 0.0879 

with(summary(hgan), 1 - deviance/null.deviance)  # R2 =  0.398
with(summary(hgan4), 1 - deviance/null.deviance)  # R2 =  0.449, even better 
with(summary(hganw), 1 - deviance/null.deviance)  # R2 = 0.5136, definitely better 
with(summary(hganws), 1 - deviance/null.deviance)   # R2 = 0.523, slope doesn't add anything
with(summary(hganwd), 1 - deviance/null.deviance)   # R2 = 0.512, soil depth doesn't add either 

with(summary(hganw2), 1 - deviance/null.deviance)  # R2 = 0.5326
with(summary(hganw3), 1 - deviance/null.deviance)  # R2 = 0.5139 
with(summary(hganw4), 1 - deviance/null.deviance)  # R2 = 0.5144
with(summary(hganw5), 1 - deviance/null.deviance)  # R2 = 0.5341
with(summary(hganw6), 1 - deviance/null.deviance)  # R2 = 0.5149
with(summary(hganw7), 1 - deviance/null.deviance)  # R2 = 0.5326, has basically plateaued 

# choose model with only ndvi and wetness and use AIC to determine the best of those 
AIC(hgan4, hganw, hganws, hganw2, hganw3, hganw4, hganw5, hganw6, hganw7, hganw8)
  # use 'hganw2'

## Height Model Results ## ----
# height_site ~ grazing_m * aspect + ndvi + wetness, family = Gamma(link = log)

summary(hganw2)        ## USE THIS MODEL'S RESULTS ##
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


# . ----
#### Other Basic Statistics ----
### Richness ~ Aspect
ra1 <- lm(richness_siteT ~ aspect, data = richness_site)
shapiro.test(resid(ra1))  # not normal 

wilcox.test(richness_siteT ~ aspect, data = richness_site)
  # W = 5205, p = < 1.013e-07

# average height S vs. N
richness_site %>% group_by(aspect) %>% summarise(rich = mean(richness_siteT),
                                                 se = std.error(richness_siteT))
  # N = 17.1 ± 0.284
  # S = 14.7 ± 0.310


### Richness Proportion ~ Sp Group
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


# . ----
#### NMDS Relative Abundance ----
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

stressplot(mds6b)   # non-metric fit = 0.994, linear fit = 0.934
stressplot(mds7b)   # non-metric fit = 0.996, linear fit = 0.949
stressplot(mds8b)   # non-metric fit = 0.997, linear fit = 0.959
stressplot(mds9b)   # non-metric fit = 0.998, linear fit = 0.967

# 7 dimensions improves the linear fit quite a bit, then afterwards it tapers off 

# choosing k = 7

set.seed(7)
nmds7 <- metaMDS(sp_matrix4, distance = "bray", k = 7, autotransform = F, trymax = 500, 
                 sratmax = 0.99999)  
# note that autotransformation is off (default = T) 

nmds7
# stress = ~0.06574
# 7 dimensions 


### Effect of Env Variables ----
## For Sites
# removing unnecessary info from GIS data 
gisvar2 <- gisvar %>% 
              filter(!(plot_nr == 15)) %>% dplyr::select(!c(X, Y))

# making environmental variable vectors for the site 
gisvar_long <- left_join(gisvar2, sites)  
gisvar_long <- gisvar_long %>% mutate(grazing_m = grazing_s/60)

gisvar_sites <- gisvar_long %>% 
                  dplyr::select(site_nr, ndvi, slope_deg, wetness, soil_depth, grazing_s) %>% 
                  group_by(site_nr) %>% 
                  summarize(ndvi = mean(ndvi),
                            slope_deg = mean(slope_deg),
                            soil_depth = mean(soil_depth),
                            wetness = mean(wetness),
                            grazing_m = mean(grazing_m)) %>% 
                  ungroup() %>% 
                  dplyr::select(!site_nr)

gisfit <- envfit(nmds6, gisvar_sites, permu = 999)
gisfit   
  # GIS variables don't have an effect, and neither does aspect or grazing 
    # ndvi: R2 = 0.0268, p = 0.637
    # slope: R2 = 0.1065, p = 0.169 
    # wetness: R2 = 0.0004,  p = 0.988 
    # soil depth: R2 = 0.0325, p = 0.608
    # grazing: R2 = 0.0848, p = 0.240

## For Plots 
gisvar_plots <- gisvar_long %>%  
                  dplyr::select(site_nr, plot_nr, plot, ndvi, slope_deg, wetness, 
                                soil_depth, grazing_m) 
gisvar_plots2 <- gisvar_plots %>% dplyr::select(!c(plot, plot_nr))

gisfit2 <- envfit(nmds7, gisvar_plots2, permu = 999)
gisfit2   
  # ndvi: R2 = 0.2264, p = 0.001 (SIGNIFICANT)
  # slope: R2 = 0.2169, p = 0.001 (SIGNIFICANT)
  # wetness: R2 = 0.1994,  p = 0.001 (SIGNIFICANT) 
  # soil depth: R2 = 0.0073, p = 0.706  
  # grazing: R2 = 0.0038, p = 0.822   


## NMDS with GIS as matrix
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

#### Visualizing NMDS's ----
### Site
plot(nmds6, type = "t", display = "sites") 
plot(gisfit2, p.max = 0.01, col = "red")
dev.off()

### Plot
plot(nmds7, type = "t", display = "sites") 
plot(gisfit2, p.max = 0.01, col = "red")
dev.off()

### GIS ~ Sp 
gisvar_plots <- gisvar_plots %>% 
                  mutate(plot_rep2 = case_when(plot == "1" ~ "A",
                                               plot == "2" ~ "B",
                                               plot == "3" ~ "C")) %>% 
                  mutate(site_plot = str_c(site_nr, "", plot_rep2))

plot(nmds4, type = "n") 
text(nmds4, labels = gisvar_plots$site_plot, cex = 0.5, col = gisvar_plots$site_nr)
# dev.off()

# envfit() for relative abundance 
spfit <- envfit(nmds4, sp_matrix4, permutations = 999)

# plot(nmds4, type = "n") 
# text(nmds4, labels = gisvar_plots$site_plot, cex = 0.5, col = gisvar_plots$site_nr)
plot(spfit, p.max = 0.001, cex = 0.8)  # adding arrows to the above plot §

# plot(gisfit2, p.max = 0.01, col = "red")
dev.off()

#### ANOSIM Tests ----
### By Site 
## Rel abundance ~ aspect 
ano_sp <- anosim(sp_matrix2, sp_matrix$aspect, distance = "bray", permutations = 9999)
ano_sp
  # ANOSIM R = 0.04279, p = 0.1167 (not significant)

## Rel abundance ~ grazing category 
# making grazing categories for sites instead of plots 
grazecat <- sites %>% 
              mutate(grazing_cat2 = as.numeric(grazing_cat2)) %>% 
              group_by(site_nr) %>% 
              summarize(grazecat = mean(grazing_cat2)) %>% 
              mutate(grazecat = round(grazecat)) %>% 
              mutate(grazecat = as.character(grazecat))

ano_g <- anosim(sp_matrix2, grazecat$grazecat, distance = "bray", permutations = 9999)
ano_g 
  # no difference 

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



#
#
# OLD #
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

