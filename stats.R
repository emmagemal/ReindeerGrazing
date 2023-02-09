## Data Exploration and Statistics ##
## For SU Master's Thesis in Landscape Ecology
## Emma Gemal
## Last updated 9/2/23

### Library ----
library(tidyverse)


### Data Import ----
sites <- read.csv("Data/site_conditions.csv")
plots <- read.csv("Data/plots_tidy.csv")
coverage <- read.csv("Data/coverage.csv")

### Data Exploration & Cleaning ----
view(sites)
view(plots)
view(coverage)

str(sites)   # change date to date, plot to cat, and maybe x and y coord to coordinates
str(plots)   # change date to date and check for spelling mistakes and duplicates
str(coverage)   # change date to date

## Editing import format  
sites <- sites %>% 
            mutate(date = as.Date(date, format = "%d/%m/%y"),
                   plot = as.character(plot)) %>% 
            mutate(aspect = case_when(grepl("S", site) ~ "S",
                                      grepl("N", site) ~ "N"))   # adding aspect
str(sites)            

plots <- plots %>% 
            mutate(date = as.Date(date, format = "%d/%m/%y"),
                   sp_group = as.factor(sp_group))
str(plots)

coverage <- coverage %>%  
              mutate(date = as.Date(date, format = "%d/%m/%y"),
                     sp_group = as.factor(sp_group))
str(coverage) 

## Checking species 
sort(unique(plots$sp_common))  # remove whitespace after words 
                               # "purple mountain heather" & "purple mountain-heath" 

sort(unique(plots$sp_latin))  # some don't have a latin name... not good 
                              # remove whitespace after words
                              # "Peltigera aphthosa" & "Peltigera apthosa" 
                              # "Pleurozium schereberi" & "Pleurozium schreberi"
                              # "Stereocaulon alpinus" & "Stereocaulon alpinum"
                              # "Stereocaulon sp," & "Stereocaulon sp."
                              # "Vaxinium uliginosum" & "Vaccinium uliginosum"

unique(coverage$sp_group)
unique(plots$sp_group)

## Editing species that had mistakes 
plots <- plots %>% 
            mutate(sp_common = ifelse(sp_common == "purple mountain-heath", 
                                      "purple mountain heather",
                                      sp_common)) %>% 
            mutate(sp_common = str_trim(sp_common, side = "right")) %>% 
            mutate(sp_latin = str_trim(sp_latin, side = "right")) %>% 
            mutate(sp_latin = ifelse(sp_latin == "Peltigera apthosa", "Peltigera aphthosa",
                                     sp_latin),
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

plots %>% group_by(plot_nr) %>% summarize(n = length(unique(sp_latin)))
plots %>% group_by(plot_nr) %>% summarize(n = length(sp_latin))  # same number of names = no duplicates


### Variable Calculations ----
## Species richness
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
                        richness_group = mean(richness_group)) %>% 
              ungroup() %>% 
              group_by(plot_nr) %>% 
              mutate(rich_group_prop = (richness_group/sum(richness_group))) %>% 
              ungroup() %>%
              group_by(site_nr) %>% 
              mutate(richness_siteT = mean(richness_plot))   # true richness per site (with reps)

# combining 'richness' with 'sites' to add grazing data 
grazing <- sites %>% dplyr::select(site_nr, plot_nr, aspect, grazing_s)

richness <- left_join(richness, grazing)
str(richness)
richness <- as.data.frame(richness)

## Adding grazing data to 'coverage' 
coverage <- left_join(coverage, grazing)
str(coverage)


### Initial Visualizations ----
ggplot(richness, aes(x = grazing_s, y = richness_plot)) +
  geom_point()

ggplot(richness, aes(x = grazing_s, y = richness_plot)) +
  geom_point(aes(color = aspect)) +
  stat_smooth(method = "lm", aes(color = aspect))

ggplot(richness, aes(x = grazing_s, y = richness_siteT)) +
  geom_point()

ggplot(richness, aes(x = grazing_s, y = richness_siteT)) +
  geom_point(aes(color = aspect)) +
  stat_smooth(method = "lm", aes(color = aspect))

ggplot(richness, aes(x = grazing_s, y = rich_group_prop)) +
  geom_point(aes(color = sp_group)) +
  stat_smooth(method = "lm", aes(color = sp_group))

ggplot(richness, aes(x = grazing_s, y = rich_group_prop)) +
  geom_point(aes(color = sp_group)) +
  stat_smooth(method = "lm", aes(color = sp_group)) +
  facet_wrap(~aspect)

ggplot(richness, aes(x = aspect, y = richness_siteT)) +
  geom_boxplot()


ggplot(coverage, aes(x = grazing_s, y = coverage_perc)) +
  geom_point()

ggplot(coverage, aes(x = grazing_s, y = coverage_perc)) +
  geom_point(aes(color = sp_group)) +
  stat_smooth(method = "lm", aes(color = sp_group))

ggplot(coverage, aes(x = grazing_s, y = coverage_perc)) +
  geom_point(aes(color = sp_group)) +
  stat_smooth(method = "lm", aes(color = sp_group)) +
  facet_wrap(~aspect)
