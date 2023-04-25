## Data Visualization (Figures) ##
## For SU Master's Thesis in Landscape Ecology
## Emma Gemal
## Last updated 10/2/23

### Library ----
library(tidyverse)
library(ggeffects)   # for 'ggpredict()'
library(viridis)
library(ggh4x)
library(ggpubr)   # 'ggarrange'


#### Thesis Plot Theme ---- 
theme_thesis <- theme_bw() +
                  theme(panel.grid = element_blank(),
                        axis.title.x = 
                          element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
                        axis.title.y = 
                          element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
                  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))


#### Data Import ----
sites <- read.csv("Data/site_conditions.csv")
plots <- read.csv("Data/plots_tidy.csv")
coverage <- read.csv("Data/coverage.csv")

# GIS variables
ndvi <- read.csv("Data/NDVI_plots.csv")
slope <- read.csv("Data/slope.csv")
soildepth <- read.csv("Data/soildepth.csv")
wetness <- read.csv("Data/wetness.csv")

#### Editing Import Format ----
## Main variables 
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

## GIS variables
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

# checking if groups match the names (no incorrect groups)
names <- plots %>% 
            group_by(sp_latin) %>% 
            summarize(sp_latin = unique(sp_latin),
                      sp_group = sp_group) %>% 
            distinct()    # Antennaria alpina = herb and shrub, should only be herb 
                          # Polytrichum hyperboreum = moss and lichen, should be moss
                          # Lysimachia europaea = shrub and herb, should be herb
                          # Lycopodium annotinum = herb and moss, should be herb 
                          # Cladonia rangiferina = moss and lichen, should be lichen 


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
                   sp_latin = ifelse(sp_latin == "Pilocella sp.", "Pilosella sp.", sp_latin)) %>% 
            mutate(sp_group = ifelse(sp_latin == "Antennaria alpina", "herb", sp_group),
                   sp_group = ifelse(sp_latin == "Polytrichum hyperboreum", "moss", sp_group),
                   sp_group = ifelse(sp_latin == "Lysimachia europaea", "herb", sp_group),
                   sp_group = ifelse(sp_latin == "Lycopodium annotinum", "herb", sp_group),
                   sp_group = ifelse(sp_latin == "Cladonia rangiferina", "lichen", sp_group))

plots[(plots$sp_latin == ""), ]   # no latin name because they are "unknown"
plots <- plots %>%  
            mutate(sp_latin = ifelse(sp_latin == "", "unknown", sp_latin))


#### Variable Calculations ----
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
              mutate(rich_propT = mean(rich_group_prop)) %>% 
              mutate(rich_prop_perc = rich_propT*100)

## GIS Standardization
gisvar <- gisvar %>% 
            mutate(across(c("ndvi":"soil_depth"), 
                          .fns = list(s = scale))) %>% 
            mutate(ndvi_s = as.vector(ndvi_s),
                   slope_deg_s = as.vector(slope_deg_s),
                   wetness_s = as.vector(wetness_s),
                   soil_depth_s = as.vector(soil_depth_s))


#### Combining dataframes ----
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


### Creating summary dataframes
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
#### Pie Chart of Species ----
# creating a df with the total number of species per group
rich_sum <- plots %>% 
              group_by(sp_group) %>% 
              summarize(richness = length(unique(sp_latin))) %>% 
              mutate(sp_group = factor(sp_group, 
                                       levels = c("moss", "shrub", "grass", "lichen", "herb"),
                                       labels = c("Bryophytes", "Shrubs", "Graminoids", "Lichens",
                                                  "Forbs"))) %>% 
              arrange(desc(richness))

(pie_plot <- ggplot(rich_sum, aes(x = "", y = richness, fill = sp_group)) +
                geom_bar(stat = "identity", width = 1, color = "white") +
                coord_polar("y", start = 0) +
                theme_thesis +
                theme(axis.title.x = element_blank(),
                      axis.title.y = element_blank(),
                      axis.text.x = element_blank(),
                      panel.border = element_blank(),
                      axis.ticks = element_blank()) +   
                geom_text(aes(label = paste(round(richness))), 
                              position = position_stack(vjust = 0.5), color = "white") +
                scale_fill_manual(values = c("#73A6E7", "#867441", "#3A69B4", "#BFBF88", "#524B49"),
                                  name = "Species Group"))

ggsave("Figures/piechart.png", plot = pie_plot, width = 5, height = 5, units = "in")


# . ----
#### Richness Plots ----
# final model 
rgawn_og <- lm(richness_siteT ~ grazing_m + aspect + wetness + ndvi, richness_site2)
  # use this model instead of actual final due to similar outputs in the correct scale 

## Richness per site ~ grazing ----
# extracting model predictions for plotting
rgawn_graze <- ggpredict(rgawn_og, terms = c("grazing_m", "aspect")) %>% 
                  rename(aspect = group)
rich_graze_data <- left_join(richness_site2, rgawn_graze)

# the plot 
(rich_plot <- ggplot(rich_graze_data, aes(x, predicted)) +
                geom_point(aes(x = grazing_m, y = richness_siteT, color = aspect, shape = aspect,
                               fill = aspect), alpha = 0.5, size = 1) +
                geom_line(aes(color = aspect)) +
                geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = aspect), alpha = 0.4) +
                theme_thesis + 
                theme(plot.margin = unit(c(1, 0.5, 0.1, 0.5), "cm"),
                      legend.margin = margin(l = 0.5, b = 0.5, unit = "cm")) +  
                            # for no boxplot, make legend left margin 1.5
                labs(x = "Grazing (min)", y = "Species Richness") +
                scale_color_manual(values = c("#FF4B0A", "#FFA033"),
                                   labels = c("North", "South"),
                                   name = "Aspect") +
                scale_shape_manual(values = c(21, 22),
                                   labels = c("North", "South"),
                                   name = "Aspect") +
                scale_fill_manual(values = c("#FF4B0A", "#FFA033"),
                                  labels = c("North", "South"),
                                  name = "Aspect"))

## Richness per site ~ NDVI ----
# extracting model predictions for plotting
rgawn_ndvi <- ggpredict(rgawn_og, terms = c("ndvi [-0.05, 0, 0.05, 0.15, 0.25]", "aspect")) %>% 
                rename(aspect = group)

rich_ndvi_data <- left_join(richness_site2, rgawn_ndvi)

# the plot 
(rich_plot2 <- ggplot(rich_ndvi_data, aes(x, predicted)) +
                  geom_point(aes(x = ndvi, y = richness_siteT, color = aspect, shape = aspect,
                                 fill = aspect), alpha = 0.5, size = 1) +
                  geom_line(aes(color = aspect)) +
                  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = aspect), alpha = 0.4) +
                  theme_thesis + 
                  theme(plot.margin = unit(c(1, 0.5, 0.1, 0.5), "cm")) +
                  labs(x = "NDVI", y = "Species Richness") +
                  scale_color_manual(values = c("#FF4B0A", "#FFA033"),
                                     labels = c("North", "South"),
                                     name = "Aspect") +
                  scale_shape_manual(values = c(21, 22),
                                     labels = c("North", "South"),
                                     name = "Aspect") +
                  scale_fill_manual(values = c("#FF4B0A", "#FFA033"),
                                    labels = c("North", "South"),
                                    name = "Aspect"))

## Richness per site ~ wetness ----
# extracting model predictions for plotting
rgawn_wet <- ggpredict(rgawn_og, terms = c("wetness", "aspect")) %>% 
                rename(aspect = group)
rich_wet_data <- left_join(richness_site2, rgawn_wet)

# the plot 
(rich_plot3 <- ggplot(rich_wet_data, aes(x, predicted)) +
                  geom_point(aes(x = wetness, y = richness_siteT, color = aspect, shape = aspect,
                                 fill = aspect), alpha = 0.5, size = 1) +
                  geom_line(aes(color = aspect)) +
                  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = aspect), alpha = 0.4) +
                  theme_thesis + 
                  theme(plot.margin = unit(c(1, 0.5, 0.1, 0.5), "cm")) +
                  labs(x = "Soil Wetness", y = "Species Richness") +
                  scale_color_manual(values = c("#FF4B0A", "#FFA033"),
                                     labels = c("North", "South"),
                                     name = "Aspect") +
                  scale_shape_manual(values = c(21, 22),
                                     labels = c("North", "South"),
                                     name = "Aspect") +
                  scale_fill_manual(values = c("#FF4B0A", "#FFA033"),
                                    labels = c("North", "South"),
                                    name = "Aspect"))

## Richness per site ~ aspect (boxplot) ----
(rich_plot4 <- ggplot(richness_site2, aes(x = aspect, y = richness_siteT)) +
                  geom_boxplot(aes(color = aspect, fill = aspect), alpha = 0.5) +
                  annotate("text", x = "N", y = 22, label = "***", fontface = 2) +
                  theme_thesis + 
                  theme(plot.margin = unit(c(1, 0.5, 0.1, 0.5), "cm"),
                        legend.position = "none") +
                  labs(x = "Aspect", y = "Species Richness") +
                  scale_color_manual(values = c("#FF4B0A", "#FFA033"),
                                     labels = c("North", "South"),
                                     name = "Aspect") +
                  scale_fill_manual(values = c("#FF4B0A", "#FFA033"),
                                    labels = c("North", "South"),
                                    name = "Aspect") +
                  scale_x_discrete(labels = c("North", "South")) +
                  ylim(c(9, 23)))


## Combined plots ----
(rich_grid <- ggarrange(nrow = 2, heights = c(1,1),
                        ggarrange(rich_plot, NULL, widths = c(3,0.2), labels = c("A", "")),
                        ggarrange(rich_plot2, rich_plot3, legend = F, labels = c("B", "C"),
                                  widths = c(1, 1))))

# with boxplot (USE THIS ONE)
(rich_grid2 <- ggarrange(nrow = 2, heights = c(1.15,1),
                         ggarrange(rich_plot, rich_plot4, widths = c(1, 1), labels = c("A", "B"),
                                   common.legend = T),
                         ggarrange(rich_plot3, rich_plot2, legend = F, labels = c("C", "D"),
                                   widths = c(1, 1))))


# ggsave("Figures/richness_plots.png", plot = rich_grid, width = 6, height = 5.8, unit = "in")
ggsave("Figures/richness_4figs.png", plot = rich_grid2, width = 6, height = 6.1, unit = "in")


#### Richness Proportion Plot ----
### Richness proportion ~ grazing ----
## Models ----
# lichen
lichen <- richness_site %>% filter(sp_group == "lichen")
lgan_log3_og <- lm(log(rich_propT) ~ grazing_m*aspect + ndvi, data = lichen)

lgan_pred <- ggpredict(lgan_log3_og, terms = c("grazing_m", "aspect"), back.transform = T) %>% 
                rename(aspect = group)
lichen_lgan <- left_join(lichen, lgan_pred)

# moss
moss <- richness_site %>% filter(sp_group == "moss")
mganw_og <- lm(rich_propT ~ grazing_m + aspect + ndvi + wetness, data = moss)

mganw_pred <- ggpredict(mganw_og, terms = c("grazing_m", "aspect")) %>% 
                rename(aspect = group)
moss_mganw <- left_join(moss, mganw_pred)

# shrub
shrub <- richness_site %>% filter(sp_group == "shrub")
sgaw3_og <- lm(rich_propT ~ grazing_m*aspect + wetness, data = shrub)

sgaw3_pred <- ggpredict(sgaw3_og, terms = c("grazing_m", "aspect")) %>% 
                rename(aspect = group)
shrub_sgaw3 <- left_join(shrub, sgaw3_pred)

# grass
grass <- richness_site %>% filter(sp_group == "grass")
ggaw4_og <- lm(rich_propT ~ grazing_m*aspect + wetness*aspect, data = grass)

ggaw4_pred <- ggpredict(ggaw4_og, terms = c("grazing_m", "aspect")) %>% 
                rename(aspect = group)
grass_ggaw4 <- left_join(grass, ggaw4_pred)

# herb
herb <- richness_site %>% filter(sp_group == "herb")
hgaw_og <- lm(rich_propT ~ grazing_m + aspect + wetness, data = herb)

hgaw_pred <- ggpredict(hgaw_og, terms = c("grazing_m", "aspect")) %>% 
                rename(aspect = group)
herb_hgaw <- left_join(herb, hgaw_pred)

# combined
groups <- full_join(lichen_lgan, moss_mganw)
groups <- full_join(groups, shrub_sgaw3)
groups <- full_join(groups, grass_ggaw4)
groups <- full_join(groups, herb_hgaw)

## Plots ----
# with only aspect facetted 
(prop_plot <- ggplot(groups, aes(x, predicted, group = sp_group)) +
                  geom_point(aes(x = grazing_m, y = rich_propT, color = sp_group)) +
                  geom_line(aes(color = sp_group)) +
                  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = sp_group), alpha = 0.3) +
                  labs(x = "Grazing (min)", y = "Relative Species Richness") +
                  theme_thesis + 
                  facet_wrap(~aspect))


# with only sp group facetted (this is better than above)
groups <- groups %>% 
            mutate(sp_group = factor(sp_group, 
                                     levels = c("grass", "lichen", "shrub", "moss", "herb"),
                                     labels = c("Graminoids", "Lichens", "Shrubs", "Bryophytes", 
                                                "Forbs")))

stripcol <- strip_themed(background_x = elem_list_rect(color = c("#3A69B4", "#BFBF88", "#867441", 
                                                                      "#73A6E7", "#524B49"),
                                                       fill = NA))
stripcol2 <- strip_themed(background_x = elem_list_rect(fill = c("#93AFDC", "#E2E2CA", "#C3B283", 
                                                                          "#DCE9F9", "#9F9593")))

(prop_plot2 <- ggplot(groups, aes(x, predicted, group = aspect)) +
                  geom_point(aes(x = grazing_m, y = rich_propT, color = aspect, shape = aspect, 
                                 fill = aspect), alpha = 0.5, size = 1) +
                  geom_line(aes(color = aspect)) +
                  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = aspect), alpha = 0.4) +
                  labs(x = "Grazing (min)", y = "Relative Species Richness") +
                  theme_thesis + 
                  facet_wrap2(~sp_group, scales = "free_y", strip = stripcol2) +
                  theme(legend.position = c(1, 0),
                        legend.justification = c(1, 0),
                        strip.text = element_text(face = "bold")) +
               #         strip.background = element_rect(linewidth = 2, fill = "grey92")) +
                  scale_color_manual(values = c("#FF4B0A", "#FFA033"),
                                     labels = c("North", "South"),
                                     name = "Aspect") +
                  scale_shape_manual(values = c(21, 22),
                                     labels = c("North", "South"),
                                     name = "Aspect") +
                  scale_fill_manual(values = c("#FF4B0A", "#FFA033"),
                                    labels = c("North", "South"),
                                    name = "Aspect"))

ggsave("Figures/sp_prop2.png", plot = prop_plot2, width = 6.5, height = 4.8, units = "in")


### Richness proportion ~ NDVI + wetness ----
## Models ----
# lichen
lgan_pred2 <- ggpredict(lgan_log3_og, terms = c("ndvi", "aspect"), back.transform = T) %>% 
                rename(aspect = group)
lichen_lgan2 <- left_join(lichen, lgan_pred2)

# moss
mganw_pred2 <- ggpredict(mganw_og, terms = c("ndvi", "aspect")) %>% 
                  rename(aspect = group)
moss_mganw2 <- left_join(moss, mganw_pred2)

mganw_pred3 <- ggpredict(mganw_og, terms = c("wetness", "aspect")) %>% 
                  rename(aspect = group)
moss_mganw3 <- left_join(moss, mganw_pred3)


# shrub
sgaw3_pred2 <- ggpredict(sgaw3_og, terms = c("wetness", "aspect")) %>% 
                  rename(aspect = group)
shrub_sgaw32 <- left_join(shrub, sgaw3_pred2)

# grass
ggaw4_pred2 <- ggpredict(ggaw4_og, terms = c("wetness", "aspect")) %>% 
                  rename(aspect = group)
grass_ggaw42 <- left_join(grass, ggaw4_pred2)

# herb
hgaw_pred2 <- ggpredict(hgaw_og, terms = c("wetness", "aspect")) %>% 
                rename(aspect = group)
herb_hgaw2 <- left_join(herb, hgaw_pred2)

# combined
groups_ndvi <- full_join(lichen_lgan2, moss_mganw2)

groups_wetness <- full_join(moss_mgws23, shrub_sgaw32)
groups_wetness <- full_join(groups_wetness, grass_ggaw42)
groups_wetness <- full_join(groups_wetness, herb_hgaw2)

groups_ndvi <- groups_ndvi %>% 
                  mutate(sp_group = factor(sp_group, 
                                           levels = c("grass", "lichen", "shrub", "moss", "herb"),
                                           labels = c("Graminoids", "Lichens", "Shrubs", "Bryophytes", 
                                                      "Forbs")))
groups_wetness <- groups_wetness %>% 
                    mutate(sp_group = factor(sp_group, 
                                             levels = c("grass", "lichen", "shrub", "moss", "herb"),
                                             labels = c("Graminoids", "Lichens", "Shrubs", "Bryophytes", 
                                                        "Forbs")))
## Plots ----
# NDVI
stripcoln <- strip_themed(background_x = elem_list_rect(fill = c("#E2E2CA", "#DCE9F9")))
                                                                          
(prop_plot4 <- ggplot(groups_ndvi, aes(x, predicted, group = aspect)) +
                  geom_point(aes(x = ndvi, y = rich_propT, color = aspect, shape = aspect, 
                                 fill = aspect), alpha = 0.5, size = 1) +
                  geom_line(aes(color = aspect)) +
                  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = aspect), alpha = 0.4) +
                  labs(x = "NDVI", y = "Relative Species Richness") +
                  theme_thesis + 
                  theme(plot.margin = margin(t = 0.5, r = 0.3, unit = "cm")) +
                  facet_wrap2(~sp_group, strip = stripcoln, dir = "v") +
                  theme(strip.text = element_text(face = "bold")) +
                  #         strip.background = element_rect(linewidth = 2, fill = "grey92")) +
                  scale_color_manual(values = c("#FF4B0A", "#FFA033"),
                                     labels = c("North", "South"),
                                     name = "Aspect") +
                  scale_shape_manual(values = c(21, 22),
                                     labels = c("North", "South"),
                                     name = "Aspect") +
                  scale_fill_manual(values = c("#FF4B0A", "#FFA033"),
                                    labels = c("North", "South"),
                                    name = "Aspect"))

# ggsave("Figures/sp_prop_ndvi.png", plot = prop_plot4, width = 6, height = 3, units = "in")


# Wetness
stripcolw <- strip_themed(background_x = elem_list_rect(fill = c("#93AFDC", "#C3B283", "#DCE9F9", 
                                                                          "#9F9593")))

(prop_plot5 <- ggplot(groups_wetness, aes(x, predicted, group = aspect)) +
                  geom_point(aes(x = wetness, y = rich_propT, color = aspect, shape = aspect, 
                                 fill = aspect), alpha = 0.5, size = 1) +
                  geom_line(aes(color = aspect)) +
                  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = aspect), alpha = 0.4) +
                  labs(x = "Soil Wetness", y = "Relative Species Richness") +
                  theme_thesis + 
                  theme(plot.margin = margin(t = 0.5, l = 0.3, unit = "cm")) +
                  facet_wrap2(~sp_group, scales = "free_y", strip = stripcolw) +
                  theme(strip.text = element_text(face = "bold")) +
                  #         strip.background = element_rect(linewidth = 2, fill = "grey92")) +
                  scale_color_manual(values = c("#FF4B0A", "#FFA033"),
                                     labels = c("North", "South"),
                                     name = "Aspect") +
                  scale_shape_manual(values = c(21, 22),
                                     labels = c("North", "South"),
                                     name = "Aspect") +
                  scale_fill_manual(values = c("#FF4B0A", "#FFA033"),
                                    labels = c("North", "South"),
                                    name = "Aspect"))

# ggsave("Figures/sp_prop_wetness.png", plot = prop_plot5, width = 6, height = 5, units = "in")


## Combined plots
(abiotic_grid <- ggarrange(nrow = 1, ncol = 2, widths = c(1.1,2), common.legend = T,
                           prop_plot4, prop_plot5, labels = c("A", "B")))

ggsave("Figures/sp_prop_abiotic.png", plot = abiotic_grid, width = 6.5, height = 5, units = "in")


# ----
# not using below (but nice to keep for future)
scales <- list(sp_group == "Graminoids" ~ scale_y_continuous(limits = c(0.05, 0.30)),
               sp_group == "Forbs" ~ scale_y_continuous(limits = c(0, 0.35)),
               sp_group == "Lichens" ~ scale_y_continuous(limits = c(0.1, 0.60)),
               sp_group == "Bryophytes" ~ scale_y_continuous(limits = c(0, 0.35)),
               sp_group == "Shrubs" ~ scale_y_continuous(limits = c(0.1, 0.45)))
               
(prop <- ggplot(groups, aes(x, predicted, group = aspect)) +
            geom_point(aes(x = grazing_m, y = rich_propT, color = aspect, shape = aspect,
                           fill = aspect), alpha = 0.5, size = 1) +
            geom_line(aes(color = aspect)) +
            geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = aspect), alpha = 0.3) +
            labs(x = "Grazing (min)", y = "Relative Species Richness") +
            theme_thesis + 
            facet_nested_wrap(~sp_group, scales = "free_y") +
            facetted_pos_scales(y = scales) +
            scale_color_manual(values = c("#FFA033", "#FF4B0A"),
                               labels = c("N", "S"),
                               name = "Aspect") +
            scale_shape_manual(values = c(21, 22),
                               labels = c("N", "S"),
                               name = "Aspect") +
            scale_fill_manual(values = c("#FFA033", "#FF4B0A"),
                              labels = c("N", "S"),
                              name = "Aspect"))

(prop_violin <- ggplot(richness_site, aes(x = sp_group, y = rich_propT)) +   
                  geom_violin(aes(fill = aspect), color = NA, alpha = 0.5) +
                  geom_boxplot(aes(color = aspect), fill = NA, width = 0.5, 
                               position = position_dodge(width = 0.9)) +
                  theme_thesis + 
                  labs(x = "Grazing (min)", y = "Relative Species Richness"))


#### True Richness Plots ---- 
## Richness ~ grazing ----
richness_site <- richness_site %>% 
                    mutate(aspect2 = case_when(aspect == "N" ~ "North",
                                               aspect == "S" ~ "South")) %>% 
                    mutate(sp_group = factor(sp_group, 
                                             levels = c("grass", "lichen", "shrub", "moss", "herb"),
                                             labels = c("Graminoids", "Lichens", "Shrubs", 
                                                        "Bryophytes", "Forbs")))

(group_plot <- ggplot(richness_site, aes(grazing_m, richness_group, group = sp_group)) +
                stat_smooth(method = "lm", aes(color = sp_group, fill = sp_group), se = T,
                            alpha = 0.1) +
                labs(x = "Grazing (min)", y = "Species Richness") +
                theme_thesis + 
                theme(plot.margin = margin(r = 1, b = 0.5, l = 0.05, t = 0.05, unit = "cm"),
                      legend.margin = margin(l = 5.5, t = 3.2, unit = "cm")) +
                facet_wrap(~aspect2) +
                scale_color_manual(values = c("#3A69B4", "#BFBF88", "#867441", "#73A6E7", 
                                                       "#524B49"),
                                                       labels = c("Graminoids", "Lichens", "Shrubs", 
                                                                  "Bryophytes", "Forbs"),
                                   name = "Species Group") +
                scale_fill_manual(values = c("#3A69B4", "#BFBF88", "#867441", "#73A6E7", 
                                                      "#524B49"),
                                                      labels = c("Graminoids", "Lichens", "Shrubs", 
                                                                 "Bryophytes", "Forbs"),
                                  name = "Species Group"))

# ggsave("Figures/group_richness.png", plot = group_plot, width = 5.5, height = 4, units = "in")


## Richness ~ wetness ----
# no aspect
(group_plot2 <- ggplot(richness_site, aes(wetness, richness_group, group = sp_group)) +
                  stat_smooth(method = "lm", aes(color = sp_group, fill = sp_group), se = T,
                              alpha = 0.1) +
                  labs(x = "Soil Wetness", y = "Species Richness") +
                  theme_thesis + 
                  theme(plot.margin = margin(0.5, 0.75, 0.5, 0.5, "cm")) +
                  scale_color_manual(values = c("#3A69B4", "#BFBF88", "#867441", "#73A6E7", 
                                                         "#524B49"),
                                                         labels = c("Graminoids", "Lichens", "Shrubs", 
                                                                    "Bryophytes", "Forbs"),
                                     name = "Species Group") +
                  scale_fill_manual(values = c("#3A69B4", "#BFBF88", "#867441", "#73A6E7", 
                                                        "#524B49"),
                                                        labels = c("Graminoids", "Lichens", "Shrubs", 
                                                                   "Bryophytes", "Forbs"),
                                    name = "Species Group"))

# facetted by aspect 
(group_plot3 <- ggplot(richness_site, aes(wetness, richness_group, group = sp_group)) +
                  stat_smooth(method = "lm", aes(color = sp_group, fill = sp_group), se = T,
                              alpha = 0.1) +
                  labs(x = "Soil Wetness", y = "Species Richness") +
                  theme_thesis + 
                  theme(plot.margin = margin(t = 0.5, l = 0.5, r = 0.05, b = 0.05, unit = "cm")) +
                  facet_wrap(~aspect2) +   # facetted by aspect 
                  scale_color_manual(values = c("#3A69B4", "#BFBF88", "#867441", "#73A6E7", 
                                                         "#524B49"),
                                                         labels = c("Graminoids", "Lichens", "Shrubs", 
                                                                    "Bryophytes", "Forbs"),
                                     name = "Species Group") +
                  scale_fill_manual(values = c("#3A69B4", "#BFBF88", "#867441", "#73A6E7", 
                                                        "#524B49"),
                                                        labels = c("Graminoids", "Lichens", "Shrubs", 
                                                                   "Bryophytes", "Forbs"),
                                    name = "Species Group"))

# ggsave("Figures/group_wetness.png", plot = group_plot3, width = 5.5, height = 4, units = "in")


## Richness ~ NDVI ----
# no aspect
(group_plot4 <- ggplot(richness_site, aes(ndvi, richness_group, group = sp_group)) +
                  stat_smooth(method = "lm", aes(color = sp_group, fill = sp_group), se = T,
                              alpha = 0.1) +
                  labs(x = "NDVI", y = "Species Richness") +
                  theme_thesis + 
                  theme(plot.margin = margin(0.5, 0.5, 0.5, 0.75, "cm")) +
                  scale_color_manual(values = c("#3A69B4", "#BFBF88", "#867441", "#73A6E7", 
                                                         "#524B49"),
                                                         labels = c("Graminoids", "Lichens", "Shrubs", 
                                                                    "Bryophytes", "Forbs"),
                                     name = "Species Group") +
                  scale_fill_manual(values = c("#3A69B4", "#BFBF88", "#867441", "#73A6E7", 
                                                        "#524B49"),
                                                        labels = c("Graminoids", "Lichens", "Shrubs", 
                                                                   "Bryophytes", "Forbs"),
                                    name = "Species Group"))

# facetted by aspect
(group_plot5 <- ggplot(richness_site, aes(ndvi, richness_group, group = sp_group)) +
                  stat_smooth(method = "lm", aes(color = sp_group, fill = sp_group), se = T,
                              alpha = 0.1) +
                  labs(x = "NDVI", y = "Species Richness") +
                  theme_thesis + 
                  theme(plot.margin = margin(t = 0.5, r = 0.5, l = 0.05, b = 0.05, unit = "cm")) + 
                  facet_wrap(~aspect2) +
                  scale_color_manual(values = c("#3A69B4", "#BFBF88", "#867441", "#73A6E7", 
                                                         "#524B49"),
                                                         labels = c("Graminoids", "Lichens", "Shrubs", 
                                                                    "Bryophytes", "Forbs"),
                                     name = "Species Group") +
                  scale_fill_manual(values = c("#3A69B4", "#BFBF88", "#867441", "#73A6E7", 
                                                        "#524B49"),
                                                        labels = c("Graminoids", "Lichens", "Shrubs", 
                                                                   "Bryophytes", "Forbs"),
                                    name = "Species Group"))

# ggsave("Figures/group_ndvi.png", plot = group_plot5, width = 5.5, height = 4, units = "in")


## Combined plots ----
(group_grid <- ggarrange(group_plot, nrow = 2, ncol = 1, labels = c("A"), heights = c(1, 1),
                         ggarrange(nrow = 1, ncol = 2, legend = F,
                                   group_plot5, group_plot3, labels = c("B", "C"))))

ggsave("Figures/true_richness.png", plot = group_grid, width = 7, height = 6.5, units = "in")


# . ----
#### Landscape ~ Grazing Plot ----
gw_glm2 <- glm(formula = grazing_m ~ wetness, family = Gamma(link = "log"), 
               data = plot_grazing)

gw_pred <- ggpredict(gw_glm2, terms = c("wetness"))

(land_plot <- ggplot(gw_pred, aes(x, predicted)) +
                geom_point(data = plot_grazing, aes(x = wetness, y = grazing_m), color = "#4C4443",
                           fill = "#4C4443", alpha = 0.5, size = 1) +
                geom_line(color = "#4C4443", size = 0.5) +
                geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.4, fill = "#4C4443") +
                xlab("Soil Wetness") +
                ylab("Grazing (min)") +
                theme_thesis)
  
ggsave("Figures/interaction_plot.png", plot = land_plot, width = 4, height = 4, units = "in")

# . ----
#### Coverage Plots ----
coverage_site <- coverage_site %>% 
                    mutate(aspect2 = case_when(aspect == "N" ~ "North",
                                               aspect == "S" ~ "South")) %>% 
                    mutate(sp_group = factor(sp_group, 
                                             levels = c("grass", "lichen", "shrub", "moss", "herb"),
                                             labels = c("Graminoids", "Lichens", "Shrubs", "Bryophytes", 
                                                        "Forbs")))
# Boxplot 
(cov_box <- ggplot(coverage_site, aes(x = aspect2, y = coverage_perc)) +   
              geom_boxplot(aes(fill = sp_group, color = sp_group), alpha = 0.6) +
              theme_thesis + 
              theme(plot.margin = margin(r = 0.5, b = 0.5, t = 0.5, unit = "cm")) + 
              labs(x = "Aspect", y = "Coverage (%)") +
              scale_color_manual(values = c("#3A69B4", "#BFBF88", "#867441", "#73A6E7", 
                                                     "#524B49"),
                                                     labels = c("Graminoids", "Lichens", "Shrubs", 
                                                                "Bryophytes", "Forbs"),
                                 name = "Species Group") +
              scale_fill_manual(values = c("#3A69B4", "#BFBF88", "#867441", "#73A6E7", 
                                                    "#524B49"),
                                                    labels = c("Graminoids", "Lichens", "Shrubs", 
                                                               "Bryophytes", "Forbs"),
                                name = "Species Group"))

ggsave("Figures/coverage_boxplot.png", plot = cov_box, width = 5.3, height = 3.8, units = "in")

## Coverage ~ Grazing + NDVI + Wetness + Slope ----
# Grazing 
(cov_graze <- ggplot(coverage_site, aes(x = grazing_m, y = coverage_perc)) +  
                stat_smooth(method = "lm", aes(color = sp_group, fill = sp_group), se = T,
                            alpha = 0.1) +
                labs(x = "Grazing (min)", y = "Coverage (%)") +
                theme_thesis + 
                facet_wrap(~aspect2) +
                scale_color_manual(values = c("#3A69B4", "#BFBF88", "#867441", "#73A6E7", 
                                                       "#524B49"),
                                                       labels = c("Graminoids", "Lichens", "Shrubs", 
                                                                  "Bryophytes", "Forbs"),
                                   name = "Species Group") +
                scale_fill_manual(values = c("#3A69B4", "#BFBF88", "#867441", "#73A6E7", 
                                                      "#524B49"),
                                                      labels = c("Graminoids", "Lichens", "Shrubs", 
                                                                 "Bryophytes", "Forbs"),
                                  name = "Species Group"))

# NDVI
(cov_ndvi <- ggplot(coverage_site, aes(x = ndvi, y = coverage_perc)) +  
                stat_smooth(method = "lm", aes(color = sp_group, fill = sp_group), se = T,
                            alpha = 0.1) +
                labs(x = "NDVI", y = "Coverage (%)") +
                theme_thesis + 
                theme(plot.margin = margin(l = 0.5, b = 0.5, t = 0.5, unit = "cm")) + 
            #    theme(plot.margin = margin(r = 0.5, b = 0.5, l = 0.05, t = 0.05, unit = "cm")) +
                                                  # for 'cov_grid'
                facet_wrap(~aspect2) +
                scale_color_manual(values = c("#3A69B4", "#BFBF88", "#867441", "#73A6E7", 
                                                       "#524B49"),
                                                       labels = c("Graminoids", "Lichens", "Shrubs", 
                                                                  "Bryophytes", "Forbs"),
                                   name = "Species Group") +
                scale_fill_manual(values = c("#3A69B4", "#BFBF88", "#867441", "#73A6E7", 
                                                      "#524B49"),
                                                      labels = c("Graminoids", "Lichens", "Shrubs", 
                                                                 "Bryophytes", "Forbs"),
                                  name = "Species Group"))

# Wetness
(cov_wet <- ggplot(coverage_site, aes(x = wetness, y = coverage_perc)) +  
              stat_smooth(method = "lm", aes(color = sp_group, fill = sp_group), se = T,
                          alpha = 0.1) +
              labs(x = "Soil Wetness", y = "Coverage (%)") +
              theme_thesis + 
              theme(plot.margin = margin(r = 0.5, t = 0.5, unit = "cm")) + 
            #  theme(plot.margin = margin(l = 0.5, b = 0.5, r = 0.05, t = 0.05, unit = "cm")) +
                                                  # for 'cov_grid'
              facet_wrap(~aspect2) +
              scale_color_manual(values = c("#3A69B4", "#BFBF88", "#867441", "#73A6E7", 
                                                     "#524B49"),
                                                     labels = c("Graminoids", "Lichens", "Shrubs", 
                                                                "Bryophytes", "Forbs"),
                                 name = "Species Group") +
              scale_fill_manual(values = c("#3A69B4", "#BFBF88", "#867441", "#73A6E7", 
                                                    "#524B49"),
                                                    labels = c("Graminoids", "Lichens", "Shrubs", 
                                                               "Bryophytes", "Forbs"),
                                name = "Species Group"))

# Slope 
(cov_slope <- ggplot(coverage_site, aes(x = slope_deg, y = coverage_perc)) +  
                stat_smooth(method = "lm", aes(color = sp_group, fill = sp_group), se = T,
                            alpha = 0.1) +
                labs(x = "Slope Angle (˚)", y = "Coverage (%)") +
                theme_thesis + 
                theme(plot.margin = margin(l = 0.5, t = 0.5, unit = "cm")) + 
             #   theme(plot.margin = margin(t = 0.5, l = 0.05, r = 0.05, b = 0.05, unit = "cm"),
              #        legend.margin = margin(l = 4.2, t = 3.2, unit = "cm")) +
                                                  # for 'cov_grid'
                facet_wrap(~aspect2) +
                scale_color_manual(values = c("#3A69B4", "#BFBF88", "#867441", "#73A6E7", 
                                                       "#524B49"),
                                                       labels = c("Graminoids", "Lichens", "Shrubs", 
                                                                  "Bryophytes", "Forbs"),
                                   name = "Species Group") +
                scale_fill_manual(values = c("#3A69B4", "#BFBF88", "#867441", "#73A6E7", 
                                                      "#524B49"),
                                                      labels = c("Graminoids", "Lichens", "Shrubs", 
                                                                 "Bryophytes", "Forbs"),
                                  name = "Species Group"))

## Combined plots ----
(cov_grid <- ggarrange(ggarrange(cov_ndvi, cov_wet, ncol = 2, legend = F, labels = c("A", "B")), 
                       nrow = 2, heights = c(1, 1),
                       ggarrange(cov_slope, labels = c("C"))))

# with boxplot 
(cov_grid2 <- ggarrange(cov_box, cov_ndvi, cov_wet, cov_slope,
                        ncol = 2, nrow = 2, common.legend = T, labels = c("A", "B", "C", "D"), 
                        heights = c(1, 1)))

# ggsave("Figures/coverage.png", plot = cov_grid, width = 6, height = 6, units = "in")
ggsave("Figures/coverage_full.png", plot = cov_grid2, width = 6, height = 6, units = "in")


# . ----
#### NMDS Plot (Plots) ----
### Matrix preparation 
sp_cov2 <- left_join(plots, coverage)
sp_cov2 <- sp_cov2 %>% 
              dplyr::select(site_nr, plot_nr, plot_rep, aspect, sp_latin, sp_group, coverage_perc) %>% 
              group_by(plot_nr, sp_group) %>% 
              mutate(rel_abund = coverage_perc/length(sp_group)) %>% 
              ungroup() %>% 
              na.omit() %>% 
              dplyr::select(!c(coverage_perc, sp_group))

# matrix 
sp_matrix3 <- sp_cov2 %>% 
                distinct() %>% 
                pivot_wider(names_from = "sp_latin", values_from = "rel_abund", 
                            values_fill = NA)

sp_matrix3[is.na(sp_matrix3)] <- 0  # making sure non-existent sp just have coverage of 0

# matrix with only abundance data 
sp_matrix4 <- sp_matrix3 %>% 
                dplyr::select(!c(site_nr, plot_nr, plot_rep, aspect)) 


### Running the NMDS
set.seed(7)
nmds7 <- metaMDS(sp_matrix4, distance = "bray", k = 7, autotransform = F, trymax = 500, 
                 sratmax = 0.99999)  
nmds7  # stress = 0.06575167 

# extracting NMDS scores (x and y coordinates)
data.scoresb <- as.data.frame(scores(nmds7))
species_scoresb <- as.data.frame(scores(nmds7, "species"))
species_scoresb$species <- rownames(species_scoresb)

# preparing other important variables 
gisvar2 <- gisvar %>% 
              filter(!(plot_nr == 15)) %>% dplyr::select(!c(X, Y))
gisvar_long <- left_join(gisvar2, sites)  
gisvar_long <- gisvar_long %>% 
                  mutate(grazing_m = grazing_s/60)
  
data.scoresb$aspect <- sp_matrix3$aspect
data.scoresb$plot_nr <- sp_matrix3$plot_nr
data.scoresb$site_nr <- sp_matrix3$site_nr
data.scoresb$plot_rep <- sp_matrix3$plot_rep
data.scoresb$grazing_m <- gisvar_long$grazing_m


NMDSplot <- data.frame(NMDS1 = nmds7$points[,1], NMDS2 = nmds7$points[,2], 
                       group = data.scoresb$aspect)
NMDSplot$group <- as.factor(NMDSplot$group)

NMDSplot_mean <- aggregate(NMDSplot[,1:2], list(group = NMDSplot$group), "mean")


## Making the ellipsoids
# function for ellipses
veganCovEllipse <- function (cov, center = c(0, 0), scale = 1, npoints = 100) 
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}  # run from here

plot(nmds7)
ord2 <- ordiellipse(nmds7, data.scoresb$aspect, label = T, conf = 0.95)
dev.off()

df_ell2 <- data.frame()   # run from here (this side)

for(g in levels(NMDSplot$group)){
  df_ell2 <- rbind(df_ell2, 
                  cbind(as.data.frame(with(NMDSplot[NMDSplot$group==g,],
                                           veganCovEllipse(ord2[[g]]$cov, ord2[[g]]$center,
                                                           ord2[[g]]$scale)))
                        ,group=g))
}


## GIS variables 
# not standardized 
gisvar_plots <- gisvar_long %>% 
                    dplyr::select(ndvi, slope_deg, wetness, soil_depth, grazing_m)

gisfit3 <- envfit(nmds7, gisvar_plots, permu = 999)
gisfit3 

gisarrows2 <- as.data.frame(scores(gisfit3, "vectors") * ordiArrowMul(gisfit3))
gisarrows2

# editing row names 
gisarrows2 <- gisarrows2 %>% 
                mutate(varnames = c("NDVI", "Slope angle", "Soil wetness", "Soil depth",
                                 "Grazing duration")) %>% 
                mutate(NMDS1_nudge = c(-1.25, -1.14, 0.78, 0.20, 0.60),
                       NMDS2_nudge = c(-0.69, 0.85, -1.07, -0.23, -0.13))


## NMDS Plots
# normal  
(nmds_plot <- ggplot(data.scoresb, aes(x = NMDS1, y = NMDS2)) + 
                  geom_polygon(data = df_ell2, aes(x = NMDS1, y = NMDS2, group = group,
                                                   color = group, fill = group, linetype = group), 
                               alpha = 0.2, size = 0.5) +
                  geom_point(aes(color = aspect, shape = aspect, fill = aspect), 
                             size = 1.5, alpha = 0.5) +
                  annotate("text", x = -1, y = -1.3, label = "Stress = 0.066", color = "grey20",
                           size = 3.5, fontface = "italic") +
                  geom_segment(aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), 
                               data = gisarrows2, size = 0.5, color = "grey20",
                               arrow = arrow(length = unit(0.25, "cm"))) +
                  geom_text(data = gisarrows2, aes(x = NMDS1_nudge, y = NMDS2_nudge, 
                                                   label = varnames), size = 3.5, color = "grey20") + 
                  theme_thesis + 
                  labs(x = "NMDS1", y = "NMDS2") +
                  scale_color_manual(values = c("#FF4B0A", "#FFA033"),
                                     labels = c("North", "South"),
                                     name = "Aspect") +
                  scale_shape_manual(values = c(21, 22),
                                     labels = c("North", "South"),
                                     name = "Aspect") +
                  scale_fill_manual(values = c("#FF4B0A", "#FFA033"),
                                    labels = c("North", "South"),
                                    name = "Aspect") +
                  scale_linetype_manual(values = c("dashed", "solid"),
                                        labels = c("North", "South"),
                                        name = "Aspect") +
                  xlim(c(-1.4, 1.4)) +
                  ylim(c(-1.4, 1.4)))

ggsave("Figures/nmds.png", plot = nmds_plot, width = 5.5, height = 5, units = "in")

# points colored by grazing (not using!!)
(nmds_plot2 <- ggplot(data.scoresb, aes(x = NMDS1, y = NMDS2)) + 
                  geom_polygon(data = df_ell2, aes(x = NMDS1, y = NMDS2, group = group,
                                                   fill = group), alpha = 0.2, 
                               size = 0.5, linetype = 1) +
                  geom_point(aes(color = grazing_m), size = 2, alpha = 0.6) +
                  #   geom_text(data = species_scores, aes(x = NMDS1, y = NMDS2, label = species),
                  #            alpha = 0.5, size = 2) +
                  geom_segment(aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), 
                               data = gisarrows2, size = 1, alpha = 0.7, colour = "grey40",
                               arrow = arrow(length = unit(0.25, "cm"))) +
                  geom_text(data = gisarrows2, aes(x = NMDS1, y = NMDS2), colour = "grey40", 
                            label = row.names(gisarrows2)) + 
                  theme_thesis + 
                  labs(x = "NMDS1", y = "NMDS2") +
                  scale_fill_manual(values = c("#458264", "#F4A460"),
                                     labels = c("N", "S"),
                                     name = "Aspect") +
                  scale_shape_manual(values = c(15, 16),
                                     labels = c("N", "S"),
                                     name = "Aspect"))

## Extra NMDS Plots ----
# to be able to see what sites and plots points belong to 
data.scoresb <- data.scoresb %>% 
                  mutate(site_nr2 = as.numeric(site_nr)) %>% 
                  mutate(plot_rep2 = case_when(plot_rep == "1" ~ "A",
                                               plot_rep == "2" ~ "B",
                                               plot_rep == "3" ~ "C")) %>% 
                  mutate(site_plot = str_c(site_nr, "", plot_rep2)) 

# making ellipsoids per site
plot(nmds7)
ord_site <- ordiellipse(nmds7, data.scoresb$site_nr3, label = T, conf = 0.95)
dev.off()
?ordiellipse

# with plot and site names as points (for variance visualization)
(nmds_plot3 <- ggplot(data.scoresb, aes(x = NMDS1, y = NMDS2)) + 
                  geom_polygon(data = df_ell, aes(x = NMDS1, y = NMDS2, group = group, fill = group),
                               size = 0.5, linetype = 1, alpha = 0.2) +
                  geom_text(aes(label = site_plot, color = site_nr2), size = 3) +
                  geom_segment(aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), 
                               data = gisarrows2, size = 1, alpha = 0.7, colour = "grey40",
                               arrow = arrow(length = unit(0.25, "cm"))) +
                  geom_text(data = gisarrows2, aes(x = NMDS1, y = NMDS2), colour = "grey40", 
                            label = row.names(gisarrows2)) + 
                  theme_thesis + 
                  labs(x = "NMDS1", y = "NMDS2") +
                  scale_color_viridis_c(option = "plasma") +
                  scale_fill_manual(values = c("#458264", "#F4A460"),
                                    labels = c("N", "S"),
                                    name = "Aspect"))

# without aspect
(nmds_plot4 <- ggplot(data.scoresb, aes(x = NMDS1, y = NMDS2)) + 
                  geom_text(aes(label = site_plot, color = site_nr2), size = 3) +
                  #   geom_text(data = species_scores, aes(x = NMDS1, y = NMDS2, label = species),
                  #            alpha = 0.5, size = 2) +
                  geom_segment(aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), 
                               data = gisarrows2, size = 1, alpha = 0.7, colour = "grey20",
                               arrow = arrow(length = unit(0.25, "cm"))) +
                  geom_text(data = gisarrows2, aes(x = NMDS1, y = NMDS2), colour = "grey20", 
                            label = row.names(gisarrows2)) + 
                  theme_thesis + 
                  labs(x = "NMDS1", y = "NMDS2")) +
                  scale_color_viridis_c(option = "plasma")


# . ----
# . ----
#### OLD PLOTS #### ----
#### Coverage Plots ----
## Coverage ~ grazing
(cov_plot <- ggplot(coverage_site, aes(x = grazing_m, y = coverage_perc)) +  
   geom_point(aes(color = sp_group, fill = sp_group)) +
   stat_smooth(method = "lm", se = T, aes(color = sp_group, fill = sp_group)) +
   facet_wrap(~aspect) +
   theme_thesis + 
   labs(x = "Grazing (min)", y = "Coverage (%)"))

## Coverage ~ aspect (boxplot)
(cov_boxplot <- ggplot(coverage_site, aes(x = aspect, y = coverage_perc)) +  
    geom_boxplot(aes(color = sp_group, fill = sp_group), alpha = 0.5) +
    theme_thesis + 
    labs(x = "Aspect", y = "Coverage (%)"))


#### Height Plot ----
(height_plot <- ggplot(richness_site, aes(x = grazing_m, y = height_site)) +   
   geom_point(aes(color = aspect, fill = aspect, shape = aspect)) +
   stat_smooth(method = "lm", aes(color = aspect, fill = aspect)) +
   theme_thesis + 
   labs(x = "Grazing (min)", y = "Height (cm)") +
   scale_color_manual(values = c("#458264", "#F4A460"),
                      labels = c("N", "S"),
                      name = "Aspect") +
   scale_shape_manual(values = c(21, 22),
                      labels = c("N", "S"),
                      name = "Aspect") +
   scale_fill_manual(values = c("#458264", "#F4A460"),
                     labels = c("N", "S"),
                     name = "Aspect"))


#### NMDS Plot (Site) ----
### Matrix preparation 
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

# matrix 
sp_matrix <- sp_cov %>% 
  distinct() %>% 
  pivot_wider(names_from = "sp_latin", values_from = "rel_abund2", 
              values_fill = NA)  # get a warning, but no duplicates found

sp_matrix[is.na(sp_matrix)] <- 0  # making sure non-existent sp just have coverage of 0

# matrix with only coverage data 
sp_matrix2 <- sp_matrix %>% 
  dplyr::select(!c(site_nr, aspect)) 


### Running the NMDS
set.seed(6)
nmds6 <- metaMDS(sp_matrix2, distance = "bray", k = 6, autotransform = F, trymax = 500, 
                 sratmax = 0.99999)  

# extracting NMDS scores (x and y coordinates)
data.scores <- as.data.frame(scores(nmds6))
species_scores <- as.data.frame(scores(nmds6, "species"))
species_scores$species <- rownames(species_scores)

data.scores$aspect <- sp_matrix$aspect

NMDS <- data.frame(NMDS1 = nmds6$points[,1], NMDS2 = nmds6$points[,2], 
                   group = data.scores$aspect)
NMDS$group <- as.factor(NMDS$group)

NMDS_mean <- aggregate(NMDS[,1:2], list(group = NMDS$group), "mean")


## Making the ellipsoids
# function for ellipses
veganCovEllipse <- function (cov, center = c(0, 0), scale = 1, npoints = 100) 
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}  # run from here

plot(nmds6)
ord <- ordiellipse(nmds6, data.scores$aspect, label = T, conf = 0.95)
dev.off()

df_ell <- data.frame()   # run from here (this side)

for(g in levels(NMDS$group)){
  df_ell <- rbind(df_ell, 
                  cbind(as.data.frame(with(NMDS[NMDS$group==g,],
                                           veganCovEllipse(ord[[g]]$cov, ord[[g]]$center,
                                                           ord[[g]]$scale)))
                        ,group=g))
}  # run from here 


## GIS variables 
# removing unnecessary info from GIS data 
gisvar2 <- gisvar %>% 
  filter(!(plot_nr == 15)) %>% dplyr::select(!c(X, Y))

# making environmental variable vectors for the site 
gisvar_long <- left_join(gisvar2, sites)  

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

gisarrows <- as.data.frame(scores(gisfit, "vectors")) * ordiArrowMul(gisfit)
# use if significant environmental variables 


## NMDS Plot
(nmds_plot <- ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
    geom_polygon(data = df_ell, aes(x = NMDS1, y = NMDS2, group = group,
                                    color = group, fill = group), alpha = 0.2, 
                 size = 0.5, linetype = 1) +
    geom_point(aes(color = aspect, shape = aspect, fill = aspect), 
               size = 2, alpha = 0.6) +
    #   geom_text(data = species_scores, aes(x = NMDS1, y = NMDS2, label = species),
    #            alpha = 0.5, size = 2) +
    geom_segment(aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), 
                 data = gisarrows, size = 1, alpha = 0.7, colour = "grey40",
                 arrow = arrow(length = unit(0.25, "cm"))) +
    geom_text(data = gisarrows, aes(x = NMDS1, y = NMDS2), colour = "grey40", 
              label = row.names(gisarrows)) + 
    theme_thesis + 
    labs(x = "NMDS1", y = "NMDS2") +
    scale_color_manual(values = c("#458264", "#F4A460"),
                       labels = c("N", "S"),
                       name = "Aspect") +
    scale_shape_manual(values = c(21, 22),
                       labels = c("N", "S"),
                       name = "Aspect") +
    scale_fill_manual(values = c("#458264", "#F4A460"),
                      labels = c("N", "S"),
                      name = "Aspect"))

