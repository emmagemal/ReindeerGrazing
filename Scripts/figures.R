## Data Visualization (Figures) ##
## For SU Master's Thesis in Landscape Ecology
## Emma Gemal
## Last updated 10/2/23

### Library ----
library(tidyverse)
library(viridis)


#### Thesis Plot Theme ---- 
theme_thesis <- theme_bw() +
                  theme(panel.grid = element_blank(),
                        axis.title.x = 
                          element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
                        axis.title.y = 
                          element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
                  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))


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
              mutate(rich_propT = mean(rich_group_prop))

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
grazing <- sites %>% dplyr::select(site_nr, plot_nr, plot, aspect, grazing_s, grazing_cat)
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
#### Richness Plots ----
## Richness per site ~ grazing 
(rich_plot <- ggplot(richness_site, aes(x = grazing_m, y = richness_siteT)) +   
                geom_point(aes(color = aspect, shape = aspect, fill = aspect)) +
                stat_smooth(method = "lm", aes(color = aspect, fill = aspect)) +
                theme_thesis + 
                labs(x = "Grazing (min)", y = "Species Richness") +
                scale_color_manual(values = c("#458264", "#F4A460"),
                                   labels = c("N", "S"),
                                   name = "Aspect") +
                scale_shape_manual(values = c(21, 22),
                                   labels = c("N", "S"),
                                   name = "Aspect") +
                scale_fill_manual(values = c("#458264", "#F4A460"),
                                  labels = c("N", "S"),
                                  name = "Aspect"))

## Richness per site ~ NDVI
(rich_ndvi <- ggplot(richness_site, aes(x = ndvi, y = richness_siteT)) +   
                geom_point(aes(color = aspect, shape = aspect, fill = aspect)) +
                stat_smooth(method = "lm", aes(color = aspect, fill = aspect)) +
                theme_thesis + 
                labs(x = "NDVI", y = "Species Richness") +
                scale_color_manual(values = c("#458264", "#F4A460"),
                                   labels = c("N", "S"),
                                   name = "Aspect") +
                scale_shape_manual(values = c(21, 22),
                                   labels = c("N", "S"),
                                   name = "Aspect") +
                scale_fill_manual(values = c("#458264", "#F4A460"),
                                  labels = c("N", "S"),
                                  name = "Aspect"))

## Richness per site ~ wetness
(rich_wet <- ggplot(richness_site, aes(x = wetness, y = richness_siteT)) +   
                geom_point(aes(color = aspect, shape = aspect, fill = aspect)) +
                stat_smooth(method = "lm", aes(color = aspect, fill = aspect)) +
                theme_thesis + 
                labs(x = "Soil Wetness", y = "Species Richness") +
                scale_color_manual(values = c("#458264", "#F4A460"),
                                   labels = c("N", "S"),
                                   name = "Aspect") +
                scale_shape_manual(values = c(21, 22),
                                   labels = c("N", "S"),
                                   name = "Aspect") +
                scale_fill_manual(values = c("#458264", "#F4A460"),
                                  labels = c("N", "S"),
                                  name = "Aspect"))


## Richness per site ~ slope
(rich_slope <- ggplot(richness_site, aes(x = slope_deg, y = richness_siteT)) +   
                  geom_point(aes(color = aspect, shape = aspect, fill = aspect)) +
                  stat_smooth(method = "lm", aes(color = aspect, fill = aspect)) +
                  theme_thesis + 
                  labs(x = "Slope Angle (˚)", y = "Species Richness") +
                  scale_color_manual(values = c("#458264", "#F4A460"),
                                     labels = c("N", "S"),
                                     name = "Aspect") +
                  scale_shape_manual(values = c(21, 22),
                                     labels = c("N", "S"),
                                     name = "Aspect") +
                  scale_fill_manual(values = c("#458264", "#F4A460"),
                                    labels = c("N", "S"),
                                    name = "Aspect"))

## Richness per site ~ slope
(rich_depth <- ggplot(richness_site, aes(x = soil_depth, y = richness_siteT)) +   
                  geom_point(aes(color = aspect, shape = aspect, fill = aspect)) +
                  stat_smooth(method = "lm", aes(color = aspect, fill = aspect)) +
                  theme_thesis + 
                  labs(x = "Soil Depth (m)", y = "Species Richness") +
                  scale_color_manual(values = c("#458264", "#F4A460"),
                                     labels = c("N", "S"),
                                     name = "Aspect") +
                  scale_shape_manual(values = c(21, 22),
                                     labels = c("N", "S"),
                                     name = "Aspect") +
                  scale_fill_manual(values = c("#458264", "#F4A460"),
                                    labels = c("N", "S"),
                                    name = "Aspect"))


#### Richness Proportion Plot ----
(prop_plot <- ggplot(richness_site, aes(x = grazing_m, y = rich_propT)) +   
                geom_point(aes(color = sp_group, fill = sp_group)) +
                stat_smooth(method = "lm", aes(color = sp_group, fill = sp_group)) +   
                facet_wrap(~aspect) +
                theme_thesis + 
                labs(x = "Grazing (min)", y = "Species Richness"))


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

#### Coverage Plots ----
## Coverage ~ grazing
(cov_plot <- ggplot(coverage_site, aes(x = grazing_m, y = coverage_perc)) +  
                geom_point(aes(color = sp_group, fill = sp_group)) +
                stat_smooth(method = "lm", se = T, aes(color = sp_group, fill = sp_group)) +
                facet_wrap(~aspect) +
                theme_thesis + 
                labs(x = "Grazing (min)", y = "Coverage (%)"))

## Coverage ~ aspect (boxplot)
(cov_boxplot <-ggplot(coverage_site, aes(x = aspect, y = coverage_perc)) +  
                  geom_boxplot(aes(color = sp_group, fill = sp_group), alpha = 0.5) +
                  theme_thesis + 
                  labs(x = "Aspect", y = "Coverage (%)"))


# . ----
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

### Aspect ----
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


### Grazing Category ----
# making grazing categories per site instead of plot
grazecat <- sites %>% 
              mutate(grazing_cat = as.numeric(grazing_cat)) %>% 
              group_by(site_nr) %>% 
              summarize(grazecat = mean(grazing_cat)) %>% 
              mutate(grazecat = round(grazecat)) %>% 
              mutate(grazecat = as.character(grazecat))

# adding grazing to NMDS scores 
data.scores$grazing <- grazecat$grazecat

NMDSg <- data.frame(NMDS1 = nmds6$points[,1], NMDS2 = nmds6$points[,2], 
                   group = data.scores$grazing)
NMDSg$group <- as.factor(NMDSg$group)

NMDSg_mean <- aggregate(NMDSg[,1:2], list(group = NMDSg$group), "mean")


## Making the ellipsoids
ordg <- ordiellipse(nmds6, data.scores$grazing, label = T, conf = 0.95)

df_ellg <- data.frame()   # run from here (this side)

for(g in levels(NMDSg$group)){
  df_ellg <- rbind(df_ellg, 
                  cbind(as.data.frame(with(NMDSg[NMDSg$group==g,],
                                           veganCovEllipse(ordg[[g]]$cov, ordg[[g]]$center,
                                                           ordg[[g]]$scale)))
                        ,group=g))
}  # run from here 


## NMDS Plot
(nmds_plot2 <- ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
                  geom_polygon(data = df_ellg, aes(x = NMDS1, y = NMDS2, group = group,
                                                  color = group, fill = group), alpha = 0.2, 
                               size = 0.5, linetype = 1) +
                  geom_point(aes(color = grazing, fill = grazing), 
                             size = 2, alpha = 0.6) +
                  #   geom_text(data = species_scores, aes(x = NMDS1, y = NMDS2, label = species),
                  #            alpha = 0.5, size = 2) +
                  theme_thesis + 
                  labs(x = "NMDS1", y = "NMDS2"))


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

### Aspect ----
# extracting NMDS scores (x and y coordinates)
data.scoresb <- as.data.frame(scores(nmds7))
species_scoresb <- as.data.frame(scores(nmds7, "species"))
species_scoresb$species <- rownames(species_scoresb)

data.scoresb$aspect <- sp_matrix3$aspect
data.scoresb$plot_nr <- sp_matrix3$plot_nr
data.scoresb$site_nr <- sp_matrix3$site_nr
data.scoresb$plot_rep <- sp_matrix3$plot_rep


NMDSplot <- data.frame(NMDS1 = nmds7$points[,1], NMDS2 = nmds7$points[,2], 
                       group = data.scoresb$aspect)
NMDSplot$group <- as.factor(NMDSplot$group)

NMDSplot_mean <- aggregate(NMDSplot[,1:2], list(group = NMDSplot$group), "mean")


## Making the ellipsoids
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
gisvar_plots <- gisvar_long %>% dplyr::select(ndvi, slope_deg, wetness, soil_depth) 

gisfit3 <- envfit(nmds7, gisvar_plots, permu = 999)
gisfit3 

# standardized 
gisvar_plots2 <- gisvar_long %>% dplyr::select(ndvi_s, slope_deg_s, wetness_s, soil_depth_s)

gisfit4 <- envfit(nmds7, gisvar_plots2, permu = 999)
gisfit4

gisarrows2 <- as.data.frame(scores(gisfit4, "vectors") * ordiArrowMul(gisfit4))


## NMDS Plot
# to be able to see what sites and plots points belong to 
data.scoresb <- data.scoresb %>% 
                  mutate(site_nr2 = as.numeric(site_nr)) %>% 
                  mutate(plot_rep2 = case_when(plot_rep == "1" ~ "A",
                                               plot_rep == "2" ~ "B",
                                               plot_rep == "3" ~ "C")) %>% 
                  mutate(site_plot = str_c(site_nr, "", plot_rep2))


(nmds_plot3 <- ggplot(data.scoresb, aes(x = NMDS1, y = NMDS2)) + 
                  geom_polygon(data = df_ell2, aes(x = NMDS1, y = NMDS2, group = group,
                                                   color = group, fill = group), alpha = 0.2, 
                               size = 0.5, linetype = 1) +
                  geom_point(aes(color = aspect, shape = aspect, fill = aspect), 
                             size = 2, alpha = 0.6) +
                  #   geom_text(data = species_scores, aes(x = NMDS1, y = NMDS2, label = species),
                  #            alpha = 0.5, size = 2) +
                  geom_segment(aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), 
                               data = gisarrows2, size = 1, alpha = 0.7, colour = "grey40",
                               arrow = arrow(length = unit(0.25, "cm"))) +
                  geom_text(data = gisarrows2, aes(x = NMDS1, y = NMDS2), colour = "grey40", 
                            label = row.names(gisarrows2)) + 
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

# with plot and site names as points 
(nmds_plot3b <- ggplot(data.scoresb, aes(x = NMDS1, y = NMDS2)) + 
                  geom_polygon(data = df_ell2, aes(x = NMDS1, y = NMDS2, group = group,
                                                   fill = group),
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
