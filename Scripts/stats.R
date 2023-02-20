## Data Exploration and Statistics ##
## For SU Master's Thesis in Landscape Ecology
## Emma Gemal
## Last updated 10/2/23

#### Library ----
library(MASS)   # for Box-Cox transformation 
library(tidyverse)
library(lmtest)
library(lme4)
library(plotrix)  # for 'std.error()'
library(vegan)

#### Data Import ----
sites <- read.csv("Data/site_conditions.csv")
plots <- read.csv("Data/plots_tidy.csv")
coverage <- read.csv("Data/coverage.csv")

#### Data Exploration & Cleaning ----
view(sites)
view(plots)
view(coverage)

str(sites)   # change date to date, plot to cat, and maybe x and y coord to coordinates
str(plots)   # change date to date and check for spelling mistakes and duplicates
str(coverage)   # change date to date

### Editing import format  
sites <- sites %>% 
            mutate(date = as.Date(date, format = "%d/%m/%y"),
                   plot = as.character(plot)) %>% 
            mutate(aspect = case_when(grepl("S", site) ~ "S",
                                      grepl("N", site) ~ "N"),
                   grazing_cat = substr(site, 1, 1))   # adding aspect
str(sites)            

plots <- plots %>% 
            mutate(date = as.Date(date, format = "%d/%m/%y"))
str(plots)

coverage <- coverage %>%  
              mutate(date = as.Date(date, format = "%d/%m/%y"),
                     plot_rep = as.character(plot_rep))
str(coverage) 

### Checking species 
sort(unique(plots$sp_common))  # remove whitespace after words 
                               # "purple mountain heather" & "purple mountain-heath" 

sort(unique(plots$sp_latin))  # some don't have a latin name... not good 
                              # remove whitespace after words
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
#### Variable Calculations ----
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
                        

# combining 'richness' with 'sites' to add grazing data 
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
coverage_site <- coverage %>% 
                    group_by(site_nr, sp_group, aspect, grazing_cat) %>% 
                    summarize(coverage_cm2 = mean(coverage_cm2),
                              coverage_perc = mean(coverage_perc),
                              coverage_prop = coverage_perc/100,
                              grazing_s = mean(grazing_s),
                              grazing_m = mean(grazing_m))
coverage_site <- as.data.frame(coverage_site)
str(coverage_site)


# . ----
#### Initial Visualizations ----
## Richness
ggplot(richness, aes(x = grazing_m, y = richness_plot)) +   # richness with grazing 
  geom_point()

ggplot(richness, aes(x = grazing_m, y = richness_plot)) +   # N and S accounted for 
  geom_point(aes(color = aspect)) +
  stat_smooth(method = "lm", aes(color = aspect))

ggplot(richness, aes(x = grazing_m, y = richness_siteT)) +   # averaged across the plots
  geom_point()

# will use this one for final 
ggplot(richness, aes(x = grazing_m, y = richness_siteT)) +   # averaged and aspect accounted for
  geom_point(aes(color = aspect)) +
  stat_smooth(method = "lm", aes(color = aspect))

ggplot(richness, aes(x = grazing_m, y = rich_group_prop)) +   # how proportions of groups changes
  geom_point(aes(color = sp_group)) +
  stat_smooth(method = "lm", aes(color = sp_group))

ggplot(richness, aes(x = grazing_m, y = rich_group_prop)) +   # aspect accounted for 
  geom_point(aes(color = sp_group)) +
  stat_smooth(method = "lm", aes(color = sp_group)) +   
  facet_wrap(~aspect)

ggplot(richness, aes(x = grazing_m, y = rich_propT)) +   # proportions of groups per site
  geom_point(aes(color = sp_group)) +
  stat_smooth(method = "lm", aes(color = sp_group))

ggplot(richness, aes(x = grazing_m, y = rich_propT)) +   # averaged and aspect 
  geom_point(aes(color = sp_group)) +
  stat_smooth(method = "lm", aes(color = sp_group)) +   
  facet_wrap(~aspect)

ggplot(richness, aes(x = aspect, y = rich_group_prop)) +   # relative species nr for each group 
  geom_boxplot(aes(fill = sp_group))                       # for N vs. S 

ggplot(richness, aes(x = aspect, y = richness_siteT)) +   # averaged richness N vs. S
  geom_boxplot()

## Coverage
ggplot(coverage_site, aes(x = grazing_m, y = coverage_perc)) +  # coverage of groups with grazing 
  geom_point(aes(color = sp_group)) +
  stat_smooth(method = "lm", se = T, aes(color = sp_group, fill = sp_group), alpha = 0.3)

ggplot(coverage_site, aes(x = grazing_m, y = coverage_perc)) +  # differences for N vs. S
  geom_point(aes(color = sp_group)) +
  stat_smooth(method = "lm", se = T, aes(color = sp_group, fill = sp_group)) +
  facet_wrap(~aspect)
 
ggplot(coverage_site, aes(x = sp_group, y = coverage_perc)) +  # difference in coverage per group
  geom_boxplot()

ggplot(coverage_site, aes(x = aspect, y = coverage_perc)) +   # coverage differences N vs. S
  geom_boxplot(aes(fill = sp_group))

## Veg height
ggplot(richness, aes(x = grazing_m, y = height_site)) +  # height with grazing 
  geom_point() +
  stat_smooth(method = "lm")

ggplot(richness, aes(x = grazing_m, y = height_site)) +   # height of N vs. S plots 
  geom_point(aes(color = aspect)) +
  stat_smooth(method = "lm", aes(color = aspect))

ggplot(richness, aes(x = aspect, y = height_site)) +
  geom_boxplot()

# . ----
#### Models ----
### Richness ~ Grazing (Site) ----
# checking assumptions
hist(richness$richness_siteT)  # no clear distribution 
hist(richness$grazing_m)

rg1 <- lm(richness_siteT ~ grazing_m, data = richness)
plot(rg1)
shapiro.test(resid(rg1))   # p < 0.05 = not normal
bptest(rg1)  # p > 0.05 = no heteroskedasticity though 

## Transformations 
# log
rg1_log <- lm(log(richness_siteT) ~ grazing_m, data = richness)
shapiro.test(resid(rg1_log))   # p < 0.05 = not normal
bptest(rg1_log)  # p < 0.05 = and heteroskedasticity  

# sqrt
rg1_sqrt <- lm(sqrt(richness_siteT) ~ grazing_m, data = richness)
shapiro.test(resid(rg1_sqrt))   # p < 0.05 = not normal
bptest(rg1_sqrt)  # p < 0.05 = and heteroskedasticity  

## GLM 
rg_glm <- glm(richness_siteT ~ grazing_m, data = richness)
summary(rg_glm)
 
# calculating McFadden's R2 
with(summary(rg_glm), 1 - deviance/null.deviance)  # R2 = 0.027 (not a good model though)

### Models with aspect 
rga1 <- lm(richness_siteT ~ grazing_m + aspect, data = richness)
plot(rga1)
shapiro.test(resid(rga1))   # p < 0.05 = not normal
bptest(rga1)  # p > 0.05 = no heteroskedasticity though

rga_glm <- glm(richness_siteT ~ grazing_m + aspect, data = richness)
summary(rga_glm)       ## USE THIS MODEL'S RESULTS ##
  # grazing: -0.05083 ± 0.01435, p = 0.000434 (SIGNIFICANT)
  # aspect SOUTH: -2.34132 ± 0.2422, p = < 2e-16 (SIGNIFICANT)

# calculating McFadden's R2 
with(summary(rga_glm), 1 - deviance/null.deviance)  # R2 = 0.1863 (an ok model)

## Choosing the best model 
r_null <- glm(richness_siteT ~ 1, data = richness)

AIC(r_null, rg_glm, rga_glm)  # 'rga_glm' is the best 


### Richness ~ Grazing (Sp Group Proportions) ----
# do model with rich_group_prop ~ grazing_m + sp_group + aspect 
hist(richness$rich_propT)   # a bit skewed 

rpg1 <- lm(rich_propT ~ grazing_m, data = richness)
plot(rpg1)
shapiro.test(resid(rpg1))   # p < 0.05 = not normal
bptest(rpg1)  # p > 0.05 = no heteroskedasticity though 

rpg2 <- lm(rich_propT ~ grazing_m + sp_group, data = richness)
plot(rpg2)
shapiro.test(resid(rpg2))   # p < 0.05 = not normal
bptest(rpg2)  # p < 0.05 = and heteroskedasticity 

rpg3 <- lm(rich_propT ~ grazing_m*sp_group, data = richness)
plot(rpg3)
shapiro.test(resid(rpg3))   # p < 0.05 = not normal
bptest(rpg3)  # p < 0.05 = and heteroskedasticity 

## Transformations 
# log
rpg2_log <- lm(log(rich_propT) ~ grazing_m + sp_group, data = richness)
shapiro.test(resid(rpg2_log))   # p < 0.05 = not normal
bptest(rpg2_log)  # p < 0.05 = and heteroskedasticity
hist(resid(rpg2_log))

# sqrt
rpg2_sqrt <- lm(sqrt(rich_propT) ~ grazing_m + sp_group, data = richness)
shapiro.test(resid(rpg2_sqrt))   # p < 0.05 = not normal
bptest(rpg2_sqrt)  # p < 0.05 = and heteroskedasticity 
hist(resid(rpg2_sqrt))

## GLM
rpg_glm <- glm(rich_propT ~ grazing_m + sp_group, data = richness)
hist(resid(rpg_glm))  # looks pretty 'normal', can use Gaussian
summary(rpg_glm)

rpg_glm2 <- glm(rich_propT ~ grazing_m*sp_group, data = richness)
summary(rpg_glm2)

### Models with aspect 
rpga1 <- lm(rich_propT ~ grazing_m + sp_group + aspect, data = richness)
shapiro.test(resid(rpga1))   # p < 0.05 = not normal
bptest(rpga1)  # p < 0.05 = and heteroskedasticity 

rpga_glm <- glm(rich_propT ~ grazing_m + sp_group + aspect, data = richness)

rpga_glm2 <- glm(rich_propT ~ grazing_m + sp_group*aspect, data = richness)

rpga_glm3 <- glm(rich_propT ~ grazing_m*sp_group + aspect, data = richness)

rpga_glm4 <- glm(rich_propT ~ grazing_m*sp_group*aspect, data = richness)
summary(rpga_glm)      ## USE THIS MODEL'S RESULTS ##
  # grass N (intercept): 0.16437 ± 0.01224
  # grazing (grass) N: -0.00189 ± 0.000943, p = 0.045512
  # herb N: 0.011317 ± 0.01837, p = 0.538 
  # lichen N: 0.1264 ± 0.0173, p = 1.24e-12
  # moss N: 0.0286 ± 0.0173, p = 0.0987
  # shrub N: 0.0418 ± 0.0173, p = 0.01615
  # grass S: -0.028309 ± 0.01774, p = 0.111
  # herb S: 0.029646 ± 0.027358, p = 0.2791
  # lichen S: 0.07058 ± 0.02509, p = 0.00511
  # moss S: 0.00682 ± 0.025415, p = 0.78854
  # shrub S: 0.09161 ± 0.025089, p = 0.00029
  # grazing*herb N: 0.0004679 ± 0.00144, p = 0.74541
  # grazing*lichen N: 0.002539 ± 0.00133, p = 0.0576
  # grazing*moss N: 0.001904 ± 0.00133, p = 0.15423
  # grazing*shrub N: 0.004683 ± 0.00133, p = 0.000491
  # grazing (grass) S: 0.003534 ± 0.0013498, p = 0.009125
  # grazing*herb S: -0.004078 ± 0.002042, p = 0.04641
  # grazing*lichen S: -0.007002 ± 0.0019089, p = 0.000273
  # grazing*moss S: -0.00353 ± 0.001923, p = 0.06692
  # grazing*shrub S: -0.00635 ± 0.001909, p = 0.000943

## Choosing the best model 
rp_null <- glm(rich_propT ~ 1, data = richness)

AIC(rp_null, rpg_glm, rpg_glm2, rpga_glm, rpga_glm2, rpga_glm3, rpga_glm4)  # 'rpga_glm4' is the best 


### Height ~ Grazing ----
hist(richness$height_site)  # right skewed (positive skew)

hg1 <- lm(height_site ~ grazing_m, data = richness)
plot(hg1)
shapiro.test(resid(hg1))   # p < 0.05 = not normal (skewed)
bptest(hg1)  # p > 0.05 = no heteroskedasticity though 

## Transformations 
# log
hg2 <- lm(log(height_site) ~ grazing_m, data = richness)
plot(hg2)
shapiro.test(resid(hg2))   # p < 0.05 = not normal
bptest(hg2)  # p > 0.05 = no heteroskedasticity 

# sqrt 
hg3 <- lm(sqrt(height_site) ~ grazing_m, data = richness)
plot(hg3)
shapiro.test(resid(hg3))   # p < 0.05 = not normal, almost worse 
bptest(hg3)  # p > 0.05 = no heteroskedasticity 

# box-cox 
bc <- boxcox(hg1)
(lambda <- bc$x[which.max(bc$y)])  # extracting exact lambda for transformation 

hg4 <- lm(((height_site^lambda-1)/lambda) ~ grazing_m, data = richness)
plot(hg4)
shapiro.test(resid(hg4))   # p < 0.05 = not normal, nothing works 
bptest(hg4)  # p > 0.05 = no heteroskedasticity 

## GLM    
hg_glm <- glm(height_site ~ grazing_m, family = Gamma(link = log), data = richness)
hist(resid(hg_glm))  # without 'Gamma' distribution it wasn't super normal 
summary(hg_glm) 

# calculating McFadden's R2 
with(summary(hg_glm), 1 - deviance/null.deviance)  # R2 = 0.0030 (not a good model)

### Models with aspect 
hga1 <- lm(height_site ~ grazing_m + aspect, data = richness)
plot(hga1)
shapiro.test(resid(hga1))   # p < 0.05 = not normal 
bptest(hga1)  # p < 0.05 = and heteroskedasticity 

hga_glm <- glm(height_site ~ grazing_m + aspect, family = Gamma(link = log), data = richness)
hist(resid(hga_glm))

hga_glm2 <- glm(height_site ~ grazing_m*aspect, family = Gamma(link = log), data = richness)
hist(resid(hga_glm2))
summary(hga_glm2)        ## USE THIS MODEL'S RESULTS ##
  # grazing: 10^(-0.010242) ± 10^(0.004606), p = 0.026629 (SIGNIFICANT)
  # aspect SOUTH: 10^(-0.019415) ± 10^(0.087245, p = 0.823993 (not significant)
  # grazing*aspect SOUTH: 10^(0.024912) ± 10^(0.006588), p = 0.000176 (SIGNIFICANT)

# calculating McFadden's R2 
with(summary(hga_glm2), 1 - deviance/null.deviance)  # R2 = 0.06669 (not a good model)

## Choosing the best model 
h_null <- glm(height_site ~ 1, family = Gamma(link = log), data = richness)

AIC(h_null, hg_glm, hga_glm, hga_glm2)  # 'hga_glm2' is best


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

# calculating McFadden's R2 
with(summary(cg_glm), 1 - deviance/null.deviance)  # R2 = 0.6841018 (very good model)


### Models with aspect
cga1 <- lm(coverage_perc ~ grazing_m*sp_group*aspect, data = coverage_site)
plot(cga1)
shapiro.test(resid(cga1))   # p < 0.05 = not normal, can't use it 
bptest(cga1)  # p < 0.05 = and heteroskedasticity 

cga_glm <- glm(coverage_perc ~ grazing_m + sp_group + aspect, data = coverage_site)
hist(resid(cga_glm))  
summary(cga_glm)   ## USE THIS MODEL'S RESULTS ##
  # 6.74757 ± 2.8971, p = 0.0211
  # grazing: -0.01126 ± 0.12946, p = 0.9308    
  # herb: -5.14285 ± 3.41962, p = 0.1346    
  # lichen: 16.96416 ± 3.36667, p = 1.25e-06 (SIGNIFICANT)
  # moss: 4.72480 ± 3.39244, p = 0.1656    
  # shrub: 50.38268 ± 3.36667, p = < 2e-16 (SIGNIFICANT)
  # aspect SOUTH: 0.19833 ± 2.15187, p = 0.9267    

# calculating McFadden's R2 
with(summary(cga_glm), 1 - deviance/null.deviance)  # R2 = 0.6833 (very good model)


cga_glm2 <- glm(coverage_perc ~ grazing_m*sp_group + aspect, data = coverage_site)

cga_glm3 <- glm(coverage_perc ~ grazing_m + sp_group*aspect, data = coverage_site)

cga_glm4 <- glm(coverage_perc ~ grazing_m*sp_group*aspect, data = coverage_site)
summary(cga_glm)          

## Choosing the best model
c_null <- glm(coverage_perc ~ 1, data = coverage_site)

AIC(c_null, cg_glm, cga_glm, cga_glm2, cga_glm3, cga_glm4)   # best is 'cga_glm' (no interaction)


# . ----
#### Other Basic Statistics ----
### Height ~ Aspect
ha1 <- lm(height_site ~ aspect, data = richness)
shapiro.test(resid(ha1))  # not normal 

wilcox.test(height_site ~ aspect, data = richness)
  # W = 25775, p = 0.02745 (SIGNIFICANT difference in height between N and S)

# average height S vs. N
richness %>% group_by(aspect) %>% summarise(height = mean(height_site),
                                            se = std.error(height_site))

### Richness ~ Aspect
ra1 <- lm(richness_siteT ~ aspect, data = richness)
shapiro.test(resid(ra1))  # not normal 

wilcox.test(richness_siteT ~ aspect, data = richness)
  # W = 42251, p = < 2.2e-16

# average height S vs. N
richness %>% group_by(aspect) %>% summarise(rich = mean(richness_siteT),
                                            se = std.error(richness_siteT))


# . ----
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


## Plotting by aspect ----
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

## Plotting by grazing category ----
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

#### NMDS (Coverage by Sp) ----
### Dividing coverage across species
sp_cov <- left_join(plots, coverage)
sp_cov <- sp_cov %>% 
            dplyr::select(site_nr, plot_nr, aspect, sp_common, sp_latin, sp_group, coverage_perc) %>% 
            group_by(site_nr, plot_nr, sp_group) %>% 
            mutate(rel_abund = coverage_perc/length(sp_group)) %>% 
            ungroup() %>% 
            na.omit()

print(sp_cov %>% group_by(plot_nr) %>% summarise(sum(rel_abund)), n = 105)  # all = 100%! 

### Making a matrix
sp_matrix <- sp_cov %>% 
                dplyr::select(plot_nr, aspect, sp_latin, rel_abund) %>% 
                distinct() %>% 
                pivot_wider(names_from = "sp_latin", values_from = "rel_abund", 
                            values_fill = NA)  # get a warning, but no duplicates found

sp_matrix[is.na(sp_matrix)] <- 0  # making sure non-existent sp just have coverage of 0

# with only coverage data 
sp_matrix2 <- sp_matrix %>% dplyr::select(!c(plot_nr, aspect)) 

### Looking at how many axes to extract 
set.seed(1)
mds1b <- metaMDS(sp_matrix2, distance = "bray", k = 1, sratmax = 0.99999)  # no convergence 
mds2b <- metaMDS(sp_matrix2, distance = "bray", k = 2, sratmax = 0.99999)  # no convergence 
mds3b <- metaMDS(sp_matrix2, distance = "bray", k = 3, sratmax = 0.99999)  # converged!   
mds4b <- metaMDS(sp_matrix2, distance = "bray", k = 4, sratmax = 0.99999)  # no convergence 
mds5b <- metaMDS(sp_matrix2, distance = "bray", k = 5, sratmax = 0.99999)  # no convergence 
mds6b <- metaMDS(sp_matrix2, distance = "bray", k = 6, sratmax = 0.99999)  # no convergence 
mds7b <- metaMDS(sp_matrix2, distance = "bray", k = 7, sratmax = 0.99999)  # converged!
mds8b <- metaMDS(sp_matrix2, distance = "bray", k = 8, sratmax = 0.99999)  # converged! 
mds9b <- metaMDS(sp_matrix2, distance = "bray", k = 9, sratmax = 0.99999)  # no convergence 

# only k = 3, k = 7 and k = 8 converged

set.seed(1)
nmds3 <- metaMDS(sp_matrix2, distance = "bray", k = 3, autotransform = TRUE, trymax = 500, 
                sratmax = 0.99999) 
  # stress = ~0.166
nmds7 <- metaMDS(sp_matrix2, distance = "bray", k = 7, autotransform = TRUE, trymax = 500, 
                sratmax = 0.99999)  
  # stress = ~0.0796 (going with this one!)
nmds8 <- metaMDS(sp_matrix2, distance = "bray", k = 8, autotransform = TRUE, trymax = 500, 
                 sratmax = 0.99999)  
  # stress = ~0.0692

### Plotting the NMDS
# extracting NMDS scores (x and y coordinates)
data.scoresb <- as.data.frame(scores(nmds7))
species_scoresb <- as.data.frame(scores(nmds7, "species"))
species_scoresb$species <- rownames(species_scores)

data.scoresb$aspect <- sp_matrix$aspect

## Plotting by aspect ----
NMDSb <- data.frame(NMDS2 = nmds7$points[,1], NMDS3 = nmds7$points[,2], 
                    group = data.scoresb$aspect)
NMDSb$group <- as.factor(NMDSb$group)

NMDSb_mean <- aggregate(NMDSb[,1:2], list(group = NMDSb$group), "mean")

# making the ellipsoids
ord3 <- ordiellipse(nmds7, data.scoresb$aspect, label = T, conf = 0.95)

df_ell3 <- data.frame()   # run from here (this side)

for(g in levels(NMDSb$group)){
  df_ell3 <- rbind(df_ell3, 
                   cbind(as.data.frame(with(NMDSb[NMDSb$group==g,],
                                           veganCovEllipse(ord3[[g]]$cov, ord3[[g]]$center,
                                                           ord3[[g]]$scale)))
                        ,group=g))
}  # run from here 

# plot
(nmds_plot3 <- ggplot(data.scoresb, aes(x = NMDS1, y = NMDS2)) + 
                  geom_polygon(data = df_ell3, aes(x = NMDS1, y = NMDS2, group = group,
                                                   color = group, fill = group), alpha = 0.2, 
                               size = 0.5, linetype = 1) +
                  geom_point(aes(color = aspect, shape = aspect, fill = aspect), 
                             size = 2, alpha = 0.6) +
               #   geom_text(data = species_scoresb, aes(x = NMDS1, y = NMDS2, label = species),
                #            alpha = 0.5, size = 2) +
                  theme_thesis + 
                  labs(x = "NMDS1", y = "NMDS2") +
                  scale_color_manual(values = c("#458264", "#F4A460"),
                                     labels = c("N", "S")) +
                  scale_shape_manual(values = c(21, 22),
                                     labels = c("N", "S")) +
                  scale_fill_manual(values = c("#458264", "#F4A460"),
                                    labels = c("N", "S")))


# . ----
#### ANOSIM Tests ----
## For sp group ~ aspect 
ano_a <- anosim(cov_matrix_sp, cov_matrix$aspect, distance = "bray", permutations = 9999)
ano_a
  # not significantly different (R = 0.01435, p = 0.2577)

## For sp group ~ grazing category
ano_g <- anosim(cov_matrix_sp, cov_matrix$grazing_cat, distance = "bray", permutations = 9999)
ano_g
  # not significantly different (R = -0.08811, p = 0.9864) 

## For species ~ aspect
ano_sp <- anosim(sp_matrix2, sp_matrix$aspect, distance = "bray", permutations = 9999)
ano_sp
