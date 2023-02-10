## Data Exploration and Statistics ##
## For SU Master's Thesis in Landscape Ecology
## Emma Gemal
## Last updated 10/2/23

#### Library ----
library(MASS)
library(tidyverse)
library(lmtest)
library(lme4)
library(car)


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

unique(coverage$sp_group)
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

plots %>% group_by(plot_nr) %>% summarize(n = length(unique(sp_latin)))
plots %>% group_by(plot_nr) %>% summarize(n = length(sp_latin))  # same number of names = no duplicates


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
                        avg_height = mean(avg_height)) %>% 
              ungroup() %>% 
              group_by(plot_nr) %>% 
              mutate(rich_group_prop = (richness_group/sum(richness_group))) %>% 
              ungroup() %>%
              group_by(site_nr) %>% 
              mutate(richness_siteT = mean(richness_plot))   # true richness per site (with reps)

# combining 'richness' with 'sites' to add grazing data 
grazing <- sites %>% dplyr::select(site_nr, plot_nr, plot, aspect, grazing_s)

richness <- left_join(richness, grazing)
str(richness)
richness <- as.data.frame(richness)

## Adding grazing data to 'coverage' 
coverage <- left_join(coverage, grazing)
str(coverage)


#### Initial Visualizations ----
## Richness
ggplot(richness, aes(x = grazing_s, y = richness_plot)) +   # richness with grazing 
  geom_point()

ggplot(richness, aes(x = grazing_s, y = richness_plot)) +   # N and S accounted for 
  geom_point(aes(color = aspect)) +
  stat_smooth(method = "lm", aes(color = aspect))

ggplot(richness, aes(x = grazing_s, y = richness_siteT)) +   # averaged across the plots
  geom_point()

ggplot(richness, aes(x = grazing_s, y = richness_siteT)) +   # averaged and aspect accounted for
  geom_point(aes(color = aspect)) +
  stat_smooth(method = "lm", aes(color = aspect))

ggplot(richness, aes(x = grazing_s, y = rich_group_prop)) +   # how proportions of groups changes
  geom_point(aes(color = sp_group)) +
  stat_smooth(method = "lm", aes(color = sp_group)) +
  facet_wrap(~sp_group)

ggplot(richness, aes(x = grazing_s, y = rich_group_prop)) +   # aspect accounted for 
  geom_point(aes(color = sp_group)) +
  stat_smooth(method = "lm", aes(color = sp_group)) +   
  facet_wrap(~aspect)

ggplot(richness, aes(x = aspect, y = rich_group_prop)) +   # relative species nr for each group 
  geom_boxplot(aes(fill = sp_group))                       # for N vs. S 

ggplot(richness, aes(x = aspect, y = richness_siteT)) +   # averaged richness N vs. S
  geom_boxplot()

## Coverage
ggplot(coverage, aes(x = grazing_s, y = coverage_perc)) +  # coverage of groups with grazing 
  geom_point(aes(color = sp_group)) +
  stat_smooth(method = "lm", se = F, aes(color = sp_group))

ggplot(coverage, aes(x = grazing_s, y = coverage_perc)) +  # differences for N vs. S
  geom_point(aes(color = sp_group)) +
  stat_smooth(method = "lm", se = F, aes(color = sp_group)) +
  facet_wrap(~aspect)

ggplot(coverage, aes(x = aspect, y = coverage_perc)) +   # coverage differences N vs. S
  geom_boxplot(aes(fill = sp_group))

## Veg height
ggplot(richness, aes(x = grazing_s, y = avg_height)) +  # height with grazing 
  geom_point() +
  stat_smooth(method = "lm")

ggplot(richness, aes(x = grazing_s, y = avg_height)) +   # height of N vs. S plots 
  geom_point(aes(color = aspect)) +
  stat_smooth(method = "lm", aes(color = aspect))

ggplot(richness, aes(x = aspect, y = avg_height)) +
  geom_boxplot()


#### Statistics ----
### Richness ~ Grazing 
# checking assumptions
hist(richness$richness_siteT)  # no clear distribution 
hist(richness$grazing_s)

# if I use 'richness_plot' then I use family = "poisson" 

rg1 <- lm(richness_siteT ~ grazing_s, data = richness)
plot(rg1)
shapiro.test(resid(rg1))   # p < 0.05 = not normal
bptest(rg1)  # p > 0.05 = no heteroskedasticity though 

## GLM 
rg_glm <- glm(richness_siteT ~ grazing_s, data = richness)
summary(rg_glm)
  # slope = -0.0009091 ± 0.0002444 
  # p = 0.000224 (SIGNIFICANT!) 

# calculating McFadden's R2 
with(summary(rg_glm), 1 - deviance/null.deviance)  # R2 = 0.027 (not a good model though)

### Models with aspect 
rga1 <- lm(richness_siteT ~ grazing_s + aspect, data = richness)
plot(rga1)
shapiro.test(resid(rga1))   # p < 0.05 = not normal (it's count data)
bptest(rga1)  # p < 0.05 = and heteroskedasticity

rga_glm <- glm(richness_siteT ~ grazing_s + aspect, data = richness)
summary(rga_glm)       ## USE THIS MODEL'S RESULTS ##
  # grazing: -0.000799 ± 0.000225, p = 0.000432 (SIGNIFICANT)
  # aspect SOUTH: -2.1257 ± 0.2284, p = < 2e-16 (SIGNIFICANT)

# calculating McFadden's R2 
with(summary(rga_glm), 1 - deviance/null.deviance)  # R2 = 0.176 (an ok model)

## Choosing the best model 
AIC(rg_glm, rga_glm)  # 'rga_glm' is the best 


### Height ~ Grazing
hist(richness$avg_height)  # right skewed (positive skew)

hg1 <- lm(avg_height ~ grazing_s, data = richness)
plot(hg1)
shapiro.test(resid(hg1))   # p < 0.05 = not normal (skewed)
bptest(hg1)  # p > 0.05 = no heteroskedasticity though 

## Transformations 
# log
hg2 <- lm(log(avg_height) ~ grazing_s, data = richness)
plot(hg2)
shapiro.test(resid(hg2))   # p < 0.05 = not normal
bptest(hg2)  # p > 0.05 = no heteroskedasticity 

# sqrt 
hg3 <- lm(sqrt(avg_height) ~ grazing_s, data = richness)
plot(hg3)
shapiro.test(resid(hg3))   # p < 0.05 = not normal, almost worse 
bptest(hg3)  # p > 0.05 = no heteroskedasticity 

# box-cox 
bc <- boxcox(hg1)
(lambda <- bc$x[which.max(bc$y)])  # extracting exact lambda for transformation 

hg4 <- lm(((avg_height^lambda-1)/lambda) ~ grazing_s, data = richness)
plot(hg4)
shapiro.test(resid(hg4))   # p < 0.05 = not normal, nothing works 
bptest(hg4)  # p > 0.05 = no heteroskedasticity 

## GLM    
hg_glm <- glm(avg_height ~ grazing_s, data = richness)
summary(hg_glm) 
  # slope = 0.00026 ± 0.000229
  # p = 0.255 (not significant)

# calculating McFadden's R2 
with(summary(hg_glm), 1 - deviance/null.deviance)  # R2 = 0.0027 (not a good model)

### Models with aspect 
hga1 <- lm(avg_height ~ grazing_s + aspect, data = richness)
plot(hga1)
shapiro.test(resid(hga1))   # p < 0.05 = not normal 
bptest(hga1)  # p < 0.05 = and heteroskedasticity 

hga_glm <- glm(avg_height ~ grazing_s + aspect, data = richness)
summary(hga_glm)        ## USE THIS MODEL'S RESULTS ##
  # grazing: 0.0002136 ± 0.000226, p = 0.344 (not significant)
  # aspect SOUTH: 0.9095 ± 0.2286, p = 8.02e-5 (SIGNIFICANT)

# calculating McFadden's R2 
with(summary(hga_glm), 1 - deviance/null.deviance)  # R2 = 0.0345 (not a good model)

## Choosing the best model 
AIC(hg_glm, hga_glm)  # 'hga_glm' is best


### Coverage ~ Grazing
ggplot(coverage, aes(x = coverage_perc)) +
  geom_histogram() +
  facet_wrap(~sp_group)   # very zero-inflated/ lots of low values 

cg1 <- lm(coverage_perc ~ grazing_s + sp_group, data = coverage)
plot(cg1)
shapiro.test(resid(cg1))   # p < 0.05 = not normal 
bptest(cg1)  # p < 0.05 = heteroskedasticity too  

cg2 <- lm(coverage_perc ~ grazing_s*sp_group, data = coverage)
plot(cg2)
shapiro.test(resid(cg2))   # p < 0.05 = not normal 
bptest(cg2)  # p < 0.05 = heteroskedasticity too  

## Transformations 
# sqrt 
cg3 <- lm(sqrt(coverage_perc) ~ grazing_s + sp_group, data = coverage)
plot(cg3)
shapiro.test(resid(cg3))   # p < 0.05 = not normal
bptest(cg3)  # p < 0.05 = and heteroskedasticity 

# 0's in data = log and boxcox don't work 

## GLM
cg_glm <- glm(coverage_perc ~ grazing_s + sp_group, data = coverage)
summary(cg_glm)
  # grazing (grass): -0.001615 ± 0.00237, p = 0.496 (not significant, expected)
  # herb coverage: -2.284 ± 4.414, p = 0.605 (not significant, large variation)
  # lichen coverage: 0.2565 ± 3.8339, p = 0.9467 (not significant, very large variation)
  # moss coverage: -2.881 ± 4.139, p = 0.4867 (not significant, large variation)
  # shrub coverage: 8.513 ± 4.1217, p = 0.0394 (SIGNIFICANT!)

# can't use Gamma distribution due to presence of non-positive data (0's)

### Models with aspect
cga1 <- lm(coverage_perc ~ grazing_s + sp_group + aspect, data = coverage)
plot(cga1)
shapiro.test(resid(cga1))   # p < 0.05 = not normal 
bptest(cga1)  # p < 0.05 = and heteroskedasticity 

cga_glm <- glm(coverage_perc ~ grazing_s + sp_group + aspect, data = coverage)
summary(cga_glm) 
 
cga_glm2 <- glm(coverage_perc ~ grazing_s + sp_group*aspect, data = coverage)
summary(cga_glm2)          ## USE THIS MODEL'S RESULTS ##
  # grazing: -0.001557 ± 0.00237, p = 0.5128 (not significant)
  # N herb coverage: -6.955 ± 6.1954, p = 0.26216 (not significant)
  # N lichen coverage: -3.9057 ± 5.2948, p = 0.46110 (not significant, large variation)
  # N moss coverage: -5.4507 ± 5.74815, p = 0.34349 (not significant)
  # N shrub coverage: -3.9716 ± 5.7564, p = 0.1392 (not significant, large variation)
  # aspect SOUTH: -9.3224 ± 6.2933, p = 0.7197 (not significant)
  # S herb coverage: 9.49658 ± 8.776, p = 0.2797 (not significant, large variation)
  # S lichen coverage: 8.41128 ± 7.6068, p = 0.269 (not significant, large variation)
  # S moss coverage: 5.1482 ± 8.219, p = 0.5314 (not significant, very large variation)
  # S shrub coverage: 25.0958 ± 8.16559, p = 0.00224 (SIGNIFICANT!)

## Choosing the best model 
AIC(cg_glm, cga_glm, cga_glm2)  # 'cga_glm2' is the best model


