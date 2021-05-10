#Joshua Grant
#April 2021
#Electrofishing comparison 
#joshuagrant@email.arizona.edu
#Purpose of this study is to compare 3 types of electrofishing methods and the fish composition of each


#load dplyr, ggpubr
library(dplyr)

#load excel sheet with fish species and named it efish

efish <- read.csv("~/Downloads/ELECTRO.csv")

#view data
#View(efish)
head(efish)


#I am only going to work with BASS so I will filter only bass
efish_bass<- efish %>% 
  filter(SPECIES == "MISA") %>% 
  select(METHOD, SPECIES, LENGTH, WEIGHT)

# view
head(efish_bass)
efish_bass <- (na.omit(efish_bass))
efish_bass$MISA <- ifelse(efish_bass$SPECIES == "MISA", 1,0)
head(efish_bass)

# This was because I was bored and I wanted to play with length to weight data. 
plot(LENGTH ~ WEIGHT, data = efish_bass)


#show group levels

levels(efish_bass$METHOD)

#bass length by net type

efish_bass %>%                                       
  group_by(METHOD) %>%                       
  summarise_at(vars(LENGTH),             
               list(name = mean)) 

# boxplot of results

boxplot(LENGTH ~ METHOD, data = efish_bass)   
#### located a couple of outliers. May want to check size on two of those fishes!

# ANOVA Analysis

length_AOV <- aov(LENGTH ~ METHOD, data = efish_bass)
summary(length_AOV) # no difference in the length of fish caught. 

#bass weight by net type
efish_bass %>%                                       
  group_by(METHOD) %>%                       
  summarise_at(vars(Weight),             
               list(name = mean)) 

# boxplot of weights and method

boxplot(WEIGHT ~ METHOD, data = efish_bass)

#ANOVA weight vs method
weight_AOV <-aov(WEIGHT ~ METHOD, data = efish_bass)
summary(weight_AOV)
### evidence of a difference in weights captured by each fish! P = 0.0834
# Post-hoc bonferonni test thing
pairwise.t.test(efish_bass$WEIGHT, efish_bass$METHOD, p.adj = "bonf")
#ARC INTER appears to be different than both other methods however not statistically significant at p<0.10


#Step 2 Catch Per Unit Effort.

# I need to upload a different CSV for CPUE data. 
CPUE <- read.csv("~/Downloads/CPUE.csv")
head(CPUE)

#visualize the distribution of MISA among the three net types
boxplot(MISA ~ METHOD, data = CPUE)

#Summary statistics of the MISA catch data
CPUE %>%                                       
  group_by(METHOD) %>%                       
  summarise_at(vars(MISA),             
               list(name = mean)) 

# ANOVA Analysis of the three different net types
CPUE_ANOVA <-aov(MISA ~ METHOD, data = CPUE)
summary(CPUE_ANOVA)
# No difference in CPUE between the three net types for Bass. (P = 0.283)
# For Bass it doesnt seem to matter all that much, but i usually see CPUE data log transformed
CPUE_Trans <- aov(log(MISA) ~ METHOD, data = CPUE)
summary(CPUE_Trans)
# P = 0.326
