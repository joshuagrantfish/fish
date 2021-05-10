#Joshua Grant
#April 2021
#Electrofishing comparison 
#joshuagrant@email.arizona.edu
#Purpose of this study is to compare 3 types of electrofishing methods and the fish composition of each

#load dplyr, ggpubr

#load excel sheet with fish species and named it efish
library(readxl)
efish <- read_excel("~/Documents/School related/Grad/Dataa/ELECTRO.xlsx", 
                    sheet = "SPPperHR")

#view data
View(efish)

#show group levels
efish$METHOD<-as.factor(efish$METHOD) 
levels(efish$METHOD)



#summary stats for carp
library(dplyr)
group_by(efish, METHOD) %>%
  summarise(
    count = n(),
    mean = mean(CYCA, na.rm = TRUE),
    sd = sd(CYCA, na.rm = TRUE)
  )
#boxplot for carp between 3 methods

library("ggpubr")
ggboxplot(efish, x = "METHOD", y = "CYCA", 
          order = c("ctrl", "trt1", "trt2"),
          ylab = "CYCA", xlab = "METHOD")

#Kruskal test to see if there is significant differences
kruskal.test(CYCA ~ METHOD, data = efish)

#preform the same steps above for each of the other species caught