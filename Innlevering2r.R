# HELP SCRIPT FILE  Utfording 2.3 - sok-2008

# Download the data file union_unempl.csv and store it in an easily accessible location, such as a folder on your Desktop or in your personal folder.
# Install any R packages that you need using the command install.packages("package name").
# You will need the following packages for the assignment: 

install.packages("ggplot2")
install.packages("tidyverse")
install.packages("readr")
install.packages("plyr")
install.packages("dplyr")


# Set your working directory to the correct folder. 
# Insert your file path for 'YOURFILEPATH'. 

#setwd("FILEPATH")

# You will need the following libraries for the assignment:

library(readr) # fileformat of the dataset
library(ggplot2)     # the ggplot package
library(tidyverse)  # the tidyverse package


# To carry out the assignment, you will need to combine the union_unempl data with map data. 


union<- read_csv("union_unempl.csv") #This loads the data with information about the variables of interest
View(union) #Displays the data
#To combine the unemployment and union data with the map data, we merge on country name. 
#We face two problems here: 1) United Kingdom is called "UK" in the map data, 2) the variable "country" is called "region" in the map data. We need to change this.

#Changing the name of a single observation. The below code changes all observations called "United Kingdom" to "UK" in the union data. 
union$country <- gsub("United Kingdom", "UK", union$country)
View(union) 

# Renaming a variable. The below code renames the variable "Country" to "Region".
names(union)[names(union) == "country"] <- "region"
View(union) 

# Creating a new variable. To create a map showing "Excess coverage", you need to create a new variable. The below code shows how to create a new variable in R. 
union$newvar2<-union$var1 + union$var2 #A sum
union$newvar1<-union$var1 - union$var2 #A difference
union$newvar3<-(union$var1 + union$var2)/2 # A mean value

# You are now ready to create your maps! Follow the tutorial at https://www.youtube.com/watch?v=AgWgPSZ7Gp0 

mapdata <- map_data("world") %>% 
  left_join(., union, by = "region") %>% 
  filter(!is.na(mapdata$year))

Arb <- ggplot(mapdata, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = unempl), color = "black") +
  scale_fill_gradient(name="Arbeidsledighet", low="orange", high="blue", na.value="grey50") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        rect = element_blank())
Arb

Fag <- ggplot(mapdata, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = density), color = "black") +
  scale_fill_gradient(name="Fagforeningsdensitet", low="orange", high="blue", na.value="grey50") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        rect = element_blank())
Fag

Cover <- mapdata %>%  
  filter(!is.na(mapdata$coverage)) %>% 
  mutate(Ex_cov = coverage - density) %>% 
  ggplot(., aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = Ex_cov), color = "black") +
  scale_fill_gradient(name="Excess coverage", low="orange", high="blue", na.value="grey50") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        rect = element_blank())
Cover

Koord <- ggplot(mapdata, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = coord), color = "black") +
  scale_fill_brewer(name="Koordinering av lønnsfastsettelse", palette = "Set1") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        rect = element_blank())
Koord

# The "Coord" variable takes 5 discrete levels. It may therefore be better to use a discrete scale for the coloring. 
# To do this, simply replace "scale_fill_gradient(name="name", low="color1", high="color2", na.value="grey50")" with "scale_fill_brewer(name="name", palette = "Set1")" (or another set you prefer)