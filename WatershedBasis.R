######Intro#######
#Hi team, welcome to our R script for creating plots for Area 23. To follow along
#view my comments in the "#" regions, these will provide you a guide for what to
#change in your own analysis

#GUIDE#
#Areas where you will need to make changes are indicated with "#$#", copy 
#and paste this text and paste it into the find function (command F), to view
#all the areas where you need to make changes

#whenever begging an R script, make sure to write down key infromation to the script
#such as its title, author, date updated, etc. Example below for this script:

#Title: Area 23 Plots Template
#Author: Critty (Christian) Carson
#Last updated : August 19th, 2022
#Description : Summaries of NUSEDS escapement, hydromet data, 

#all scripts should start with installing packages, which are basically mini programs
#that you can add within R to help you do analyses or make specific kinds of plots

#the packages you need are below, unhashtag the sentance below to install packages 
#hit cmd + enter to run this code or highlight it and click run in the top right corner

#install.packages(c("boot", "MASS","plyr","dplyr", "plot2", "tibble", "car", "reshape2",
#                  "epitools", "readxl", "tidyverse","arsenal")))
#install.packages(c("gt","gtExtras"))
#now we need to load all the packages into our r script, this is done with the
#function, library(). For instance, if we want to load the package "boot", we would
#run the function below. Now, highlight all the functions below in a row and run them
library(boot)
library(MASS)
library(plyr)
library(dplyr)
library(ggplot2)
library(tibble)
library(car)
library(reshape2)
library(epitools)
library(readxl)
library(tidyverse)
library(readr)
library(arsenal)
#source("https://raw.githubusercontent.com/koundy/ggplot_theme_Publication/master/ggplot_theme_Publication-2.R")
#install.packages(c("ggplot2", "patchwork", "palmerpenguins"))
library(tidyverse)
library(patchwork)
library(palmerpenguins)
library(viridis)
library(gt)
library(gtExtras)
library(RColorBrewer)
library(reshape2)
library(dplyr)
library(ggplot2)
library(knitr)
# <- "https://raw.githubusercontent.com/gadenbuie/yule-rstudio/master/Yule-RStudio.rstheme"
#rstudioapi::addTheme(yule_theme, apply = TRUE)


#ok, so we have loaded all out packages and we are now ready to import data for
#specific analyses. First, we must designate a section; this is done bye putting 5 or more "#"'s in a row
#below we are going to analyize escapement data from the NUseds database, so we naming the section as:

#####Setup#####

#--------------any libraries needed are loaded and displayed below--------------
#
library(dplyr)
library("zoo")
#
#--------------make project folders and folder paths----------------------------


wd <- getwd()  # working directory

folders <- c("Data Output", "Figures")
# function to create folders below
for(i in 1:length(folders)){
  if(file.exists(folders[i]) == FALSE)
    dir.create(folders[i])
}


# we also need to store the paths to these new folders
data.output.path <- paste(wd, "/", folders[1], sep = "")
figures.path <- paste(wd, "/", folders[2], sep = "")


# now we can access and save stuff to these folders!



#---------------------Below, we upload and clean the data----------


#first, lets load in out data. Do this either by selecting the drop down menu
#go to file, import data, import data from excel, and import your data
#copy and paste the output from the console tab below, like I did here
#alternativley, just run my code below
#make sure you name and assign your new spreadsheet, below I assign this import
#by naming it "nuseds" below, now when I use the name nuseds, it will be called
#on in the program
data.path <- paste(wd, "/", "Data", sep = "")


FWRA <- read.csv(paste(data.path,"/", "Area23_Watersheds_Cleaned.csv",
                       sep = ""), stringsAsFactors = FALSE)

FWRA <- subset(FWRA, LF != 23 & LF != 24)

# time to upload the datas
watersheds <- c(print(unique(FWRA$W)))

options(ggrepel.max.overlaps = Inf)
for (i in watersheds) {
  
Sarita <- subset(FWRA, FWRA$W == i)

Sarita<-subset(Sarita, CR!= 0 & CR!= -1)
library(vtable)
library(RColorBrewer)
library(dplyr)
library(ggplot2)
library(ggrepel)

myData <- matrix(c(2,2,3,3,3,1,2,2,3,3,1,1,2,2,3,1,1,2,2,2,1,1,1,1,2), nrow = 5, ncol = 5, byrow = TRUE)
longData <- reshape2::melt(myData)
colnames(longData) <- c("CR", "FR", "TR")
longData <- mutate(longData, TR = CR * FR)
mycols <- rev(c("red3","darkorange1","gold1","yellowgreen","forestgreen"))
cols <- colorRampPalette(mycols)
myvals <- c(0,6.5,12.5,18.75,25)
scaled_val <- scales::rescale(myvals, 0:1)
set.seed(42)
ggplot(longData,aes(x = FR, y = CR, fill = TR))+ 
  theme_classic()+ 
  geom_tile()+
  scale_fill_gradientn(name = "Total Risk", colours = cols(length(mycols)), 
values = scaled_val, 
breaks=c(0,6.5,12.5,18.75,25),
labels=c("VL","L","M","H","VH"),
                       limits=c(0,25))+
   guides(fill = guide_colorbar(barheight = 5, barwidth = 2, direction = "vertical"))+
 theme(legend.position = "right") +
 coord_fixed()+ 
  geom_point(data = Sarita, aes(), size = 3, color = "white") +
  geom_label_repel(data = Sarita,
    aes(label = LF),
    fontface = 'bold', color = 'white',size = 5,
    box.padding = unit(0.35, "lines"),
    point.padding = unit(0.5, "lines"),
    segment.color = 'white', max.iter = Inf) +
  labs(x = "Future Risk", y = "Current Risk") +
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
        axis.text.y=element_text(color="black")) +
  theme(plot.title = element_text(hjust = 0.5))+ 
  scale_x_continuous(breaks = c(1,2,3,4,5),labels = c("VL","L","M","H","VH"))+
  scale_y_continuous(breaks = c(1,2,3,4,5),labels = c("VL","L","M","H","VH")) 

  
#Save the plot, define your folder location as "/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures"
ggsave(filename = paste0("/Users/critty/Desktop/Dekstop/GitHub/FWRA_2022_Analysis/Figures/Area_23_Figures/", print(i),".jpeg"),
       device = "jpeg",
       width = 30,
       height = 30,
       units = "cm",
       dpi = 300)


}
