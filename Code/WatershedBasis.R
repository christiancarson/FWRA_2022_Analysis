######Intro#######
#Hi team, welcome to our R script for creating plots for Area 23.
#view my comments in the "#" regions, these will provide you a guide for what to
#change in your own analysis
#GUIDE#
#Areas where you will need to make changes are indicated with "#$#", copy
#and paste this text and paste it into the find function (command F), to view
#all the areas where you need to make changes

#whenever begging an R script, make sure to write down key infromation
#such as its title, author, date updated, etc. Example below for this script:
#Title: Area 23 Plots Template
#Author: Critty (Christian) Carson
#Last updated : August 19th, 2022
#Description : Summaries of NUSEDS escapement, hydromet data,

#install.packages(c("boot", "MASS","plyr","dplyr", "plot2", "tibble",
# "car","reshape2","epitools", "readxl", "tidyverse","arsenal"))
#install.packages(c("ggplot2", "patchwork", "palmerpenguins"))

#install.packages(c("gt","gtExtras"))
#now we need to load all the packages into our r script, this is done with the
#function, library(). For instance, if we want to load the package "boot", we would
#run the function below. Now, highlight all the functions below in a row and run them
#remotes::install_github("ManuelHentschel/vscDebugger")
library(boot)
library(MASS)
library(plyr)
library(dplyr)
library(ggplot2)
library(tibble)
library(reshape2)
library(epitools)
library(readxl)
library(tidyverse)
library(readr)
library(arsenal)
source("https://raw.githubusercontent.com/koundy/ggplot_theme_Publication/master/ggplot_theme_Publication-2.R")
library(patchwork)
library(palmerpenguins)
library(viridis)
library(gt)
library(gtExtras)
library(RColorBrewer)
library(reshape2)
library(knitr)
#install.packages("vtable")
library(vtable)
#install.packages("ggrepel")
library(ggrepel)
# <- "https://raw.githubusercontent.com/gadenbuie/yule-rstudio/master/Yule-RStudio.rstheme"
#rstudioapi::addTheme(yule_theme, apply = TRUE)


#ok, so we have loaded all out packages and we are now ready to import data for
#specific analyses. First, we must designate a section; this is done bye putting 5 or more "#"'s in a row
#below we are going to analyize escapement data from the NUseds database, so we naming the section as:

#####Setup#####

#--------------any libraries needed are loaded and displayed below--------------
#
library(dplyr)
library(zoo)
library(kableExtra)
#
#--------------make project folders and folder paths----------------------------

library(httpgd)
hgd()

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

#####you will need to find the exact path of the FINAL_FWRA_RESULTS_ALL_AREAS_MASTER path on our sharepoint and paste it below
                       
FWRA <- read_excel("/Users/critty/Desktop/Base/GitHub/FWRA_2022_Analysis/Data/FWRA_2021_RESULTS_MASTER.xlsx", sheet = 1)
head(FWRA)

colnames(FWRA)
FWRA <- subset(FWRA, LF_Number != "23" & LF_Number != "24")

#subset for only indicator systems
FWRA <- subset(FWRA, FWRA_CONDUCTED == "Y")
unique(FWRA$SYSTEM_SITE)

#use dyplyr to make a table that lists all the unique watersheds in the data and what A they are in
watersheds <- c(print(unique(FWRA$SYSTEM_SITE)))


#save watersheds as a csv
write.csv(watersheds, file = "watersheds.csv")
  
 options(ggrepel.max.overlaps = Inf)
 require(ggrepel)

for (i in watersheds) {
Sarita <- subset(FWRA, FWRA$SYSTEM_SITE == i)

Sarita<-subset(Sarita, Current_Bio_Risk!= 0 & Current_Bio_Risk!= -1)
FWRA$Current_Bio_Risk <- as.numeric(FWRA$Current_Bio_Risk)
FWRA$Future_Bio_Risk <- as.numeric(FWRA$Future_Bio_Risk)
FWRA$Total_Bio_Risk <- as.numeric(FWRA$Total_Bio_Risk)

library(vtable)
library(RColorBrewer)
library(dplyr)
library(ggplot2)
library(ggrepel)

myData <- matrix(c(2,2,3,3,3,1,2,2,3,3,1,1,2,2,3,1,1,2,2,2,1,1,1,1,2), nrow = 5, ncol = 5, byrow = TRUE)
longData <- reshape2::melt(myData)
colnames(longData) <- c("Current_Bio_Risk", "Future_Bio_Risk", "Total_Bio_Risk")
longData <- mutate(longData, Total_Bio_Risk = Current_Bio_Risk * Future_Bio_Risk)
mycols <- rev(c("red3","darkorange1","gold1","yellowgreen","forestgreen"))
cols <- colorRampPalette(mycols)
myvals <- c(0,6.5,12.5,18.75,25)
scaled_val <- c(0, 0.1, 0.3, 0.7, 1)
set.seed(42)
myplot <- ggplot(longData,aes(x = Future_Bio_Risk, y = Current_Bio_Risk, fill = Total_Bio_Risk))+ 
  theme_classic()+ 
  geom_tile()+
  scale_fill_gradientn(name = element_text(size=20, face="bold", "Total Risk"), 
                     colours = cols(length(mycols)), 
                     values = scaled_val, 
                     breaks=c(0,6.5,12.5,18.75,25),
                     labels=c("VL","L","M","H","VH"),
                     limits=c(0,25))+
   guides(fill = guide_colorbar(barheight = 5, barwidth = 2, direction = "vertical"))+
 theme(legend.position = "right") +
 theme(axis.title=element_text(size=16, face="bold"))+
 coord_fixed()+ 
  geom_point(data = Sarita, aes(), size = 3, color = "#000000") +
  geom_label_repel(data = Sarita,
    aes(label = LF_Number, fill = Total_Bio_Risk),
    fontface = 'bold', color = '#000000',size = 4,
    box.padding = unit(0.35, "lines"),
    point.padding = unit(0.5, "lines"),
    segment.color = '#000000', max.iter = Inf, force = 5, direction = "both", min.segment.length = 0) +
  labs(x = "Future Risk", y = "Current Risk") +
  theme(axis.text=element_text(size=14, face="bold"),
        axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
        axis.text.y=element_text(color="black")) +
  theme(plot.title = element_text(hjust = 0.5))+ 
  scale_x_continuous(breaks = c(1,2,3,4,5),labels = c("VL","L","M","H","VH"))+
  scale_y_continuous(breaks = c(1,2,3,4,5),labels = c("VL","L","M","H","VH")) 

  #Save the plot, define your folder location as "/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures"
ggsave(filename = paste0("/Users/critty/Desktop/Base/GitHub/FWRA_2022_Analysis/Figures/Watershed_Risks/", print(i),".jpeg"),
       device = "jpeg",
       width = 30,
       height = 30,
       units = "cm",
       dpi = 300)




}




###Reload for tables######
# time to upload the datas
####Risk Rankings

FWRA <- read_excel("/Users/critty/Desktop/Base/GitHub/FWRA_2022_Analysis/Data/FWRA_2021_RESULTS_MASTER.xlsx", sheet = 1)
head(FWRA)

FWRA <- subset(FWRA, LF_Number != "23" & LF_Number != "24")
FWRA<-subset(FWRA, Current_Bio_Risk!= 0 & Current_Bio_Risk!= -1 & Future_Bio_Risk!= 0 & Future_Bio_Risk!= -1)

colnames(FWRA)

FWRA <- select(FWRA,SYSTEM_SITE,LF_Name,Total_Bio_Risk,Current_Bio_Risk,Future_Bio_Risk, Stage)

FWRA <- subset(FWRA, LF_Name != "LF24: Mortality of eggs due to lack of groundwater upwelling on lakeshore" & LF_Name != "LF23: Mortality of eggs during incubation due to variable lake water levels")
unique(FWRA$LF_Name)

watersheds <- c(print(unique(FWRA$SYSTEM_SITE)))

for (i in watersheds) {

SARITA_NUMERIC <- dplyr::filter(FWRA, SYSTEM_SITE %in% c(print(i)))
colnames(SARITA_NUMERIC)
SARITA_NUMERIC$Total_Bio_Risk <- as.numeric(SARITA_NUMERIC$Total_Bio_Risk) 
SARITA_NUMERIC$Rank <- rank(-SARITA_NUMERIC$Total_Bio_Risk, ties.method = "min")

colnames(SARITA_NUMERIC)
col_order <- c("SYSTEM_SITE","Stage","LF_Name","Rank","Total_Bio_Risk","Current_Bio_Risk", "Future_Bio_Risk")

SARITA_NUMERIC <- SARITA_NUMERIC[, col_order]
SARITA_NUMERIC <- SARITA_NUMERIC[order(SARITA_NUMERIC$Total_Bio_Risk, decreasing = TRUE),]  
SARITA_NUMERIC <- SARITA_NUMERIC %>% 
  mutate(across(Current_Bio_Risk:Future_Bio_Risk, as.character))

SARITA_NUMERIC$Current_Bio_Risk[SARITA_NUMERIC$Current_Bio_Risk=="1"]<-"VL"
SARITA_NUMERIC$Current_Bio_Risk[SARITA_NUMERIC$Current_Bio_Risk=="2"]<-"L"
SARITA_NUMERIC$Current_Bio_Risk[SARITA_NUMERIC$Current_Bio_Risk=="3"]<-"M"
SARITA_NUMERIC$Current_Bio_Risk[SARITA_NUMERIC$Current_Bio_Risk=="4"]<-"H"
SARITA_NUMERIC$Current_Bio_Risk[SARITA_NUMERIC$Current_Bio_Risk=="5"]<-"VH"
SARITA_NUMERIC$Future_Bio_Risk[SARITA_NUMERIC$Future_Bio_Risk=="1"]<-"VL"
SARITA_NUMERIC$Future_Bio_Risk[SARITA_NUMERIC$Future_Bio_Risk=="2"]<-"L"
SARITA_NUMERIC$Future_Bio_Risk[SARITA_NUMERIC$Future_Bio_Risk=="3"]<-"M"
SARITA_NUMERIC$Future_Bio_Risk[SARITA_NUMERIC$Future_Bio_Risk=="4"]<-"H"
SARITA_NUMERIC$Future_Bio_Risk[SARITA_NUMERIC$Future_Bio_Risk=="5"]<-"VH"

#mycols <- rev(c("red3","darkorange1","gold1","yellowgreen","forestgreen"))
#cols <- colorRampPalette(mycols)

colnames(SARITA_NUMERIC)[which(names(SARITA_NUMERIC) == "SYSTEM_SITE")] <- "Watershed"
colnames(SARITA_NUMERIC)[which(names(SARITA_NUMERIC) == "LF_Name")] <- "LF Name"
colnames(SARITA_NUMERIC)[which(names(SARITA_NUMERIC) == "Total_Bio_Risk")] <- "Total Risk"
colnames(SARITA_NUMERIC)[which(names(SARITA_NUMERIC) == "Current_Bio_Risk")] <- "Current Risk"
colnames(SARITA_NUMERIC)[which(names(SARITA_NUMERIC) == "Future_Bio_Risk")] <- "Future Risk"

#remove columns Watershed, Total Risk, Rank, and Future Risk


SARITA_NUMERIC %>%
    head(68) %>%
    gt() %>%
gtsave(filename = paste0("/Users/critty/Desktop/Base/GitHub/FWRA_2022_Analysis/Figures/Watershed_Risk_Tables/", print(i),".docx"))

}

###Reload for tables######
# time to upload the datas
####Data Gaps

FWRA <- read_excel("/Users/critty/Desktop/Base/GitHub/FWRA_2022_Analysis/Data/FWRA_2021_RESULTS_MASTER.xlsx", sheet = 1)
head(FWRA)

FWRA <- subset(FWRA, LF_Number != "23" & LF_Number != "24")
  FWRA<-subset(FWRA, Current_Bio_Risk!= 1 & Current_Bio_Risk!= 2 & Current_Bio_Risk!= 3 & Current_Bio_Risk!= 4 & Current_Bio_Risk!= 5 & Future_Bio_Risk!= 1 & Future_Bio_Risk!= 2 & Future_Bio_Risk!= 3 & Future_Bio_Risk!= 4 & Future_Bio_Risk!= 5)

FWRA <- select(FWRA,SYSTEM_SITE,LF_Name,Total_Bio_Risk,Current_Bio_Risk,Future_Bio_Risk,Stage)

FWRA <- subset(FWRA, LF_Name != "LF24: Mortality of eggs due to lack of groundwater upwelling on lakeshore" & LF_Name != "LF23: Mortality of eggs during incubation due to variable lake water levels")
unique(FWRA$LF_Name)

watersheds <- c(print(unique(FWRA$SYSTEM_SITE)))

for (i in watersheds) {

SARITA_NUMERIC <- dplyr::filter(FWRA, SYSTEM_SITE %in% c(print(i)))
colnames(SARITA_NUMERIC)
SARITA_NUMERIC$Total_Bio_Risk <- as.numeric(SARITA_NUMERIC$Total_Bio_Risk) 
SARITA_NUMERIC$Rank <- rank(-SARITA_NUMERIC$Total_Bio_Risk, ties.method = "min")

colnames(SARITA_NUMERIC)
col_order <- c("SYSTEM_SITE","Stage","LF_Name","Rank","Total_Bio_Risk","Current_Bio_Risk", "Future_Bio_Risk")

SARITA_NUMERIC <- SARITA_NUMERIC[, col_order]
SARITA_NUMERIC <- SARITA_NUMERIC[order(SARITA_NUMERIC$Total_Bio_Risk, decreasing = TRUE),]  
SARITA_NUMERIC <- SARITA_NUMERIC %>% 
  mutate(across(Current_Bio_Risk:Future_Bio_Risk, as.character))

  SARITA_NUMERIC$Current_Bio_Risk[SARITA_NUMERIC$Current_Bio_Risk=="-1"]<-"HPDG"
  SARITA_NUMERIC$Current_Bio_Risk[SARITA_NUMERIC$Current_Bio_Risk=="0"]<-"LPDG"
  SARITA_NUMERIC$Future_Bio_Risk[SARITA_NUMERIC$Future_Bio_Risk=="-1"]<-"HPDG"
  SARITA_NUMERIC$Future_Bio_Risk[SARITA_NUMERIC$Future_Bio_Risk=="0"]<-"LPDG"

#mycols <- rev(c("red3","darkorange1","gold1","yellowgreen","forestgreen"))
#cols <- colorRampPalette(mycols)

colnames(SARITA_NUMERIC)[which(names(SARITA_NUMERIC) == "SYSTEM_SITE")] <- "Watershed"
colnames(SARITA_NUMERIC)[which(names(SARITA_NUMERIC) == "LF_Name")] <- "LF_Name"
colnames(SARITA_NUMERIC)[which(names(SARITA_NUMERIC) == "Total_Bio_Risk")] <- "Total Risk"
colnames(SARITA_NUMERIC)[which(names(SARITA_NUMERIC) == "Current_Bio_Risk")] <- "Current and Future Rating"
colnames(SARITA_NUMERIC)[which(names(SARITA_NUMERIC) == "Future_Bio_Risk")] <- "Future Risk"


#remove columns Watershed, Total Risk, Rank, and Future Risk
SARITA_NUMERIC <- SARITA_NUMERIC[, -c(1,4,5,7)]

SARITA_NUMERIC %>%
    head(68) %>%
    gt() %>%
gtsave(filename = paste0("/Users/critty/Desktop/Base/GitHub/FWRA_2022_Analysis/Figures/Watershed_DG_Tables/", print(i),".docx"))

}


####Data Gaps Counts

FWRA <- read_excel("/Users/critty/Desktop/Dekstop/GitHub/FWRA_2022_Analysis/Data/FWRA_2021_RESULTS_MASTER.xlsx", sheet = 1)
head(FWRA)

FWRA <- subset(FWRA, LF_Name != "23" & LF_Name != "24")
  FWRA<-subset(FWRA, Current_Bio_Risk!= 1 & Current_Bio_Risk!= 2 & Current_Bio_Risk!= 3 & Current_Bio_Risk!= 4 & Current_Bio_Risk!= 5 & Future_Bio_Risk!= 1 & Future_Bio_Risk!= 2 & Future_Bio_Risk!= 3 & Future_Bio_Risk!= 4 & Future_Bio_Risk!= 5)

FWRA <- select(FWRA,SYSTEM_SITE,LF_Name,Total_Bio_Risk,Current_Bio_Risk,Future_Bio_Risk)

FWRA <- subset(FWRA, LF_Name != "LF24: Mortality of eggs due to lack of groundwater upwelling on lakeshore" & LF_Name != "LF23: Mortality of eggs during incubation due to variable lake water levels")
unique(FWRA$LF_Name)

watersheds <- c(print(unique(FWRA$SYSTEM_SITE)))

#change all 0s to LPDG and -1s to HPDG
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="-1"]<-"HPDG"
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="0"]<-"LPDG"
FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="-1"]<-"HPDG"
FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="0"]<-"LPDG"

#using dplyr count the number of times each risk value appears in each watershed
DG <- FWRA %>%
  group_by(SYSTEM_SITE) %>%
  count(Current_Bio_Risk, Future_Bio_Risk) %>%
  ungroup()





#Save the table
write.csv(FWRA, file = "/Users/critty/Desktop/Dekstop/GitHub/
FWRA_2022_Analysis/Figures/Watershed_DG_Tables/DG_Table_Breakdown.csv")




#################### Risk Table ####################
FWRA <- read_excel("/Users/critty/Desktop/Dekstop/GitHub/FWRA_2022_Analysis/Data/FWRA_2021_RESULTS_MASTER.xlsx", sheet = 1)
head(FWRA)

FWRA <- subset(FWRA, LF_Number != "23" & LF_Number != "24")

FWRA <- select(FWRA,SYSTEM_SITE,LF_Number,Current_Bio_Risk,Future_Bio_Risk)

FWRA$Current_Bio_Risk <- as.character(FWRA$Current_Bio_Risk)
FWRA$Future_Bio_Risk <- as.character(FWRA$Future_Bio_Risk)

#remove all rows with a 0 or -1 in the Current_Bio_Risk or Future_Bio_Risk columns
FWRA <- subset(FWRA, Current_Bio_Risk!= 0 & Current_Bio_Risk!= -1 & Future_Bio_Risk!= 0 & Future_Bio_Risk!= -1)

#change all numeric values to character
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="1"]<-"VL"
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="2"]<-"L"
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="3"]<-"M"
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="4"]<-"H"
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="5"]<-"VH"

FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="1"]<-"VL"
FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="2"]<-"L"
FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="3"]<-"M"
FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="4"]<-"H"
FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="5"]<-"VH"


library(dplyr)

unique(FWRA$Current_Bio_Risk)
unique(FWRA$Future_Bio_Risk)

sorted <- FWRA %>%
mutate(VH = ifelse(Current_Bio_Risk %in% c("VH"), 1, 0),
VL = ifelse(Current_Bio_Risk %in% c("VL"), 1, 0),
H = ifelse(Current_Bio_Risk %in% c("H"), 1, 0),
M = ifelse(Current_Bio_Risk %in% c("M"), 1, 0),
L = ifelse(Current_Bio_Risk %in% c("L"), 1, 0),
VH_FR = ifelse(Future_Bio_Risk %in% c("VH"), 1, 0),
VL_FR = ifelse(Future_Bio_Risk %in% c("VL"), 1, 0),
H_FR = ifelse(Future_Bio_Risk %in% c("H"), 1, 0),
M_FR = ifelse(Future_Bio_Risk %in% c("M"), 1, 0),
L_FR = ifelse(Future_Bio_Risk %in% c("L"), 1, 0)) %>%
group_by(LF_Number) %>%
summarise(VH = sum(VH),VH_FR = sum(VH_FR),
VL = sum(VL),VL_FR = sum(VL_FR),
H = sum(H),H_FR = sum(H_FR),
M = sum(M),M_FR = sum(M_FR),
L = sum(L),L_FR = sum(L_FR))%>%
arrange(LF_Number)
print(sorted, n = Inf)

#remove future columns
current <- select(sorted, -VH_FR, -VL_FR, -H_FR, -M_FR, -L_FR)
current <- current%>%
arrange(desc(VH), desc(H), desc(M),desc(L), desc(VL))

#order columns by VH, H, M, L, VL
current <- current %>%
  select(LF_Number, VH, H, M, L, VL)

print(current)

#future
future <- select(sorted, -VH, -VL, -H, -M, -L)
future <- future%>%
arrange(desc(VH_FR), desc(H_FR), desc(M_FR),desc(L_FR), desc(VL_FR))

#order columns by VH, H, M, L, VL
future <- future %>%
  select(LF_Number, VH_FR, H_FR, M_FR, L_FR, VL_FR)

# assign current lf names to a variable
v1 <- current$LF_Number
# assign future lf names to a variable
v2 <- future$LF_Number

# Set a value for o
o <- 0.05

# Create a data frame with x, x1, y, and g columns
DF <- data.frame(x = c(rep(1, length(v1)), rep(2, length(v2))),
                 x1 = c(rep(1 + o, length(v1)), rep(2 - o, length(v2))),
                 y = c(rev(seq_along(v1)), rev(seq_along(v2))),
                 g = c(v1, v2))

# Load ggplot2 and grid libraries
library(ggplot2)
library(grid)

# Create a ggplot object with DF as data, x as x-axis, y as y-axis, and g as group
p <- ggplot(DF, aes(x=x, y=y, group=g, label=g))

# Add a path to the plot with x1 as x-axis and green color
p <- p + geom_path(aes(x=x1), arrow = arrow(length = unit(0.02,"npc")), 
            size=1, color="green")

# Add text to the plot
p <- p + geom_text(size=10)

# Remove all non-essential elements of the theme
p <- p + theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

# Display the plot
p

#save the table as a csv
write.csv(sorted, file = "/Users/critty/Desktop/Dekstop/GitHub/FWRA_2022_Analysis/Figures/Watershed_DG_Tables/Overall_Table.csv")

overall <- data.frame(sorted$LF_Name)

#save the table as a csv
write.csv(overall, file = "/Users/critty/Desktop/Dekstop/GitHub/FWRA_2022_Analysis/Figures/Watershed_DG_Tables/Overall_Considerable_Table.csv")

#################### Considerable w/ HPDG Table ####################
FWRA <- read_excel("/Users/critty/Desktop/Dekstop/GitHub/FWRA_2022_Analysis/Data/FWRA_2021_RESULTS_MASTER.xlsx", sheet = 1)
head(FWRA)

FWRA <- subset(FWRA, LF_Name != "23" & LF_Name != "24")

FWRA <- select(FWRA,SYSTEM_SITE,LF_Name,Total_Bio_Risk,Current_Bio_Risk,Future_Bio_Risk)

FWRA <- subset(FWRA, LF_Name != "LF24: Mortality of eggs due to lack of groundwater upwelling on lakeshore" & LF_Name != "LF23: Mortality of eggs during incubation due to variable lake water levels")

FWRA$Current_Bio_Risk <- as.character(FWRA$Current_Bio_Risk)
FWRA$Future_Bio_Risk <- as.character(FWRA$Future_Bio_Risk)


#change all numeric values to character
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="1"]<-"VL"
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="2"]<-"L"
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="3"]<-"M"
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="4"]<-"H"
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="5"]<-"VH"

FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="1"]<-"VL"
FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="2"]<-"L"
FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="3"]<-"M"
FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="4"]<-"H"
FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="5"]<-"VH"

FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="-1"]<-"HPDG"
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="0"]<-"LPDG"
FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="-1"]<-"HPDG"
FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="0"]<-"LPDG"
unique(FWRA$Current_Bio_Risk)
unique(FWRA$Future_Bio_Risk)
library(dplyr)

unique(FWRA$Current_Bio_Risk)
unique(FWRA$Future_Bio_Risk)

sorted <- FWRA %>%
mutate(HPDG = ifelse(Current_Bio_Risk %in% c("HPDG"), 1, 0),
LPDG = ifelse(Current_Bio_Risk %in% c("LPDG"), 1, 0),
VH = ifelse(Current_Bio_Risk %in% c("VH"), 1, 0),
VL = ifelse(Current_Bio_Risk %in% c("VL"), 1, 0),
H = ifelse(Current_Bio_Risk %in% c("H"), 1, 0),
M = ifelse(Current_Bio_Risk %in% c("M"), 1, 0),
L = ifelse(Current_Bio_Risk %in% c("L"), 1, 0),
HPDG_FR = ifelse(Future_Bio_Risk %in% c("HPDG"), 1, 0),
LPDG_FR = ifelse(Future_Bio_Risk %in% c("LPDG"), 1, 0),
VH_FR = ifelse(Future_Bio_Risk %in% c("VH"), 1, 0),
VL_FR = ifelse(Future_Bio_Risk %in% c("VL"), 1, 0),
H_FR = ifelse(Future_Bio_Risk %in% c("H"), 1, 0),
M_FR = ifelse(Future_Bio_Risk %in% c("M"), 1, 0),
L_FR = ifelse(Future_Bio_Risk %in% c("L"), 1, 0)) %>%
group_by(LF_Name) %>%
summarise(LPDG_sum = sum(LPDG, LPDG_FR),
VL_sum = sum(VL, VL_FR),
C_sum = sum(VH, VH_FR,H, H_FR,M, M_FR,HPDG, HPDG_FR),
L_sum = sum(L, L_FR)) %>%
arrange(LF_Name)
print(sorted, n = Inf)

sorted <- sorted %>%
arrange(desc(C_sum),desc(L_sum), desc(VL_sum),desc(LPDG_sum))
print(sorted)


considerable_w_HPDG <- data.frame(sorted$LF_Name)
#################### Considerable w/o HPDG Table ####################
FWRA <- read_excel("/Users/critty/Desktop/Dekstop/GitHub/FWRA_2022_Analysis/Data/FWRA_2021_RESULTS_MASTER.xlsx", sheet = 1)
head(FWRA)

FWRA <- subset(FWRA, LF_Name != "23" & LF_Name != "24")

FWRA <- select(FWRA,SYSTEM_SITE,LF_Name,Total_Bio_Risk,Current_Bio_Risk,Future_Bio_Risk)

FWRA <- subset(FWRA, LF_Name != "LF24: Mortality of eggs due to lack of groundwater upwelling on lakeshore" & LF_Name != "LF23: Mortality of eggs during incubation due to variable lake water levels")

FWRA$Current_Bio_Risk <- as.character(FWRA$Current_Bio_Risk)
FWRA$Future_Bio_Risk <- as.character(FWRA$Future_Bio_Risk)


#change all numeric values to character
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="1"]<-"VL"
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="2"]<-"L"
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="3"]<-"M"
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="4"]<-"H"
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="5"]<-"VH"

FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="1"]<-"VL"
FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="2"]<-"L"
FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="3"]<-"M"
FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="4"]<-"H"
FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="5"]<-"VH"

FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="-1"]<-"HPDG"
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="0"]<-"LPDG"
FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="-1"]<-"HPDG"
FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="0"]<-"LPDG"
unique(FWRA$Current_Bio_Risk)
unique(FWRA$Future_Bio_Risk)
library(dplyr)

unique(FWRA$Current_Bio_Risk)
unique(FWRA$Future_Bio_Risk)

sorted <- FWRA %>%
mutate(HPDG = ifelse(Current_Bio_Risk %in% c("HPDG"), 1, 0),
LPDG = ifelse(Current_Bio_Risk %in% c("LPDG"), 1, 0),
VH = ifelse(Current_Bio_Risk %in% c("VH"), 1, 0),
VL = ifelse(Current_Bio_Risk %in% c("VL"), 1, 0),
H = ifelse(Current_Bio_Risk %in% c("H"), 1, 0),
M = ifelse(Current_Bio_Risk %in% c("M"), 1, 0),
L = ifelse(Current_Bio_Risk %in% c("L"), 1, 0),
HPDG_FR = ifelse(Future_Bio_Risk %in% c("HPDG"), 1, 0),
LPDG_FR = ifelse(Future_Bio_Risk %in% c("LPDG"), 1, 0),
VH_FR = ifelse(Future_Bio_Risk %in% c("VH"), 1, 0),
VL_FR = ifelse(Future_Bio_Risk %in% c("VL"), 1, 0),
H_FR = ifelse(Future_Bio_Risk %in% c("H"), 1, 0),
M_FR = ifelse(Future_Bio_Risk %in% c("M"), 1, 0),
L_FR = ifelse(Future_Bio_Risk %in% c("L"), 1, 0)) %>%
group_by(LF_Name) %>%
summarise(LPDG_sum = sum(LPDG, LPDG_FR),
HPDG_sum = sum(HPDG, HPDG_FR),
VL_sum = sum(VL, VL_FR),
C_sum = sum(VH, VH_FR,H, H_FR,M, M_FR),
L_sum = sum(L, L_FR)) %>%
arrange(LF_Name)
print(sorted, n = Inf)

sorted <- sorted %>%
arrange(desc(C_sum),desc(HPDG_sum),desc(L_sum), desc(VL_sum),desc(LPDG_sum))
print(sorted)

considerable_wo_HPDG <- data.frame(sorted$LF_Name)


#combine the tables
overall <- cbind(overall, considerable_w_HPDG, considerable_wo_HPDG)

#add column for row numbers
overall <- cbind(overall, row.names(overall))
print(overall)

#save as csv
write.csv(overall, file = "/Users/critty/Desktop/Dekstop/GitHub/FWRA_2022_Analysis/Data/Overall.csv")
