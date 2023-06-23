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
library(vtable)
library(ggrepel)

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
hgd_browse()

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
#go to file, import data, import data Future_Bio_Riskom excel, and import your data
#copy and paste the output Future_Bio_Riskom the console tab below, like I did here
#alternativley, just run my code below
#make sure you name and assign your new spreadsheet, below I assign this import
#by naming it "nuseds" below, now when I use the name nuseds, it will be called
#on in the program
data.path <- paste(wd, "/", "Data", sep = "")

#################### Risk Table ####################
FWRA <- read_excel("/Users/critty/Desktop/Base/GitHub/FWRA_2022_Analysis/Data/FWRA_2021_RESULTS_MASTER.xlsx", sheet = 1)
head(FWRA)
colnames(FWRA)
FWRA <- subset(FWRA, LF_Number != "23" & LF_Number != "24")

FWRA <- select(FWRA,Watershed,LF_Number,Current_Bio_Risk,Future_Bio_Risk,LF_Name) 

#add "LF" before LF number
FWRA$LF <- paste("LF", FWRA$LF_Number, sep = "")

FWRA$Current_Bio_Risk <- as.character(FWRA$Current_Bio_Risk)
FWRA$Future_Bio_Risk <- as.character(FWRA$Future_Bio_Risk)

#change all numeric values to character
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="1"]<-"VL"
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="2"]<-"L"
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="3"]<-"M"
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="4"]<-"H"
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="5"]<-"VH"
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="0"]<-"LPDG"
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="-1"]<-"HPDG"


FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="1"]<-"VL"
FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="2"]<-"L"
FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="3"]<-"M"
FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="4"]<-"H"
FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="5"]<-"VH"
FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="0"]<-"LPDG"
FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="-1"]<-"HPDG"

#remove all rows with a 0 or -1 in the Current_Bio_Risk or Future_Bio_Risk columns
Risks <- subset(FWRA, Current_Bio_Risk!= "LPDG" & Current_Bio_Risk!= "HPDG" & Future_Bio_Risk!= "LPDG" & Future_Bio_Risk!= "HPDG")


Risks <- Risks %>%
mutate(VH = ifelse(Current_Bio_Risk %in% c("VH"), 1, 0),
VL = ifelse(Current_Bio_Risk %in% c("VL"), 1, 0),
H = ifelse(Current_Bio_Risk %in% c("H"), 1, 0),
M = ifelse(Current_Bio_Risk %in% c("M"), 1, 0),
L = ifelse(Current_Bio_Risk %in% c("L"), 1, 0),
VH_Future_Bio_Risk = ifelse(Future_Bio_Risk %in% c("VH"), 1, 0),
VL_Future_Bio_Risk = ifelse(Future_Bio_Risk %in% c("VL"), 1, 0),
H_Future_Bio_Risk = ifelse(Future_Bio_Risk %in% c("H"), 1, 0),
M_Future_Bio_Risk = ifelse(Future_Bio_Risk %in% c("M"), 1, 0),
L_Future_Bio_Risk = ifelse(Future_Bio_Risk %in% c("L"), 1, 0)) %>%
group_by(LF_Number,LF_Name) %>%
summarise(VH = sum(VH),VH_Future_Bio_Risk = sum(VH_Future_Bio_Risk),
VL = sum(VL),VL_Future_Bio_Risk = sum(VL_Future_Bio_Risk),
H = sum(H),H_Future_Bio_Risk = sum(H_Future_Bio_Risk),
M = sum(M),M_Future_Bio_Risk = sum(M_Future_Bio_Risk),
L = sum(L),L_Future_Bio_Risk = sum(L_Future_Bio_Risk))%>%
arrange(LF_Number)
print(Risks, n = Inf)
head(Risks)
colnames(Risks)

#remove future columns
current_risks <- select(sorted, -VH_Future_Bio_Risk, -VL_Future_Bio_Risk, -H_Future_Bio_Risk, -M_Future_Bio_Risk, -L_Future_Bio_Risk)
current_risks <- current_risks%>%
arrange(desc(VH), desc(H), desc(M),desc(L), desc(VL))

#order columns by VH, H, M, L, VL
current_risks <- current_risks %>%
  select(LF,LF_Name, VH, H, M, L, VL)

print(current_risks)

#future
future_risks <- select(Risks, -VH, -VL, -H, -M, -L)
future_risks <- future_risks%>%
arrange(desc(VH_Future_Bio_Risk), desc(H_Future_Bio_Risk), desc(M_Future_Bio_Risk),desc(L_Future_Bio_Risk), desc(VL_Future_Bio_Risk))

#rename future columns
future_risks <- future_risks%>%rename(LF_Future_Bio_Risk = LF)

#order columns by VH, H, M, L, VL
future_risks <- future_risks %>%
  select(LF_Future_Bio_Risk,LF_Name, VH_Future_Bio_Risk, H_Future_Bio_Risk, M_Future_Bio_Risk, L_Future_Bio_Risk, VL_Future_Bio_Risk)
  

  comb <- cbind(current_risks, future_risks)

#make csv for risk current and future
write.csv(future_risks, file = "/Users/critty/Desktop/Base/GitHub/FWRA_2022_Analysis/Data/Future_Risks_Sorted.csv")
write.csv(current_risks, file = "/Users/critty/Desktop/Base/GitHub/FWRA_2022_Analysis/Data/Current_Risks_Sorted.csv")
write.csv(combined_risks, file = "/Users/critty/Desktop/Base/GitHub/FWRA_2022_Analysis/Data/Current_and_Future_Risks_Sorted.csv")


Data_Gaps <- FWRA %>%
mutate(HPDG = ifelse(Current_Bio_Risk %in% c("HPDG"), 1, 0),
LPDG = ifelse(Current_Bio_Risk %in% c("LPDG"), 1, 0),
  VH = ifelse(Current_Bio_Risk %in% c("VH"), 1, 0),
VL = ifelse(Current_Bio_Risk %in% c("VL"), 1, 0),
H = ifelse(Current_Bio_Risk %in% c("H"), 1, 0),
M = ifelse(Current_Bio_Risk %in% c("M"), 1, 0),
L = ifelse(Current_Bio_Risk %in% c("L"), 1, 0),
HPDG_Future_Bio_Risk = ifelse(Future_Bio_Risk %in% c("HPDG"), 1, 0),
LPDG_Future_Bio_Risk = ifelse(Future_Bio_Risk %in% c("LPDG"), 1, 0),
VH_Future_Bio_Risk = ifelse(Future_Bio_Risk %in% c("VH"), 1, 0),
VL_Future_Bio_Risk = ifelse(Future_Bio_Risk %in% c("VL"), 1, 0),
H_Future_Bio_Risk = ifelse(Future_Bio_Risk %in% c("H"), 1, 0),
M_Future_Bio_Risk = ifelse(Future_Bio_Risk %in% c("M"), 1, 0),
L_Future_Bio_Risk = ifelse(Future_Bio_Risk %in% c("L"), 1, 0)) %>%
group_by(LF_Number,LF_Name) %>%
summarise(HPDG = sum(HPDG),HPDG_Future_Bio_Risk = sum(HPDG_Future_Bio_Risk),
LPDG = sum(LPDG),LPDG_Future_Bio_Risk = sum(LPDG_Future_Bio_Risk),
  VH = sum(VH),VH_Future_Bio_Risk = sum(VH_Future_Bio_Risk),
VL = sum(VL),VL_Future_Bio_Risk = sum(VL_Future_Bio_Risk),
H = sum(H),H_Future_Bio_Risk = sum(H_Future_Bio_Risk),
M = sum(M),M_Future_Bio_Risk = sum(M_Future_Bio_Risk),
L = sum(L),L_Future_Bio_Risk = sum(L_Future_Bio_Risk))%>%
arrange(LF_Number)
print(Risks, n = Inf)
head(Data_Gaps)
colnames(Data_Gaps)

#remove future columns
Current_Data_Gaps <- select(Data_Gaps, -HPDG_Future_Bio_Risk, -LPDG_Future_Bio_Risk, -VH_Future_Bio_Risk, -VL_Future_Bio_Risk, -H_Future_Bio_Risk, -M_Future_Bio_Risk, -L_Future_Bio_Risk)
Current_Data_Gaps <- Current_Data_Gaps%>%
arrange(desc(HPDG),desc(VH), desc(H), desc(M),desc(L), desc(VL))

#order columns by VH, H, M, L, VL
current_risks <- current_risks %>%
  select(LF,LF_Name, VH, H, M, L, VL)

print(current_risks)

#future
future_risks <- select(Risks, -VH, -VL, -H, -M, -L)
future_risks <- future_risks%>%
arrange(desc(VH_Future_Bio_Risk), desc(H_Future_Bio_Risk), desc(M_Future_Bio_Risk),desc(L_Future_Bio_Risk), desc(VL_Future_Bio_Risk))

#rename future columns
future_risks <- future_risks%>%rename(LF_Future_Bio_Risk = LF)

#order columns by VH, H, M, L, VL
future_risks <- future_risks %>%
  select(LF_Future_Bio_Risk,LF_Name, VH_Future_Bio_Risk, H_Future_Bio_Risk, M_Future_Bio_Risk, L_Future_Bio_Risk, VL_Future_Bio_Risk)
  

  comb <- cbind(current_risks, future_risks)

#make csv for risk current and future
write.csv(future_risks, file = "/Users/critty/Desktop/Base/GitHub/FWRA_2022_Analysis/Data/Future_Risks_Sorted.csv")
write.csv(current_risks, file = "/Users/critty/Desktop/Base/GitHub/FWRA_2022_Analysis/Data/Current_Risks_Sorted.csv")
write.csv(combined_risks, file = "/Users/critty/Desktop/Base/GitHub/FWRA_2022_Analysis/Data/Current_and_Future_Risks_Sorted.csv")




library(ggplot2)
library(ggiraph)
library(grid)

# Add row numbers to comb
comb$row <- seq_len(nrow(comb))
colnames(comb)
# Create the data

o <- 0.025
lf_adjustment <- 0.01 # increased adjustment for LF and LF_Future_Bio_Risk groups

DF <- 



# Add a new column to DF that contains the order of each g value
DF$order <- as.numeric(factor(DF$g, levels = unique(DF$g)))

p <- ggplot(DF, aes(x = x, y = y, group = g, label = g)) +
  geom_path_interactive(aes(x = x1, data_id = g), arrow = arrow(length = unit(0.02, "npc")), 
            size = .3, color = "#000000") +
  geom_text_interactive(aes(tooltip = paste0("LF Name: ", LF_Name, "<br>",
                                             "VH: ", VH, "<br>",
                                             "H: ", H, "<br>",
                                             "M: ", M, "<br>",
                                             "L: ", L, "<br>",
                                             "VL: ", VL),
                             data_id = g),
                         size = 2.5) +
  scale_fill_manual(values = c("#000000")) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())+
        scale_y_reverse()+
        #y titles
        annotate("text", x = 1, y = 0, label = "Current Rating", size = 2, hjust = 0.5, vjust = 0.05)+
        annotate("text", x = 2, y = 0, label = "Future Rating", size = 2, hjust = 0.5, vjust = 0.05)+
        # Add an annotation showing the order of each g value next to the y-axis
        annotate("text", x = 0.5, y = 1:length(unique(DF$g)), label = paste0("#", 1:length(unique(DF$g))), 
                 size = 3, hjust = 0.4, vjust = 0.5, color = "#000000")

ggiraph(code = print(p), hover_css = "cursor:pointer;stroke-width:.75px;stroke:#e46511;")

#Save the HTML file
#saveWidget(ggiraph(code = print(p), hover_css = "cursor:pointer;stroke-width:.75px;stroke:#e46511;"), file = "myplot.html")

