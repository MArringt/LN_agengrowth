# Read in datafile
Skate_data <- read.csv("//akc0ss-n086/REFM_AgeUnit/AGEUNIT/Arrington/Longnose Specimen Data/Race15_IPHC1314_Obs0917_NWFSC.csv")
Skate_data$DATA_SOURCE <- as.factor(Skate_data$DATA_SOURCE)
Skate_data$TOTAL_LENGTH_cm <- as.numeric(Skate_data$TOTAL_LENGTH_cm)

# Load packages
library(dplyr)
library(ggplot2)
library(ggmap)

# Set plot look
mytheme <- theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_line(colour="grey80", linetype="dashed"))
mytheme <- mytheme + theme(text=element_text(size=12)) + theme(axis.title.x=element_text(size=16) ,axis.title.y=element_text(size=16))
mytheme <- mytheme + theme(panel.background = element_rect(fill="transparent", color = NA), panel.border = element_rect(colour="black",fill=NA,size=1.2), plot.background = element_rect(fill = "transparent",color = NA), legend.background = element_rect(fill = NA, color = NA))


#Check lengths and weights
plot(Skate_data$WEIGHT, Skate_data$TOTAL_LENGTH_cm)
?plot

# Plot some stuff

## Specimen length distribution
lengths <- Skate_data %>%
  group_by(DATA_SOURCE, TOTAL_LENGTH_cm) %>%
  count(TOTAL_LENGTH_cm)
  
ggplot(lengths)+
  geom_line(aes(TOTAL_LENGTH_cm, y = n, group = DATA_SOURCE, color = DATA_SOURCE)) +
  mytheme


## What are my latitude breaks based on cut offs?

maplocation <- c(-130,30,-105,50)
sampmap <- get_map(location = maplocation, source = "google", maptype = "satellite", crop = FALSE)
ggmap(sampmap) +
  geom_hline(yintercept=34.4486, linetype = "dashed", color = "darkred") +
  geom_hline(yintercept=40.30, linetype = "dashed", color = "darkred") +
  geom_hline(yintercept=43.00, linetype = "dashed", color = "darkred") +
  geom_hline(yintercept=47.30, linetype = "dashed", color = "darkred")
  

### South of Point Conception 34.4486'N
## Filter data
S_PC <- filter(Skate_data, LATITUDE < 34.4486)
spc_loca <- c(lon = -120.47, lat = 34.45)

## Map data
spc_map <- get_map(location = spc_loca, source = "google", maptype = "satellite", zoom = 7, crop = FALSE)
ggmap(spc_map) +
  geom_point(aes(x = LONGITUDE, y = LATITUDE), data = S_PC,
             alpha = .5, color = "darkred", size = 3)

# Length distribution
ggplot(S_PC) +
  geom_bar(aes(x = TOTAL_LENGTH_cm)) +
  mytheme

# Write to csv
write.csv(x = S_PC, file = "Data_South_PtConception.csv")

### Between Point Conception 34.4486'N and Cape Mendocino 40 30'N

PC_CM <- filter(Skate_data, LATITUDE > 34.4486 & LATITUDE < 40.30)
pc_cm_samp <- sample_n(PC_CM, 150, weight = TOTAL_LENGTH_cm)

### Between Cape Mendocino 40 30'N and Cape Blanco 43 00'

### Between Cape Blanco 43 00'N and Cape Flattery/ Columbia River 47 30'N





### ECW GOA - ask Olav, get back to this.
       