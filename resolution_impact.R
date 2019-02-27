#Author: Balazs Dienes
#Contact: dienes.balazs88@gmail.com

#This is an exemplary work to demonstrate the processing of solar irradiance data on a selected wall surface.
#Identical steps are conducted on files representing low and a high spatial resolution.
#Notable irradiance values are then visualized together for the two files.

#Please note:
#Solar irradiance data was produced by the Solar Energy on Building Envelopes (SEBE) model.
#For details about SEBE please follow the link below:
#https://umep-docs.readthedocs.io/en/latest/processor/Solar%20Radiation%20Solar%20Energy%20on%20Building%20Envelopes%20(SEBE).html

##########################################################
### Locating wall pixels in the solar irradiance files ###
##########################################################


#1.) Low resolution input raster - 1m ##

#1.1 Read table
IrradiationValues_Med_1m <- read.table("irradiance_total_low_resolution.txt", header = FALSE, skip = "1", col.names = paste0("v",seq_len(77)))
colnames(IrradiationValues_Med_1m) <- c("row", "col", paste0("m",seq_len(75)))

#1.2 Identify wall location in file based on wall pixel coordinates
XMinMed1m <- 385963.489
YMaxMedl1m <- 5819620.024
CellsizeMed1m <- 1
VectorXMed1m <- c(386289.869, 386289.968, 386290.999, 386291.033, 386291.931, 386291.898)
VectorYMed1m <- c(5819492.641, 5819482.330, 5819481.731, 5819470.223, 5819469.658, 5819468.361)
RowMed1m <- (ceiling(YMaxMedl1m-VectorYMed1m))/CellsizeMed1m # %row
ColMed1m <- (ceiling(VectorXMed1m-XMinMed1m))/CellsizeMed1m # col
cbind(RowMed1m,ColMed1m)

#1.3 Slice and write wall pixels
Wall13 <- subset(IrradiationValues_Med_1m, 
                 (col == 327 & (row >= 128 & row <= 138)) |
                   (col == 328 & (row >= 139 & row <= 150)) | 
                   (col == 329 & (row >= 151 & row <= 152)))

Wall13
write.table(Wall13, "wall_low_resolution.txt", quote = TRUE, sep = "\t", row.names = FALSE)

#2.) High resolution input raster - 0.5m ##

#2.1 Read table
IrradiationValues_Med_05m <- read.table("irradiance_total_high_resolution.txt", header = FALSE, skip = "1", col.names = paste0("v",seq_len(153)))
colnames(IrradiationValues_Med_05m) <- c("row", "col", paste0("m",seq_len(151)))
summary(IrradiationValues_Med_05m)

#2.2 Identify wall location in file based on wall pixel coordinates
XMinMed05m <- 385963.489
YMaxMedl05m <- 5819620.024
CellsizeMed05m <- 0.5
VectorXMed05m <- c(386289.149, 386289.190, 386289.707, 386289.728, 386290.224, 386290.244, 386290.761, 386290.637, 386291.195, 386291.133)
VectorYMed05m <- c(5819492.353, 5819491.816, 5819491.299, 5819485.781, 5819485.409, 5819479.209, 5819478.837, 5819473.194, 5819472.884, 5819467.738)
RowMed05m <- ceiling((YMaxMedl05m-VectorYMed05m)/CellsizeMed05m) # %row
ColMed05m <- ceiling((VectorXMed05m-XMinMed05m)/CellsizeMed05m) # col
cbind(RowMed05m,ColMed05m)

#2.3 Slice and write wall pixels
Wall1 <- subset(IrradiationValues_Med_05m, 
                (col == 652 & (row >= 256 & row <= 257)) |
                  (col == 653 & (row >= 258 & row <= 269)) | 
                  (col == 654 & (row >= 270 & row <= 282)) | 
                  (col == 655 & (row >= 283 & row <= 294)) | 
                  (col == 656 & (row >= 295 & row <= 305)))
Wall1
write.table(Wall1, "wall_high_resolution.txt", quote = TRUE, sep = "\t", row.names = FALSE)

###################################################
### Identification of notable irradiance values ###
###################################################

#Irradiance values were compared at four "notable locations":
#northern location,
#southern location,
#sensor location (irradiation values were validated by ground measurements),
#and at the mean of all irradiation values at certain heights.

#Comparison was conducted at each 0.5 meter of wall height.

#1.) Irradiance values based on 0.5m resolution

#Read table:
InputTable1 <- read.table("wall_high_resolution.txt", header = TRUE)
InputTable1

#Calculation of mean irradiance:
TableTransposed1 <- as.data.frame(t(InputTable1))
EnergyMean1 <- rowMeans(TableTransposed1)
TableTransposedMean1 <- cbind(TableTransposed1, EnergyMean1)
Height1 <- c(0,0,1:(nrow(TableTransposedMean1)-2))
Height1 <- Height1*0.5
TableTransposedHeight1 <- cbind(TableTransposedMean1, Height1)

#Identification of irradiance values at notable locations:
NotableValues1 <- TableTransposedHeight1[3:30,c(1,24,50,51,52)]
colnames(NotableValues1) <- c("EnergyNorth", "EnergySensor", "EnergySouth", "EnergyMean", "Height")
SpatialResolution <- rep("0.5m", times = 28)
NotableValues1_05m <- cbind(NotableValues1, SpatialResolution)

#2.) Irradiance values based on 1m resolution
#Read table:
InputTable2 <- read.table("wall_low_resolution.txt", header = TRUE)
InputTable2

#Calculation of mean irradiance:
TableTransposed2 <- as.data.frame(t(InputTable2))
EnergyMean2 <- rowMeans(TableTransposed2)
TableTransposedMean2 <- cbind(TableTransposed2, EnergyMean2)
Height2 <- c(0,0,1:(nrow(TableTransposedMean2)-2))
TableTransposedHeight2 <- cbind(TableTransposedMean2, Height2)

#Identification of irradiance values at notable locations:
NotableValues2 <- TableTransposedHeight2[3:16,c(1,12,25,26,27)]
colnames(NotableValues2) <- c("EnergyNorth", "EnergySensor", "EnergySouth", "EnergyMean", "Height")
SpatialResolution <- rep("1m", times = 14)
NotableValues2_1m <- cbind(NotableValues2, SpatialResolution)

NotableValuesTotal <- rbind(NotableValues1_05m, NotableValues2_1m)

##################################
### Resolution comparison plot ###
##################################

install.packages('ggplot2')
library(ggplot2)
PlotTest <- ggplot(data = NotableValuesTotal, aes(x=value, y=NotableValuesTotal$Height, shape=SpatialResolution)) +
  geom_point(aes(x=NotableValuesTotal$EnergyNorth, color="North"),size=2) +
  geom_point(aes(x=NotableValuesTotal$EnergySensor, color="Sensor"),size=2) +
  geom_point(aes(x=NotableValuesTotal$EnergySouth, color="South"),size=2) +
  geom_point(aes(x=NotableValuesTotal$EnergyMean, color="Mean"),size=2) +
  geom_line(aes(x=NotableValuesTotal$EnergyNorth, color="North")) +
  geom_line(aes(x=NotableValuesTotal$EnergySensor, color="Sensor")) +
  geom_line(aes(x=NotableValuesTotal$EnergySouth, color="South")) +
  geom_line(aes(x=NotableValuesTotal$EnergyMean, color="Mean")) +
  labs(x="Energy (kWh)", y="Height (m)", title="Comparison of spatial resolution (0.5m v 1m)", subtitle="AOI: medium; met. station: TUmain; time span: 27.Aug.2014 - 02.Sept.2014; albedo: 0.2") +
  theme(plot.subtitle=element_text(size=12, face="italic", color="black")) +
  labs(color="Location")
print(PlotTest)
ggsave("resolution_impact.png", dpi = 600, width = 8, height = 6, units = "in")
