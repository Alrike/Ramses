#usr/bin/R
#16.09.2019
#reading and comparing uncalibrated and calibrated data lines

pixelnum <- 147
file_out <-"raw_and_cal_data"

library(ggplot2)
gliderstation<-readRDS("gliderstation.RDS")
datestation<-readRDS("datestation.RDS")
landerstation<-readRDS("landerstation.RDS")
northernstation<-readRDS("Northern_Lander.RDS")

for(i in 1:3){
  if (i==1){workdf <- gliderstation; file = "Glider_Recovery_Station/Glider_Recovery_Station.dat"}
  if (i==2){workdf <- datestation; file = "31-07-2019/31-07-2019.dat"}
  if (i==3){workdf <- landerstation; file = "BBL_Lander_Station/BBL_Lander_Station.dat"}
  outdf<-data.frame(depth = rep(NA, 140),
                    cal = rep(NA, 140), raw = rep(NA, 140),
                    number = rep(NA, 140), InclinationX = rep(NA, 140),
                    InclinationY = rep(NA, 140), InclinationV = rep(NA, 140),
                    integration_time = rep(NA, 140))
  for (rowcount in workdf$rownum){
    outdf$depth[rowcount] <- read.table(file = file,
                                   header = FALSE,
                                   sep = " ",
                                   skip = workdf$line_pressure[rowcount],  #skip only until "[Data]"
                                   nrows = 1)[1,1]*10
    inclination <- read.table(file = file,
                                        header = FALSE,
                                        sep = " ",
                                        skip = workdf$line_pressure[rowcount]-25,  #skip only until "[Data]"
                                        nrows = 1)
    
    outdf$InclinationX[rowcount]<-inclination$V1
    outdf$InclinationY[rowcount]<-inclination$V2
    outdf$InclinationV[rowcount]<-inclination$V3
    
    outdf$cal[rowcount] <- read.table(file = file,  #read file
                                      header = FALSE,
                                      sep = " ",
                                      skip = workdf$line_spectrum[rowcount]+pixelnum, #line for 500nm
                                      nrows = 1)$V3[1]
    outdf$integration_time[rowcount] <- read.table(file = file,  #read file
                                      header = FALSE,
                                      sep = " ",
                                      skip = workdf$line_spectrum[rowcount]-17, 
                                      nrows = 1)$V3[1] #line for Integration time
    outdf$number[rowcount] <- read.table(file = file,  #read file
                                      header = FALSE,
                                      sep = " ",
                                      skip = workdf$line_spectrum[rowcount], #line for 0X00
                                      nrows = 1)$V3[1]
    outdf$raw[rowcount] <- read.table(file = file,  #read file
                                      header = FALSE,
                                      sep = " ",
                                      skip = workdf$line_uncal[rowcount]+pixelnum, #line for 500nm
                                      nrows = 1)$V3[1]
  }
  if (i==1){gliderresults <- outdf}
  if (i==2){dateresults <- outdf}
  if (i==3){landerresults <- outdf}
}
workdf <- northernstation
rawdf  <- data.frame(depth = rep(NA, 254), raw = rep(NA, 254),rownum=(1:254))
caldf  <- data.frame(depth = rep(NA, 254), cal = rep(NA, 254),
                     number = rep(NA, 254),  rownum=(1:254), inclinationX = rep(NA, 254),
                     InclinationY = rep(NA, 254), InclinationV = rep(NA, 254),
                     integration_time = rep(NA, 254))
for(rowcount in 1:nrow(northernstation)){
  if (workdf$filetype[rowcount] == "RAW"){
    rawdf$depth[rowcount] <- workdf$pressure[rowcount]*10
    rawdf$raw[rowcount] <- read.table(file = workdf$filename[rowcount],  #read file
                                      header = FALSE,
                                      sep = " ",
                                      skip = 77+pixelnum, #line for 500nm
                                      nrows = 1)$V3[1]
  }else{
    caldf$depth[rowcount] <- workdf$pressure[rowcount]*10
    inclination <- read.table(file = workdf$filename[rowcount],
                              header = FALSE,
                              sep = " ",
                              skip = 357,  #skip only until "[Data]"
                              nrows = 1)
    
    caldf$InclinationX[rowcount]<-inclination$V1
    caldf$InclinationY[rowcount]<-inclination$V2
    caldf$InclinationV[rowcount]<-inclination$V3
    caldf$cal[rowcount] <- read.table(file = workdf$filename[rowcount],  #read file
                                      header = FALSE,
                                      sep = " ",
                                      skip = 77+pixelnum, #line for 500nm
                                      nrows = 1)$V3[1]
    caldf$number[rowcount] <- read.table(file = workdf$filename[rowcount],  #read file
                                      header = FALSE,
                                      sep = " ",
                                      skip = 77, 
                                      nrows = 1)$V3[1]
    caldf$integration_time[rowcount] <- read.table(file = workdf$filename[rowcount],  #read file
                                      header = FALSE,
                                      sep = " ",
                                      skip = 60, 
                                      nrows = 1)$V3[1]
  }
}
rawdf <- subset(rawdf, rawdf$rownum%%2 == 1)#every first was raw data
caldf <- subset(caldf, caldf$rownum%%2 == 0)#every second was calibrated data
northernresults <- merge(rawdf, caldf, by="depth")


plot(northernresults$cal~northernresults$raw,
     col="red", pch=8,
     ylab="calibrated numbers", xlab="raw data", xlim=c(0,60000))
points(dateresults$cal~dateresults$raw, col="blue", pch=4)
points(gliderresults$cal~gliderresults$raw, col="black", pch=1)
points(landerresults$cal~landerresults$raw, col="darkgreen", pch=5)
legend("topleft", 
       legend=c("Glider Recovery", "31-07-2019", "BBL Lander", "Northern Lander"),
       col=c("black", "blue", "darkgreen", "red"), pch=c(1, 4, 5, 8), cex= 0.65)

value <- read.table(file = file,  #read file
                                  header = FALSE,
                                  sep = " ",
                                  skip = landerstation$line_spectrum[50]+pixelnum, #line for 500nm
                                  nrows = 1)
print(value)

landerresults<-na.omit(landerresults)
landerresults$station<-rep("BBL Lander", nrow(landerresults))
gliderresults<-na.omit(gliderresults)
gliderresults$station<-rep("Glider Recovery", nrow(gliderresults))
dateresults<-na.omit(dateresults)
dateresults$station<-rep("31-07-2019", nrow(dateresults))
northernresults<-subset(northernresults,
                        select=c(depth, cal, raw, number, InclinationX,
                                 InclinationY, InclinationV, integration_time))
northernresults$station<-rep("Northern Lander", nrow(northernresults))
all_results<-rbind(landerresults, gliderresults, dateresults, northernresults)
sorteddf<-all_results[order(all_results$raw, decreasing=TRUE),]

write.table(sorteddf, file = file_out, 
                         sep = "\t", dec = ".",
                         row.names = F, col.names = T)
plot<-qplot(x=raw, y=cal, data=all_results, geom="point", 
            shape=as.character(integration_time), colour=station)
print(plot)

