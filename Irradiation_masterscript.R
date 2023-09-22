##usr/bin/R
#21.07.2019   Lindsay Scheidemann
#automated file recognition and sorting added 23.07.2019
#allowing for !simpledata and tidiying done on 19.09.2019
#optional output-file added 28.07.2019
#integration of light profiles for
#visible light

#prepare the function and the reading of the files
setwd("/Users/Lini/Documents/M156_RAMSES")
source("irradiation_script.R")

#give information to work with
unit        = 2             #integer (1,2), 1 for total Irradiance in mW/m^2
                            #               2 for total photon flux in umol/(m^2*s)
numberfiles = 254
filestart   = "Northern_Lander_Station/DAT_SAMIP_ONLY/RAMSES_Northern_Lander_Station_"
fileend     = ".dat"
saveresults = 1             # 1, if you want to have the file, 
                            # 0 (or anything else, if not)
filename    = "RAMSES_Northern_Lander_Station_summary.xls"      #.tsv format
decimalseparator <- "." #choose one suitable for your Excel

simpledata  <- FALSE         #only the sum is returned, if TRUE
extendedoutput <-TRUE      #if true also the binned data are written to the file,
                           #if they were calculated
wavelength_borders <- c(380,720) #where should the bins start/end (380-720 ~ visible light)
                                 #280-720 is the ~range the sensor measures (279-722)
by <- 20                   #how large in nm should the bins be? 
                           #(remember: the difference between two pixels is ~2nm)

###################last intern preparations #######################
if (!simpledata){ #load the stuff only needed if you want more details
  source("irradiation_extended_script.R") #extrafunction
  library(reshape2) #for the plot
  library(ggplot2)
}

#create dataframe for working with
mastertable <- data.frame(Nr         =c(1:numberfiles),         #to be filled later
                          filename   =rep(NA, numberfiles),
                          pressure   =rep(NA, numberfiles),
                          Irradiance =rep(NA, numberfiles),
                          filetype   =rep(NA, numberfiles))
##################looping over the files ####################################
#create filenames and read the pressure as well as filetype
for (m in c(1:numberfiles))
{

  #filename first
  if (m<=1000)
  {
    mastertable$filename[m]<-paste(filestart, 
                                   "00", as.character(m-1), 
                                   fileend, sep="")
  }
  if (m<=100)
  {
    mastertable$filename[m]<-paste(filestart, 
                                   "000", as.character(m-1), 
                                   fileend, sep="")
  }
  if (m<=10)
  {
    mastertable$filename[m]<-paste(filestart, 
                                   "0000", as.character(m-1), 
                                   fileend, sep="")
  }
  if (m>1000)
  {
    print("!!!number to high, please add scriptlines")
  }
  
  #read the line with pressure data from file and save the obtained information
  pressure<-read.table(file=mastertable$filename[m],
                   header = FALSE,
                   sep=" ",
                   skip=382,
                   nrows=1)
  mastertable$pressure[m]<-pressure[1,1]

  #read a line with information on the filetype (raw/calibrated), 
  #extract and save information  
  filetype<-read.table(file=mastertable$filename[m],
                       header= FALSE,
                       sep=" ",
                       skip=36,
                       nrows=1)
  mastertable$filetype[m]<-as.character(filetype$V7[1])
}
############################pre-sorting of data ######################
#take only the downward cast of the sensor
#take the calibrated files only
end<-which.max(mastertable$pressure)
interestingfiles<-mastertable[1:end,]
interestingfiles<-subset(interestingfiles, interestingfiles$filetype=="CALIBRATED")

#add more informative numbers
#  #unitconversion
# pressure in hektopascal to dbar 
# which ~ equals depth in m 
#(sufficiently exact for shallow depth, offset increases ~1cm/m) 
interestingfiles$depth<-interestingfiles$pressure*10

############################do the calculations ##############################
#calculate the irradiance by using the function
#depth rounded, to achieve better plot captions
#function(filename, desired unit (1 or 2), depth(for the title))
for (p in 1:length(interestingfiles$filename))
{
  interestingfiles$Irradiance[p]<-Integration_lightsensor(interestingfiles$filename[p],
                                                          unit, 
                                                          round(interestingfiles$depth[p],2))
  
  if(!simpledata){ #call to the specific function to obtain the binned results
    
    if(p == 1){ #first call to function, initialize df
      moredetails <- binned_integration_lightsensor(wavelength_borders = wavelength_borders,
                                                    by=by, unit=unit,
                                                    filename= interestingfiles$filename[p])
      
      names(moredetails) <- c("class", #assign some names
                              paste(p))
    }else{#when the df already exists, create a temporary one, to merge with the rest of the data
      temp<-binned_integration_lightsensor(wavelength_borders = wavelength_borders,
                                           by=by, unit=unit,
                                           filename= interestingfiles$filename[p])
      names(temp) <- c("class",
                       paste(p))
      moredetails <- merge(moredetails, temp, by="class") #put them together again
    }
    
  } #end of if(!simpledata)
}
#######################formatting#############################################
interestingfiles$percent<- interestingfiles$Irradiance/(max(interestingfiles$Irradiance)) *100
start<-which.max(interestingfiles$percent)
interestingfiles<-interestingfiles[start:nrow(interestingfiles),]
interestingfiles$rownum <- c(1:nrow(interestingfiles)) #only turns impotant for !simpledata

#plot the respective results
#here unit needs to be adjusted to setting first
if(unit==1)
{xtext="Irradiance in mW/(m^2)"
 ytext="Irradiance in mW/(m^2)"}
if(unit==2)
{xtext="Photon flux in umol/(m^2*s)"
 ytext="Photon flux in umol/(m^2*s)"}
if (!unit %in% c(1,2))
{xtext="I don't know the unit, sorry..."}
plot((-interestingfiles$depth)~interestingfiles$Irradiance,
     ylab = "depth in m", xlab=xtext,
     main="Irradiance profile")

if(!simpledata){
  #formatting for output
  details <- t(moredetails) #turn the df, in this process df becomes a matrix!!
  colnames(details) <- details[1,] #turn the classes to colnames
  details <- data.frame(details)# redo the formatting
  details <- details[!row.names(details) %in% c("class"), ] #delete class row
  details <- cbind(rownum = rownames(details), details) #turn the rownums into a simple column
  details <- subset(details, subset = rownum %in% interestingfiles$rownum) #delete extra rows
  interestingfiles <- merge(interestingfiles, details, by = "rownum") #add to mastertable
  
  #formatting for plotting
  moredetails <- subset(moredetails, select= c("class",interestingfiles$rownum)) #erase "extra-entrys
  vec<-round(interestingfiles$depth, 1) #read the depths belonging to rowcounts
  colnames(moredetails)<-c("class", vec) #rename the columns
  
  #prepare for plotting
  plotdf<- melt(moredetails, id.vars = c("class")) 
  #from the reshape2 package turns this into a long df: class, depth, value
  #prepare the colour-palette manually, this is in shades of grey
  colbasic<- seq(100,0, length.out = length(vec)) #create sequence from white to black
  colvec <- rep(NA, length(vec)) #build the colour names
  for (count in 1:length(vec)){
    colvec[count] <- paste("grey",round(colbasic[count]), sep = "")
  }
  
  #actual plotting in ggplot
  plot<-ggplot(data=plotdf,aes(x=class, y=value, fill=variable, #initial call to ggplot
                               color=variable, alpha=variable)) +
    geom_bar(stat="identity")+  #adding all the layers (to lazy to put them into variables)
    scale_alpha_manual(values = seq(1, 1, length.out = length(vec)))+ #transparency would counteract the color gradient
    scale_fill_manual(values = colvec)+
    scale_colour_manual(values = rep("lightblue", length(vec)))+
    theme_bw()+
    theme(axis.text.x.bottom = element_text(size=7))+
    labs(x = "wavelength in nm", y = ytext)
  
  print(plot) #otherwise ggplots don't show, when a script is running
}

#######################saving #########################
#save the results into specified file, if requested
#tab seperated format suitable for german Excel
if(saveresults==1)
{
  #if simpledata, or (not simpledata and not extendedoutput)
  if(simpledata | (!(simpledata) & !(extendedoutput))){
    table<-subset(interestingfiles, select=c(Irradiance, depth, percent))
  }else{ # only if not simpledata and extendedoutput
    table<-interestingfiles  #take everything, although this also contains rownum...
  }
  write.table(table, file = filename, 
              sep = "\t", dec = decimalseparator,
              row.names = F, col.names = T)
}