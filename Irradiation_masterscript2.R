#usr/bin/R
#original 28.08.2019   Lindsay Scheidemann
#rewritten 4.9.2019, tidied 19.09.2019
#integration of light profiles for
#visible light

#prepare libraries and functions
setwd("/Users/Lini/Documents/M156_RAMSES")
source("irradiation_script.R")
library(beepr)

#give information to work with
in_filename <- "31-07-2019/31-07-2019.dat"
saveresults <- TRUE       #TRUE, if you want to have the file, else FALSE
out_filename    <- "Test.xls"      #.tsv format
decimalseparator <- "."  #choose one that fits your Excel

unit        <- 1             #integer (1,2), 1 for total Irradiance in mW/m^2
                             #2 for total photon flux in umol/(m^2*s)
simpledata  <- FALSE         #only the sum is returned, if TRUE
  extendedoutput <-TRUE      #if true also the binned data are written to the file,
                             #if they were calculated
  wavelength_borders <- c(380,720) #where should the bins start/end (380-720 ~ visible light)
                             #280-720 is the ~range the sensor measures (279-722)
  by <- 20                   #how large in nm should the bins be? 
                             #(remember: the difference between two pixels is ~2nm)

#######################################last intern preparations####################
if (!simpledata){ #load the stuff only needed if you want more details
  source("irradiation_extended_script.R") #extrafunction
  library(reshape2) #for the plot
  library(ggplot2)
}

#set texts for graphs
if(unit == 1)
{xtext="Irradiance in mW/(m^2)"
 ytext="Irradiance in mW/(m^2)"
}
if(unit == 2)
{xtext="Photon flux in umol/(m^2*s)"
 ytext="Photon flux in umol/(m^2*s)"
}
if (!unit %in% c(1,2))
{unit=2
 print("Unit is not known, set to default: Photon flux")
}

#prepare dataframes for information
information_spectrum <- data.frame(time = rep(NA, 200),
                                   line_spectrum = rep(NA, 200),
                                   line_uncal = rep(NA, 200))
information_pressure <- data.frame(time = rep(NA, 400),
                                   line_pressure = rep(NA, 400), p = rep(NA, 400))
#if "subscript out of bounds" => try increasing these numbers, you may have made 
#to many single measurements and each needs a line to be saved in

print("Please be patient with me! This takes some time...")
print(Sys.time())
all_lines <- read.table(in_filename, sep = "\t", header = FALSE,
                       blank.lines.skip = FALSE)    #otherwise the rowcounts are wrong
#######################working on the file########################################
#initiate counts
pline <- 1 #pressure linecount  because there are two pressure entries per spectrum entry
sline <- 1 #spectrum linecount  except for the last spectrum entry, which simply has one  
line  <- 1 # general linecount

while(line <= nrow(all_lines)){           #while loop allows more modification of the count
  
  if(all_lines[line, ] == "[Spectrum]"){  #light data??
    
    if(all_lines[line+5, ] == "IDDataTypeSub1     = CALIBRATED"){  #if yes, calibrated??
      lengthnum <- nchar(as.character(all_lines[line+8, ]))        #DateTime line
      information_spectrum$time[sline] <- substr(as.character(all_lines[line+8, ]),
                                             lengthnum-7, lengthnum)
      information_spectrum$line_spectrum[sline] <- line +45 #this notes the line where "[DATA]" is
      sline <- sline+1       #use the next row for the next data point
      line  <-  line+301     #jump to line before [END] of [DATA]
    }else {                  #Spectrum data, but not calibrated
      information_spectrum$line_uncal[sline] <- line +45 #I keave it in in case you'd like to do more calibration checks
      line <- line+301 #jump to line before [END] of [DATA]
    }
  }
  
  if(line > nrow(all_lines)){print("Debugging or Testing?"); break} #only happens if you try to read part of the file
  
  if(all_lines[line, ] == "[Pressure]"){ #pressure data
    lengthnum <- nchar(as.character(all_lines[line+8, ]))
    information_pressure$time[pline] <- substr(as.character(all_lines[line+8, ]),
                                               lengthnum-7, lengthnum)
    information_pressure$line_pressure[pline] <- line+20 #[DATA]-line
    information_pressure$p[pline] <- pline
    pline <- pline +1            #new row for the next entry
    line  <-  line +22           #jump to the line before [END] of [DATA]-line
  }
  
  if(line > nrow(all_lines)){print("Debugging or Testing?"); break} #break the loop early, if line increased to high
  #only necessary for test runs with "small" parts of the file
  
  if(all_lines[line, ] == "Version            = 1"){ #second line of each data block
    #consequence: the line is in a block, that is not pressure or spectrum data...save loop runs
    line <- line +21 
  }
  
  line <- line + 1 #in any case advance one line 
  #(make sure to not end in an infinite loop)
}

##############################interim processing and formatting###################
#processing of the obtained data, preparation of the next steps
information_pressure <- information_pressure[1:pline-1, ] #take only the informative lines
information_spectrum <- information_spectrum[1:sline-1, ] #-1, because count is increased by
                                                          # one after each entry
information_pressure <- subset(information_pressure,
                               information_pressure$p%%2 == 1) #keep every first pressure entry (=odd rownums only)
information_total    <- merge(information_pressure,
                              information_spectrum, by="time") #time is used to make sure points don't get mixed up

print("Scanning process completed...I'm over to integration now...")
mastertable <- data.frame(time = information_total$time, #initiate the dataframe
                          pressure   = rep(NA, length(information_total$time)),
                          Irradiance = rep(NA, length(information_total$time)))

############################ calculating the results ########################
for(rowcount in 1:nrow(information_total)) { #loop over the lines of the df
  
  mastertable$pressure[rowcount] <- read.table(file = in_filename,
                                               header = FALSE,
                                               sep = " ",
                                               skip = information_total$line_pressure[rowcount],  #skip only until "[Data]"
                                               nrows = 1)[1,1] #R reads the number as a matrix, 
                                               #due to the properties of read.table
  mastertable$Irradiance[rowcount] <- Integration_lightsensor(filename=in_filename, #call the script
                                                              unit = unit, 
                                                              info = round(mastertable$pressure[rowcount]*10, 2),
                                                              skip = information_total$line_spectrum[rowcount])
  
    if(!simpledata){ #call to the specific function to obtain the binned results
      
    if(rowcount == 1){ #first call to function, initialize df
      moredetails <- binned_integration_lightsensor(wavelength_borders = wavelength_borders,
                                                  by=by, unit=unit,
                                                  filename= in_filename,
                                                  skip= information_total$line_spectrum[rowcount])

      names(moredetails) <- c("class", #assign some names
                              paste(rowcount))
    }else{#when the df already exists, create a temporary one, to merge with the rest of the data
      temp<-binned_integration_lightsensor(wavelength_borders = wavelength_borders,
                                                  by=by, unit=unit,
                                                  filename= in_filename,
                                                  skip= information_total$line_spectrum[rowcount])
      names(temp) <- c("class",
                       paste(rowcount))
      moredetails <- merge(moredetails, temp, by="class") #put them together again
    }
      
  } #end of if(!simpledata)
}#end of for Loop

######################## formatting ######################################
cat("\t", "Max irradiation is", "\t", max(mastertable$Irradiance), "\n")
mastertable$rownum <- c(1:nrow(mastertable)) #only turns impotant for !simpledata
end <- which.max(mastertable$pressure) #take "unconverted" data for the cut
mastertable$percent <- mastertable$Irradiance/(max(mastertable$Irradiance)) *100
start <- which.max(mastertable$percent)  #surface measurement
mastertable <- mastertable[start:end, ] #the cast with the highest measurement only, 
                                        #cut air measurements, if necessary
mastertable$depth <- mastertable$pressure*10 # pressure in hektopascal to dbar
mastertable <- mastertable[order(mastertable$depth), ] #sort by depth

if(!simpledata){
  #formatting for output
  details <- t(moredetails) #turn the df, in this process df becomes a matrix!!
  colnames(details) <- details[1,] #turn the classes to colnames
  details <- data.frame(details)# redo the formatting
  details <- details[!row.names(details) %in% c("class"), ] #delete class row
  details <- cbind(rownum = rownames(details), details) #turn the rownums into a simple column
  details <- subset(details, subset = rownum %in% mastertable$rownum) #delete extra rows
  mastertable <- merge(mastertable, details, by = "rownum") #add to mastertable
  
  #formatting for plotting
  moredetails <- subset(moredetails, select= c("class",mastertable$rownum)) #erase "extra-entrys
  vec<-round(mastertable$depth, 1) #read the depths belonging to rowcounts
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

#plot the respective results
plot(mastertable$depth ~ mastertable$Irradiance,
     ylab = "depth in m", xlab = xtext,
     main = "Irradiance profile", ylim = c(80,0))

######################### saving #######################
#save the results into specified file, if requested
#tab seperated format suitable for german Excel
if(saveresults)
{
  #if simpledata, or (not simpledata and not extendedoutput)
  if(simpledata | (!(simpledata) & !(extendedoutput))){
    table<-subset(mastertable, select=c(Irradiance, depth, time, percent))
  }else{ # only if not simpledata and extendedoutput
    table<-mastertable  #take everything, although this also contains rownum...
  }
  write.table(table, file = out_filename, 
              sep = "\t", dec = decimalseparator,
              row.names = F, col.names = T)
}

#end of script stuff
print("I'm done now!! Results!!") 
print(Sys.time()) 
beep(sound = 2)

