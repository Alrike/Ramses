#usr/bin/R
#04.09.2019   Lindsay Scheidemann, 
#performs the integration in bins, so barcharts turn a possibility

#input arguments: 
#wavelength_borders: a numeric vector, length two, containing the 
#                    lower and upper limit of the wavelength range to be binned
#by:                 the range in nm each bin should have
#unit:               integer (1,2), 1 for total Irradiance in mW/m^2
#                            2 for total photon flux in umol/(m^2*s)
#filename:           the file containing the data
#skip                the number of lines to skip, before the data begin

#Output:
#A data frame contining the classes binned by and the Irradiance in the 
#given unit

binned_integration_lightsensor<-function(wavelength_borders = c(280, 720),
                                         by = 20, unit,
                                         filename, skip=78){
  if(!unit %in% c(1,2)){
    cat("unit not known, photon flux is given out", "\n")
    unit <- 2
    }
  data<-read.table(file = filename,  #read file
                   header = FALSE,
                   sep = " ",
                   skip = skip,
                   nrows = 255)
  
  data$lambda <- data$V2 #for calibrated files
  
  #determination of binwidth
  data$binwidth <- rep(NA, length(data$lambda))
  for (i_func in 1:length(data$lambda))
  {
    data$binwidth[i_func] <- data$lambda[i_func+1]-data$lambda[i_func]
  }
  data$binwidth[length(data$lambda)] <- data$binwidth[length(data$lambda)-1]
  #since 3rd order polynomials are not linear,
  #binwidth is calculated manually
  #last binwidth is estimated to equal the previous,
  #since binwidth is slowly decreasing with wavelength within range of interest
  
  #Irradiance from mW/(m^2) to umol/(m^2*s)
  #conversion formula being
  # E(QF)= E*lambda*0.836*10^(-2)
  # with E(QF) Irradiance in umol/(m^2*s), E Irradiance in W/m^2,
  # lambda wavelength in nm, 10^(-3) conversion mW to W;)
  data$IrradianceQF <- data$V3 * 10^(-3) * data$lambda *
    0.836 * 10^(-2)
  
  #next line creates a list, with the individual bin-data as independent dfs
  datatemp   <- split(data,cut(data$lambda,seq(wavelength_borders[1],
                                               wavelength_borders[2],by=by)))
  binnedresults <- data.frame(class=names(datatemp), #start results df
                              Irradiances=rep(NA, length(names(datatemp))))
  for (z_func in names(datatemp)){ #loop over the list elements
    
    #INTEGRATION step
    Irradiance <- 0 # need to be set zero before each Integration
    IrradianceQF <- 0
    int<-datatemp[[z_func]]  #refer the respective part of the list to a full df
    for(k in 1:length(int$lambda))
    {
      #mW/m^2
      Irradiance<-Irradiance+
        int$V3[k]*int$binwidth[k]
      
      #umol/(m^2*s)
      IrradianceQF<-IrradianceQF+
        int$IrradianceQF[k]*int$binwidth[k]
    }
    #write the results into the df
    if (unit == 1){
      binnedresults$Irradiances[binnedresults$class == z_func] <- Irradiance  
    }
    if (unit == 2){
      binnedresults$Irradiances[binnedresults$class == z_func] <- IrradianceQF  
    }
      }  #end of loop over the bins

  return(binnedresults)
  
}