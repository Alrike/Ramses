#usr/bin/R
#21.07.2019   Lindsay Scheidemann, 
#photon flux added and minor changes in style 23.07.2019, renaming i 4.9.

#integration of light profiles for
#visible light

#contains function Integration_lightsensor
#Input: 
#filename (.dat from Ramses lightsensor data export)
#unit      integer (1,2), 1 for total Irradiance in mW/m^2
#                         2 for total photon flux in umol/(m^2*s)
#info      anything that should be included into the graphics title

Integration_lightsensor <- function(filename, unit, info = -3, skip = 77)
{data<-read.table(file = filename,  #read file
           header = FALSE,
           sep = " ",
           skip = skip,
           nrows = 255)

data$lambda <- data$V2 #for calibrated files

#convert pixel to wavelength            #for raw files
#function    lambda(n)=C0s+c1s*n+c2s*n^2+c3s*n^3
#where n = pixel (1 to 256), C0s,c1s,c2s, and c3s are constants (see manual)
#data$pixel<-data$V2+1 #because 0-255 in data, but 1-256 in function

#data$lambda<-185.218+
#  data$pixel*2.1515+
#  (data$pixel^2)*-5.9257*10^-5+
#  (data$pixel^3)*-6.41524*10^-7
#### if this needs being done, raw files also need to be calibrated!!!

#include only visible light (350-750nm), PAR is 400-700nm
#datacomplete<-data  #copied over in case these are required later
data <- subset(data, data$lambda >= 380)
data <- subset(data, data$lambda <= 725)

#checkplot
#this will appear inside the plot window, whenever the function is called
plot(data$V3~data$lambda,
     xlab = "lambda(nm)", ylab = "intensity(mW/(m^2*nm)",
     xlim = c(380, 700),
     main = paste("RAMSES profile", info, "m", sep = " "))


#first crude integration:
#rectangular fit

#determination of binwidth
data$binwidth <- rep(NA, length(data$lambda))
for (i_func in 1:length(data$lambda))
  {data$binwidth[i_func] <- data$lambda[i_func+1]-data$lambda[i_func]
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

#INTEGRATION step
Irradiance <- 0
IrradianceQF <- 0
for (k in 1:length(data$lambda))
{Irradiance <- Irradiance+ #mW/m^2
    data$V3[k] * data$binwidth[k]

  #umol/(m^2*s)
  IrradianceQF <- IrradianceQF +
    data$IrradianceQF[k] * data$binwidth[k]
}


#return the right value in dependency of the desired unit
if (unit == 1)
  {#cat("\n","Observed total irradiance is", Irradiance, "mW/m2")
   return(Irradiance)
  }
if (unit == 2)
{#cat("\n","Observed total photon flux is", IrradianceQF, "umol/(m2*s)")
  return(IrradianceQF)
  }
if (!unit %in% c(1,2))
  {cat("\t\t","Wrong unit!!!", "\n", "possible units are:", "\n",
     "1", "\t", "mW/m^2", "\t", "total Irradiance", "\n",
     "2", "\t", "umol/(m^2*s)", "\t", "total photon flux", "\n",
     "NA is returned")
  return(NA)
  }

}
