#Liana's model 

library(ggplot2)
library(help='ggplot2')

#germination

Gh = data.frame(1:nrow(Th))

Gh <- sapply(1:nrow(Th), function(i) {
  mpsib = psib + kT*(temp[i,]-To)
  if (temp[i,] <= To & psi[i,] > psib) {Gh[i,1] = (psi[i,]-psib)*(temp[i,]-Tb.g)}
  else if (temp[i,] > To & psi[i,] > mpsib) {Gh[i,1] = (psi[i,]-mpsib)*(To-Tb.g)}
  else {Gh[i,1] = 0}
})

#Th = df of hour-averaged temps
#Temp = df of temperatures
#Tb.g = base temp below which seed no longer progresses towards germination
#To = optimal germination temp

#Psi = df of hour-averaged moisture 
#psi = Psi[i,] = moisture at hour i
#psib = base moisture level below which seed no longer progresses towards germination (proxy for dormancy level of the seed)
#msib = 
#kT = empirically derived scalar that describes shift in psib for each degree above current temp



#CHEW ET AL MODEL 
#calculates modified photothermal unit (MPTU) from parameterized per genotype environmental inputs

#time = df of hours being considered
#hours = length of time under consideration
hours = 8000
time = data.frame(1:hours)

#photoperiod
#dl = df of daylengths for each hour
#generate daylength distributions
dl = rnorm(nrow(time), mean = 11, sd = 4)
dl = as.data.frame(dl)

#critical short day & long day lengths
CSDL = 8
CLDL = 14

#minimum and max rates
Dsd = 3
Dld = 4

Ph <- sapply(1:nrow(dl), function(i) { 
  if (dl[i,] <= CSDL) {P = Dsd}
  else if (dl[i,] >= CLDL) {P = Dld}
  else if (dl[i,] > CSDL & dl < CLDL) {P = Dsd + ((dl[i,]-(CSDL*(Dld-Dsd)))/(CLDL-CSDL))}
})

Precip = as.data.frame(Ph)


#thermal time
#generate hourly temps
dt = rnorm(nrow(time), mean = 11, sd = 4)
hourlytemp = as.data.frame(dt)
Tb = 3


Th <- sapply(1:nrow(hourlytemp), function(i) { 
  sunrise = pfilt[i,1]
  sunset = pfilt[i,2]
  if (hour[i,] >= sunrise & hour[i,] <= sunset) {
    if (hourlytemp[i,] >= Tb) {Th = (hourlytemp[i,] - Tb)*P}
    else if (hourlytemp[i,] < Tb) {Th = 0}}
  else {Th = 0}
})

#Tb = base temp
#Th = thermal unit at time t
#T = temperature at a particular time (df)
#pfilt = df with the hour of the sunrise and sunset for each day
#sunset = hour of sunset in the 2nd col of pfilt
#sunrise = hour of sunrise in the 1st col of pfilt

therm = as.data.frame(Th)

#vernalization

#vernalization effectiveness
Tvmin = -3.5
Tvmax = 6
K = 0.5
w = 0.3
o = 0.6
temps = seq(-3.5, 6, by=0.01) #calculate a ve for each of these temperatures
temps = as.data.frame(temps)

ves <- sapply(1:nrow(temps), function(i) { 
  ve = exp(K)*((temps[i,]-Tvmin)^w)*((Tvmax-temps[i,])^o)
})

#ves = vernailizing effectiveness
#Tvmin/max = min/max vernalizing temp bounds
# K = beta function parameter -- of what?
# o = beta function parameter -- of what?
# w = beta function paramter also


#vernalization time normalization
#put hourly temps in hundredth decimals place terms
temps = round(Th, 2)

counts = as.data.frame(table(temps))
str(counts)
ve_temps=as.numeric(counts[,1])
counts = cbind(counts, ve_temps)
counts = counts[,-1]

#vh_cum = cumulative effect vernalization hours 
names(counts)

vh = counts$ve_temps * counts$Freq
vh_cum = sum(vh)
vh_cum

#vernalization function
vsat = 960
Fb = 60

ves <- sapply(1:nrow(temps), function(i) { 
  ve = exp(K)*((temps[i,]-Tvmin)^w)*((Tvmax-temps[i,])^o)
})

if (vh_cum <= vsat) {vern = Fb + ((vh_cum*(1-Fb))/vsat)}
else if (vh_cum > vsat) {vern = 1}

vern = 1
#Fb = parameter representing baseline FLC repression; as cold exposure increses FLC becomes more repressed;
#vsat = in hours; when FLC is totally repressed; 960 based on lit

#MPTU 

MPTU = Precip*therm*vern
cumMPTU = sum(MPTU)
cumMPTU


