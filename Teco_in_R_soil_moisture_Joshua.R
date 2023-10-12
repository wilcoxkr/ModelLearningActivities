###Soil moisture modeling 
###09/14/2023
###Authors: Joshua Ajowele, Kevin Wilcox, Shelby Williford, Jordan Winter

Precip<-5
#Precip<-runif(10,1,10)
Infilt<-Precip
FieldCap<-0.45
WILTPT<-0
Thick<-100
Wc<-0
Wtdeficit<-FieldCap-Wc
if(Infilt >0) {WaterAdd=min(Infilt,Wtdeficit*Thick*10)
Wcnew<-(Wc*(Thick*10)+WaterAdd)/(Thick*10)
FractWc=Wcnew
Perc=Infilt-WaterAdd
}



#produce runoff during infiltration
roff_layer<-0
if(Perc >0) {roff_layer=roff_layer + Perc*0.05}
Infilt_new<-Perc-Perc*0.05

#runoff
runoff=Infilt_new + roff_layer  #weng 10072006

##	water redistribution among soil layers
wsc<-max(0.00,(Wcnew-WILTPT)*Thick*10.0)
omegaL=max(0.001,(Wcnew-WILTPT)/(FieldCap-WILTPT))


supply=0.0
demand=0.0

if(omegaL >0.3){supply=wsc/360.0*omegaL
demand<-(FieldCap-Wcnew*Thick*10.0/360.0*(1.0-omegaL))
exchangeL=min(supply,demand)
wsc_min=wsc-exchangeL
wsc_max=wsc+exchangeL
Wcnew_min=wsc_min/(Thick*10.0)+WILTPT
Wcnew_max=wsc_max/(Thick*10.0)+WILTPT
}
