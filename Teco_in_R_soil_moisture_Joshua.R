###Soil moisture modeling 
###09/14/2023
###Authors: Joshua Ajowele, Kevin Wilcox, Shelby Williford, Jordan Winter

Precip<-5
#Precip<-runif(10,1,10)
Infilt<-Precip
FieldCap<-0.45
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


#if(precp.gt.0.0.and.wcl(1).gt.wcl(2))then
supply=(wcl(1)-wcl(2))/3.0
wcl(1)=wcl(1)-2.0*supply
wcl(2)=wcl(2)+supply