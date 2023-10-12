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

###calculate evap demand by eq.24 of Seller et al. 1996 (demand)
#setting parameters and formula for evap
#created a function to returns sat vapour pressure in Pa
#based on air temperature
esat<-function(x){610.78*exp(17.27*x/(x+237.3))
  }
Tair<-25
RH<-30 #from Teco model
{Rsoil=10.1*exp(1.0/Wcnew_min)
Rd=20.5 #*exp(LAI/1.5)!LAI is added by Weng
P=101325.0  #Pa, atmospheric pressure
density=1.204 #kg/m3
la=(2.501-0.00236*Tair)*1000000.0 #J/kg
sp_heat=1012.0  #J/kg/K
psychro=1628.6*P/la
if(Wcnew_min < WILTPT) {evap=0} 
else {evap=1.0*esat(Tair)*(1.0-RH/100.0)/(Rsoil+Rd)*density*sp_heat/psychro/la*3600.0}
}
### Soil evaporation; SRDT(I) for contribution of each layer. 
##    Units here are g H2O m-2 layer-1 h-1.
Twater=0
do i=1,10
wsc(i)=(wcl(i)-wiltpt)*THKSL(I)*10.0
Twater=Twater+wsc(i)  ! total water in soils,mm
enddo

Tsrdt=0.0
DO i=1,10
!		Fraction of SEVAP supplied by each soil layer
SRDT(I)=EXP(-4.73*(DEPTH(I)-THKSL(I)/2.0)/100.0)/1.987
!			SRDT(I)=AMAX1(0.0,SRDT(I)*(wcl(i)-wiltpt)) !*THKSL(I))
Tsrdt=Tsrdt+SRDT(i)/(i*i)  ! to normalize SRDT(i)
enddo

do i=1,10
SRDT(i)=SRDT(i)/Tsrdt
enddo

do i=1,10
EVAPL(I)=Amax1(AMIN1(evap*SRDT(i),wsc(i)),0.0)  !mm
DWCL(I)=EVAPL(I)/(THKSL(I)*10.0) !ratio
enddo

!	update water content of every layer
do i=1,10
wcl(i)=wcl(i)-DWCL(i)
enddo

!	the actual evapration
evap=0.0	
do i=1,10
evap=evap+EVAPL(I)
enddo