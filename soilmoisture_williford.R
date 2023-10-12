# water infiltration through layers
precp <- 5
infilt <- precp  # mm/hour
WTADD <- 0
TWTADD <- 0
roff_layer <- 0.0
watercontent <- 0
fieldcap <- 0.45
wtdeficit <- fieldcap - watercontent
THKSL <- 100
WCL <- 0
FWCLN <- 0
wsmin <- 1 #arbitrary
WILTPT <- wsmin/100.0
wsmax <- 7 #arbitrary
FLDCAPL <- wsmax/100.0
wcl <- FILDCP
FILDCP <- wsmax/100.0
wsc <- wcl*THKSL*10.0

# Loop over all soil layers
for (i in 1:10) {
  
  if (infilt > 0.0) {
    # Add water to this layer, pass extra water to the next.
    WTADD <- min(infilt, wtdeficit * THKSL * 10.0)  # from cm to mm
    
    # Change water content of this layer
    WCL <- (WCL * (THKSL * 10.0) + WTADD) / (THKSL * 10.0)
    FWCLN <- WCL  # update fwcln of this layer
    
    TWTADD <- TWTADD + WTADD  # calculating total added water to soil layers (mm)
    infilt <- infilt - WTADD  # update infilt
  } }
  
  # Produce runoff during infiltration
  if (infilt > 0.0) {
    roff_layer <- roff_layer + infilt * 0.05 * (i - 1)
    infilt <- infilt - infilt * 0.05 * (i - 1)
  }
}

if (!is.na(precp) && !is.na(WCL[1]) && !is.na(WCL[2]) && precp > 0.0 && WCL[1] > WCL[2]) {
  supply <- (WCL[1] - WCL[2]) / 3.0
  WCL[1] <- WCL[1] - 2.0 * supply
  WCL[2] <- WCL[2] + supply
}

# Runoff
runoff <- infilt + roff_layer  # precp - TWTADD + roff_layer
for (i in 1:10) {
  wsc=max(0.00,(WCL[i]-WILTPT)*THKSL*10.0)
  omegaL=max(0.001,(WCL-WILTPT)/(FLDCAPL-WILTPT))
}

supply=0.0
demand=0.0

for (i in 1:9) {
  if(omegaL > 0) {
    supply=wsc/360.0*omegaL
    demand=(FLDCAPL-WCL*THKSL*10.0/360.0 *(1.0-omegaL))
            exchangeL=min(supply,demand)
            wsc=wsc- exchangeL
            wsc=wsc+ exchangeL
            wcl=wsc/(THKSL*10.0)+WILTPT
            wcl=wsc/(THKSL*10.0)+WILTPT 
            } }

Tair = 3
esat = 7
RH = 2

if(WCL < WILTPT) {
  evap=0.0
  Rsoil=10.1*exp(1.0/WCL) 
  Rd=   20.5 
  P=101325.0  
  density=1.204 
  la=(2.501-0.00236*Tair)*1000000.0 
  sp_heat=1012.0  
  psychro=1628.6*P/la
  evap=1.0*esat*(1.0-RH/100.0)/(Rsoil+Rd)*density*sp_heat/psychro/la*3600.0 
}

Twater=0
 
wsc=(WCL-WILTPT)*THKSL*10.0
Twater=Twater+wsc

Tsrdt=0.0
DEPTH=0.5

SRDT=(-4.73*(DEPTH-THKSL/2.0)/100.0)/1.987
SRDT=max(0.0,SRDT*(WCL-WILTPT)) 
Tsrdt=Tsrdt+SRDT

SRDT=SRDT/Tsrdt

EVAPL=max(min(evap*SRDT,wsc,0.0))
DWCL=EVAPL/(THKSL*10.0)

WCL=WCL-DWCL
