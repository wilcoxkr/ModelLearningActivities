# water infiltration through layers
precp <- 5
infilt <- precp  # mm/hour
TWTADD <- 0
roff_layer <- 0.0
watercontent <- 0
fieldcap <- 0.45
wtdeficit <- fieldcap - watercontent
thksl <- 100
WCL <- as.logical()
FWCLN <- as.logical()

# Loop over all soil layers
for (i in 1:10) {
  
  if (infilt > 0.0) {
    # Add water to this layer, pass extra water to the next.
    WTADD <- min(infilt, wtdeficit[i] * thksl[i] * 10.0)  # from cm to mm
    
    # Change water content of this layer
    WCL[i] <- (WCL[i] * (thksl[i] * 10.0) + WTADD) / (thksl[i] * 10.0)
    FWCLN[i] <- WCL[i]  # update fwcln of this layer
    
    TWTADD <- TWTADD + WTADD  # calculating total added water to soil layers (mm)
    infilt <- infilt - WTADD  # update infilt
  }
  
  # Produce runoff during infiltration
  if (infilt > 0.0) {
    roff_layer <- roff_layer + infilt * 0.05 * (i - 1)
    infilt <- infilt - infilt * 0.05 * (i - 1)
  }
}

if (precp > 0.0 && WCL[1] > WCL[2]) {
  supply <- (WCL[1] - WCL[2]) / 3.0
  WCL[1] <- WCL[1] - 2.0 * supply
  WCL[2] <- WCL[2] + supply
}

# Runoff
runoff <- infilt + roff_layer  # precp - TWTADD + roff_layer