
mergeNo  <- read.csv("top.csv")[c(18)]
mergeInc <- read.csv("top.csv")[c(20)]
insert   <- read.csv("top.csv")[c(22)]

# =======================================  P L O T   L A Y E R  ============ 

xrange = range(0:50)
yrange = range(0:450) 

plot( main ="top-FT: Updates (insertion and appending) in a forest of 50-node trees" 
    , xrange, yrange
    , xlab = "Number of Operations (x10,000)"
    , ylab = expression(paste("Time (", mu, "s)"))
#    , type = "l"          # line type (just straight)
#    , lty  = 2            # line pattern (dashed)
#    , lwd  = 2            # line width 
#    , log  = "")
)

# =====================  Merge (no including searching) L I N E  ========= 

lines(lowess(mergeNo)          # lowess (trees)  
     , type = "o"      # line type (straight and bullets as circles)
     , col  = "darkmagenta"  
     , pch  = 24       # line character (triangles pointing up)
     , lty  = 1        # line pattern (solid)
     , lwd  = 2)       # line width 


# =========================  Merge (including searching) L I N E  ========= 

lines(lowess(mergeInc) 
     , type = "o"      # line type (straight and bullets as circles)
     , col  = "black"
     , pch  = 25       # line character (triangles pointing down)
     , lty  = 1        # line pattern (solid)
     , lwd  = 2)

# ======================================  Insertion   L I N E  ============= 

lines(lowess(insert) 
     , type = "o"      # line type (straight and bullets as circles)
     , col  = "blue"
     , pch  = 23       # line character (diamonds)
     , lty  = 1        # line pattern (solid)
     , lwd  = 2)


# =========================================  L  E  G  E  N  D  =============== 

legText = c( "Append Trees (no search)"
           , "Append Trees (with search)"
           , "Insertion" )           
legCol  = c("darkmagenta","black", "blue") 
legLTY  = c(1,1,1) 
legPCH  = c(24,25,23)

legend( "topleft"
      , legend = legText
      , col = legCol
      , lty = legLTY    # line pattern (dashed, solid)
      , pch = legPCH    # line character (none i.e. NA, solid circle i.e. 16)
      , lwd = 2
      , cex = 1)
