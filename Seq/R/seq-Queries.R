
viewLR <- read.csv("seq.csv")[c(7)]
n50    <- read.csv("seq.csv")[c(9)]
n75    <- read.csv("seq.csv")[c(11)]
n101   <- read.csv("seq.csv")[c(13)]
n25    <- read.csv("seq.csv")[c(15)]

# =======================================  P L O T   L A Y E R  ============ 

xrange = range(0:50)
yrange = range(0: 1137052) 

plot( main ="seq-FT: Looking up elements in a forest of 50-node trees" 
    , xrange, yrange
    , xlab = "Number of Operations (x10,000)"
    , ylab = expression(paste("Time (", mu, "s)"))
#    , type = "o"          # line type (just straight)
#    , lty  = 2            # line pattern (dashed)
#    , lwd  = 2            # line width 
#    , log  = "")
)
# pch = [11, 10, 12, 7, 13]
# col = [darkmagenta, blue, darkgreen, brown, blue4]
# ==========================  ViewLeftRightMost   L I N E  ============== 

lines(lowess(viewLR + 15)          # lowess (viewLR)  
     , type = "o"      # line type (straight and bullets as circles)
     , col  = "darkgreen"  
     , pch  = 11       # line character (King David star)
     , lty  = 1        # line pattern (solid)
     , lwd  = 2)       # line width 

# ==================================  Search N/2   L I N E  ============= 

lines(lowess(n50) 
     , type = "o"      # line type (straight and bullets as circles)
     , col  = "blue4"
     , pch  = 10       # line character (Vertical crossed circle)
     , lty  = 1        # line pattern (solid)
     , lwd  = 2)

# ===============================  Search N(3/4)   L I N E  ============= 

lines(lowess(n75) 
     , type = "o"      # line type (straight and bullets as circles)
     , col  = "darkmagenta"
     , pch  = 12       # line character (Vertical crossed square)
     , lty  = 1        # line pattern (solid)
     , lwd  = 2)

# ==================================  Search N+1   L I N E  ============= 

lines(lowess(n101) 
     , type = "o"      # line type (straight and bullets as circles)
     , col  = "brown"
     , pch  = 7        # line character (Diagonal crossed circle)
     , lty  = 1        # line pattern (solid)
     , lwd  = 2)

# ==================================  Search N/4   L I N E  ============= 

lines(lowess(n25) 
     , type = "o"      # line type (straight and bullets as circles)
     , col  = "blue"
     , pch  = 13       # line character (Diagonal crossed square)
     , lty  = 1        # line pattern (solid)
     , lwd  = 2)

# ====================================  L  E  G  E  N  D  =============== 

legText = c( "viewl (viewr)"
           , "Search N/2"
           , "Search N(3/4)"
           , "Search N+1"
           , "Search N/4" )           
legCol  = c("darkgreen","blue4", "darkmagenta", "brown", "blue" ) 
legLTY  = c(1,1,1,1,1) 
legPCH  = c(11,10,12,7,13)

legend( "topleft"
      , legend = legText
      , col = legCol
      , lty = legLTY    # line pattern (dashed, solid)
      , pch = legPCH    # line character (none i.e. NA, solid circle i.e. 16)
      , lwd = 2
      , cex = 1)
