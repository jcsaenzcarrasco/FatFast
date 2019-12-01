
trees  <- read.csv("semi.csv")[c(2)]
forest <- read.csv("semi.csv")[c(3)]
both   <- read.csv("semi.csv")[c(4)]

# =======================================  P L O T   L A Y E R  ============ 

xrange = range(0:50)
yrange = range(0:12) 

plot( main ="semi-FT: Performance creating a forest of 50-node trees" 
    , xrange, yrange
    , xlab = "Number of Operations (x10,000)"
    , ylab = "Time(s)"
    , type = "l"          # line type (just straight)
    , lty  = 2            # line pattern (dashed)
    , lwd  = 2            # line width 
    , log  = "")


# ======================================  Trees   L I N E  ============== 

lines(lowess(trees)            # lowess (trees)  
     , type = "o"      # line type (straight and bullets as circles)
     , col  = "darkmagenta"  
     , pch  = 24       # line character (triangles pointing up)
     , lty  = 1        # line pattern (solid)
     , lwd  = 2)       # line width 

# ======================================  Forest   L I N E  ============= 

lines(lowess(forest) 
     , type = "o"      # line type (straight and bullets as circles)
     , col  = "blue"
     , pch  = 25       # line character (triangles pointing down)
     , lty  = 1        # line pattern (solid)
     , lwd  = 2)

# ======================================  BOTH   L I N E  ============= 

lines(lowess(both) 
     , type = "o"      # line type (straight and bullets as circles)
     , col  = "black"
     , pch  = 23       # line character (diamonds)
     , lty  = 1        # line pattern (solid)
     , lwd  = 2)


# =========================================  L  E  G  E  N  D  =============== 

legText = c( "Creating Trees", "Creating Forest", "Both" )           
legCol  = c("darkmagenta","blue", "black") 
legLTY  = c(1,1,1) 
legPCH  = c(24,25,23)

legend( "topleft"
      , legend = legText
      , col = legCol
      , lty = legLTY    # line pattern (dashed, solid)
      , pch = legPCH    # line character (none i.e. NA, solid circle i.e. 16)
      , lwd = 2
      , cex = 1)
