my.ellipse <-
function(x, y, level = 0.75, segments = 51){
	dfn <- 2
    dfd <- length(x) - 1
    if (dfd < 3){
    	ellipse <- rbind(c(NA,NA))	
    } else {
        require(MASS)
        v <- cov.trob(cbind(x, y))
        shape <- v$cov
        center <- v$center
        radius <- sqrt(dfn * qf(level, dfn, dfd))
        angles <- (0:segments) * 2 * pi/segments
    	unit.circle <- cbind(cos(angles), sin(angles))
	    ellipse <- t(center + radius * t(unit.circle %*% chol(shape)))
    }
    
    ellipse <- as.data.frame(ellipse)
    colnames(ellipse) <- c("x","y")
    return(ellipse)
}

