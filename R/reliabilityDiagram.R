#' Produce a reliability diagram
#'
#' @param pred_prob a vector of predicted probabilities.
#' @param binary_obs a vector of binary observations.
#' @param bins the vector of bins, given as the points of distinction between individual bins, including 0 and 1.
#' @param main the title of the plot. Defaults to "Reliability diagram".
#' @param xlab the x-label. Defaults to "Forecast probability".
#' @param ylab the y-label. Defaults to "Observed relative frequency".
#' @return a reliability diagram produced from the input data.
#' @export

reliabilityDiagram <- function(pred_prob,binary_obs,bins="default",main="Reliability diagram",xlab="Forecast probability",ylab="Observed relative frequency"){
  
  # Default bins if unspecified
  if(is.character(bins)){
    bins = c(0.05,0.15,0.25,0.35,0.45,0.55,0.65,0.75,0.85,0.95)
  }
  
  # Get the reliability points
  rel <- reliability(pred_prob,binary_obs,bins)
  
  # Sample climatology ("no resolution")
  cli <- sum(binary_obs) / length(binary_obs)
  
  # Various coordinates/lines for drawing purpose
  xp <- c(0,1)
  ns1 <- (xp[1] + cli) / 2 # "No skill" line left endpoint
  ns2 <- (xp[2] + cli) / 2 # "No skill" right left endpoint
  
  # Plot
  invisible(par(mar=c(3.5,3.2,2,1)+0.1,mgp=c(1.9,0.7,0)))
  plot(rel,type="n",main=main,xlab=xlab,ylab=ylab,xlim=c(0,1),ylim=c(0,1))
  polygon(c(0,cli,cli,0),c(0,0,cli,ns1),col="light gray",border = NA)
  polygon(c(cli,1,1,cli),c(cli,ns2,1,1),col="light gray",border = NA)
  lines(xp,xp,lty=2) # Perfect reliablity
  lines(xp,c(cli,cli),lty=2) # No resolution
  points(rel,col="dark red",pch=16)
  lines(rel,col="dark red")
  text(0.9,cli-0.04,labels="No resolution")
  text(0.9,(0.9+cli)/2-0.05,labels="No skill")
  
  # Reset plot parameters to default
  par(mar=c(5,4,4,2)+0.1,mgp=c(3,1,0))
  
  return(rel)
  
}
