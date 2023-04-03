## MY FUNCTION FOR GENERATING A PLOT OF THE IMPACT OF R0 ON THE FINAL EPIDEMIC SIZE

plot.final.size <- function(r0.vector){
    root.matrix <- matrix(NA, nrow=length(r0.vector), ncol=2)
    r0.index <- 0
    
    for (r0 in r0.vector){
      r0.index <- r0.index + 1
      sol.all.roots <- uniroot.all(function(s.inf){
                                      return(final.size.root.twoargs(r0, s.inf))
                                   }, c(0,1))
      root.matrix[r0.index,] <- sol.all.roots
    }
    
    par(new=FALSE)
    plot(r0.vector, root.matrix[,1], type= "b", xlim=c(min(r0.vector), max(r0.vector)), ylim=c(0,1), ylab = "Root value", xlab = "R0")
    points(r0.vector, root.matrix[,2], type= "b")
}



final.size.root.twoargs <- function(R0, s.inf){
  final.size <- R0*(s.inf - 1) - log(s.inf)
  return(final.size)
}
