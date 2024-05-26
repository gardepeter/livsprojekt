load("./data/resources/fits.rda")

microStateIntensity = function(microStateAmount, macroState, iMicroState, jMicroState){
  if(iMicroState == jMicroState){
    stop("ERROR: i = j ")
  }
  if( !(microStateAmount %in% c(1, 2, 3, 5, 7, 10)) ){
    stop("ERROR: microStateAmount")
  }
  
  glmObject = fits[[microStateAmount]]$micro_fits[[macroState]][[iMicroState]][[jMicroState]]
  return( glmObject[[1]] )
}

microStateAmount = 2
macroState = 2
iMicroState = 1
jMicroState = 2
microStateIntensity(microStateAmount, macroState, iMicroState, jMicroState)

