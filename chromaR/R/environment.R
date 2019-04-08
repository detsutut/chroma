chromaRenv <- new.env(parent = emptyenv())
chromaRenv$fps <- 24
chromaRenv$linesize <- list("ultrathin"=1500,"thin"=900,"middle"=500,"bold"=250,"bold"=140)
chromaRenv$colorHueRef <- cbind(
  Red = c(min=330,max=360,hex="#FF0000"),
  Red = c(min=0,max=30,hex="#FF0000"),
  Yellow = c(min=30,max=85,hex="#FFFF00"),
  Green = c(min=85,max=150,hex="#00FF00"),
  Cyan = c(min=150,max=210,hex="#00FFFF"),
  Blue = c(min=210,max=270,hex="#0000FF"),
  Violet = c(min=270,max=330,hex="#FF00FF"),
  Neutral = c(min=-1,max=-1,hex="#BFBFBF")
)

chromaRenv$colorHueRefExtra <- cbind(
  Red = c(min=345,max=360,hex="#FF1919"),
  Red = c(min=0,max=15,hex="#FF1919"),
  Orange = c(min=15,max=45,hex="#FF8C1A"),
  Yellow = c(min=45,max=75,hex="#FFE24C"),
  Green = c(min=75,max=140,hex="#4EFF4C"),
  Turquoise = c(min=140,max=160,hex="#31FFBB"),
  Cyan = c(min=160,max=195,hex="#19D9FF"),
  Azure = c(min=195,max=225,hex="#1A8CFF"),
  Blue = c(min=225,max=255,hex="#1919FF"),
  Violet = c(min=255,max=285,hex="#8C19FF"),
  Magenta = c(min=285,max=315,hex="#FF33BB"),
  Pink = c(min=315,max=345,hex="#FF85C1"),
  Neutral = c(min=-1,max=-1,hex="#BFBFBF")
)

#' fileName
#'
#' fileName
#'
#' @param name name
#' @param ext ext
#'
#' @return name
#'
#' @examples
#' print("example")
#'
#' @export
fileName <- function(name,ext="png"){
  return(paste(name,format(Sys.time(),"%d%m%H%M%S"),".",ext,sep = ""))
}
