#' getFrames
#'
#' getFrames
#'
#' @param orderBySeason orderBySeason
#'
#' @return frames
#'
#' @examples
#' print("example")
#'
#' @export
getFrames <- function(orderBySeason=FALSE){
  #=================================================
  #=         1 - retrieve files and names          =
  #=================================================
  paths <- choose.files(caption = "Select files",multi = TRUE,                         #get file paths
                       filters = matrix(c("All files","*.*","Csv files","*.csv"),
                                        ncol = 2, byrow = TRUE ))
  names <- tools::file_path_sans_ext(basename(paths))                                  #get file names                                     #    output path
  csvRGB <- lapply(paths,function(path){read.csv(path,header = FALSE)})                #    csv files
  framesCollection <- list()
  #=================================================
  #=         2 - populate data frame               =
  #=================================================
  for (i in 1:length(paths)) {
    names(csvRGB[[i]]) <- c("R","G","B")
    frames <- data.frame(csvRGB[[i]],                                                  #R,G,B channels
                        hex =rgb(csvRGB[[i]]$R, csvRGB[[i]]$G, csvRGB[[i]]$B,         #hex color of frame
                                 maxColorValue=255),
                        lum =luminance(csvRGB[[i]]$R, csvRGB[[i]]$G, csvRGB[[i]]$B),  #luminance of frame
                        frameId =1:length(csvRGB[[i]]$R),
                        stringsAsFactors = FALSE)
    attr(frames, "avgRGB") <- rgb(mean(frames$R), mean(frames$G), mean(frames$B),     #hex color of the clip
                                  maxColorValue = 255)
    attr(frames, "title") <- names[i]
    attr(frames, "duration") <- round(max(frames$frameId)/chromaRenv$fps)
    framesCollection[[i]] <- frames
  }
  if(orderBySeason) return(framesCollection[order(getSeason(names),getEpisode(names))])
  else return(framesCollection)
}

#' groupframes
#'
#' groupframes
#'
#' @param frames frames
#' @param seconds seconds
#' @param fps fps
#'
#' @return frames
#'
#' @examples
#' print("example")
#'
#' @export
groupframes <- function(frames,seconds=NULL,fps=chromaRenv$fps){
  if(is.null(seconds)) {
    seconds <- round(length(frames$frameId)/(fps*chromaRenv$linesize$bold))
  }
  #1 group of frames <- 1 colored tile <- *seconds* sec of clip
  numFrames <- round(fps*seconds) #number of frames for each group
  oldw <- getOption("warn")
  options(warn = -1)
  R <- colMeans(matrix(frames$R, nrow=numFrames)) #average for each group of numFrames frames
  G <- colMeans(matrix(frames$G, nrow=numFrames))
  B <- colMeans(matrix(frames$B, nrow=numFrames))
  lum <- colMeans(matrix(frames$lum, nrow=numFrames))
  options(warn = oldw)
  seconds <- (1:length(R))*seconds
  hexRGB <- rgb(R,G,B, maxColorValue=255) #color of each tile
  frames.reduced <- data.frame(cbind(seconds,hexRGB,lum,R,G,B),stringsAsFactors = FALSE)
  attr(frames.reduced, "title") <- attributes(frames)$title
  attr(frames.reduced, "avgRGB") <- rgb(mean(R),mean(G),mean(B), maxColorValue=255)
  frames.reduced$R <- as.numeric(frames.reduced$R)
  frames.reduced$G <- as.numeric(frames.reduced$G)
  frames.reduced$B <- as.numeric(frames.reduced$B)
  frames.reduced$lum <- as.numeric(frames.reduced$lum)
  frames.reduced$seconds <- as.numeric(frames.reduced$seconds)
  return(frames.reduced)
}

#' getSummary
#'
#' getSummary
#'
#' @param framesCollection frames
#'
#' @return summary
#'
#' @examples
#' print("example")
#'
#' @export
getSummary <- function(framesCollection){
  summary <- as.data.frame(do.call(rbind,lapply(framesCollection,function(x){
    df <- x[,c("R","G","B","lum")]
    f <- colMeans(df)
    f[5] <- max(x$seconds)
    names(f)[5] <- "duration"
    return(f)
  })))
  summary$RGB <- unlist(lapply(framesCollection,function(x){attributes(x)$avgRGB}))
  summary$title <- unlist(lapply(framesCollection,function(x){attributes(x)$title}))
  summary$frameId <- 1:length(summary$R)
  return(summary)
}

#' getSeason
#'
#' getSeason
#'
#' @param names names
#'
#' @return season
#'
#' @examples
#' print("example")
#'
getSeason <- function(names){return(as.numeric(str_extract(names,"\\d*(?=e)")))}

#' getEpisode
#'
#' getEpisode
#'
#' @param names names
#'
#' @return episode
#'
#' @examples
#' print("example")
#'
getEpisode <- function(names){return(as.numeric( str_extract(names,"\\d*(?=$)")))}

#' range
#'
#' range
#'
#' @param x x
#'
#' @return y
#'
#' @examples
#' print("example")
#'
range <- function(x){((x-min(x))/(max(x)-min(x)))*0.9+0.05}
