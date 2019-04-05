# GET FRAMES
# from: csv 1+-->1+ to: data frame
# read csv file(s) and make them suitable for the exploration
getFrames = function(orderBySeason=FALSE){
  #=================================================
  #=         1 - retrieve files and names          =
  #=================================================
  paths = choose.files(caption = "Select files",multi = TRUE,                         #get file paths
                       filters = matrix(c("All files","*.*","Csv files","*.csv"),    
                                        ncol = 2, byrow = TRUE ))
  names = tools::file_path_sans_ext(basename(paths))                                  #get file names
  .session$framespath <<- unique(dirname(paths))[1]                                     #    output path
  csvRGB = lapply(paths,function(path){read.csv(path,header = FALSE)})                #    csv files
  framesCollection = list()
  #=================================================
  #=         2 - populate data frame               =
  #=================================================
  for (i in 1:length(paths)) {
    names(csvRGB[[i]]) = c("R","G","B")
    frames = data.frame(csvRGB[[i]],                                                  #R,G,B channels
                        hex =rgb(csvRGB[[i]]$R, csvRGB[[i]]$G, csvRGB[[i]]$B,         #hex color of frame
                                 maxColorValue=255),
                        lum =luminance(csvRGB[[i]]$R, csvRGB[[i]]$G, csvRGB[[i]]$B),  #luminance of frame
                        frameId =1:length(csvRGB[[i]]$R),
                        stringsAsFactors = FALSE)
    attr(frames, "avgRGB") <- rgb(mean(frames$R), mean(frames$G), mean(frames$B),     #hex color of the clip
                                  maxColorValue = 255)
    attr(frames, "title") <- names[i]  
    attr(frames, "duration") <- round(max(frames$frameId)/.session$fps)
    framesCollection[[i]] = frames
  }
  if(orderBySeason) return(framesCollection[order(getSeason(names),getEpisode(names))])
  else return(framesCollection)
}

# GROUP FRAMES
# from: data frame 1-->1 to: data frame
# shrink the frame-by-frame original data frame into a window-by-window data frame
# window = number of seconds/number of frames to merge together
groupframes = function(frames,seconds=NULL,fps=.session$fps){
  if(is.null(seconds)) {
    seconds = round(length(frames$frameId)/(fps*.session$linesize$bold))
  }
  #1 group of frames = 1 colored tile = *seconds* sec of clip
  numFrames = round(fps*seconds) #number of frames for each group
  oldw <- getOption("warn")
  options(warn = -1)
  R = colMeans(matrix(frames$R, nrow=numFrames)) #average for each group of numFrames frames
  G = colMeans(matrix(frames$G, nrow=numFrames))
  B = colMeans(matrix(frames$B, nrow=numFrames))
  lum = colMeans(matrix(frames$lum, nrow=numFrames))
  options(warn = oldw)
  seconds = (1:length(R))*seconds
  hexRGB = rgb(R,G,B, max=255) #color of each tile
  frames.reduced = data.frame(cbind(seconds,hexRGB,lum,R,G,B),stringsAsFactors = FALSE)
  attr(frames.reduced, "title") <- attributes(frames)$title
  attr(frames.reduced, "avgRGB") <- rgb(mean(R),mean(G),mean(B), maxColorValue=255)
  frames.reduced$R = as.numeric(frames.reduced$R)
  frames.reduced$G = as.numeric(frames.reduced$G)
  frames.reduced$B = as.numeric(frames.reduced$B)
  frames.reduced$lum = as.numeric(frames.reduced$lum)
  frames.reduced$seconds = as.numeric(frames.reduced$seconds)
  return(frames.reduced)
}

# GET SUMMARY
# from: data frame 1+-->1+ to: data frame
# compress each input data frame into a single row,
# averagin each column of the input data frame
getSummary = function(framesCollection){
  summary = as.data.frame(do.call(rbind,lapply(framesCollection,function(x){
    df = x[,c("R","G","B","lum")]
    f = colMeans(df)
    f[5] = max(x$seconds)
    names(f)[5] = "duration"
    return(f)
  })))
  summary$RGB = unlist(lapply(framesCollection,function(x){attributes(x)$avgRGB}))
  summary$title = unlist(lapply(framesCollection,function(x){attributes(x)$title}))
  summary$frameId = 1:length(summary$R)
  return(summary)
}

getSeason = function(names){return(as.numeric(str_extract(names,"\\d*(?=e)")))}
getEpisode = function(names){return(as.numeric( str_extract(names,"\\d*(?=\\.)")))}

.range <- function(x){((x-min(x))/(max(x)-min(x)))*0.9+0.05}