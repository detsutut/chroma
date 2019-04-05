.linesize = list("ultrathin"=1500,"thin"=900,"middle"=500,"bold"=250,"bold"=140)

luminance = function(r,g,b){
  return((r * 0.3) + (g * 0.59) + (b * 0.11))
}

changeHue = function(hexString, increment_perc=10){
  return(.changeHSV("h",hexString,increment_perc))
}
changeSat = function(hexString, increment_perc=10){
  return(.changeHSV("s",hexString,increment_perc))
}
changeLum = function(hexString, increment_perc=10){
  return(.changeHSV("v",hexString,increment_perc))
}

.changeHSV = function(channel, hexString, increment_perc){
  hsv = rgb2hsv(col2rgb(hexString))
  hsv[channel,] = hsv[channel,]*(1+increment_perc/100)
  for(c in channel){
    if(hsv[c,]>1) hsv[channel,]=1
    if(hsv[c,]<0) hsv[channel,]=0
  }
  return(hsv(hsv["h",],hsv["s",],hsv["v",]))
}

fixSat = function(hexString, value=.5){
  return(.fixHSV("s",hexString,value))
}
fixLum = function(hexString, value=.5){
  return(.fixHSV("v",hexString,value))
}
fixHue = function(hexString, value=.5){
  return(.fixHSV("h",hexString,value))
}

.fixHSV = function(channels,hexString, values){
  hsv = rgb2hsv(col2rgb(hexString))
  hsv[channels,] = values
  for(channel in channels){
    if(hsv[channel,]>1) hsv[channel,]=1
    if(hsv[channel,]<0) hsv[channel,]=0
  }
  return(hsv(hsv["h",],hsv["s",],hsv["v",]))
}

vividHex = function(hexString, intensity = "strong"){
  darkIntensity = switch(EXPR=intensity,light = .05,medium = .1,strong =.2, ultra =.3)
  satIntensity = switch(EXPR=intensity,light = .1,medium = .2,strong =.3, ultra =.4)
  return(modCol(hexString, darken = -darkIntensity, saturate = satIntensity, modhue = 0))
}

lumOnly = function(hexString){
  return(modCol(hexString, darken = 0, saturate = -1, modhue = 0))
}

hueOnly = function(hexString){
  return(hsl2col(as.matrix(c(hslGet(hexString,"h"),1,0.5))))
}

satOnly = function(hexString){
  return(hsl2col(as.matrix(c(1,hslGet(hexString,"s"),0.5))))
} 

hslGet = function(hexString, channel){
  values = list()
  for(hex in hexString){
    values = append(values,col2hsl(hex)[toupper(channel),][[1]])
  }
  return(unlist(values))
}

isWarm = function(hexString){
  hue = col2hsl(hexString)["H",][[1]]
  sat = col2hsl(hexString)["S",][[1]]
  temp = list()
  c = abs(hue-180) #distance from coldest color
  if(hue>180) w = abs(hue-360) else w = abs(hue-0) #distance from warmest color
  temp = ifelse(w<=c,"WARM","COLD")
  if(sat<0.05) { 
    temp = "NEUTRAL"
    dist = NULL
  }
  return(list(temp = temp, dist = w))
}

colorCircle = function(frames, extra = FALSE){
  palette = colorHueRef
  if(extra) palette = colorHueRefExtra
  from = unlist(lapply(frames$hexRGB,function(x){getHueRef(x,extra = extra)}))
  to = c(from[2:length(from)],from[length(from)])
  # browser()
  # from = from[which(from!="N")]
  # to = from[which(to!="N")]
  df = data.frame(from=from,to=to)
  chordDiagram(df,grid.col = palette["hex",], self.link = 2,
               directional=1, scale = FALSE, transparency = 0.5,
               annotationTrack = c("grid"),
               order=rev(unique(colnames(palette))))
  circos.clear()
}

colorHueRef = cbind(
  Red = c(min=330,max=360,hex="#FF0000"),
  Red = c(min=0,max=30,hex="#FF0000"),
  Yellow = c(min=30,max=90,hex="#FFFF00"),
  Green = c(min=90,max=150,hex="#00FF00"),
  Cyan = c(min=150,max=210,hex="#00FFFF"),
  Blue = c(min=210,max=270,hex="#0000FF"),
  Violet = c(min=270,max=330,hex="#FF00FF"),
  Neutral = c(min=-1,max=-1,hex="#BFBFBF")
)

colorHueRefExtra = cbind(
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

getHueRef = function(hexString, extra = FALSE){
  palette = colorHueRef
  if(extra) palette = colorHueRefExtra
  hue = col2hsl(hexString)["H",][[1]]
  sat = col2hsl(hexString)["S",][[1]]
  name = colnames(palette)[which(hue>=as.numeric(palette["min",]) & hue<as.numeric(palette["max",]))]
  if(sat<0.05) name = "Neutral"
  return(name)
}

temperature = function(frames){
  frames$tf= unlist(lapply(frames$hexRGB,function(y){isWarm(y)$temp}))
  frames$distance= unlist(lapply(frames$hexRGB,function(y){isWarm(y)$dist}))
  frames$distanceFromTop=1-ifelse(frames$distance>90,frames$distance-90,frames$distance)
  palette = list("WARM"="#FF1A00","COLD"="#2185D0","NEUTRAL"="#F2F2F2")
  unit = frames$seconds[2]-frames$seconds[1]
  p = ggplot(data=frames,aes(x=seconds))+
    geom_rect(mapping=aes(xmin=seconds-unit, xmax=seconds, 
                          ymin=1,
                          ymax=0, fill=tf,alpha=distanceFromTop))+
    geom_line(aes(y = .range(distance)), stat="smooth",method = "lm",
              formula = y ~ poly(x, 26), se = FALSE,color = "#70005A",alpha=0.6,size=2)+
    scale_fill_manual(name = "Temperature", values = unlist(palette))+
    geom_line(aes(y = .range(distance)), stat="smooth",method = "lm",
              formula = y ~ poly(x, 26), se = FALSE,color = "white",alpha=0.2,size=1)+
    geom_line(aes(y = .range(distance)), stat="smooth",method = "lm", linetype = "dotted",
              formula = y ~ poly(x, 1), se = FALSE,color = "#70005A",alpha=0.8,size=1)+
    scale_alpha_continuous(name = "Distance from pole",range = c(0.4, 1),
                           breaks = c(0,-20,-40,-60,-80), labels = c("0°","20°","40°","60°","80°"))+
    scale_y_continuous(breaks = c(0,0.33,0.66,1), labels = c("0°\n(R)","60°\n(Y/M)","120°\n(G/B)","180°\n(C)"),expand = c(0.01, 0))+
    scale_x_continuous(expand = c(0.01, 0))+
    labs(title = "Temperature Analysis", subtitle = waiver(), caption = waiver(),
         tag = waiver(),x = "seconds",y = "Distance from pure red")
    theme_minimal()
  plot(p)
  temp_perc = length(which(frames$tf=="WARM"))/length(frames$tf)
}

.exaltHSV = function(channel,hexString){
  channelIndex = c("h"=1,"s"=2,"v"=3)
  hsv = rgb2hsv(col2rgb(hexString))
  hsvcomponents = hsv[-channelIndex[channel]]
  maxincrement = (1-max(hsvcomponents))/max(hsvcomponents)
  return(.changeHSV(names(channelIndex[-channelIndex[channel]]),hexString,maxincrement*50))
}


testColor = function(hexString){
  plot(c(0, 1), c(0, 1), type= "n", xlab = "", ylab = "",main = hexString)
  rect(0, 0, 1, 1, density = -50, border = hexString, col=hexString)
  cat("HSL: ",col2hsl(hexString),"\nRGB: ",col2rgb(hexString),"\n")
}

plotFrameline = function(frames, verbose = 0, summary = TRUE, vivid = FALSE, timeScale = TRUE, title=toupper(.session$name),
                         subtitle="frameline"){
  if(length(frames$R)>0) return(plotSingleFrameline(frames = frames,verbose = verbose, summary = summary, vivid = vivid,
                                                    title = title, subtitle = subtitle))
  else return(plotMultiFrameline(framescollection = frames, verbose = verbose, summary = summary, vivid = vivid, timeScale = timeScale, 
                                 title=title, subtitle=subtitle))
}

plotSingleFrameline = function(frames, verbose = 0, summary = TRUE, vivid = FALSE, title=toupper(.session$name),
                         subtitle="frameline", xlim = NULL){
  frames$mins = frames$seconds/60                                            #x unit = minutes
  if(is.null(xlim)) xlim = max(frames$mins)                                                  
  if(vivid) {
    frames$hexRGB = vividHex(frames$hexRGB, intensity = "light")
    summary_fill = vividHex(attributes(frames)$avgRGB[[1]],intensity = "ultra")
    summary_line = vividHex(attributes(frames)$avgRGB[[1]],intensity = "medium") 
  } else {
    summary_fill = vividHex(attributes(frames)$avgRGB[[1]],intensity = "medium") 
    summary_line = vividHex(attributes(frames)$avgRGB[[1]],intensity = "light") 
  }     
  usr_palette = frames$hexRGB                                                #define a palette for ggplot's fields
  names(usr_palette) = usr_palette
  p = ggplot(data=frames,aes(x=mins))+
    geom_rect(mapping=aes(xmin=mins-1, xmax=mins,                            #build tiles
                          ymin=1,
                          ymax=0, fill=hexRGB), alpha=1)                     #fill tiles with RGB frame color
  if(summary){
    p= p+annotate("rect", xmin = xlim+0.005*xlim,                            #build the summary tile
                  xmax = xlim+0.02*xlim, 
                  ymin = 0, ymax = 1,
                  fill = summary_fill,
                  linetype = 0, 
                  color = summary_line,
                  size=0.6)
  }
  p=p+scale_x_continuous(expand = c(0.01, 0),breaks = seq(0,xlim,5))+
    labs(y = attributes(frames)$title)+
    scale_fill_manual(values = usr_palette)+                                #tell ggplot to use our palette
    guides(fill=FALSE)                                                      #no guides and no theme to get an
  if(verbose == 0) p = p + theme_void()                                     # artistic plot (not very scientific though)
  else if(verbose == 1) p = p + .theme_semiVoid(xaxis = verbose>1)          #add more details when verbose>0
  else p = p + .theme_semiVoid(xaxis = verbose>1)+labs(title=title,
                                                       subtitle = subtitle
  )
  return(p)
}

plotMultiFrameline = function(framescollection, verbose = 0, summary = TRUE, vivid = FALSE, timeScale = TRUE, 
                              title=toupper(.session$name), subtitle="frameline"){
  ggplot.list =  vector("list", length(framescollection))
  if(verbose>1) verbose.iter=1 else verbose.iter=verbose
  for(i in 1:length(framescollection)){
    if(timeScale) xlim = NULL else xlim = max(unlist(lapply(framescollection,function(x){max(x$seconds)})))/60
    ggplot.list[[i]] = plotSingleFrameline(framescollection[[i]], verbose = verbose.iter, summary = summary, vivid = vivid, 
                                     title = title, subtitle = subtitle, xlim = xlim)
  }
  if(!timeScale) ggplot.list[[i]] =  ggplot.list[[i]]+.theme_semiVoid(xaxis = verbose>1)
  ggplot.list = Filter(Negate(is.null), ggplot.list)
  if(verbose == 0) title = subtitle = NULL
  return(do.call("grid.arrange", c(grobs = ggplot.list, ncol=1, top = toupper(title),bottom = subtitle)))
}

plotFramesCollection = function(framescollection, season=0, verbose =0, summary = TRUE, 
                                vivid = FALSE, scaleTime = TRUE){
  maxlen = max(unlist(lapply(framescollection,function(x){max(x$seconds)})))/60
  allseasons = length(season)==1 && season==0
  ggplot.list =  vector("list", length(framescollection)) 
  for(i in 1:length(framescollection)){
    frames = framescollection[[i]]
    if(!allseasons && is.na(match(attributes(frames)$season,season))) next
    frames$mins = frames$seconds/60
    if(scaleTime) maxlen = max(frames$mins)
    if(vivid){
      frames$hexRGB = vividHex(frames$hexRGB, intensity = "light")
    }
    p = ggplot(data=frames,aes(x=mins))+
      geom_rect(mapping=aes(xmin=mins-1, xmax=mins, 
                            ymin=1,
                            ymax=0, fill=hexRGB), alpha=1)
    if(summary) {
      if(vivid) rectfill = vividHex(attributes(frames)$avgRGB[[1]], intensity = "ultra")
      else rectfill = attributes(frames)$avgRGB[[1]]
      p=p+
        annotate("rect", xmin = maxlen+0.005*maxlen, xmax = maxlen+0.02*maxlen, 
                 ymin = 0, ymax = 1,
                 fill = rectfill,
                 linetype = 0, color = vividHex(attributes(frames)$avgRGB[[1]], intensity = "light"),
                 size=0.6)
    }
    temp_hex = frames$hexRGB
    names(temp_hex) = temp_hex
    p=p+
      scale_fill_manual(values = temp_hex)+
      scale_x_continuous(expand = c(0.01, 0), breaks = seq(0,maxlen,5))+
      guides(fill=FALSE) +
      labs(y = attributes(frames)$title)
    if(verbose == 0) p = p + theme_void()
    else p = p + .theme_semiVoid(FALSE)
    if(verbose > 1 && i==length(framescollection)) p = p + .theme_semiVoid(!scaleTime)
    ggplot.list[[i]] = p
  }
  ggplot.list = Filter(Negate(is.null), ggplot.list)
  if(verbose > 0) title = toupper(.session$name) else title = NULL
  if(allseasons) f = do.call("grid.arrange", ggplot.list)
  else  f = do.call("grid.arrange", c(grobs = ggplot.list, ncol=1, top = title))
  return(f)
}

.brighter = function(hexString){
  temp = attributes(hex2RGB(hexString))$coords[1,]*1.5
  if(max(temp)>1) return(rgb(red=temp["R"], green=temp["G"], blue=temp["B"],maxColorValue = max(temp)))
  else return(rgb(red=temp["R"], green=temp["G"], blue=temp["B"]))
}

plotTilesSummary = function(summary,mode="",verbose = 0){
  switch(mode,
         h={
           temp_hex = unlist(lapply(summary$RGB,hueOnly))
           names(temp_hex) = temp_hex
           summary$hex = temp_hex
           text = ifelse(unlist(lapply(summary$RGB,function(x){isWarm(x)$isWarm})),"WARM","COOL")
           dist = unlist(lapply(summary$RGB,function(x){isWarm(x)$dist}))
           max = summary[which(dist == max(dist)),]
           min = summary[which(dist == min(dist)),]
           caption =paste("HUE channel (coldest:",
                          "S",max$season,"E",max$episode,
                          " warmest: S",min$season,"E",min$episode,")",sep="")
           p = ggplot(data = summary, aes(x = episode, y = season)) +
             geom_tile(data = summary, aes(fill = hex),width=0.85, height=0.85) +
             scale_fill_manual(values = temp_hex)+
             guides(fill=FALSE)
         },
         s={
           temp_hex = unlist(lapply(summary$RGB,satOnly))
           names(temp_hex) = temp_hex
           summary$hex = temp_hex
           val = hslGet(summary$RGB,"s")
           text = val
           max = summary[which(val == max(val)),]
           min = summary[which(val == min(val)),]
           caption =paste("BRIGHTNESS channel (max:",
                          "S",max$season,"E",max$episode,
                          " min: S",min$season,"E",min$episode,")",sep="")
           p = ggplot(data = summary, aes(x = episode, y = season)) +
             geom_tile(data = summary, aes(fill = hex),width=0.85, height=0.85) +
             scale_fill_manual(values = temp_hex)+
             guides(fill=FALSE)
           caption =paste("SATURATION channel (max:",
                          "S",max$season,"E",max$episode,
                          " min: S",min$season,"E",min$episode,")",sep="")
         },
         lum={
           temp_hex = unlist(lapply(summary$RGB,lumOnly))
           names(temp_hex) = temp_hex
           summary$hex = temp_hex
           max = summary[which(summary$lum == max(summary$lum)),]
           min = summary[which(summary$lum == min(summary$lum)),]
           text = summary$lum
           caption =paste("BRIGHTNESS channel (brightest:",
                          "S",max$season,"E",max$episode,
                          " darkest: S",min$season,"E",min$episode,")",sep="")
           p = ggplot(data = summary, aes(x = episode, y = season)) +
             geom_tile(data = summary, aes(fill = hex),width=0.85, height=0.85) +
             scale_fill_manual(values = temp_hex)+
             guides(fill=FALSE)
         },
         {
           temp_hex = unlist(lapply(summary$RGB,vividHex))
           names(temp_hex) = temp_hex
           summary$hex = temp_hex
           caption = ""
           text = summary$hex
           p = ggplot(data = summary, aes(x = episode, y = season)) +
             geom_tile(data = summary, aes(fill = hex),width=0.85, height=0.85) +
             scale_fill_manual(values = temp_hex)+
             guides(fill=FALSE)
         }
  )
  if(verbose>0){
    p=p+geom_text(data=summary,aes(label=text),color="black")
  }
  p=p +
    # scale_fill_gradient(low = "grey", high = "grey20")+
    scale_x_continuous(expand=c(0,0),position = "top",
                       breaks = c(summary$episode),labels = paste("EP",summary$episode)) + 
    scale_y_continuous(expand=c(0,0),
                       breaks = c(summary$season),labels = paste("S",summary$season)) +
    theme_minimal() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_blank(),
          axis.title=element_blank(),plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5) )+
    labs(title=toupper(.session$name),
         subtitle = "Data exploration and visualization through colors",
         caption = caption)
  plot(p)
  return(p)
}

plotLines = function(frames){
  temp_hex = frames$hexRGB
  names(temp_hex) = temp_hex
  # frames$mins = frames$seconds/60
  p = ggplot(data = frames, aes(x = seconds)) +
    geom_rect(mapping=aes(xmin=seconds-1, xmax=seconds, 
                          ymin=1,
                          ymax=0, fill=hexRGB), alpha=1)+
    geom_line(aes(y = scale(V)), stat="smooth",method = "lm",
              formula = y ~ poly(x, 26), se = TRUE,color = "black",alpha=0.8,size=1.2)+
    geom_line(aes(y = scale(H)), stat="smooth",method = "lm",
              formula = y ~ poly(x, 26), se = TRUE,color = "red",alpha=0.8,size=1.2)+
    geom_line(aes(y = scale(S)), stat="smooth",method = "lm",
              formula = y ~ poly(x, 26), se = TRUE,color = "blue",alpha=0.8,size=1.2)+
    scale_fill_manual(values = temp_hex)+
    # scale_x_continuous(expand = c(0.01, 0), breaks = seq(0,maxlen,5))+
    guides(fill=FALSE) +
    theme_minimal()
  p
}

.theme_semiVoid = function(xaxis = FALSE){
  half_line = 0
  if(!xaxis) {
    xelem = element_blank()
    xtick = element_blank()
    xticklen = 0
    } else{
      xelem = element_text(angle = 0)
      xtick = element_line(color = "grey30",size = 1, linetype = 1,lineend = "square", inherit.blank = FALSE)
      xticklen = 1
    } 
  return(theme(text = element_text(face = "plain",family = "",colour = "grey30",lineheight = 0.9,hjust = 0.5,vjust = 0.5,angle = 0,margin = margin(),size = 10,debug = FALSE),
               line = element_line(color = "white",size = 0, linetype = 0,lineend = "square", inherit.blank = FALSE),
               # line = element_blank(),
               rect = element_blank(),
               axis.title.x=element_blank(),
               axis.title.y = element_text(angle = 90),
               axis.text.x = xelem,
               axis.text.y = element_blank(),
               axis.ticks.x.bottom = xtick,
               axis.ticks.length = unit(xticklen,"pt"),
               legend.box = NULL,
               legend.key.size = unit(1.2,"lines"),
               legend.position = "right",
               legend.text = element_text(size = rel(0.8)),
               legend.title = element_text(hjust = 0),
               strip.text = element_text(size = rel(0.8)),
               strip.switch.pad.grid = unit(half_line/2, "pt"),
               strip.switch.pad.wrap = unit(half_line/2,"pt"),
               panel.ontop = FALSE,
               panel.spacing = unit(0,"pt"),
               plot.margin = unit(c(0, 0, 0, 0), "lines"),
               plot.title = element_text(size = rel(1.2),hjust = 0,vjust = 1,margin = margin(t = half_line)),
               plot.subtitle = element_text(hjust = 0,vjust = 1,margin = margin(t = half_line)),
               plot.caption = element_text(size = rel(0.8),hjust = 1,vjust = 1,margin = margin(t = half_line)),
               plot.tag = element_text(size = rel(1.2),hjust = 0.5,vjust = 0.5),
               plot.tag.position = "topleft", complete = TRUE))
}

extractFramePalette = function(jpegFramePath = NULL, paletteDim = 10){
  if(is.null(jpegFramePath)) jpegFramePath = choose.files(default = framesPath, caption = "Select files",
                                                          multi = FALSE)
  jpegFrame <- readJPEG(jpegFramePath)
  height = dim(jpegFrame)[1]
  width = dim(jpegFrame)[2]
  df <- data.frame(
    x = rep(1:width, each = height),
    y = rep(height:1, width),
    R = as.vector(jpegFrame[,,1]),
    G = as.vector(jpegFrame[,,2]),
    B = as.vector(jpegFrame[,,3])
  )
  km <- kmeans(df[,c("R","G","B")], centers = paletteDim, iter.max = 30)
  colorPalette = vividHex(rgb(km$centers),intensity = "light")
  
  km2 <- kmeans(df[,c("R","G","B")], centers = paletteDim*3, iter.max = 30)
  pal2 = vividHex(rgb(km2$centers),intensity = "light")
  pal = list()
  while(length(pal)<paletteDim){
    pal.hdist = as.matrix((dist(col2hsl(pal2)["H",])))
    diag(pal.hdist) <- 0
    colnames(pal.hdist) = rownames(pal.hdist) = pal2
    names = unique(names(which(pal.hdist == max(apply(pal.hdist,2,max,na.rm=TRUE)), arr.ind = TRUE)[,"row"]))
    pal = append(pal,names)
    pal2 = setdiff(pal2,names)
  }
  colorPalette = unlist(pal)[1:paletteDim]
  pal2.hdist = as.matrix((dist(col2hsl(colorPalette)["H",])))
  colnames(pal2.hdist) = rownames(pal2.hdist) = colorPalette
  pheatmap(pal2.hdist)
  
  jpegFrame_pixelate = t(matrix(km$cluster,nrow = height,ncol = width,byrow = FALSE))
  jpegFrame_pixelate = jpegFrame_pixelate[,ncol(jpegFrame_pixelate):1]
  img = image(jpegFrame_pixelate,useRaster = TRUE,col =colorPalette)
  colorPalette.df = as.data.frame(cbind(id=1:length(colorPalette),color = colorPalette, size = km$size))
  colorPalette.df$id = as.numeric(colorPalette.df$id)
  temp_hex = levels(colorPalette.df$color)
  names(temp_hex) = temp_hex
  p = ggplot(data = colorPalette.df, aes(x = id, y = 1)) +
    geom_tile(data = colorPalette.df, aes(fill = color),width=0.85, height=0.85) +
    scale_fill_manual(values = temp_hex)+
    guides(fill=FALSE)+
    theme_void()+
    labs(title=toupper(.session$name),
         subtitle = "Data exploration and visualization through colors")
  plot(p)
}
