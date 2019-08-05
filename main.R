#workspace cleaning
remove(list = ls())
cat("\014")

library(chromaR)

allframes = getFrames()
clip = allframes[[14]]
framelines = lapply(allframes,function(x){groupframes(x,seconds = 1)})
framelines.summary = getSummary(framelines)

frap = plotTimeWindows(verbose = 1,
                       vivid = TRUE,
                       title = "Title Test",
                       subtitle = "Sub Test",
                       left = "Left Test")

p = plotFrameline(framelines,
                  vivid = TRUE,
                  verbose = 2,
                  timeScale = T,
                  summary = TRUE,
                  title = "Christopher Nolan",
                  subtitle = "Framelines")

p= plotTilesSummary(framelines.summary,
                    mode = "s",
                    verbose = 1,
                    title = "Love Death & Robots",
                    subtitle = "Saturation")

colorCircle(framelines[[1]],extra = TRUE)

temperature(framelines[[14]])

ggsave("bobo.png", plot = p, dpi = "retina",
       device = "png", scale = 1.5, width = 6, height = 3)

for(i in 1:length(framelines.redux)){
  p = colorCircle(framelines.redux[[i]],extra = TRUE)
  dev.copy(png,file.path(framesPath,paste("circle",i,".png",sep="")))
  dev.off()
}

extractFramePalette(paletteDim = 5)

bigframe = do.call("rbind",framelines)
bigframe$seconds=seq(from=bigframe$seconds[[1]],
                     to=length(bigframe$seconds)*(bigframe$seconds[[2]]-bigframe$seconds[[1]]),
                     by=(bigframe$seconds[[2]]-bigframe$seconds[[1]]))
plotFrameline(bigframe)
temperature(bigframe)
plotChannel(bigframe,"l",npoly = 1)
colorCircle(bigframe,extra = FALSE)
