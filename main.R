#workspace cleaning
remove(list = ls())
cat("\014")

library(chromaR)

framelines = lapply(getFrames(),function(x){groupframes(x,seconds = 10)})
framelines.summary = getSummary(framelines)

p = plotTimeWindows(verbose = 1,
                    vivid = TRUE,
                    title = "Title Test",
                    subtitle = "Sub Test",
                    left = "Left Test")

p = plotFrameline(framelines,
              vivid = TRUE,
              verbose = 1,
              timeScale = TRUE,
              summary = TRUE,
              title = "Title Test",
              subtitle = "Sub Test")

p= plotTilesSummary(framelines.summary,
                    mode = "lum",
                    verbose = 0,
                    title = "Title Test",
                    subtitle = "")

colorCircle(framelines[[1]],extra = TRUE)

temperature(framelines[[1]])

ggsave("tilesSat.png", plot = p, dpi = "retina",
       device = "png", path = framesPath, scale = 1.5, width = 6, height = 3)

for(i in 1:length(framelines.redux)){
  p = colorCircle(framelines.redux[[i]],extra = TRUE)
  dev.copy(png,file.path(framesPath,paste("circle",i,".png",sep="")))
  dev.off()
}

extractFramePalette(paletteDim = 5)
