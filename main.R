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
                    mode = "lum", verbose = 0,
                    title = "Title Test", subtitle = "")

ggsave("tilesSummary.png", plot = p, dpi = "retina",
       device = "png", path = framesPath, scale = 1.5, width = 6, height = 3)

p= plotTilesSummary(summary,mode="lum")

ggsave("tilesLum.png", plot = p, dpi = "retina",
       device = "png", path = framesPath, scale = 1.5, width = 6, height = 3)
p= plotTilesSummary(summary,mode="h")

ggsave("tilesHue.png", plot = p, dpi = "retina",
       device = "png", path = framesPath, scale = 1.5, width = 6, height = 3)

p= plotTilesSummary(summary,mode="s")

ggsave("tilesSat.png", plot = p, dpi = "retina",
       device = "png", path = framesPath, scale = 1.5, width = 6, height = 3)

for(i in 1:length(framelines.redux)){
  p = colorCircle(framelines.redux[[i]],extra = TRUE)
  dev.copy(png,file.path(framesPath,paste("circle",i,".png",sep="")))
  dev.off()
}
temperature(framelines.redux[[i]])

extractFramePalette(paletteDim = 5)
