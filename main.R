.session = list(
  name = "MIYAZAKI COLLECTION",
  fps = 24,
  linesize = list("ultrathin"=1500,"thin"=900,"middle"=500,"bold"=250,"bold"=140),
  homedir = dirname(dirname(rstudioapi::getSourceEditorContext()$path)),
  framespath = dirname(dirname(rstudioapi::getSourceEditorContext()$path))
)

source(file.path(.session$homedir,"src/init.R"), echo = FALSE)

framelines = getFrames()

framelines.redux = lapply(framelines,function(x){groupframes(x,seconds = 10)})

####Try different groupings
# seconds = c(1,5,10,20,35,60,120,200,500,1000)
# gglist = list()
# for(second in seconds){
#   framelines.redux = lapply(framelines,function(x){groupframes(x,seconds = second)})[1:2]
#   p=plotFramesCollection(framelines.redux, season = c(1,2,3,4,5),
#                          verbose = 0, vivid = TRUE, scaleTime = TRUE)
# }
# f = do.call("grid.arrange", c(grobs = gglist, ncol=1))

plotFrameline(framelines.redux,vivid = TRUE,verbose = 1, timeScale = TRUE,
               title = "LOVE, DEATH + ROBOTS", summary = TRUE,
               subtitle = "Episodes framelines")

#PRINT FRAMELINES
p = plotFramesCollection(framelines.redux, season = c(1,2,3,4,5),
                         verbose = 2, vivid = FALSE, scaleTime = TRUE)
ggsave("allFrames.png", plot = p, dpi = "retina",
       device = "png", path = framesPath, scale = 1.5, width = 16, height = 9)
p = plotFramesCollection(framelines.redux, season = c(1,2,3,4,5),
                         verbose = 2, vivid = TRUE, scaleTime = TRUE)
ggsave("allFramesVivid.png", plot = p, dpi = "retina",
       device = "png", path = framesPath, scale = 1.5, width = 9, height = 16)
p = plotFramesCollection(framelines.redux, season = c(1,2,3,4,5),
                         verbose = 2, vivid = TRUE, scaleTime = FALSE)
ggsave("allFramesVividTime.png", plot = p, dpi = "retina",
       device = "png", path = framesPath, scale = 1.5, width = 16, height = 9)
p = plotFramesCollection(framelines.redux, season = c(1,2,3,4,5),
                         verbose = 0, vivid = TRUE, scaleTime = TRUE)
ggsave("allFramesArt.png", plot = p, dpi = "retina",
       device = "png", path = framesPath, scale = 1.5, width = 16, height = 9)

summary = getSummary(framelines.redux)

p= plotTilesSummary(summary)

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
