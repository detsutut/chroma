.session = list(
  fps = 24,
  linesize = list("ultrathin"=1500,"thin"=900,"middle"=500,"bold"=250,"bold"=140),
  homedir = dirname(rstudioapi::getSourceEditorContext()$path),
  framespath = dirname(rstudioapi::getSourceEditorContext()$path)
)

source(file.path(.session$homedir,"init.R"), echo = FALSE)

framelines = lapply(getFrames(),function(x){groupframes(x,seconds = 10)})
framelines.summary = getSummary(framelines)

p = plotTimeWindows(verbose = 1,title = "Nausicaa of the Valley of the Wind     Princess Mononoke")
ggsave(fileName("frameline",ext = "png"), plot = p, path = .session$framespath,
       dpi = "retina",  device = "png", scale = 1, width = 9, height = 16)

p = plotFrameline(framelines,
              vivid = TRUE,
              verbose = 1, 
              timeScale = TRUE,
              summary = TRUE,
              title = "MIYAZAKI",
              subtitle = "Movie Collection")
ggsave(fileName("frameline",ext = "png"), plot = p, path = .session$framespath,
       dpi = "retina",  device = "png", scale = 1.5, width = 9, height = 16)


#PRINT FRAMELINES
p = plotFramesCollection(framelines.redux, season = c(1,2,3,4,5),
                         verbose = 2, vivid = FALSE, scaleTime = TRUE)
ggsave("allFrames.png", plot = p, dpi = "retina",
       device = "png", path = .session$framespath, scale = 1.5, width = 16, height = 9)
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
