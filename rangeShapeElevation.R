rangeShapeElevation <- function(data, out_shape, asc_in, elevation=NULL, buffer.elevation=200, buffer.circle=25, format=".tif", overlay=T) {
  getwd() -> wd
  colnames(data) <- c("Species", "x", "y")
  unique(data) -> data
  sort(unique(as.character(data$Species))) -> taxa
  for (i in 1:length(taxa)) {
    as.character(taxa[i]) -> sp0
    cat("Range of",sp0, fill=T)
    data[data$Species == sp0,] -> sp0.data
    if (nrow(sp0.data) == 1) {
      rbind(sp0.data, sp0.data) -> sp0.data
    }
    coordinates(sp0.data) = ~x+y
    range(extract(elevation, sp0.data)) -> t0
    t0[1]-buffer.elevation -> t0[1]
    t0[2]+buffer.elevation -> t0[2]
    x <- polygons(circles(sp0.data, d=buffer.circle*1000, lonlat=TRUE))
    crop(elevation, x) -> layer2
    mask(layer2, x) -> layer2
    layer2[layer2 < t0[1]] <- NA
    layer2[layer2 > t0[2]] <- NA
    rasterToPolygons(layer2, dissolve=F) -> conv2
    aggregate(conv2) -> convF
    if (overlay == T) {
      out0 <- lapply(convF@polygons , slot , "Polygons")[[1]]
      plist <- vector("list", length=length(out0))
      if (length(plist) > 1) {
        unlist(lapply(out0, FUN=function(x)(x@hole))) -> w0
        for (k in 1:length(out0)) {
          Polygons(list(Polygon(out0[[k]])), k) -> plist[[k]]
        }
        plist[which(w0)] -> w0
        SpatialPolygons(w0) -> w0
        SpatialPolygons(plist) -> convS
        na.omit(over(sp0.data, convS)) -> over0
        SpatialPolygons(plist[unique(over0)]) -> convF
        aggregate(convF) -> convF
      }
    } 
    convF<-SpatialPolygonsDataFrame(convF,data=as.data.frame("1"))
    setwd(out_shape)
    writeVector(vect(convF), filename = paste0(sp0, ".shp"), filetype = "ESRI Shapefile", overwrite=T)   
  }
  setwd(wd)  
}