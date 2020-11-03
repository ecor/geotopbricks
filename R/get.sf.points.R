NULL
#' Get a \code{\link{sf}} object for Meteorological Stations or Control Points in a GEOtop simulation
#'
#' @param prefix keyword prefix
#' @param suffixes keyword suffixes
#' @param coord_suffixes coordinate keyword suffixes. Default is \code{c("PointX","PointY")}
#' @param wpath GEOtop simulation path 
#' @param ... further arguments for \code{\link{get.geotop.inpts.keyword.value}}
#'
#'
#'
#' @examples
#' 
#' wpath <- "/stablo/local/simulations/sumava_test009b_distr/"
#' out <- get.geotop.points(wpath=wpath)
#' out <- get.geotop.points(prefix="CoordinatePoint",suffix=c("Code","Source"),wpath=wpath)
#'  out <- get.geotop.points(prefix="MeteoStation",suffix=c("Code","Source"),wpath=wpath)

get.geotop.points <- function(prefix=c("MeteoStation","CoordinatePoint"),suffixes=c("Code","Elevation","Source"),coord_suffixes=list(MeteoStation=c("CoordinateX","CoordinateY"),CoordinatePoint=c("X","Y")),wpath,...,vector_sep=",") {


##celevm <- colorNumeric(rev(brewer.pal(11,"BrBG")),domain=seq(from=0,to=1500,by=100))
###
if (!(prefix[[1]] %in% names(coord_suffixes)))  {
  
  stop("prefix and coord_suffixes not supported!")
  
}
coord_suffixes <- coord_suffixes[[prefix[1]]]
suffixes <- union(suffixes,coord_suffixes)  
print(suffixes)
print(coord_suffixes)

##
crs <- paste(wpath,"geotop.proj",sep="/") %>% getProjection()
##
if (!all(prefix %in% c("MeteoStation","CoordinatePoint"))) {
  
  stop ("prefix value not supported (see options in default value!")
}
    
if (!all(coord_suffixes %in% suffixes)) {
  
  stop ("suffix values not supported (X or Y missing)")
}
###DOMANI CONTINUARE
out_names <- paste0(prefix[1],suffixes)
names(out_names) <- suffixes

out_coords <- out_names[coord_suffixes]
out_names <- out_names[!(out_names %in% out_coords)
                       ]
geometry <- get.geotop.inpts.keyword.value(out_coords,wpath=wpath,numeric=TRUE,,vector_sep=vector_sep) %>% as.data.frame()
geometry <- geometry %>% t() %>% as.data.frame() %>% as.list() %>% lapply(st_point) %>% st_sfc()
out <- get.geotop.inpts.keyword.value(out_names,wpath=wpath,vector_sep=vector_sep,...) %>% as.data.frame() 
out <- st_sf(out,geometry=geometry,crs=crs)
# 
# 
# 
# for (it in coord_suffixes) {
# out[[out_names["CoordinateX"]]] <- get.geotop.inpts.keyword.value("CoordinatePointX",wpath=wpath,numeric=TRUE)
# checkpoints$CoordinatePointY <- get.geotop.inpts.keyword.value("CoordinatePointY",wpath=wpath,numeric=TRUE)
# checkpoints_g <- checkpoints[c("CoordinatePointX","CoordinatePointY")]  %>% as.data.frame() %>% t() %>% as.data.frame() %>% as.list() %>% lapply(st_point) %>% st_sfc() 
# checkpoints <- checkpoints %>% as.data.frame()
# checkpoints$CoordinatePointCode <- get.geotop.inpts.keyword.value("CoordinatePointCode",wpath=wpath,vector_sep=",")
# checkpoints$CoordinatePointSource <- get.geotop.inpts.keyword.value("CoordinatePointSource",wpath=wpath,vector_sep=",")
# checkpoints <- st_sf(checkpoints,geometry=checkpoints_g,crs=crs)

return(out)

}
