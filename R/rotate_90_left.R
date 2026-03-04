rotate_90_left <- function(sf_obj){

  if(!inherits(sf_obj, "sf"))
    stop("Object must be sf")

  # centre of rotation
  bb <- st_bbox(sf_obj)
  cx <- (bb["xmin"] + bb["xmax"]) / 2
  cy <- (bb["ymin"] + bb["ymax"]) / 2

  geom_type <- unique(st_geometry_type(sf_obj))

  # ---- POINTS ----
  if(all(geom_type %in% c("POINT","MULTIPOINT"))){

    xy <- st_coordinates(sf_obj)

    x0 <- xy[,1] - cx
    y0 <- xy[,2] - cy

    new_xy <- cbind(
      -y0 + cx,
      x0 + cy
    )

    st_geometry(sf_obj) <- st_sfc(
      lapply(seq_len(nrow(new_xy)),
             function(i) st_point(new_xy[i,])),
      crs = st_crs(sf_obj)
    )

    return(sf_obj)
  }

  # ---- POLYGONS ----
  if(all(geom_type %in% c("POLYGON","MULTIPOLYGON"))){

    new_geom <- lapply(st_geometry(sf_obj), function(g){

      coords <- st_coordinates(g)

      x0 <- coords[,1] - cx
      y0 <- coords[,2] - cy

      coords[,1] <- -y0 + cx
      coords[,2] <-  x0 + cy

      # rebuild polygon
      st_polygon(list(coords[,1:2]))
    })

    st_geometry(sf_obj) <- st_sfc(new_geom, crs = st_crs(sf_obj))
    return(sf_obj)
  }

  stop("Geometry type not supported")
}
