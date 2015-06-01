

GeotoXY <- function(geom){
  #Function that converts GPS coordinates into rectangular Cartesian system
  #geom
  #geoOrigin is global
  Olat = geoOrigin[1]
  Olon = geoOrigin[2]
  cst = 6371*pi/180
  x= (geom[2]- Olon)*cos(Olat)*cst
  y=(geom[1]-Olat)*cst
  return(c(x,y))
}



XYtoGeo<-function(xycoords){
  #Convert back xy-coordinates to geometric coordinates
  #geoOrigin is global
  Olat = geoOrigin[1]
  Olon = geoOrigin[2]
  cst = 6371*pi/180
  lat = Olat + (xycoords[2]/cst)
  lon =Olon + xycoords[1]/(cst*cos(Olat))
  return(c(lat,lon))
}
