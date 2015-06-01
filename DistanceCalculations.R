
distcartesian <- function(a,b){
 #Distance between two points in the XY plane
  k =(a[1]-b[1])^2+(a[2]-b[2])^2
  return(sqrt(k))
}

disttosection <- function(p,a,b){
 #compute length to linear section of piece-wise linear path
  dist=0
  len=distcartesian(a,b)
  if (len==0){
    dist=distcartesian(p,a)
  }
  projection=((p[1]-a[1])*(b[1]-a[1])+(p[2]-a[2])*(b[2]-a[2]))/len^2
  if (projection<0){
    dist=distcartesian(p,a)
  }
  else
    if (projection>1){
      dist=distcartesian(p,b)
    }
  else dist=distcartesian(p,c(a[1]+projection*(b[1]-a[1]),a[2]+projection*(b[2]-a[2])))
  return(dist)
}


disttopath <- function(p,path){
 #path is made of several sections

  distance=disttosection(p,path[1,],path[2,])
  #search for minimum distance section
  for (i in 2:(nrow(path)-1)){
    if (distance>disttosection(p,path[i,],path[i+1,])){
      distance=disttosection(p,path[i,],path[i+1,])
    }
  }
  return(distance)
}
