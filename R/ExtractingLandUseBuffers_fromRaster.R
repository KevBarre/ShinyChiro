extractLandUseRaster=function(Points,
                              bufwidth,
                              ID,
                              Hab,
                              saveDir
                              )
{


  # Converts coordinates into Lambert 93 (2154)
  Points <- spTransform (Points, CRS ("+init=epsg:2154"))

  HabufPropFinal = data.frame(Points[,ID])
  #HabufPropFinal = data.frame(Points[1:10,ID])

  for (j in bufwidth) {

    # Creating buffer around points

    Buffer = buffer(Points, width = j, dissolve = FALSE)

    #plot(Points, add=T)

    HabufProp=list()

    for (i in 1:nrow(Buffer)) {
    #for (i in 1:10) {

      Habuf=extract(Hab,Buffer[i,])

      HabufProp[[i]]=as.data.table(t(as.matrix(table(Habuf))/sum(table(Habuf))))

      if(i%%100==1){print(paste("BufferSize",j,Sys.time()))}

    }

    HabufProp2=rbindlist(HabufProp,fill=T)

    HabufPropTemp = as.data.frame(HabufProp2)

    colnames(HabufPropTemp)=paste0(colnames(HabufPropTemp),"_", j)

    HabufPropFinal=cbind(HabufPropFinal, HabufPropTemp)

  }

  HabufPropFinal[is.na(HabufPropFinal)]=0

  fwrite(HabufPropFinal, paste0(saveDir,"/crossedTableRaster.csv"))

  return(HabufPropFinal)

}
