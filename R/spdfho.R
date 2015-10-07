spdfho <-
function(SrDf)
{
   if (class(SrDf)[1] == "SpatialPolygonsDataFrame")
   {
      lukas = c()
      for (i in 1:length(SrDf@polygons))
      {
         ps = SrDf@polygons[i][[1]]
         for (p in ps@Polygons)
         {
            if(p@hole==TRUE) lukas = c(lukas, i)
         }
      }
      SrDf@plotOrder = c(lukas,SrDf@plotOrder[-lukas])
      return(SrDf)
   }
}
