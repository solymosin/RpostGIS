wkt2sp <-
function(geoms, gcol, idcol)
{   
   mt = sub(' ', '', strsplit(geoms[1,gcol], '\\(')[[1]][1])
   
   if (mt == "POINT")
   {
      xc = c()
      yc = c()
      for (geomi in geoms[,gcol])
      {
         xc = c(xc, .pont.pars(geomi)[1])
         yc = c(yc, .pont.pars(geomi)[2])
      }
      xy.sp = SpatialPoints(cbind(xc,yc))      
      return(xy.sp)
   }   
   
   if (mt == "LINESTRING" | mt == "MULTILINESTRING")
   {
      ids = as.character(geoms[,idcol])
      shps = list()
      n = 1
      for(i in 1:dim(geoms)[1])
      {
         geomi = geoms[i,gcol]
         typ = sub(' ', '', strsplit(geomi, '\\(')[[1]][1])
         geom.i = as.character(geomi)
        
         if (typ == "LINESTRING") 
         {
            plist = list()
            geomi = sub(paste(typ, " " , sep=""), "", geomi); geomi = sub(typ, "", geomi)         
            geomi = gsub("\\), \\(", "\\),\\(", geomi)
            geomi = strsplit(geomi, "\\),\\(")[[1]]       
            for (u in geomi)
            {
               m = .poly.pars(u)
               plist[[1]] = Line(m)            
            }
            srs = Lines(plist, ID= ids[i])
            shps[[n]] = srs
            n = n+1
         }   
         if (typ == "MULTILINESTRING") 
         {       
            geomi = sub(paste(typ, " " , sep=""), "", geomi); geomi = sub(typ, "", geomi)      
            geomi = gsub("\\), \\(", "\\),\\(", geomi)
            polys = strsplit(geomi, "\\)\\),\\(\\(")[[1]]   
            plist = list()
            p = 1
            for (poly in polys)
            {                
               poly = strsplit(poly, "\\),\\(")[[1]]                  
               for (u in poly)
               {
                  m = .poly.pars(u)
                  plist[[p]] = Line(m)
                  p = p+1
               }                   
            }
            srs = Lines(plist, ID= ids[i])
            shps[[n]] = srs
            n = n+1       
         }
         
      }
      SpP = SpatialLines(shps)
      return(SpP)
   }   
   
   if (mt == "POLYGON" | mt == "MULTIPOLYGON")
   {
      ids = geoms[,idcol]
      shps = list()
      n = 1
      for(i in 1:dim(geoms)[1])
      {
         geomi = geoms[i,gcol]
         typ = sub(' ', '', strsplit(geomi, '\\(')[[1]][1])
         geom.i = as.character(geomi)   

# 	 print(i)
        
         if (typ == "POLYGON") 
         {
            plist = list()
            geomi = sub(paste(typ, " " , sep=""), "", geomi); geomi = sub(typ, "", geomi)         
            geomi = gsub("\\), \\(", "\\),\\(", geomi)
            geomi = strsplit(geomi, "\\),\\(")[[1]]       
            j = 1
            for (u in geomi)
            {
               m = .poly.pars(u)
               if (j==1) plist[[j]] = Polygon(m)
               if (j==2) plist[[j]] = Polygon(m, hole=TRUE)         
               j = j+1
            }
            srs = Polygons(plist, ids[i])            
            shps[[n]] = srs
            n = n+1
         }     
        
         if (typ == "MULTIPOLYGON") 
         {       
            geomi = sub(paste(typ, " " , sep=""), "", geomi); geomi = sub(typ, "", geomi)      
            geomi = gsub("\\), \\(", "\\),\\(", geomi)
            polys = strsplit(geomi, "\\)\\),\\(\\(")[[1]]   
            plist = list()
            p = 1
            for (poly in polys)
            {                
               j = 1
               poly = strsplit(poly, "\\),\\(")[[1]]                  
               for (u in poly)
               {
                  m = .poly.pars(u)
                  if (j==1) plist[[p]] = Polygon(m)
                  if (j==2) plist[[p]] = Polygon(m, hole=TRUE)
                  j = j+1
                  p = p+1
               }                   
            }
            srs = Polygons(plist, ids[i])
            shps[[n]] = srs
            n = n+1       
         }  
#          print(i)
      }
      SpP = SpatialPolygons(shps, 1:length(shps))
      return(SpP)
   }
}
