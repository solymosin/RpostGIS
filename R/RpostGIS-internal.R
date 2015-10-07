.pont.pars <-
function(geomi)
{
   geomi = sub(paste("POINT " , sep=""), "", geomi)
   geomi = sub("POINT", "", geomi) 
   geomi = gsub("\\(", "", geomi)
   geomi = gsub("\\)", "", geomi)   
   geomi = strsplit(geomi, " ")   
   geomi = as.numeric(unlist(.splsp(geomi[[1]])))
   return(geomi)
}
.poly.pars <-
function(u)
{
   u = gsub("\\(", "", u)
   u = gsub("\\)", "", u)
   u = gsub(", ", ",", u)
   u = strsplit(u, ",")   
   u = .splsp(u[[1]])
   x = as.numeric(unlist(u)[seq(1,length(unlist(u)),2)])
   y = as.numeric(unlist(u)[seq(2,length(unlist(u)),2)])   
   m = cbind(x,y)
   m = m[is.na(m[, 2]) == FALSE, ]
   m = m[is.na(m[, 1]) == FALSE, ]   
   return(m)
}
.splsp <-
function(x) strsplit(x, " ")
