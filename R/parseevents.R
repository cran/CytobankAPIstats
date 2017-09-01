#' Modifies the list obtained from a call to statistics.events to a matrix with rows corresponding to fcs files and columns corresponding to the population types 
#It takes results of statistics.events call (results), the population ids of interest (popsofinterest),
# and fcs file ids of interest (fcs) and returns a matrix of event counts with rows corresponding to fcs files
# and columns corresponding to populations of interest
#' @param  results - The results of a call to statistics.events function
#' @param  popsinterest - List of gateSetID numbers for populations of interest with descriptions as names
#' @param  fcs - List of fcs file IDs of interest with description of FCS files as names
#' @return Returns a matrix of event counts with rows corresponding to fcs files and columns corresponding to populations of interest
#' @examples \donttest{
#' library(CytobankAPI)
#' 
#' cyto_session <- authenticate(site="premium", username="myusername", password="mypassword")
#' exptno<-2
#' popsofinterest1<-c("CD4 T cells","NK cells")
#' popsinterest<-getpops(popsofinterest1,exptno,cyto_session)
#' fcs<-getfcsfiles(exptno,cyto_session)
#' results<-statistics.event_counts(cyto_session, exptno, gate_version = 1, 
#' compensation_id=1,fcs_files=fcs,populations = popsinterest,output = "default",
#' timeout = UserSession@long_timeout)
#' parseevents(results,popsinterest,fcs)}
#' @export
parseevents<-function(results,popsinterest,fcs){
  x<-matrix(0,nrow=length(fcs),ncol=length(popsinterest))
  rownames(x)<-names(fcs)
  colnames(x)<-names(popsinterest)
  col=vector()
  row=vector()
  for (i in 1:length(results$eventCounts)){
    col[i]=i%%length(popsinterest)
    if (col[i]==0){
      col[i]=length(popsinterest)
    }
    row[i]=ceiling(i/length(popsinterest))
    x[row[i],col[i]]=results$eventCounts[i]
  }
  return(x)
}