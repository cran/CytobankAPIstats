#' Takes the results of a call to statistics.general and returns a matrix of raw means with columns corresponding to fcs files and rows to molecules of interest in different cell types
#It takes the results of a call to statistics.general (results), as well as the 
#inputs in the call to this function: ids populations of interest (popsintererst),
#fcs file ids of interest (fcs), marker ids of interest (markersofinterest). It returns
#a matrix with columns corresponding to fcs files and rows corresponding to markers
#of interest in cell types of interest
#' @param  results - The results of a call to statistics.general function
#' @param  popsinterest - List of gateSetID numbers for populations of interest with descriptions as name
#' @param  fcs - List of fcs file IDs of interest with description of fcs file names as names
#' @param markersofinterest - List of ID numbers for markers of interest with descriptions as name
#' @return Returns a matrix of mean signaling intensities with columns corresponding to fcs files and rows corresponding to markers of interest in cell types of interest
#' @examples \donttest{
#' library(CytobankAPI)
#' cyto_session <- authenticate(site="premium", username="myusername", password="mypassword")
#' exptno<-2
#' popsofinterest1<-c("CD4 T cells","NK cells")
#' popsinterest<-getpops(popsofinterest1,exptno,cyto_session)
#' fcs<-getfcsfiles(exptno,cyto_session)
#' markersofinterest1<-c("CD3","CD56")
#' markersofinterest<-getmarkers(markersofinterest1,exptno,cyto_session)
#' results<-statistics.general(UserSession=cyto_session, experiment_id=2, gate_version = -1, 
#' compensation_id=1,fcs_files=fcs, populations = popsinterest,
#' output = "default",timeout = UserSession@long_timeout)
#' parsestatsmean(results,popsinterest,fcs,markersofinterest)}
#' @export
parsestatsmean<-function(results,popsinterest,fcs,markersofinterest){
  x=matrix(nrow=length(markersofinterest)*length(popsinterest),ncol=length(fcs))
  colnames(x)<-names(fcs)
  m<-names(markersofinterest)
  newmarkerorder<-vector()
  for (a in names(results[[1]]$means)){
    newmarkerorder<-c(newmarkerorder,grep(a,markersofinterest))
  }
  m<-m[newmarkerorder]
  markers<-c(rep(m,length(popsinterest)))
  c<-names(popsinterest)
  pops<-vector()
  for (j in c){
    pops<-c(pops,rep(j,length(markersofinterest)))
  }
  features<-vector()
  for (k in 1:length(pops)){
    features[k]<-paste(pops[k],markers[k],sep=" ")
  }
  
  rownames(x)<-t(features)
  col=vector()
  row=vector()
  
  col[1]=1
  row[1]=1
  dimrow<-length(markersofinterest)*length(popsinterest)
  numpops<-length(popsinterest)
  x[row[1]:(row[1]+length(markersofinterest)-1),col[1]]<-as.double(results[[1]]$means)
  
  for (i in (2:length(results))){
    inc=row[i-1]+length(markersofinterest)
    row[i]=inc%%dimrow
    col[i]=ceiling(i/numpops)
    if(length(results[[i]]$means)==0){
      results[[i]]$means<-c(rep(0,length(markersofinterest)))
    }
    x[row[i]:(row[i]+length(markersofinterest)-1),col[i]]=as.double(results[[i]]$means)
  }
  return(x)
}