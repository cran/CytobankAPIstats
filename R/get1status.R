#' Filters matrix based on single sample name condition
#It returns a matrix of statistics results correponding to columns of interest for all search keys in list key
#Care must be taken when using this function, as some experiments are labeled differently than others
#' @param  key - Search string of interest for names
#' @param  results - Results matrix with fcs files corresponding to columns
#' @return Returns a matrix with columns matching all search keys
#' @examples \donttest{
#' #Example starting with obtaining data from Cytobank
#' library(CytobankAPI)
#' cyto_session <- authenticate(site="premium", username="myusername", password="mypassword")
#' markersofinterest<-c("CD3","CD56")
#' popsofinterest<-c("CD4 T cells","NK cells")
#' exptID=4
#' type=F
#' results<-analyzedata(cyto_session,markersofinterest,popsofinterest,exptID,type)
#' key<-c("Patient","Control")
#' get1status(key,results)}
#' 
#' #Example with simple data matrix
#' data<-matrix(1:18,nrow=3,ncol=6,byrow=TRUE)
#' colnames(data)<-c("Ctrl1 unst","Pt1 unst","Pt3 unst","Ctrl1 stim","Pt1 stim","Pt3 stim")
#' rownames(data)<-c("Marker1","Marker2","Marker3")
#' #Getting all patient samples
#' get1status("Pt",data)
#' #Getting all patient and stimulated samples
#' get1status(c("Pt","stim"),data)
#' 
#' @export
get1status<-function(key,results){
  keyind=vector()
  for (i in key){
    keyind=union(keyind,grep(i,colnames(results)))
  }
  keyind=sort(keyind)
  return(results[,keyind])
}
