#' Filters matrix columns based on two conditions per file, e.g. patient status, stimluation, time points, etc.
#It returns a matrix of statistics results correponding to columns that meet both criteria described in key1 and key2
#Care must be taken when using this function, as some experiments are labeled differently than others
#' @param  key1 - Search string of interest for names
#' @param  key2 - Search string of interest for names
#' @param  results - Results matrix with fcs files corresponding to columns
#' @return Returns a list of IDs with names matching search both serach strings with names being the description of these features
#' @examples \donttest{
#' #Example starting with obtaining data from Cytobank
#' library(CytobankAPI)
#' cyto_session <- authenticate(site="premium", username="myusername", password="mypassword")
#' markersofinterest<-c("CD3","CD56")
#' popsofinterest<-c("CD4 T cells","NK cells")
#' exptID=4
#' type=F
#' results<-analyzedata(cyto_session,markersofinterest,popsofinterest,exptID,type)
#' key1<-"Patient"
#' key2<-"Untreated"
#' get2status(key1,key2,results)}
#' 
#' #Example with simple data matrix
#' data<-matrix(1:18,nrow=3,ncol=6,byrow=TRUE)
#' colnames(data)<-c("Ctrl1 unst","Pt1 unst","Pt3 unst","Ctrl1 stim","Pt1 stim","Pt3 stim")
#' rownames(data)<-c("Marker1","Marker2","Marker3")
#' #Getting all stimulated patient samples
#' get2status(c("Pt","Ctrl"),"stim", data)
#' #Getting all stimulated patient and control samples
#' get2status(c("Pt","Ctrl"),"stim", data)
#' @export
get2status<-function(key1,key2,results){
  key1ind=vector()
  key2ind=vector()
  for (i in key1){
    key1ind=union(key1ind,grep(i,colnames(results)))
  }
  for (i in key2){
    key2ind=union(key2ind,grep(i,colnames(results)))
  }
  keepind=intersect(key1ind,key2ind)
  keepind=sort(keepind)
  return(results[,keepind])
}
