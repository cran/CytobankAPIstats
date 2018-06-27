#' Rearranges signaling results matrix with rows in the desired order as outputs
#it takes the desired label order (fixlabels) and the results matrix (results: output of parsestats with signaling statistics run)
#' @param fixlabels - List of strings with desired order of labels
#' @param results - Output of call to parsestats
#' @return - Returns a matrix with rows organized in desired order specified by fixlabels
#' @examples \donttest{
#' #Example starting with obtaining data from Cytobank
#' library(CytobankAPI)
#' cyto_session <- authenticate(site="premium", username="myusername", password="mypassword")
#' markersofinterest<-c("CD3","CD56")
#' popsofinterest<-c("CD4 T cells","NK cells")
#' exptID=4
#' type=F
#' results<-analyzedata(cyto_session,markersofinterest,popsofinterest,exptID,type)
#' fixlabels<-c("CD4 T cells CD56","NK cells CD56","CD4 T cells CD3","NK cells CD3")
#' getnewind(fixlabels,results)}
#' 
#' #Example with simple matrix
#' data<-matrix(1:8,nrow=4,ncol=2,byrow=TRUE)
#' colnames(data)<-c("Control","Patient")
#' rownames(data)<-c("NK cells CD3","CD4 T cells CD3","CD4 T cells CD56","NK cells CD56")
#' fixlabels<-c("CD4 T cells CD56","NK cells CD56","CD4 T cells CD3","NK cells CD3")
#' getnewind(fixlabels,data)
#' @export
getnewind<-function(fixlabels,results){
  newind=vector()
  for (k in fixlabels){
    newind=c(newind,grep(k,rownames(results)))
  }
  return(results[newind,])
}