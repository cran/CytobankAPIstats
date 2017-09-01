#' Calculates percentages of of cell types of interest out of total cell population
#columns are cell types. First column corresponds to the population as a total references, eg. all live PBMCs
#rows correspond to fcs files.
#a matrix with values as percent of first column is returned. Columns correspond to cell types
#Rows correspond to fcs files
#' @param  results - The result of a call to the parseevents function
#' @return Returns a matrix with values as percent of first column. Columns correspond to cell types. First column corresponds to the population as a total reference, eg. all live cells run. Rows correspond to fcs files.
#' 
#' @examples \donttest{
#' #Example starting with obtaining data from Cytobank
#' library(CytobankAPI)
#' results1<-statistics.event_counts(UserSession, experiment_id, gate_version = 1,
#' experiment_version, compensation_id,fcs_files, populations = c("Live","NK cells"),
#' output = "default",  timeout = UserSession@long_timeout)
#' popsofinterest<-c("CD4 T cells","NK cells")
#' cyto_session <- authenticate(site="premium", username="myusername", password="mypassword")
#' exptno<-2
#' popsinterest<-getpops(popsofinterest,exptno,cyto_session)
#' fcs<-getfcsfiles(exptno,cyto_session)
#' results<-parseevents(results1,popsinterest,fcs)
#' calcperevent(results)}
#'
#'#Example from simple dataset
#' data<-matrix(9:1,nrow=3,ncol=3,byrow=FALSE)
#' rownames(data)<-c("Control","Patient1","Patient2")
#' colnames(data)<-c("Live cells","Cell type 1","Cell type 2")
#' calcperevent(data)
#' @export
calcperevent<-function(results){
  total=results[,1]
  x<-results[,1:length(results[1,])]/total*100
  return(x)
}