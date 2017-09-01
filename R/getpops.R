#' Gets appropriate gate set IDs for populations of interest 
#populations of interest (variable popsofinterest) should be input as a list strings, "Live",B cells",etc.These strings should match population names given in Cytobank gating
#The exptid for the experiment of interest is also an input. This is obtained from the experiments.list function.
#The function also takes cyto_session, an API token from calling the authenticate function
#This function returns a list of population names (names) and IDs (values) for those of interest in the given expt. 
#' @param  popsofinterest - Names of gates of interest in Cytobank as list of strings
#' @param  exptno - Integer representing an experiment ID on Cytobank account
#' @param  cyto_session - API authentication token for session
#' @return Returns a list of gateSetIDs for populations of interest with names of populations as names of list
#' @import CytobankAPI
#' @examples \donttest{
#' library(CytobankAPI)
#' popsofinterest<-c("CD4 T cells","NK cells")
#' cyto_session <- authenticate(site="premium", username="myusername", password="mypassword")
#' exptno<-2
#' getpops(popsofinterest,exptno,cyto_session)}
#' @export
getpops<-function(popsofinterest,exptno,cyto_session){
  allpops<-populations.list(cyto_session,exptno)
  indpop<-vector()
  i=1
  for (x in popsofinterest){
    indpop[i]=grep(paste("^",x,"$",sep=""),allpops$name)
    i=i+1
  }
  idpop<-allpops$gateSetId[indpop]
  names(idpop)<-popsofinterest
  return(idpop)
}