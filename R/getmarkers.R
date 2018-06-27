#' Gets appropriate marker IDs for channels of interest
#Inputs are markers of interest, a list containing strings of channel names wanted for
#further analyses ex "CD3","p-STAT1", etc. These should correspond to channel names given on Cytobank
#exptno is the id for the experiment of interest
#cyto_session is the API authentication token
#This function returns a list with the names and ID numbers for markers of interest
#' @param  markersofinterest - Names of channel parameters in Cytobank as list of strings
#' @param  exptno - Integer representing an experiment ID on Cytobank account
#' @param  cyto_session - API authentication token for session
#' @return Returns a list of IDs for markers of interest with names of markers as names of list
#' @import CytobankAPI
#' @examples \donttest{
#' library(CytobankAPI)
#' markersofinterest<-c("CD3","CD56")
#' cyto_session <- authenticate(site="premium", username="myusername", password="mypassword")
#' exptno<-2
#' getmarkers(markersofinterest,exptno,cyto_session)}
#' @export
getmarkers<-function(markersofinterest,exptno,cyto_session){
  allmarkers<-panels.list(cyto_session,experiment_id = exptno)
  panel<-ls(allmarkers)
  panel<-paste('`',panel,'`',sep="")
  search<-paste("allmarkers$",panel,"$channels$longName",sep="")
  indmarker<-vector()
  i=1
  for (x in markersofinterest){
    indmarker[i]=grep(paste("^",x,"$",sep=""),eval(parse(text=search)))
    i=i+1
  }
  idname<-paste('allmarkers$',panel,'$channels$normalizedShortNameId[indmarker]',sep="")
  idmarker<-eval(parse(text=idname))
  names(idmarker)<-markersofinterest
  return(idmarker)
}