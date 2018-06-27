#analyze data returns matrix containing either event counts or medians for an experiment of interest
#This function takes an API validation key (cyto_session), a list of channels of interest (markersofinterest),
# a list of cell types of interest (popsofinterest), the ID for the experiment of interest (exptID),
#and the type of analysis (type); type is a boolean, for event analysis, type=TRUE, for statistics, type=FALSE
#' Returns a matrix of event counts or raw medians, as specified by inputs. Columns correspond to fcs files and rows to markers in cell types
#' @param  cyto_session - API authentication token for session
#' @param  markersofinterest - Names of channel parameters in Cytobank as list of strings
#' @param popsofinterest - Names of gates of interest in Cytobank as list of strings
#' @param exptID - Integer representing an experiment ID on Cytobank account
#' @param type - boolean with TRUE to analyze events, FALSE to analyze marker intensity statistics
#' @return Returns a data matrix of event counts or raw signal medians, as specified by variable type
#' @import CytobankAPI
#' @examples \donttest{
#' library(CytobankAPI)
#' cyto_session <- authenticate(site="premium", username="myusername", password="mypassword")
#' markersofinterest<-c("CD3","CD56")
#' popsofinterest<-c("CD4 T cells","NK cells")
#' exptID=4
#' type=TRUE
#' analyzedata(cyto_session,markersofinterest,popsofinterest,exptID,type)}
#' @export
analyzedata<-function(cyto_session,markersofinterest,popsofinterest,exptID,type){
  
  pops<-getpops(popsofinterest=popsofinterest,exptno = exptID,cyto_session = cyto_session)
  markers<-getmarkers(markersofinterest = markersofinterest,exptno = exptID,cyto_session = cyto_session)
  fcs<-getfcsfiles(exptno = exptID,cyto_session = cyto_session)
  y<-experiments.show(UserSession = cyto_session,experiment_id = exptID)
  if (type==TRUE){
    eventsi<-statistics.event_counts(UserSession = cyto_session, experiment_id = exptID,gate_version =y$gateVersion,compensation_id = -2, fcs_files = fcs,populations=pops,timeout = 10000,output="dataframe")
    events<-parseevents(results=eventsi,popsinterest = popsofinterest,fcs = fcs)
    colnames(events)<-popsofinterest
    return(events)
  }
  medsi<-statistics.general(UserSession = cyto_session, experiment_id = exptID,gate_version =y$gateVersion,compensation_id = -2, fcs_files = fcs,populations=pops,timeout = 10000,output="default",channels = markers)
  meds<-parsestats(results=medsi,popsinterest = pops,fcs=fcs,markersofinterest = markers)   
  return(meds)
}
