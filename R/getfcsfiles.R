#' Gets fcs ID numbers and sample names from a given experiment
#It's inputs are the ID# of the experiment of interest (exptno) and API authentication token (cyto_session)
#This function returns a list of all fileIDs with their names
#' @param  exptno - Integer representing an experiment ID on Cytobank account
#' @param  cyto_session - API authentication token for session
#' @return Returns a list of fcs file IDs with names of fcs files as names of list
#' @examples \donttest{
#' library(CytobankAPI)
#' cyto_session <- authenticate(site="premium", username="myusername", password="mypassword")
#' exptno<-2
#' getfcsfiles(exptno,cyto_session)}
#' @export
getfcsfiles<-function(exptno,cyto_session){
  allfcsfiles<-fcs_files.list(cyto_session,experiment_id = exptno)
  filenames<-allfcsfiles$filename
  fileid<-allfcsfiles$id
  names(fileid)<-filenames
  return(fileid)
}