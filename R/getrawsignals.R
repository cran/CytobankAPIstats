#' Computes the untransformed medians for cellular markers in populations of interest
#This function takes an API validation  key (cyto_session), a list of strings of markers of interest (markersofinterest),
#a list of strings of cellular populations of interest (popsofinterest),the ID number for the experiment to analyze (exptID),
#a list of stings corresponding to the desired marker order (markerorder), a list of desired stimulation conditions (stimterms),
#and a list of sample conditions (ptterms)
#getrawsignals(cyto_session,markersofinterest,popsofinterest,exptID,markerorder,stimterms,ptterms)
#' @param cyto_session - API authentication token for session
#' @param markersofinterest - List of strings of markers of interest, corresponding to names in Cytobank
#' @param popsofinterest - List of strings of populations of interest to calculate statistics
#' @param exptID - Integer representing an experiment ID on Cytobank account
#' @param markerorder - A list of stings corresponding to the desired marker order
#' @param stimterms - A list of desired stimulation conditions to analyze in matrix
#' @param ptterms - A list of desired sample conditions to analyze in matrix
#' @return - Returns matrix of untransformed medians for cellular markers in populations of interest
#' @examples \donttest{
#' library(CytobankAPI)
#' cyto_session <- authenticate(site="premium", username="myusername", password="mypassword")
#' markersofinterest<-c("CD3","CD56")
#' popsofinterest<-c("CD4 T cells","NK cells")
#' exptID=4
#' markerorder<-c("CD4 T cells CD56","NK cells CD56","CD4 T cells CD3","NK cells CD3")
#' stimterms<-c("Unstim","IL-15")
#' ptterm<-c("Pt","Ctrl")
#' getrawsignals(cyto_session,markersofinterest,popsofinterest,exptID,markerorder,stimterms,
#' ptterms)}
#' @export
getrawsignals<-function(cyto_session,markersofinterest,popsofinterest,exptID,markerorder,stimterms,ptterms){
  signalstats<-analyzedata(cyto_session = cyto_session,markersofinterest = markersofinterest,popsofinterest =popsofinterest ,exptID=exptID,type=FALSE)
  signalstats<-getnewind(fixlabels=markerorder,signalstats)
  stimind=vector()
  ptind=vector()
  for (i in stimterms){
    stimind=union(stimind,grep(i,colnames(signalstats)))
  }
  for (i in ptterms){
    ptind=union(ptind,grep(i,colnames(signalstats)))
  }
  keepind=intersect(stimind,ptind)
  keepind=sort(keepind)
  return(signalstats[,keepind])
}
