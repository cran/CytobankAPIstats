#' Filters a list of fcs files by search terms
#This function returns a filtered list of fcs files of interest from a larger list
#It takes the list of file IDs with FCS file names as names (files), and one or more strings of interest as a list to filter samples(string)
#It may be used to filter samples by pt/control status or stimulation. 
#It returns a list of sample IDs with their names meeting filtering criteria
#' @param  files - List of fcs file IDs with FCS file name as names for list
#' @param  string - List of one or more strings of interest as a list to filter samples
#' @return Returns a list of  file IDs matching with names matching string(s)
#' @examples \donttest{
#' #Example starting with obtaining data from Cytobank
#' library(CytobankAPI)
#' exptno<-4
#' cyto_session <- authenticate(site="premium", username="myusername", password="mypassword")
#' files<-getfcsfiles(exptno,cyto_session)
#' string<-c("patient","IL-1b")
#' filterfiles(file,string)}
#' 
#' #Simple example when list of file names is already available
#' files<-1:4
#' names(files)<-c("Pt1 unst.fcs","Pt2 stim.fcs","Ctrl1 unst.fcs","Ctrl2 stim.fcs")
#' #Filtering file list to contain only unstimulated files
#' filterfiles(files,"unstimulated")
#' #Filtering file list to contain only patient files
#' filterfiles(files,"Pt")
#' #Filtering file list to contain both unstimulated and patient files
#' filterfiles(files,c("Pt","unst"))
#' @export
filterfiles<-function(files,string){
  ind=vector()
  for(x in string){
    ind=union(ind,grep(x,names(files)))
  }
  ind=sort(ind)
  filtered=files[ind]
  return(filtered)
}