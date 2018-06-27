.onLoad <- function(libname = find.package("CytobankAPIstats"), pkgname = "CytobankAPIstats"){
  

  # declaration of global variables (https://stackoverflow.com/questions/9439256)
  if(getRversion() >= "2.15.1"){
    utils::globalVariables(c("cellpops", "eventmat", "events", "exptID", "exptlist", "exptsid", "key","markers",
                             "markers12", "populations12", "savepath", "signalmat", "signals"))}
    if(getRversion() >= "3.1.0") {utils::suppressForeignCheck(c("cellpops", "eventmat", "events", "exptID", "exptlist", "exptsid", "key","markers",
                                                                "markers12", "populations12", "savepath", "signalmat", "signals"))}
  invisible()
  
}