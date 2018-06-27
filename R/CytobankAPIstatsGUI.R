#This function contains a shiny based graphical user interface (GUI) that allows for
#users to export population frequency and marker data from Cytobank to an rda and
#xlsx file as well as a PDF of a signaling heat map.
#' Exports processed events and signaling data
#' @import CytobankAPI
#' @import shiny
#' @import xlsx
#' @import shinyFiles
#' @import pheatmap
#' @examples  \dontrun{
#' library(CytobankAPIstats)
#' CytobankAPIstatsGUI()}
#' @export
CytobankAPIstatsGUI<-function(){
  ui<-fluidPage(
  tabsetPanel(id="tabset1",
              tabPanel("Welcome",
                       h1("Welcome to the CytobankAPIstats GUI!"),
                       actionButton("start","Click here to start")
                       ),
              tabPanel("Enter Credentials",
                       h1("Enter Cytobank user credentials"),
                       p("Please enter Cytobank user credentials to login through the CytobankAPI package. Username and password are the same as used for logging into Cytobank.org in a web browser. The site depends on type of subscription, may be 'premium' or a site specific license, for example 'wustl'. It is specified for your organization's login as the prefix before cytobank.org used for login."),
                       textInput("site","Site",""),
                       textInput("username","Username",""),
                       passwordInput(inputId="password","Password",""),
                       actionButton("connect", label = "Connect to Cytobank"),
                       hr(),
                       fluidRow(column(2, textOutput("login")))
                       
              ),
              tabPanel("Logged in",
                       p("Cytobank connection was sucessful"),
                       actionButton("goback1", label = "Click to go back"),
                       actionButton("continue1", label = "Click to continue")),
              tabPanel("Select Experiment of Interest",value="expts",
                       uiOutput("exptnames1")
              ),
              tabPanel("Select Analysis Type", 
                       p("Select events if interested in population frequency and markers if interested in looking at intensity of cytometry measured markers on different cell populations"),
                       checkboxGroupInput(inputId = "analysisType",label="Choose analysis type(s)",choices=c("Events","Markers")),
                       actionButton("goback3", label = "Click to go back"),
                       actionButton("continue3", label = "Click to continue")),
              tabPanel("Select Cell Populations",
                       uiOutput("pops")),
              tabPanel("Select Markers",
                       uiOutput("markers1")),
              tabPanel("Get statistics",
                       uiOutput("stats1"),
                       uiOutput("stats2"),
                       uiOutput("stats")),
              tabPanel("Export Data",
                       shinyDirButton("dir", "Chose directory to export data", "Upload"),
                       textInput(inputId="varname",label="Enter desired name for exported files"),
                       textOutput("dir"),
                       h1("Data will be exported to desired folder as .RData and .xlsx files with heat maps as pdf files."),
                       actionButton(inputId = "exportdat",label = "Export data"))
  ))

server<-function(input, output) {
  hideTab(inputId = "tabset1",target = "Enter Credentials")
  hideTab(inputId = "tabset1",target = "Logged in")
  hideTab(inputId = "tabset1",target = "expts")
  hideTab(inputId = "tabset1",target = "Select Analysis Type")
  hideTab(inputId = "tabset1",target = "Select Cell Populations")
  hideTab(inputId = "tabset1",target = "Select Markers")
  hideTab(inputId = "tabset1",target = "Get statistics")
  hideTab(inputId = "tabset1",target = "Export Data")
  variables<-reactiveValues(cellpops=c("a"),
  eventmat=matrix(0),
  events=matrix(0),
  exptID=1,
  exptlist=1,
  exptsid=1,
  key=1,
  markers=c("a"),
  markers12=c("a"),
populations12=c("a"),
savepath=c("a"),
signalmat=c(matrix(0)),
signals=c(matrix(0)))
  observeEvent(input$start,{
    showTab(inputId = "tabset1",target = "Enter Credentials")
    hideTab(inputId="tabset1",target="Welcome")
  })
  observeEvent(input$connect,{
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Logging in", value = 1)
    
    validation <- paste("authenticate(site='",input$site,"',username='",input$username,"',password='",isolate(input$password),"')",sep="")
    variables$key<-eval(parse(text=validation))

    variables$exptlist<-experiments.list(variables$key)
    success<-"Cytobank connection was sucessful"
    showTab(inputId = "tabset1",target = "Logged in")
    output$login<-renderPrint({success})
    hideTab(inputId="tabset1",target="Enter Credentials")
  })
  observeEvent(input$continue1, {
    showTab(inputId = "tabset1", target = "expts")
    hideTab(inputId = "tabset1", target = "Logged in")
    output$exptnames1 <- renderUI({
      tagList(
        selectInput(
          inputId = "expts1",
          label = "Select experiment(s) of interest",
          choices = variables$exptlist$experimentName
        ),
        actionButton("goback2", label = "Click to go back"),
        actionButton("continue2", label = "Click to continue")
      )
    })
  })
  
  observeEvent(input$goback1,{
    showTab(inputId = "tabset1",target = "Enter Credentials")
    hideTab(inputId = "tabset1",target="Logged in")
  })
  
  observeEvent(input$goback2,{
    showTab(inputId = "tabset1",target = "Logged in")
    hideTab(inputId = "tabset1",target="expts")
  })
  
  observeEvent(input$continue2,{
    showTab(inputId = "tabset1",target = "Select Analysis Type")
    hideTab(inputId = "tabset1",target="expts")
    variables$exptsofinterest<-variables$exptlist$id[input$expts1]
  })
  
  observeEvent(input$goback3,{
    showTab(inputId = "tabset1",target = "expts")
    hideTab(inputId = "tabset1",target="Select Analysis Type")
  })
  
  observeEvent(input$continue3,{
    showTab(inputId = "tabset1",target = "Select Cell Populations")
    hideTab(inputId = "tabset1",target="Select Analysis Type")
    exptsind<-grep(input$expts1,variables$exptlist$experimentName)
    variables$exptsid<-variables$exptlist$id[exptsind]
    variables$cellpops<-populations.list(UserSession =variables$key,experiment_id = variables$exptsid )
    output$pops<-renderUI({tagList(
      checkboxGroupInput(inputId="pops1",label="Select cell populations of interest",choices=variables$cellpops$name),
      actionButton("goback4", label = "Click to go back"),
      actionButton("continue4", label = "Click to continue"))})
  })
  
  observeEvent(input$goback4,{
    showTab(inputId = "tabset1",target = "Select Analysis Type")
    hideTab(inputId = "tabset1",target="Select Cell Populations")
  })
  
  observeEvent(input$continue4,{
    showTab(inputId = "tabset1",target = "Select Markers")
    variables$markers<-panels.list(UserSession = variables$key,experiment_id = variables$exptsid)
    markerstring<-paste("variables$markers","$`",ls(variables$markers),"`$channels$longName",sep="")
    output$markers1<-renderUI({tagList(
      checkboxGroupInput(inputId="markers2",label="Select cell markers of interest",choices=eval(parse(text=markerstring))),
      actionButton("goback5", label = "Click to go back"),
      actionButton("continue5", label = "Click to continue"))})
    
    hideTab(inputId = "tabset1",target="Select Cell Populations")
  })
  
  observeEvent(input$goback5,{
    showTab(inputId = "tabset1",target = "Select Cell Populations")
    hideTab(inputId = "tabset1",target="Select Markers")
  })
  
  observeEvent(input$continue5,{
    progress1 <- shiny::Progress$new()
    on.exit(progress1$close())
    progress1$set(message = "Calculating statistics", value = 1)
    showTab(inputId = "tabset1",target = "Get statistics")
    hideTab(inputId = "tabset1",target="Select Markers")
    variables$populations12<-input$pops1
    variables$exptID<-unlist(variables$exptsid)
    variables$markers12<-input$markers2
    if(length(input$analysisType)==1){
      if(input$analysisType=="Events"){
        variables$events<-analyzedata(cyto_session = variables$key,markersofinterest = variables$markers12,popsofinterest = variables$populations12,exptID=variables$exptID, type = T)
        output$stats1<-renderUI({tagList(
          #renderDataTable(events),
          checkboxGroupInput(inputId="eventsOnlySamples",label="Select samples to keep in downstream event analysis",choices = rownames(variables$events)),
          selectInput(inputId = "referenceEvents",label="Choose population as reference for percentage calculations",choices = variables$populations12),
          actionButton("goback6", label = "Click to go back"),
          actionButton("continue6", label = "Click to continue"))})
      }else{
        variables$signals<-analyzedata(cyto_session = variables$key,markersofinterest = variables$markers12,popsofinterest = variables$populations12,exptID=variables$exptID,type = F)
        output$stats2<-renderUI({tagList(
          #renderDataTable(signals),
          checkboxGroupInput(inputId="signalsOnlySamples",label="Select samples to keep in downstream marker analysis",choices = colnames(variables$signals)),
          selectInput(inputId = "referenceNorm",label="Choose sample as reference for marker normalization",choices = colnames(variables$signals)),
          selectInput(inputId = "normalize",label="Do you wish to normalize data by the arcsinh ratio in relation to a specific sample?",choices = c("Yes","No")),
          actionButton("goback7", label = "Click to go back"),
          actionButton("continue7", label = "Click to continue"))})
      }
    }else{
      variables$events<-analyzedata(cyto_session = variables$key,markersofinterest = variables$markers12,popsofinterest = variables$populations12,exptID=variables$exptID,type = T)
      variables$signals<-analyzedata(cyto_session = variables$key,markersofinterest = variables$markers12,popsofinterest = variables$populations12,exptID=variables$exptID,type = F)
      output$stats<-renderUI({tagList(
        fluidPage(
          fluidRow(
            column(4,
        #renderDataTable(events),
        #renderDataTable(signals),
        checkboxGroupInput(inputId="eventsOnlySamples1",label="Select samples to keep in downstream event analysis",choices = rownames(variables$events)),
        selectInput(inputId = "referenceEvents1",label="Choose population as reference for percentage calculations",choices = variables$populations12),
        selectInput(inputId = "perCompute1",label="Do you wish to compute percentage of events in total population?",choices = c("Yes","No"))),
        column(8,
        checkboxGroupInput(inputId="signalsOnlySamples1",label="Select samples to keep in downstream marker analysis",choices = colnames(variables$signals)),
        selectInput(inputId = "referenceNorm1",label="Choose sample as reference for marker normalization",choices = colnames(variables$signals)),
        selectInput(inputId = "normalize1",label="Do you wish to normalize data by the arcsinh ratio in relation to a specific sample?",choices = c("Yes","No")),
        actionButton("goback8", label = "Click to go back"),
        actionButton("continue8", label = "Click to continue")))))})
    }
  })
  observeEvent(input$goback6,{
    showTab(inputId = "tabset1",target = "Select Markers")
    hideTab(inputId = "tabset1",target="Get statistics")
  })
  
  observeEvent(input$continue6, {
    showTab(inputId = "tabset1", target = "Export Data")
    hideTab(inputId = "tabset1", target = "Get statistics")
    variables$eventmat<-
      variables$events[match(input$eventsOnlySamples, rownames(variables$events)),]
    variables$eventmat<-
      cbind(variables$eventmat[,match(input$referenceEvents, colnames(variables$events))], variables$eventmat[,-match(input$referenceEvents, colnames(variables$events))])
    variables$eventper<-calcperevent(results = variables$eventmat)
  })
  
  observeEvent(input$goback7,{
    showTab(inputId = "tabset1",target = "Select Markers")
    hideTab(inputId = "tabset1",target="Get statistics")
  })
  
  observeEvent(input$continue7, {
    showTab(inputId = "tabset1", target = "Export Data")
    hideTab(inputId = "tabset1", target = "Get statistics")
    variables$signalmat<-
      variables$signals[, match(input$signalsOnlySamples, colnames(variables$signals))]
    variables$signalmatnorm<-
      asinnorm(
        mat = variables$signalmat,
        col = grep(input$referenceNorm, colnames(variables$signalmat)),
        cofactor = 5
      )
  })
  
  observeEvent(input$goback8,{
    showTab(inputId = "tabset1",target = "Select Markers")
    hideTab(inputId = "tabset1",target="Get statistics")
  })
  
  observeEvent(input$continue8, {
    showTab(inputId = "tabset1", target = "Export Data")
    hideTab(inputId = "tabset1", target = "Get statistics")
    variables$eventmat<-
      variables$events[match(input$eventsOnlySamples1, rownames(variables$events)),]
    variables$eventmat<-
      cbind(variables$eventmat[,match(input$referenceEvents1, colnames(variables$events))], variables$eventmat[,-match(input$referenceEvents1, colnames(variables$events))])
      variables$eventper<-calcperevent(results = variables$eventmat)
    variables$signalmat<-
      variables$signals[, match(input$signalsOnlySamples1, colnames(variables$signals))]
      variables$signalmatnorm<-
      asinnorm(
        mat = variables$signalmat,
        col = grep(input$referenceNorm1, colnames(variables$signalmat)),
        cofactor = 5
      )
    })
  shinyDirChoose(input, 'dir', roots = c(home = '~'), filetypes = c('', 'txt'))

  
  
  observeEvent(input$exportdat,{
    dir2<-parseDirPath(roots=c(home = '~'),selection=input$dir)
    dir2<-path.expand(dir2)
    variables$savepath<-paste(dir2,input$varname,sep="/")
    if(length(input$analysisType)==1){
      if(input$analysisType=="Events"){
        eventper<-isolate(variables$eventper)
        eventmat<-isolate(variables$eventmat)
        saverda<-paste("save(eventper,eventmat,file='",variables$savepath,".rda')",sep="")
        savexlsx1<-paste("write.xlsx(variables$eventmat,file='",variables$savepath,".xlsx',sheetName='Raw event counts',append=T)",sep="")
        savexlsx2<-paste("write.xlsx(variables$eventper,file='",variables$savepath,".xlsx',sheetName='Percentage of events',append=T)",sep="")
      }else{
        signalmat<-isolate(variables$signalmat)
        signalmatnorm<-isolate(variables$signalmatnorm)
        saverda<-paste("save(signalmat,signalmatnorm,file='",variables$savepath,".rda')",sep="")
        savexlsx1<-paste("write.xlsx(variables$signalmat,file='",variables$savepath,".xlsx',sheetName='Raw markers',append=T)",sep="")
        savexlsx2<-paste("write.xlsx(variables$signalmatnorm,file='",variables$savepath,".xlsx',sheetName='Arcsinh ratio normalized markers',append=T)",sep="")
        heatmap1<-paste("pheatmap(mat=variables$signalmatnorm,color=colorRampPalette(c('navy', 'white', 'firebrick3'))(50),cluster_cols=F,cluster_rows=F,filename='",variables$savepath,".pdf')",sep="")
        eval(parse(text=heatmap1))
        }
    }else{
      eventper<-isolate(variables$eventper)
      eventmat<-isolate(variables$eventmat)
      signalmat<-isolate(variables$signalmat)
      signalmatnorm<-isolate(variables$signalmatnorm)
      saverda<-paste("save(eventper,eventmat,signalmat,signalmatnorm,file='",variables$savepath,".rda')",sep="")
      savexlsx1<-paste("write.xlsx(variables$eventmat,file='",variables$savepath,".xlsx',sheetName='Raw event counts',append=T)",sep="")
      savexlsx2<-paste("write.xlsx(variables$eventper,file='",variables$savepath,".xlsx',sheetName='Percentage of events',append=T)",sep="")
      savexlsx3<-paste("write.xlsx(variables$signalmat,file='",variables$savepath,".xlsx',sheetName='Raw markers',append=T)",sep="")
      savexlsx4<-paste("write.xlsx(variables$signalmatnorm,file='",variables$savepath,".xlsx',sheetName='Arcsinh ratio normalized markers',append=T)",sep="")
      heatmap1<-paste("pheatmap(mat=variables$signalmatnorm,color=colorRampPalette(c('navy', 'white', 'firebrick3'))(50),cluster_cols=F,cluster_rows=F,filename='",variables$savepath,".pdf')",sep="")
      eval(parse(text=savexlsx3))
      eval(parse(text=savexlsx4))
      eval(parse(text=heatmap1))
    }
    eval(parse(text=saverda))
    eval(parse(text=savexlsx1))
    eval(parse(text=savexlsx2))
    output$dir <- renderPrint("Data has been exported")
  })

  
}

shinyApp(ui = ui, server = server)
}
