#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
options(shiny.maxRequestSize=100*1024^2) 

# library(org.Hs.eg.db)
library(shiny)
library(clusterProfiler)
library(shinymanager)
library(annotationTools)
library(dplyr)
library("shinybusy")
library(DT)
library(multiMiR)
# Define server logic required to draw a histogram
options(repos = BiocManager::repositories())
credentials <- data.frame(
  user = c("karen", "1a"),
  password = c("karen123", "1a"), # password will automatically be hashed
  stringsAsFactors = FALSE)

server <- function(input, output,session) {
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  
 ###  SELECCIO DELS DATATSET DE AFFYMETRIX PER EL BOXLPLOT
  observe({
    x <- input$dataset
    b<-NA
    if(!is.null(x)){
    
        db<-db_selected_1()
        b<-colnames(db)
      }
      
    ## actualitzar la info a seleccionar
      updateCheckboxGroupInput(session, "Options",
                             label = paste("Checkboxgroup label", length(b)),
                             choices = b,
                             selected = c("Probe.Set.ID","probeset_id","Ensembl","gene_assignment","Accession"))
      
  })  
  
  # Introduir gens amb text
  
  output$genes_text <- renderText({ 
    if(input$genes_text=="introduce probeset names")
      return("Number introduced: 0")
    b<-gsub(" ","",input$genes_text)
    b<-strsplit(b,",")[[1]]
    print(paste0("Number of probes introduced: ",length(b)));})
  
  
  # Introduir gens amb file
  
  output$genes_file <- renderText({ 
    inFile <- input$file
    if (is.null(inFile))
      return(print(paste0("Number of probes introduced 0: ")))
    file_in_num_genes<-read.csv(inFile$datapath,header = F)
    print(paste0("Number introduced: ",dim(file_in_num_genes)[1]))
    })
  
  
  
  # Seleccionad Database de affymetrix
  
  db_selected_1<-reactive({
    show_modal_spinner(spin = "cube-grid",
                       color = "firebrick",
                       text = "Loading Affymetrix info")
    
    db<-(read.csv(input$dataset,colClasses='character',comment.char='#',header = T))
    
    remove_modal_spinner()
    return(db)
    
    
    
  })
  
  output$db_selected <- renderText({ 
    
   db<-db_selected_1()
  
   
    print(paste0("Number of probes in db: ",dim(db)[1]));
    
    
    })
  
 
  ## Generar taula amb la info
  output$info<- renderDataTable({
    # annotation_db<-db<-read.csv('./HG-U133_Plus_2.na36.annot.csv',colClasses='character',comment.char='#')
    
    
    ## condicional file genes
    if(!is.null(input$file)){
      inFile <- input$file
      inFile<-db_selected_1()
      genes<-as.list(inFile)[[1]]}
    ## condicional file text
    else if(input$genes_text!=""){
      genes<-gsub(" ","",input$genes_text)
      genes<-strsplit(genes,",")[[1]]}
    if(length(genes==0)){
      db<-NULL
    }
      db<-db_selected_1()
      
      db<-db[,input$Options]
    
    
    
    
    if(input$dataset=="Clariom_D_Human.r1.na36.hg38.a1.transcript.csv"){
    db<- db%>% filter(probeset_id%in%genes)}else{db<- db%>% filter(Probe.Set.ID%in%genes)}
    
    data.frame(db)
    }
    
   )
  

    observeEvent(input$update, {
    newtab <- switch(input$tabs, "Use" = "Results")
    updateTabItems(session, "tabs", newtab)
  
    })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset,".csv", sep = "")
    },
    content = function(file) {
      write.csv(db, file, row.names = FALSE)
    },
    contentType = ".csv"
  )
  
  output$results <-DT::renderDataTable({
    if(input$sumbit!=0){    
      show_modal_spinner()
      if(is.null(input$miRNA_file)){
        data_mirna<-input$miRNA_text
      # example1<-get_multimir(mirna = input$miRNA_text, summary = TRUE,table = input$options)
      example1 <- get_multimir(org     = 'hsa',
                               target  = data_mirna,
                               table = input$options,
                               summary = TRUE,
                               predicted.cutoff.type = 'n')
      remove_modal_spinner()
      datatable(example1@data)}else{
        
        
        
        data_mirna<- reactive({
          
          infile <- input$miRNA_file
          if (is.null(infile)) {
            # User has not uploaded a file yet
            return(NULL)
          }
          read.csv(infile$datapath,header = F)
          

        })
        
       
        
        example1 <- get_multimir(org     = 'hsa',
                                 target  = data_mirna()[,1],
                                 table = input$options,
                                 summary = TRUE
                                 )
        remove_modal_spinner()
        datatable(example1@data)
      }
      
      }
    
  })
}

server

