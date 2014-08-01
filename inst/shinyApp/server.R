## server.R
require(rCharts)
library(IT2)
library(plyr) 
library(devtools)


shinyServer(function(input, output) {
    
  ##########################################################################################
  ### CREATE PROJECT TAB

  
  output$createproject <- renderText({
    textout <- "Waiting to create project"
    if(input$submitProject!=0){
      isolate({
        no.spaces<-make.names(input$project.id.make)
        no.spaces2<-gsub("\\.","_",no.spaces)
        textout <- ifelse( plant.tree(no.spaces2, input$project.directory, input$swap.directory),
                           paste("Made project",no.spaces2),"Failed")
      })
    }
    
    all.orchards <<- read.csv(file.path(path.expand.2("~"), "ProjectPaths", "projectid_2_directory.csv"), as.is = TRUE)
    textout  
  })
  
  output$selectUI<-renderUI({
    input$submitProject
    selectInput(inputId = "project.id", label="", choices=get_orchard()$project.id, 
                selected = get_orchard()$project.id[1])
  })
  
  
  ##########################################################################################
  ### PROGRAMS AND LIBRARIES TAB

  
  output$myChart <- renderTable({
    temp <- subset(all.orchards,project.id==input$project.id,select="project.path")
    names(temp) <- "Project Filepath"
    rownames(temp) <- "Location:"
    temp
  })
  
  output$text1 <- renderText({
    if(input$submit!=0){
      isolate({
        no.spaces<-make.names(input$filename)
        no.spaces2<-gsub("\\.","_",no.spaces)
        program.name<-gsub("(.*)_(.*)","\\1.\\2",no.spaces2)
        if(sprout.program(input$project.id,source.file.name=program.name,input$description)){
          temp <- subset(all.orchards,project.id==input$project.id,select="project.path")
          clean_source(file.path(temp,project.directory.tree$analysis,program.name),quiet=TRUE) 
          #run program in a new r session after you make it
          paste(program.name,"Created")
        }
        else {"Failed"}
      })
    }
  })
  
  output$libraryTable <- renderTable({
    if(input$submitLibrary!=0){ 
      source_info <- pull_source_info(input$project.id)    
      library.file <- file.path(source_info$project.path,project.directory.tree$support,"common_libs.csv")
      library.table <- read.csv(library.file,as.is=FALSE)
      library.table <- library.table[order(library.table$Package),]
      if(is.null(library.table$specific)){library.table$specific <- FALSE}
      library.table$specific <- as.logical(library.table$specific )
      #    print(library.file)
      #   load.install.library.file(library.data.file)
      library.table }
  })
  
  
  output$addLibraryTable <- renderTable({
    source_info <- pull_source_info(input$project.id)    
    library.file <- file.path(source_info$project.path,project.directory.tree$support,"common_libs.csv")
    if(input$submitLibrary!=0){                                             
      isolate({  
        if(!(input$library.name %in% c("","mylibrary"))){
          subgroup <- data.frame(Package=input$library.name,repos=input$library.install,
                                 specific=as.logical(input$library.specific))  
          load.install.library.file(library.file,subgroup)
          print(read.csv(library.file))
          subgroup
        }# if real library 
      })
    }   
  })
  

  ##########################################################################################
  ### REPORT TAB

  
  output$projectus <- renderText({ 
    if(input$submitReport!=0){                                             
      isolate({  
        source_info <- pull_source_info(input$project.id)
        project_reporter(source_info) 
        browseURL(paste0("file://",(file.path(source_info$results.dir,"project_summary.html"))))
        paste("Creating report",input$project.id)   
      })
    }
  })       
  
  
  ##########################################################################################
  ### SYNCHRONIZE TAB

  
  output$syncPlot <- renderImage({
    source_info <- pull_source_info(input$project.id)
    filename <- file.path(source_info$project.path,project.directory.tree$results,
                          "tree_controller.R","sync_updater.png")
    filename <- gsub("\\\\","/",filename)
    # Return a list containing the filename and alt text
    list(src = filename,
         alt = paste("Waiting to sync"))  
  }, deleteFile = FALSE)
  
  output$syncTest <- renderText({ 
    if(input$submitSyncTest!=0){                                             
      isolate({  
        source_info <- pull_source_info(input$project.id)
        test.sync0 <- sync.test.si(source_info)
        print(ifelse(identical(test.sync0$synchronize,TRUE),"Syncrhonized","Not Synchronized"))
      })
    } 
  }) 
  
  output$syncer <- renderPlot({ 
    if(input$submitSyncRun!=0){                                             
      #isolate({  
      source_info <- pull_source_info(input$project.id)
      filename <- file.path(source_info$project.path,project.directory.tree$results,
                            "tree_controller.R","sync_updater.png")
      htmlfile <- file.path(source_info$project.path,project.directory.tree$results,
                            "tree_controller.R","syncrhonize.html")
      html.out <- paste("<img src=sync_updater.png>")
      write(html.out,htmlfile)
      browseURL(paste0("file://",htmlfile))
      test.sync <- source.sync.si(source_info,run=TRUE,TRUE)
      #  test.sync <- sync.test.si(source_info)
      #   print(test.sync)
      # print("Hello")
      # })
    }
  })
  
  output$CommitOut <- renderText({ 
    if(input$submitCommitRun!=0){                                             
      isolate({  
        source_info <- pull_source_info(input$project.id)
        #test.sync <- source.sync.si(source_info,run=TRUE)
        setwd(source_info$project.path)
        analysis.dir <- file.path(source_info$project.path,project.directory.tree$analysis)
        all.programs <- matrix(list.files(analysis.dir,recursive=TRUE,full.names=TRUE))
        add <-  apply(all.programs,1,function(x){git.add(source_info$project.path,filename=x)})
        commited <- git.commit(analysis.dir,input$commit.message)
        #  test.sync <- sync.test.si(source_info)
        #   print(test.sync)
        print(paste("Git","\n",add,"\n",commited))
      })
    }
  })
  

  ##########################################################################################
  ### SEND TAB

  
  output$Programs <- renderTable({
    source_info <- pull_source_info(input$project.id)
    pi <- get.project.info.si(source_info)
    temp <- subset(pi$all.files,select=c("file","description"),file.class=="source")
    rownames(temp) <- 1:nrow(temp)
    temp <- rename(temp,replace=c("file"="File","description"="Description"))
    temp
  })
  
  output$Sent <- renderText({ 
    if(input$submitSend!=0){                                             
      isolate({  
        source_info <- pull_source_info(input$project.id)
        send.branch.si(source_info,input$filename.send,FALSE)
        paste("Sent",input$filename.send,Sys.time()) 
      })
    } 
  })   
  
  
  ##########################################################################################
  ### GRAFT TAB

  
  output$Branches <- renderTable({
    input$submitSend
    source_info <- pull_source_info(input$project.id)
    branch.directory <- file.path(get.project.swap.directory(source_info$project.id),"Branches")
    branches <- sort(list.files(branch.directory))
    data.frame(Branches=branches)
  })
  
  output$Grafted<- renderText({ 
    if(input$submitGraft!=0){                                             
      isolate({  
        source_info <- pull_source_info(input$project.id)
        graft.branch(input$graft.branch_name,run=as.logical(input$graft.run),start.up=TRUE,
                     input$project.id,as.logical(input$graft.overwriteTF))
        paste("Grafted",input$graft.branch_name,Sys.time()) 
      })
    }
  }) 
  
  
  ##########################################################################################
  ###CONFIGURE TAB
  
  
  output$Git<-renderUI({ 
    textout <- "Waiting to check Git"
    if(input$submitGitCheck!=0){
      temp <- git.configure.test()
      if(temp==0){
        textout<-"Git is configured."
      }
      else{
        textout<-a("Git is not configured. Click here to get Git",href="http://git-scm.com/downloads")
      }
    }
    textout  
  })
  
  output$IT2<-renderText({
    textout<-"Waiting to install latest IT2"
    if (input$installit2!=0){
      install_github("IT2", "gelfondjal")
      library(IT2)
      textout<-"IT2 has been installed."
    }
    textout
  })
  
  
  ##########################################################################################
 
})
