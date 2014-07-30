## server.R
require(rCharts)
library(IT2)
library(plyr) 
library(devtools)

shinyServer(function(input, output) {
 
  
  output$treePic <- renderImage({
    # When input$n is 3, filename is ./images/image3.jpeg
    filename <- "C:/Users/gelfondjal/Google Drive/DEB/Tree_Controller/images/plantingTree.jpg"
      
    filename <- gsub("\\\\","/",filename)
      
    # Return a list containing the filename and alt text
    list(src = filename,
         alt = paste("Logo"))
    
  }, deleteFile = FALSE)
  
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
          subgroup <- data.frame(Package=input$library.name,repos=input$library.install,specific=as.logical(input$library.specific))
        
          load.install.library.file(library.file,subgroup)
        
          print(read.csv(library.file))
        
          subgroup
        }# if real library 
      })
    }
    
    
    })


output$makeProgramPic <- renderImage({
    # When input$n is 3, filename is ./images/image3.jpeg
    filename <- "C:/Users/gelfondjal/Google Drive/DEB/Tree_Controller/images/makeProgram.jpg"
    
    filename <- gsub("\\\\","/",filename)
    
    # Return a list containing the filename and alt text
    list(src = filename,
         alt = paste("Logo"))
    
  }, deleteFile = FALSE)
  
  
  
  output$projectReport <- renderImage({
    # When input$n is 3, filename is ./images/image3.jpeg
    filename <- "C:/Users/gelfondjal/Google Drive/DEB/Tree_Controller/images/projectReport.jpg"
    
    filename <- gsub("\\\\","/",filename)
    
    # Return a list containing the filename and alt text
    list(src = filename,
         alt = paste("Logo"))
    
  }, deleteFile = FALSE)
  
  
  output$syncTree <- renderImage({
    # When input$n is 3, filename is ./images/image3.jpeg
    filename <- "C:/Users/gelfondjal/Google Drive/DEB/Tree_Controller/images/syncTree.jpg"
    
    filename <- gsub("\\\\","/",filename)
    
    # Return a list containing the filename and alt text
    list(src = filename,
         alt = paste("Logo"))
    
  }, deleteFile = FALSE) 
  
  output$sendBranch <- renderImage({
    # When input$n is 3, filename is ./images/image3.jpeg
    filename <- "C:/Users/gelfondjal/Google Drive/DEB/Tree_Controller/images/sendBranch.jpg"
    
    filename <- gsub("\\\\","/",filename)
    
    # Return a list containing the filename and alt text
    list(src = filename,
         alt = paste("Logo"))
    
  }, deleteFile = FALSE) 
  
  
  
  output$graftBranch <- renderImage({
    # When input$n is 3, filename is ./images/image3.jpeg
    filename <- "C:/Users/gelfondjal/Google Drive/DEB/Tree_Controller/images/graftBranch.jpg"
    
    filename <- gsub("\\\\","/",filename)
    
    # Return a list containing the filename and alt text
    list(src = filename,
         alt = paste("Logo"))
    
  }, deleteFile = FALSE)   
  
  
  output$createproject <- renderText({
  
            textout <- "Waiting to create project"
            if(input$submitProject!=0){
              isolate({
               textout <- ifelse( plant.tree(input$project.id.make, input$project.directory, input$swap.directory),paste("Made project",input$project.id.make),"Failed")
              })
            }
            
          all.orchards <<- read.csv(file.path(path.expand.2("~"), "ProjectPaths", "projectid_2_directory.csv"), as.is = TRUE)
          textout  
          })
          
 
 output$text1 <- renderText({
 
          if(input$submit!=0){
              isolate({
                ifelse(sprout.program(input$project.id,source.file.name=input$filename,input$description),paste(input$filename,"Created"),"Failed")
              })
            }
          })
 
 output$projectus <- renderText({ 
 

                                 if(input$submitReport!=0){                                             
                                   isolate({  
                                   source_info <- pull_source_info(input$project.id.report)
                                   project_reporter(source_info) 
                              
                                   browseURL(paste0("file://",(file.path(source_info$results.dir,"project_summary.html"))))
                                     
                                    paste("Creating report",input$project.id.report)   
                                   })
                                 }
                               })       

 
 
 output$syncer <- renderPlot({ 
   
   if(input$submitSyncRun!=0){                                             
     #isolate({  
       source_info <- pull_source_info(input$project.id.sync)
       
       filename <- file.path(source_info$project.path,project.directory.tree$results,"tree_controller.R","sync_updater.png")
       
       htmlfile <- file.path(source_info$project.path,project.directory.tree$results,"tree_controller.R","syncrhonize.html")
       
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
     source_info <- pull_source_info(input$project.id.sync)
     
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
 
 
 
 
 output$syncTest <- renderText({ 
   
   
   if(input$submitSyncTest!=0){                                             
     isolate({  
       source_info <- pull_source_info(input$project.id.sync)
       
       test.sync0 <- sync.test.si(source_info)
       
       print(ifelse(identical(test.sync0$synchronize,TRUE),"Syncrhonized","Not Synchronized"))
       
     })
   }
   
   
   
 })   
 
 
 
 
output$Programs <- renderTable({
  source_info <- pull_source_info(input$project.id.send)
  
  pi <- get.project.info.si(source_info)
  temp <- subset(pi$all.files,select=c("file","description"),file.class=="source")
  rownames(temp) <- 1:nrow(temp)
  temp <- rename(temp,replace=c("file"="File","description"="Description"))
  temp
})


output$Branches <- renderTable({
  source_info <- pull_source_info(input$project.id.graft)
  
  branch.directory <- file.path(get.project.swap.directory(source_info$project.id),"Branches")
  
  branches <- sort(list.files(branch.directory))
  
  data.frame(Branches=branches)
  
})





output$Sent <- renderText({ 
  
  
  if(input$submitSend!=0){                                             
    isolate({  
      source_info <- pull_source_info(input$project.id.send)
      
      send.branch.si(source_info,input$filename.send,FALSE)
      
      paste("Sent",input$filename.send,Sys.time())
      
    })
  }
  
  
  
})   





output$Grafted<- renderText({ 
  
  
  if(input$submitGraft!=0){                                             
    isolate({  
      source_info <- pull_source_info(input$project.id.graft)

      graft.branch(input$graft.branch_name,run=as.logical(input$graft.run),start.up=TRUE,input$project.id.graft,as.logical(input$graft.overwriteTF))
      
      paste("Grafted",input$filename.send,Sys.time())
      
    })
  }
  
  
  
})   









output$syncPlot <- renderImage({
  
  
  
  source_info <- pull_source_info(input$project.id.sync)
  
  
  filename <- file.path(source_info$project.path,project.directory.tree$results,"tree_controller.R","sync_updater.png")
  
  filename <- gsub("\\\\","/",filename)
  
  
  
  # Return a list containing the filename and alt text
  
  
  list(src = filename,
       alt = paste("Waiting to sync"))  
  
  
}, deleteFile = FALSE)





 output$myChart <- renderTable({

 
    temp <- subset(all.orchards,project.id==input$project.id,select="project.path")
    
    names(temp) <- "Project Filepath"
    
    rownames(temp) <- "Location:"
    
    temp
    
    })
 
})
