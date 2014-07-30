library(IT2)

## ui.R
shinyUI(fluidPage(pageWithSidebar(

  headerPanel('Stat Navigator'),
                      
  sidebarPanel(
         
    conditionalPanel(condition="input.conditionedPanels == 'Create Project'",                                                                        
                                                                            textInput('project.id.make', "Project ID:", value = "Example project"),
                                                                            br(),
                                                                            textInput('project.directory', "Project directory:", value = paste0(path.expand.2("~"),"...")),
                                                                            br(),
                                                                            textInput("swap.directory", label = "Swap directory:", value = paste0(path.expand.2("~"),"...")),
                                                                            br(),br(),
                                                                            actionButton("submitProject","Create project"),  
                                                                            br(),br(),
                                                                            img(src="treePic.jpg",height=360,width=280)),
                                                                            
                                                              
    conditionalPanel(condition="input.conditionedPanels == 'Programs & Libraries'", selectInput(inputId = "project.id", "Choose Project", get_orchard()$project.id, selected = get_orchard()$project.id[1]),
                                                                            br(),br(),
                                                                            helpText(h3("Make Program")),
                                                                            textInput('filename', "Filename:", value = "MyProgram.R"),
                                                                            br(),
                                                                            textInput("description", label = "Program description:", value = "Description of your program..."),
                                                                            br(),br(),
                                                                            actionButton("submit","Create program"),
                                                                            br(),br(),
                                                                           img(src="makeProgramPic.jpg",height=240,width=300),
                                                                            br(),
                                                                            helpText(h3("Manage Libraries")),
                                                                            br(),
                                                                            textInput("library.name", label = "Library Name", value = "mylibrary"),
                                                                            textInput("library.install", label = "Install Command (bioC or '' for CRAN)", value = ""),
                                                                            selectInput('library.specific', label="Library for specific program?", c("FALSE","TRUE"),"FALSE"),                                                                            br(),br(),
                                                                            actionButton("submitLibrary","Add Library"),
                                                                            br(),br()),
                                                                        
                                                                                                                                                                                                                                 
    conditionalPanel(condition="input.conditionedPanels == 'Report'", selectInput(inputId = "project.id.report", "Choose Project", get_orchard()$project.id, selected = get_orchard()$project.id[1]),
                                                                              br(),br(),
                                                                              actionButton("submitReport","Create report"),
                                                                              br(),br(),
                                                                              img(src="projectReport.jpg",height=340,width=240)),
    
    conditionalPanel(condition="input.conditionedPanels == 'Synchronize'", selectInput(inputId = "project.id.sync", "Choose Project", get_orchard()$project.id, selected = get_orchard()$project.id[1]),
                     br(),br(),
                     actionButton("submitSyncTest","Check Sync"),
                     br(),br(),
                     actionButton("submitSyncRun","Synchronize Now"),
                     br(),br(),
                     textInput('commit.message', "Commit Message", value = "Message"),
                     br(),br(),
                     actionButton("submitCommitRun","Commit Project"),
                     br(),br(),
                     img(src="syncTree.jpg",height=320,width=260))
                     
    ,
    
    
    conditionalPanel(condition="input.conditionedPanels == 'Send'", selectInput(inputId = "project.id.send", "Choose Project", get_orchard()$project.id, selected = get_orchard()$project.id[1]),
                     br(),br(),
                     textInput('filename.send', "Filename:", value = "MyProgram.R"),
                     br(),br(),
                     actionButton("submitSend","Send"),
                     br(),br(),
                     img(src="sendBranch.jpg",height=275,width=450)
                     ),
    
    conditionalPanel(condition="input.conditionedPanels == 'Graft'", selectInput(inputId = "project.id.graft", "Choose Project", get_orchard()$project.id, selected = get_orchard()$project.id[1]),
                     br(),br(),
                     textInput('graft.branch_name', "Branch to get:", value = "GetBranch"),
                     br(),br(),
                     selectInput('graft.run', "Run Branch", c("FALSE","TRUE"),"FALSE"),
                     br(),br(),
                     selectInput('graft.overwriteTF', "Overwrite existing programs", c("FALSE","TRUE"),"FALSE"),
                     br(),br(),
                     actionButton("submitGraft","Graft"),
                     br(),br(),
                     img(src="graftBranch.jpg",height=300,width=360)
                     )
    
    
  ),
  
  mainPanel(
        
    tabsetPanel(type = "tabs",
      tabPanel("Create Project",br(),br(),tableOutput("createproject")),
      tabPanel("Programs & Libraries",br(),br(),tableOutput("myChart"), tableOutput('text1'),tableOutput('libraryTable'),tableOutput('addLibraryTable')),
      tabPanel("Report",br(),br(), tableOutput("projectus")),
      tabPanel("Synchronize",br(),br(), imageOutput("syncPlot"),tableOutput("syncTest"),br(),br(), plotOutput("syncer"),br(),br(),textOutput("CommitOut")),
      tabPanel("Send",br(),br(), tableOutput("Programs"),br(),br(), textOutput("Sent")),
      tabPanel("Graft",br(),br(), tableOutput("Branches"),br(),br(), textOutput("Grafted")),
      
      id="conditionedPanels" 
    )
  )

)))
