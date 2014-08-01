## ui.R

library(IT2)
library(stringr)

shinyUI(fluidPage(pageWithSidebar(

  headerPanel('Stat Navigator'),
                      
  sidebarPanel(
         
    conditionalPanel(condition="input.conditionedPanels == 'Select Project'",
                     
                     helpText(h3("Create a New Project")),
                     textInput('project.id.make', "Project ID:", value = "Example project"),
                     textInput('project.directory', "Project directory:", 
                               value= str_replace(all.orchards$project.path[1],"(^.*)(\\/.*$)","\\1")),
                     br(),
                     textInput("swap.directory", label = "Swap directory:",
                               value=str_replace(all.orchards$swap.directory[1],"(^.*)(\\/.*$)","\\1")),
                     br(),br(),
                     actionButton("submitProject","Create project"),  
                     br(),br(),
                     helpText(h3("Choose an Existing Project")),
                     htmlOutput("selectUI"),
                     br(),
                     img(src="treePic.jpg",height=360,width=280)
                     ),
                                                                                                                 
    conditionalPanel(condition="input.conditionedPanels == 'Programs & Libraries'", 
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
                     selectInput('library.specific', label="Library for specific program?", c("FALSE","TRUE"),"FALSE"),                                                                            
                     br(),br(),
                     actionButton("submitLibrary","Add Library"),
                     br(),br()
                     ),
                                                                                                                                                                                                                                                                                  
    conditionalPanel(condition="input.conditionedPanels == 'Report'", 
                     actionButton("submitReport","Create report"),
                     br(),br(),
                     img(src="projectReport.jpg",height=340,width=240)
                     ),
    
    conditionalPanel(condition="input.conditionedPanels == 'Synchronize'", 
                     actionButton("submitSyncTest","Check Sync"),
                     br(),br(),
                     actionButton("submitSyncRun","Synchronize Now"),
                     br(),br(),
                     textInput('commit.message', "Commit Message", value = "Message"),
                     br(),br(),
                     actionButton("submitCommitRun","Commit Project"),
                     br(),br(),
                     img(src="syncTree.jpg",height=320,width=260)
                     ),
 
    conditionalPanel(condition="input.conditionedPanels == 'Send'",
                     textInput('filename.send', "Filename:", value = "MyProgram.R"),
                     br(),br(),
                     actionButton("submitSend","Send"),
                     br(),br(),
                     img(src="sendBranch.jpg",height=275,width=450)
                     ),
    
    conditionalPanel(condition="input.conditionedPanels == 'Graft'",
                     textInput('graft.branch_name', "Branch to get:", value = "GetBranch"),
                     br(),br(),
                     selectInput('graft.run', "Run Branch", c("FALSE","TRUE"),"FALSE"),
                     br(),br(),
                     selectInput('graft.overwriteTF', "Overwrite existing programs", c("FALSE","TRUE"),"FALSE"),
                     br(),br(),
                     actionButton("submitGraft","Graft"),
                     br(),br(),
                     img(src="graftBranch.jpg",height=300,width=360)
                     ),
    
    conditionalPanel(condition="input.conditionedPanels == 'Configure'", helpText("See if Git is working."),
                     actionButton("submitGitCheck","Check Git"),
                     br(),br(),
                     helpText("Install the latest version of IT2"),
                     actionButton("installit2","Install"),
                     br(),br(),
                     img(src="configure.jpg",height=330, width=300)
                     )
    ),
  
  
  mainPanel(
        
    tabsetPanel(type = "tabs",
      tabPanel("Select Project",br(),br(),tableOutput("createproject")),
      tabPanel("Programs & Libraries",br(),br(),tableOutput("myChart"), tableOutput('text1'),
               tableOutput('libraryTable'),tableOutput('addLibraryTable')),
      tabPanel("Report",br(),br(), tableOutput("projectus")),
      tabPanel("Synchronize",br(),br(), imageOutput("syncPlot"),tableOutput("syncTest"),br(),br(), 
               plotOutput("syncer"),br(),br(),textOutput("CommitOut")),
      tabPanel("Send",br(),br(), tableOutput("Programs"),br(),br(), textOutput("Sent")),
      tabPanel("Graft",br(),br(), tableOutput("Branches"),br(),br(), textOutput("Grafted")),
      tabPanel("Configure",br(),br(),uiOutput("Git"),br(),br(),textOutput("IT2")),
      id="conditionedPanels" 
    )
  )

)))
