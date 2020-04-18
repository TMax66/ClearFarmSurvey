

ui <- dashboardPage(
  dashboardHeader(title = "Clear Farm Survey"),
  dashboardSidebar(
    sidebarMenu(
      
      
      menuItem("Introduzione", tabName = "intro", icon = icon("th")),
      hr(),
      p("Visualizza i risultati"),
      menuItem("sezione1", tabName = "risp1", icon = icon("dashboard")),
      menuItem("sezione2", tabName = "risp2", icon = icon("dashboard")),
      menuItem("sezione3", tabName = "risp3", icon = icon("dashboard"))
     # menuItem("Dati", tabName = "dati", icon = icon("th"))
      #menuItem("Text mining Analysis", tabName="tm", icon = icon("edit"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName="intro", 
        fluidRow(
          box(width = 12,
          includeHTML("intro.html")
          )
        )
      ),
      tabItem(
        tabName = "risp1",
        fluidRow(
          box(width=12, solidHeader = TRUE,
            fluidRow(
          column(3,
          valueBoxOutput("Quest", width = NULL)),
          column(3,
          valueBoxOutput("hsize", width = NULL)),
          column(3,
          valueBoxOutput("dry", width = NULL)),
          column(3,
          valueBoxOutput("heif", width = NULL))
        ))
        ), 
        fluidRow(
          column(10, 
          box(solidHeader = TRUE, title="", width= 12,
              plotOutput("gr1"))),
          column(2, 
          valueBoxOutput("gas",width = NULL), 
          valueBoxOutput("senso", width=NULL), 
          valueBoxOutput("inter",width=NULL))
        )

          ), 
      tabItem(
        tabName = "risp2",
         fluidRow(
           column(4,
          valueBoxOutput("morsens", width = NULL),
          plotOutput("quest12")),
          
          column(4, 
                 valueBoxOutput("robott",width = NULL),
                 plotOutput("quest14")), 
          column(4,
                valueBoxOutput("norobot", width = NULL),
                plotOutput("quest16")
                )
         ), 
        br(), 
        hr(),
        fluidRow(
          
          column(4, 
                 plotOutput("quest18")),
                 
          column(4, 
                 plotOutput("quest20")), 
          
          column(4, 
                 plotOutput("quest22"))
        )

              
              ), 
      tabItem(tabName = "risp3",
              
              fluidRow(
                column(4, 
                       plotOutput("quest24")),
                column(4, 
                       plotOutput("likert")), 
                column(4, 
                       plotOutput("quest26"))
                
              ), 
              br(), 
              hr(), 
              
              fluidRow(
                column(5, 
                plotOutput("quest29"))
              )
            
              
              
              
              )
        )
      )
                )

      
      
      

 
 