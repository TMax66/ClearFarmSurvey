

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
            fluidRow(
          column(3,
          valueBoxOutput("Quest", width = NULL)),
          column(3,
          valueBoxOutput("hsize", width = NULL)),
          column(3,
          valueBoxOutput("dry", width = NULL)),
          column(3,
          valueBoxOutput("heif", width = NULL))
        )
        ), 
        fluidRow(
          
              fluidRow(
                column(3,
                       valueBoxOutput("gas", width = NULL)),
                column(3,
                       valueBoxOutput("senso", width = NULL)),
                column(3,
                       valueBoxOutput("inter", width = NULL)),
                column(3,
                       valueBoxOutput("noninter", width = NULL))
              )
        ),
        fluidRow(
          column(6, 
          box(solidHeader = TRUE, title="", width= 12,
              plotOutput("gr1"))),
          column(6, 
                 box(solidHeader = TRUE, title="", width= 12,
                     plotOutput("gr2"))   
                 )
          )

          ), 
      tabItem(
        tabName = "risp2",
         fluidRow(
           column(6,
           plotOutput("psamb")),
           column(6,
           plotOutput("psanim"))), 
      
        hr(),
        fluidRow(
          
          column(6, 
                 valueBoxOutput("robott", width = NULL),
                 plotOutput("pinforobo")),
                 
          column(6, 
                 valueBoxOutput("norobot", width = NULL),
                 plotOutput("pnorob"))
        ),
        hr(),
        fluidRow(
          
          column(6, 
                 plotOutput("panpar")),
          column(6, 
                 plotOutput("pamb")
        )), 
        
        fluidRow(
          
          column(6, 
                 plotOutput("pambpr"))
              )
              ), 
      tabItem(tabName = "risp3",
              
              fluidRow(
                column(6, 
                       plotOutput("ptuso")),
                column(6, 
                       plotOutput("likert")) 
              ), 
              br(), 
              hr(), 
              
              fluidRow(
                column(6, 
                valueBoxOutput("easy", width = NULL),      
                plotOutput("pmiglior")), 
                column(6, 
                       plotOutput("pinnov"))    
              )
            
              
              
              
              )
        )
      )
                )

      
      
      

 
 