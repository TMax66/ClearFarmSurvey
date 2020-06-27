

ui <- dashboardPage(
  dashboardHeader(title = "Clear Farm Survey"),
  dashboardSidebar(
    sidebarMenu(
      
      
      menuItem("Introduzione", tabName = "intro", icon = icon("th")),
      hr(),
      p("Visualizza i risultati"),
      menuItem("Quadro generale", tabName = "risp1", icon = icon("dashboard")),
      menuItem("Sensori amb o anim", tabName = "risp2", icon = icon("dashboard")),
      menuItem("Sensori amb e anim", tabName = "risp3", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems(
      ####INTRODUZIONE####
      tabItem(
        tabName="intro", 
        fluidRow(
          box(width = 12,
          includeHTML("intro.html")
          )
        )
      ),
      tabItem(
        ####SEZIONE 1####
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
           plotOutput("preg")),
          column(6, 
           plotOutput("pruol"))   
                 ),
        hr(),
        fluidRow(
          column(6,
                 plotOutput("ptipol")),
          column(6, 
                 plotOutput("psensor")) 
                ), 
        hr(),
        fluidRow(
          column(6,
                 plotOutput("pyesint")),
          column(6, 
                 plotOutput("pnonint")) 
                )
          ), 
      tabItem(
        ####SEZIONE 2####
        tabName = "risp2",
        fluidPage(  
           tabBox(title="",width = 12, 
                  ######sensori ambientali#####
              tabPanel( "Utilizzo esclusivo di sensori ambientali" , 
                        fluidPage( 
                 fluidRow(
                  box( width = 6, 
                   plotOutput("psamb")),
                  box(width = 6, 
                   plotOutput("psamb2"))), 
                fluidRow(
                  box(
                   plotOutput("psamb3")))
                        )), 
                   #####sensori animali####
              tabPanel(" Utilizzo esclusivo di sensori su animali",
                       fluidRow(
                         box(width=6, 
                          plotOutput("psanim")),
                         box(width = 6, 
                          plotOutput("psanim2"))),
                       fluidRow(
                         box(
                           plotOutput("psanim3")
                         )
                       )
                       ) 

              ))), 
      tabItem(
        tabName = "risp3", 
           fluidPage(
             tabBox(title="",width = 12, 
                    ####sensori ambientali####
            tabPanel("sensori ambientali",
              fluidRow(
                box(
                  plotOutput("pBsamb")), 
                box(
                  plotOutput("pBsamb2"))
                ),
              fluidRow(
                box(
                  plotOutput("pBsamb3")
                )
              )
                  
                )
              )
            ), 
                 ####sensori animali###
            tabPanel("sensori animali", 
                fluidRow(
                  box(
                    plotOutput("pBsanim")
                  )
                  
                )
            )
               
             )
           )
      )
      
      
      
      
      
      
      
      
      )
                
                 #valueBoxOutput("robott", width = NULL),
                  
                 
          # column(6, 
          #       # valueBoxOutput("norobot", width = NULL),
          #        plotOutput("pnorob"))
    
        # hr(),
        # fluidRow(
        #   
        #   column(6, 
        #          plotOutput("panpar")),
        #   column(6, 
        #          plotOutput("pamb")
    
       
      #  ), 
      #   hr(),
      #   fluidRow(
      #     
      #     column(6, 
      #            plotOutput("pambpr"))
      #         )
      #         ), 
      # tabItem(tabName = "risp3",
      #         
      #         fluidRow(
      #           column(6, 
      #                  plotOutput("ptuso")),
      #           column(6, 
      #                  plotOutput("likert")) 
      #         ), 
      #         br(), 
      #         hr(), 
      #         
      #         fluidRow(
      #           column(6, 
      #           valueBoxOutput("easy", width = NULL),      
      #           plotOutput("pmiglior")), 
      #           column(6, 
      #                  plotOutput("pinnov"))    
      #         )
      #       
      #         
      #         
      #         
      #         )
      #   )
      # )
                  

      
      
      

 
 