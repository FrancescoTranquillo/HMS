#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

## app.R ##

library(shiny)
library(shinydashboard)
library (DT)
library (ggplot2)
library (plotly)
library(reshape2)
library(data.table)

###### TITLE ######
header<- dashboardHeader(
  title=tags$b("SERENA")
)





###### SIDEBAR #####
Sidebar<-dashboardSidebar(
  sidebarMenu(

    menuItem(tags$strong("Welcome"), tabName="Welcome page", icon=icon("home"), selected= TRUE),
    menuItem(tags$b("Employee Section"), tabName= "Employee", icon=icon("user")),
    menuItem(tags$b("Doctor Section"), tabName= "SP", icon = icon("user-md")),
    menuItem(tags$b("User guide"), tabName="guide", icon=icon("info-circle")),
    menuItem(tags$b("Source code"), icon = icon("file-code-o"), href = "https://github.com/FrancescoTranquillo/HMS-Serena"),
    tags$img(src="logo2.png", height="auto",width="auto")
  )
)



########## BODY ##########
body<-dashboardBody(

  tabItems(

      ###### MEDIC ##########
    tabItem(tabName = "SP",
            h2(
              icon("user-md"),
              tags$b("Monitor your patients")
            ),


            #####FIRST ROW#####
            fluidRow(

              box( title = strong("Upload your file here"),
                   solidHeader = TRUE, status = "success", background = "green",


                   fileInput("file1", label = NULL, accept = c("text/csv","text/comma-separated-values,text/plain",".csv"))



              ),
              box( title = strong("Help me"),
                   solidHeader = TRUE, status = "warning", collapsible = TRUE, collapsed = TRUE,

                   "Serena accepts only files with a", strong(".csv"),"extension. To upload a file, click on the Browse button on the left and select your file from your computer.",
                   br(),
                   br(),
                   "Too slow?",
                   br(),
                   "Serena supports Drag&Drop feature! Drag your file and drop it in the Browse button!",
                   br(),
                   br(),
                   "When the file will be uploaded, you can expand the next boxto visualize and interact with your data"
              )
            ),



            ######SECOND ROW#######
            fluidRow(

              # box(
              #   status="success", collapsible=TRUE,  collapsed=FALSE, solidHeader = TRUE,title=tags$b("Overview"), width = "85%",
                
                  tabBox(title = "Overview", width = "85%",
                    tabItem(
                      tabName = "tab1", title= "Trends",
                      selectInput('ycol', 'Choose the desired parameter:', "", selected =""),
                      plotlyOutput("plot1")
                    ),
                    tabItem(
                      tabName = "tab2", title= "Table",
                      DTOutput("dati")
                    )
                  )
                
                
               # )
              ),
            fluidRow(
              box(
                title="Employee-Specific",
                
                box( title="Set thresholds",
                     selectInput('EMP2', 'Select an employee:', "", selected = "abc"),
                     selectInput('Parameter2', "Select a specific parameter:", "", selected="")
                ),
                box(
                  sliderInput("slider2", label = h3("Slider Range"), min = 0, max = 200, value = c(40, 60))
                ),
                box(
                  plotlyOutput("plot4")
                )
              )
            )
    ),

     ##### EMPLOYEE  HR#######
    tabItem(tabName = "Employee",
            h2(
              icon("user"),
              tags$b("Your parameters")
               ),

            ######FIRST ROW######
            fluidRow(

              box( title = strong("Upload your file here"),
                   solidHeader = TRUE, status = "info", background = "aqua",


                   fileInput("file2", label = NULL, accept = c("text/csv","text/comma-separated-values,text/plain",".csv"))



              ),
              box( title = strong("Help me"),
                   solidHeader = TRUE, status = "warning", collapsible = TRUE, collapsed = TRUE,

                   "Serena accepts only files with a", strong(".csv"),"extension. To upload a file, click on the Browse button on the left and select your file from your computer.",
                   br(),
                   br(),
                   "Too slow?",
                   br(),
                   "Serena supports Drag&Drop feature! Drag your file and drop it in the Browse button!",
                   br(),
                   br(),
                   "When the file will be uploaded, you can expand the next box to visualize and interact with your data"
              )
            ),

            ####SECOND ROW######


            box(plotlyOutput("plot2"), collapsible=TRUE, title=tags$b("Generic"), solidHeader = TRUE, status="info", width = "100%"),
            fluidRow(
              box(
                selectInput('Parameter', "Select a specific parameter:", "", selected=""),
                htmlOutput("stats")
              ),
              box(
                  plotlyOutput("plot3"),collapsible=TRUE, title=tags$b("Parameter-specific "), solidHeader = TRUE, status = "info"
                )
              )
            )

  )
)



###### UI #####
ui <- dashboardPage(
  header,
  Sidebar,
  body
)

####### SERVER #####
server <- shinyServer(function(input, output, session) {
  # added "session" because updateSelectInput requires it



  ####DOC FILE for plot M1####
  data1 <- reactive({

    req(input$file1) ## ?req #  require that the input is available

    inFile <- input$file1


    df <- read.csv(inFile$datapath, header = TRUE, sep = ";")
    
    df$Dates<- as.Date(df$Dates, format="%d/%m/%Y")

    updateSelectInput(session, inputId = 'ycol', label = 'Choose the desired parameter:',
                      choices = names(df[,-2]), selected = names(df[,-2]))



    #!!!!!

    return(df)
  })


  ####DOC FILE for plot M2####


  data4 <- reactive({


      req(input$file1) ## ?req #  require that the input is available

      inFile <- input$file1

      df <- read.csv(inFile$datapath, header = TRUE, sep = ";")

      df<- mutate(df, Dates=as.Date(df$Date, "%d/%m/%Y"))

      df[2]<-NULL
      #extraction of employee



      updateSelectInput(session, inputId = "Parameter2", label="Select a parameter:",
                        choices=names(df[,-2]),selected=names(df[,-2]))
      attach(df)

      updateSelectInput(session, inputId="EMP2", label="Select Employee:",
                        choices=EMP_FiscalCode, selected=EMP_FiscalCode)

      detach(df)

      library(data.table)
      mydata<-as.data.frame(df)
      return(mydata)

      })



  #######EMP FILE for plot E1#######
  data2 <- reactive({

    req(input$file2)
    inFile2 <- input$file2

    df2<-read.csv(inFile2$datapath, sep = ";", header= TRUE)


    df2$Date<-as.Date(df2$Date, format="%d/%m/%Y")


    dfmelted<-melt(df2, id.vars = c('Date'))

    names(dfmelted)[2]<-'Parameter'
    names(dfmelted)[3]<-'Value'
    dfmelted<-na.omit(dfmelted)
    #!!!!!!!
    return(dfmelted)


  })
  #####EMP FILE for plot E2#####
  data3<- reactive({

    req(input$file2)
    inFile2<-input$file2
    df2<-read.csv(inFile2$datapath, sep = ";", header= TRUE)


    df2$Date<-as.Date(df2$Date, format="%d/%m/%Y")



    updateSelectInput(session, inputId = "Parameter", label="Select a parameter:",
                      choices=names(df2[,-1]),selected=names(df2[,-1]))

    df2<-na.omit(df2)
    return(df2)
  })

  output$stats= renderText({



    paste("<b>Mean = ", mean(data3()[[input$Parameter]], na.rm=TRUE),
          "<br>",
          "<b>SD = ", signif(sd(data3()[[input$Parameter]],na.rm=TRUE), 3),
          "<br>",
          "<b>Max = ", max(data3()[[input$Parameter]],na.rm=TRUE),
          "<br>",
          "<b>Min = ", min(data3()[[input$Parameter]],na.rm=TRUE)

    )
  })
  ##### TABLE RENDER ####
  output$dati = renderDT(data1())

  #####PLOT M1####
  output$plot1 = renderPlotly({


    # data<-data1()
    # 
    # Time<-as.POSIXct(strptime(data$Dates,"%d/%m/%Y"))

    # selectedInput<-data[[input$ycol]]
    Employee<-data1()$EMP_FiscalCode



    #plotly formatting
    ggplotly(ggplot(data1(), aes_string( x="Dates" , y=input$ycol, color="Employee"  ))
             +geom_line(size="0.7")
             +geom_point(size="2")
             +xlab("Time")
             +ylab(input$ycol)

    )
  })


  #####PLOT E1####
  output$plot2 = renderPlotly({

    ggplotly(ggplot(data2(), aes(x=Date, y=Value, color=Parameter))

             +geom_line()
             # +geom_hline(stat="identity", aes(yintercept=50), show.legend = TRUE)
             # +facet_wrap(~Parameter, ncol=2)

    )


  })
  #####PLOT E2#######
  output$plot3=renderPlotly({


    # thr<-input$Parameter<100& input$Parameter>50
    ggplotly(
      ggplot(data3(),aes_string(x="Date", y=input$Parameter))
      +geom_point()
      +geom_line()

      +theme(legend.position='none')

      )
  })




  #####PLOT M2#####

  selecteddata<-reactive({

    empdata<-filter(data4(), EMP_FiscalCode == input$EMP2)

    #removing FC column
    empdata[2]<-NULL

    return(empdata)

  })
  output$plot4 = renderPlotly({



    ggplotly(ggplot(selecteddata(), aes_string(x="Dates", y=input$Parameter2))
             +geom_point()
             +geom_line()
             +theme(legend.position='none')
             # +geom_hline(stat="identity", aes(yintercept=50), show.legend = TRUE)
             # +facet_wrap(~Parameter, ncol=2)
    )




  })
})

shinyApp(ui, server)
