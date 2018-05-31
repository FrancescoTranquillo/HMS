
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

## app.R ##

# 1) Libraries ----
library(shiny)
library(shinydashboard)
library (DT)
library (ggplot2)
library (plotly)
library(reshape2)
library(data.table)
library(dplyr)
#library(rmarkdown)


  ## 2) Titles-----
header<- dashboardHeader(
  title="SERENA"
)
###### SIDEBAR #####
Sidebar<-dashboardSidebar(
  sidebarMenu(




    menuItem(tags$strong("Welcome"), tabName="Welcomepage", icon=icon("home"), selected= TRUE),
    menuItem(tags$b("Employee"), tabName= "Employee", icon=icon("user")),
    menuItem(tags$b("Specialized Practitioner"), tabName= "SP", icon = icon("user-md")),
    menuItem(tags$b("Source code"), icon = icon("file-code-o"), href = "https://github.com/FrancescoTranquillo/Serena"),
    tags$img(src="logo2.png", height="auto",width="auto")
  )
)
########## BODY ##########
body<-dashboardBody(

  tabItems(
    ######WELCOME####
    tabItem(tabName="Welcomepage",
            fluidRow(
              column(width=12,
                     tabBox(width=12,
                            title="Welcome to Serena",
                            tabPanel("Introduction",
                                    includeMarkdown("Introduction.md")

                            ),
                            tabPanel("1 Upload your data",
                            includeMarkdown("1)upload.md")

                            ),
                            tabPanel("2 Visualize your data",
                                     includeMarkdown("2)visualize.md")
                            ),
                            tabPanel("3 HEy use MS",
                                     includeMarkdown("README.md"))

                     )
              )

            )

    )
    ,


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

              h2("Employee-Specific"),

              box(title=tags$b("Settings"), status="success", solidHeader = TRUE,
                  selectInput('EMP2', 'Select an employee:', "", selected = "abc"),
                  selectInput('Parameter2', "Select a specific parameter:", "", selected=""),
                  sliderInput("slider1", label = "Inferior threshold", min = 0, max = 200, value = c(50)),
                  sliderInput("slider2", label = "Superior threshold", min = 0, max = 200, value = c(50))
              ),
              box(status="success", solidHeader = TRUE, title=tags$b("Trend"),
                  plotlyOutput("plot4")
              )
            ),
            fluidRow(
              infoBoxOutput("AbnormalsBox"),
              infoBoxOutput("MeanBox"),
              infoBoxOutput("SDBox")
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
              box( solidHeader = TRUE, status = "info", title=tags$b("Settings"),
                   selectInput('Parameter', "Select a specific parameter:", "", selected=""),
                   sliderInput("slider3", label = "Inferior threshold", min = 0, max = 200, value = c(50)),
                   sliderInput("slider4", label = "Superior threshold", min = 0, max = 200, value = c(50))
              ),
              box(
                plotlyOutput("plot3"),collapsible=TRUE, title=tags$b("Parameter-specific "), solidHeader = TRUE, status = "info"
              )
            ),
            fluidRow(
              h2("Results"),
              infoBoxOutput("AbnormalsBoxEMP"),
              infoBoxOutput("MeanBoxEMP"),
              infoBoxOutput("SDBoxEMP")
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



####DOC FILE for plot M1#####
  data1 <- reactive({

    req(input$file1) ## ?req #  require that the input is available

    inFile <- input$file1


    df <- read.csv(inFile$datapath, header = TRUE, sep = ";", dec=",")


    df$Date<- as.Date(df$Date, format="%d/%m/%Y")
    #transmute(Dates = as.Date(Dates, format = '%d/%m/%Y'))

    updateSelectInput(session, inputId = 'ycol', label = 'Choose the desired parameter:',
                      choices=names(df[-c(1,2,3)]),selected=names(df[-c(1,2,3)]))



for(i in 1:ncol(df)){
df[is.na(df[,i]), i] <- mean(df[,i], na.rm = TRUE)
}


    #!!!!!

    return(df)
  })


  ####DOC FILE for plot M2####


  data4 <- reactive({


    req(input$file1) ## ?req #  require that the input is available

    inFile <- input$file1

    df <- read.csv(inFile$datapath, header = TRUE, sep = ";", dec=",")

    df$Date<- as.Date(df$Date, "%d/%m/%Y")

    df[2]<-NULL
    #extraction of employee



    updateSelectInput(session, inputId = "Parameter2", label="Select a parameter:",
                      choices=names(df[-c(1,2)]),selected=names(df[-c(1,2)]))
    attach(df)

    updateSelectInput(session, inputId="EMP2", label="Select Employee:",
                      choices=EMP_FiscalCode, selected=EMP_FiscalCode)

    detach(df)

    library(data.table)
    mydata<-as.data.frame(df)

    for(i in 1:ncol(mydata)){
  mydata[is.na(mydata[,i]), i] <- mean(mydata[,i], na.rm = TRUE)
}
    return(mydata)

  })




  #######EMP FILE for plot E1#######
  data2 <- reactive({

    req(input$file2)
    inFile2 <- input$file2

    df2<-read.csv(inFile2$datapath, sep = ";", header= TRUE, dec=",")

    for(i in 1:ncol(df2)){
    df2[is.na(df2[,i]), i] <- mean(df2[,i], na.rm = TRUE)
    }



    df2$Date<-as.Date(df2$Date, format="%d/%m/%Y")
    #df2[df2==""]<-NA


    dfmelted<-melt(df2, id.vars = c('Date'))

    names(dfmelted)[2]<-'Parameter'
    names(dfmelted)[3]<-'Value'
    #dfmelted[dfmelted==""]<-NA
    dfmelted<-na.omit(dfmelted)

    #!!!!!!!
    return(dfmelted)


  })
  #####EMP FILE for plot E2#####
  data3<- reactive({

    req(input$file2)
    inFile2<-input$file2
    df2<-read.csv(inFile2$datapath, sep = ";", header= TRUE, dec=",")


    df2$Date<-as.Date(df2$Date, format="%d/%m/%Y")



    updateSelectInput(session, inputId = "Parameter", label="Select a parameter:",
                      choices=names(df2[,-1]),selected=names(df2[,-1]))

                      for(i in 1:ncol(df2)){
                    df2[is.na(df2[,i]), i] <- mean(df2[,i], na.rm = TRUE)
                  }
    return(df2)
  })

  ##### INFO BOXES #####
  output$AbnormalsBoxEMP<- renderInfoBox({
    infoBox(
      tags$b("Number of abnormal values"), paste0( countabEMP()),
      icon=icon("exclamation-triangle"), fill= TRUE,
      color="aqua"
    )
  })

  output$MeanBoxEMP<- renderInfoBox({
    infoBox(
      tags$b("Mean"), paste0(signif(mean(data3()[[input$Parameter]], na.rm=TRUE),3)),
      icon=icon("calculator"), fill=TRUE, color="aqua"
    )
  })

  output$SDBoxEMP<- renderInfoBox({
    infoBox(
      tags$b("Standard deviation"), paste0(signif(sd(data3()[[input$Parameter]], na.rm=TRUE),3)),
      icon=icon("align-center"), fill=TRUE, color="aqua"
    )
  })
  abnormalsEMP<-reactive({
    thrEMP<-data3()[input$Parameter]<input$slider3 | data3()[input$Parameter]>input$slider4
    return(thrEMP)
  })
  countabEMP<-reactive({
    numEMP<-length(abnormalsEMP()[abnormalsEMP()==TRUE])
    return(numEMP)
  })

  output$AbnormalsBox<- renderInfoBox({
    infoBox(
      tags$b("Number of abnormal values"), paste0( countab()),
      icon=icon("exclamation-triangle"), fill= TRUE, color="green")
  })
  output$MeanBox<- renderInfoBox({
    infoBox(
      tags$b("Mean"), paste0(signif(mean(selecteddata()[[input$Parameter2]], na.rm=TRUE),3)),
      icon=icon("calculator"), fill=TRUE, color="green"
    )
  })
  output$SDBox<- renderInfoBox({
    infoBox(
      tags$b("Standard deviation"), paste0(signif(sd(selecteddata()[[input$Parameter2]], na.rm=TRUE),3)),
      icon=icon("align-center"), fill=TRUE, color="green"
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
    ggplotly(ggplot(data1(), aes_string( x="Date" , y=input$ycol, color="Employee"  ))
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

    colsEMP<-reactive({
      if(all(abnormalsEMP()>0)) casesEMP<-"red" else
        if(all(abnormalsEMP()<0)) casesEMP<-"black" else
          casesEMP<-c("black","red")
        return(casesEMP)
    })
    # thr<-input$Parameter<100& input$Parameter>50
    ggplotly(
      ggplot(data3(),aes_string(x="Date", y=input$Parameter))
      +geom_point(aes(color=abnormalsEMP()))

      +geom_line()
      +theme(legend.position='none')
      +scale_color_manual(values=colsEMP())
      +geom_hline(yintercept=input$slider3,color="red")
      +geom_hline(yintercept=input$slider4,color="red")
    )
  })






  #####PLOT M2#####

  selecteddata<-reactive({

    empdata<-filter(data4(), EMP_FiscalCode == input$EMP2)

    #removing FC column
    empdata[2]<-NULL



    return(empdata)


  })

  abnormals<-reactive({
    thr<-selecteddata()[input$Parameter2]<input$slider1 | selecteddata()[input$Parameter2]>input$slider2
    return(thr)
  })

  countab<-reactive({
    num<-length(abnormals()[abnormals()==TRUE])
    return(num)
  })


  output$plot4 = renderPlotly({
    cols<-reactive({
      if(all(abnormals()>0)) cases<-"red" else
        if(all(abnormals()<0)) cases<-"black" else
          cases<-c("black","red")
        return(cases)
    })

    ggplotly(ggplot(selecteddata(), aes_string("Date", input$Parameter2))
             +geom_point(aes(color=abnormals()))
             +geom_line()
             +theme(legend.position='none')
             +scale_color_manual(values=cols())
             +geom_hline(yintercept=input$slider1,color="red")
             +geom_hline(yintercept=input$slider2,color="red")
    )
  }
  )
}
)
shinyApp(ui, server)
