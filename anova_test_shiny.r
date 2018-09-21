library(shiny)
library(ggplot2)

################################################################################
# Define UI for data upload app ----
################################################################################
ui <- fluidPage(
  # title
  titlePanel("Analysis of Variance"),
  # layout control for sidebar, main panel 
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      fileInput("file1", "Input File", accept=c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      checkboxInput("header", "Header", TRUE),
      selectInput('sep', 'Separator',c(Comma=',',Semicolon=';',Tab='\t'),'\t'),
      selectInput('type', 'Sums of Squares type', c(I = 'type1', II = 'type2', III = 'type3'), 'type1'),
      #  tell Shiny where the renderUI controls (see server, below) should be rendered
      uiOutput('var') 
    ), 
    # Main panel for displaying outputs ----
    mainPanel(
      h4('File Preview'),
      tableOutput("contents"),
      h4('ANOVA Table'),
      tableOutput('aovSummary'),
      h4('Plot-1'),
      plotOutput("plot1")
    )
  )
)


################################################################################
# Define server logic to read selected file ----
################################################################################  
server <- function(input, output, session) {
  
  # define reactive function "f_readfile1" to read file 
  f_readfile1 <- reactive({
    csvfile <- input$file1
    if (is.null(csvfile)){return(NULL)}
    df <- read.csv(csvfile$datapath, header=input$header, sep=input$sep)
    return(df)
  })
  
  # create variable-selection controls on-the-fly with renderUI
  output$var <- renderUI({
    req(input$file1)
    list (
      selectInput("dvar", "Dependent Variable",names(f_readfile1())),
      selectInput("ivar", "Independent Variable",names(f_readfile1())),
      actionButton("do_anova", "ANOVA"),
      actionButton("do_plot1", "PLOT1")
    )
  })
  
  # display the file contents
  output$contents <- renderTable({
    req(input$file1)
    df <- read.csv(input$file1$datapath, header= input$header, sep= input$sep, quote= input$quote)
    return(head(df,n=10))
  },spacing="xs",width=250)
  
  # run the ANOVA 
  output$aovSummary = renderTable({
    req(input$file1)
    req(input$do_anova)
    if(input$type == 'type1'){ return(isolate(anova(lm(f_readfile1()[,input$dvar] ~ f_readfile1()[,input$ivar], data = f_readfile1())))) }
    if(input$type == 'type2'){ return(isolate(Anova(lm(f_readfile1()[,input$dvar] ~ f_readfile1()[,input$ivar], data = f_readfile1())), Type = "II", test.statistic = "F")) }
    if(input$type == 'type3'){
      isolate(fit <- aov(f_readfile1()[,input$dvar] ~ f_readfile1()[,input$ivar], data = f_readfile1()))
      return(drop1(fit, ~ . , test = 'F'))
    }
  })
  
  # list dvar
  output$listdvar = renderTable({
    req(input$file1)
    req(input$do_listdvar)
    df= f_readfile1()
    xdata= lapply(df[input$ivar],as.numeric)
    ydata= lapply(df[input$dvar],as.numeric)
    return(head(ydata,n=5))
  })

    # plot1
  output$plot1 = renderPlot({
    req(input$file1)
    req(input$do_plot1)
    # read data-frame - by default, contents are a list (ie potentially mixed-type) 
    df= f_readfile1()
    
    # store independent variable (xdata), "unlist", and cast as numeric
    xdata= as.numeric(unlist(df[input$ivar]))
    # make a factor version of the variable - plot will calculate the mean, and x-axis will have tic-marks at levels 
    xfactor= as.factor(xdata)
    # store dependent variable (ydat) similarly, but cast as double
    ydata= as.numeric(unlist(df[input$dvar]))

    # create the first plot object 
    myplot1 <- plot(
      ydata ~ xfactor,
      xlab= input$ivar,ylab=input$dvar, # set axis-labels 
      par(pin=c(2,4)), # define plot dimensions in inches
      col=levels(xfactor), # define colours according to xdata levels
      palette(rainbow(8)) # define an 8-colour rainbow palette 
    )

    # return the plot object 
    return(myplot1)
  })

  
}


################################################################################
# Create Shiny app ----
################################################################################
shinyApp(ui, server)

