library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(ggplot2)

ui <- dashboardPage(
  dashboardHeader(title="Cement Manufacturing", titleWidth = 240),
  dashboardSidebar(
    width=240,
    sidebarMenu(
      menuItem("About",tabName = "tab1"),
      menuItem("Dataset",tabName = "tab0"),
      menuItem("Visualizing attributes",tabName = "tab2"),
      menuItem("Bivariate relations",tabName = "tab3"),
      menuItem("Multivariate relations",tabName = "tab4"),
      menuItem("Conclusion",tabName = "tab5"),
      menuItem("Credits",tabName = "tab6")
    )
  ),
  dashboardBody(
    shinyDashboardThemes(theme = "flat_red"),
    tabItems(
      tabItem("tab0", fluidPage(align="center",
        tags$p("The variables of the dataset have been described in the ", 
               tags$i("About"), "section. Here's a view of the dataset based 
               on your selected range!"),
        sliderInput("row", label = "Select range of rows", min = 1, 
                    max = 1030, value = c(1, 5)),
        actionButton("go0","Show"),
        tableOutput("Data")
      )),
      tabItem("tab1",fluidPage(
        tags$b(tags$h1("Abstract", style="color: red")),
        tags$hr(),
        tags$p("In civil engineering, concrete forms a major part of most projects. 
               Among all materials used in construction, concrete is the most crucial 
               to any structure. Measuring and understanding the concrete compressive strength, 
               thus, is vital. It has been observed that the concrete compressive strength is a 
               highly nonlinear function of age and ingredients. Some of these ingredients are 
               cement, blast furnace slag, fly ash, water, superplasticizer, coarse aggregate, 
               and fine aggregate."), 
        tags$b(tags$h1("Objectives", style="color: red")),
        tags$hr(),
        tags$p("Using the data described below, data visualization can be used to 
               determine the following:"),
        tags$ul(
          tags$li("How each ingredient is distributed"),
          tags$li("How the proportion of each ingredient varies with different 
                  values of concrete compressive strength"),
          tags$li("How the proportion of each ingredient varies with age"),
          tags$li("How each ingredient proportion varies with each other when 
                  any two are taken at a time"),
          tags$li("Finally, how any two ingredient behaves when compared to the 
                  concrete compressive strength")
        ),
        tags$p("The above objectives can be achieved using histograms, and 
               boxplots for univariate analysis, scatter plots and line plots 
               for bivariate analysis, and gradient scatter plots for 
               multivariate analysis."),
        tags$b(tags$h1("Dataset Description", style="color: red")),
        tags$hr(),
        tags$p("This dataset has been taken from ",
               tags$i(tags$a(href="https://www.kaggle.com/datasets/vinayakshanawad/cement-manufacturing-concrete-dataset",
                      "Kaggle", style="color: red")),
              ". The actual concrete compressive strength (MPa - Mega Pascal) for 
              a given mixture under a specific age (days) was determined from a 
              laboratory. The data has 8 quantitative input variables, 1 
              quantitative output variable, and 1030 instances (observations). 
              The variables can be described as:"),
        tableOutput("table")
      )),
      tabItem("tab2",h1("Univariate Analysis", style="color: red"), fluidPage(
        tags$hr(),
        tabsetPanel(
          tabPanel("Histogram", fluid=TRUE,
                   tags$br(),
                   tags$b("Histograms and Density plots"),
                   tags$p("A histogram can be quite useful in visualizing the distribution 
               of each ingredient across a 1030 instances. From the list of 
               dropdown items below, choose an attribute and view its distribution 
               across the dataset."),
                   sidebarLayout(
                     sidebarPanel(
                       radioButtons("radio", label = p("Select an attribute"), choices = 
                                      c('cement','slag','ash','water','superplastic',
                                        'coarseagg','fineagg','age', 'strength'),
                                    selected=1),
                       selectInput(inputId="color1",label="Choose colour of histogram",choices = 
                                     c("yellow","grey","white","steelblue","green","blue","brown","purple")),
                       sliderInput("bins", p("Number of bins"),
                                   min = 1, max = 50, value = 22),
                       actionButton("go1","Show")
                     ),
                     mainPanel(plotOutput("histogram"))
                   )
          ),
          tabPanel("Boxplot", fluid=TRUE,
                   tags$br(),
                   tags$b("Boxplot of all attributes"),
                   tags$p("The boxplot of all the attributes (input and output) 
                          gives us a general idea of the range of their values and 
                          provides more descriptive statistical measures like mean, 
                          median, maximum, and minimum which can be useful for creating 
                          concrete mixtures. Further, it is useful in spotting outliers 
                          in the data. The following gives the boxplot described above."),
                   tags$br(),
                   plotOutput("Boxplot"),
                   tags$br(),
                   tags$p("The boxplot above provides certain information about each attribute. 
                          For example, in the case of fly ash (", tags$code("ash"),") the maximum, 
                          minimum, first quartile, median, and third quartile are 200.1, 0, 0, 0, 
                          and 118.3, respectively. Also, ", tags$code("age, fineagg, slag, 
                                                                      superplastic, and water"),
                          "show the presence of outliers.")
                   )
        )
      )),
      tabItem("tab3",h1("Bivariate Analysis", style="color: red"), fluidPage(
        tags$hr(),
        tags$b("Scatter plot and line of best fit"),
        tags$p("The relationship between any two ingredients that are used in the 
               composition of concrete or an ingredient’s change of behaviour with 
               respect to the concrete compressive strength which is the output 
               variable can be observed through a scatter plot. Additionally, a 
               line of best fit has been added to the plots to better conclude 
               their relationship."),
        sidebarLayout(
          sidebarPanel(
            checkboxGroupInput("BiGroup", label = "Select any 2 attributes", choices = 
                                 c('cement','slag','ash','water','superplastic',
                                   'coarseagg','fineagg','age', 'strength'),
                               selected=1),
            actionButton("go2","Show")
          ),
          mainPanel(plotOutput("BiScatter"))
        ),
        tags$i("(Note: if more than 2 attributes are selected, only the first 2 will be considered)")
      )),
      tabItem("tab4",h1("Multivariate Analysis", style="color: red"), fluidPage(
        tags$hr(),
        tags$b("Scatter plots with colour gradient"),
        tags$p("In a more complicated analysis, the scatter plot is used to map 
               the behaviour of not just one but two ingredients against a third 
               attribute which could also be the compressive strength. This is 
               done by using a colour gradient on the scatter plot on the third 
               variable."),
        sidebarLayout(
          sidebarPanel(
            checkboxGroupInput("MulGroup", label = "Select any 3 attributes", choices = 
                                 c('cement','slag','ash','water','superplastic',
                                   'coarseagg','fineagg','age', 'strength'),
                               selected=1),
            actionButton("go3","Show")
          ),
          mainPanel(plotOutput("MulScatter"))
        ),
        tags$i("(Note: if more than 3 attributes are selected, only the first 3 will be considered)")
      )),
      tabItem("tab5",h1("Conclusion", style="color: red"), fluidPage(
        tags$hr(),
        tags$p("The following points summarize the observations in this report:"),
        tags$br(),
        tags$ol(
          tags$li("From the distribution of the attributes, some of the observations 
                  are as follows:"),
          tags$ul(
            tags$li("most of the time the amount of cement varies from 140 to 160"),
            tags$li("amount of water varies from 190 to 192.5 with a probability of 0.56")
          ),
          tags$br(),
          tags$li("The interactions of each ingredient with the age of concrete 
                  provides the observations stated below:"),
          tags$ul(
            tags$li("cement, water and concrete strength are positively correlated with age"),
            tags$li("blast (slag), fly ash, superplasticizer, course and fine aggregate 
                    are negatively correlated with age")
          ),
          tags$br(),
          tags$li("The interactions of each ingredient with the concrete strength 
                  provides the observations stated below:"),
          tags$ul(
            tags$li("cement, blast (slag), superplasticizer, and age are positively 
                    correlated with concrete strength"),
            tags$li("fly ash, water, course and fine aggregate are negatively 
                    correlated with concrete strength")
          ),
          tags$br(),
          tags$li("Interactions of each ingredient with age and strength of concrete
                  provide observations, some of which are:"),
          tags$ul(
            tags$li("Moderately aged concrete (30 - 80 days) have the highest strength
                    which increases with an increase in the amount of blast (slag)"),
            tags$li("Moderately aged concrete (30 - 80 days) have the highest strength
                    which increases with a decrease in the amount of water")
          )
        )
      )),
      tabItem("tab6", fluidPage(
        tags$h1("References", style = "color:red"),
        tags$hr(),
        tags$p("Shanawad, V. (2021)", tags$i("Civil Engineering: Cement Manufacturing Dataset, Kaggle"), ". Available at: 
               https://www.kaggle.com/datasets/vinayakshanawad/cement-manufacturing-concrete-dataset 
               (Accessed: November 30, 2022). "),
        tags$br(),
        tags$br(),
        tags$br(),
        tags$br(),
        tags$br(),
        tags$br(),
        tags$b(tags$p("Created by", style = "color:red", align="right")),
        tags$p("Shubhangi Sanyal", tags$br(), "MSc Data Science, 2022", tags$br(),
               "Chennai Mathematical Institute", tags$br(), "Email:shubhangi@cmi.ac.in", align="right")
      ))
    )
  )
)
server <- function(input,output){
  
  Attribute=c('cement','slag','ash','water','superplastic','coarseagg','fineagg','age',
              'strength')
  Description=c("Cement measured in kg in a cubic metre mixture",
                "Blast measured in kg in a cubic metre mixture",
                "Fly ash measured in kg in a cubic metre mixture",
                "Water measured in kg in a cubic metre mixture",
                "Superplasticizer measured in kg in a cubic metre mixture",
                "Coarse Aggregate measured in kg in a cubic metre mixture",
                "Fine Aggregate measured in kg in a cubic metre mixture",
                "Age in days (1~365)",
                "Concrete compressive strength measured in MPa")
  var_table<-data.frame(cbind(Attribute, Description))
  
  output$table <- renderTable(var_table, striped=TRUE)
  
  
  data <- read.csv("concrete.csv", header=TRUE, stringsAsFactors=FALSE)
  
  observeEvent(input$go0,
               output$Data<-renderTable({
                 data[input$row[1]:input$row[2],]
               }, align='c', striped=TRUE, hover=TRUE, rownames=TRUE)
  )
  
  #Histograms
  observeEvent(input$go1,
               output$histogram <- renderPlot({
                 isolate(ggplot(data, aes(.data[[input$radio]])) + 
                           geom_histogram(bins=input$bins,color="black", fill = input$color1,
                                          aes(y = ..density..))+
                           geom_density(lwd=0.7, color=2, fill=2, alpha=0.25)+
                           xlab(paste(input$radio))+ylab("Frequency")+
                           ggtitle(paste("Histogram and density plot of", input$radio)))
               })
              )
  
  #Boxplot
  
  long_data <- data.frame(ingredient=rep(c("cement","slag","ash","water",
                                           "superplastic","coarseagg","fineagg",
                                           "age","strength"), each=1030),
                          amount=c(data$cement, data$slag, data$ash, data$water, 
                                   data$superplastic, data$coarseagg, 
                                   data$fineagg, data$age, data$strength))
  
  output$Boxplot <- renderPlot({
    ggplot(long_data, aes(x=ingredient,y=amount,fill=ingredient))+geom_boxplot()+
      ggtitle("Boxplot of all variables")+theme(legend.position="None")+ 
      scale_fill_brewer(palette="Paired")
  })
  
  observeEvent(input$go2,
               output$BiScatter <- renderPlot({
                 isolate(ggplot(data, aes(x = .data[[input$BiGroup[1]]], y = .data[[input$BiGroup[2]]])) + 
                           geom_point(color="cornflowerblue")+
                           geom_smooth(color="tomato")+
                           ggtitle(paste("Scatter plot of",input$BiGroup[1],"and",input$BiGroup[2])))
               })
               )
  
  observeEvent(input$go3,
               output$MulScatter <- renderPlot({
                 isolate(ggplot(data, aes(x = .data[[input$MulGroup[3]]], 
                                          y = .data[[input$MulGroup[2]]], 
                                          color = .data[[input$MulGroup[1]]]))+
                           geom_point(size=1.7)+
                           scale_color_gradient(low="dark blue", high="red")+
                           ggtitle(paste("Scatter plot of",input$MulGroup[1],",",input$MulGroup[2],
                                         "and",input$MulGroup[3])))
               })
  )
  
}

shinyApp(ui,server)
