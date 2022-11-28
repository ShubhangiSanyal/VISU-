library(shiny)
library(shinydashboard)
library(ggplot2)

ui <- dashboardPage(
  dashboardHeader(title="Cement Manufacturing", titleWidth = 240),
  dashboardSidebar(
    width=240,
    sidebarMenu(
      menuItem("About",tabName = "tab1"),
      menuItem("Viewing attributes",tabName = "tab2"),
      menuItem("Bivariate relations",tabName = "tab3"),
      menuItem("Multivariate relations",tabName = "tab4"),
      menuItem("Conclusion",tabName = "tab5")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("tab1",fluidPage(
        tags$b(tags$h1("Abstract", style="color: red")),
        tags$p("In civil engineering, concrete forms a major part of most projects. 
               Among all materials used in construction, concrete is the most crucial 
               to any structure. Measuring and understanding the concrete compressive strength, 
               thus, is vital. It has been observed that the concrete compressive strength is a 
               highly nonlinear function of age and ingredients. Some of these ingredients are 
               cement, blast furnace slag, fly ash, water, superplasticizer, coarse aggregate, 
               and fine aggregate."), 
        tags$b(tags$h1("Objectives", style="color: red")),
        tags$p("Using the data described above, data visualization can be used to 
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
                                     c("black","grey","white","steelblue","green","blue","brown","purple")),
                       sliderInput("bins", p("Number of bins"),
                                   min = 1, max = 50, value = 22),
                       actionButton("go1","Show")
                     ),
                     mainPanel(plotOutput("histogram"),textOutput("write"))
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
        )
      )),
      tabItem("tab4",h1("Multivariate Analysis", style="color: red"), fluidPage(
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
        )
      )),
      tabItem("tab5",h1("Conclusion",style="color: red"), fluidPage(
        tags$p("The following points summarize the observations in this report:"),
        tags$ol(
          tags$li("Histogram provides the distribution of every attribute. For cement, the histogram observes that cement concentration varies from 140 to 160 units most of the time."),
          tags$li("Correlation between `water` and `slag` is 0.107"),
          tags$li("Correlation between `cement` and `strength` is 0.498"),
          tags$li("Correlation between `water` and `strength` is -0.289")
        )
      ))
    )
  ),
  skin="red"
)
server <- function(input,output){
  output$table<-renderTable({
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
    data.frame(cbind(Attribute, Description))
  })
  data <- read.csv("concrete.csv", header=TRUE, stringsAsFactors=FALSE)
  
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
  output$Boxplot <- renderPlot({
    long_data <- data.frame(ingredient=rep(c("cement","slag","ash","water",
                                             "superplastic","coarseagg","fineagg",
                                             "age","strength"), each=1030),
                            amount=c(data$cement, data$slag, data$ash, data$water, 
                                     data$superplastic, data$coarseagg, 
                                     data$fineagg, data$age, data$strength))
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
                 isolate(ggplot(data, aes(x = .data[[input$MulGroup[1]]], 
                                          y = .data[[input$MulGroup[2]]], 
                                          color = .data[[input$MulGroup[3]]]))+
                           geom_point(size=1.7)+
                           scale_color_gradient(low="blue", high="yellow")+
                           ggtitle(paste("Scatter plot of",input$MulGroup[1],",",input$MulGroup[2],
                                         "and",input$MulGroup[3])))
               })
  )
  
}

shinyApp(ui,server)