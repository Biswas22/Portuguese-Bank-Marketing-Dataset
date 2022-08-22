#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#Link to Dashboard: https://sohambiswas.shinyapps.io/Bank_Marketing_Dataset_Component_2/
#

library(shiny)
library(shinyjs)
library(shinydashboard)
library(rlang)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(dgof)
library(data.table)

bank_data <- read.csv("bank_marketing_dataset.csv")
bank_data2 = copy(bank_data)

bank_data2$age[bank_data2$age < 30] = "15 - 29"
bank_data2$age[bank_data2$age < 45 & bank_data2$age >= 30] = "30 - 44"
bank_data2$age[bank_data2$age < 60 & bank_data2$age >= 45] = "45 - 59"
bank_data2$age[bank_data2$age < 99 & bank_data2$age >= 60] = "60 - 99"

campaign_calls = array(dim = 1)

for(i in 1:length(bank_data$campaign))
{
    if(bank_data$campaign[i] == 1)
        campaign_calls[i] = "1"
    
    else if(bank_data$campaign[i] == 2)
        campaign_calls[i] = "2"
    
    else   
        campaign_calls[i] = "more than 2"
    
}
bank_data2$campaign_calls = campaign_calls

# Define UI for application that draws a histogram
ui <- dashboardPage(
    
    dashboardHeader(title="Marketing Campaign of a Portugese Bank"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Introduction", tabName = "M0"),
            menuItem("Variable description + visualization",tabName = "M1"),
            menuItem("Analysis of campaign calls",tabName = "M3")
        )
    ),
    dashboardBody(
        useShinyjs(),  
        tabItems(
            
            tabItem("M0",
                    h2("Introduction",align = "center"),
                    
                    fluidRow(align = "center",
                             
                             verbatimTextOutput("textIni")
                    )
                    
            ),
            tabItem("M1",
                    titlePanel("Univariate Graphs to visualize the data"),
                    
                    fluidRow(
                        column(4,actionButton("Bar_G","Bar Graph")),
                        column(4,actionButton("Pie_G","Pie Graph"))
                    ),
                    hr(),     
                    sidebarLayout(
                        sidebarPanel(
                            
                            selectInput("Univ", "Choose a Variable:", 
                                        choices=c("Age","Job","Marital_Status","Education","Housing_Loan","Personal_Loan","Previous_Outcome","Campaign_Calls","Subscription_Status"),selected = c("Age")),
                        ),
                        mainPanel(
                            verbatimTextOutput("textUniv"),
                            plotOutput("plotUniv", height = "400px")
                        )
                    )
                    
            ),
            
            tabItem("M3",
                    titlePanel("Clients of a group against their subscription status for a term deposit given the number of campaign calls made"),
                    
                    tabsetPanel(
                        
                        sidebarLayout(
                            sidebarPanel(
                                
                                selectInput("Mult1", "Choose the number of calls made:", 
                                            choices=c("1","2","Greater_than_2","Display_All","Overall"),selected = c("Overall")),
                                hr()
                            ),
                            mainPanel(verbatimTextOutput("Calls"))
                        ),
                        
                        tabPanel("Age",
                                 
                                 fluidRow(
                                     # Show a plot of the generated distribution
                                     mainPanel(
                                         plotOutput("AgePlot")
                                     )
                                 )
                                 
                        ),
                        
                        tabPanel("Job",
                                 sidebarLayout(
                                     sidebarPanel(
                                         
                                         checkboxGroupInput("Jobs1", label = h3("Choose the Jobs to be included"), 
                                                            choices = list("Admin." = "admin.", "Blue-collar" = "blue-collar", "Management" = "management", "Services" = "services", "Entrepreneur" = "entrepreneur", "Technician" = "technician", "Housemaid" = "housemaid", "Retired" = "retired", "Student" = "student", "Self-employed" = "self-employed", "Unemployed" = "unemployed", "Unknown" = "unknown"),selected = c("admin.","blue-collar","management","services","entrepreneur","technician","housemaid","retired","student","self-employed","unemployed")),
                                         
                                         hr(),
                                         
                                     ),
                                     # Show a plot of the generated distribution
                                     mainPanel(
                                         plotOutput("JobPlot")
                                     )
                                 )     
                        ),
                        
                        tabPanel("Marital Status",
                                 sidebarLayout(
                                     sidebarPanel(
                                         
                                         selectInput("Mar1", "Include or Exclude clients with unknown Martital Status:", 
                                                     choices=c("Include","Exclude"),selected = c("Include")),
                                         hr(),
                                         
                                         selectInput("Mar2", "Include or Exclude clients with a Marital Status of Divorced:", 
                                                     choices=c("Include","Exclude"),selected = c("Include")),
                                         hr()
                                         
                                     ),
                                     # Show a plot of the generated distribution
                                     mainPanel(
                                         plotOutput("MaritalPlot")
                                     )
                                 )     
                        ),
                        
                        tabPanel("Education",
                                 sidebarLayout(
                                     sidebarPanel(
                                         
                                         selectInput("Edu1", "Include or Exclude clients with unknown Education Backgrounds:", 
                                                     choices=c("Include","Exclude"),selected = c("Include")),
                                         hr(),
                                         
                                         selectInput("Edu3", "Include or Exclude clients with an Education Background of Illiterate:", 
                                                     choices=c("Include","Exclude"),selected = c("Include")),
                                         hr(),
                                         
                                         selectInput("Edu2", "Include clients from one of these groups of Education Backgrounds:", 
                                                     choices=c("HighSchool_and_Above","Below_HighSchool","Display_All"),selected = c("Display_All")),
                                         hr()
                                     ),
                                     # Show a plot of the generated distribution
                                     mainPanel(
                                         plotOutput("EduPlot")
                                     )
                                 )     
                        ),
                        
                        tabPanel("Housing Loan",
                                 sidebarLayout(
                                     sidebarPanel(
                                         
                                         selectInput("Hloan1", "Include or Exclude clients with unknown Housing Loan Statuses:", 
                                                     choices=c("Include","Exclude"),selected = c("Include")),
                                         hr()
                                     ),
                                     # Show a plot of the generated distribution
                                     mainPanel(
                                         plotOutput("HousingPlot")
                                     )
                                 )     
                        ),
                        
                        tabPanel("Personal Loan",
                                 sidebarLayout(
                                     sidebarPanel(
                                         
                                         selectInput("Ploan1", "Include or Exclude clients with unknown Personal Loan Statuses:", 
                                                     choices=c("Include","Exclude"),selected = c("Include")),
                                         hr()
                                     ),
                                     # Show a plot of the generated distribution
                                     mainPanel(
                                         plotOutput("PersonalPlot")
                                     )
                                 )     
                        ),
                        
                        tabPanel("Previous Outcome",
                                 sidebarLayout(
                                     sidebarPanel(
                                         
                                         selectInput("Pout1", "Include or Exclude clients with Non-existent Previous Outcomes:", 
                                                     choices=c("Include","Exclude"),selected = c("Include")),
                                         hr()
                                     ),
                                     # Show a plot of the generated distribution
                                     mainPanel(
                                         plotOutput("PoutPlot")
                                     )
                                 )     
                        )
                    )
                    
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$textIni <- renderText({
        paste0("This dashboard is based on a Bank Marketing Dataset.\nThe dataset explores the Direct Marketing campaigns (phone calls)\nof a Portuguese banking institution.\n\nThe aim of this project is to analyze how efficient\nthe current marketing campaign of a Portuguese Bank performed\nvia phone-calls has been in getting the targeted clients\nto subscribe a term deposit based on the number of calls\nthe bank’s marketing team has made to them.\n\nIn the 'Variable description + visualization' board a brief description \nabout the nature of the variables considered and respective\nUnivariate Graphs have been provided for the purpose of visualizing them.\nIt is to be noted that all the variables explored\nare categorical in nature.\n\nIn the 'Analysis of campaign calls' board a multivariate graphical analysis\n has been established to analyze how well the telemarketing campaign\nhas fared until now and possbile improvements can thus be explored.\nThe output variable determining the success of the campaign\nis based on a positive subscription status \nof the clients for a term deposit.\n\nThe dataset has been obtained from Kaggle.\nLink to dataset: https://www.kaggle.com/berkayalan/bank-marketing-data-set")
    })
    G1 <- reactive({
        if (input$Univ == "Age"){
            bank_data2[,1]
        }
        else if (input$Univ == "Job"){
            bank_data2[,2]
        }
        else if (input$Univ == "Marital_Status"){
            bank_data2[,3]
        }
        else if (input$Univ == "Education"){
            bank_data2[,4]
        }
        else if (input$Univ == "Housing_Loan"){
            bank_data2[,6]
        }
        else if (input$Univ == "Personal_Loan"){
            bank_data2[,7]
        }
        else if (input$Univ == "Previous_Outcome"){
            bank_data2[,15]
        }
        else if (input$Univ == "Campaign_Calls"){
            bank_data2[,22]
        }
        else if (input$Univ == "Subscription_Status"){
            bank_data2[,21]
        }
    }) 
    
    output$textUniv <- renderText({
        
        if (input$Univ == "Age"){
            paste0("The 'Age' variable gives the age of the clients in the campaign.\nThe age of the clients have been grouped into 6 intervals:\n15 - 29, 30 - 44, 45 - 59, 60 - 74, 75 - 99.")
        }
        else if (input$Univ == "Job"){
            paste0("The 'Job' variable gives the job specifications of the clients\nin the campaign.\nThere are 11 job specifications attributed to in this variable.\nThere are also a few clients whose job specification is unknown.")
        }
        else if (input$Univ == "Marital_Status"){
            paste0("The 'Marital_Status' variable gives the marital status of the\nclients in the campaign.\nSingle, Married and Divorced are the 3 marital statuses\nattributed to this varaible.\nThe marital status of a few clients are unknown")
        }
        else if (input$Univ == "Education"){
            paste0("The 'Education' variable gives the highest education qualification\nof the clients in the campaign. There are 7 education degrees\nattributed to this variable ranging from\nilliterate to a person holding an university degree.\nThere are a few clients whose educational background is unknown.")
        }
        else if (input$Univ == "Housing_Loan"){
            paste0("The 'Housing_Loan' variable indicates whether a client\nhas taken a housing loan.\nThe housing loan status of a few clients are unknown.")
        }
        else if (input$Univ == "Personal_Loan"){
            paste0("The 'Personal_Loan' variable indicates whether a client\nhas taken a personal loan.\nThe personal loan status of a few clients are unknown.")
        }
        else if (input$Univ == "Previous_Outcome"){
            paste0("The variable 'Previous_Outcome' gives the outcome(if valid) of the\nprevious marketing campaign for the clients in this campaign.\nSuccess and Failure is attributed respectively to this variable.\nIn the situation that the previous campaign is not applicable for a\ncurrent client the variable gives the value of Non-existent\nin that situation.")
        }
        else if (input$Univ == "Campaign_Calls"){
            paste0("The variable 'Campaign_Calls' gives the number of calls made to\nthe clients during the current campaign.\nThe calls made are categorized as 1, 2 or more than 2.\nIn this project we have analyzed and tried to determine that increasing\nthe number of calls for which client groups would produce a better result.")
        }
        else if (input$Univ == "Subscription_Status"){
            paste0("The variable 'Subscription_Status' is the  output variable\nindicating whether the client has subscribed for a term deposit.")
        }
        
    })
    
    observeEvent(
        input$Bar_G,{output$plotUniv <- renderPlot({
            df1 <- isolate(data.frame(G1()))
            colnames(df1) <- c(input$Univ)
            
            b1 <- ggplot(data = df1,aes_string(input$Univ,fill = input$Univ)) + geom_bar() + labs(title = "Bar Chart of the chosen Variable", x = input$Univ, y = "Frequency") + theme(axis.text.x = element_text(angle = 90))
            
            b1
            
        })})
    
    observeEvent(
        input$Pie_G,{output$plotUniv <- renderPlot({
            df2 <- isolate(data.frame(G1()))
            colnames(df2) <- c(input$Univ)
            
            p1 <- ggplot(data = df2,aes_string(x = factor(1),fill = input$Univ)) + geom_bar(position = "fill", width = 1) + coord_polar(theta = "y") + labs(title = "Pie Chart of the chosen Variable", x = "", y = "")
            
            p1
            
        })})
    
    output$Calls <- renderPrint({
        list = c("Choose 1 for visualizing the situation when only 1 call is made to the clients",
                 "Choose 2 for visualizing the situation when only 2 calls are made to the clients",
                 "Choose More than 2 for visualizing the situation when more than 2 calls are made to the clients",
                 "Choose Display All for visualizing all the above 3 situations together for comparision",
                 "Choose Overall for visualizing the situation irrespective of the number of calls made")
        print(list)
    })  
    
    A1 <- reactive({
        if (input$Mult1 == "1"){
            bank_data2[which(bank_data2$campaign_calls == "1"),c(1,21)]
        }
        
        else if (input$Mult1 == "2"){
            bank_data2[which(bank_data2$campaign_calls == "2"),c(1,21)]
        }
        
        else if (input$Mult1 == "Greater_than_2"){
            bank_data2[which(bank_data2$campaign_calls == "more than 2"),c(1,21)]
        }
        
        else if (input$Mult1 == "Display_All"){
            bank_data2[,c(1,21,22)]
        }
        
        else if (input$Mult1 == "Overall"){
            bank_data2[,c(1,21)]
        }
        
    }) 
    
    J1 <- reactive({
        if (input$Mult1 == "1"){
            j2 = bank_data2[which(bank_data2$campaign_calls == "1"),c(2,21)]
        }
        
        else if (input$Mult1 == "2"){
            j2 = bank_data2[which(bank_data2$campaign_calls == "2"),c(2,21)]
        }
        
        else if (input$Mult1 == "Greater_than_2"){
            j2 = bank_data2[which(bank_data2$campaign_calls == "more than 2"),c(2,21)]
        }
        
        else if (input$Mult1 == "Display_All"){
            j2 = bank_data2[,c(2,21,22)]
        }
        
        else if (input$Mult1 == "Overall"){
            j2 = bank_data2[,c(2,21,22,20)]
        }
        j2[which(j2$job %in% input$Jobs1),]
    }) 
    
    M1 <- reactive({
        if (input$Mult1 == "1"){
            m2 = bank_data2[which(bank_data2$campaign_calls == "1"),c(3,21)]
        }
        
        else if (input$Mult1 == "2"){
            m2 = bank_data2[which(bank_data2$campaign_calls == "2"),c(3,21)]
        }
        
        else if (input$Mult1 == "Greater_than_2"){
            m2 = bank_data2[which(bank_data2$campaign_calls == "more than 2"),c(3,21)]
        }
        
        else if (input$Mult1 == "Display_All"){
            m2 = bank_data2[,c(3,21,22)]
        }
        
        else if (input$Mult1 == "Overall"){
            m2 = bank_data2[,c(3,21,22,20)]
        }
        
        if(input$Mar1 == "Exclude"){
            m3 = m2[!(m2$marital == "unknown"),]
        }
        
        else{
            m3 = copy(m2)  
        }
        
        if(input$Mar2 == "Exclude"){
            m3[!(m3$marital == "divorced"),]
        }
        
        else{
            m3
        }
        
        
        
    }) 
    
    E1 <- reactive({
        if (input$Mult1 == "1"){
            e2 =  bank_data2[which(bank_data2$campaign_calls == "1"),c(4,21)]
        }
        
        else if (input$Mult1 == "2"){
            e2 = bank_data2[which(bank_data2$campaign_calls == "2"),c(4,21)]
        }
        
        else if (input$Mult1 == "Greater_than_2"){
            e2 = bank_data2[which(bank_data2$campaign_calls == "more than 2"),c(4,21)]
        }
        
        else if (input$Mult1 == "Display_All"){
            e2 = bank_data2[,c(4,21,22)]
        }
        
        else if (input$Mult1 == "Overall"){
            e2 = bank_data2[,c(4,21,22,20)]
        }
        
        if(input$Edu1 == "Exclude"){
            e5 = e2[!(e2$education == "unknown"),]
        }
        
        else{
            e5 = copy(e2)
        }
        
        if(input$Edu3 == "Exclude"){
            e3 = e5[!(e5$education == "illiterate"),]
        }
        
        else{
            e3 = copy(e5)
        }
        
        if(input$Edu2 == "HighSchool_and_Above"){
            e3[which(e3$education == "high.school" | e3$education == "professional.course" | e3$education == "university.degree"),]
        }
        
        else if(input$Edu2 == "Below_HighSchool"){
            e3[!(e3$education == "high.school" | e3$education == "professional.course" | e3$education == "university.degree"),]
        }
        
        else if(input$Edu2 == "Display_All"){
            e3
        }
    }) 
    
    H1 <- reactive({
        if (input$Mult1 == "1"){
            h2 = bank_data2[which(bank_data2$campaign_calls == "1"),c(6,21)]
        }
        
        else if (input$Mult1 == "2"){
            h2 = bank_data2[which(bank_data2$campaign_calls == "2"),c(6,21)]
        }
        
        else if (input$Mult1 == "Greater_than_2"){
            h2 = bank_data2[which(bank_data2$campaign_calls == "more than 2"),c(6,21)]
        }
        
        else if (input$Mult1 == "Display_All"){
            h2 = bank_data2[,c(6,21,22)]
        }
        
        else if (input$Mult1 == "Overall"){
            h2 = bank_data2[,c(6,21,22,20)]
        }
        
        if(input$Hloan1 == "Exclude"){
            h2[!(h2$housing == "unknown"),]
        }
        
        else{
            h2
        } 
    }) 
    
    P1 <- reactive({
        if (input$Mult1 == "1"){
            p2 = bank_data2[which(bank_data2$campaign_calls == "1"),c(7,21)]
        }
        
        else if (input$Mult1 == "2"){
            p2 = bank_data2[which(bank_data2$campaign_calls == "2"),c(7,21)]
        }
        
        else if (input$Mult1 == "Greater_than_2"){
            p2 = bank_data2[which(bank_data2$campaign_calls == "more than 2"),c(7,21)]
        }
        
        else if (input$Mult1 == "Display_All"){
            p2 = bank_data2[,c(7,21,22)]
        }
        
        else if (input$Mult1 == "Overall"){
            p2 = bank_data2[,c(7,21,22,20)]
        }
        
        if(input$Ploan1 == "Exclude"){
            p2[!(p2$loan == "unknown"),]
        }
        
        else{
            p2
        }  
    })
    
    O1 <- reactive({
        if (input$Mult1 == "1"){
            o2 = bank_data2[which(bank_data2$campaign_calls == "1"),c(15,21)]
        }
        
        else if (input$Mult1 == "2"){
            o2 = bank_data2[which(bank_data2$campaign_calls == "2"),c(15,21)]
        }
        
        else if (input$Mult1 == "Greater_than_2"){
            o2 = bank_data2[which(bank_data2$campaign_calls == "more than 2"),c(15,21)]
        }
        
        else if (input$Mult1 == "Display_All"){
            o2 = bank_data2[,c(15,21,22)]
        }
        
        else if (input$Mult1 == "Overall"){
            o2 = bank_data2[,c(15,21,22,20)]
        }
        
        if(input$Pout1 == "Exclude"){
            o2[!(o2$poutcome == "nonexistent"),]
        }
        
        else{
            o2
        }  
        
    })
    
    output$AgePlot <- renderPlot({
        
        df3 <- data.frame(A1()) 
        if(length(df3) == 2)
        {
            colnames(df3) <- c("Age","Subscription")
            t1 = ggplot(data = df3,aes(Age, fill = Subscription)) + geom_bar() + labs(title = "Bar Chart for Subscription Status of clients for chosen number of calls in each Age-group", x = "Age", y = "Count") + theme(plot.title = element_text(size = 10, face = "bold"),axis.text.x = element_text(angle = 90))
            
        }
        
        else if(length(df3) == 3)
        {
            colnames(df3) <- c("Age","Subscription","Campaigns")
            
            t1 = ggplot(df3, aes(x = Age,fill = Subscription))+
                geom_bar(position = "stack") + facet_wrap(~Campaigns,nrow = 3) +
                labs(title = "Subscriptions made by clients of various age groups based on number of calls", x = "Age", y = "Count") +
                theme(plot.title = element_text(size = 10,hjust = 0.5),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
            t1 = t1 + coord_flip()
        }
        
        else
        {
            colnames(df3) <- c("Age","Subscription","Campaigns","Filler")
            t1 = ggplot(data = df3,aes(Age, fill = Subscription)) + geom_bar() + labs(title = "Bar Chart for Subscription Status of clients in each Age-group", x = "Age", y = "Count") + theme(plot.title = element_text(size = 10, face = "bold"),axis.text.x = element_text(angle = 90)) 
        }
        
        t1
    })
    
    output$JobPlot <- renderPlot({
        
        df4 <- data.frame(J1()) 
        if(length(df4) == 2)
        {
            colnames(df4) <- c("Job","Subscription")
            t2 = ggplot(data = df4,aes(Job, fill = Subscription)) + geom_bar() + labs(title = "Bar Chart for Subscription Status of clients for chosen number of calls across various Job Descriptions", x = "Job", y = "Count") + theme(plot.title = element_text(size = 10, face = "bold"),axis.text.x = element_text(angle = 90))
            
        }
        
        else if(length(df4) == 3)
        {
            colnames(df4) <- c("Job","Subscription","Campaigns")
            
            t2 = ggplot(df4, aes(x = Job,fill = Subscription))+
                geom_bar(position = "stack") + facet_wrap(~Campaigns,nrow = 3) +
                labs(title = "Subscriptions made by clients  across various Job Descriptions based on number of calls", x = "Job", y = "Count") +
                theme(plot.title = element_text(size = 10,hjust = 0.5),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
            t2 = t2 + coord_flip()
        }
        else
        {
            colnames(df4) <- c("Job","Subscription","Campaigns","Filler")
            t2 = ggplot(data = df4,aes(Job, fill = Subscription)) + geom_bar() + labs(title = "Bar Chart for Subscription Status of clients across various Job Descriptions", x = "Job", y = "Count") + theme(plot.title = element_text(size = 10, face = "bold"),axis.text.x = element_text(angle = 90))
            
        }    
        t2
    })
    
    output$MaritalPlot <- renderPlot({
        
        df5 <- data.frame(M1()) 
        if(length(df5) == 2)
        {
            colnames(df5) <- c("Marital","Subscription")
            t3 = ggplot(data = df5,aes(Marital, fill = Subscription)) + geom_bar() + labs(title = "Bar Chart for Subscription Status of clients for chosen number of calls across various Marital Statuses", x = "Marital Status", y = "Count") + theme(plot.title = element_text(size = 10, face = "bold"),axis.text.x = element_text(angle = 90))
            
        }
        
        else if(length(df5) == 3)
        {
            colnames(df5) <- c("Marital","Subscription","Campaigns")
            
            t3 = ggplot(df5, aes(x = Marital,fill = Subscription))+
                geom_bar(position = "stack") + facet_wrap(~Campaigns,nrow = 3) +
                labs(title = "Subscriptions made by clients  across various Marital Statuses based on number of calls", x = "Marital Status", y = "Count") +
                theme(plot.title = element_text(size = 10,hjust = 0.5),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
            t3 = t3 + coord_flip()
        }
        else
        {
            colnames(df5) <- c("Marital","Subscription","Campaigns","Filler")
            t3 = ggplot(data = df5,aes(Marital, fill = Subscription)) + geom_bar() + labs(title = "Bar Chart for Subscription Status of clients across various Marital Statuses", x = "Marital Status", y = "Count") + theme(plot.title = element_text(size = 10, face = "bold"),axis.text.x = element_text(angle = 90))
            
        }    
        t3
    })
    
    output$EduPlot <- renderPlot({
        
        df6 <- data.frame(E1()) 
        if(length(df6) == 2)
        {
            colnames(df6) <- c("Edu","Subscription")
            t4 = ggplot(data = df6,aes(Edu, fill = Subscription)) + geom_bar() + labs(title = "Bar Chart for Subscription Status of clients for chosen number of calls based on their Educational backgrounds", x = "Educational Background", y = "Count") + theme(plot.title = element_text(size = 10, face = "bold"),axis.text.x = element_text(angle = 90))
            
        }
        
        else if(length(df6) == 3)
        {
            colnames(df6) <- c("Edu","Subscription","Campaigns")
            
            t4 = ggplot(df6, aes(x = Edu,fill = Subscription)) +
                geom_bar(position = "stack") + facet_wrap(~Campaigns,nrow = 3) +
                labs(title = "Subscriptions made by clients wrt their Educational Statuses based on number of calls", x = "Educational Background", y = "Count")+
                theme(plot.title = element_text(size = 10,hjust = 0.5),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
            t4 = t4 + coord_flip()
        }
        else
        {
            colnames(df6) <- c("Edu","Subscription","Campaigns","Filler")
            t4 = ggplot(data = df6,aes(Edu, fill = Subscription)) + geom_bar() + labs(title = "Bar Chart for Subscription Status of clients based on their Educational Backgrounds", x = "Marital Status", y = "Count") + theme(plot.title = element_text(size = 10, face = "bold"),axis.text.x = element_text(angle = 90))
            
        }    
        t4
    })
    
    output$HousingPlot <- renderPlot({
        
        df7 <- data.frame(H1()) 
        if(length(df7) == 2)
        {
            colnames(df7) <- c("Housing","Subscription")
            t5 = ggplot(data = df7,aes(Housing, fill = Subscription)) + geom_bar() + labs(title = "Bar Chart for Subscription Status of clients for chosen number of calls based on their Housing Loan Statuses", x = "Housing Loan Status", y = "Count") + theme(plot.title = element_text(size = 10, face = "bold"),axis.text.x = element_text(angle = 90))
            
        }
        
        else if(length(df7) == 3)
        {
            colnames(df7) <- c("Housing","Subscription","Campaigns")
            
            t5 = ggplot(df7, aes(x = Housing,fill = Subscription)) +
                geom_bar(position = "stack") + facet_wrap(~Campaigns,nrow = 3) +
                labs(title = "Subscriptions made by clients wrt their Housing Loan Statuses based on number of calls", x = "Housing Loan Status", y = "Count") +
                theme(plot.title = element_text(size = 10,hjust = 0.5),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
            t5 = t5 + coord_flip()
        }
        else
        {
            colnames(df7) <- c("Housing","Subscription","Campaigns","Filler")
            t5 = ggplot(data = df7,aes(Housing, fill = Subscription)) + geom_bar() + labs(title = "Bar Chart for Subscription Status of clients based on their Housing Loan Statuses", x = "Housing Loan Status", y = "Count") + theme(plot.title = element_text(size = 10, face = "bold"),axis.text.x = element_text(angle = 90))
            
        }    
        t5
    })
    
    output$PersonalPlot <- renderPlot({
        
        df8 <- data.frame(P1()) 
        if(length(df8) == 2)
        {
            colnames(df8) <- c("Personal","Subscription")
            t6 = ggplot(data = df8,aes(Personal, fill = Subscription)) + geom_bar() + labs(title = "Bar Chart for Subscription Status of clients for chosen number of calls based on their Personal Loan Statuses", x = "Personal Loan Status", y = "Count") + theme(plot.title = element_text(size = 10, face = "bold"),axis.text.x = element_text(angle = 90))
            
        }
        
        else if(length(df8) == 3)
        {
            colnames(df8) <- c("Personal","Subscription","Campaigns")
            
            t6 = ggplot(df8, aes(x = Personal,fill = Subscription)) +
                geom_bar(position = "stack") + facet_wrap(~Campaigns,nrow = 3) +
                labs(title = "Subscriptions made by clients wrt their Personal Loan Statuses based on number of calls", x = "Personal Loan Status", y = "Count") +
                theme(plot.title = element_text(size = 10,hjust = 0.5),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
            t6 = t6 + coord_flip()
        }
        else
        {
            colnames(df8) <- c("Personal","Subscription","Campaigns","Filler")
            t6 = ggplot(data = df8,aes(Personal, fill = Subscription)) + geom_bar() + labs(title = "Bar Chart for Subscription Status of clients based on their Personal Loan Statuses", x = "Personal Loan Status", y = "Count") + theme(plot.title = element_text(size = 10, face = "bold"),axis.text.x = element_text(angle = 90))
            
        }    
        t6
    })
    
    output$PoutPlot <- renderPlot({
        
        df9 <- data.frame(O1()) 
        if(length(df9) == 2)
        {
            colnames(df9) <- c("Pout","Subscription")
            t7 = ggplot(data = df9,aes(Pout, fill = Subscription)) + geom_bar() + labs(title = "Bar Chart for Subscription Status of clients for chosen number of calls based on the previous campaign's Outcome", x = "Previous Campaign Outcome", y = "Count") + theme(plot.title = element_text(size = 10, face = "bold"),axis.text.x = element_text(angle = 90))
            
        }
        
        else if(length(df9) == 3)
        {
            colnames(df9) <- c("Pout","Subscription","Campaigns")
            
            t7 = ggplot(df9, aes(x = Pout,fill = Subscription)) +
                geom_bar(position = "stack") + facet_wrap(~Campaigns,nrow = 3) +
                labs(title = "Subscriptions made by clients wrt the previous campaign's Outcome based on number of calls", x = "Previous Campaign Outcome", y = "Count") +
                theme(plot.title = element_text(size = 10,hjust = 0.5),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
            t7 = t7 + coord_flip()
        }
        else
        {
            colnames(df9) <- c("Pout","Subscription","Campaigns","Filler")
            t7 = ggplot(data = df9,aes(Pout, fill = Subscription)) + geom_bar() + labs(title = "Bar Chart for Subscription Status of clients based on the previous campaign's Outcome", x = "Previous Campaign Outcome", y = "Count") + theme(plot.title = element_text(size = 10, face = "bold"),axis.text.x = element_text(angle = 90))
        }    
        t7
    })
    
    
}
# Run the application 
shinyApp(ui = ui, server = server)
