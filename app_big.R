#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
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
            menuItem("Client Demography visualization",tabName = "M2"),
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
                                 plotOutput("plotUniv")
                                )
                     )
                         
                   ),
                        
            tabItem("M2",
                    titlePanel("Bivariate visualization of client demographics"),
                 tabsetPanel(   
                    tabPanel("Age",
                             sidebarLayout(
                                 sidebarPanel(
                                     
                                     checkboxGroupInput("Age2", label = h3("Choose the Age-groups to be included"), choices = list("15 - 29" = "15 - 29", "30 - 44" = "30 - 44", "45 - 59" = "45 - 59", "60 - 99" = "60 - 99"),selected = c("15 - 29", "30 - 44", "45 - 59", "60 - 99")),
                                     
                                     hr(),
                                     
                                     conditionalPanel(
                                         id = "AC1",
                                         condition = "input.AgeSub1 == Job",
                                         checkboxGroupInput("AgeSub2", label = h3("Choose the Jobs to be included"), 
                                                            choices = list("Admin." = "admin.", "Blue-collar" = "blue-collar", "Management" = "management", "Services" = "services", "Entrepreneur" = "entrepreneur", "Technician" = "technician", "Housemaid" = "housemaid", "Retired" = "retired", "Student" = "student", "Self-employed" = "self-employed", "Unemployed" = "unemployed", "Unknown" = "unknown"),selected = c("admin.","blue-collar","management","services","entrepreneur","technician","housemaid","retired","student","self-employed","unemployed")),
                                     ),
                                     conditionalPanel(
                                         id = "AC2",
                                         condition = "input.AgeSub1 == Marital_Status",
                                         checkboxGroupInput("AgeSub3", label = h3("Choose the Marital Statuses to be included"), 
                                                            choices = list("Single" = "single", "Married" = "married", "Divorced" = "divorced", "Unknown" = "unknown"),selected = c("single", "married", "divorced", "unknown")),
                                     ),
                                     conditionalPanel(
                                         id = "AC3",
                                         condition = "input.AgeSub1 == Education",
                                         checkboxGroupInput("AgeSub4", label = h3("Choose the Education Backgrounds to be included"), 
                                                            choices = list("Basic 4 years" = "basic.4y", "Basic 6 years" = "basic.6y", "Basic 9 years" = "basic.9y", "High School" = "high.school", "Professional Course" = "professional.course", "University Degree" = "university.degree", "Illiterate" = "illiterate", "Unknown" = "unknown"),selected = c("basic.4y", "basic.6y", "basic.9y", "high.school", "professional.course", "university.degree", "illiterate", "unknown")),
                                         
                                     ),
                                     conditionalPanel(
                                         id = "AC4",
                                         condition = "input.AgeSub1 == Housing_Loan",
                                         checkboxGroupInput("AgeSub5", label = h3("Choose the Housing Loan Statuses to be included"), 
                                                            choices = list("Yes" = "yes", "No" = "no", "Unknown" = "unknown"),selected = c("yes","no","unknown")),
                                     ),
                                     conditionalPanel(
                                         id = "AC5",
                                         condition = "input.AgeSub1 == Personal_Loan",
                                         checkboxGroupInput("AgeSub6", label = h3("Choose the Personal Loan Statuses to be included"), 
                                                            choices = list("Yes" = "yes", "No" = "no", "Unknown" = "unknown"),selected = c("yes","no","unknown")),
                                     ),
                                 ),
                                     
                                 
                                 # Show a plot of the generated distribution
                                 mainPanel(
                                     selectInput("AgeSub1", "Choose a Variable:", 
                                                 choices=c("Job","Marital_Status","Education","Housing_Loan","Personal_Loan"),selected = c("Job")),
                                     plotOutput("AgePlot2")
                                 ),
                                     
                            ),
                     
                    ),
                    
                    tabPanel("Job",
                             sidebarLayout(
                                 sidebarPanel(
                                     
                                     checkboxGroupInput("Job2", label = h3("Choose the Job Specifications to be included"), 
                                                        choices = list("Admin." = "admin.", "Blue-collar" = "blue-collar", "Management" = "management", "Services" = "services", "Entrepreneur" = "entrepreneur", "Technician" = "technician", "Housemaid" = "housemaid", "Retired" = "retired", "Student" = "student", "Self-employed" = "self-employed", "Unemployed" = "unemployed", "Unknown" = "unknown"),selected = c("admin.","blue-collar","management","services","entrepreneur","technician","housemaid","retired","student","self-employed","unemployed")),
                                     
                                     hr(),
                                     
                                     conditionalPanel(
                                         id = "JC1",
                                         condition = "input.JobSub1 == Age",
                                         checkboxGroupInput("JobSub2", label = h3("Choose the Age-groups to be included"), 
                                                            choices = list("15 - 29" = "15 - 29", "30 - 44" = "30 - 44", "45 - 59" = "45 - 59", "60 - 99" = "60 - 99"),selected = c("15 - 29", "30 - 44", "45 - 59", "60 - 99")),
                                     ),
                                     conditionalPanel(
                                         id = "JC2",
                                         condition = "input.JobSub1 == Marital_Status",
                                         checkboxGroupInput("JobSub3", label = h3("Choose the Marital Statuses to be included"), 
                                                            choices = list("Single" = "single", "Married" = "married", "Divorced" = "divorced", "Unknown" = "unknown"),selected = c("single", "married", "divorced", "unknown")),
                                     ),
                                     conditionalPanel(
                                         id = "JC3",
                                         condition = "input.JobSub1 == Education",
                                         checkboxGroupInput("JobSub4", label = h3("Choose the Education Backgrounds to be included"), 
                                                            choices = list("Basic 4 years" = "basic.4y", "Basic 6 years" = "basic.6y", "Basic 9 years" = "basic.9y", "High School" = "high.school", "Professional Course" = "professional.course", "University Degree" = "university.degree", "Illiterate" = "illiterate", "Unknown" = "unknown"),selected = c("basic.4y", "basic.6y", "basic.9y", "high.school", "professional.course", "university.degree", "illiterate", "unknown")),
                                         
                                     ),
                                     conditionalPanel(
                                         id = "JC4",
                                         condition = "input.JobSub1 == Housing_Loan",
                                         checkboxGroupInput("JobSub5", label = h3("Choose the Housing Loan Statuses to be included"), 
                                                            choices = list("Yes" = "yes", "No" = "no", "Unknown" = "unknown"),selected = c("yes","no","unknown")),
                                     ),
                                     conditionalPanel(
                                         id = "JC5",
                                         condition = "input.JobSub1 == Personal_Loan",
                                         checkboxGroupInput("JobSub6", label = h3("Choose the Personal Loan Statuses to be included"), 
                                                            choices = list("Yes" = "yes", "No" = "no", "Unknown" = "unknown"),selected = c("yes","no","unknown")),
                                     ),
                                     
                                 ),
                                 # Show a plot of the generated distribution
                                 mainPanel(
                                     selectInput("JobSub1", "Choose a Variable:", 
                                                 choices=c("Age","Marital_Status","Education","Housing_Loan","Personal_Loan"),selected = c("Age")),
                                     plotOutput("JobPlot2")
                                 ),
                                 
                             ),
                             
                    
                      ),
                    tabPanel("Marital Status",
                             sidebarLayout(
                                 sidebarPanel(
                                     
                                     checkboxGroupInput("Mar22", label = h3("Choose the Marital Statuses to be included"), 
                                                        choices = list("Single" = "single", "Married" = "married", "Divorced" = "divorced", "Unknown" = "unknown"),selected = c("single", "married", "divorced", "unknown")),
                                     
                                     hr(),
                                     
                                     conditionalPanel(
                                         id = "MC1",
                                         condition = "input.MarSub1 == Age",
                                         checkboxGroupInput("MarSub2", label = h3("Choose the Age-groups to be included"), 
                                                            choices = list("15 - 29" = "15 - 29", "30 - 44" = "30 - 44", "45 - 59" = "45 - 59", "60 - 99" = "60 - 99"),selected = c("15 - 29", "30 - 44", "45 - 59", "60 - 99")),
                                     ),
                                     conditionalPanel(
                                         id = "MC2",
                                         condition = "input.MarSub1 == Job",
                                         checkboxGroupInput("MarSub3", label = h3("Choose the Jobs to be included"), 
                                                            choices = list("Admin." = "admin.", "Blue-collar" = "blue-collar", "Management" = "management", "Services" = "services", "Entrepreneur" = "entrepreneur", "Technician" = "technician", "Housemaid" = "housemaid", "Retired" = "retired", "Student" = "student", "Self-employed" = "self-employed", "Unemployed" = "unemployed", "Unknown" = "unknown"),selected = c("admin.","blue-collar","management","services","entrepreneur","technician","housemaid","retired","student","self-employed","unemployed")),
                                     ),
                                     conditionalPanel(
                                         id = "MC3",
                                         condition = "input.MarSub1 == Education",
                                         checkboxGroupInput("MarSub4", label = h3("Choose the Education Backgrounds to be included"), 
                                                            choices = list("Basic 4 years" = "basic.4y", "Basic 6 years" = "basic.6y", "Basic 9 years" = "basic.9y", "High School" = "high.school", "Professional Course" = "professional.course", "University Degree" = "university.degree", "Illiterate" = "illiterate", "Unknown" = "unknown"),selected = c("basic.4y", "basic.6y", "basic.9y", "high.school", "professional.course", "university.degree", "illiterate", "unknown")),
                                         
                                     ),
                                     conditionalPanel(
                                         id = "MC4",
                                         condition = "input.MarSub1 == Housing_Loan",
                                         checkboxGroupInput("MarSub5", label = h3("Choose the Housing Loan Statuses to be included"), 
                                                            choices = list("Yes" = "yes", "No" = "no", "Unknown" = "unknown"),selected = c("yes","no","unknown")),
                                     ),
                                     conditionalPanel(
                                         id = "MC5",
                                         condition = "input.MarSub1 == Personal_Loan",
                                         checkboxGroupInput("MarSub6", label = h3("Choose the Personal Loan Statuses to be included"), 
                                                            choices = list("Yes" = "yes", "No" = "no", "Unknown" = "unknown"),selected = c("yes","no","unknown")),
                                     ),
                                     
                                 ),
                                 # Show a plot of the generated distribution
                                 mainPanel(
                                     selectInput("MarSub1", "Choose a Variable:", 
                                                 choices=c("Age","Job","Education","Housing_Loan","Personal_Loan"),selected = c("Age")),
                                     plotOutput("MarPlot2")
                                 ),
                                 
                             ),
                             
                             
                    ),
                    
                    tabPanel("Education",
                             sidebarLayout(
                                 sidebarPanel(
                                     
                                     checkboxGroupInput("Ed22", label = h3("Choose the Education Backgrounds to be included"), 
                                                        choices = list("Basic 4 years" = "basic.4y", "Basic 6 years" = "basic.6y", "Basic 9 years" = "basic.9y", "High School" = "high.school", "Professional Course" = "professional.course", "University Degree" = "university.degree", "Illiterate" = "illiterate", "Unknown" = "unknown"),selected = c("basic.4y", "basic.6y", "basic.9y", "high.school", "professional.course", "university.degree", "illiterate", "unknown")),
                                     
                                     hr(),
                                     
                                     conditionalPanel(
                                         id = "EC1",
                                         condition = "input.EdSub1 == Age",
                                         checkboxGroupInput("EdSub2", label = h3("Choose the Age-groups to be included"), 
                                                            choices = list("15 - 29" = "15 - 29", "30 - 44" = "30 - 44", "45 - 59" = "45 - 59", "60 - 99" = "60 - 99"),selected = c("15 - 29", "30 - 44", "45 - 59", "60 - 99")),
                                     ),
                                     conditionalPanel(
                                         id = "EC2",
                                         condition = "input.EdSub1 == Job",
                                         checkboxGroupInput("EdSub3", label = h3("Choose the Jobs to be included"), 
                                                            choices = list("Admin." = "admin.", "Blue-collar" = "blue-collar", "Management" = "management", "Services" = "services", "Entrepreneur" = "entrepreneur", "Technician" = "technician", "Housemaid" = "housemaid", "Retired" = "retired", "Student" = "student", "Self-employed" = "self-employed", "Unemployed" = "unemployed", "Unknown" = "unknown"),selected = c("admin.","blue-collar","management","services","entrepreneur","technician","housemaid","retired","student","self-employed","unemployed")),
                                     ),
                                     conditionalPanel(
                                         id = "EC3",
                                         condition = "input.EdSub1 == Marital_Status",
                                         checkboxGroupInput("EdSub4", label = h3("Choose the Marital Statuses to be included"), 
                                                            choices = list("Single" = "single", "Married" = "married", "Divorced" = "divorced", "Unknown" = "unknown"),selected = c("single", "married", "divorced", "unknown")),
                                         
                                     ),
                                     conditionalPanel(
                                         id = "EC4",
                                         condition = "input.EdSub1 == Housing_Loan",
                                         checkboxGroupInput("EdSub5", label = h3("Choose the Housing Loan Statuses to be included"), 
                                                            choices = list("Yes" = "yes", "No" = "no", "Unknown" = "unknown"),selected = c("yes","no","unknown")),
                                     ),
                                     conditionalPanel(
                                         id = "EC5",
                                         condition = "input.EdSub1 == Personal_Loan",
                                         checkboxGroupInput("EdSub6", label = h3("Choose the Personal Loan Statuses to be included"), 
                                                            choices = list("Yes" = "yes", "No" = "no", "Unknown" = "unknown"),selected = c("yes","no","unknown")),
                                     ),
                                     
                                 ),
                                 # Show a plot of the generated distribution
                                 mainPanel(
                                     selectInput("EdSub1", "Choose a Variable:", 
                                                 choices=c("Age","Job","Marital_Status","Housing_Loan","Personal_Loan"),selected = c("Age")),
                                     plotOutput("EdPlot2")
                                 ),
                                 
                             ),
                            
                             
                    ),
        

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
                                         plotOutput("AgePlot", height = "600px",width = "600px")
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
                                         plotOutput("HousingPlot",height = "600px",width = "600px")
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
        paste0("This dashboard is based on a Bank Marketing Dataset.\nThe dataset explores the Direct Marketing campaigns (phone calls)\nof a Portuguese banking institution.\n\nThe aim of this project is to analyze how efficient\nthe current marketing campaign of a Portuguese Bank performed\nvia phone-calls has been in getting the targeted clients\nto subscribe a term deposit based on the number of calls\nthe bank’s marketing team has made to them.\n\nIn the 'Variable description + visualization' board a brief description \nabout the nature of the variables considered and respective\nUnivariate Graphs have been provided for the purpose of visualizing them.\nIt is to be noted that all the variables explored\nare categorical in nature.\n\nIn the 'Client Demography visualization' board bivariate graphs\nare provided to develop an understanding of the grouping\nof clients with respect to the various personal factors considered.\n\nIn the 'Analysis of campaign calls' board a multivariate graphical analysis\n has been established to analyze how well the telemarketing campaign\nhas fared until now and possbile improvements can thus be explored.\nThe output variable determining the success of the campaign\nis based on a positive subscription status \nof the clients for a term deposit.\n\nThe dataset has been obtained from Kaggle.\nLink to dataset: https://www.kaggle.com/berkayalan/bank-marketing-data-set")
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
    
    A2 <- reactive({
        
        if (input$AgeSub1 == "Job"){
            a11 = bank_data2[,c(1,2)]
            a12 = a11[which(a11$job %in% input$AgeSub2),]
        }
        
        else if (input$AgeSub1 == "Marital_Status"){
            a11 = bank_data2[,c(1,3)]
            a12 = a11[which(a11$marital %in% input$AgeSub3),]
        }
        
        else if (input$AgeSub1 == "Education"){
            a11 = bank_data2[,c(1,4)]
            a12 = a11[which(a11$education %in% input$AgeSub4),]
        }
        
        else if (input$AgeSub1 == "Housing_Loan"){
            a11 = bank_data2[,c(1,6)]
            a12 = a11[which(a11$housing %in% input$AgeSub5),]
        }
        
        else if (input$AgeSub1 == "Personal_Loan"){
            a11 = bank_data2[,c(1,7)]
            a12 = a11[which(a11$loan %in% input$AgeSub6),]
        }
        a12[which(a12$age %in% input$Age2),]
    }) 
    
    output$AgePlot2 <- renderPlot({
        
        df11 <- data.frame(A2()) 
        colnames(df11) = c("Age",input$AgeSub1)
        z11 = ggplot(data = df11, aes_string(x = "Age", fill = input$AgeSub1)) + geom_bar(position="dodge") + labs(title = "Bar Chart for chosen factor of clients based on Age groups", x = "Age Groups", y = "Count") + theme(plot.title = element_text(size = 10, face = "bold"),axis.text.x = element_text(angle = 90))
            
        z11
        #geom_bar(position="dodge", stat="identity")
    })
    
    observe({
        if(input$AgeSub1 == "Job"){
        shinyjs::show("AC1")
        shinyjs::hide("AC2")
        shinyjs::hide("AC3")
        shinyjs::hide("AC4")
        shinyjs::hide("AC5")
        }
    })
    
    observe({
        if(input$AgeSub1 == "Marital_Status"){
        shinyjs::show("AC2")
        shinyjs::hide("AC1")
        shinyjs::hide("AC3")
        shinyjs::hide("AC4")
        shinyjs::hide("AC5")
        }
    })
    
    observe({
        if(input$AgeSub1 == "Education"){
            shinyjs::show("AC3")
            shinyjs::hide("AC1")
            shinyjs::hide("AC2")
            shinyjs::hide("AC4")
            shinyjs::hide("AC5")
        }
    })
    
    observe({
        if(input$AgeSub1 == "Housing_Loan"){
            shinyjs::show("AC4")
            shinyjs::hide("AC1")
            shinyjs::hide("AC2")
            shinyjs::hide("AC3")
            shinyjs::hide("AC5")
        }
    })
    
    observe({
        if(input$AgeSub1 == "Personal_Loan"){
            shinyjs::show("AC5")
            shinyjs::hide("AC1")
            shinyjs::hide("AC2")
            shinyjs::hide("AC3")
            shinyjs::hide("AC4")
        }
    })
    
    
    J2 <- reactive({
        
        if (input$JobSub1 == "Age"){
            j11 = bank_data2[,c(2,1)]
            j12 = j11[which(j11$age %in% input$JobSub2),]
        }
        
        else if (input$JobSub1 == "Marital_Status"){
            j11 = bank_data2[,c(2,3)]
            j12 = j11[which(j11$marital %in% input$JobSub3),]
        }
        
        else if (input$JobSub1 == "Education"){
            j11 = bank_data2[,c(2,4)]
            j12 = j11[which(j11$education %in% input$JobSub4),]
        }
        
        else if (input$JobSub1 == "Housing_Loan"){
            j11 = bank_data2[,c(2,6)]
            j12 = j11[which(j11$housing %in% input$JobSub5),]
        }
        
        else if (input$JobSub1 == "Personal_Loan"){
            j11 = bank_data2[,c(2,7)]
            j12 = j11[which(j11$loan %in% input$JobSub6),]
        }
        j12[which(j12$job %in% input$Job2),]
    }) 
    
    output$JobPlot2 <- renderPlot({
        
        df22 <- data.frame(J2()) 
        colnames(df22) = c("Job",input$JobSub1)
        z22 = ggplot(data = df22, aes_string(x = "Job", fill = input$JobSub1)) + geom_bar(position="dodge") + labs(title = "Bar Chart for chosen factor of clients based on Job Specifications", x = "Job Specifications", y = "Count") + theme(plot.title = element_text(size = 10, face = "bold"),axis.text.x = element_text(angle = 90))
        
        z22
    })
    
    observe({
        if(input$JobSub1 == "Age"){
            shinyjs::show("JC1")
            shinyjs::hide("JC2")
            shinyjs::hide("JC3")
            shinyjs::hide("JC4")
            shinyjs::hide("JC5")
        }
    })
    
    observe({
        if(input$JobSub1 == "Marital_Status"){
            shinyjs::show("JC2")
            shinyjs::hide("JC1")
            shinyjs::hide("JC3")
            shinyjs::hide("JC4")
            shinyjs::hide("JC5")
        }
    })
    
    observe({
        if(input$JobSub1 == "Education"){
            shinyjs::show("JC3")
            shinyjs::hide("JC1")
            shinyjs::hide("JC2")
            shinyjs::hide("JC4")
            shinyjs::hide("JC5")
        }
    })
    
    observe({
        if(input$JobSub1 == "Housing_Loan"){
            shinyjs::show("JC4")
            shinyjs::hide("JC1")
            shinyjs::hide("JC2")
            shinyjs::hide("JC3")
            shinyjs::hide("JC5")
        }
    })
    
    observe({
        if(input$JobSub1 == "Personal_Loan"){
            shinyjs::show("JC5")
            shinyjs::hide("JC1")
            shinyjs::hide("JC2")
            shinyjs::hide("JC3")
            shinyjs::hide("JC4")
        }
    })
    
    M2 <- reactive({
        
        if (input$MarSub1 == "Age"){
            m11 = bank_data2[,c(3,1)]
            m12 = m11[which(m11$age %in% input$MarSub2),]
        }
        
        else if (input$MarSub1 == "Job"){
            m11 = bank_data2[,c(3,2)]
            m12 = m11[which(m11$job %in% input$MarSub3),]
        }
        
        else if (input$MarSub1 == "Education"){
            m11 = bank_data2[,c(3,4)]
            m12 = m11[which(m11$education %in% input$MarSub4),]
        }
        
        else if (input$MarSub1 == "Housing_Loan"){
            m11 = bank_data2[,c(3,6)]
            m12 = m11[which(m11$housing %in% input$MarSub5),]
        }
        
        else if (input$MarSub1 == "Personal_Loan"){
            m11 = bank_data2[,c(3,7)]
            m12 = m11[which(m11$loan %in% input$MarSub6),]
        }
        m12[which(m12$marital %in% input$Mar22),]
    }) 
    
    output$MarPlot2 <- renderPlot({
        
        df33 <- data.frame(M2()) 
        colnames(df33) = c("Marital_Status",input$MarSub1)
        z33 = ggplot(data = df33, aes_string(x = "Marital_Status", fill = input$MarSub1)) + geom_bar(position="dodge") + labs(title = "Bar Chart for chosen factor of clients based on Marital Statuses", x = "Marital Status", y = "Count") + theme(plot.title = element_text(size = 10, face = "bold"),axis.text.x = element_text(angle = 90))
        #
        #
        z33
    })
    
    observe({
        if(input$MarSub1 == "Age"){
            shinyjs::show("MC1")
            shinyjs::hide("MC2")
            shinyjs::hide("MC3")
            shinyjs::hide("MC4")
            shinyjs::hide("MC5")
        }
    })
    
    observe({
        if(input$MarSub1 == "Job"){
            shinyjs::show("MC2")
            shinyjs::hide("MC1")
            shinyjs::hide("MC3")
            shinyjs::hide("MC4")
            shinyjs::hide("MC5")
        }
    })
    
    observe({
        if(input$MarSub1 == "Education"){
            shinyjs::show("MC3")
            shinyjs::hide("MC1")
            shinyjs::hide("MC2")
            shinyjs::hide("MC4")
            shinyjs::hide("MC5")
        }
    })
    
    observe({
        if(input$MarSub1 == "Housing_Loan"){
            shinyjs::show("MC4")
            shinyjs::hide("MC1")
            shinyjs::hide("MC2")
            shinyjs::hide("MC3")
            shinyjs::hide("MC5")
        }
    })
    
    observe({
        if(input$MarSub1 == "Personal_Loan"){
            shinyjs::show("MC5")
            shinyjs::hide("MC1")
            shinyjs::hide("MC2")
            shinyjs::hide("MC3")
            shinyjs::hide("MC4")
        }
    })
    
    E2 <- reactive({
        
        if (input$EdSub1 == "Age"){
            e11 = bank_data2[,c(4,1)]
            e12 = e11[which(e11$age %in% input$EdSub2),]
        }
        
        else if (input$EdSub1 == "Job"){
            e11 = bank_data2[,c(4,2)]
            e12 = e11[which(e11$job %in% input$EdSub3),]
        }
        
        else if (input$EdSub1 == "Marital_Status"){
            e11 = bank_data2[,c(4,3)]
            e12 = e11[which(e11$education %in% input$EdSub4),]
        }
        
        else if (input$EdSub1 == "Housing_Loan"){
            e11 = bank_data2[,c(4,6)]
            e12 = e11[which(e11$housing %in% input$EdSub5),]
        }
        
        else if (input$EdSub1 == "Personal_Loan"){
            e11 = bank_data2[,c(4,7)]
            e12 = e11[which(e11$loan %in% input$EdSub6),]
        }
        e12[which(e12$education %in% input$Ed22),]
    }) 
    
    output$EdPlot2 <- renderPlot({
        
        df44 <- data.frame(E2()) 
        colnames(df44) = c("Education",input$EdSub1)
        z44 = ggplot(data = df44, aes_string(x = "Education", fill = input$EdSub1)) + geom_bar(position="dodge") + labs(title = "Bar Chart for chosen factor of clients based on Education Backgrounds", x = "Education Backgrounds", y = "Count") + theme(plot.title = element_text(size = 10, face = "bold"),axis.text.x = element_text(angle = 90))
        
        z44
    })
    
    observe({
        if(input$EdSub1 == "Age"){
            shinyjs::show("EC1")
            shinyjs::hide("EC2")
            shinyjs::hide("EC3")
            shinyjs::hide("EC4")
            shinyjs::hide("EC5")
        }
    })
    
    observe({
        if(input$EdSub1 == "Job"){
            shinyjs::show("EC2")
            shinyjs::hide("EC1")
            shinyjs::hide("EC3")
            shinyjs::hide("EC4")
            shinyjs::hide("EC5")
        }
    })
    
    observe({
        if(input$EdSub1 == "Marital_Status"){
            shinyjs::show("EC3")
            shinyjs::hide("EC1")
            shinyjs::hide("EC2")
            shinyjs::hide("EC4")
            shinyjs::hide("EC5")
        }
    })
    
    observe({
        if(input$EdSub1 == "Housing_Loan"){
            shinyjs::show("EC4")
            shinyjs::hide("EC1")
            shinyjs::hide("EC2")
            shinyjs::hide("EC3")
            shinyjs::hide("EC5")
        }
    })
    
    observe({
        if(input$EdSub1 == "Personal_Loan"){
            shinyjs::show("EC5")
            shinyjs::hide("EC1")
            shinyjs::hide("EC2")
            shinyjs::hide("EC3")
            shinyjs::hide("EC4")
        }
    })
}
# Run the application 
shinyApp(ui = ui, server = server)
