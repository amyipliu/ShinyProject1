######UI####


library(shinydashboard)

shinyUI(dashboardPage(skin = 'black',
  dashboardHeader(title = "Default of Credit Card Clients Dataset"),
  dashboardSidebar(sidebarUserPanel("Amy IP Liu"),
                   sidebarMenu(
                     menuItem("Home", tabName = 'home', icon = icon("home")),
                     menuItem("Population", tabName = 'population', icon = icon("bar-chart"),
                              menuItem("Age",tabName = "page",icon = icon("circle-o")),
                              menuItem("Gender",tabName = "pgender",icon = icon("circle-o")),
                              menuItem("Education",tabName = "pedu",icon = icon("circle-o")),
                              menuItem("Marital Status",tabName = "pmar",icon = icon("circle-o")),
                              menuItem("Gender & Education",tabName = "pge",icon = icon("circle-o")),
                              menuItem("Gender & Age",tabName = "pga",icon = icon("circle-o"))
                              ),
                     menuItem("Credit Limit", tabName = 'limb', icon = icon("credit-card-alt")),
                     menuItem("Default", tabName = 'df', icon = icon("bank")),
                     menuItem("Data", tabName = 'data', icon = icon("tasks"))
                   )),
  dashboardBody(
    tabItems(
      tabItem(tabName = 'home',
              box(width = 12, height = 'auto', 
                  style="padding: 20px;",
                ## abstract ##
                  h3("This App is using the data set 'Default of Credit Card Clients Dataset',
                  which is from a Taiwanese bank in 2005.", align = "center", font = "Times New Roman"),
                br(),
                  h4(align = "center", "The data set includes customers' basic information and payment records."),
                  h4(align = "center", "The App will analyze card users,"),  h4(align = "center","credit limit and default distribution by different variables 
                     such as gender, education level."),
                  h4(align = "center", "Opportunity for future research includes analysis of payment patterns and variables influenceing default,"),
                     h4(align = "center","in order to improve risk management for banks"),
                  br(), br(),
                  #img(src = "pp.jpg", height = 600, weight =800, align="center")
                HTML('<center><img src="pp.jpg", height = 200, width = 850 ></center>')
                  )),
      
      tabItem(tabName = 'pge', fluidRow(
        box(title = 'Male Education Portion', 
            plotlyOutput('ed_M', height = 200), 
            background = 'light-blue',
            solidHeader = T),
        box(title = 'Female Education Portion', 
            plotlyOutput('ed_F', height = 200), 
            background = 'maroon',
            solidHeader = T),
        box(title = 'Education & Gender Distribution', width = 12,
            plotlyOutput('edgepop'),
            background = 'teal',
            solidHeader = T)
      )),
      tabItem(tabName = 'pga',
              fluidRow(box(title = 'Gender & Age Distribution', width = 12,
                           htmlOutput('age_gender'),
                           background = 'teal',
                           solidHeader = T))),
      tabItem(tabName = 'pgender',
              fluidRow(
                box(title = 'Card Users by Gender',htmlOutput('plot1'), background = "light-blue"),
                box(title = 'Card Users by Gender', htmlOutput('gensum'), background = "maroon") 
              )),
      tabItem(tabName = 'page',
              fluidRow( #test
                box(title = 'Card Users by Age', solidHeader = TRUE, background = 'light-blue',
                    width = 8, plotOutput('test')),
                box(title = "Inputs", width = 4, status = "warning", solidHeader = TRUE,
                    "Box content here", br(), "More box content",
                    sliderInput("slider", "Slider input:", 
                                min(cc1$age), max(cc1$age), c(min(cc1$age),max(cc1$age)),step = 5)),
                box(title = 'Portion by Age', htmlOutput('ageppie'), width = 4, height = 'auto', background = 'light-blue')
                
              )), #age end
      tabItem(tabName = 'pedu',
              fluidRow(
                box(title = 'Card Users by Education', htmlOutput('edpie'), width = 5, background = "light-blue"),
                box(title = 'Card Users by Education', htmlOutput('edpbar'), background = "maroon")
              )),
      tabItem(tabName = 'pmar',        
              fluidRow(
                box(title = 'Card Users by Marital Status', htmlOutput('marppie'), width = 5, background = 'light-blue'),
                box(title = 'Card Users by Marital Status', htmlOutput('marpbar'), background = 'maroon')
              )),
      tabItem(tabName = 'limb', 
              fluidRow(
              box(plotOutput('lbsb'), title = 'Credit Limit Range', background = 'light-blue'),
              box(plotOutput('lbgbx'), title = 'Credit Limit by Gender',
               background = 'light-blue'),
              box(plotOutput('lb_edu'), title = 'Credit Limit by Education', background = 'light-blue'),
              box(plotOutput('lb_mar'), title = 'Credit Limit by Marital Status', background = 'light-blue')
      )),
      tabItem(tabName = 'data', 
              selectizeInput("selected2",
                             "Select Item to Display",
                             c("Credit_Limit", "Gender", "Age", "Education", "Marital", 'Default')),
              fluidRow(box(DT::dataTableOutput('table'), width = 12))
              
              
    ),
    tabItem(tabName = 'df',        
            fluidRow(
              box(title = 'Default percentage by gender', plotlyOutput('dfge'), width = 6, background = 'light-blue'),
              box(title = 'Default percentage by Education', plotlyOutput('dfedu'), background = 'maroon'),
              box(title = 'Default percentage by Martial Status', plotlyOutput('dfmar'), width = 6, background = 'light-blue'),
              box(title = 'Default percentage by Age', plotOutput('dfage'), background = 'maroon')
            ))
  )
)))



