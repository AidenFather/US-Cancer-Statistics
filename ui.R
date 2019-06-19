## ui.R ##

shinyUI(
  dashboardPage(skin = "red",
  dashboardHeader(
    title = "US Cancer Statistics",
    titleWidth = 250
  ),
  
  # Sidebar with a slider input for number of bins 
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      menuItem("Overview", tabName = "home", icon = icon("book")),
      menuItem("State Cancer Rates", tabName = "facts", icon = icon("bar-chart-o")),
      menuItem("Caner Rates per Age Group", tabName = "annual", icon = icon("line-chart")),
      menuItem("Top 10 Cancers by State", tabName = "top10", icon = icon("chart-area")),
      menuItem(text = HTML("Cancer Rates by Cancer Type,</br> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;  Race, and Gender"), tabName = "old", icon = icon("chart-pie"))
                     
      
    )
  ),
  
  dashboardBody(
    tabItems(  
      ###Overview tab###
      tabItem(tabName = "home",
              fluidRow(
                column(width = 6, 
                       box(height = 950, 
                         title = h2(strong("Overview")), width = NULL, solidHeader = TRUE, background = "navy", 
                          tags$h4("Cancer statistics basically describe what happens in large groups of populations and provide a big picture 
                                   in time of the burden of cancer on society. More specifically, cancer statistics provide the number of people diagnosed with 
                                    and who die from cancer each year. They also show differences of incidence and mortality among the age group, gender, race, location, and cancer type. 
                                   This Shiny project was conducted to provide such cancer statistics with various visualizations. For a better understanding of the influence of 
                                   age, race, sex, and region on the incidence and mortality of cancers, the listed analyses below were performed.
                                  "),
                          br(),
                          
                          tags$ol(
                                  h4(tags$li("State Cancer Rates")),
                                  h4(tags$li("Cancer Rates per Age Group")),
                                  h4(tags$li("Top 10 Cancers by State")),
                                  h4(tags$li("Cancer Rates by Cancer Type, Race, and Gender"))
                                  ),
                          br(),
                          tags$h4("'State Cancer Rates' section provides state cancer incidence and mortality rates for the years from 1999 to 2015. 
                          'Cancer Rates per Age Group' section shows caner rate distributions and normalized rates per age group.        
                          'Top 10 Cancers by State' section displays the top 10 cancer cases and cancer deaths in each state.       
                          'Cancer Rates by Cancer Type, Race, and Gender' section exhibits incidence (or mortality) percentage of the five age groups for a specific cancer type. 
                          "),
                          HTML('<center><img src = "http://nycdatascience.com/blog/wp-content/uploads/2019/06/mitosis-3876669_1280.jpg" width ="75%"></center>'),
                           
                          tags$h4("Figure. Cancer cells illustration", align = "center")
                          
    
                       )
                ), 
                column(width = 6,
                       box(height = 500,
                         title = h2(strong("Summary of Findings")), width = NULL, solidHeader = TRUE, background = "navy",
                         tags$ol(
                         h4(tags$li("The results clearly show that cancer impacts people of all ages, but it does not always affect them equally. Differences in genetics, hormones, environmental exposures, 
                                    and other factors can lead to differences in risk among different groups of people. 
                                    For most cancers, though, increasing age is the most important risk factor.")),
                         h4(tags$li("The results also show that cancer incidence and mortality rates are increasing over the years. 
                                    This might result from the increased population size and the aged population each year.")),
                         h4(tags$li("The most common cancers in each state are breast cancer, lung and bronchus cancer, 
                                    prostate cancer, colon and rectum cancer, melanoma of the skin, corpus and uterus cancer, and bladder cancer.")),
                         h4(tags$li("Men's cancer rates are higher than women's rates. When comparing groups based on race and sex, the cancer mortality rate is 
                                    highest in African American men and lowest in Asian/Pacific Islander women.")),
                         h4(tags$li("The primary age group for thyroid and testis cancers is 30 - 49 (years), while the majority age group of the other cancers is 65+ (years).")),
                         h4(tags$li("The mortality rate of lung and bronchus cancer has been decreased over the years, 
                                    but lung and bronchus cancer had the highest mortality rate in 2015."))
                         )
                       ),
                       box(height = 400,
                         title = h2(strong("Data Source")), width = NULL, background = "navy",
                         h4("The data used for this study were obtained from the following sources:"),
                         tags$ol(
                         
                         h4(tags$li("Centers for Disease Control and Prevention (https://www.cdc.gov/cancer/uscs/USCS_1999_2015_ASCII.zip)")),
                         h4(tags$li("Enigma (https://public.enigma.com/datasets/71dc0128-624e-4713-8a0d-0f277cb781ba)"))
                         
                       )
                       )  
                )
              )
      ),
      
      
      
      ###Regional Facts tab### 
      tabItem(tabName = "facts",
              fluidRow(
                column(width = 2, selectInput("df_eventtype1", label = h4(strong("Event Type")), choices = cancer.event_type, selected = "Incidence")),
                column(width = 2, selectInput("df_site1", label = h4(strong("Cancer Type")), choices = cancer.type, selected = "All Cancer Sites Combined")),
                column(width = 2, selectInput("df_race1", label = h4(strong("Race")), 
                                              choices = c("All Races", "American Indian/Alaska Native", "Asian/Pacific Islander", "Black", "Hispanic" , "White"), selected = "All Races")),
                column(width = 2, selectInput("df_sex1", label = h4(strong("Sex")), choices = c("Female", "Male", "Male and Female"), selected = "Male and Female"))
              ),
              fluidRow( 
                box(width = 10, 
                    solidHeader = TRUE, status = NULL, plotlyOutput("cancer_map", height = 600))
              )
              
              ),     
      
      
      ## Cancer Rates vs. Age Group ##
      tabItem(tabName = "annual",
              fluidRow(
                column(width = 2, selectInput("df_eventtype2", label = h4(strong("Event Type")), choices = cancer.event_type, selected = "Incidence")),
                column(width = 2, selectInput("df_site2", label = h4(strong("Cancer Type")), choices = cancer.type, selected = "All Cancer Sites Combined")),
                column(width = 2, selectInput("df_race3", label = h4(strong("Race")), 
                                              choices = c("All Races", "American Indian/Alaska Native", "Asian/Pacific Islander", "Black", "Hispanic" , "White"), selected = "All Races")),
                column(width = 2, selectInput("df_sex3", label = h4(strong("Sex")), choices = c("Female", "Male", "Male and Female"), selected = "Male and Female"))
              ),
              fluidRow(
                column(width = 6,
                       box(height = 500,
                         title = strong(textOutput("boxplot.title")), width = NULL, solidHeader = FALSE, status = "warning", plotlyOutput("boxplot.rate"))
                ),
                
                column(width = 6, 
                       box(height = 500,
                         title = strong(textOutput("divergebar.title")), width = NULL, solidHeader = FALSE, status = "warning", plotlyOutput("divergebar.rate"))
                )   
              )
        
        
        
      ),
      
      ## Top 10 Cancers tab ##
      tabItem(tabName = "top10",
              fluidRow(
                column(width = 2, selectInput("df_area1", label = h4(strong("Area")), choices = state.name, selected = "Alabama")),
                column(width = 2, selectInput("df_race2", label = h4(strong("Race")), 
                                              choices = c("All Races", "American Indian/Alaska Native", "Asian/Pacific Islander", "Black", "Hispanic" , "White"), selected = "All Races")),
                column(width = 2, selectInput("df_sex2", label = h4(strong("Sex")), choices = c("Female", "Male", "Male and Female"), selected = "Male and Female")),
                column(width = 1, selectInput("df_year2", label = h4(strong("Year")), choices = 1999:2015, selected = 2015))
              ),
              fluidRow(
                column(width = 6,
                       box(height = 500,
                         title = strong("Top 10 Cancers by Incidence Rate"), width = NULL, solidHeader = TRUE, status = "primary", plotlyOutput("incidence.rate"))
                ),
                
                column(width = 6, 
                       box(height = 500,
                         title = strong("Top 10 Cancers by Mortality Rate"), width = NULL, solidHeader = TRUE, status = "warning", plotlyOutput("death.rate"))
                )   
              )
             
              ),
      
      ## Racts by Type, Gender, Race tab ##
      tabItem(tabName = "old",
              fluidRow(
                column(width = 2, selectInput("df_eventtype3", label = h4(strong("Event Type")), choices = cancer.event_type, selected = "Incidence")),
                column(width = 2, selectInput("df_race4", label = h4(strong("Race")), 
                                              choices = c("All Races", "American Indian/Alaska Native", "Asian/Pacific Islander", "Black", "Hispanic" , "White"), selected = "All Races")),
                column(width = 2, selectInput("df_sex4", label = h4(strong("Sex")), choices = c("Female", "Male", "Male and Female"), selected = "Male and Female")),
                column(width = 2, selectInput("df_year3", label = h4(strong("Year")), choices = 1999:2015, selected = 2015))
              ),
              fluidRow(
                column(width = 6,
                       box(height = 500,
                           title = strong(textOutput("siterace.title")), width = NULL, solidHeader = FALSE, status = "warning", plotOutput("site_race"))
                ),
                
                column(width = 6, 
                       box(height = 500,
                           title = strong(textOutput("agerace.title")), width = NULL, solidHeader = FALSE, status = "warning", plotOutput("sex_race"))
                )   
              )
              
      
      )
      
   )
  ) 
 )
)



