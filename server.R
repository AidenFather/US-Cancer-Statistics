## server.R ##


shinyServer(function(input, output) {
   
  
  ##### CANCER RATES BY STATES #####
  
  ##reactive cancer_state map data##
  state_map_reactive = reactive({
    state.df1999 %>%
      filter(., event_type == input$df_eventtype1) %>%
      filter(., site == input$df_site1) %>% 
      filter(., race == input$df_race1) %>%
      filter(., sex == input$df_sex1) 
  })
  
  ##reactive event_type input (map)##
  df_eventtype1.choice = reactive({
    as.character(input$df_eventtype1)
  })
  
  ##reactive sex input (map)##
  df_sex1.choice = reactive({
    as.character(input$df_sex1)
  })
  
  ## reactive race1 input (map) ##
  df_race1.choice = reactive({
    as.character(input$df_race1)
  })
  
  ## reactive year input (map)
  df_site1.choice = reactive({
    as.numeric(input$df_site1)
  })
  
  ## reactive map colors (map) ##
  colors.choice = reactive({
    if (input$df_eventtype1 == "Incidence"){
      "Blues"
    }
    else {
      "Reds"
    }
  })
  
  output$cancer_map = renderPlotly({
    
    l = list(color = toRGB("white"), width = 2)
    
    g = list(scope = 'usa',projection = list(type = 'albers usa'), showlakes = FALSE,lakecolor = toRGB('white'))
    
    p = plot_geo(state_map_reactive(), locationmode = 'USA-states') %>%
      add_trace(
        z = ~rate, text = ~area1, locations = ~X,
        color = ~rate, colors = colors.choice(), frame = ~year) %>%
      colorbar(title = "Rate per 100,000 People") %>%
      layout(title = paste0("US"," ","Cancer"," ",df_eventtype1.choice()," ", 
                            "Rate by State"),
             # title = list(size = 30, color = "#070707", font = t), 
             # legend = list(font = list(size = 30)),
             geo = g) %>% 
      animation_opts(
           frame = 750, easing = "linear", redraw = TRUE
         ) %>%
         animation_slider(
           currentvalue = list(prefix = "YEAR ", font = list(color="Black"))
         ) 
    
  })
  
  
  ##### TOP 10 CANCERs #####
  
  #reactive cancer incidence.rate chart data ##
  incidence.rate_reactive = reactive({
    state.df1999 %>%
      filter(., event_type == "Incidence") %>%
      filter(., area1 == input$df_area1) %>%
      filter(., race == input$df_race2) %>%
      filter(., sex == input$df_sex2) %>%
      filter(., year == input$df_year2) %>%
      filter(., site != "All Cancer Sites Combined") %>% 
      mutate(., rate_ref = rate) %>% 
      arrange(., desc(rate)) %>% 
      top_n(10)  
    #arrange(., desc(rate1)) 
  })
  
  #reactive cancer death.rate chart data ##
  death.rate_reactive = reactive({
    state.df1999 %>%
      filter(., event_type == "Mortality") %>%
      filter(., area1 == input$df_area1) %>%
      filter(., race == input$df_race2) %>%
      filter(., sex == input$df_sex2) %>%
      filter(., year == input$df_year2) %>%
      filter(., site != "All Cancer Sites Combined") %>% 
      mutate(., rate_ref= rate) %>% 
      arrange(., desc(rate)) %>% 
      top_n(10) 
    
  })
  
  
    # reactive year input (chart)
  df_year2.choice = reactive({
    as.numeric(input$df_year2)
  })
  
  ## reactive state input (chart)
  df_area1.choice = reactive({
    as.character(input$df_area1)
  })
  
  ## reactive race2 input (chart) 
  df_race2.choice = reactive({
    as.character(input$df_race2)
  })
  
  ##reactive sex input (chart)
  df_sex2.choice = reactive({
    as.character(input$df_sex2)
  })
  
  
  ## Incidence Bar Chart ##
  output$incidence.rate = renderPlotly({

      p1 = ggplot(data=incidence.rate_reactive(),
               aes(x = reorder(site, rate), y = rate)) +
      geom_bar(position ="dodge", stat = "identity", fill = "#79D9FA") +
      coord_flip() +
      labs(title = paste0("New Cancer Cases<br>",df_area1.choice(),", ",df_race2.choice(),", ",df_sex2.choice(),", ",df_year2.choice()),
           x = "Cancer Type", y = "Rate per 100,000 People") +
      theme(axis.title = element_text(size=14), plot.title = element_text(hjust = 0), legend.position = "none"
            )
    ggplotly(p1)

  })
  
  ## Mortality Bar Chart ##
  output$death.rate = renderPlotly({
    
    p2 = ggplot(data=death.rate_reactive(),
               aes(x = reorder(site, rate), y = rate)) +
      geom_bar(position ="dodge", stat = "identity", fill ="#F5CBA7") +
      coord_flip() +
      labs(title = paste0("Cancer Deaths<br>",df_area1.choice(),", ",df_race2.choice(),", ",df_sex2.choice(),", ",df_year2.choice()) ,
           x = "Cancer Type", y = "Rate per 100,000 People") +
      theme(axis.title = element_text(size=14), plot.title = element_text(hjust = 0), legend.position = "none"
      )
    ggplotly(p2)
    
  })
  
  
  ##### RATES PER AGE GROUP #####
  
  ## boxplot data prep ##
  boxplot.rate_reactive = reactive({
        age.df %>%
          filter(., event_type == input$df_eventtype2) %>%
          filter(., site == input$df_site2) %>%
          filter(., race == input$df_race3) %>% 
          filter(., sex == input$df_sex3)  
  })
  
  ## divergebar chart data prep ##
  divergebar.rate_reactive = reactive({
        age.df %>% 
          mutate(., rate_z = round((rate-mean(rate))/sd(rate)),2) %>% 
          filter(., event_type == input$df_eventtype2) %>%
          filter(., site == input$df_site2) %>%
          filter(., race == input$df_race3) %>% 
          filter(., sex == input$df_sex3) %>% 
          group_by(., age) %>% 
          summarise(., avg.rate = mean(rate))
  })
  
  ## divergebar rate_z calculation ##
  divergebar.rate_z_reactive = reactive({
    divergebar.rate_reactive() %>% 
      mutate(., rate_z = round((avg.rate-(mean(avg.rate)))/sd(avg.rate),2)) %>% 
      mutate(., rate_type = ifelse(rate_z <0, "Below Average", "Above Average"))
  })

  
  ##reactive event_type2 input (chart)##
  df_eventtype2.choice = reactive({
    as.character(input$df_eventtype2)
  })
  
  ## reactive site2 input (chart) ##
  df_site2.choice = reactive({
    as.character(input$df_site2)
  })
  
  ## reactive race input (chart)
  df_race3.choice = reactive({
    as.numeric(input$df_race3)
  })
  
  ##reactive sex3 input (chart)##
  df_sex3.choice = reactive({
    as.character(input$df_sex3)
  })
  
  ## reactive boxplot chart colors (chart) ##
  colors.choice1 = reactive({
    if (input$df_eventtype2 == "Incidence"){
      "steelblue"
    }
    else {
      "plum"
    }
  })
  
  ## reactive divergebar chart colors (chart)
  colors2.choice = reactive({
    if (input$df_eventtype2 == "Incidence"){
      c("Above Average" = "#f8766d", "Below Average" = "#00ba38")
    }
    else {
      c("Above Average" = "#C70039", "Below Average" = "#1A7305")
    }
  })
  
  
  output$boxplot.title = renderText({
    paste0("Cancer", " ", df_eventtype2.choice(), " ", "Rate Distribution per Age Group (1999 - 2015)")
  })
  
  output$divergebar.title = renderText({
    paste0("Normalized  Cancer", " ",df_eventtype2.choice()," ", "Rate (1999-2015)")
  })
  
  
  
  ## Trend in rate bar chart ##
  output$boxplot.rate = renderPlotly({

    theme_set(theme_classic())

    g1= ggplot(boxplot.rate_reactive(), aes(age,rate))
     g1 + geom_boxplot(varwidth=T, fill=colors.choice1()) +
      labs(
           x="Age Group",
           y="Rate per 100,000 People") +
      theme(axis.text = element_text(size = 9),
            axis.title = element_text(size = 12))
  })
  
  
  ##Rates per Age Group ##
  output$divergebar.rate = renderPlotly({
    g2 = ggplot( divergebar.rate_z_reactive(), aes(x = age, y = rate_z, label = rate_z)) 
      
        g2 +geom_point(stat='identity', aes(fill= rate_type), size=6)  +
        geom_segment(aes(y = 0, 
                         x = age, 
                         yend = rate_z, 
                         xend = age), 
                     color = "black") +
        geom_text(color="white", size=2) +
        coord_flip() +
        theme(axis.title = element_text(size=12),
              panel.grid.major.x = element_line(colour ="grey"))+
        labs( x = "Age Group",
              y = "Rate_Z") +
        scale_fill_manual(name = "Cancer Rate",
                          labels = c("Above Average", "Below Average"),
                          values = colors2.choice() 
            )
  })
  
  
  ##### TREND IN CANCER RATES #####
 
  output$siterace.title = renderText({
    paste0("Cancer", " ", df_eventtype3.choice()," Percentage by Age Group",":", df_sex3.choice()," ","(",df_year3.choice(),")")
  })
  
  output$agerace.title = renderText({
    paste0("Cancer ",df_eventtype3.choice()," Rate vs. Cancer Type: ", df_race4.choice(),", ", df_sex3.choice(), " (",df_year3.choice(),")")
  })
  
  
  # reactive event_type input (chart)
  df_eventtype3.choice = reactive({
    as.character(input$df_eventtype3)
  })
  
  # reactive race input (chart)
  df_race4.choice = reactive({
    as.character(input$df_race4)
  })
  
  # reactive year input (chart)
  df_year3.choice = reactive({
    as.numeric(input$df_year3)
  })
  

  ##reactive sex3 input (chart)##
  df_sex3.choice = reactive({
    as.character(input$df_sex4)
  })
  
  
  ## Cancer Type, Gender, and Race chart data prep (1) ##
  type.rate_reactive = reactive({
    age.df %>%
      filter(., event_type == input$df_eventtype3) %>%
      filter(., sex == input$df_sex4) %>% 
      filter(., year == input$df_year3) %>% 
      filter(., race == input$df_race4) %>% 
      mutate(., age_at_diagnosis = case_when(age == "<1" ~ "0-14",
                                             age == "1-4" ~ "0-14",
                                             age == "5-9" ~ "0-14",
                                             age == "10-14" ~ "0-14", 
                                             age == "15-19" ~ "15-29",
                                             age == "20-24" ~ "15-29",
                                             age == "25-29" ~ "15-29", 
                                             age == "30-34" ~ "30-49",
                                             age == "35-39" ~ "30-49",
                                             age == "40-44" ~ "30-49",
                                             age == "45-49" ~ "30-49",
                                             age == "50-54" ~ "50-64", 
                                             age == "55-59" ~ "50-64",
                                             age == "60-64" ~ "50-64",
                                             age == "65-69" ~ "65+",
                                             age == "70-74" ~ "65+",
                                             age == "75-79" ~ "65+",
                                             age == "80-84" ~ "65+",
                                             age == "85+" ~ "65+")) %>% 
      group_by(., site) %>% 
      mutate(., countT = sum(count)) %>% 
      group_by(., age_at_diagnosis, add = TRUE) %>% 
      mutate(., percentage = 100*(count/countT))
    
  })
  
  ## Cancer Type, Gender, and Race chart data prep (2) ##
  race_sex_reactive = reactive({
    age.df %>%
      filter(., site != "All Cancer Sites Combined") %>% 
      filter(., event_type == input$df_eventtype3) %>% 
      filter(., race == input$df_race4 ) %>% 
      filter(., sex == input$df_sex4) %>% 
      filter(., year == input$df_year3) %>% 
      group_by(., site) %>% 
      summarise(., rate1 = mean(rate))
  })
  
  
  
  ## event_type, race, sex, year, and site chart ##
  output$site_race = renderPlot({
    g5 = ggplot(data = type.rate_reactive(), aes(x = site, y = percentage, fill = age_at_diagnosis))
    g5 + geom_bar(stat = 'identity', position = 'fill', width = 0.6) +
      theme(legend.position = "top",
            axis.text.x = element_text(angle=0, size = 12, vjust=0.6),
            axis.text.y = element_text(size = 12),
            axis.title.x = element_text(face="bold", size = 16),
            axis.title.y = element_text(face="bold", size = 16)) +
      labs(x= "Cancer Type", fill ="Age Group (years)") +
      coord_flip() +
      scale_fill_manual(values = c("#031D88", "#F9EA02", "#9F0B0B", "#456D5B", "#FF8033")) +
      scale_y_continuous(labels = scales::percent_format())
  })

  ## event_type, race, sex, year, age group chart
  output$sex_race = renderPlot({
    
    g6 = ggplot(race_sex_reactive(), aes(x = reorder(site, rate1), y = rate1, fill = site))
    g6 + geom_bar(stat = 'identity') + coord_flip() +
      xlab("Cancer Type") + ylab("Rate per 100,000 People") + 
      theme(legend.position = 'none',
            axis.text.x = element_text(angle=0, size = 12, vjust=0.6),
            axis.text.y = element_text(size = 12),
            axis.title.x = element_text(face="bold", size = 16),
            axis.title.y = element_text(face="bold", size = 16)
            )
      
    
    
  })
    
 
  
  
  
  
  
  
  
    

  
})
