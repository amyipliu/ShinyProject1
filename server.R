#SERVER

library(shinydashboard)

shinyServer(function(input, output){
  output$plot1 <- renderGvis({
    pgg = cc1 %>%
      group_by(., gender) %>% select(., gender) %>%
      summarise(gender_per = n()/nrow(cc1)*100) %>%
      gvisPieChart(., labelvar = 'gender', 
                   numvar = 'gender_per',
                   options=list(
        width='auto',
        height='400',
        title='Gender Portion',
        legend='none',
        colors="['orange', 'blue']",
        pieSliceText='label',
        pieHole=0.3),
        chartid="doughnut")
  })
  output$gensum = renderGvis({
    cc1 %>%
      group_by(., gender) %>% select(., gender) %>%
      summarise(gender_no = n()) %>%
      gvisBarChart(., xvar = 'gender', yvar = 'gender_no',
                   options = list(title="Population of Card Users by Gender",
                                  titleTextStyle="{color:'dark',fontName:'Courier',fontSize:16}",
                                  hAxes="[{title:'Portion', titleTextStyle: {color: 'dark'}}]",
                                  vAxes="[{title:'Gender', titleTextStyle: {color: 'dark'}}]",
                                  width='auto',
                                  height='400'
      ))
  })
  output$edpie = renderGvis({
    cc1 %>% 
      group_by(., educ) %>% 
      select(., educ) %>%
      filter(., educ %in% c('Graduate', 'Undergraduate', 'High School', 'Others')) %>% 
      summarise(e_sum = n()) %>%
      arrange(., desc(e_sum)) %>% 
      gvisPieChart(., 'educ', 'e_sum', options = list(height='400',
                                                      titleTextStyle="{color:'dark',fontName:'Courier',fontSize:16}"))
  })
  output$edpbar = renderGvis({
    cc1 %>% 
      group_by(., educ) %>% 
      select(., educ) %>%
      filter(., educ %in% c('Graduate', 'Undergraduate', 'High School', 'Others')) %>% 
      summarise(e_sum = n()) %>%
      arrange(., desc(e_sum)) %>%
      gvisBarChart(., 'educ', 'e_sum', list(height='400',
                                               titleTextStyle="{color:'dark',fontName:'Courier',fontSize:16}"
                                           ))
  })
  output$ageppie = renderGvis({
    cc1 %>% 
      group_by(., ager) %>% 
      summarise(aget = n()) %>%
      gvisPieChart('ager', 'aget', list(height='200'))
  })
  output$agepbar = renderPlot({
    cc1 %>% ggplot(aes(x = ager)) + 
      geom_bar(aes(fill = ager)) + 
      xlab('Age Range') +
      scale_fill_discrete(name = 'Age Range')+
      ggtitle('Card Users by Age')+
      ylab('Count')
  })
  output$marppie = renderGvis({
    cc1 %>% 
      group_by(., marr) %>% 
      summarise(Marriage = n()) %>% 
      arrange(., desc(Marriage)) %>%
      gvisPieChart('marr', 'Marriage', list(height='400', 
                                            pieSliceText='label',
                                            pieHole=0.3))
  })
  output$marpbar = renderGvis({
    cc1 %>% 
      group_by(., marr) %>% 
      summarise(Marriage = n()) %>% 
      arrange(., desc(Marriage)) %>%
      gvisBarChart('marr', 'Marriage', list(height='400'))
  })
  output$lbsb = renderPlot({
    boxplot(cc1$limit_bal2)
  })
  output$lbgbx = renderPlot({
    cc1 %>% 
      ggplot() + 
      geom_boxplot(aes( x= gender, y = limit_bal2, fill = gender))+
      xlab('Gender') + ylab('Credit Limit')
  })
  output$test = renderPlot({
    cc1 %>% group_by(., age) %>% 
      select(., age) %>% 
      filter(age > input$slider[1] & age < input$slider[2]) %>%
      ggplot(aes(x=age)) + geom_histogram(aes(fill=as.character(age))) +
      xlab('Age Range') +
      scale_fill_discrete(name = 'Age Range')+
      ggtitle('Card Users by Age')+
      ylab('Count')
  })
  output$edgepop = renderPlotly({
    cc1 %>% 
      group_by(., educ) %>%
      summarise(Male = sum(gender == 'M'), Female = sum(gender == 'F')) %>%
      arrange(., desc(Female)) %>%  
      plot_ly(x = ~educ, y = ~Female, type = 'bar', name = 'Female') %>%
      add_trace(y = ~Male, name = 'Male') %>%
      layout(yaxis = list(title = 'Count'), barmode = 'group')
      })
  output$ed_M = renderPlotly({
    cc1 %>% 
      group_by(., educ) %>%
      summarise(Male = sum(gender == 'M')) %>%
      arrange(., desc(Male)) %>% 
      plot_ly(labels = ~educ, values = ~Male) %>%
      add_pie(hole = 0.3) %>%
      layout(showlegend = T, 
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
  })
  output$ed_F = renderPlotly({
    cc1 %>% 
      group_by(., educ) %>%
      summarise(Female = sum(gender == 'F')) %>%
      arrange(., desc(Female)) %>% 
      plot_ly(labels = ~educ, values = ~Female) %>%
      add_pie(hole = 0.3) %>%
      layout(showlegend = T, 
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
  })
  output$age_gender = renderGvis({
    cc1 %>%
      group_by(., ager) %>%
      summarise(Female = sum(gender == 'F'), Male = sum(gender == 'M')) %>% 
      gvisColumnChart('ager', c('Male', 'Female'), options = list(width = '900',height='400'))
  })
  output$lb_edu = renderPlot({
    cc1 %>% 
      ggplot(aes(x= educ, y = limit_bal2)) +  
      geom_boxplot(aes(fill=educ)) + xlab('Education Level') + 
      ylab('Credit Limit') +
      scale_fill_brewer(palette = 'YlGnBu', name = 'Edu Level')
  })
  output$lb_mar = renderPlot({
    cc1 %>% 
      ggplot(aes(x = marr, y = limit_bal2)) +  
      geom_boxplot(aes(fill=marr)) + xlab('Marital') + 
      ylab('Credit Limit') +
      scale_fill_brewer(palette = 'YlGnBu', name = 'Marital')
  })
  output$table <- DT::renderDataTable({
    datatable(cc1 %>%
              select(., limit_bal2, gender, age, educ, marr, defaultp) %>% 
              rename(., Credit_Limit = limit_bal2, Gender = gender, Age = age, Education = educ, Marital = marr,
                        Default = defaultp)
              , rownames=FALSE, filter = 'top') %>%
      formatStyle(input$selected2, background="teal", fontWeight='bold')
  })
  output$dfge = renderPlotly({
    dm = as.numeric(
      cc1 %>% filter( cc1$gender == "M") %>%
        summarise(n()))
    
    df = as.numeric(cc1 %>% filter( cc1$gender == "F" ) %>%
                      summarise(n()))
        cc1 %>%
      group_by(., defaultp) %>% 
      summarise(., male = round(sum(gender =="M")/dm*100), female = round(sum(gender == 'F')/df*100)) %>%
      filter(., defaultp == "Yes") %>% 
      plot_ly(., x = ~defaultp, y = ~male, type = 'bar', name = 'Male') %>%
      add_trace(., y = ~female, name = 'Female') %>%
      layout(yaxis = list(title = 'Portion'), barmode = 'group', xaxis = list(title = 'Default'))
    
  })
  output$dfedu = renderPlotly({
    sum(cc1$educ == 'Graduate')
    cc1 %>% 
      group_by(., defaultp) %>%
      summarise(., g = round(sum(educ =="Graduate")/10585*100), ud = round(sum(educ == 'Undergraduate')/14030*100),
                h = round(sum(educ == 'High School')/4917*100),o = round(sum(educ %in% c('Others', 'Unknow')/468*100))) %>%
      filter(., defaultp == 'Yes') %>%
      plot_ly(., x = ~defaultp, y = ~g, type = 'bar', name = 'Graduate') %>%
      add_trace(., y = ~ud, name = 'Undergraduate') %>%
      add_trace(., y = ~h, name = 'High School') %>%
      add_trace(., y = ~o, name = 'Others') %>%
      layout(yaxis = list(title = 'Portion'), barmode = 'group', xaxis = list(title = 'Default'))
  })
  output$dfmar = renderPlotly({
    cc1 %>% 
      group_by(., defaultp) %>%
      summarise(., m = round(sum(marr =="Married")/13659*100), s = round(sum(marr == 'Single')/15964*100),
                o = round(sum(marr == 'Others')/377*100)) %>%
      filter(., defaultp == 'Yes') %>% 
      plot_ly(., x = ~defaultp, y = ~m, type = 'bar', name = 'Married') %>%
      add_trace(., y = ~s, name = 'Single') %>%
      add_trace(., y = ~o, name = 'Others') %>%
      layout(yaxis = list(title = 'Portion'), barmode = 'group', xaxis = list(title = 'Default'))
    })
  output$dfage = renderPlot({
    groupt = cc1 %>% group_by(., ager) %>% summarise(., gt = n())
    cc1 %>%
      group_by(., defaultp, ager) %>%
      summarise(., dgt = n()) %>%
      filter(., defaultp == 'Yes') %>% 
      inner_join(., groupt, by = "ager") %>% 
      mutate(agep = dgt/gt *100) %>% 
      ggplot(aes(x=ager, y = agep)) +
      geom_col(aes(fill = ager)) + xlab('Age Range') +
      ylab('Portion') })
  })
