
library(rgdal)
library(ggmap)
library(ggplot2)
library(scales)
library(shiny)
library(ggthemes)
library(grid)
library(Rmisc)
library(plotly)

shinyServer(function(input, output){
  
  #### home page tabPanel

  #### Metrics efficiency tabPanel
  output$ecdf <- renderPlot({
    # Create the efficacy sample
    dpt_choice = input$department
    if (!is.null(dpt_choice)){
      r = input$r
      adj_efficacy = qos2014$CmO/(r*qos2014$TmO)
      n <- 0
      for(i in dpt_choice){
        n <- n + 1
        ind = which(qos2014$SUBJECT == i)    
        Fn = ecdf(adj_efficacy[ind])
        #par(bg="#add8e6")
        if (n==1)
          plot(Fn,xlim = c(0,2),ylim = c(0,1),col = which(dpt==i),lwd=2,
               main="",col.main ="#222222",font.main=3,cex.main=3,
               ylab = "Proportion of Jobs Completed",xlab = "Proportion of Target Time Used",col.lab="#222222",font.lab=2,cex.lab=1.2,
               col.axis='#222222',font.axis=1,cex.axis=1.3
          )        
        else
          lines(ecdf(adj_efficacy[ind]),xlim = c(0,2),ylim = c(0,1),col = which(dpt==i),lwd=2)
      }
      abline(v = 1)
      legend("bottomright",dpt_choice,col=which(dpt %in% dpt_choice),
             cex = 1,
             lty = rep(1,1))
    }
    abline(v = 1,lty = 2)
    
  })
  
  output$ecdf_dpt <- renderPlot({
    dpt_choice <- input$department
    if (!is.null(dpt_choice)){
      r <- c(input$r1,input$r2,input$r3,input$r4,input$r5,input$r6,input$r7,input$r8)
      adj_efficacy_dpt <- qos2014$CmO/qos2014$TmO
      n <- 0
      for(i in dpt_choice){
        n <- n + 1
        nr <- which(qos2014$SUBJECT==i)
        adj_efficacy_dpt[nr] <- adj_efficacy_dpt[nr] / r[which(dpt==i)]
        fecdf <- ecdf(adj_efficacy_dpt[nr])
        #par(bg="#add8e6")
        if (n ==1) {
          plot(fecdf,xlim=c(0,2),ylim =c(0,1),col=which(dpt==i),lwd=2,
               main="",col.main ="#222222",font.main=3,cex.main=3, # Main title
               ylab="Proportion of Jobs Completed",xlab="Proportion of Target Time Used",col.lab="#222222",font.lab=2,cex.lab=1.2, # Label
               col.axis='#222222',font.axis=1,cex.axis=1.3 # Axis
          )
          
        } else {
          lines(fecdf,xlim = c(0,2),ylim = c(0,1),col = which(dpt==i),lwd=2)
        }
      }
      abline(v = 1)
      
      legend("bottomright",dpt_choice,col=which(dpt %in% dpt_choice),
             cex = 1,
             lty = rep(1,1))
    }
    abline(v=1,lty=2)
  })
  
  #### Bubble Charts tabPanel
  output$B_department <- renderPlotly({
    p <- plot_ly(qos.depar, x = time_consume, y = efficiency, mode = "markers",
                 group = department,size = Requests_Volume, sizemode = "area", 
                 text = sprintf("Duration: %s, Efficiency: %s, Neighborhood: %s", time_consume, efficiency, neighborhood),
                 hoverinfo = "x+text")
    layout(p, title = "Quality of 311 Service by Department", xaxis = list(title = "Duration (Hours)"),yaxis = list(title = "Efficiency (%)"))
  })
  output$B_dpt_quality <- renderPlotly({
    qos.pwd <- subset(qos.depar, qos.depar$department == input$`select one department`)
    p.pwd <- plot_ly(qos.pwd, x = time_consume, y = efficiency, mode = "markers",
                     size = Requests_Volume, color = On_time_Rate, colors = "YlOrRd",text = sprintf("Duration: %s, Efficiency: %s, On time Rate: %s, Neighborhood: %s", time_consume, efficiency, On_time_Rate, neighborhood),
                     hoverinfo = "x+text")
    layout(p.pwd, title = paste("Quality of 311 Service in ", input$`select one department`), xaxis = list(title = "Duration (Hours)"),yaxis = list(title = "Efficiency (%)"))
  })
  output$B_dpt_density <- renderPlotly({
    qos.pwd <- subset(qos.depar, qos.depar$department == input$`select one department`)
    p.pwd1 <- plot_ly(qos.pwd, x = density, y = time_consume, mode = "markers",
                      size = Requests_Volume, color = On_time_Rate, colors = "YlOrRd", text = sprintf("Density: %s, Duration: %s, On time Rate: %s, Neighborhood: %s", density, time_consume, On_time_Rate, neighborhood),
                      hoverinfo = "x+text")
    layout(p.pwd1, title = paste("Association between Quality of Service and Density in ", input$`select one department`), xaxis = list(title = "Density (Pop/SqMile)"), yaxis = list(title = "Duration (Hours)"))
  })
  output$B_dpt_income <- renderPlotly({
    qos.pwd <- subset(qos.depar, qos.depar$department == input$`select one department`)
    p.pwd2 <- plot_ly(qos.pwd,x = median_income, y = time_consume, mode = "markers",
                      size = Requests_Volume, color = On_time_Rate, colors = "YlOrRd", text = sprintf("Median Income: %s, Duration: %s, On time Rate: %s, Neighborhood: %s", median_income, time_consume, On_time_Rate, neighborhood),
                      hoverinfo = "x+text")
    layout(p.pwd2, title = paste("Association between Quality of Service and Income in ", input$`select one department`), xaxis = list(title = "Median Income ($)"), yaxis = list(title = "Duration (Hours)"))
  })

  #### map tabPanel
  
  nei <- readOGR(".", "Bos_neighborhoods_new")
  neighborhoods <- spTransform(nei, CRS("+proj=longlat +datum=WGS84"))
  neighborhoods <- fortify(neighborhoods)
  name.nei <- nei$Name
  n <- length(name.nei)
  data.nei <- data.frame(Name = name.nei, id = 0:(n-1))
  merge.nei <- merge(neighborhoods, data.nei, by = "id", all.x = TRUE)
  final.nei <- merge.nei[order(merge.nei$order),]
  levels(final.nei$Name) <- c(levels(final.nei$Name), "Back Bay")
  final.nei$Name[final.nei$Name=="Bay Village"] <- "Back Bay"
  levels(final.nei$Name) <- c(levels(final.nei$Name), "Beacon Hill")
  final.nei$Name[final.nei$Name=="West End"] <- "Beacon Hill"
  levels(final.nei$Name) <- c(levels(final.nei$Name), "South Boston / South Boston Waterfront")
  final.nei$Name[final.nei$Name=="South Boston"] <- "South Boston / South Boston Waterfront"
  final.nei$Name[final.nei$Name=="South Boston Waterfront"] <- "South Boston / South Boston Waterfront"
  levels(final.nei$Name) <- c(levels(final.nei$Name), "Downtown / Financial District")
  final.nei$Name[final.nei$Name=="Downtown"] <- "Downtown / Financial District"
  final.nei$Name[final.nei$Name=="Chinatown"] <- "Downtown / Financial District"
  final.nei$Name[final.nei$Name=="North End"] <- "Downtown / Financial District"
  final.nei$Name[final.nei$Name=="Leather District"] <- "Downtown / Financial District"
  levels(final.nei$Name) <- c(levels(final.nei$Name), "Fenway / Kenmore / Audubon Circle / Longwood")
  final.nei$Name[final.nei$Name=="Fenway"] <- "Fenway / Kenmore / Audubon Circle / Longwood"
  final.nei$Name[final.nei$Name=="Longwood Medical Area"] <- "Fenway / Kenmore / Audubon Circle / Longwood"
  levels(final.nei$Name) <- c(levels(final.nei$Name), "Allston / Brighton")
  final.nei$Name[final.nei$Name=="Allston"] <- "Allston / Brighton"
  final.nei$Name[final.nei$Name=="Brighton"] <- "Allston / Brighton"
  
  pop <- read.csv("pop.csv")
  colnames(pop) <- c("Name", "Population", "Pop/SqMile", "Dollars")
  
  user1 <- reactive(pop[, c(1, as.numeric(input$var1))])
  
  output$map <- renderPlot({
    new <- user1()
    item <- colnames(new)[2]
    colnames(new) <- c("Name", "Legend")
    hope <- merge(final.nei, new, by = "Name", all.x = TRUE)
    hope <- hope[order(hope$order),]
    hope <- na.omit(hope)
    names.neigh <- aggregate(cbind(long, lat) ~ Name, data=hope, FUN=function(x) mean(range(x)))
    
    ggplot() +theme_update(plot.background = element_rect(fill="lightblue"))+
      geom_polygon(data = hope,
                   aes(x = long, y = lat, group = group, fill = Legend),
                   color = "white", size = 0.25) +
      coord_map() +
      scale_fill_gradient(name=item, low="ghostwhite", high="steelblue")+
      theme_nothing(legend = TRUE)+
      geom_text(data=names.neigh, aes(long, lat, label = Name), size=3)
  })
  
  duration_dept <- read.csv("duration.csv")
  efficiency_dept <- read.csv("efficiency.csv")
  efficiency_dept <- cbind(efficiency_dept[,1], efficiency_dept[, 2:9]*100)
  ontime_rate_dept <- read.csv("ontime_rate.csv")
  request_volume_dept <- read.csv("request_volume.csv")
  
  datasetInput <- reactive({
    switch(input$dataset,
           "Duration" = duration_dept,
           "Efficiency" = efficiency_dept,
           "Ontime Rate" = ontime_rate_dept,
           "Requests Volume" = request_volume_dept)
  })
  
  output$quality <- renderPlot({
    
    user_new <- datasetInput()
    user2 <- reactive(user_new[, c(1, as.numeric(input$var2))])
    new2 <- user2()
    item2 <- input$dataset
    if(item2=="Duration"){
      item2 <- "Hours"
    }
    if(item2=="Efficiency"){
      item2 <- "Efficiency (%)"
    }
    colnames(new2) <- c("Name", "Legend")
    hope2 <- merge(final.nei, new2, by = "Name", all.x = TRUE)
    hope2 <- hope2[order(hope2$order),]
    hope2 <- na.omit(hope2)
    names.neigh2 <- aggregate(cbind(long, lat) ~ Name, data=hope2, FUN=function(x) mean(range(x)))
    
    ggplot() +
      geom_polygon(data = hope2,
                   aes(x = long, y = lat, group = group, fill = Legend),
                   color = "white", size = 0.25) +
      coord_map() +
      scale_fill_gradient(name=item2, low="ghostwhite", high="darkgreen")+
      theme_nothing(legend = TRUE)+
      geom_text(data=names.neigh2, aes(long, lat, label = Name), size=3)
  })
  
  duration_PWD <- read.csv("duration_PWD.csv")
  duration_IS <- read.csv("duration_IS.csv")
  duration_TR <- read.csv("duration_TR.csv")
  
  efficiency_PWD <- read.csv("efficiency_PWD.csv")
  efficiency_IS <- read.csv("efficiency_IS.csv")
  efficiency_TR <- read.csv("efficiency_TR.csv")
  
  ontime_rate_PWD <- read.csv("ontime_rate_PWD.csv")
  ontime_rate_IS <- read.csv("ontime_rate_IS.csv")
  ontime_rate_TR <- read.csv("ontime_rate_TR.csv")
  
  request_volume_PWD <- read.csv("request_volume_PWD.csv")
  request_volume_IS <- read.csv("request_volume_IS.csv")
  request_volume_TR <- read.csv("request_volume_TR.csv")
  
  duration <- merge(duration_PWD, duration_IS, by = "name")
  duration <- merge(duration, duration_TR, by = "name")
  
  efficiency <- merge(efficiency_PWD, efficiency_IS, by = "name")
  efficiency <- merge(efficiency, efficiency_TR, by = "name")
  efficiency <- cbind(efficiency[,1], efficiency[, 2:12]*100)
  colnames(efficiency)[1] <- "name"
  
  ontime_rate <- merge(ontime_rate_PWD, ontime_rate_IS, by = "name")
  ontime_rate <- merge(ontime_rate, ontime_rate_TR, by = "name")
  
  request_volume <- merge(request_volume_PWD, request_volume_IS, by = "name")
  request_volume <- merge(request_volume, request_volume_TR, by = "name")
  
  all <- rbind(duration, efficiency)
  all <- rbind(all, ontime_rate)
  all <- rbind(all, request_volume)
  
  
  output$type <- renderPlot({
    user_ty <- reactive(input$ty)
    user_qos <- reactive(input$qos)
    t <- user_ty()
    q <- user_qos()
    if(q=="1"){
      r <- c(1:17)
      item3 <- "Hours"
    }
    if(q=="2"){
      r <- c(18:34)
      item3 <- "Efficiency (%)"
    }
    if(q=="3"){
      r <- c(35:51)
      item3 <- "Ontime Rate"
    }
    if(q=="4"){
      r <- c(52:68)
      item3 <- "Requests Volume"
    }
    new3 <- all[r, c(1, as.numeric(t))]
    
    colnames(new3) <- c("Name", "Legend")
    hope3 <- merge(final.nei, new3, by = "Name", all.x = TRUE)
    hope3 <- hope3[order(hope3$order),]
    hope3 <- na.omit(hope3)
    names.neigh3 <- aggregate(cbind(long, lat) ~ Name, data=hope3, FUN=function(x) mean(range(x)))
    
    ggplot() +
      geom_polygon(data = hope3,
                   aes(x = long, y = lat, group = group, fill = Legend),
                   color = "white", size = 0.25) +
      coord_map() +
      scale_fill_gradient(name=item3, low="ghostwhite", high="darkorange")+
      theme_nothing(legend = TRUE)+
      geom_text(data=names.neigh3, aes(long, lat, label = Name), size=3)
  })
  
  #### Interactive Scatter Plot tabPanel
  output$scatter_time_consume <- renderPlotly({
    qplot(Neighborhood, Duration, data = metric.all, color = Department)
    p1 <- ggplotly()
    layout(p1, yaxis = list(title = "Duration (Hours)"))
  })
  output$scatter_efficiency <- renderPlotly({
    qplot(Neighborhood, Efficiency, data = metric.all, color = Department)
    p2 <- ggplotly()
    layout(p2, yaxis = list(title = "Efficiency (%)"))
  })
  output$scatter_ontime_rate <- renderPlotly({
    qplot(Neighborhood, ontime_rate, data = metric.all, color = Department)
    p3 <- ggplotly()
    layout(p3, yaxis = list(title = "On-time Rate"))
  })
  output$scatter_requests_number <- renderPlotly({
    qplot(Neighborhood, request_volume, data = metric.all, color = Department)
    p4 <- ggplotly()
    layout(p4, yaxis = list(title = "Requests Volume"))
  })
})