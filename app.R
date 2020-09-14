



#update data with automated script
source("jhu_data1.R")




library(shiny)
library(shinydashboard)
library(shinydashboardPlus)

library(ggplot2)
library(reshape2)
library(gridExtra)
library(grid)
library(plotly)
library(cpm)
# shinydashboardPlusGallery()
# 
# ui <- fluidPage(
#   h1("Covid Tracker"),
#   sliderInput(inputId = "num",
#               label= "Choose a number",
#               value= 25, min=1,max=100),
#   plotOutput("hist")
#   
# )
# 
# server <- function(input, output){
#   output$hist <- renderPlot({
#     title <- paste(input$num, "random normal values")
#     hist(rnorm(input$num), main=title)
#     })
# }
# 
# shinyApp(ui=ui, server=server)

# rsconnect::setAccountInfo(name='jovitagudan',
#                           token='B567173B782DB3FADC6CD8ED0AFD3429',
#                           secret='a39ncsRGu3YOfvsoFiwBTy3Z1JpVJtQ/AAPtV+io')
# 
# library(rsconnect)


countries<- rownames(normalized)
min_date <- as.Date(min(colnames(normalized)))
current_date <- as.Date(max(colnames(normalized)))

#Estimation of negative exponential model
neg_expo_model <- function(dependent_var, start_id, end_id){
  
  y <- dependent_var[start_id:end_id]
  x <- 1:length(y) 
  theta.0 <- max(y) * 1.1
  model.0 <- lm(log(- y + theta.0) ~ x)
  alpha.0 <- -exp(coef(model.0)[1])
  beta.0 <- coef(model.0)[2]
  
  start <- list(alpha = alpha.0, beta = beta.0, theta = theta.0)
  
  # Fit the model
  model <- nls(y ~ alpha * exp(beta * x) + theta , start = start)
  return(model)
}


### MAP FUNTIONS ###

t <- list(
  family = "sans serif",
  size = 14,
  color = toRGB("grey50"))

cumulative_plot  = function(var_to_plot, text) {
  
  fig <- plot_ly(height = 500, width = 800) 
  
  fig <- fig %>% add_trace(x = colnames(var_to_plot), 
                           y = t(var_to_plot),
                           name = paste(text),
                           type = 'scatter',
                           mode = "lines+markers",
                           line = list(
                             color = '#7F7F7F'
                           ),
                           marker= list(
                             color = '#7F7F7F'
                           ),
                           hovertemplate = paste('<b>',text,'</b>: %{y:.0f}',
                                                 '<br><b>Date</b>: %{x}<br>',
                                                 "<extra></extra>"))
  
  fig <- fig %>% layout(title = '',
                        xaxis = list(title = 'Date', 
                                     titlefont = list(size = 12),
                                     type = 'date',
                                     tickformat = "%Y-%m-%d"),
                        yaxis=list(title = paste(text),
                                   titlefont = list(size = 12)),
                        margin = list(l = 50, r = 50, t = 60, b = 150)
  )
  fig
}



prediction_plot  = function(var_to_plot, text) {
  
  
  vline <- function(x = 0, color = "red") {
    list(
      type = "line", 
      y0 = 0, 
      y1 = 1, 
      yref = "paper",
      x0 = x, 
      x1 = x, 
      line = list(color = color)
    )
  }
  
  
  fig <- plot_ly(height = 500, width = 800) 
  
  fig <- fig %>% add_trace(x = var_to_plot$Date, 
                           y = var_to_plot$log_data,
                           name = paste(text),
                           type = 'scatter',
                           mode = "lines+markers",
                           line = list(
                             color = '#7F7F7F'
                           ),
                           marker= list(
                             color = '#7F7F7F'
                           ),
                           hovertemplate = paste('<b>',text,'</b>: %{y:.0f}',
                                                 '<br><b>Date</b>: %{x}<br>',
                                                 "<extra></extra>"))
  fig <- fig %>% add_trace(x = var_to_plot$Date, 
                           y = var_to_plot$Predicted,
                           name = paste("Predicted"),
                           type = 'scatter',
                           mode = "lines+markers",
                           line = list(
                             color = '#CC0000'
                           ),
                           marker= list(
                             color = '#CC0000'
                           ),
                           hovertemplate = paste('<b>Predicted</b>: %{y:.0f}',
                                                 '<br><b>Date</b>: %{x}<br>',
                                                 "<extra></extra>"))
  
  tekstas <- paste("Alpha=",unique(var_to_plot$alpha)[2],"\n Beta=",unique(var_to_plot$beta)[2],
                   "\n Theta=",unique(var_to_plot$theta)[2])
  
  x_cord <- c(1.15)
  y_cord <- c(-0.3)
  
  
  fig <- fig %>% layout(title = '',
                        shapes = list(vline(as.Date(unique(var_to_plot$T_0)[2], origin="1970-01-01")), vline(as.Date(unique(var_to_plot$T_1)[2], origin="1970-01-01"))),
                        xaxis = list(title = 'Date', 
                                     titlefont = list(size = 12),
                                     type = 'date',
                                     tickformat = "%Y-%m-%d"),
                        yaxis=list(title = paste(text),
                                   titlefont = list(size = 12)),
                        margin = list(l = 50, r = 50, t = 60, b = 150),
                        annotations = list(text = tekstas,
                                           font = list(size = 12),
                                           showarrow = FALSE,
                                           xref = 'paper', x = x_cord,
                                           yref = 'paper', y = y_cord,
                                           align="right")
  )
  
  fig <- fig %>% add_text(x = as.Date(unique(var_to_plot$T_0)[2], origin="1970-01-01")+2, 
                          y = max(na.omit(var_to_plot$log_data)),
                          text = ~"T_0",
                          textposition = "right",
                          showlegend = FALSE,
                          name="tekstas",
                          visible=T,
                          hoverinfo="none")
  
  fig <- fig %>% add_text(x = as.Date(unique(var_to_plot$T_1)[2], origin="1970-01-01")+2, 
                          y = max(na.omit(var_to_plot$log_data)),
                          text = ~"T_1",
                          textposition = "right",
                          showlegend = FALSE,
                          name="tekstas",
                          visible=T,
                          hoverinfo="none")
  
  fig
}



ui <- dashboardPage(
  dashboardHeader(title = "Analysis of Covid-19"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Plots", tabName = "plots", icon = icon("chart-line")),
      menuItem("Analysis", tabName = "analysis", icon = icon("cogs"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "plots",
              fluidRow(
                box(
                  selectInput("country", "Select country or region:",  choices = countries),
                  selectInput("outcome", "Outcome:", 
                              choices =  c("Cases per 100,000", 
                                           "Cases (total)", "Deaths per 100,000",
                                           "Deaths (total)", "Recovered (total)")),
                  selectInput("scale", "Scale:", 
                              choices =  c("Original", 
                                           "Natural logarithm")),
                  sliderInput("plot_date",
                              label="Select initial date:",
                              min = as.Date(min_date, "%Y-%m-%d"),
                              max = as.Date(current_date, "%Y-%m-%d"),
                              value=as.Date(min_date))
               
                )
              ),
             
              fluidRow(
                plotlyOutput("plot1", height = 500)
              )
      ),
      
      # Second tab content
      tabItem(tabName = "analysis",
              fluidRow(
                box(
                  selectInput("country2", "Select country or region:",  choices = countries),
                  selectInput("outcome2", "Outcome:", 
                              choices =  c("Cases per 100,000")),
                  selectInput("scale2", "Scale:", 
                              choices =  c("Natural logarithm"))
                  # sliderInput("plot_date2",
                  #             label="Select initial date:",
                  #             min = as.Date(min_date, "%Y-%m-%d"),
                  #             max = as.Date(current_date, "%Y-%m-%d"),
                  #             value=as.Date(min_date))
                  
                )
              ),
              
              fluidRow(
                plotlyOutput("plot2", height = 500)
              )
      )
    )
  )
)

server <- function(input, output) {
  
  

  
 # i_country <- reactive({normalized[which(rownames(normalized) %in% input$country),] })
  
  
  # create dataframe with selected countries
  country_reactive_db = reactive({
    if (input$outcome=="Cases per 100,000") { 
      db = normalized
    } 
    if (input$outcome=="Deaths (total)") { 
      db = deaths
    }
    if (input$outcome=="Recovered (total)") { 
      db = recovered
    }
    if (input$outcome=="Deaths per 100,000") { 
      db = normalized_deaths
    }
    if (input$outcome=="Cases (total)") { 
      db = acc_cases
    }
    if (input$scale=="Natural logarithm") {
      db = log(db)
      for(i in 1:length(rownames(db))){
        db[i,which(db[i,] %in% -Inf)] <- NA
      }
    }
    
    db = db %>% filter(rownames(db) %in% input$country)
    db = db[, which(as.Date(colnames(db)) >= as.Date(input$plot_date))]
    
  
  })
  
  country_reactive_db2 = reactive({
    if (input$outcome2=="Cases per 100,000") { 
      db2 = normalized
    } 
    if (input$scale2=="Natural logarithm") {
      db2 = log(db2)
      for(i in 1:length(rownames(db2))){
        db2[i,which(db2[i,] %in% -Inf)] <- NA
      }
    }
    
    #db2 = db2 %>% filter(rownames(db2) %in% "Lithuania")
    
    
    db2 = db2 %>% filter(rownames(db2) %in% input$country2)
    #db2 = db2[, which(as.Date(colnames(db2)) >= as.Date(input$plot_date2))]
    
    #Detection for the start of exponential growth
    diff_db <- diff(na.omit(t(db2)))
    colnames(diff_db)[1] <- "diff"
    diff_db <- cbind(diff_db, 2:(dim(diff_db)[1]+1))
    colnames(diff_db)[2] <- "t"
    begin_exp <- which(diff_db!=0)[1]
 

    #Detection for the end of exponential growth
    fit_cpm = processStream(diff_db[begin_exp:dim(diff_db)[1],1], cpmType = "ExponentialAdjusted")
    end_exp <- fit_cpm$changePoints[which(fit_cpm$changePoints-begin_exp>40)]
    

    #data table
    log_data <- t(db2)
    colnames(log_data)[1] <- "log_data"
    Date <- as.Date(rownames(t(db2)))
    log_data <- as.data.frame(log_data)
    log_data$Date <- Date
    
    #subset data without NA
    log_data_wna <- na.omit(t(db2))
    log_data_wnaa <- as.data.frame(log_data_wna[1:dim(log_data_wna)[1]])
    colnames(log_data_wnaa)[1] <- "log_data"
    log_data_wnaa$Date <- as.Date(rownames(log_data_wna))
    
    
    #detection of optimal negative exponential model
    rss_all <- matrix(NA,ncol=3, nrow=(length(begin_exp:(begin_exp+5))*length(end_exp)*length(0:10)))
    colnames(rss_all) <- c("Begin","End","RSS")
    count <- 0
    for(j in begin_exp:(begin_exp+5)){
      for(u in end_exp){
        for(k in 0:10){
          count <- count + 1
          model <- neg_expo_model(log_data_wnaa$log_data, j, u+k)
          rss <- sum(residuals(model)^2)
          rss_all[count,1] <- j
          rss_all[count,2] <- u+k
          rss_all[count,3] <- rss
        }
      }
    }
    
    #estimation of optimal negative exponential model
    min_id <- which(rss_all[,3] %in% max(rss_all[,3]))
    min_index <- rss_all[min_id,1]:rss_all[min_id,2]
    model <- neg_expo_model(log_data_wnaa$log_data, rss_all[min_id,1], rss_all[min_id,2])
    
    
    
    
    #summary(model)
    # add fitted curve
   # plot( log_data_wnaa$Date[min_index], log_data_wnaa$log_data[min_index])
    x <- 1:length(log_data_wnaa$log_data[min_index])
    #lines(log_data_wnaa$Date[min_index], predict(model, list(x = x)), col = 'skyblue', lwd = 3)
    
    #save predicted values
    predicted_values <- as.data.frame(matrix(NA, ncol=2, nrow=length(log_data_wnaa$Date[min_index])))
    predicted_values[,1] <- log_data_wnaa$Date[min_index]
    predicted_values[,2] <- predict(model, list(x = x))
    colnames(predicted_values) <- c("Date","Predicted")
    
    
    
    
    place_to_write <- which(log_data$Date %in% log_data_wnaa$Date[min_index])
    result <- merge(log_data,predicted_values,by=c("Date"),all.x=TRUE)
    result$T_0 <- NA
    result$T_0[place_to_write] <- as.Date(log_data_wnaa$Date[min_index][1]) 
    result$T_1 <- NA
    result$T_1[place_to_write] <- as.Date(log_data_wnaa$Date[min_index][length(log_data_wnaa$Date[min_index])])
    result$alpha <- NA
    result$alpha[place_to_write] <- coef(model)[1]
    result$beta <- NA
    result$beta[place_to_write] <- coef(model)[2]
    result$theta <- NA
    result$theta[place_to_write] <- coef(model)[3]
    result$type <- NA
    result$type[place_to_write] <- "Neg_exponential"
    return(result)
  })
  #create plot 
  output$plot1 <- renderPlotly({
   
    cumulative_plot(country_reactive_db(), input$outcome)
    
  })
  
  output$plot2 <- renderPlotly({
    
    prediction_plot(country_reactive_db2(), input$outcome2)
    
  })
  
  
  
  
}

shinyApp(ui, server)

