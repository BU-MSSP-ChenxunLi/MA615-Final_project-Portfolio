library(tidyverse)
library(lubridate)
library(bizdays)
library(tidyquant)
library(shiny)
library(scales)
library(data.table)
library(dplyr)

stock <- c("MSFT","GOOG","AMZN")
#use 'tidyquant' to get price data
stock_info <- stock %>% 
  tq_get(get="stock.prices",from = "2020-07-01", to = "2020-12-01")
# Change into English
Sys.setlocale("LC_TIME", "English")



ui <- navbarPage("Stock",
                 tabPanel("Stock Information",
                          br(),
                          sidebarPanel(
                            selectInput(
                              inputId = "from",
                              label = "From:",
                              choices = sort(unique(stock_info$date))
                            ),
                            uiOutput('ui_to'),
                            ),
                          mainPanel(h3('Choose your Date to explore the information!'),
                                    plotOutput('plot1'),
                                    plotOutput('plot2'),
                                    DT::dataTableOutput("table"))
                          ),
                 tabPanel("Portfolio",
                          br(),
                          sidebarPanel(
                            sliderInput("MSFT_bin",
                                        "Please choose your proportion for MSFT:",
                                        min = 0, max=1, value = 0.01
                            ),
                            sliderInput("GOOG_bin",
                                        "Please choose your proportion for GOOG:",
                                        min = 0, max=1, value = 0.01
                            ),
                            sliderInput("AMZN_bin",
                                        "Please choose your proportion for AMZN:",
                                        min = 0, max=1, value = 0.01
                            ),
                            uiOutput("ui_rest")
                            
                          ),
                          mainPanel(h3('Choose your proportion for each stock!'),
                                    
                                    plotOutput('plot3')
                                    )
                        
                          )
                 )




server <- function(input,output){
 
  ##proportion

  output$ui_rest <- renderUI({
    weight <- c(input$MSFT_bin,input$GOOG_bin,input$AMZN_bin)
    textInput("rest",
                 "The proportion left:",
                if(sum(weight)<=1){ value = 1-(input$MSFT_bin+input$GOOG_bin+input$AMZN_bin)}
                else{ value = "Warning: Your total proportion exceeds 1"}
                 )

  })
  
  
  ##to
  output$ui_to <- renderUI({
    selectInput(
      inputId = "to",
      label = "To:",
      selected = "2020-11-30",
      choices = subset(sort(unique(stock_info$date)),sort(unique(stock_info$date))>input$from)
    )
  })
  
  ##plot1
  plot_1=reactive({
    x1 <- filter(stock_info,date>=input$from & date<=input$to)
    
    close_day <- ggplot(x1, aes(x=date,y=close,color=symbol))+
      geom_line()+
      geom_smooth(method = "lm")+
      theme_tq()+
      scale_color_tq()+
      facet_wrap(~ symbol, ncol = 2, scales = "free_y")+
      ggtitle("Close Price Curve for AMZN & GOOG & MSFT")+
      labs(x="Month",y="Close Price")+
      theme(plot.title = element_text(hjust = 0.5, size = 20))
    
    ggpubr::ggarrange(close_day,ncol = 1)
  })
  output$plot1=renderPlot(plot_1())
  
  
  ##table
  output$table <- DT::renderDataTable(DT::datatable({
    data <- filter(stock_info,date>=input$from & date<=input$to)
   
    data[,-1]
  }))
  
  ##plot2
  plot_2=reactive({
    x1 <- filter(stock_info,date>=input$from & date<=input$to)
    stock_return_month <- x1 %>% 
      group_by(symbol) %>%
      tq_transmute(select     = adjusted, 
                   mutate_fun = periodReturn, 
                   period     = "monthly", 
                   col_rename = "month_return")
    
    return_month <- ggplot(stock_return_month, aes(x = date, y = month_return, color = symbol)) + 
      geom_line() +
      theme_tq() +
      scale_color_tq()+
      ggtitle("Return rate Curve for AMZN & GOOG & MSFT")+
      labs(x="Month",y="Return rate")+
      theme(plot.title = element_text(hjust = 0.5, size = 20))
    
    ggpubr::ggarrange(return_month,ncol = 1)
  })
  output$plot2=renderPlot(plot_2())
  ##plot3
  plot_3=reactive({
    money <- 250000
    weight <- c(input$MSFT_bin,input$GOOG_bin,input$AMZN_bin)
    if(sum(weight)<=1){
      stock_return_month <- stock_info %>% 
        group_by(symbol) %>%
        tq_transmute(select     = adjusted, 
                     mutate_fun = periodReturn, 
                     period     = "monthly", 
                     col_rename = "month_return")
      
    portfolio_renturn_month <- stock_return_month %>%
      tq_portfolio(assets_col   = symbol, 
                   returns_col  = month_return, 
                   weights      = weight, 
                   col_rename   = "portfolio_renturn",
                   wealth.index = TRUE) %>%
      mutate(portfolio_renturn = portfolio_renturn * money)
    
    portfolio_value <- ggplot(portfolio_renturn_month,aes(x = date, y = portfolio_renturn)) +
      geom_line(size = 2, color = "skyblue") +
      labs(title = "Portfolio Value",
           x = "Month", y = "Portfolio Value") +
      geom_smooth(method = "loess") +
      geom_text(aes(label = round(portfolio_renturn)),nudge_x=0.1,nudge_y=0.1)+
      theme_tq() +
      scale_color_tq() +
      scale_y_continuous(labels = scales::dollar)+
      theme(plot.title = element_text(hjust = 0.5, size = 20))
    
    ggpubr::ggarrange(portfolio_value,ncol = 1)
    }else
    { 
      df <- data.frame(num1=1,num2=1,zi="Warning: Your total proportion exceeds 1")
      a <- ggplot(df,aes(x=num1,y=num2))+
        geom_bar(stat = "identity",alpha=0)+
        geom_text(aes(label=zi),position = position_stack(.5),size=5,color="red")+
        labs(x = "Month", y = "Portfolio Value")
      ggpubr::ggarrange(a,ncol = 1)
    }
  })
  output$plot3=renderPlot(plot_3())
  
}



shinyApp(ui = ui, server = server)