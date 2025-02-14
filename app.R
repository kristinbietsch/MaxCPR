library(rsconnect)
library(shiny)
library(dplyr)
#library(DT)
library(ggplot2)

# just has miscarriage incorporated

countries <- read.csv("data/SpacingLimitingDefault021225.csv")
countries$ideal <- round(countries$ideal,0)

countries$s_neversex <- round(countries$s_neversex,1)
countries$s_sex_fbirth <- round(countries$s_sex_fbirth,1)
countries$s_infecund <- round(countries$s_infecund,1)
countries$s_birth1 <- round(countries$s_birth1,1)
countries$s_birth2 <- round(countries$s_birth2,1)
countries$s_birth3 <- round(countries$s_birth3,1)
countries$s_birth4 <- round(countries$s_birth4,1)
countries$s_birth5 <- round(countries$s_birth5,1)
countries$s_birth6 <- round(countries$s_birth6,1)
countries$s_birth7 <- round(countries$s_birth7,1)
countries$s_birth8 <- round(countries$s_birth8,1)
countries$s_birth9 <- round(countries$s_birth9,1)
countries$ttc <- round(countries$ttc,1)

countries$Notes <- ""
countries$NotesFR <- ""

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  titlePanel(h1("Maximum CPR Model")),
  
  sidebarLayout(
    sidebarPanel( h3("Choose Country") ,
                  selectInput("var", 
                              label = "Choose a Country",
                              choices = unique(countries$Country),
                              selected = ""),
                  h4("Population Distribution"),
                  h5("Women 15-49, distributed by:"),
                  fluidRow( column(10, 
                                   numericInput("s_neversex", 
                                                h5("Percent who have never had sex"), 
                                                value="20"))   ),
                  
                  fluidRow(      column(10, 
                                        numericInput("s_sex_fbirth", 
                                                     h5("Percent who have had sex but not first birth"), 
                                                     value = "20")) ),
                 
                  fluidRow(   column(10, 
                                     numericInput("s_infecund", 
                                                  h5("Percent who are infecund"), 
                                                  value = "20"))),
                  
                  h5("Percent who have had first birth and are fecund, by parity:"),
                  fluidRow(   column(3, 
                                     numericInput("s_birth1", 
                                                  h5("1"), 
                                                  value = "20")),
                              column(3, 
                                     numericInput("s_birth2", 
                                                  h5("2"), 
                                                  value = "20")),
                              column(3, 
                                     numericInput("s_birth3", 
                                                  h5("3"), 
                                                  value = "20"))),
                  fluidRow(   column(3, 
                                     numericInput("s_birth4", 
                                                  h5("4"), 
                                                  value = "20")),
                              column(3, 
                                     numericInput("s_birth5", 
                                                  h5("5"), 
                                                  value = "20")),
                              column(3, 
                                     numericInput("s_birth6", 
                                                  h5("6"), 
                                                  value = "20"))),
                  fluidRow(   column(3, 
                                     numericInput("s_birth7", 
                                                  h5("7"), 
                                                  value = "20")),
                              column(3, 
                                     numericInput("s_birth8", 
                                                  h5("8"), 
                                                  value = "20")),
                              column(3, 
                                     numericInput("s_birth9", 
                                                  h5("9+"), 
                                                  value = "20"))),
                  h4("Note: Must sum to 100%"), textOutput("dist_tot"),
                  plotOutput("plot3")),
    mainPanel(img(src = "logo_150_trans1.png", align="right"),
              h2("Data"),
              fluidRow(
                
                column(3, 
                       sliderInput("ideal", h4("Ideal Number of Children"),
                                   min = 2, max = 9, value = 5)) ,
                column(3, 
                       numericInput("age_fsex", 
                                    h4("Median Age at First Sex"), 
                                    value = "17.7")),
                column(3, 
                       numericInput("age_fbirth", 
                                    h4("Median Age at First Birth"), 
                                    value = "20.2"))  
              ),
              fluidRow(
                
                column(3, 
                       numericInput("ave_bi", 
                                    h4("Average Birth Interval (Months)"), 
                                    value = "36")),
                column(3, 
                       numericInput("ttc", 
                                    h4("Time to Conceive after Discontinuing Method (Months)"), 
                                    value = "5")),
                column(3, 
                       numericInput("ppi", 
                                    h4("Median Post Partum Insusceptibility (Months)"), 
                                    value = "10"))
                
              ),
              fluidRow(
                column(5, 
                       sliderInput("ppfp", h4("Percent of Post Partum Insusceptibile Women Using Family Planning"),
                                   min = 0, max = 100, value = 20)) 
              ),
              
              
              h5("Source Notes:"),
              textOutput("notes"),
              h2("Results"),
              fluidRow(
                column(5, 
                       tableOutput("CPR_dhs_table"),
                       h5("Note: CPR for All Women")
                       ),
                column(6,
                       tableOutput("pop_table"))
                ),
              fluidRow(
                column(7,
                       plotOutput("plot2"))
                
              ),
              fluidRow(
                 uiOutput("tab")
                
              ),
              fluidRow(
                uiOutput("tab2")
                
              ),
              h3("")

              
              )))



# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  
  observeEvent(input$var,{

    updateNumericInput(session,'s_neversex',
                       value=(countries$s_neversex[countries$Country==input$var]))
    updateNumericInput(session,'s_sex_fbirth',
                       value=(countries$s_sex_fbirth[countries$Country==input$var]))
    updateNumericInput(session,'s_infecund',
                       value=(countries$s_infecund[countries$Country==input$var]))
    
    updateNumericInput(session,'s_birth1',
                       value=(countries$s_birth1[countries$Country==input$var]))
    updateNumericInput(session,'s_birth2',
                       value=(countries$s_birth2[countries$Country==input$var]))
    updateNumericInput(session,'s_birth3',
                       value=(countries$s_birth3[countries$Country==input$var]))
    updateNumericInput(session,'s_birth4',
                       value=(countries$s_birth4[countries$Country==input$var]))
    updateNumericInput(session,'s_birth5',
                       value=(countries$s_birth5[countries$Country==input$var]))
    updateNumericInput(session,'s_birth6',
                       value=(countries$s_birth6[countries$Country==input$var]))
    updateNumericInput(session,'s_birth7',
                       value=(countries$s_birth7[countries$Country==input$var]))
    updateNumericInput(session,'s_birth8',
                       value=(countries$s_birth8[countries$Country==input$var]))
    updateNumericInput(session,'s_birth9',
                       value=(countries$s_birth9[countries$Country==input$var]))
    
    updateNumericInput(session,'age_fsex',
                       value=(countries$age_fsex[countries$Country==input$var]))
    updateNumericInput(session,'age_fbirth',
                       value=(countries$age_fbirth[countries$Country==input$var]))
    updateNumericInput(session,'ave_bi',
                       value=(countries$ave_bi[countries$Country==input$var]))
    updateNumericInput(session,'ttc',
                       value=(countries$ttc[countries$Country==input$var]))
    updateNumericInput(session,'ppi',
                       value=(countries$ppi[countries$Country==input$var]))
    updateSliderInput(session,'ideal',
                      value=(countries$ideal[countries$Country==input$var]))
    updateSliderInput(session,'ppfp',
                      value=(countries$ppfp[countries$Country==input$var]))
    
    
  })
  
  url <- a("Click Here", href="http://track20.org/pages/our_work/innovative_tools/maximum_cpr.php")
  output$tab <- renderUI({
    tagList("The Maximum CPR Model was developed by Kristin Bietsch, PhD and Emily Sonneveldt, PhD of the Track20 Project, funded by the Bill and Melinda Gates Foundation.  For more information, please contact kbietsch@avenirhealth.org.  For technical notes and code to calculate default data:", url)
  })
  
  url2 <- a("Cliquez Ici", href="https://track20.shinyapps.io/tpc_maximum")
  output$tab2 <- renderUI({
    tagList("Accéder au Modele de TPC Maximum en Français:", url2)
  })
  
  
  output$dist_tot <- renderText({
    distribution <- round(input$s_neversex +  input$s_sex_fbirth +  input$s_infecund +  input$s_birth1 +  input$s_birth2 +  input$s_birth3 +  input$s_birth4 +  input$s_birth5 +  input$s_birth6 +  input$s_birth7 +  input$s_birth8 +  input$s_birth9)
    paste("Distribution total =", distribution)
  })
  
  ########################################################################################
  
  
  vals <- reactiveValues()
  observe({

    vals$totalpreg <- input$ideal/(1-.1)
    vals$totalterm <- vals$totalpreg -input$ideal 
    
    vals$monthsmisc <- (input$ttc+3)*vals$totalterm
    
    vals$months_per_BI <- vals$monthsmisc*(1/input$ideal)
    
    
    vals$pop_space<- ifelse(input$ideal==1, 0,
                       ifelse(input$ideal==2, input$s_birth1,
                              ifelse(input$ideal==3, input$s_birth1+ input$s_birth2,
                                     ifelse(input$ideal==4, input$s_birth1+ input$s_birth2+ input$s_birth3,
                                            ifelse(input$ideal==5, input$s_birth1+ input$s_birth2+ input$s_birth3+ input$s_birth4,
                                                   ifelse(input$ideal==6, input$s_birth1+ input$s_birth2+ input$s_birth3+ input$s_birth4+ input$s_birth5,
                                                          ifelse(input$ideal==7, input$s_birth1+ input$s_birth2+ input$s_birth3+ input$s_birth4+ input$s_birth5+ input$s_birth6,
                                                                 ifelse(input$ideal==8, input$s_birth1+ input$s_birth2+ input$s_birth3+ input$s_birth4+ input$s_birth5+ input$s_birth6+ input$s_birth7,
                                                                        ifelse(input$ideal==9, input$s_birth1+ input$s_birth2+ input$s_birth3+ input$s_birth4+ input$s_birth5+ input$s_birth6+ input$s_birth7+ input$s_birth8, NA)))))))))
    
    

    
    vals$per_bfb <- ((input$age_fbirth*12)-(input$age_fsex*12)-9-input$ttc-vals$months_per_BI)/((input$age_fbirth*12)-(input$age_fsex*12))
    vals$per_space <- ((input$ave_bi - (input$ppi*((100-input$ppfp)/100))-input$ttc-9-vals$months_per_BI)*(input$ideal-1))/(input$ave_bi*(input$ideal-1))
    vals$cpr_space <- (vals$per_bfb*input$s_sex_fbirth) + (vals$per_space*vals$pop_space)
    
    
    vals$pop_limit<- ifelse(input$ideal==1,  input$s_birth1+ input$s_birth2+ input$s_birth3+ input$s_birth4+ input$s_birth5+ input$s_birth6+ input$s_birth7+ input$s_birth8+ input$s_birth9,
                       ifelse(input$ideal==2,  input$s_birth2+ input$s_birth3+ input$s_birth4+ input$s_birth5+ input$s_birth6+ input$s_birth7+ input$s_birth8+ input$s_birth9,
                              ifelse(input$ideal==3,  input$s_birth3+ input$s_birth4+ input$s_birth5+ input$s_birth6+ input$s_birth7+ input$s_birth8+ input$s_birth9,
                                     ifelse(input$ideal==4, input$s_birth4+ input$s_birth5+ input$s_birth6+ input$s_birth7+ input$s_birth8+ input$s_birth9,
                                            ifelse(input$ideal==5, input$s_birth5+ input$s_birth6+ input$s_birth7+ input$s_birth8+ input$s_birth9,
                                                   ifelse(input$ideal==6, input$s_birth6+ input$s_birth7+ input$s_birth8+ input$s_birth9,
                                                          ifelse(input$ideal==7, input$s_birth7+ input$s_birth8+ input$s_birth9,
                                                                 ifelse(input$ideal==8, input$s_birth8+ input$s_birth9,
                                                                        ifelse(input$ideal==9, input$s_birth9, NA)))))))))
    vals$age_lb <- ((input$age_fbirth*12) + (input$ave_bi*(input$ideal-1)))/12
    
    vals$ualb <- (50- vals$age_lb)*12-(input$ppi*((100-input$ppfp)/100))
    vals$per_limit <- ( vals$ualb)/((50- vals$age_lb)*12)
    
    vals$cpr_limit <-  vals$per_limit *  vals$pop_limit
    
    vals$cpr_total <- vals$cpr_space + vals$cpr_limit
    
    vals$dhs_total <-  round(countries$CPR_all[countries$Country==input$var],1)
    vals$dhs_space <-  round(countries$Spacing_all[countries$Country==input$var],1)
    vals$dhs_limit <-  round(countries$Limiting_all[countries$Country==input$var],1)
    
    vals$pop_total <-  (countries$pop[countries$Country==input$var]/1000)
    
    vals$user_dhs <- (vals$dhs_total/100)*vals$pop_total
    vals$user_cpr <- (vals$cpr_total/100)*vals$pop_total
    
    
    
  })
  
  output$CPR_space <- renderText({
    
    paste("CPR due to Spacing =", round(vals$cpr_space,1))
  })

  output$CPR_limit <- renderText({
    
    paste("CPR due to Limiting =", round(vals$cpr_limit,1))
  })
  
  output$CPR_total <- renderText({
    paste("CPR =", round(vals$cpr_total,1))
  })
  
  
  output$notes <- renderText({ 
    paste("Default data from DHS ", countries$Source[countries$Country==input$var], countries$Notes[countries$Country==input$var], sep="")
    
  })
  
  output$CPR_dhs <- renderText({ 
    paste("CPR from DHS= ",  countries$CPR_all[countries$Country==input$var])
    
  })
  output$CPR_dhs_space <- renderText({ 
    paste("CPR from DHS due to Spacing= ",  countries$Spacing_all[countries$Country==input$var])
    
  })
  output$CPR_dhs_limit <- renderText({ 
    paste("CPR from DHS due to Limiting= ",  countries$Limiting_all[countries$Country==input$var])
    
  })
  
  output$CPR_dhs_table <- renderTable({ 
    
    
    labels <- c("Total CPR", "CPR due to Limiting", "CPR due to Spacing")
    dhs <- c(vals$dhs_total, vals$dhs_limit, vals$dhs_space)
    model <- c(vals$cpr_total, vals$cpr_limit, vals$cpr_space)
    datatable <- data.frame(labels, dhs, model)
    
    datatable %>%
      rename("Variable" = labels, "Most Recent DHS" = dhs, "Custom Model Output" = model) 
    
  }, digits=1)
  
  output$plot2<-renderPlot({

    labels <- c( "Spacing", "Limiting","Spacing", "Limiting")
    group <- c("Most Recent DHS", "Most Recent DHS", "Custom Model Output", "Custom Model Output" )
    data <- c(vals$dhs_space, vals$dhs_limit, round(vals$cpr_space,1), round(vals$cpr_limit,1))
    datatable <- data.frame(labels, group, data)

    ggplot(datatable, aes(x=group,y=data, fill=labels, label=data))+
      geom_bar(stat="identity") +
      geom_text(size = 5, position = position_stack(vjust = 0.5))+
      scale_x_discrete(limits=c("Most Recent DHS","Custom Model Output"))+
      scale_fill_manual(values=c('#3892C6','#8DC645'))  +
      labs( title="Maximum CPR Based On Ideal Number of Children", y="Percent of Women", x="", fill="CPR due to:") +
      theme_bw() +
      theme(axis.line=element_blank(),
            axis.ticks = element_blank(),
            axis.text=element_text(size = 15),
            legend.text=element_text(size = 15),
            legend.title=element_text(size = 15),
            axis.title=element_text(size = 15),
            plot.title = element_text(hjust = 0.5, size=15, face="bold"),
            panel.background=element_blank(),panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),plot.background=element_blank(),
            legend.position = "bottom")
    
    
    },height = 300,width = 400)

  output$pop_table <- renderTable({ 

    pop_total <- prettyNum(round(vals$pop_total,0), big.mark=",")
    user_dhs <- prettyNum(round(vals$user_dhs), big.mark=",")
    user_cpr <- prettyNum(round(vals$user_cpr), big.mark=",")
    
    labels <- c("Female Population\n(15-49)", "Estimated Users\n(DHS CPR)", "Maximum Users\n(Model CPR)")
    model <- c(pop_total, user_dhs, user_cpr)
    poptable <- data.frame(labels,  model)
    
    poptable %>%
      rename("Population" = labels,  "Number of Women (in '000s)" = model) 
    
  }, digits=0)

  output$plot3<-renderPlot({
    labels <- c( "Never Sex", "Sex,Parity 0","1", "2", "3", "4",  "5", "6", "7", "8", "9+","Infecund")
    row <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 )
    data <- c(input$s_neversex,
              input$s_sex_fbirth,
              input$s_birth1,
              input$s_birth2,
              input$s_birth3,
              input$s_birth4,
              input$s_birth5,
              input$s_birth6,
              input$s_birth7,
              input$s_birth8,
              input$s_birth9,
              input$s_infecund)
    datatable <- data.frame(labels, row, data)
    
    datatable$labels <- ordered(datatable$labels, levels = c("Infecund",  "9+",  "8","7", "6", "5", "4", "3","2","1","Sex,Parity 0", "Never Sex"     ))
    
    ggplot(datatable, aes(x=labels,y=data))+
      geom_bar(stat="identity") +
      coord_flip()+
      labs( title="Population Distribution", y="Percent of Women", x="") +
      theme_bw() +
      theme(axis.line=element_blank(),
            axis.ticks = element_blank(),
            axis.text=element_text(size = 15),
            axis.title=element_text(size = 15),
            plot.title = element_text(hjust = 0.5, size=15, face="bold"),
            panel.background=element_blank(),panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),plot.background=element_blank())
    
    
  },height = 300,width = 300)
  
  
  
}



shinyApp(ui = ui, server = server)

