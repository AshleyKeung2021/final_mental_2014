library(shiny)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readxl)

#load data
q7 <- read.csv('q7.csv')
#View(q7)

#change timestamp to year
df <-q7
df$Year <- as.POSIXct(q7$Timestamp, format="%Y-%m-%d %H:%M:%S")
df$Year <- format(df$Year, format="%Y")

#Clean the typo in gender
male_code <- c("male", "m", "M", "Male-ish", "maile", "Cis Male", "Mal", "Male", "Male (CIS)", "Make", "male ", "Man","msle", "Mail", "cis male","malr",
               "Cis Man","Malr", "Male ")
unknown_code <- c("Trans-female", "something kinda male?", "queer/she/they", "non-binary","Nah", "All", "Enby", "fluid", "Genderqueer", "Androgyne", 
                  "Agender", "male leaning androgynous", "Guy (-ish) ^_^", "Trans woman", "Neuter", "Female (trans)", "queer", "ostensibly male, unsure what that really means" , 
                  "A little about you","p" )
female_code <- c("female", "Cis Female", "f", "F", "Woman", "woman", "Femake", "female ","cis-female/femme", "Female (cis)", "femail", "Female ")

df$Gender <- sapply(as.vector(df$Gender), function(x) if(x %in% male_code) "Male" else x )
df$Gender <- sapply(as.vector(df$Gender), function(x) if(x %in% female_code) "Female" else x )
df$Gender <- sapply(as.vector(df$Gender), function(x) if(x %in% unknown_code) "Unknown" else x )


#check null values in all columns
df%>%
  summarise_all(funs(sum(is.na(.))))

#clear weird age, self_employed|work_interfere=NA, year<>2014
df_clean <- df %>%
  select(-Timestamp, -comments) %>%
  filter(Year == "2014") %>%
  filter(Age>18 & Age<100) %>%
  drop_na(self_employed) %>%
  drop_na(work_interfere)

#write out the file for Tableau dashboard
write.csv(df_clean, file = "df_clean.csv")


# ui ----------------------------------------------------------------------


ui <- (fluidPage(
  
  #header
  titlePanel(
    h1("Dashboard for Mental Health Survey 2014", align = "center")
  ),
  
  #draw a line
  tags$hr(style="border-color: black;"),
  
  sidebarLayout(
    sidebarPanel(

      #select IT vs not IT Company
      selectInput(inputId = "it_type", 
                  label = "Tech Company vs Non-Tech Company", 
                  choices = list("Tech"= "Yes", 
                                 "Non-Tech"= "No"),
                  selected =  "Yes"),
      
            
      #select age range
      sliderInput(inputId = "age_range",
                  label = "Age Range",
                  sep = "",
                  value = c(19, 72), min = 19, max = 72),
      


      #select country filter
      selectInput(inputId ="country_list", label = "Explore Different Countries", 
                  choices = c(unique(df_clean$Country)), multiple = TRUE, selected = "United States"),

      #select grouping(y axis)
      radioButtons(inputId = "fill_type", 
                   label = "Choose the Grouping for the Graph(y-axis)", 
                   choices = c("Gender"= "Gender", 
                               "remote_work" = "remote_work"), 
                   selected = "Gender", 
                   inline = FALSE,
                   width = NULL),
      
      #download original dataset
      downloadButton("downloadData", "Download Data")


    ),
      mainPanel(
        plotOutput("plot"), plotOutput("plot2"), plotOutput("plot3"), plotOutput("plot4"), plotOutput("plot5")
      
    )
  )
))



# server ------------------------------------------------------------------


server <- function(input, output) ({
  output$plot <- renderPlot({
    
    #output$country_graph <- 
      #renderPlot(
    
    # select age range
    select_max_age <- max(input$age_range)
    select_min_age <- min(input$age_range)
    
   #work_interfere
    if (input$fill_type == "Gender") {
      df_clean %>%
        filter(tech_company==input$it_type, 
               df_clean$Age>= select_min_age, df_clean$Age<= select_max_age) %>%
        subset(Country %in% input$country_list) %>%
      ggplot() +
        geom_bar(mapping=aes(x=work_interfere, fill=Gender)) + ggtitle("If you have a mental health condition, does it interferes with your work?")
    }


    else if (input$fill_type == "remote_work") {
      df_clean %>%
        filter(tech_company==input$it_type, 
               df_clean$Age>= select_min_age, df_clean$Age<= select_max_age) %>%
        subset(Country %in% input$country_list) %>%
        ggplot() +
        geom_bar(mapping=aes(x=work_interfere, fill=remote_work)) + ggtitle("If you have a mental health condition, does it interferes with your work?")
    }
    })
  output$plot2 <- renderPlot({
    
    # select age range
    select_max_age <- max(input$age_range)
    select_min_age <- min(input$age_range)
    
    #mental_health_consequence
    if (input$fill_type == "Gender") {
      df_clean %>%
        filter(tech_company==input$it_type, 
               df_clean$Age>= select_min_age, df_clean$Age<= select_max_age) %>%
        subset(Country %in% input$country_list) %>%
      ggplot(aes(x = factor(1),fill = factor(Gender))) + 
        geom_bar(stat = "count") + 
        coord_polar(theta='y') +
        theme(axis.text.y = element_blank(), 
              axis.title.y = element_blank(), 
              axis.ticks.y = element_blank(),
              axis.title.x = element_blank()) +
        labs(fill = "Gender") + facet_wrap(~mental_health_consequence) + ggtitle("Is there any negative effect to discuss mental health issue with your employer?")
    }
    
    else if (input$fill_type == "remote_work") {
      df_clean %>%
        filter(tech_company==input$it_type, 
               df_clean$Age>= select_min_age, df_clean$Age<= select_max_age) %>%
        subset(Country %in% input$country_list) %>%
        ggplot(aes(x = factor(1),fill = factor(remote_work))) + 
        geom_bar(stat = "count") + 
        coord_polar(theta='y') +
        theme(axis.text.y = element_blank(), 
              axis.title.y = element_blank(), 
              axis.ticks.y = element_blank(),
              axis.title.x = element_blank()) +
        labs(fill = "remote_work") + facet_wrap(~mental_health_consequence) + ggtitle("Is there any negative effect to discuss mental health issue with your employer?")
    }
   
  })
  
  
  output$plot3 <- renderPlot({

    # select age range
    select_max_age <- max(input$age_range)
    select_min_age <- min(input$age_range)
    
    #Benefits  
      if (input$fill_type == "Gender") {
        df_clean %>%
          filter(tech_company==input$it_type, 
                 df_clean$Age>= select_min_age, df_clean$Age<= select_max_age) %>%
          subset(Country %in% input$country_list) %>%
          ggplot() +
          geom_bar(mapping=aes(fill=Gender, x=benefits)) + ggtitle("Does your employer provide mental health benefits?") + facet_wrap(~Gender)
     
    }
    
    else if (input$fill_type == "remote_work") {
      df_clean %>%
          filter(tech_company==input$it_type, 
                 df_clean$Age>= select_min_age, df_clean$Age<= select_max_age) %>%
          subset(Country %in% input$country_list) %>%
        ggplot() +
        geom_bar(mapping=aes(fill=remote_work, x=benefits)) + ggtitle("Does your employer provide mental health benefits?") + facet_wrap(~remote_work)
      
    }
  })
  
  
  output$plot4 <- renderPlot({
    
    # select age range
    select_max_age <- max(input$age_range)
    select_min_age <- min(input$age_range)
    
    #seek_help
    if (input$fill_type == "Gender") {
      df_clean %>%
        filter(tech_company==input$it_type, 
               df_clean$Age>= select_min_age, df_clean$Age<= select_max_age) %>%
        subset(Country %in% input$country_list) %>%
        ggplot() +
        geom_bar(mapping=aes(x=seek_help, fill=Gender)) + ggtitle("Does your employer provide resources about how to seek help?") + coord_flip()
    }
    
    
    else if (input$fill_type == "remote_work") {
      df_clean %>%
        filter(tech_company==input$it_type, 
               df_clean$Age>= select_min_age, df_clean$Age<= select_max_age) %>%
        subset(Country %in% input$country_list) %>%
        ggplot() +
        geom_bar(mapping=aes(x=seek_help, fill=remote_work)) + ggtitle("Does your employer provide resources about how to seek help?") + coord_flip()
    }

    
})
  
  output$plot5 <- renderPlot({
    
    # select age range
    select_max_age <- max(input$age_range)
    select_min_age <- min(input$age_range)
    
    #treatment
    if (input$fill_type == "Gender") {
      df_clean %>%
        filter(tech_company==input$it_type, 
               df_clean$Age>= select_min_age, df_clean$Age<= select_max_age) %>%
        subset(Country %in% input$country_list) %>%
        ggplot(aes(x = factor(1),fill = factor(Gender))) + 
        geom_bar(stat = "count") + 
        coord_polar(theta='y') +
        theme(axis.text.y = element_blank(), 
              axis.title.y = element_blank(), 
              axis.ticks.y = element_blank(),
              axis.title.x = element_blank()) +
        labs(fill = "Gender") + facet_wrap(~treatment) + ggtitle("Have you sought treatment for a mental health condition?")
    }
    
    else if (input$fill_type == "remote_work") {
      df_clean %>%
        filter(tech_company==input$it_type, 
               df_clean$Age>= select_min_age, df_clean$Age<= select_max_age) %>%
        subset(Country %in% input$country_list) %>%
        ggplot(aes(x = factor(1),fill = factor(remote_work))) + 
        geom_bar(stat = "count") + 
        coord_polar(theta='y') +
        theme(axis.text.y = element_blank(), 
              axis.title.y = element_blank(), 
              axis.ticks.y = element_blank(),
              axis.title.x = element_blank()) +
        labs(fill = "remote_work") + facet_wrap(~treatment) + ggtitle("Have you sought treatment for a mental health condition?")
    }  
  
  })
  
  #download data  
  data <- df_clean
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0(input$dataset, ".csv")
    },
    content = function(file) {
      write.csv(data, file)
    }
  )
})

# Run the application 
shinyApp(ui = ui, server = server)
