library(reshape2)
library(shiny)
library(dplyr)
library(ggplot2)

#setwd("~/Documents/USF/Spring_Module_2/Data_Visualization/Asg2")
population <- read.csv("API_SP.POP.TOTL_DS2_en_csv_v2.csv")
fertility <- read.csv("API_SP.DYN.TFRT.IN_DS2_en_csv_v2.csv", skip = 4)
life_exp <- read.csv("API_SP.DYN.LE00.IN_DS2_en_csv_v2.csv", skip = 4)
metadata_country <- read.csv("Metadata_Country_API_SP.DYN.LE00.IN_DS2_en_csv_v2.csv", na.strings = c("","NA"))


cleaning_data <- function(df,variable){
  df <- df[, !names(df) %in% c("Indicator.Name", "Indicator.Code","X2015","X2016","X")]
  years <- as.character(1960:2014)
  colnames(df) <- c("Country.Name", "Country.Code", years)
  
  inx <- which(is.na(df), arr.ind = TRUE)
  df[inx] <- rowMeans(df[ , 3:ncol(df)],  na.rm = TRUE)[inx[ , 1]]
  df <- df[rowSums(is.na(df))!= length(years), ]
  
  df_melt <- melt(df,id=c("Country.Name", "Country.Code"))
  colnames(df_melt) <- c("Country.Name", "Country.Code","Year",variable)
  return(df_melt)
}

df_population <- cleaning_data(population,"Population")
df_fertility <- cleaning_data(fertility,"Fertility_Rate")
df_life_exp <- cleaning_data(life_exp,"Life_Expectancy")

df_metadata <- metadata_country[,1:2]
df_metadata <- na.omit(df_metadata)

#merging the data
df_life_exp_pop <- merge(df_population,df_life_exp, by = c("Country.Code","Country.Name","Year"))
df_life_exp_pop_fert <- merge(df_life_exp_pop,df_fertility, by = c("Country.Code","Country.Name","Year"))
df_all <- merge(df_life_exp_pop_fert,df_metadata, by = "Country.Code")
df_all <- df_all[, !names(df_all) %in% c("Country.Code")]
df_all$Year <- as.character(df_all$Year)

ui <- fluidPage(
  headerPanel("Gapminder Interactive Plot\n\n"),

  mainPanel(
    plotOutput("plot1", hover = hoverOpts(id = "plot_hover", delay = 100, delayType = "debounce")),
    uiOutput("hover_info")
  ), # end main panel

  sidebarPanel(
    checkboxGroupInput("region", "Select Regions", choices = levels(df_all$Region)),
    sliderInput("year", "Select Year", 1960, 2014, 1, animate = TRUE),
    sliderInput("size", "Select Bubble Size", 1, 10, 1, value = 5)
  ) # end side panel
)

server <- function(input, output) {
  
  input_size <- reactive({input$size})
  data <- reactive({df_all %>% filter(Year == input$year)})
  
  output$plot1 <- renderPlot({
    if (is.null(input$region)) {
      ggplot(data=data(), aes(Life_Expectancy, Fertility_Rate)) +
        geom_point(aes(size = Population, colour = Region)) +
        theme(plot.title = element_text(hjust = 0.5), 
              legend.justification = c("right", "top"),
              legend.position="none",
              panel.background = element_rect(fill="white",color = "black", size = 0.5),
              axis.title.x = element_text(size = 16),
              axis.title.y = element_text(size = 16),
              axis.text = element_text(size = 14),
              panel.grid.major = element_line(colour = "gray97")) +
        ylab("Fertility Rate\n ") + xlab("\n Life Expectancy\n") +
        scale_size(range = c(1, 6)*input_size())}
    
    else{
      data <- data()[data()$Region %in% input$region,]

      ggplot(data=data, aes(Life_Expectancy, Fertility_Rate)) +
        geom_point(aes(size = Population)) +
        theme(plot.title = element_text(hjust = 0.5),
              legend.justification = c("right", "top"),
              legend.position="none",
              panel.background = element_rect(fill="white",color = "black", size = 0.5),
              axis.title.x = element_text(size = 16),
              axis.title.y = element_text(size = 16),
              axis.text = element_text(size = 14)) +
        ylab("Fertility Rate\n ") + xlab("\n Life Expectancy") +
        scale_size(range = c(1, 6)*input_size())}
  })
  
  output$hover_info <- renderUI({
    hover <- input$plot_hover
    point <- nearPoints(data(), hover, threshold = 5, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)

    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)

    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)

    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")

    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Country: </b>", point$Country.Name, "<br/>",
                    "<b> Year: </b>", point$Year, "<br/>",
                    "<b> Population: </b>", point$Population, "<br/>",
                    "<b> Life Expectancy: </b>", point$Life_Expectancy, "<br/>",
                    "<b> Fertility Rate: </b>", point$Fertility_Rate, "<br/>"
                    )))
    )
  })
}

#runApp(list(ui = ui, server = server))

shinyApp(ui = ui, server = server)