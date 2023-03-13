library(tidyverse)
library(dplyr)
library(lubridate)
library(plotly)
theme_set(theme_classic())

# Helper functions
################################################################################
extract_top <- function(df, x, num) {
  (
    df %>%
      group_by({{x}}) %>%
      summarise(freq=n()) %>%
      ungroup() %>%
      arrange(-freq) %>%
      pull({{x}})
  )[1:{{num}}]
}

kdrama_df1_union_df2 <- function(df1, df2, by_col){
  merge(df1,df2, by=by_col) %>%
    drop_na()
}

kdrama_count_summarize <- function(df, col) {
  df %>%
    group_by(category_value=df[[col]], year_airing) %>%
    summarize(kdrama_count=n())
}

generate_heat_map <- function(df,category_value,fill_option){
  ggplot(df, aes(x=year_airing, y=category_value, fill=kdrama_count)) +
    geom_tile(colour="white", linewidth=1) +
    coord_equal() +
    labs(title='',
         x='',
         y='') +
    scale_x_discrete(limits = seq(2012, 2022, 1)) +
    theme(
      axis.ticks = element_blank(),
      axis.line  = element_blank(),
      plot.margin=grid::unit(c(0,0,0,0), "mm")
      ) +
    scale_fill_viridis_c(option = fill_option, direction = -1)
}

plot_heat_map_between <- function(df1,df2,fill_option='H'){
  merged_df <- kdrama_df1_union_df2({{df1}}, {{df2}}, 'kdrama_name')
  category_value <- (merged_df%>%colnames())[2]
  merged_df_summarized <- kdrama_count_summarize(merged_df,category_value)
  hm <- generate_heat_map(merged_df_summarized, category_value, fill_option)
  ggplotly(hm)
}

make_scatter_plot <- function(filter_criteria) {
  filtered_data <- kdrama_metrics %>% filter(content_rating %in% filter_criteria)
  sp = ggplot(filtered_data, aes(label=kdrama_name, label2=watchers)) +
    geom_jitter(aes(year_airing, score, col=content_rating),alpha=0.5) +
    scale_y_discrete(limits = seq(5.5,9.5,0.5)) +
    scale_x_discrete(limits = seq(2012,2022,1))
  ggplotly(sp)
}

# Read CSVs
################################################################################

#### Read CSV-1: kdrama_actors
kdrama_actors <-
  read_csv("https://raw.githubusercontent.com/NiharikaCNR/stat436-hw2-kdrama-analytics/main/actors.csv") %>%
  drop_na()
kdrama_top_actors <- kdrama_actors %>%
  subset(actor %in% extract_top(kdrama_actors, actor, 7))
kdrama_actors_summary <- kdrama_actors %>% 
  group_by(kdrama_name) %>%
  summarize(n_actors = n()) %>% 
  arrange(n_actors)

#### Read CSV-2: kdrama_days
kdrama_days <- read_csv("https://raw.githubusercontent.com/NiharikaCNR/stat436-hw2-kdrama-analytics/main/aired_on.csv") %>%
  drop_na()
kdrama_days$day <-
  recode(kdrama_days$day, 'monday'='Mon', 'tuesday'='Tue', 'wednesday'='Wed', 'thursday'='Thu', 'friday'='Fri', 'saturday'='Sat', 'sunday'='Sun')  %>%
  factor(levels = c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun'))

#### Read CSV-3: kdrama networks
kdrama_networks <- read_csv('https://raw.githubusercontent.com/NiharikaCNR/stat436-hw2-kdrama-analytics/main/networks.csv') %>%
  drop_na()
kdrama_top_networks <- kdrama_networks %>%
  subset(original_networks %in% extract_top(kdrama_networks, original_networks,7))

#### Read CSV-4: kdrama_genres
kdrama_genres <- read_csv('https://raw.githubusercontent.com/NiharikaCNR/stat436-hw2-kdrama-analytics/main/genres.csv') %>% 
  drop_na()
kdrama_top_genres <- kdrama_genres %>%
  subset(genre %in% extract_top(kdrama_genres, genre, 7))

#### Read CSV-5: kdrama_descriptors
  kdrama_descriptors <- read_csv('https://raw.githubusercontent.com/NiharikaCNR/stat436-hw2-kdrama-analytics/main/main_descriptors.csv')

#### Read CSV-6: kdrama_year
kdrama_year <- kdrama_descriptors %>%
  mutate(start_airing = case_when(
    grepl(",", start_airing, ignore.case = T) ~ mdy(start_airing),
    grepl("", start_airing, ignore.case = T) ~ dmy(start_airing)
  )) %>%
  mutate(year_airing = year(start_airing)) %>%
  select(c('kdrama_name', 'year_airing')) %>%
  drop_na() %>%
  subset(year_airing %in% seq(2012, 2022))

#### Read CSV-7: kdrama_metrics
features <- c("kdrama_name","score","watchers","content_rating")
kdrama_metrics <- kdrama_df1_union_df2(kdrama_year, kdrama_descriptors[,features], 'kdrama_name') %>%
  mutate_at(features[2:3], as.numeric) %>%
  drop_na()

# Add Shiny Elements
################################################################################

ui <- fluidPage(
  titlePanel(
    h1(strong("K-Drama Analytics Dashboard"),
       align = "center"),
    windowTitle = "K-Drama Analytics Dashboard"
    ),
  fluidRow(
    column(6,
           fluidRow(
             column(
                 12,
                 h3(strong("Interactive Plot #1")),
                 selectInput( "hm_y_axis", "Visualization Category:", 
                              c('Days of broadcast', 'Top Genres','Top Networks'), 
                              selected='Days of broadcast'
                 ),
                 p(tags$i("Choose categories from the above dropdown to visualize different K-Drama parameters.")),
                 p(tags$i("(Hover over the tiles of the heat-map for accurate K-Drama count)")),
                 tags$hr(),
                 radioButtons( "hm_fill_option", "Change Heat Map Palette:", 
                               c('Turbo','Magma', 'Inferno', 'Plasma', 'Viridis', 'Cividis', 'Rocket', 'Mako'),
                               inline = T)
                 
               )
             ),
           ),
    column(6, plotlyOutput("heat_map"))
  ),
  tags$hr(),
  fluidRow(
    column( 6, 
      fluidRow(
        column(
            12, 
            h3(strong("Interactive Plot #2")), 
            checkboxGroupInput("content_rating", "Content Rating:", 
                        sort(unique(kdrama_metrics$content_rating)), 
                        # multiple = TRUE, 
                        selected = "Not Yet Rated"
            ),
            tags$hr(),
            p("."),
            p("."),
            p("."),
            p("."),
            p("."),
            p("Submitted by: Niharika Chunduru"),
          )
        ),
      ),
    column( 
        6, 
        plotlyOutput("scatter_plot"), 
        p(tags$i("(Hovering over the each point in the scatter plot will show more information on the K-Drama metrics)")),
    )
  ),
  
)


server <- function(input, output) {

  df1 <- reactive({
    switch(
      input$hm_y_axis,
      "Days of broadcast" = kdrama_days,
      "Top Genres" = kdrama_top_genres,
      "Top Networks" = kdrama_top_networks
    )
  })

  df2 <- kdrama_year

  fill_option <- reactive({ tolower(input$hm_fill_option) })

  output$heat_map <- renderPlotly({
    plot_heat_map_between(df1(), df2, fill_option())
  })
  
  filter_criteria <- reactive({ input$content_rating })
  
  output$scatter_plot <- renderPlotly({
    make_scatter_plot(filter_criteria())
  })
  
}

shinyApp(ui, server)
