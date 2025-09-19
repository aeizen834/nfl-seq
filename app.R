library(shiny)
library(scales)
library(bslib)
library(tidyverse)
library(rvest)
library(ggpattern)
library(shadowtext)
library(vistime)
library(ggthemes)
library(nflverse)
library(showtext)
library(sysfonts)
library(ggtext)
library(ggimage)
library(gt)
library(gtExtras)
library(rsconnect)
library(DT)

## Functions for the Code
split_data_for_display <- function(data) {
  n <- nrow(data)
  h <- round(n/2, 0)
  data_left <- data[1:h, ]
  data_right <- data[(h+1):n, ]
  
  # Balance the tables
  if (nrow(data_left) > nrow(data_right)) {
    nc <- as.data.frame(
      matrix(NA, nrow = 1, ncol = ncol(data_right)), 
      stringsAsFactors = FALSE
    )
    names(nc) <- names(data_right)
    nc[1, 1:4] <- ""
    data_right <- rbind(data_right, nc)
  } else if (nrow(data_left) < nrow(data_right)) {
    nc <- as.data.frame(
      matrix(NA, nrow = 1, ncol = ncol(data_left)), 
      stringsAsFactors = FALSE
    )
    names(nc) <- names(data_left)
    nc[1, 1:4] <- ""
    data_left <- rbind(data_left, nc)
  }
  
  # Rename right columns
  names(data_right) <- paste0(names(data_right), "_2")
  
  return(cbind(data_left, data_right))
}

seq_table <- function(play_data){
  
  seq_chart <- play_data %>% 
    group_by(posteam,seq_group) %>% 
    reframe(epa_per_play = mean(epa),
            success_rate = mean(success)) %>% 
    merge(teams_colors_logos %>% select(team_abbr,team_color, team_color2,team_color3, team_wordmark,team_logo_wikipedia),
          by.x = 'posteam',by.y = 'team_abbr') %>% 
    unique()
  
  whole_totals <- play_data %>% 
    group_by(posteam) %>% 
    mutate(total_epa = mean(epa),
           total_sr = mean(success)) 
  
  
  # print(seq_chart)
  new_table <- c()
  for (k in 1:32) {
    tms <- seq_chart %>% pull(posteam) %>% unique()
    tm <- tms[k]
    primary <- seq_chart %>% filter(posteam == tm) %>% pull(team_color) %>% unique() 
    secondary <- seq_chart %>% filter(posteam == tm) %>% pull(team_color2) %>% unique() 
    tertiary <- seq_chart %>% filter(posteam == tm) %>% pull(team_color3) %>% unique()
    wordmark <- seq_chart %>% filter(posteam == tm) %>% pull(team_wordmark) %>% unique()
    logo <- seq_chart %>% filter(posteam == tm) %>% pull(team_logo_wikipedia) %>% unique()
    
    
    # Get Pass-Pass Metrics
    SR <- whole_totals %>% 
      filter(posteam == tm) %>% 
      pull(total_sr) #%>% 
    # percent(accuracy = 0.1)
    EPA <- whole_totals %>% 
      filter(posteam == tm) %>% 
      pull(total_epa) %>% 
      round(3)
    
    # Get Pass-Pass Metrics
    PP_SR <- seq_chart %>% 
      filter(posteam == tm & seq_group == 'Pass-Pass') %>% 
      pull(success_rate) #%>% 
    # percent(accuracy = 0.1)
    PP_EPA <- seq_chart %>% 
      filter(posteam == tm & seq_group == 'Pass-Pass') %>% 
      pull(epa_per_play) %>% 
      round(3)
    
    # Get Pass-Run Metrics
    PR_SR <- seq_chart %>% 
      filter(posteam == tm & seq_group == 'Pass-Run') %>% 
      pull(success_rate) #%>% 
    # percent(accuracy = 0.1)
    PR_EPA <- seq_chart %>% 
      filter(posteam == tm & seq_group == 'Pass-Run') %>% 
      pull(epa_per_play) %>% 
      round(3)
    
    # Get Run-Pass Metrics
    RP_SR <- seq_chart %>% 
      filter(posteam == tm & seq_group == 'Run-Pass') %>% 
      pull(success_rate) #%>% 
    # percent(accuracy = 0.1)
    RP_EPA <- seq_chart %>% 
      filter(posteam == tm & seq_group == 'Run-Pass') %>% 
      pull(epa_per_play) %>% 
      round(3)
    
    # Get Run-Run Metrics
    RR_SR <- seq_chart %>% 
      filter(posteam == tm & seq_group == 'Run-Run') %>% 
      pull(success_rate) #%>% 
    # percent(accuracy = 0.1)
    RR_EPA <- seq_chart %>% 
      filter(posteam == tm & seq_group == 'Run-Run') %>% 
      pull(epa_per_play) %>% 
      round(3)
    
    team_row <- data.frame(
      posteam = tm,
      primary, 
      secondary,
      tertiary,
      wordmark,
      logo,
      EPA,
      SR,
      PP_EPA,
      PP_SR,
      PR_EPA,
      PR_SR,
      RP_EPA,
      RP_SR,
      RR_EPA,
      RR_SR
    )
    
    new_table <- bind_rows(new_table, team_row)
  }
  
  return(new_table)
}

load_app_data <- function(){
  read_csv('All Seq.csv', show_col_types = F)
}
# Define UI for application that draws a histogram
icon <- div(
  style = "position: absolute; top: 10px; right: 20px; 
           background-color: #E63946; color: white; 
           padding: 8px 15px; border-radius: 5px; 
           font-weight: bold; font-size: 12px; 
           box-shadow: 2px 2px 5px rgba(0,0,0,0.3);",
  "By: @AriEizen | Data: nflfastR/Wikipedia | Inspo: @reinhardNFL"
)

ui <- navbarPage(
  title = "NFL Draft Trends & Insights",
  theme = bs_theme(
    bg = "#F8F9FA",           # Off-white background
    fg = "#000000",           # Black text
    primary = "#E63946",      # Bold red accents
    secondary = "#457B9D",    # Blue highlights
    base_font = "Oswald"      # Sports-like font
  ),
  ############################ HTML and CSS ############################
  tags$head(
    tags$style(HTML("
    body {
      background-color: #F8F9FA !important;
      color: #000000 !important;
    }

    /* Change Navbar Background & Text Color */
    .navbar {
      background-color: #EAEAEA !important;  /* Light Grey Background */
      border-bottom: 2px solid #CCCCCC; /* Optional: Adds subtle border */
    }

    .navbar-nav > li > a {
      font-size: 18px !important;
      font-weight: bold !important;
      padding: 15px 20px !important;
      color: #000000 !important;
    }

    .navbar-brand {
      font-size: 24px !important;
      font-weight: bold !important;
      color: #000000 !important;
    }

    .tab-content {
      background-color: #FFFFFF !important;
      padding: 20px;
      border-radius: 10px;
    }

    h1, h4 {
      font-weight: bold !important;
      color: #000000 !important;
    }

    .custom-container {
      background-color: #EAEAEA;
      padding: 20px;
      border-radius: 10px;
      margin-bottom: 20px;
    }
    
     #s label {
      display: inline-block;
      width: 30%;
      margin-bottom: 5px;
    }
  "))
  #############################################################################
  ),
  tabPanel("Main Dashboard",
    fluidRow(
      column(2,sliderInput("week",
                  "Week Number:",
                  min = 1,
                  max = 22,
                  value = c(1,18))),
      column(2,sliderInput("wp",
                  "Team Win %:",
                  min = 0,
                  max = 100,
                  value = c(20,80))),
      column(1,checkboxGroupInput('down',
                         'Down:',
                         choices = 1:4,
                         selected = 1:4,
                         inline = T)),
      column(1,checkboxGroupInput('qtr',
                         'Quarter:',
                         choices = c(1,2,3,4,'OT' = 5),
                         selected = 1:5,
                         inline = T)),
      column(6,radioButtons('order',
                   'Order Data:',
                   choices = c('Pass-Pass Success Rate' = 'PP_SR', 'Pass-Pass EPA/Play' = 'PP_EPA',
                               'Pass-Run Success Rate' = 'PR_SR', 'Pass-Run EPA/Play' = 'PR_EPA',
                               'Run-Pass Success Rate' = 'RP_SR', 'Run-Pass EPA/Play' = 'RP_EPA',
                               'Run-Run Success Rate' = 'RR_SR', 'Run-Run EPA/Play' = 'RR_EPA'),
                   selected = 'PP_EPA',
                   inline = T))
      
    ),

        # Show a plot of the generated distribution
        fluidRow(
           # DTOutput("raw_data")
          column(12,gt_output('overview'))
        )
    ),
  tabPanel("Drill In Analysis",
           fluidRow(
             column(2,sliderInput("week_2",
                                  "Week Number:",
                                  min = 1,
                                  max = 22,
                                  value = c(1,18))),
             column(2,sliderInput("wp_2",
                                  "Team Win %:",
                                  min = 0,
                                  max = 100,
                                  value = c(20,80))),
             column(1,checkboxGroupInput('down_2',
                                         'Down:',
                                         choices = 1:4,
                                         selected = 1:4,
                                         inline = T)),
             column(1,checkboxGroupInput('qtr_2',
                                         'Quarter:',
                                         choices = c(1,2,3,4,'OT' = 5),
                                         selected = 1:5,
                                         inline = T)),
             column(2,selectInput('first',
                                  'First Play:',
                                  choices = c('Run','Pass'),
                                  selected = 'Pass')),
             column(2,selectInput('second',
                                  'Second Play',
                                  choices = c('Run','Pass'),
                                  selected = 'Pass'))
           ))
  )

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  app_data <- load_app_data()
  # print(app_data %>% head() %>% as.data.frame())
  # Stats Overview GT
  output$overview <- render_gt({
    
    t <- app_data %>% 
      filter(between(week,as.numeric(min(input$week)),max(input$week)),
             between(wp,min(input$wp)/100,max(input$wp)/100),
             down %in% input$down,
             qtr %in% input$qtr)
    
    
    play_table <- seq_table(t) %>% 
      select(-logo) %>% 
      arrange(desc(.data[[input$order]])) %>%
      unique() %>% 
      mutate(rank = row_number())
    
    print(head(play_table) %>% as.data.frame())
    
    table_split <- split_data_for_display(play_table) %>% 
      relocate(rank_2, .before = wordmark_2)
    
    # tab_title <- 'NFL Play Sequence Overview in 2024'
    # tab_subtitle <- paste0('Win Prob: ', min(input$wp), '% - ',max(input$week),' | Weeks ',
    #                        min(input$week),'-',max(input$week), ' | Downs: ' )
    table_split %>% 
      gt() %>% 
      cols_move_to_start(rank) %>% 
      cols_hide(c(primary, secondary,tertiary,posteam,
                  primary_2, secondary_2,tertiary_2,posteam_2)) %>% 
      tab_style(
        style = list(
          cell_fill(color = "#F8F9FA"),
          cell_text(color = "#000000")
        ),
        locations = list(
          cells_body(columns = everything()),
          cells_title(),
          cells_footnotes(),
          cells_column_labels(),
          cells_column_spanners()
        )
      ) %>%
      tab_style(
        style = cell_text(weight = "bold"),
        locations = list(
          cells_column_labels(),
          cells_column_spanners()
        )
      ) %>%
      tab_style(
        style = cell_fill(color = "#F8F9FA"), 
        locations = cells_row_groups() 
      ) %>%
      tab_style(
        style = cell_text(color = "#000000", weight = "bold"),
        locations = cells_row_groups()
      ) %>% 
      ## Left Col of Data
      tab_spanner('Overall Stats', columns = c(SR,EPA)) %>%
      tab_spanner('Run-Pass Plays', columns = c(RP_SR,RP_EPA)) %>%
      tab_spanner('Pass-Pass Plays', columns = c(PP_SR,PP_EPA)) %>%
      tab_spanner('Run-Run Plays', columns = c(RR_SR,RR_EPA)) %>%
      tab_spanner('Pass-Run Plays', columns = c(PR_SR,PR_EPA)) %>%
      cols_label(PP_SR = 'Success Rate', PP_EPA = 'EPA/Play',
                 PR_SR = 'Success Rate', PR_EPA = 'EPA/Play',
                 RR_SR = 'Success Rate', RR_EPA = 'EPA/Play',
                 RP_SR = 'Success Rate', RP_EPA = 'EPA/Play',
                 SR = 'Success Rate', EPA = 'EPA/Play',
                 wordmark = 'Team', rank = 'Rank') %>% 
      fmt_percent(columns = c(SR,PP_SR,PR_SR,RP_SR,RR_SR), decimals = 1) %>% 
      data_color(columns = EPA, palette = c("#800080","green"), domain = range(play_table$EPA)) %>% 
      data_color(columns = SR, palette = c("#800080","green"), domain = range(play_table$SR)) %>% 
      data_color(columns = PP_EPA, palette = c("#800080","green"), domain = range(play_table$PP_EPA)) %>% 
      data_color(columns = PP_SR, palette = c("#800080","green"), domain = range(play_table$PP_SR)) %>% 
      data_color(columns = PR_EPA, palette = c("#800080","green"), domain = range(play_table$PR_EPA)) %>% 
      data_color(columns = PR_SR, palette = c("#800080","green"), domain = range(play_table$PR_SR)) %>% 
      data_color(columns = RP_EPA, palette = c("#800080","green"), domain = range(play_table$RP_EPA)) %>% 
      data_color(columns = RP_SR, palette = c("#800080","green"), domain = range(play_table$RP_SR)) %>% 
      data_color(columns = RR_EPA, palette = c("#800080","green"), domain = range(play_table$RR_EPA)) %>% 
      data_color(columns = RR_SR, palette = c("#800080","green"), domain = range(play_table$RR_SR)) %>% 
      gt_img_rows(wordmark) %>% 
      ## Right Col of Data
      tab_spanner('Overall Stats ', columns = c(SR_2,EPA_2)) %>%
      tab_spanner('Run-Pass Plays ', columns = c(RP_SR_2,RP_EPA_2)) %>%
      tab_spanner('Pass-Pass Plays ', columns = c(PP_SR_2,PP_EPA_2)) %>%
      tab_spanner('Run-Run Plays ', columns = c(RR_SR_2,RR_EPA_2)) %>%
      tab_spanner('Pass-Run Plays ', columns = c(PR_SR_2,PR_EPA_2)) %>%
      cols_label(PP_SR_2 = 'Success Rate', PP_EPA_2 = 'EPA/Play',
                 PR_SR_2 = 'Success Rate', PR_EPA_2 = 'EPA/Play',
                 RR_SR_2 = 'Success Rate', RR_EPA_2 = 'EPA/Play',
                 RP_SR_2 = 'Success Rate', RP_EPA_2 = 'EPA/Play',
                 SR_2 = 'Success Rate', EPA_2 = 'EPA/Play',
                 wordmark_2 = 'Team', rank_2 = 'Rank') %>% 
      fmt_percent(columns = c(SR_2,PP_SR_2,PR_SR_2,RP_SR_2,RR_SR_2), decimals = 1) %>%
      data_color(columns = EPA_2, palette = c("#800080","green"), domain = range(play_table$EPA)) %>% 
      data_color(columns = SR_2, palette = c("#800080","green"), domain = range(play_table$SR)) %>%
      data_color(columns = PP_EPA_2, palette = c("#800080","green"), domain = range(play_table$PP_EPA)) %>% 
      data_color(columns = PP_SR_2, palette = c("#800080","green"), domain = range(play_table$PP_SR)) %>% 
      data_color(columns = PR_EPA_2, palette = c("#800080","green"), domain = range(play_table$PR_EPA)) %>% 
      data_color(columns = PR_SR_2, palette = c("#800080","green"), domain = range(play_table$PR_SR)) %>% 
      data_color(columns = RP_EPA_2, palette = c("#800080","green"), domain = range(play_table$RP_EPA)) %>% 
      data_color(columns = RP_SR_2, palette = c("#800080","green"), domain = range(play_table$RP_SR)) %>% 
      data_color(columns = RR_EPA_2, palette = c("#800080","green"), domain = range(play_table$RR_EPA)) %>% 
      data_color(columns = RR_SR_2, palette = c("#800080","green"), domain = range(play_table$RR_SR)) %>% 
      gt_img_rows(wordmark_2) 
  })
  
  # Table on Drill In Page
  # Should show epa/play and SR for both first play and second play of selected
  output$raw_data <- renderDT({
    
    seq_table <- function(){
      t <- read_csv('All Seq.csv') %>% 
        filter(between(week,as.numeric(min(input$week_2)),max(input$week_2)),
               between(wp,min(input$wp_2)/100,max(input$wp_2)/100),
               down %in% input$down_2,
               qtr %in% input$qtr_2,
               ((playType == input$first & t_next_play == input$second) |
               (t_last_play == input$first & playType == input$second)))
      
      seq_chart <- t %>% 
        group_by(posteam,seq_group) %>% 
        reframe(epa_per_play = mean(epa),
                success_rate = mean(success)) %>% 
        merge(teams_colors_logos %>% select(team_abbr,team_color, team_color2,team_color3, team_wordmark,team_logo_wikipedia),
              by.x = 'posteam',by.y = 'team_abbr') %>% 
        unique()
      
      whole_totals <- t %>% 
        group_by(posteam) %>% 
        mutate(total_epa = mean(epa),
               total_sr = mean(success)) 
      
      
      # print(seq_chart)
      new_table <- c()
      for (k in 1:32) {
        tms <- seq_chart %>% pull(posteam) %>% unique()
        tm <- tms[k]
        primary <- seq_chart %>% filter(posteam == tm) %>% pull(team_color) %>% unique() 
        secondary <- seq_chart %>% filter(posteam == tm) %>% pull(team_color2) %>% unique() 
        tertiary <- seq_chart %>% filter(posteam == tm) %>% pull(team_color3) %>% unique()
        wordmark <- seq_chart %>% filter(posteam == tm) %>% pull(team_wordmark) %>% unique()
        logo <- seq_chart %>% filter(posteam == tm) %>% pull(team_logo_wikipedia) %>% unique()
        
        
        # Get Pass-Pass Metrics
        SR <- whole_totals %>% 
          filter(posteam == tm) %>% 
          pull(total_sr) #%>% 
        # percent(accuracy = 0.1)
        EPA <- whole_totals %>% 
          filter(posteam == tm) %>% 
          pull(total_epa) %>% 
          round(3)
        
        # Get Pass-Pass Metrics
        PP_SR <- seq_chart %>% 
          filter(posteam == tm & seq_group == 'Pass-Pass') %>% 
          pull(success_rate) #%>% 
        # percent(accuracy = 0.1)
        PP_EPA <- seq_chart %>% 
          filter(posteam == tm & seq_group == 'Pass-Pass') %>% 
          pull(epa_per_play) %>% 
          round(3)
        
        # Get Pass-Run Metrics
        PR_SR <- seq_chart %>% 
          filter(posteam == tm & seq_group == 'Pass-Run') %>% 
          pull(success_rate) #%>% 
        # percent(accuracy = 0.1)
        PR_EPA <- seq_chart %>% 
          filter(posteam == tm & seq_group == 'Pass-Run') %>% 
          pull(epa_per_play) %>% 
          round(3)
        
        # Get Run-Pass Metrics
        RP_SR <- seq_chart %>% 
          filter(posteam == tm & seq_group == 'Run-Pass') %>% 
          pull(success_rate) #%>% 
        # percent(accuracy = 0.1)
        RP_EPA <- seq_chart %>% 
          filter(posteam == tm & seq_group == 'Run-Pass') %>% 
          pull(epa_per_play) %>% 
          round(3)
        
        # Get Run-Run Metrics
        RR_SR <- seq_chart %>% 
          filter(posteam == tm & seq_group == 'Run-Run') %>% 
          pull(success_rate) #%>% 
        # percent(accuracy = 0.1)
        RR_EPA <- seq_chart %>% 
          filter(posteam == tm & seq_group == 'Run-Run') %>% 
          pull(epa_per_play) %>% 
          round(3)
        
        team_row <- data.frame(
          posteam = tm,
          primary, 
          secondary,
          tertiary,
          wordmark,
          logo,
          EPA,
          SR,
          PP_EPA,
          PP_SR,
          PR_EPA,
          PR_SR,
          RP_EPA,
          RP_SR,
          RR_EPA,
          RR_SR
        )
        
        new_table <- bind_rows(new_table, team_row)
      }
      
      return(new_table)
    }
    
    dt <- seq_table() %>% 
      rename(
        'Pass-Pass EPA/Play'= PP_EPA,
        'Pass-Pass Success Rate'= PP_SR,
        'Pass-Run EPA/Play'= PR_EPA,
        'Pass-Run SR'= PR_SR,
        'Run-Pass EPA/Play'= RP_EPA,
        'Run-Pass SR'= RP_SR,
        'Run-Run EPA/Play'= RR_EPA,
        'Run-Run SR'= RR_SR,
      ) %>% 
      select(-posteam,-primary,-secondary,-tertiary,-wordmark) %>% 
      gt() %>% 
      
    
    return(dt)
    
  
  },escape = FALSE, options = list(pageLength = 10)) 
}

# Run the application 
shinyApp(ui = ui, server = server)
