# Required Libraries ----
library(shiny)
library(ggplot2)
library(bslib)
library(bsicons)

# Setup the Stanford theme with bslib ----
stanford_theme <- bs_theme(
  version = 5,
  primary = "#8C1515",   # Stanford Cardinal Red
  secondary = "#4D4F53", # Stanford Cool Grey
  success = "#175E54",   # Stanford Dark Green
  info = "#006CB8",      # Stanford Blue
  warning = "#B26F16",   # Stanford Brown
  danger = "#820000",    # Stanford Dark Red
  base_font = font_collection(
    "system-ui", "-apple-system", "BlinkMacSystemFont", "'Segoe UI'",
    "Roboto", "'Helvetica Neue'", "Arial", "sans-serif"
  ),
  heading_font = font_collection(
    "Source Serif Pro", "Georgia", "'Times New Roman'", "Times", "serif"
  ),
  code_font = font_collection(
    "Source Code Pro", "'Courier New'", "Courier", "monospace"
  )
)

# UI ----
ui <- page_navbar(
  title = "2D Random Walk Simulator",
  theme = stanford_theme,
  fillable = TRUE,
  bg = "#8C1515",

  nav_spacer(),
  
  ## Enable dark mode ----
  nav_item(
    input_dark_mode(id = "dark_mode", mode = "light")
  ),
  
  ## Simulation Panel ----
  nav_panel(
    title = "Simulation",
    icon = bs_icon("play-circle"),
    
    ### Sidebar Layout ----
    layout_sidebar(
      sidebar = sidebar(
        title = "Controls",
        open = TRUE,
        accordion(
          multiple = TRUE,
          open = c("Random Walk Settings", "Simulation Controls"),
          
          #### Random Walk Settings ----
          accordion_panel(
            title = "Random Walk Settings",
            
            selectInput("dist_type", "Step Distribution:",
                        choices = c("Cardinal Steps (←/→/↑/↓)" = "orthogonal",
                                    "Diagonal Steps (↖/↘/↗/↙)" = "discrete",
                                    "Normal Distribution" = "normal"),
                        selected = "orthogonal"),
            
            conditionalPanel(
              condition = "input.dist_type != 'discrete'",
              numericInput("steps", 
                           "Number of steps:", 
                           value = 1000,
                           min = 100,
                           max = 10000),
              
              numericInput("step_size", 
                           "Step size:", 
                           value = 1,
                           min = 0.1,
                           max = 5)
            ),
            
            conditionalPanel(
              condition = "input.dist_type == 'normal'",
              numericInput("sigma", 
                           "Standard deviation:", 
                           value = 1,
                           min = 0.1,
                           max = 2)
            ),
            
            conditionalPanel(
              condition = "input.dist_type == 'discrete'",
              numericInput("steps_discrete", 
                           "Number of steps:", 
                           value = 1000,
                           min = 100,
                           max = 10000)
            )
          ),
          
          #### Simulation Controls  ----
          accordion_panel(
            title = "Simulation Controls",
            tags$button(
              id = "single_step",
              type = "button",
              class = "btn btn-secondary w-100 mb-2 action-button",
              list(
                bs_icon("skip-forward"),
                "Take Single Step"
              )
            ),
            uiOutput("control_button"),
            tags$button(
              id = "run_simulation",
              type = "button",
              class = "btn btn-info w-100 mb-2 action-button",
              list(
                bs_icon("fast-forward"),
                "Run Full Simulation"
              )
            ),
            tags$button(
              id = "reset",
              type = "button",
              class = "btn btn-danger w-100 action-button",
              list(
                bs_icon("arrow-counterclockwise"),
                "Reset"
              )
            )
          ),
          
          #### Plot Window Settings ----
          accordion_panel(
            title = "Plot Window Settings",
            sliderInput("x_range",
                        "X-axis range:",
                        min = -30,
                        max = 30,
                        value = c(-30, 30),
                        step = 1),
            sliderInput("y_range",
                        "Y-axis range:",
                        min = -30,
                        max = 30,
                        value = c(-30, 30),
                        step = 1)
          ),
          
          #### Starting Position  ----
          accordion_panel(
            title = "Starting Position",
            numericInput("start_x", 
                         "Starting X:", 
                         value = 0,
                         step = 0.1),
            numericInput("start_y", 
                         "Starting Y:", 
                         value = 0,
                         step = 0.1)
          )
        )
      ),
      
      ### Main content area  ----
      card(
        full_screen = TRUE,
        card_header("Visualization"),
        card_body(
          plotOutput("walkPlot")
        )
      ),
      ### Statistics area ----
      card(
        class = "mt-3",
        card_header("Statistics"),
        layout_column_wrap(
          width = 1/2,
          class = "p-3",
          
          #### Progress/Displacement ----
          value_box(
            title = "Progress",
            value = textOutput("progress_stat", inline = TRUE),
            showcase = bs_icon("graph-up-arrow"),
            theme = "primary"
          ),
          value_box(
            title = "Displacement",
            value = textOutput("displacement_stat", inline = TRUE),
            showcase = bs_icon("arrows-angle-expand"),
            theme = "secondary"
          )
        ),
        layout_column_wrap(
          width = 1/2,
          class = "p-3",
          #### Start/Current Position ----
          value_box(
            title = "Start Position",
            value = textOutput("start_pos_stat", inline = TRUE),
            showcase = bs_icon("geo"),
            theme = "success"
          ),
          value_box(
            title = "Current Position",
            value = textOutput("current_pos_stat", inline = TRUE),
            showcase = bs_icon("geo-fill"),
            theme = "info"
          )
        )
      )
    )
  ),
  
  ## Help Panel ----
  nav_panel(
    title = "Help",
    icon = bs_icon("question-circle"),
    
    card(
      card_header("How to Use the Simulator"),
      card_body(
        ## Simulation Controls Description ----
        h4("Controls"),
        tags$ul(
          tags$li(bs_icon("skip-forward"), " Single Step: Add one step to the walk"),
          tags$li(bs_icon("play"), " Start/Resume: Begin/resume continuous walking"),
          tags$li(bs_icon("pause"), " Pause: Pause continuous walking"),
          tags$li(bs_icon("fast-forward"), " Run Full: Complete all steps instantly"),
          tags$li(bs_icon("arrow-counterclockwise"), " Reset: Clear the walk and start over")
        ),
        
        ## Distribution Types Description ----
        h4("Distribution Types"),
        tags$ul(
          tags$li(strong("Cardinal Steps"), ": Walker can only move left, right, up, or down"),
          tags$li(strong("Diagonal Steps"), ": Walker moves diagonally in one of four directions"),
          tags$li(strong("Normal Distribution"), ": Steps are drawn from a normal distribution, allowing for more natural-looking movement")
        ),
        
        ## Statistics Section Description ----
        h4("Statistics"),
        tags$ul(
          tags$li(strong("Progress"), ": Current step number out of total steps"),
          tags$li(strong("Displacement"), ": Straight-line distance from start to current position"),
          tags$li(strong("Start Position"), ": Initial coordinates of the walker"),
          tags$li(strong("Current Position"), ": Current coordinates of the walker")
        )
      )
    )
  )
)