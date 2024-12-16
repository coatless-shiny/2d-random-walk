library(shiny)
library(ggplot2)
library(bslib)
library(bsicons)

# Server ----
server <- function(input, output, session) {
  ## Initialize data globally with default values ----
  walk_data <<- data.frame(
    step = 0,
    x = 0,
    y = 0
  )
  
  ## Reactive values for state ----
  values <- reactiveValues(
    is_running = FALSE,
    button_state = "start"
  )
  
  ## Reactive timer with 1000ms interval  ----
  autoInvalidate <- reactiveTimer(1000)
  
  ## Boundary reflection  ----
  reflect_position <- function(pos_x, pos_y) {
    x_min <- input$x_range[1]
    x_max <- input$x_range[2]
    y_min <- input$y_range[1]
    y_max <- input$y_range[2]
    
    if (pos_x < x_min) {
      pos_x <- x_min + (x_min - pos_x)
    } else if (pos_x > x_max) {
      pos_x <- x_max - (pos_x - x_max)
    }
    
    if (pos_y < y_min) {
      pos_y <- y_min + (y_min - pos_y)
    } else if (pos_y > y_max) {
      pos_y <- y_max - (pos_y - y_max)
    }
    
    return(list(x = pos_x, y = pos_y))
  }
  
  # Reset walk data  ----
  reset_walk_data <- function() {
    walk_data <<- data.frame(
      step = 0,
      x = input$start_x,
      y = input$start_y
    )
    values$is_running <- FALSE
    values$button_state <- "start"
  }
  
  ## Observers ----
  
  ### Observe changes in starting position  ----
  observeEvent(c(input$start_x, input$start_y), {
    reset_walk_data()
  })
  
  ### Observe changes in graph window  ----
  observeEvent(c(input$x_range, input$y_range), {
    reset_walk_data()
  })
  
  ### Observe Reset button ----
  observeEvent(input$reset, {
    reset_walk_data()
  })
  
  ### Observe changes in distribution type ----
  observeEvent(input$dist_type, {
    reset_walk_data()
  })
  
  ### Observe control button ----
  observeEvent(input$control_button, {
    if (values$button_state == "start") {
      values$is_running <- TRUE
      values$button_state <- "pause"
    } else if (values$button_state == "pause") {
      values$is_running <- FALSE
      values$button_state <- "resume"
    } else {
      values$is_running <- TRUE
      values$button_state <- "pause"
    }
  })
  
  ### Observe single step button ----
  observeEvent(input$single_step, {
    if (nrow(walk_data) <= get_total_steps()) {
      take_step()
    }
  })
  
  ### Observe run simulation button ----
  observeEvent(input$run_simulation, {
    while ((nrow(walk_data) - 1) < get_total_steps()) {  # Subtract 1 to account for initial position
      take_step()
    }
    values$is_running <- FALSE
    values$button_state <- "start"
  })
  
  ## Update the current_state reactive ----
  current_state <- reactive({
    autoInvalidate()
    
    req(nrow(walk_data) > 0)
    start_point <- c(walk_data$x[1], walk_data$y[1])
    end_point <- c(walk_data$x[nrow(walk_data)], walk_data$y[nrow(walk_data)])
    displacement <- sqrt(sum((end_point - start_point)^2))
    
    list(
      current_step = nrow(walk_data) - 1,  # Subtract 1 to account for initial position
      total_steps = get_total_steps(),
      start_point = start_point,
      end_point = end_point,
      displacement = displacement,
      data = walk_data
    )
  })
  
  
  ## Total steps based on distribution type ----
  get_total_steps <- reactive({
    if (input$dist_type == "discrete") {
      input$steps_discrete
    } else {
      input$steps
    }
  })
  
  ## Step generation function ----
  take_step <- function() {
    current_steps <- nrow(walk_data) - 1  # Subtract 1 to account for initial position
    if (current_steps < get_total_steps()) {
      last_pos <- walk_data[nrow(walk_data), ]
      
      # Generate step based on selected distribution
      if (input$dist_type == "orthogonal") {
        # Randomly choose horizontal or vertical movement
        if (runif(1) < 0.5) {
          # Horizontal movement
          dx <- sample(c(-1, 1), 1) * input$step_size
          dy <- 0
        } else {
          # Vertical movement
          dx <- 0
          dy <- sample(c(-1, 1), 1) * input$step_size
        }
      } else if (input$dist_type == "normal") {
        dx <- rnorm(1, 0, input$sigma)
        dy <- rnorm(1, 0, input$sigma)
      } else { # discrete
        dx <- sample(c(-1, 1), 1)
        dy <- sample(c(-1, 1), 1)
      }
      
      new_x <- last_pos$x + dx
      new_y <- last_pos$y + dy
      
      # Apply boundary reflection
      reflected_pos <- reflect_position(new_x, new_y)
      
      new_pos <- data.frame(
        step = last_pos$step + 1,
        x = reflected_pos$x,
        y = reflected_pos$y
      )
      
      walk_data <<- rbind(walk_data, new_pos)
    }
  }
  
  ## Update walk function ----
  update_walk <- function() {
    if (values$is_running && nrow(walk_data) <= get_total_steps()) {
      take_step()
    } else if (nrow(walk_data) > get_total_steps()) {
      values$is_running <- FALSE
      values$button_state <- "start"
    }
  }
  
  ## Output functions ----
  
  ### Dynamic UI for control button ----
  output$control_button <- renderUI({
    btn_class <- switch(values$button_state,
                        "start" = "btn-success",
                        "pause" = "btn-warning",
                        "resume" = "btn-success"
    )
    
    btn_icon <- switch(values$button_state,
                       "start" = bs_icon("play"),
                       "pause" = bs_icon("pause"),
                       "resume" = bs_icon("play")
    )
    
    btn_label <- switch(values$button_state,
                        "start" = "Start Random Walk",
                        "pause" = "Pause",
                        "resume" = "Resume"
    )
    
    tags$button(
      id = "control_button",
      type = "button",
      class = paste("btn", btn_class, "w-100 mb-2 action-button"),
      list(
        btn_icon,
        btn_label
      )
    )
  })
  
  ### Show step progress ----
  output$progress_stat <- renderText({
    state <- current_state()
    sprintf("%d / %d steps", state$current_step, state$total_steps)
  })
  
  ### Show distance from start ----
  output$displacement_stat <- renderText({
    state <- current_state()
    sprintf("%.2f units", state$displacement)
  })
  
  ### Show starting location ----
  output$start_pos_stat <- renderText({
    state <- current_state()
    sprintf("(%.2f, %.2f)", state$start_point[1], state$start_point[2])
  })
  
  ### Show current location ----
  output$current_pos_stat <- renderText({
    state <- current_state()
    sprintf("(%.2f, %.2f)", state$end_point[1], state$end_point[2])
  })
  
  ### Show visualization of walk trajectory ----
  output$walkPlot <- renderPlot({
    state <- current_state()
    
    if (values$is_running) {
      update_walk()
    }
    
    # Adjust colors based on dark mode
    is_dark <- !is.null(input$dark_mode) && input$dark_mode == "dark"
    
    colors <- if (is_dark) {
      list(
        grid = "#404040",
        text = "white",
        boundary = "#505050"
      )
    } else {
      list(
        grid = "#e0e0e0",
        text = "#4D4F53",
        boundary = "#4D4F53"
      )
    }
    
    p <- ggplot() +
      coord_fixed(xlim = input$x_range, 
                  ylim = input$y_range) +
      theme_minimal() +
      theme(
        plot.background = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(color = "#8C1515", face = "bold"),
        plot.subtitle = element_text(color = colors$text),
        axis.text = element_text(color = colors$text),
        axis.title = element_text(color = colors$text),
        panel.grid.major = element_line(color = colors$grid),
        panel.grid.minor = element_blank()
      ) +
      labs(title = "2D Random Walk",
           subtitle = paste("Current step:", state$current_step, "/", state$total_steps),
           x = "X Position",
           y = "Y Position")
    
    p <- p + geom_rect(aes(xmin = input$x_range[1], xmax = input$x_range[2], 
                           ymin = input$y_range[1], ymax = input$y_range[2]),
                       color = colors$boundary, fill = NA, linetype = "dashed")
    
    if (nrow(state$data) > 1) {
      p <- p + geom_path(data = state$data, aes(x = x, y = y), 
                         color = "#8C1515", 
                         alpha = if (is_dark) 0.8 else 0.6)
    }
    
    p <- p + 
      geom_point(data = state$data[1,], aes(x = x, y = y), color = "#175E54", size = 3) +
      geom_point(data = state$data[nrow(state$data),], aes(x = x, y = y), color = "#820000", size = 3)
    
    p
  }, 
  bg = "transparent" # Required for dark mode
  )
}