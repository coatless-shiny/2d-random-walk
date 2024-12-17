# server.R
library(shiny)
library(ggplot2)
library(bslib)
library(bsicons)

# Server ----
server <- function(input, output, session) {
  ## Initialize data globally with default values ----
  walk_data <<- data.frame(
    walk_id = integer(),
    step = integer(),
    x = numeric(),
    y = numeric()
  )
  
  ## Initialize current walk ID ----
  current_walk_id <<- 0
  
  ## Color palette for different walks ----
  stanford_colors <- c(
    "#8C1515", # Cardinal Red
    "#175E54", # Dark Green
    "#006CB8", # Blue
    "#B26F16", # Brown
    "#820000", # Dark Red
    "#3F3C30", # Brown
    "#53284F", # Purple
    "#007C92"  # Turquoise
  )
  
  ## Reactive values for state ----
  values <- reactiveValues(
    is_running = FALSE,
    button_state = "start",
    should_step = FALSE,     # Renamed for clarity - controls step updates
    walks_changed = FALSE    # New flag for walk addition/removal
  )
  
  ## Reactive timer with 1000ms interval ----
  observe({
    if (values$is_running) {
      invalidateLater(1000)
      values$should_step <- TRUE
    }
  })
  
  ## Reactive timer with 1000ms interval ----
  observe({
    if (values$is_running) {
      invalidateLater(1000)
      values$should_update <- TRUE
    }
  })
  
  ## Boundary reflection ----
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
  
  ## Reset walk data ----
  reset_walk_data <- function() {
    walk_data <<- data.frame(
      walk_id = integer(),
      step = integer(),
      x = numeric(),
      y = numeric()
    )
    current_walk_id <<- 0
    values$is_running <- FALSE
    values$button_state <- "start"
  }
  
  ## Create new walk ----
  create_new_walk <- function() {
    current_walk_id <<- current_walk_id + 1
    new_walk <- data.frame(
      walk_id = current_walk_id,
      step = 0,
      x = input$start_x,
      y = input$start_y
    )
    walk_data <<- rbind(walk_data, new_walk)
  }
  
  ## Take step for all active walks ----
  take_step <- function() {
    if (nrow(walk_data) == 0) return()
    
    # Get unique walk IDs
    walk_ids <- unique(walk_data$walk_id)
    
    for (id in walk_ids) {
      walk_steps <- sum(walk_data$walk_id == id)
      
      if (walk_steps <= get_total_steps()) {
        last_pos <- walk_data[walk_data$walk_id == id,][walk_steps,]
        
        # Generate step based on selected distribution
        if (input$dist_type == "orthogonal") {
          if (runif(1) < 0.5) {
            dx <- sample(c(-1, 1), 1) * input$step_size
            dy <- 0
          } else {
            dx <- 0
            dy <- sample(c(-1, 1), 1) * input$step_size
          }
        } else if (input$dist_type == "normal") {
          dx <- rnorm(1, 0, input$sigma)
          dy <- rnorm(1, 0, input$sigma)
        } else {
          dx <- sample(c(-1, 1), 1)
          dy <- sample(c(-1, 1), 1)
        }
        
        new_x <- last_pos$x + dx
        new_y <- last_pos$y + dy
        
        reflected_pos <- reflect_position(new_x, new_y)
        
        new_pos <- data.frame(
          walk_id = id,
          step = last_pos$step + 1,
          x = reflected_pos$x,
          y = reflected_pos$y
        )
        
        walk_data <<- rbind(walk_data, new_pos)
      }
    }
  }
  
  ## Observers ----
  
  ### Reset walk data ----
  observeEvent(input$reset, {
    reset_walk_data()
    values$walks_changed <- TRUE
  })
  
  ### Add new walk ----
  observeEvent(input$add_walk, {
    create_new_walk()
    values$walks_changed <- TRUE  # Trigger update to show new walk
  })
  
  ## Start/Pause/Resume button ----
  observeEvent(input$control_button, {
    if (values$button_state == "start") {
      if (nrow(walk_data) == 0) {
        create_new_walk()
        values$walks_changed <- TRUE
      }
      values$is_running <- TRUE
      values$button_state <- "pause"
      values$should_step <- TRUE  # Ensure first step happens immediately
    } else if (values$button_state == "pause") {
      values$is_running <- FALSE
      values$button_state <- "resume"
    } else {
      values$is_running <- TRUE
      values$button_state <- "pause"
      values$should_step <- TRUE  # Ensure step happens when resuming
    }
  })
  
  ### Take single step ----
  observeEvent(input$single_step, {
    if (nrow(walk_data) == 0) {
      create_new_walk()
      values$walks_changed <- TRUE
    }
    values$should_step <- TRUE  # Set flag to allow step update
  })
  
  ### Run full simulation ----
  observeEvent(input$run_simulation, {
    if (nrow(walk_data) == 0) {
      create_new_walk()
      values$walks_changed <- TRUE
    }
    while(any(tapply(walk_data$walk_id, walk_data$walk_id, length) <= get_total_steps())) {
      take_step()
    }
    values$is_running <- FALSE
    values$button_state <- "start"
    values$walks_changed <- TRUE  # Ensure final state is shown
  })
  
  ## Total steps based on distribution type ----
  get_total_steps <- reactive({
    if (input$dist_type == "discrete") {
      input$steps_discrete
    } else {
      input$steps
    }
  })
  
  ## Current state reactive ----
  current_state <- reactive({
    # Take steps only if we should update steps
    if (values$should_step) {
      take_step()
      values$should_step <- FALSE
    }
    
    # Always recalculate state when walks change
    if (values$walks_changed) {
      values$walks_changed <- FALSE
    }
    
    if (nrow(walk_data) == 0) return(NULL)
    
    walk_ids <- unique(walk_data$walk_id)
    walk_stats <- lapply(walk_ids, function(id) {
      walk <- walk_data[walk_data$walk_id == id,]
      start_point <- c(walk$x[1], walk$y[1])
      end_point <- c(walk$x[nrow(walk)], walk$y[nrow(walk)])
      displacement <- sqrt(sum((end_point - start_point)^2))
      
      color_index <- ((id - 1) %% length(stanford_colors)) + 1
      
      list(
        id = id,
        color = stanford_colors[color_index],
        current_step = nrow(walk) - 1,
        total_steps = get_total_steps(),
        start_point = start_point,
        end_point = end_point,
        displacement = displacement,
        data = walk
      )
    })
    
    walk_stats
  })
  
  ## Output Functions ----
  
  ### Control button for starting/pausing/resuming ----
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
  
  ## Display random walk statistics ----
  output$stats_table <- renderUI({
    state <- current_state()
    if (is.null(state)) {
      return(div(
        class = "p-3 text-center text-muted",
        "No walks active"
      ))
    }
    
    # Create a single table for all walks
    div(
      class = "stats-container p-2",
      lapply(state, function(walk) {
        div(
          class = "walk-stats mb-3 p-3 border rounded",
          style = sprintf("border-color: %s !important;", walk$color),
          
          # Header
          div(
            class = "d-flex align-items-center mb-2",
            div(
              class = "walk-indicator me-2",
              style = sprintf("width: 12px; height: 12px; border-radius: 50%%; background-color: %s;", walk$color)
            ),
            h5(
              class = "m-0",
              sprintf("Walk #%d", walk$id)
            )
          ),
          
          # Stats grid
          div(
            class = "row g-2",
            div(
              class = "col-6",
              div(
                class = "stat-box p-2 bg-light rounded",
                div(class = "stat-label small text-muted", "Progress"),
                div(class = "stat-value", 
                    sprintf("%d / %d steps", walk$current_step, walk$total_steps))
              )
            ),
            div(
              class = "col-6",
              div(
                class = "stat-box p-2 bg-light rounded",
                div(class = "stat-label small text-muted", "Displacement"),
                div(class = "stat-value",
                    sprintf("%.2f units", walk$displacement))
              )
            )
          )
        )
      })
    )
  })
  
  ## Show visualization of walk trajectories ----
  output$walkPlot <- renderPlot({
    state <- current_state()
    
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
    
    if (is.null(state)) {
      
      g <- ggplot() + 
        annotate("text", x = 0, y = 0, 
                 label = "No walks active", size = 6,
                 color = colors$text) +
        theme_void() 
        coord_cartesian(xlim = c(-1, 1), ylim = c(-1, 1))

      return(g)
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
      labs(title = "Multiple 2D Random Walks",
           subtitle = sprintf("Active walks: %d", length(state)),
           x = "X Position",
           y = "Y Position")
    
    p <- p + geom_rect(aes(xmin = input$x_range[1], 
                           xmax = input$x_range[2],
                           ymin = input$y_range[1], 
                           ymax = input$y_range[2]),
                       color = colors$boundary, 
                       fill = NA, 
                       linetype = "dashed")
    
    # Add paths and points for each walk
    for (walk in state) {
      if (nrow(walk$data) > 1) {
        p <- p + geom_path(data = walk$data, 
                           aes(x = x, y = y),
                           color = walk$color,
                           alpha = if (is_dark) 0.8 else 0.6)
      }
      
      p <- p + 
        geom_point(data = walk$data[1,], 
                   aes(x = x, y = y),
                   color = walk$color,
                   size = 3,
                   alpha = 0.7) +
        geom_point(data = walk$data[nrow(walk$data),],
                   aes(x = x, y = y),
                   color = walk$color,
                   size = 3)
    }
    
    p
  },
  bg = "transparent"
  )
}