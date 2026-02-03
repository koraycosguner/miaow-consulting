# Conjoint Analysis Design Tool - Shiny App
#
# TO RUN THIS APP:
#   1. Open this file in RStudio
#   2. Click "Run App" button (top right of editor)
#   OR run in console: shiny::runApp()

# Auto-install required packages if missing
if (!require("shiny", quietly = TRUE)) {
  install.packages("shiny", repos = "https://cloud.r-project.org")
}
if (!require("conjoint", quietly = TRUE)) {
  install.packages("conjoint", repos = "https://cloud.r-project.org")
}

library(shiny)
library(conjoint)

ui <- fluidPage(
  # Custom CSS
  tags$head(
    tags$style(HTML("
      body {
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
        background: #f8fafc;
        padding: 20px;
      }
      .container-fluid {
        max-width: 1000px;
        margin: 0 auto;
      }
      h1 {
        text-align: center;
        color: #1e293b;
        margin-bottom: 5px;
      }
      .subtitle {
        text-align: center;
        color: #64748b;
        margin-bottom: 30px;
      }
      .card {
        background: white;
        border-radius: 12px;
        padding: 20px;
        margin-bottom: 20px;
        box-shadow: 0 1px 3px rgba(0,0,0,0.1);
      }
      .card-title {
        font-size: 1.1rem;
        font-weight: 600;
        margin-bottom: 15px;
        color: #1e293b;
      }
      .btn-primary {
        background: #2563eb;
        border: none;
        padding: 10px 20px;
        border-radius: 8px;
      }
      .btn-primary:hover {
        background: #1d4ed8;
      }
      .btn-success {
        background: #16a34a;
        border: none;
        padding: 10px 20px;
        border-radius: 8px;
      }
      .btn-success:hover {
        background: #15803d;
      }
      .btn-danger {
        background: #dc2626;
        border: none;
        padding: 5px 10px;
        border-radius: 6px;
        font-size: 0.875rem;
      }
      .stat-box {
        background: #f8fafc;
        border-radius: 8px;
        padding: 15px;
        text-align: center;
        margin-bottom: 10px;
      }
      .stat-value {
        font-size: 1.5rem;
        font-weight: 700;
        color: #2563eb;
      }
      .stat-label {
        font-size: 0.85rem;
        color: #64748b;
      }
      .attribute-item {
        background: #f8fafc;
        border-radius: 8px;
        padding: 12px;
        margin-bottom: 10px;
        display: flex;
        justify-content: space-between;
        align-items: center;
      }
      .attribute-name {
        font-weight: 600;
        color: #1e293b;
      }
      .attribute-levels {
        font-size: 0.9rem;
        color: #64748b;
      }
      .level-badge {
        background: #2563eb;
        color: white;
        padding: 2px 8px;
        border-radius: 4px;
        font-size: 0.75rem;
        margin-left: 8px;
      }
      .disclaimer {
        background: #fef3c7;
        border: 1px solid #f59e0b;
        border-radius: 8px;
        padding: 15px;
        margin-bottom: 20px;
        font-size: 0.9rem;
      }
      .disclaimer-title {
        font-weight: 600;
        color: #92400e;
        margin-bottom: 8px;
      }
      .disclaimer ul {
        margin-left: 20px;
        color: #78350f;
        margin-bottom: 0;
      }
      .table {
        font-size: 0.9rem;
      }
      .nav-tabs .nav-link.active {
        background: #2563eb;
        color: white;
        border: none;
      }
      .nav-tabs .nav-link {
        color: #64748b;
        border: none;
        border-radius: 6px;
        margin-right: 5px;
      }
      .help-text {
        font-size: 0.85rem;
        color: #64748b;
        margin-top: 5px;
      }
      pre.r-script {
        background: #1e293b;
        color: #e2e8f0;
        padding: 15px;
        border-radius: 8px;
        font-size: 0.85rem;
        overflow-x: auto;
      }
    "))
  ),

  # Title
  h1("Conjoint Analysis Design Tool"),
  p(class = "subtitle", "Create fractional factorial designs for conjoint studies"),

  # Disclaimer
  div(class = "disclaimer",
    div(class = "disclaimer-title", "Disclaimer"),
    tags$ul(
      tags$li(tags$strong("Results may contain errors."), " The output from this tool may be inaccurate, incomplete, or incorrect. Users should independently verify all results."),
      tags$li(tags$strong("No warranty."), " This tool is provided \"as is\" without any warranty of any kind."),
      tags$li(tags$strong("User assumes all risk."), " Users assume full responsibility for reviewing and validating any output.")
    )
  ),

  # Step 1: Define Attributes
  div(class = "card",
    div(class = "card-title", "1. Define Attributes"),
    fluidRow(
      column(4,
        textInput("attr_name", "Attribute Name", placeholder = "e.g., Brand")
      ),
      column(6,
        textInput("attr_levels", "Levels (comma-separated)", placeholder = "e.g., Apple, Samsung, Google, Sony")
      ),
      column(2,
        div(style = "margin-top: 25px;",
          actionButton("add_attr", "Add", class = "btn-primary")
        )
      )
    ),
    p(class = "help-text", "Enter at least 2 levels per attribute, separated by commas."),

    # Attributes list
    div(style = "margin-top: 20px;",
      uiOutput("attributes_list")
    )
  ),

  # Step 2: Design Statistics (conditional)
  uiOutput("stats_card"),

  # Step 3: Results (conditional)
  uiOutput("results_card")
)

server <- function(input, output, session) {

  # Reactive values to store attributes
  attributes <- reactiveVal(list())
  design_result <- reactiveVal(NULL)

  # Add attribute
  observeEvent(input$add_attr, {
    name <- trimws(input$attr_name)
    levels_str <- trimws(input$attr_levels)

    if (name == "") {
      showNotification("Please enter an attribute name.", type = "error")
      return()
    }

    levels <- trimws(unlist(strsplit(levels_str, ",")))
    levels <- levels[levels != ""]

    if (length(levels) < 2) {
      showNotification("Please enter at least 2 levels, separated by commas.", type = "error")
      return()
    }

    # Check for duplicates
    current <- attributes()
    if (tolower(name) %in% tolower(names(current))) {
      showNotification("An attribute with this name already exists.", type = "error")
      return()
    }

    # Add attribute
    current[[name]] <- levels
    attributes(current)

    # Clear inputs
    updateTextInput(session, "attr_name", value = "")
    updateTextInput(session, "attr_levels", value = "")

    # Clear previous design
    design_result(NULL)
  })

  # Remove attribute
  observeEvent(input$remove_attr, {
    idx <- as.integer(input$remove_attr)
    current <- attributes()
    if (idx <= length(current)) {
      current <- current[-idx]
      attributes(current)
      design_result(NULL)
    }
  })

  # Clear all
  observeEvent(input$clear_all, {
    attributes(list())
    design_result(NULL)
  })

  # Render attributes list
  output$attributes_list <- renderUI({
    attrs <- attributes()

    if (length(attrs) == 0) {
      return(div(style = "text-align: center; padding: 30px; color: #64748b;",
        "No attributes added yet. Add your first attribute above."
      ))
    }

    attr_items <- lapply(seq_along(attrs), function(i) {
      name <- names(attrs)[i]
      levels <- attrs[[i]]
      div(class = "attribute-item",
        div(
          span(class = "attribute-name", name),
          span(class = "level-badge", paste(length(levels), "levels")),
          div(class = "attribute-levels", paste(levels, collapse = ", "))
        ),
        actionButton(
          inputId = "remove_attr",
          label = "Remove",
          class = "btn-danger",
          onclick = sprintf("Shiny.setInputValue('remove_attr', %d, {priority: 'event'})", i)
        )
      )
    })

    do.call(tagList, attr_items)
  })

  # Stats card
  output$stats_card <- renderUI({
    attrs <- attributes()
    if (length(attrs) == 0) return(NULL)

    # Calculate stats
    num_attrs <- length(attrs)
    full_factorial <- prod(sapply(attrs, length))

    div(class = "card",
      div(class = "card-title", "2. Design Statistics"),
      fluidRow(
        column(4,
          div(class = "stat-box",
            div(class = "stat-value", num_attrs),
            div(class = "stat-label", "Attributes")
          )
        ),
        column(4,
          div(class = "stat-box",
            div(class = "stat-value", format(full_factorial, big.mark = ",")),
            div(class = "stat-label", "Full Factorial Profiles")
          )
        ),
        column(4,
          div(class = "stat-box",
            div(class = "stat-value",
              if (!is.null(design_result())) nrow(design_result()$profiles) else "-"
            ),
            div(class = "stat-label", "Fractional Design Profiles")
          )
        )
      ),
      div(style = "margin-top: 15px;",
        actionButton("generate", "Generate Fractional Design", class = "btn-success"),
        actionButton("clear_all", "Clear All Attributes", class = "btn-secondary", style = "margin-left: 10px;")
      )
    )
  })

  # Generate design
  observeEvent(input$generate, {
    attrs <- attributes()

    if (length(attrs) < 2) {
      showNotification("Please add at least 2 attributes.", type = "error")
      return()
    }

    tryCatch({
      # Create full factorial
      fullprofiles <- expand.grid(attrs, stringsAsFactors = FALSE)

      # Generate fractional design
      reducedprofiles <- caFactorialDesign(data = fullprofiles, type = "fractional")

      # Store result
      design_result(list(
        profiles = reducedprofiles,
        attributes = attrs
      ))

      showNotification(
        paste("Design generated with", nrow(reducedprofiles), "profiles."),
        type = "message"
      )

    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })

  # Results card
  output$results_card <- renderUI({
    result <- design_result()
    if (is.null(result)) return(NULL)

    div(class = "card",
      div(class = "card-title", "3. Design Profiles"),
      tabsetPanel(
        tabPanel("Profiles",
          div(style = "margin-top: 15px;",
            tableOutput("profiles_table"),
            p(style = "font-size: 0.85rem; color: #64748b; font-style: italic;",
              "Select the table and copy (Ctrl+C / Cmd+C) to paste into Excel.")
          )
        ),
        tabPanel("Dummy Coded",
          div(style = "margin-top: 15px;",
            tableOutput("dummy_table"),
            p(style = "font-size: 0.85rem; color: #64748b; font-style: italic;",
              "Reference levels (first level of each attribute) are omitted.")
          )
        ),
        tabPanel("R Script",
          div(style = "margin-top: 15px;",
            p(style = "color: #64748b;", "Use this R script to reproduce the design:"),
            verbatimTextOutput("r_script"),
            downloadButton("download_script", "Download R Script", class = "btn-secondary")
          )
        )
      )
    )
  })

  # Profiles table
  output$profiles_table <- renderTable({
    result <- design_result()
    if (is.null(result)) return(NULL)

    profiles <- result$profiles
    profiles <- cbind(Profile = 1:nrow(profiles), profiles)
    profiles
  }, rownames = FALSE)

  # Dummy coded table
  output$dummy_table <- renderTable({
    result <- design_result()
    if (is.null(result)) return(NULL)

    profiles <- result$profiles
    attrs <- result$attributes

    # Create dummy variables
    dummy_df <- data.frame(Profile = 1:nrow(profiles))

    for (attr_name in names(attrs)) {
      levels <- attrs[[attr_name]]
      # Skip first level (reference)
      for (i in 2:length(levels)) {
        col_name <- paste0(attr_name, "_", gsub(" ", "_", levels[i]))
        dummy_df[[col_name]] <- as.integer(profiles[[attr_name]] == levels[i])
      }
    }

    dummy_df
  }, rownames = FALSE)

  # R Script
  output$r_script <- renderText({
    result <- design_result()
    if (is.null(result)) return("")

    attrs <- result$attributes

    # Build attribute definitions
    attr_defs <- sapply(names(attrs), function(name) {
      clean_name <- gsub(" ", "_", name)
      levels_str <- paste0('"', attrs[[name]], '"', collapse = ", ")
      paste0(clean_name, " = c(", levels_str, ")")
    })

    attr_names <- gsub(" ", "_", names(attrs))

    script <- paste0(
      "# Conjoint Analysis Fractional Factorial Design\n",
      "# Generated by Conjoint Design Tool\n\n",
      "# Install and load required packages\n",
      "if (!require(conjoint)) install.packages(\"conjoint\")\n",
      "library(conjoint)\n\n",
      "# Define attributes and levels\n",
      paste(attr_defs, collapse = "\n"), "\n\n",
      "# Create full factorial profiles\n",
      "fullprofiles = expand.grid(", paste(attr_names, collapse = ", "), ")\n\n",
      "# Generate fractional factorial design\n",
      "reducedprofiles = caFactorialDesign(data = fullprofiles, type = \"fractional\")\n\n",
      "# View the design\n",
      "print(reducedprofiles)\n\n",
      "# Number of profiles\n",
      "cat(\"Number of profiles:\", nrow(reducedprofiles), \"\\n\")\n"
    )

    script
  })

  # Download R script
  output$download_script <- downloadHandler(
    filename = function() {
      "conjoint_design.R"
    },
    content = function(file) {
      result <- design_result()
      if (is.null(result)) return()

      attrs <- result$attributes

      attr_defs <- sapply(names(attrs), function(name) {
        clean_name <- gsub(" ", "_", name)
        levels_str <- paste0('"', attrs[[name]], '"', collapse = ", ")
        paste0(clean_name, " = c(", levels_str, ")")
      })

      attr_names <- gsub(" ", "_", names(attrs))

      script <- paste0(
        "# Conjoint Analysis Fractional Factorial Design\n",
        "# Generated by Conjoint Design Tool\n\n",
        "if (!require(conjoint)) install.packages(\"conjoint\")\n",
        "library(conjoint)\n\n",
        paste(attr_defs, collapse = "\n"), "\n\n",
        "fullprofiles = expand.grid(", paste(attr_names, collapse = ", "), ")\n\n",
        "reducedprofiles = caFactorialDesign(data = fullprofiles, type = \"fractional\")\n\n",
        "print(reducedprofiles)\n",
        "cat(\"Number of profiles:\", nrow(reducedprofiles), \"\\n\")\n"
      )

      writeLines(script, file)
    }
  )
}

shinyApp(ui = ui, server = server)
