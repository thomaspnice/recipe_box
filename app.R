# app.R
library(shiny)
library(DT)
library(dplyr)
library(readr)
library(stringr)
library(htmltools)
library(tibble)
library(pins)

rsconnect::writeManifest()


# ------------- Storage via pins (local folder board) ----------------
PIN_NAME  <- "recipes"
BOARD_DIR <- "data"  # persists within the Posit Cloud project

dir.create(BOARD_DIR, showWarnings = FALSE, recursive = TRUE)
board <- board_folder(BOARD_DIR, versioned = TRUE)

empty_recipes <- function() {
  tibble(
    id            = character(),
    name          = character(),
    link          = character(),
    ingredients   = character(),
    n_ingredients = integer(),
    added_at      = character()
  )
}

# Load recipes from pin
load_recipes <- function() {
  if (pin_exists(board, PIN_NAME)) {
    df <- pin_read(board, PIN_NAME)
    df <- as_tibble(df)
  } else {
    df <- empty_recipes()
  }
  df %>%
    mutate(
      added_at      = as.character(added_at),
      n_ingredients = as.integer(n_ingredients)
    )
}

# Save recipes to pin (CSV artifact, with versions)
save_recipes <- function(df) {
  df <- df %>%
    mutate(
      added_at      = as.character(added_at),
      n_ingredients = as.integer(n_ingredients)
    )
  pin_write(
    board,
    df,
    name        = PIN_NAME,
    type        = "csv",
    title       = "Recipe Manager data",
    description = "Data for Shiny Recipe Manager (stored via pins)"
  )
}

# ---- UI ----
ui <- fluidPage(
  titlePanel("🍳 Recipe Manager (pins storage)"),
  sidebarLayout(
    sidebarPanel(
      textInput("name", "Recipe name"),
      textInput("link", "Link"),
      textAreaInput("ingredients", "Ingredients (comma-separated)"),
      actionButton("add", "Add Recipe", class = "btn btn-primary"),
      tags$hr(),
      actionButton("delete", "🗑️ Delete Selected", class = "btn btn-danger")
    ),
    mainPanel(
      DTOutput("table"),
      tags$hr(),
      actionButton("gen_menu", "🎲 Generate Weekly Menu (5 recipes)", class = "btn btn-success"),
      br(), br(),
      DTOutput("menu"),
      br(),
      downloadButton("download_menu", "Download Menu (CSV)")
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  # Load existing data
  recipes <- reactiveVal(load_recipes())
  
  # Optional one-time import from a local CSV if you have one
  # if (file.exists("recipes.csv") && nrow(recipes()) == 0) {
  #   imported <- readr::read_csv("recipes.csv", show_col_types = FALSE)
  #   imported <- imported %>%
  #     mutate(
  #       added_at      = as.character(added_at),
  #       n_ingredients = as.integer(n_ingredients)
  #     )
  #   recipes(imported)
  #   save_recipes(imported)
  # }
  
  # Add new recipe
  observeEvent(input$add, {
    nm  <- trimws(input$name %||% "")
    lnk <- trimws(input$link %||% "")
    ing <- trimws(input$ingredients %||% "")
    
    if (!nzchar(nm)) {
      showNotification("Please provide a recipe name.", type = "warning")
      return(NULL)
    }
    
    n_ing <- if (nzchar(ing)) length(strsplit(ing, ",", fixed = TRUE)[[1]]) else 0L
    
    new_row <- tibble(
      id            = as.character(Sys.time()),
      name          = nm,
      link          = lnk,
      ingredients   = ing,
      n_ingredients = as.integer(n_ing),
      added_at      = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    )
    
    updated <- bind_rows(recipes(), new_row)
    recipes(updated)
    save_recipes(updated)
    showNotification("Recipe added.", type = "message")
    updateTextInput(session, "name", value = "")
    updateTextInput(session, "link", value = "")
    updateTextAreaInput(session, "ingredients", value = "")
  })
  
  # Inline editing in the table
  # We'll allow editing of: name, link, ingredients, n_ingredients
  # We'll disable editing of: id, added_at
  output$table <- renderDT({
    datatable(
      recipes(),
      options = list(pageLength = 10),
      rownames = FALSE,
      selection = "multiple",
      editable = list(
        target = "cell",
        disable = list(
          columns = which(names(recipes()) %in% c("id", "added_at")) - 1L  # 0-based in JS
        )
      )
    )
  }, server = FALSE)
  
  # Apply edits
  observeEvent(input$table_cell_edit, {
    info <- input$table_cell_edit
    df   <- recipes()
    
    # DT sends 0-based column index, convert to 1-based for R
    j <- info$col + 1L
    i <- info$row
    
    colname <- names(df)[j]
    newval  <- info$value
    
    # Coerce type safely
    if (colname == "n_ingredients") {
      newval <- suppressWarnings(as.integer(newval))
      if (is.na(newval)) {
        showNotification("`n_ingredients` must be an integer.", type = "warning")
        return(NULL)
      }
    } else {
      newval <- as.character(newval)
    }
    
    df[i, j] <- newval
    recipes(df)
    save_recipes(df)
  })
  
  # Delete selected rows
  observeEvent(input$delete, {
    sel <- input$table_rows_selected
    if (!length(sel)) {
      showNotification("Select at least one row to delete.", type = "message")
      return(NULL)
    }
    df <- recipes()
    df <- df[-sel, , drop = FALSE]
    recipes(df)
    save_recipes(df)
    showNotification(sprintf("Deleted %d row(s).", length(sel)), type = "message")
  })
  
  # ------- Weekly Menu Feature -------
  weekly_menu <- reactiveVal(
    tibble(name = character(), link = character(),
           ingredients = character(), n_ingredients = integer())
  )
  
  observeEvent(input$gen_menu, {
    df <- recipes()
    
    if (nrow(df) == 0) {
      showNotification("No recipes found. Add some first!", type = "warning")
      weekly_menu(empty_recipes() %>% select(name, link, ingredients, n_ingredients))
      return(NULL)
    }
    
    df <- df %>%
      filter(!is.na(name), nzchar(trimws(name))) %>%
      distinct(name, .keep_all = TRUE)
    
    if (nrow(df) == 0) {
      showNotification("All recipes have empty names. Please name your recipes.", type = "warning")
      weekly_menu(empty_recipes() %>% select(name, link, ingredients, n_ingredients))
      return(NULL)
    }
    
    k <- min(5L, nrow(df))
    if (k < 5L) {
      showNotification(paste0("Only ", k, " recipe(s) available. Add more to get 5."), type = "message")
    }
    
    df_disp <- df %>%
      slice_sample(n = k) %>%
      mutate(
        link = if_else(
          !is.na(link) & nzchar(trimws(link)),
          sprintf('<a href="%s" target="_blank">Open</a>', htmltools::htmlEscape(trimws(link))),
          ""
        )
      ) %>%
      select(name, link, ingredients, n_ingredients)
    
    weekly_menu(df_disp)
  })
  
  output$menu <- renderDT({
    datatable(
      weekly_menu(),
      escape = FALSE,
      options = list(pageLength = 5, dom = "tip"),
      rownames = FALSE
    )
  }, server = FALSE)
  
  output$download_menu <- downloadHandler(
    filename = function() paste0("weekly_menu_", Sys.Date(), ".csv"),
    content = function(file) {
      wm <- weekly_menu()
      if (nrow(wm) == 0) {
        write_csv(tibble(
          name = character(), link = character(),
          ingredients = character(), n_ingredients = integer()
        ), file)
      } else {
        url_from_anchor <- function(x) {
          m <- regmatches(x, regexpr('href="[^"]*"', x))
          ifelse(length(m) == 0, "", sub('href="([^"]*)"', "\\1", m))
        }
        export <- wm %>%
          mutate(link = vapply(link, url_from_anchor, character(1))) %>%
          mutate(link = ifelse(is.na(link), "", link))
        write_csv(export, file)
      }
    }
  )
}

shinyApp(ui, server)
