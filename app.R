library(shiny)
library(DT)
library(dplyr)
library(readr)
library(stringr)
library(htmltools)
library(googledrive)
library(tibble)

# ---- Google Drive Config ----
DRIVE_FILE_NAME <- "recipes.csv"  # Name of the file in your Drive

# Load recipes from Drive
load_recipes <- function() {
  tmp <- tempfile(fileext = ".csv")
  drive_download(DRIVE_FILE_NAME, path = tmp, overwrite = TRUE)
  if (file.exists(tmp)) {
    read_csv(tmp, show_col_types = FALSE)
  } else {
    tibble(id = character(), name = character(), link = character(),
           ingredients = character(), n_ingredients = integer(), added_at = character())
  }
}

# Save recipes to Drive
save_recipes <- function(df) {
  tmp <- tempfile(fileext = ".csv")
  write_csv(df, tmp)
  # If file exists, update; else upload
  if (length(drive_find(DRIVE_FILE_NAME)$id) > 0) {
    drive_update(DRIVE_FILE_NAME, media = tmp)
  } else {
    drive_upload(tmp, name = DRIVE_FILE_NAME)
  }
}

# ---- UI ----
ui <- fluidPage(
  titlePanel("🍳 Recipe Manager (Google Drive)"),
  sidebarLayout(
    sidebarPanel(
      textInput("name", "Recipe name"),
      textInput("link", "Link"),
      textAreaInput("ingredients", "Ingredients (comma-separated)"),
      actionButton("add", "Add Recipe", class = "btn-primary")
    ),
    mainPanel(
      DTOutput("table"),
      tags$hr(),
      actionButton("gen_menu", "🎲 Generate Weekly Menu (5 recipes)", class = "btn-success"),
      br(), br(),
      DTOutput("menu"),
      br(),
      downloadButton("download_menu", "Download Menu (CSV)")
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  # Minimal patch: ensure added_at is character after load (fixes bind_rows type mismatch)
  recipes <- reactiveVal({
    df <- load_recipes()
    dplyr::mutate(df, added_at = as.character(added_at))
  })
  
  observeEvent(input$add, {
    # NOTE: Keeping your current ingredient handling; feel free to improve later
    new_row <- tibble(
      id = as.character(Sys.time()),
      name = input$name,
      link = input$link,
      ingredients = input$ingredients,
      n_ingredients = length(strsplit(input$ingredients, ",")[[1]]),
      # Minimal patch: write character ISO-8601 in UTC
      added_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    )
    updated <- bind_rows(recipes(), new_row)
    recipes(updated)
    save_recipes(updated)
  })
  
  output$table <- renderDT({
    datatable(recipes(), options = list(pageLength = 10), rownames = FALSE)
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
      weekly_menu(tibble(
        name = character(), link = character(),
        ingredients = character(), n_ingredients = integer()
      ))
      return(NULL)
    }
    
    df <- df %>%
      filter(!is.na(name), nzchar(trimws(name))) %>%
      distinct(name, .keep_all = TRUE)
    
    if (nrow(df) == 0) {
      showNotification("All recipes have empty names. Please name your recipes.", type = "warning")
      weekly_menu(tibble(
        name = character(), link = character(),
        ingredients = character(), n_ingredients = integer()
      ))
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
