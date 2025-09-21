# app.R
library(shiny)
library(DT)
library(dplyr)
library(readr)
library(stringr)
library(htmltools)
library(tibble)
library(httr)
library(jsonlite)
library(shinyjs)
library(base64enc)

rsconnect::writeManifest()

normalize_recipes_df <- function(df_in) {
  # Ensure we only work with known columns, create any missing
  needed <- c("id","name","link","ingredients","n_ingredients","added_at")
  for (nm in needed) if (!nm %in% names(df_in)) df_in[[nm]] <- NA
  
  # Keep only expected columns and coerce types
  out <- tibble::tibble(
    #id            = as.character(df_in$id),
    name          = as.character(df_in$name),
    link          = as.character(df_in$link),
    ingredients   = as.character(df_in$ingredients),
    n_ingredients = suppressWarnings(as.integer(df_in$n_ingredients)),
    added_at      = as.character(df_in$added_at)
  )
}

# ------------- Storage via GitHub ----------------
GITHUB_OWNER <- "thomaspnice" 
GITHUB_REPO <- "recipe_box"
DATA_FILE_PATH <- "recipes.csv"

# GitHub API helper functions
get_github_token <- function() {
  token <- Sys.getenv("GITHUB_TOKEN")
  if (token == "") {
    stop("GITHUB_TOKEN environment variable not set")
  }
  return(token)
}

# Get file from GitHub (with SHA for updates)
get_github_file <- function(owner, repo, path, token) {
  url <- paste0("https://api.github.com/repos/", owner, "/", repo, "/contents/", path)
  
  response <- GET(
    url,
    add_headers(
      Authorization = paste("token", token),
      Accept = "application/vnd.github.v3+json"
    )
  )
  
  if (status_code(response) == 404) {
    return(NULL)  # File doesn't exist
  }
  
  if (status_code(response) != 200) {
    stop("Failed to fetch file from GitHub: ", content(response, "text"))
  }
  
  content(response)
}


# Update/create file on GitHub
put_github_file <- function(owner, repo, path, content_data, message, token, sha = NULL) {
  url <- paste0("https://api.github.com/repos/", owner, "/", repo, "/contents/", path)
  
  # Encode content as base64
  encoded_content <- base64enc::base64encode(charToRaw(content_data))
  
  body <- list(
    message = message,
    content = encoded_content
  )
  
  if (!is.null(sha)) {
    body$sha <- sha
  }
  
  response <- PUT(
    url,
    add_headers(
      Authorization = paste("token", token),
      Accept = "application/vnd.github.v3+json"
    ),
    body = body,
    encode = "json"
  )
  
  if (!status_code(response) %in% c(200, 201)) {
    stop("Failed to update file on GitHub: ", content(response, "text"))
  }
  
  content(response)
}

empty_recipes <- function() {
  tibble(
    #id            = character(),
    name          = character(),
    link          = character(),
    ingredients   = character(),
    n_ingredients = integer(),
    added_at      = character()
  )
}

# Load recipes from GitHub
load_recipes <- function() {
  tryCatch({
    token <- get_github_token()
    file_info <- get_github_file(GITHUB_OWNER, GITHUB_REPO, DATA_FILE_PATH, token)
    
    if (is.null(file_info)) {
      return(empty_recipes())
    }
    
    # Decode base64 content
    content_decoded <- rawToChar(base64enc::base64decode(file_info$content))
    
    # Parse CSV
    df <- read_csv(content_decoded, show_col_types = FALSE)
    df <- as_tibble(df)
    
    df %>%
      mutate(
        added_at      = as.character(added_at),
        n_ingredients = as.integer(n_ingredients)
      )
  }, error = function(e) {
    warning("Failed to load from GitHub: ", e$message)
    return(empty_recipes())
  })
}

# Save recipes to GitHub
save_recipes <- function(df) {
  tryCatch({
    token <- get_github_token()
    
    # Get current file info to get SHA (required for updates)
    file_info <- get_github_file(GITHUB_OWNER, GITHUB_REPO, DATA_FILE_PATH, token)
    current_sha <- if (!is.null(file_info)) file_info$sha else NULL
    
    # Prepare data
    df_clean <- df %>%
      mutate(
        added_at      = as.character(added_at),
        n_ingredients = as.integer(n_ingredients)
      )
    
    # Convert to CSV string
    csv_content <- format_csv(df_clean)
    
    # Commit message
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    message <- paste("Update recipes data -", timestamp)
    
    # Upload to GitHub
    put_github_file(
      GITHUB_OWNER, 
      GITHUB_REPO, 
      DATA_FILE_PATH, 
      csv_content, 
      message, 
      token, 
      current_sha
    )
    
    return(TRUE)
  }, error = function(e) {
    warning("Failed to save to GitHub: ", e$message)
    return(FALSE)
  })
}

# ---- Helper: ingredient tokenization & constrained sampling --------
normalize_tokens <- function(x_chr_vec) {
  # Split comma-separated ingredients into a list of normalized tokens (lowercased & trimmed)
  # x_chr_vec: character vector (length n rows)
  lapply(x_chr_vec, function(s) {
    if (is.na(s) || !nzchar(trimws(s))) return(character(0))
    toks <- unlist(strsplit(s, ",", fixed = TRUE))
    toks <- trimws(tolower(toks))
    toks[nzchar(toks)]
  })
}

contains_must_have <- function(tokens, must_have) {
  # Simple substring match (case-insensitive) within each token
  if (!nzchar(must_have)) return(FALSE)
  any(grepl(must_have, tokens, ignore.case = TRUE, fixed = TRUE))
}

sample_with_constraints <- function(df, k = 5L, must_have = "", max_per_token = 3L, max_tries = 500L) {
  # df: data frame with columns name, link, ingredients; plus list-column 'tokens'
  n <- nrow(df)
  if (n == 0 || k == 0) return(integer(0))
  
  has_must <- FALSE
  mh_idx <- integer(0)
  if (nzchar(trimws(must_have))) {
    mh_idx <- which(vapply(df$tokens, contains_must_have, logical(1), must_have = must_have))
    has_must <- length(mh_idx) > 0
  }
  
  # Try randomized greedy construction with backtracking-ish restarts
  for (attempt in seq_len(max_tries)) {
    selected <- integer(0)
    counts <- new.env(parent = emptyenv())
    
    # If must-have exists, start with one random must-have recipe
    if (has_must) {
      first <- sample(mh_idx, 1)
      selected <- c(selected, first)
      # update counts
      for (t in df$tokens[[first]]) {
        counts[[t]] <- (counts[[t]] %||% 0L) + 1L
      }
    }
    
    pool <- setdiff(seq_len(n), selected)
    
    # Fill remaining slots
    ok <- TRUE
    while (length(selected) < k && length(pool) > 0) {
      # Randomize candidate order each loop
      cand <- sample(pool)
      added_any <- FALSE
      
      for (r in cand) {
        toks <- df$tokens[[r]]
        # Check if adding violates cap
        violates <- FALSE
        if (length(toks)) {
          for (t in toks) {
            if (((counts[[t]] %||% 0L) + 1L) > max_per_token) {
              violates <- TRUE
              break
            }
          }
        }
        if (!violates) {
          # Accept recipe r
          selected <- c(selected, r)
          if (length(toks)) {
            for (t in toks) {
              counts[[t]] <- (counts[[t]] %||% 0L) + 1L
            }
          }
          pool <- setdiff(pool, r)
          added_any <- TRUE
          break
        }
      }
      
      if (!added_any) { ok <- FALSE; break }
    }
    
    # Validate must-have included if requested
    if (ok && length(selected) == k) {
      if (!has_must || any(selected %in% mh_idx)) {
        return(selected)
      }
    }
    # else retry with different randomization
  }
  
  # If we get here, we could not satisfy the cap within the attempts.
  # Relaxation strategy: return a k-sample that includes must-have if possible, ignoring caps.
  if (has_must) {
    first <- sample(mh_idx, 1)
    others <- setdiff(seq_len(n), first)
    if (length(others) >= (k - 1L)) {
      return(c(first, sample(others, k - 1L)))
    } else {
      return(c(first, others))
    }
  } else {
    return(seq_len(min(k, n)))
  }
}

`%||%` <- function(x, y) if (is.null(x)) y else x

# ---- UI (unchanged from original) ----
library(shiny)
library(bslib)
library(shinyjs)

# ---- Theme (Bootstrap 5 + Google Fonts) ----
theme <- bs_theme(
  version = 5,
  bootswatch = "minty",         # try "flatly", "lux", "cosmo" too
  primary = "#43aa8b",
  secondary = "#f8961e",
  success = "#2ecc71",
  info = "#17a2b8",
  warning = "#f39c12",
  danger = "#e74c3c",
  base_font = font_google("Poppins"),
  heading_font = font_google("Merriweather")
)

# ---- UI ----
ui <- navbarPage(
  theme = theme,
  title = tagList(icon("utensils"), "YUMYUM"),
  header = tagList(
    useShinyjs(),
    # ---- Global CSS ----
    tags$style(HTML("
      /* Layout helpers */
      .wheel-wrap { display:flex; align-items:center; gap:28px; flex-wrap:wrap; }
      .menu-actions { margin: 8px 0 16px; display:flex; align-items:center; gap:12px; flex-wrap:wrap; }
      .muted { color:#6c757d; font-size: 0.95rem; }
      .soft-card { background:#fff; border-radius:14px; border:1px solid #e9ecef; box-shadow: 0 8px 24px rgba(0,0,0,.06); }
      .toolbar { position: sticky; top: 0; z-index: 2; padding: 12px 16px; border-radius:10px; background:#f8fafc; border:1px solid #eef2f7; display:flex; align-items:center; gap:12px; flex-wrap:wrap; }

      /* Wheel visuals */
      #wheel-container { position: relative; width: 240px; height: 240px; }
      #wheel {
        position: relative;
        width: 240px; height: 240px; border-radius: 50%;
        border: 6px solid #2c3e50; box-shadow: 0 8px 18px rgba(0,0,0,.15);
        background: conic-gradient(
          #f94144 0 16.6%, #f3722c 16.6% 33.3%,
          #f8961e 33.3% 50%, #f9844a 50% 66.6%,
          #90be6d 66.6% 83.3%, #43aa8b 83.3% 100%
        );
        transition: transform 0.8s ease;
      }
      #wheel::after {
        /* Center label that server can set via JS attribute `data-selected` */
        content: attr(data-selected);
        position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%);
        width: 148px; height: 148px; border-radius: 50%;
        background: rgba(0,0,0,0.15);
        backdrop-filter: blur(2px);
        display:flex; align-items:center; justify-content:center; text-align:center;
        padding: 8px 10px; color:#fff; font-weight:700; font-size: 1.05rem; line-height:1.2;
        border: 3px solid rgba(255,255,255,0.35);
        box-shadow: inset 0 4px 12px rgba(0,0,0,.2);
      }
      #wheel-indicator {
        position: absolute; top: -16px; left: 50%; transform: translateX(-50%);
        width: 0; height: 0;
        border-left: 12px solid transparent;
        border-right: 12px solid transparent;
        border-bottom: 20px solid #e74c3c;
        filter: drop-shadow(0 2px 2px rgba(0,0,0,.2));
      }
      .spinning { animation: wheelspin 2.8s cubic-bezier(0.22, 0.61, 0.36, 1) 1; }
      @keyframes wheelspin {
        from { transform: rotate(0deg); }
        to   { transform: rotate(1440deg); } /* 4 full turns */
      }

      /* Card grid for Weekly Menu */
      .grid {
        display: grid; grid-template-columns: repeat(auto-fill, minmax(260px, 1fr));
        gap: 16px;
      }
      .recipe-card {
        border-radius: 14px; overflow: hidden; border: 1px solid #eef2f7; background:#fff;
        box-shadow: 0 12px 24px rgba(0,0,0,.06);
        display:flex; flex-direction: column; min-height: 200px;
        transition: transform .12s ease, box-shadow .12s ease;
      }
      .recipe-card:hover { transform: translateY(-2px); box-shadow: 0 16px 32px rgba(0,0,0,.10); }
      .recipe-card .card-header {
        background: linear-gradient(120deg, rgba(67,170,139,0.12), rgba(248,150,30,0.08));
        padding: 12px 14px; font-weight: 600; display:flex; justify-content: space-between; align-items:center;
      }
      .recipe-card .card-body { padding: 12px 14px; flex: 1; display:flex; flex-direction: column; gap: 8px; }
      .chips { display:flex; flex-wrap:wrap; gap:6px; }
      .chip {
        font-size: 12px; padding: 4px 8px; border-radius: 999px; border:1px solid #e9ecef; background:#f8fafc; color:#334155;
      }
      .card-footer { padding: 12px 14px; border-top: 1px solid #eef2f7; background:#fcfcfd; display:flex; justify-content: space-between; align-items:center; }

      /* Pills tabs in main area */
      .nav-pills .nav-link.active { background-color: #43aa8b; }
    ")),
    # ---- Minimal JS: Spin animation + button feedback ----
    tags$script(HTML("
      // On 'Spin & Generate' click: animate wheel and update status
      $(document).on('click', '#gen_menu', function() {
        const wheel = document.getElementById('wheel');
        const status = document.getElementById('spin_status');
        if (!wheel) return;

        wheel.classList.remove('spinning'); // restart animation if needed
        void wheel.offsetWidth;             // reflow
        wheel.classList.add('spinning');

        if (status) {
          status.textContent = 'Spinning the wheel and generating your weekly menu...';
        }

        // Status will be finalized by server after render, but if nothing comes back:
        setTimeout(function(){
          if (status && status.textContent.includes('Spinning')) {
            status.textContent = 'Menu generated. Scroll to see your cards or switch to the table view.';
          }
        }, 3200);
      });

      // Helper to set wheel center label (call via shinyjs::runjs from server)
      Shiny.addCustomMessageHandler('set-wheel-label', function(txt){
        const wheel = document.getElementById('wheel');
        if (wheel) wheel.setAttribute('data-selected', txt || 'Spinning...');
      });
    "))
  ),
  
  # ---------------- TAB 1: Weekly Menu ----------------
  tabPanel(
    "Weekly Menu",
    sidebarLayout(
      sidebarPanel(
        class = "soft-card",
        tags$h5(icon("calendar-week"), " Weekly Menu Options"),
        textInput("must_have", "Use this ingredient", placeholder = "e.g., tofu, spinach, chickpeas"),
        helpText("Will include at least one recipe containing this ingredient (if available), and avoid using any single ingredient more than 3 times across the menu."),
        tags$hr(),
        # GitHub sync button
        div(class = "menu-actions",
            actionButton("sync_github", tagList(icon("sync"), "Sync with GitHub"), class = "btn btn-info btn-sm"),
            div(id = "sync_status", class = "muted")
        ),
        width = 3
      ),
      mainPanel(
        # Top toolbar row (sticky)
        div(class = "toolbar",
            actionButton("gen_menu", "🎲 Spin!", class = "btn btn-success btn-lg"),
            tags$div(id = "spin_status", class = "muted"),
            tags$span(class = "badge bg-success", title = "Selected week size", "5 recipes")
        ),
        
        # Wheel + CTA
        br(),
        div(class = "wheel-wrap",
            div(id = "wheel-container",
                div(id = "wheel-indicator"),
                div(id = "wheel", `data-selected` = "Spin!")
            )
        ),
        
        tags$hr(),
        
        # Cards + Table tabs
        tabsetPanel(type = "pills",
                    tabPanel(
                      "Cards",
                      br(),
                      div(class = "grid",
                          # Server populates this via output$menu_cards
                          uiOutput("menu_cards")
                      ),
                      br(),
                      downloadButton("download_menu", "Download Menu (CSV)", class = "btn btn-outline-primary")
                    ),
                    tabPanel(
                      "Table",
                      br(),
                      div(class = "soft-card", style = "padding:10px",
                          div(class = "muted", icon("table"), " Original table view"),
                          br(),
                          DTOutput("menu"),
                          br(),
                          downloadButton("download_menu_table", "Download (CSV)", class = "btn btn-outline-secondary")
                      )
                    )
        ),
        width = 9
      )
    )
  ),
  
  # --------------- TAB 2: All Recipes -----------------
  tabPanel(
    "All Recipes",
    sidebarLayout(
      sidebarPanel(
        class = "soft-card",
        tags$h5(icon("book"), " Recipe Editor"),
        textInput("name", "Recipe name", placeholder = "e.g., Kartoffelbrei"),
        textInput("link", "Link", placeholder = "https://..."),
        textAreaInput("ingredients", "Ingredients (comma-separated)", height = "120px",
                      placeholder = "lemon, garlic, olive oil, salt, pepper"),
        div(class = "menu-actions",
            actionButton("add", tagList(icon("plus"), "Add Recipe"), class = "btn btn-primary"),
            actionButton("delete", tagList(icon("trash"), "Delete Selected"), class = "btn btn-danger"),
            fileInput("upload_file", "Upload Recipes (CSV)", accept = ".csv"),
            downloadButton("download_all", "Download All Recipes (CSV)", class = "btn btn-outline-secondary")
        ),
        tags$hr()
      ),
      mainPanel(
        div(class = "soft-card", style = "padding:10px",
            div(class = "toolbar",
                tags$div(class = "muted", icon("filter"), " You can sort/filter directly in the table.")
            ),
            DTOutput("table")
        ),
        width = 9
      )
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  # Load existing data
  recipes <- reactiveVal(load_recipes())
  
  # ------ Sync with GitHub ------
  observeEvent(input$sync_github, {
    shinyjs::html(id = "sync_status", html = "Syncing with GitHub...")
    
    tryCatch({
      new_data <- load_recipes()
      recipes(new_data)
      shinyjs::html(id = "sync_status", html = "✓ Synced successfully")
      showNotification("Data synced from GitHub", type = "message")
    }, error = function(e) {
      shinyjs::html(id = "sync_status", html = "✗ Sync failed")
      showNotification(paste("Sync failed:", e$message), type = "error")
    })
    
    # Clear status after 3 seconds
    shinyjs::delay(3000, {
      shinyjs::html(id = "sync_status", html = "")
    })
  })
  
  # ------ Add new recipe (All Recipes tab) ------
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
    
    # Save to GitHub
    if (save_recipes(updated)) {
      showNotification("Recipe added and saved to GitHub.", type = "message")
    } else {
      showNotification("Recipe added locally, but failed to save to GitHub.", type = "warning")
    }
    
    updateTextInput(session, "name", value = "")
    updateTextInput(session, "link", value = "")
    updateTextAreaInput(session, "ingredients", value = "")
  })
  
  # ------ Inline editing + delete (All Recipes tab) ------
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
  
  observeEvent(input$table_cell_edit, {
    info <- input$table_cell_edit
    df   <- recipes()
    
    j <- info$col + 1L
    i <- info$row
    colname <- names(df)[j]
    newval  <- info$value
    
    if (colname == "n_ingredients") {
      newval <- suppressWarnings(as.integer(newval))
      if (is.na(newval)) {
        showNotification("`n_ingredients` must be an integer.", type = "warning")
        return(NULL)
      }
      df[i, j] <- newval
    } else if (colname == "ingredients") {
      # Accept edit, and auto-adjust n_ingredients to match the new ingredients string
      newval <- as.character(newval)
      df[i, j] <- newval
      df$n_ingredients[i] <- if (nzchar(trimws(newval))) length(strsplit(newval, ",", fixed = TRUE)[[1]]) else 0L
    } else {
      newval <- as.character(newval)
      df[i, j] <- newval
    }
    
    recipes(df)
    
    # Save to GitHub
    if (save_recipes(df)) {
      showNotification("Changes saved to GitHub.", type = "message")
    } else {
      showNotification("Changes saved locally, but failed to sync to GitHub.", type = "warning")
    }
  })
  
  observeEvent(input$delete, {
    sel <- input$table_rows_selected
    if (!length(sel)) {
      showNotification("Select at least one row to delete.", type = "message")
      return(NULL)
    }
    df <- recipes()
    df <- df[-sel, , drop = FALSE]
    recipes(df)
    
    # Save to GitHub
    if (save_recipes(df)) {
      showNotification(sprintf("Deleted %d row(s) and saved to GitHub.", length(sel)), type = "message")
    } else {
      showNotification(sprintf("Deleted %d row(s) locally, but failed to sync to GitHub.", length(sel)), type = "warning")
    }
  })
  
  # ------- Weekly Menu Feature (with constraints) -------
  weekly_menu <- reactiveVal(
    tibble(name = character(), link = character(),
           ingredients = character(), n_ingredients = integer())
  )
  
  # Cards version keeps the RAW link for the card "Open" button
  weekly_menu_cards <- reactiveVal(
    tibble(name = character(), link = character(),
           ingredients = character(), n_ingredients = integer())
  )
  
  observeEvent(input$gen_menu, {
    shinyjs::addClass(id = "wheel", class = "spinning")
    shinyjs::html(id = "spin_status", html = "Spinning the wheel...")
    session$sendCustomMessage("set-wheel-label", "Spinning…")
    
    shinyjs::delay(2800, {
      shinyjs::removeClass(id = "wheel", class = "spinning")
      
      df <- recipes()
      
      if (nrow(df) == 0) {
        showNotification("No recipes found. Add some first!", type = "warning")
        weekly_menu(empty_recipes() %>% select(name, link, ingredients, n_ingredients))
        weekly_menu_cards(empty_recipes() %>% select(name, link, ingredients, n_ingredients))
        shinyjs::html(id = "spin_status", html = "No recipes available yet.")
        session$sendCustomMessage("set-wheel-label", "Add recipes ↑")
        return(NULL)
      }
      
      df <- df %>%
        filter(!is.na(name), nzchar(trimws(name))) %>%
        distinct(name, .keep_all = TRUE)
      
      if (nrow(df) == 0) {
        showNotification("All recipes have empty names. Please name your recipes.", type = "warning")
        weekly_menu(empty_recipes() %>% select(name, link, ingredients, n_ingredients))
        weekly_menu_cards(empty_recipes() %>% select(name, link, ingredients, n_ingredients))
        shinyjs::html(id = "spin_status", html = "Please name your recipes.")
        session$sendCustomMessage("set-wheel-label", "Name recipes")
        return(NULL)
      }
      
      # Prepare tokens for constraint logic
      df_tokens <- df %>%
        mutate(tokens = normalize_tokens(ingredients))
      
      k <- min(5L, nrow(df_tokens))
      must <- trimws(input$must_have %||% "")
      
      # Build selection with constraints (must-have + max 3 per ingredient)
      idx <- sample_with_constraints(df_tokens, k = k, must_have = must, max_per_token = 3L, max_tries = 500L)
      
      # Feedback to user if must-have was requested but not included
      if (nzchar(must)) {
        has_any <- any(vapply(df_tokens$tokens, contains_must_have, logical(1), must_have = must))
        if (!has_any) {
          showNotification(
            paste0("No recipes include '", must, "'. Generated a menu without it."),
            type = "warning", duration = 6
          )
        } else if (!any(vapply(df_tokens$tokens[idx], contains_must_have, logical(1), must_have = must))) {
          showNotification(
            paste0("Couldn't satisfy all constraints with '", must, "'. Relaxed ingredient cap to complete the menu."),
            type = "message", duration = 6
          )
        }
      }
      
      # --- Cards (raw link) ---
      df_cards <- df_tokens[idx, , drop = FALSE] %>%
        select(name, link, ingredients, n_ingredients) %>%
        mutate(n_ingredients = as.numeric(n_ingredients))
      
      # --- Table (HTML link) ---
      df_disp <- df_tokens[idx, , drop = FALSE] %>%
        mutate(
          link = if_else(
            !is.na(link) & nzchar(trimws(link)),
            sprintf('<a href="%s" target="_blank">Open</a>', htmltools::htmlEscape(trimws(link))),
            ""
          )
        ) %>%
        select(name, link, ingredients, n_ingredients)
      
      weekly_menu_cards(df_cards)
      weekly_menu(df_disp)
      
      # Update wheel center label with a 'headline' recipe
      headline <- tryCatch(
        {
          if (length(idx) > 0 && nrow(df_tokens) > 0) as.character(df_tokens$name[idx[1]]) else "Menu ready"
        },
        error = function(e) "Menu ready"
      )
      session$sendCustomMessage("set-wheel-label", headline)
      
      shinyjs::html(id = "spin_status", html = paste0("Your weekly menu is ready (", k, " recipe(s))."))
    })
  })
  
  # --------- Cards renderer (UI matches the new cards layout) ----------
  output$menu_cards <- renderUI({
    df <- weekly_menu_cards()
    if (nrow(df) == 0) {
      return(tags$div(class = "muted", "No menu generated yet."))
    }
    
    # helper: split ingredients into neat chips
    split_ing <- function(x) {
      if (is.null(x) || is.na(x) || !nzchar(trimws(x))) return(character(0))
      out <- trimws(unlist(strsplit(x, ",", fixed = TRUE)))
      out[nzchar(out)]
    }
    
    # Build a list of card divs
    lapply(seq_len(nrow(df)), function(i) {
      link  <- df$link[i]
      has_link <- !is.na(link) && nzchar(trimws(link))
      
      tags$div(class = "recipe-card",
               tags$div(class = "card-header",
                        tags$span(df$name[i]),
                        tags$span(class = "badge bg-success", paste0(length(items), " ing"))
               ),
               tags$div(class = "card-body",
                        if (length(items)) {
                          tags$div(class = "chips",
                                   lapply(head(items, 8), function(x) tags$span(class = "chip", x))
                          )
                        } else {
                          tags$div(class = "muted", "No ingredients listed")
                        }
               ),
               tags$div(class = "card-footer",
                        if (has_link) {
                          tags$a(href = link, target = "_blank", class = "btn btn-sm btn-primary",
                                 icon("external-link-alt"), " Open")
                        } else {
                          tags$span(class = "muted", icon("link"), " No link provided")
                        },
                        tags$span(class = "muted", icon("utensils"))
               )
      )
    })
  })
  
  # -------------------- Table + downloads -----------------------------
  output$menu <- renderDT({
    datatable(
      weekly_menu(),
      escape = FALSE,
      options = list(pageLength = 5, dom = "tip"),
      rownames = FALSE
    )
  }, server = FALSE)
  
  # Cards tab download (kept as-is)
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
  
  # Table tab download (same content, different button id)
  output$download_menu_table <- downloadHandler(
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
  
  # ---- Download all recipes (unchanged) ----
  output$download_all <- downloadHandler(
    filename = function() paste0("all_recipes_", Sys.Date(), ".csv"),
    content = function(file) {
      df <- recipes()
      readr::write_csv(df, file)
    }
  )
  
  # ---- Upload recipes (robust to class mismatches) ----
  observeEvent(input$upload_file, {
    req(input$upload_file)
    
    tryCatch({
      # Read everything as character first to avoid readr's mis-guessing
      uploaded_raw <- readr::read_csv(
        input$upload_file$datapath,
        col_types = readr::cols(.default = readr::col_character()),
        show_col_types = FALSE,
        guess_max = 10000
      )
      
      # Coerce to our schema safely
      normalized <- normalize_recipes_df(uploaded_raw)
      report <- attr(normalized, "coercion_report", exact = TRUE)
      
      # Merge into current data; prefer uploaded rows on name collisions
      # (requires dplyr >= 1.1; if older, fall back to distinct by name)
      if (utils::packageVersion("dplyr") >= "1.1.0") {
        combined <- dplyr::rows_upsert(recipes(), normalized, by = "name")
      } else {
        combined <- dplyr::bind_rows(
          recipes() %>% dplyr::anti_join(normalized, by = "name"),
          normalized
        )
      }
      
      # Persist + refresh
      recipes(combined)
      
      # Save to GitHub
      if (save_recipes(combined)) {
        showNotification("Upload complete and saved to GitHub.", type = "message", duration = 6)
      } else {
        showNotification("Upload complete locally, but failed to save to GitHub.", type = "warning", duration = 6)
      }
      
    }, error = function(e) {
      showNotification(paste("Upload failed:", e$message), type = "error", duration = 8)
    })
  })
}

shinyApp(ui, server)