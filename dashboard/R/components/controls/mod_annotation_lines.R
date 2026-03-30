## Vertical and Horizontal Lines Controls -----

# --- UI ---
mod_annotation_line_ui <- function(
  id,
  title = "String + Date pairs",
  add_label = "+ Add",
  placeholder_label = "Enter label…",
  placeholder_date  = "YYYY-MM-DD",
  show_delete = TRUE,
  auto_mask_date = TRUE,
  type = c("date", "value"),
  gap_px = 8  # fixed horizontal gap in pixels
) {
  type <- match.arg(type)
  ns <- NS(id)

  right_placeholder <- if (type == "date") placeholder_date else "Enter numeric value…"

  tagList(
    # CSS (scoped by IDs to this module instance)
    tags$style(HTML(sprintf("
      /* Row behaves as a 3-column grid: 50%% | 40%% | auto */
      #%s .sdlm-grid {
        display: grid;
        grid-template-columns: 50%% 40%% auto;
        column-gap: %dpx;                   /* fixed gap you control */
        align-items: center;                /* baseline safety */
        margin-bottom: 6px;
        width: 100%%;
        min-width: 0;                       /* prevent overflow from long content */
      }

      /* Ensure inputs fill their grid cell and don't overflow */
      #%s .sdlm-grid .sdlm-cell > .form-group,
      #%s .sdlm-grid .sdlm-cell input,
      #%s .sdlm-grid .sdlm-cell .form-control {
        width: 100%%;
        min-width: 0;
      }

      /* Minimal extra CSS (Option B):
         - Make every cell a flex container to vertically center contents
         - Center the delete cell horizontally (last grid cell)
         - Normalize Bootstrap form-group margins so inputs don't inflate the row */
      #%s .sdlm-grid .sdlm-cell {
        display: flex;
        align-items: center;    /* vertical centering */
        min-width: 0;
      }
      #%s .sdlm-grid .sdlm-cell:last-child {
        justify-content: center;  /* center the ✕ horizontally */
      }
      #%s .sdlm-grid .form-group {
        margin-bottom: 0;         /* avoid extra vertical space */
      }

      /* Minimalist cross (no background hover; text-only emphasis) */
      #%s .sdlm-x {
        display: inline-flex;     /* predictable centering box */
        align-items: center;
        justify-content: center;
        color: #6c757d;
        font-weight: 600;
        text-decoration: none !important;
        padding: 0;
        margin: 0;
        width: 28px;              /* small square click target */
        height: 28px;
        border-radius: 4px;
        line-height: 1;           /* prevent baseline drift */
        cursor: pointer;
        user-select: none;
        border: 0;
        background: transparent;
      }
      
      #%s .sdlm-x:hover,
      #%s .sdlm-x:focus {
        color: #dc3545;           /* make the X pop */
        font-weight: 700;         /* stronger/bolder X on hover/focus */
        background: transparent;  /* ensure no background fill */
        outline: none;
        box-shadow: none;
      }


      /* Invalid glow */
      #%s .sdlm-invalid input {
        border-color: #d9534f !important;
        box-shadow: 0 0 0 0.18rem rgba(217,83,79,.25) !important;
      }

      /* Add button area */
      #%s .sdlm-add { margin-top: 6px; }
    ",
      ns("rows_container"), gap_px,  # grid + gap

      ns("rows_container"),          # ensure inputs fill cell width
      ns("rows_container"),
      ns("rows_container"),

      ns("rows_container"),          # .sdlm-cell flex centering
      ns("rows_container"),          # last child centered (delete cell)
      ns("rows_container"),          # normalize form-group margin

      ns("rows_container"),          # .sdlm-x base
      ns("rows_container"),          # .sdlm-x:hover
      ns("rows_container"),          # .sdlm-x:focus

      ns("rows_container"),          # invalid glow
      ns("root")                     # add button scope
    ))),

    # Scope all row styles to this module instance
    div(id = ns("root"),
  div(class = "form-group shiny-input-container",
    tags$label(
      id    = paste0(ns("chart_type"), "-label"),  # matches shinyWidgets pattern
      class = "control-label",
      `for` = ns("chart_type"),                    # associate with the input
      title
            )
          )
        ), 

      # Start empty: rows will be inserted here
      div(id = ns("rows_container")),

      # Add button
      div(
        actionButton(ns("add"), add_label, icon = icon("plus")),
        class = "sdlm-add"
      ),

      # Date auto-mask (only in date mode)
      if (auto_mask_date && type == "date") tags$script(HTML(sprintf("
        (function(){
          var prefix = '%s';
          document.addEventListener('input', function(e){
            if(!e.target || !e.target.id) return;
            if(e.target.id.startsWith(prefix + 'date_')){
              let v = e.target.value.replace(/[^0-9]/g,'').slice(0,8);
              if(v.length > 4) v = v.slice(0,4) + '-' + v.slice(4);
              if(v.length > 7) v = v.slice(0,7) + '-' + v.slice(7);
              e.target.value = v;
            }
          }, true);
        })();
      ", ns("")))
    )
  )
}


# --- SERVER ---
mod_annotation_line_server <- function(id, type = c("date", "value")) {
  type <- match.arg(type)

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Start with NO rows
    max_idx   <- reactiveVal(0L)
    active_ix <- reactiveVal(integer(0))

    # Namespaced row and inputs
    id_row    <- function(i) paste0("#", ns(paste0("row_", i)))
    id_label  <- function(i) ns(paste0("label_",  i))
    id_date   <- function(i) ns(paste0("date_",   i))
    id_remove <- function(i) ns(paste0("remove_", i))

    # Add row (creates the first too)
    observeEvent(input$add, {
      i <- max_idx() + 1L
      insertUI(
        selector = paste0("#", ns("rows_container")),
        where = "beforeEnd",

        ui = tags$div(
          id = ns(paste0("row_", i)),
          class = "sdlm-grid",

          # 50% cell: label
          tags$div(class = "sdlm-cell",
            textInput(id_label(i), label = NULL, placeholder = "Enter label…")
          ),

          # 40% cell: date/value
          tags$div(class = "sdlm-cell",
            textInput(
              inputId = id_date(i),
              label   = NULL,
              placeholder = if (type == "date") "YYYY-MM-DD" else "Enter numeric value…"
            )
          ),

          # auto cell: minimalist cross (no outline)
          tags$div(class = "sdlm-cell",
            actionLink(id_remove(i), label = "✕", class = "sdlm-x", title = "Remove row")
          )
        )
      )
      max_idx(i)
      active_ix(sort(unique(c(active_ix(), i))))
    }, ignoreInit = TRUE)

    # Remove row (safe for max_idx() == 0)
    observe({
      lapply(seq_len(max_idx()), function(i) {
        rid <- paste0("remove_", i)
        if (!is.null(input[[rid]])) {
          observeEvent(input[[rid]], {
            removeUI(selector = id_row(i), multiple = FALSE)
            active_ix(setdiff(active_ix(), i))
          }, ignoreInit = TRUE, once = TRUE)
        }
        NULL
      })
    })

    # Helpers
    `%||%` <- function(a, b) if (is.null(a)) b else a
    is_real_ymd <- function(x) {
      x <- trimws(x)
      if (!nzchar(x)) return(FALSE)
      if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", x, perl = TRUE)) return(FALSE)
      px <- try(as.Date(x), silent = TRUE)
      if (inherits(px, "try-error")) return(FALSE)
      !is.na(px)
    }
    is_numeric_text <- function(x) {
      x <- trimws(x)
      if (!nzchar(x)) return(FALSE)
      grepl("^[+-]?\\d+(?:\\.\\d+)?$", x, perl = TRUE)  # no scientific notation by default
    }

    # Collect
    collected <- reactive({
      idx <- active_ix()
      if (!length(idx)) {
        return(list(index = integer(0),
                    labels = character(0),
                    date_raw = character(0),
                    valid = logical(0)))
      }

      labels <- vapply(idx, function(i) input[[paste0("label_", i)]] %||% "", character(1))
      right_raw <- vapply(idx, function(i) input[[paste0("date_",  i)]] %||% "", character(1))
      right_trim <- trimws(right_raw)

      valid <- if (type == "date") {
        vapply(right_trim, is_real_ymd, logical(1))
      } else {
        vapply(right_trim, is_numeric_text, logical(1))
      }

      # Red highlight only if non-empty and invalid
      bad_idx <- idx[!valid & nzchar(right_trim)]
      lapply(idx, function(i) {
        toggle_invalid_class(session, paste0("date_", i), add = i %in% bad_idx)
        NULL
      })

      list(index = idx, labels = labels, date_raw = right_raw, valid = valid)
    })

    # Raw outputs (all active rows)
    labels_raw <- reactive(collected()$labels)
    date_raw   <- reactive(collected()$date_raw)
    valid      <- reactive(collected()$valid)

    # Filtered outputs; return NULL if none valid
    labels_out <- reactive({
      vals <- collected()
      ok <- vals$valid
      if (!length(ok) || !any(ok, na.rm = TRUE)) return(NULL)
      vals$labels[ok]
    })
    values_out <- reactive({
      vals <- collected()
      ok <- vals$valid
      if (!length(ok) || !any(ok, na.rm = TRUE)) return(NULL)
      vals$date_raw[ok]
    })

    return(list(
      labels_raw = labels_raw,
      date_raw   = date_raw,
      valid      = valid,
      labels_out = labels_out,   # NULL if none valid
      values_out = values_out    # NULL if none valid
    ))
  })
}

# Minimal JS helper: add/remove the .sdlm-invalid class on the input's parent container
toggle_invalid_class <- function(session, input_id, add = TRUE) {
  session$sendCustomMessage("sdlm_toggle_invalid", list(id = session$ns(input_id), add = isTRUE(add)))
}

stringDateListInitJS <- function() {
  tags$script(HTML("
    Shiny.addCustomMessageHandler('sdlm_toggle_invalid', function(x){
      var el = document.getElementById(x.id);
      if(!el) return;
      var parent = el.closest('.form-group') || el.parentElement;
      if(!parent) parent = el.parentElement;
      if(x.add){ parent.classList.add('sdlm-invalid'); }
      else{ parent.classList.remove('sdlm-invalid'); }
    });
  "))
}



