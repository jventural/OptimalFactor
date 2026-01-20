# =============================================================================
#
#   EFA-Boosting Studio - Interactive Analyzer
#   Version 2.0
#
#   Aplicacion Shiny para optimizacion de Analisis Factorial Exploratorio
#   usando el algoritmo EFA-Boosting con indices de ajuste adaptativos
#
# =============================================================================

options(repos = c(CRAN = "https://cran.r-project.org"))

# Verificar e instalar paquetes necesarios
.required_pkgs <- c("shiny", "DT", "readxl", "readr", "ggplot2", "future", "promises", "bslib")
.missing_pkgs <- .required_pkgs[!sapply(.required_pkgs, requireNamespace, quietly = TRUE)]
if (length(.missing_pkgs) > 0) {
  message("Instalando paquetes faltantes: ", paste(.missing_pkgs, collapse = ", "))
  install.packages(.missing_pkgs)
}

library(shiny)
library(DT)
library(readxl)
library(readr)
library(ggplot2)
library(future)
library(promises)
library(bslib)

# Plan de futures para no bloquear Shiny
future::plan(future::multisession, workers = 2)

# =============================================================================
# UTILIDADES
# =============================================================================

read_any_table <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("xls", "xlsx")) {
    as.data.frame(readxl::read_excel(path))
  } else {
    suppressWarnings(readr::read_csv(path, show_col_types = FALSE))
  }
}

parse_item_definitions <- function(txt) {
  if (is.null(txt) || !nzchar(txt)) return(NULL)
  lines <- unlist(strsplit(txt, "\n"))
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]
  res <- list()
  for (ln in lines) {
    if (grepl("^\\s*[A-Za-z]+\\d+\\s*:", ln)) {
      key <- sub(":.*$", "", ln)
      val <- sub("^[^:]+:\\s*", "", ln)
      res[[trimws(key)]] <- val
    }
  }
  if (length(res) == 0) NULL else res
}

# =============================================================================
# UI - Diseno moderno con bslib
# =============================================================================

ui <- page_navbar(
  title = div(
    img(src = "https://raw.githubusercontent.com/jventural/OptimalFactor/master/logo_optimalfactor.png",
        height = "35px", style = "margin-right: 10px;"),
    "EFA-Boosting Studio"
  ),
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#2C3E50",
    secondary = "#18BC9C",
    success = "#18BC9C",
    info = "#3498DB",
    warning = "#F39C12",
    danger = "#E74C3C",
    base_font = font_google("Inter"),
    code_font = font_google("JetBrains Mono")
  ),

  # --- Panel: Analisis ---
  nav_panel(
    title = "Analisis",
    icon = icon("chart-line"),

    layout_sidebar(
      sidebar = sidebar(
        width = 350,

        # Seccion: Datos
        accordion(
          id = "config_accordion",
          open = c("datos", "modelo"),

          accordion_panel(
            title = "Datos",
            value = "datos",
            icon = icon("database"),

            fileInput("file", "Cargar archivo (CSV/XLSX)",
                      accept = c(".csv", ".xlsx", ".xls"),
                      buttonLabel = "Buscar...",
                      placeholder = "Ningun archivo seleccionado"),

            textInput("prefix", "Prefijo de items", value = "item",
                      placeholder = "ej: DP, IT, Q"),

            checkboxInput("auto_range", "Detectar items automaticamente", value = TRUE),

            conditionalPanel(
              condition = "!input.auto_range",
              layout_columns(
                numericInput("range_start", "Desde", value = 1, min = 1),
                numericInput("range_end", "Hasta", value = 20, min = 1)
              )
            ),

            uiOutput("data_info"),

            tags$hr(),

            textInput("exclude_items", "Items a excluir (opcional)",
                      value = "",
                      placeholder = "ej: item5, item12, item18"),
            tags$small(class = "text-muted",
                       "Separar con comas los items que deseas excluir del analisis")
          ),

          accordion_panel(
            title = "Modelo EFA",
            value = "modelo",
            icon = icon("cogs"),

            numericInput("n_factors", "Numero de factores", value = 3, min = 1, max = 15),

            selectInput("estimator", "Estimador",
                        choices = c("WLSMV (ordinal)" = "WLSMV",
                                    "ML (continuo)" = "ML",
                                    "ULS" = "ULS"),
                        selected = "WLSMV"),

            selectInput("rotation", "Rotacion",
                        choices = c("Oblimin" = "oblimin",
                                    "Geomin" = "geomin",
                                    "Varimax" = "varimax",
                                    "Promax" = "promax"),
                        selected = "oblimin"),

            checkboxInput("use_global", "Busqueda global (multi-item)", value = FALSE),

            conditionalPanel(
              condition = "input.use_global",
              sliderInput("max_drop", "Max items por paso", min = 1, max = 3, value = 2)
            )
          ),

          accordion_panel(
            title = "Criterios de Ajuste",
            value = "criterios",
            icon = icon("bullseye"),

            sliderInput("thr_rmsea", "RMSEA objetivo",
                        min = 0.05, max = 0.10, value = 0.08, step = 0.005),

            sliderInput("thr_srmr", "SRMR objetivo",
                        min = 0.04, max = 0.10, value = 0.06, step = 0.005),

            sliderInput("thr_cfi", "CFI objetivo",
                        min = 0.90, max = 0.99, value = 0.95, step = 0.01),

            sliderInput("thr_loading", "Carga minima",
                        min = 0.20, max = 0.50, value = 0.30, step = 0.05),

            numericInput("thr_min_items", "Min items por factor", value = 3, min = 2, max = 5),

            sliderInput("min_interfactor", "Corr. interfactorial minima",
                        min = 0.20, max = 0.50, value = 0.32, step = 0.02)
          ),

          accordion_panel(
            title = "IA (Opcional)",
            value = "ia",
            icon = icon("robot"),

            checkboxInput("use_ai", "Activar analisis con IA", value = FALSE),

            conditionalPanel(
              condition = "input.use_ai",
              passwordInput("api_key", "API Key OpenAI"),
              selectInput("gpt_model", "Modelo GPT",
                          choices = c("GPT-4o" = "gpt-4o",
                                      "GPT-4" = "gpt-4",
                                      "GPT-3.5" = "gpt-3.5-turbo")),
              selectInput("language", "Idioma",
                          choices = c("Espanol" = "spanish", "English" = "english")),
              textAreaInput("item_defs", "Definiciones de items", rows = 3,
                            placeholder = "item1: Texto del item 1\nitem2: Texto del item 2")
            )
          )
        ),

        tags$hr(),

        actionButton("run", "EJECUTAR ANALISIS",
                     class = "btn-primary btn-lg w-100",
                     icon = icon("play")),

        tags$br(), tags$br(),

        conditionalPanel(
          condition = "output.is_running == 'true'",
          div(
            class = "alert alert-info",
            icon("spinner", class = "fa-spin"),
            " Analisis en progreso...",
            tags$br(),
            textOutput("progress_info", inline = TRUE)
          )
        )
      ),

      # Panel principal
      navset_card_tab(
        id = "main_tabs",

        nav_panel(
          title = "Consola",
          icon = icon("terminal"),

          card(
            card_header(
              class = "d-flex justify-content-between align-items-center",
              span(icon("terminal"), " Salida en tiempo real"),
              actionButton("clear_console", "Limpiar", class = "btn-sm btn-outline-secondary")
            ),
            card_body(
              class = "p-0",
              tags$div(
                id = "console_container",
                style = "background-color: #1a1a2e; border-radius: 0 0 8px 8px;",
                tags$pre(
                  id = "console_output",
                  style = "
                    background-color: #1a1a2e;
                    color: #00ff88;
                    font-family: 'JetBrains Mono', 'Consolas', monospace;
                    font-size: 13px;
                    padding: 15px;
                    margin: 0;
                    height: 550px;
                    overflow-y: auto;
                    white-space: pre-wrap;
                    word-wrap: break-word;
                  ",
                  textOutput("console_live", container = span)
                )
              )
            )
          ),

          # JavaScript para auto-scroll inteligente (solo si esta cerca del final)
          tags$script(HTML("
            Shiny.addCustomMessageHandler('scrollConsole', function(message) {
              var consoleEl = document.getElementById('console_output');
              if (consoleEl) {
                // Solo hacer auto-scroll si el usuario esta cerca del final (dentro de 150px)
                var isNearBottom = (consoleEl.scrollHeight - consoleEl.scrollTop - consoleEl.clientHeight) < 150;
                if (isNearBottom) {
                  consoleEl.scrollTop = consoleEl.scrollHeight;
                }
              }
            });
          "))
        ),

        nav_panel(
          title = "Resumen",
          icon = icon("chart-pie"),

          layout_columns(
            col_widths = c(6, 6),

            card(
              card_header(icon("table"), " Metricas del Analisis"),
              tableOutput("summary_table")
            ),

            card(
              card_header(icon("check-circle"), " Indices de Ajuste Final"),
              tableOutput("fit_table")
            )
          ),

          layout_columns(
            col_widths = c(8, 4),

            card(
              card_header(icon("chart-line"), " Evolucion del Ajuste"),
              plotOutput("fit_evolution_plot", height = "300px")
            ),

            card(
              card_header(icon("project-diagram"), " Correlaciones Interfactoriales"),
              tableOutput("phi_table")
            )
          )
        ),

        nav_panel(
          title = "Estructura",
          icon = icon("th"),

          card(
            card_header(
              class = "d-flex justify-content-between align-items-center",
              span(icon("th"), " Estructura Factorial Final"),
              downloadButton("dl_structure", "Descargar CSV", class = "btn-sm btn-success")
            ),
            DTOutput("final_dt")
          )
        ),

        nav_panel(
          title = "Historial",
          icon = icon("history"),

          card(
            card_header(
              class = "d-flex justify-content-between align-items-center",
              span(icon("history"), " Items Eliminados"),
              downloadButton("dl_steps", "Descargar CSV", class = "btn-sm btn-info")
            ),
            DTOutput("steps_dt")
          )
        )
      )
    )
  ),

  # --- Panel: Ayuda ---
  nav_panel(
    title = "Ayuda",
    icon = icon("question-circle"),

    card(
      card_header(icon("book"), " Guia de Uso"),
      card_body(
        markdown("
### Como usar EFA-Boosting Studio

1. **Cargar datos**: Sube un archivo CSV o Excel con tus items
2. **Configurar prefijo**: Indica el prefijo de tus items (ej: 'item', 'DP', 'Q')
3. **Ajustar modelo**: Define numero de factores, estimador y rotacion
4. **Ejecutar**: El algoritmo EFA-Boosting optimizara tu modelo

### Algoritmo EFA-Boosting

El algoritmo utiliza:
- **Indices adaptativos**: Pesos de RMSEA/SRMR/CFI segun df x N
- **Deteccion automatica**: Heywood cases, cross-loadings, cargas bajas
- **Busqueda global**: Opcionalmente evalua multiples items simultaneamente

### Criterios de parada
- RMSEA <= objetivo Y CFI >= objetivo Y SRMR <= objetivo
- Estructura factorial valida (min items por factor)
- Correlaciones interfactoriales adecuadas

### Referencias
- Kenny, D.A. & McCoach, D.B. (2003). Effect of the number of variables on SEM fit indices.
- Shi, D., Lee, T., & Maydeu-Olivares, A. (2019). Understanding the model size effect.
        ")
      )
    )
  ),

  nav_spacer(),

  nav_item(
    tags$a(
      href = "https://github.com/jventural/OptimalFactor",
      target = "_blank",
      icon("github"), " GitHub"
    )
  )
)

# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {

  # --- Reactivos de datos ---
  dataset <- reactive({
    req(input$file)
    tryCatch({
      df <- read_any_table(input$file$datapath)
      if (is.null(df) || ncol(df) == 0) return(NULL)
      df
    }, error = function(e) NULL)
  })

  detected_items <- reactive({
    req(dataset())
    grep(paste0("^", input$prefix, "\\d+$"), names(dataset()), value = TRUE)
  })

  data_for_analysis <- reactive({
    df <- dataset()
    if (is.null(df)) return(NULL)

    if (isTRUE(input$auto_range)) {
      items <- detected_items()
    } else {
      items <- paste0(input$prefix, seq(input$range_start, input$range_end))
    }
    items <- intersect(items, names(df))
    if (length(items) == 0) return(NULL)

    df[, items, drop = FALSE]
  })

  # Info de datos cargados
  output$data_info <- renderUI({
    df <- data_for_analysis()
    if (is.null(df)) return(NULL)

    div(
      class = "alert alert-success mt-2 mb-0 p-2",
      style = "font-size: 0.85em;",
      icon("check"),
      sprintf(" %d casos, %d items detectados", nrow(df), ncol(df))
    )
  })

  # --- Estado del analisis ---
  analysis_running <- reactiveVal(FALSE)
  log_path <- reactiveVal(NULL)
  results <- reactiveVal(NULL)

  output$is_running <- renderText({
    if (analysis_running()) "true" else "false"
  })
  outputOptions(output, "is_running", suspendWhenHidden = FALSE)

  # --- Consola en tiempo real ---
  console_text <- reactiveVal("EFA-Boosting Studio v2.0\n\nCarga tus datos y presiona 'EJECUTAR ANALISIS' para comenzar.\n")

  output$console_live <- renderText({
    # Refresco cada 100ms durante analisis, 2000ms en reposo
    invalidateLater(if (analysis_running()) 100 else 2000, session)

    p <- log_path()
    if (!is.null(p) && file.exists(p)) {
      tryCatch({
        lines <- readLines(p, warn = FALSE, encoding = "UTF-8")
        if (length(lines) > 0) {
          # Mantener ultimas 200 lineas para rendimiento
          if (length(lines) > 200) {
            lines <- c(paste0("... [", length(lines) - 200, " lineas anteriores] ...\n"),
                       tail(lines, 200))
          }
          text <- paste(lines, collapse = "\n")
          # Enviar mensaje para auto-scroll
          session$sendCustomMessage("scrollConsole", list())
          return(text)
        }
      }, error = function(e) NULL)
    }

    console_text()
  })

  # Limpiar consola
  observeEvent(input$clear_console, {
    log_path(NULL)
    console_text("Consola limpiada.\n\n")
  })

  # --- Ejecutar analisis ---
  observeEvent(input$run, {
    df <- data_for_analysis()
    if (is.null(df)) {
      showNotification("Por favor, carga datos validos primero.", type = "error")
      return()
    }

    # Crear archivo de log
    log_dir <- file.path(tempdir(), "optimalfactor_logs")
    if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE)
    lf <- file.path(log_dir, paste0("efa_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
    file.create(lf)
    lf <- normalizePath(lf, winslash = "/")

    log_path(lf)
    analysis_running(TRUE)
    results(NULL)

    # Parsear items a excluir
    exclude_items_list <- NULL
    if (!is.null(input$exclude_items) && nzchar(trimws(input$exclude_items))) {
      exclude_items_list <- trimws(unlist(strsplit(input$exclude_items, ",")))
      exclude_items_list <- exclude_items_list[nzchar(exclude_items_list)]
      if (length(exclude_items_list) == 0) exclude_items_list <- NULL
    }

    # Mensaje inicial
    exclude_msg <- if (!is.null(exclude_items_list) && length(exclude_items_list) > 0) {
      paste0("- Items excluidos: ", paste(exclude_items_list, collapse = ", "), "\n")
    } else {
      ""
    }

    cat(sprintf("
================================================================================
   EFA-Boosting Studio - EFA-Boosting Analysis
   %s
================================================================================

Configuracion:
- Items: %d
- Factores: %d
- Estimador: %s
- Rotacion: %s
- Busqueda global: %s
%s
Objetivos:
- RMSEA <= %.3f
- SRMR <= %.3f
- CFI >= %.2f
- Carga minima: %.2f
- Min items/factor: %d

Iniciando optimizacion...

", format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      ncol(df), input$n_factors, input$estimator, input$rotation,
      if(input$use_global) "Si" else "No",
      exclude_msg,
      input$thr_rmsea, input$thr_srmr, input$thr_cfi,
      input$thr_loading, input$thr_min_items
    ), file = lf)

    # Parametros
    params <- list(
      data = df,
      name_items = input$prefix,
      n_factors = input$n_factors,
      n_sample = nrow(df),
      exclude_items = exclude_items_list,
      thresholds = list(
        loading = input$thr_loading,
        min_items_per_factor = input$thr_min_items,
        heywood_tol = 1e-6,
        near_heywood = 0.015,
        min_interfactor_correlation = input$min_interfactor
      ),
      model_config = list(
        estimator = input$estimator,
        rotation = input$rotation
      ),
      fit_config = list(
        targets = list(rmsea = input$thr_rmsea, srmr = input$thr_srmr, cfi = input$thr_cfi),
        margins = list(rmsea = 0.03, srmr = 0.03, cfi = 0.03),
        base_weights = list(rmsea = 0.50, srmr = 0.25, cfi = 0.25)
      ),
      use_global = input$use_global,
      global_opt = list(
        max_drop = if(input$use_global) input$max_drop else 1,
        max_global_combinations = 3000,
        verbose = TRUE,
        progress_bar = FALSE
      ),
      use_ai_analysis = input$use_ai,
      ai_config = list(
        api_key = input$api_key,
        gpt_model = input$gpt_model,
        language = input$language,
        item_definitions = parse_item_definitions(input$item_defs),
        only_removed = TRUE,
        analysis_detail = "detailed"
      ),
      verbose = TRUE,
      log_file = lf
    )

    # Ejecutar en proceso separado
    future_promise({
      # Configurar log
      options(efa_log_path = params$log_file)
      on.exit(options(efa_log_path = NULL), add = TRUE)

      # Funcion para escribir al log
      write_log <- function(...) {
        msg <- paste0(..., collapse = "")
        cat(msg, file = params$log_file, append = TRUE)
        cat(msg)
        flush.console()
      }

      # Verificar disponibilidad de efa_boosting
      if (requireNamespace("OptimalFactor", quietly = TRUE)) {
        write_log("Usando OptimalFactor::efa_boosting\n\n")

        # Redirigir output al archivo
        sink(params$log_file, append = TRUE, split = TRUE)
        on.exit(sink(), add = TRUE)

        result <- tryCatch({
          OptimalFactor::efa_boosting(
            data = params$data,
            name_items = params$name_items,
            n_factors = params$n_factors,
            n_sample = params$n_sample,
            exclude_items = params$exclude_items,
            thresholds = params$thresholds,
            model_config = params$model_config,
            fit_config = params$fit_config,
            use_global = params$use_global,
            global_opt = params$global_opt,
            use_ai_analysis = params$use_ai_analysis,
            ai_config = params$ai_config,
            verbose = TRUE
          )
        }, error = function(e) {
          write_log("\n\nERROR: ", conditionMessage(e), "\n")
          NULL
        })

        sink()

        if (!is.null(result)) {
          write_log("\n\n================================================================================\n")
          write_log("   ANALISIS COMPLETADO EXITOSAMENTE\n")
          write_log("================================================================================\n")
          write_log(sprintf("\nItems eliminados: %d\n", length(result$removed_items)))
          write_log(sprintf("RMSEA final: %.4f\n", result$final_rmsea))
          write_log(sprintf("Iteraciones: %d\n", result$iterations))
        }

        result

      } else {
        write_log("\nERROR: OptimalFactor no esta instalado.\n")
        write_log("Instala con: devtools::install_github('jventural/OptimalFactor')\n")
        NULL
      }

    }) %...>% (function(res) {
      results(res)
      analysis_running(FALSE)

      if (!is.null(res)) {
        showNotification("Analisis completado exitosamente", type = "message", duration = 5)
        updateNavlistPanel(session, "main_tabs", selected = "Resumen")
      } else {
        showNotification("El analisis termino con errores. Revisa la consola.", type = "warning", duration = 8)
      }
    }) %...!% (function(e) {
      analysis_running(FALSE)
      cat(sprintf("\n\nERROR CRITICO: %s\n", conditionMessage(e)),
          file = log_path(), append = TRUE)
      showNotification(paste("Error:", conditionMessage(e)), type = "error", duration = 10)
    })

  }, ignoreInit = TRUE)

  # --- Info de progreso ---
  output$progress_info <- renderText({
    invalidateLater(500, session)

    p <- log_path()
    if (!is.null(p) && file.exists(p)) {
      tryCatch({
        content <- paste(readLines(p, warn = FALSE), collapse = "\n")
        iters <- length(gregexpr("ITERATION\\s+\\d+", content, ignore.case = TRUE)[[1]])
        if (iters < 1) iters <- 0

        # Buscar ultimo RMSEA
        rmsea_matches <- gregexpr("RMSEA:\\s*([0-9.]+)", content)
        if (rmsea_matches[[1]][1] != -1) {
          last_pos <- tail(rmsea_matches[[1]], 1)
          last_rmsea <- substr(content, last_pos + 7, last_pos + 12)
          return(sprintf("Iteracion %d | RMSEA: %s", iters, last_rmsea))
        }

        sprintf("Iteracion %d", iters)
      }, error = function(e) "Procesando...")
    } else {
      "Iniciando..."
    }
  })

  # --- Tablas de resultados ---
  output$summary_table <- renderTable({
    req(results())
    res <- results()

    n_initial <- length(res$removed_items) + nrow(res$final_structure)
    n_final <- nrow(res$final_structure)
    pct <- round(100 * n_final / n_initial, 1)

    data.frame(
      Metrica = c("Items iniciales", "Items finales", "Items eliminados",
                  "Porcentaje retenido", "Iteraciones"),
      Valor = c(n_initial, n_final, length(res$removed_items),
                paste0(pct, "%"), res$iterations),
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%")

  output$fit_table <- renderTable({
    req(results())
    res <- results()
    bo <- res$bondades_original

    if (is.null(bo)) return(NULL)

    # Obtener ultima fila (modelo final)
    idx <- nrow(bo)

    get_val <- function(name) {
      cols <- grep(name, names(bo), ignore.case = TRUE, value = TRUE)
      if (length(cols) > 0) {
        val <- bo[[cols[1]]][idx]
        if (is.numeric(val)) sprintf("%.4f", val) else as.character(val)
      } else "N/A"
    }

    data.frame(
      Indice = c("Chi-cuadrado", "gl", "RMSEA", "SRMR", "CFI", "TLI"),
      Valor = c(
        get_val("chisq"),
        get_val("^df"),
        get_val("rmsea"),
        get_val("srmr"),
        get_val("cfi"),
        get_val("tli")
      ),
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%")

  output$phi_table <- renderTable({
    req(results())
    phi <- results()$inter_factor_correlation
    if (is.null(phi) || nrow(phi) <= 1) return(NULL)

    phi_df <- as.data.frame(round(phi, 3))
    colnames(phi_df) <- paste0("F", seq_len(ncol(phi_df)))
    rownames(phi_df) <- paste0("F", seq_len(nrow(phi_df)))
    phi_df
  }, rownames = TRUE, striped = TRUE, hover = TRUE, bordered = TRUE)

  # --- Grafico de evolucion ---
  output$fit_evolution_plot <- renderPlot({
    req(results())
    res <- results()

    if (is.null(res$steps_log) || nrow(res$steps_log) == 0) {
      plot.new()
      text(0.5, 0.5, "No hay datos de evolucion", cex = 1.5, col = "gray50")
      return()
    }

    df_plot <- res$steps_log
    df_plot$step <- as.numeric(df_plot$step)

    # Verificar columnas disponibles
    has_srmr <- "srmr" %in% names(df_plot)
    has_cfi <- "cfi" %in% names(df_plot)

    p <- ggplot(df_plot, aes(x = step)) +
      geom_line(aes(y = rmsea, color = "RMSEA"), linewidth = 1.2) +
      geom_point(aes(y = rmsea, color = "RMSEA"), size = 3)

    if (has_srmr) {
      p <- p +
        geom_line(aes(y = srmr, color = "SRMR"), linewidth = 1.2, linetype = "dashed") +
        geom_point(aes(y = srmr, color = "SRMR"), size = 3)
    }

    p <- p +
      geom_hline(yintercept = input$thr_rmsea, linetype = "dotted", color = "red", linewidth = 0.8) +
      scale_color_manual(values = c("RMSEA" = "#2C3E50", "SRMR" = "#18BC9C")) +
      labs(
        title = "Evolucion de indices de ajuste",
        x = "Iteracion",
        y = "Valor",
        color = "Indice"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "bottom"
      )

    print(p)
  })

  # --- DataTables ---
  output$final_dt <- renderDT({
    # Forzar dependencia reactiva
    res <- results()
    req(res)
    df <- res$final_structure
    req(df)

    load_cols <- which(startsWith(names(df), "f"))
    for (col in load_cols) {
      df[[col]] <- round(df[[col]], 3)
    }

    datatable(
      df,
      options = list(
        pageLength = 20,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      class = 'cell-border stripe hover',
      rownames = FALSE
    ) %>%
      formatStyle(
        columns = load_cols,
        backgroundColor = styleInterval(
          c(-0.3, 0.3),
          c('#ffcccc', 'white', '#ccffcc')
        )
      )
  }, server = FALSE)  # server=FALSE fuerza re-render completo

  output$steps_dt <- renderDT({
    # Forzar dependencia reactiva
    res <- results()
    req(res)
    df <- res$steps_log

    if (is.null(df) || nrow(df) == 0) {
      return(datatable(data.frame(Mensaje = "No se eliminaron items")))
    }

    df$rmsea <- round(df$rmsea, 4)
    if ("srmr" %in% names(df)) df$srmr <- round(df$srmr, 4)
    if ("cfi" %in% names(df)) df$cfi <- round(df$cfi, 4)

    datatable(
      df,
      options = list(
        pageLength = 20,
        scrollX = TRUE,
        order = list(list(0, 'asc'))
      ),
      class = 'cell-border stripe hover',
      rownames = FALSE
    )
  }, server = FALSE)  # server=FALSE fuerza re-render completo

  # --- Descargas ---
  output$dl_structure <- downloadHandler(
    filename = function() paste0("estructura_factorial_", Sys.Date(), ".csv"),
    content = function(file) {
      req(results())
      write.csv(results()$final_structure, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  output$dl_steps <- downloadHandler(
    filename = function() paste0("historial_eliminacion_", Sys.Date(), ".csv"),
    content = function(file) {
      req(results())
      write.csv(results()$steps_log, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
}

# =============================================================================
# EJECUTAR APP
# =============================================================================
shinyApp(ui, server)
