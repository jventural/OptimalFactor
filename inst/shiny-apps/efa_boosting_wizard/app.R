# =============================================================================
# OptimalFactor — EFA Boosting Wizard (v2)
# =============================================================================
# Guided 5-phase wizard around efa_boosting(): Data → Parallel Diagnostic →
# EFA Boosting (with conceptual AI analysis) → Reliability → External Validity.
# Uses bslib (flatly theme) to look distinct from the EasyValidation wizard.
# =============================================================================

suppressPackageStartupMessages({
  library(shiny); library(bslib); library(DT)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------
ui <- page_navbar(
  title = tags$span(
    tags$i(class = "fa fa-magic-sparkles",
            style = "color:#18BC9C; margin-right:8px;"),
    tags$strong("OptimalFactor"),
    tags$span("Wizard", style = "opacity:.55; margin-left:8px; font-size:14px;")
  ),
  id = "main_nav",
  theme = bs_theme(
    version = 5, bootswatch = "flatly",
    primary = "#2C3E50", secondary = "#18BC9C",
    success = "#18BC9C", info = "#3498DB",
    warning = "#F39C12", danger = "#E74C3C",
    base_font = font_google("Inter"),
    code_font = font_google("JetBrains Mono")),

  header = tags$head(tags$style(HTML("
    body { background:#F4F6F8; }
    .step-card {
      background:#fff; border:1px solid #E1E5EA; border-radius:12px;
      padding:24px; box-shadow:0 2px 8px rgba(44,62,80,.05);
    }
    .step-title {
      font-size:20px; font-weight:700; color:#2C3E50;
      margin-bottom:6px;
    }
    .step-subtitle {
      font-size:13px; color:#7F8C8D; margin-bottom:14px;
    }
    .step-body {
      background:#F8FAFB; border-radius:8px; padding:16px;
      font-family:'JetBrains Mono','Consolas',monospace;
      font-size:12.5px; color:#2C3E50; line-height:1.55;
      white-space:pre-wrap;
      max-height:420px; overflow-y:auto;
    }
    .action-row {
      display:grid; grid-template-columns:1fr 1fr; gap:14px;
      margin:16px 0;
    }
    @media (max-width: 800px) {
      .action-row { grid-template-columns:1fr; }
    }
    .action-box {
      border-radius:10px; padding:14px 16px; background:#fff;
      border:1px solid #E1E5EA; position:relative;
      box-shadow:0 2px 6px rgba(44,62,80,.06);
    }
    .action-box::before {
      content:' '; position:absolute; left:0; top:0; bottom:0;
      width:4px; border-radius:4px 0 0 4px;
    }
    .action-do::before    { background:#3498DB; }
    .action-after::before { background:#18BC9C; }
    .action-label {
      font-size:10.5px; font-weight:700; letter-spacing:.08em;
      color:#7F8C8D; margin-bottom:6px; text-transform:uppercase;
    }
    .action-do .action-label    { color:#2980B9; }
    .action-after .action-label { color:#16A085; }
    .action-text {
      font-size:14px; font-weight:600; color:#2C3E50;
    }
    .action-code {
      font-family:'JetBrains Mono',monospace; font-size:11px;
      color:#95A5A6; margin-top:4px;
    }
    .btn-go {
      background:#18BC9C; color:#fff; border:none; padding:10px 16px;
      border-radius:8px; font-weight:600; font-size:14px;
    }
    .btn-go:hover { background:#15A589; color:#fff; }
    .btn-back {
      background:#95A5A6; color:#fff; border:none; padding:10px 16px;
      border-radius:8px; font-weight:600; font-size:14px;
    }
    .stepper {
      display:flex; gap:6px; margin-bottom:18px;
    }
    .stepper-pill {
      flex:1; padding:8px 10px; border-radius:6px;
      background:#ECF0F1; color:#7F8C8D; text-align:center;
      font-size:11.5px; font-weight:600; letter-spacing:.04em;
    }
    .stepper-pill.active {
      background:#18BC9C; color:#fff;
    }
    .stepper-pill.done {
      background:#2C3E50; color:#fff;
    }
    .stat-card {
      background:#fff; border:1px solid #E1E5EA; border-radius:10px;
      padding:12px 16px; min-height:78px;
    }
    .stat-num { font-size:24px; font-weight:700; color:#2C3E50; line-height:1.1; }
    .stat-lbl { font-size:11px; color:#7F8C8D; font-weight:600;
                 text-transform:uppercase; letter-spacing:.04em; }
    .removed-pill {
      display:inline-block; background:#FDEDEC; color:#C0392B;
      border:1px solid #F5B7B1; padding:3px 9px; border-radius:14px;
      font-family:'JetBrains Mono',monospace; font-size:11.5px;
      margin:2px 3px;
    }
    .chat-box {
      background:#fff; border:1px solid #E1E5EA; border-radius:10px;
      padding:14px; max-height:520px; overflow-y:auto; font-size:14px;
    }
    .chat-msg { padding:12px 14px; border-radius:8px; margin-bottom:10px;
                line-height:1.55; font-size:14px; color:#1F2937; }
    .chat-user { background:#EBF5FB; border-left:3px solid #3498DB; }
    .chat-ai   { background:#E8F8F5; border-left:3px solid #18BC9C; }
    .chat-role { font-size:11px; font-weight:700; color:#7F8C8D;
                 text-transform:uppercase; letter-spacing:.06em; margin-bottom:6px; }
    /* Markdown rendering inside chat */
    .chat-msg p { margin: 0 0 8px 0; }
    .chat-msg p:last-child { margin-bottom: 0; }
    .chat-msg strong { color:#1F2937; font-weight:600; }
    .chat-msg em { color:#475569; font-style:italic; }
    .chat-msg ul, .chat-msg ol { margin:6px 0 8px 22px; padding:0; }
    .chat-msg li { margin-bottom:3px; }
    .chat-msg li > p { margin: 0 0 3px 0; }
    .chat-msg code {
      background:rgba(44,62,80,.08); color:#2C3E50; padding:1px 6px;
      border-radius:3px; font-family:'JetBrains Mono','Consolas',monospace;
      font-size:12px;
    }
    .chat-msg pre {
      background:#2C3E50; color:#ECF0F1; padding:12px 14px;
      border-radius:6px; overflow-x:auto; font-size:12.5px;
      font-family:'JetBrains Mono','Consolas',monospace;
      margin:8px 0;
    }
    .chat-msg pre code { background:transparent; color:#ECF0F1; padding:0; }
    .chat-msg blockquote {
      border-left:3px solid #BDC3C7; padding:4px 12px; margin:6px 0;
      color:#5D6D7E; background:rgba(189,195,199,.12);
    }
    .chat-msg table {
      border-collapse:collapse; margin:8px 0; font-size:13px;
    }
    .chat-msg th, .chat-msg td {
      border:1px solid #E1E5EA; padding:5px 9px;
    }
    .chat-msg th { background:#F4F6F8; font-weight:600; }
    .chat-msg h1, .chat-msg h2, .chat-msg h3, .chat-msg h4 {
      font-size:15px; font-weight:700; color:#1F2937;
      margin:10px 0 4px 0;
    }
    .chat-msg hr { border:none; border-top:1px solid #E1E5EA; margin:10px 0; }
    .ai-driving-badge {
      display:inline-block; background:#FFF7E0; color:#B45309;
      border:1px solid #F5C242; padding:3px 10px; border-radius:14px;
      font-size:11px; font-weight:700; margin-left:8px;
    }
    #shiny-notification-panel {
      position:fixed !important; top:50% !important; left:50% !important;
      right:auto !important; bottom:auto !important;
      transform: translate(-50%, -50%) !important;
      z-index:100000 !important;
      width:520px !important; max-width:92vw !important;
    }
    .shiny-notification {
      background:#fff !important; color:#2C3E50 !important;
      border:1px solid #E1E5EA !important; border-radius:12px !important;
      box-shadow:0 12px 32px rgba(44,62,80,.18) !important;
      padding:22px 24px !important; font-size:14.5px !important;
    }
    .shiny-notification-message {
      font-weight:700 !important; color:#2C3E50 !important;
      font-size:16px !important;
    }
    .shiny-progress-detail {
      font-family:'JetBrains Mono',monospace; font-size:12px;
      color:#7F8C8D; margin-top:4px;
    }
  ")),
  tags$script(HTML("
    // Autopilot: when server says click, click the visible accept button.
    Shiny.addCustomMessageHandler('autopilot_click', function(_) {
      var btn = document.getElementById('accept');
      if (btn && !btn.disabled) btn.click();
    });
    // Chat: Enter sends, Shift+Enter inserts newline.
    $(document).on('keydown', '#chat_input', function(e) {
      if (e.key === 'Enter' && !e.shiftKey) {
        e.preventDefault();
        var btn = document.getElementById('chat_send');
        if (btn) btn.click();
      }
    });
  "))),

  # ---- Panel: Wizard ----
  nav_panel(title = "Wizard", icon = icon("wand-magic-sparkles"),
    layout_sidebar(
      sidebar = sidebar(width = 360, title = "Configuración",
        accordion(id = "cfg_acc", open = c("data", "items"),

          accordion_panel(title = "1. Datos", value = "data",
                           icon = icon("database"),
            fileInput("data_file", "Archivo (CSV/XLSX/RDS)",
                       accept = c(".csv", ".tsv", ".xlsx", ".xls", ".rds"),
                       buttonLabel = "Buscar...",
                       placeholder = "Ningún archivo cargado"),
            uiOutput("data_brief")
          ),

          accordion_panel(title = "2. Ítems", value = "items",
                           icon = icon("list-check"),
            textInput("name_items", "Prefijo de ítems", value = "",
                       placeholder = "p.ej. PMV, EAI"),
            tags$small(class = "text-muted",
              "Auto-detección por prefijo. El selector de abajo permite override."),
            selectizeInput("items", "Override (selecciona columnas):",
                            choices = NULL, multiple = TRUE,
                            options = list(plugins = list("remove_button")))
          ),

          accordion_panel(title = "3. Configuración EFA",
                           value = "cfg", icon = icon("cogs"),
            selectInput("estimator", "Estimador",
                         choices = c("WLSMV (ordinal)" = "WLSMV",
                                      "MLR" = "MLR",
                                      "ML" = "ML"),
                         selected = "WLSMV"),
            selectInput("rotation", "Rotación",
                         choices = c("oblimin" = "oblimin",
                                      "promax" = "promax",
                                      "varimax" = "varimax",
                                      "geomin" = "geomin"),
                         selected = "oblimin"),
            sliderInput("loading_threshold", "Carga mínima |λ|",
                         min = 0.20, max = 0.50, value = 0.30, step = 0.05),
            numericInput("min_items_per_factor", "Mín. ítems / factor",
                          value = 3L, min = 2L, max = 6L, step = 1L)
          ),

          accordion_panel(title = "4. IA (opcional)", value = "ia",
                           icon = icon("robot"),
            checkboxInput("use_ai", "Activar análisis con IA", FALSE),
            conditionalPanel(condition = "input.use_ai",
              passwordInput("openai_api_key", "API Key OpenAI",
                             placeholder = "sk-..."),
              selectInput("gpt_model", "Modelo",
                           choices = c("gpt-4.1" = "gpt-4.1",
                                        "gpt-4o" = "gpt-4o",
                                        "gpt-4" = "gpt-4",
                                        "gpt-3.5-turbo" = "gpt-3.5-turbo"),
                           selected = "gpt-4.1"),
              selectInput("ai_language", "Idioma del análisis",
                           choices = c("español" = "spanish",
                                        "english" = "english"),
                           selected = "spanish"),
              checkboxInput("autopilot", "Modo autopiloto (la IA decide cada paso)", FALSE),
              conditionalPanel(condition = "input.autopilot",
                sliderInput("autopilot_read_s",
                  "Segundos de lectura antes de que la IA confirme",
                  min = 1, max = 8, value = 3, step = 0.5),
                tags$small(class = "text-muted",
                  HTML("La IA confirmará cada paso automáticamente tras N segundos.<br>El botón <em>Tomar el control</em> te devuelve el control manual."))
              )
            )
          ),

          accordion_panel(title = "5. Idioma UI", value = "lang",
                           icon = icon("globe"),
            radioButtons("language", NULL,
                          choices = c("Español" = "es", "English" = "en"),
                          selected = "es", inline = TRUE)
          )
        ),
        tags$hr(),
        actionButton("start", HTML('<i class="fa fa-play"></i>&nbsp;Iniciar / Reiniciar'),
                      class = "btn-go w-100"),
      ),

      # Main panel
      uiOutput("stepper_ui"),
      uiOutput("stats_row"),
      uiOutput("step_ui")
    )
  ),

  # ---- Panel: Resultados ----
  nav_panel(title = "Resultados", icon = icon("chart-column"),
    layout_column_wrap(width = 1/2,
      card(card_header(tags$strong("Estructura factorial final")),
            DT::DTOutput("tbl_loadings")),
      card(card_header(tags$strong("Índices de ajuste por iteración")),
            DT::DTOutput("tbl_fit"))),
    br(),
    layout_column_wrap(width = 1/2,
      card(card_header(tags$strong("Fiabilidad")),
            DT::DTOutput("tbl_reliability")),
      navset_card_tab(
        title = tags$strong("Validez externa"),
        nav_panel(title = "Tabla", icon = icon("table"),
          DT::DTOutput("tbl_validity")),
        nav_panel(title = "Gráfico", icon = icon("chart-simple"),
          plotOutput("plt_validity", height = "320px")))),
    br(),
    navset_card_tab(
      title = tags$strong("Validez convergente / discriminante"),
      nav_panel(title = "Tabla",  icon = icon("table"),
        DT::DTOutput("tbl_convergent")),
      nav_panel(title = "Heatmap", icon = icon("border-all"),
        plotOutput("plt_convergent", height = "420px"))
    )
  ),

  # ---- Panel: Reporte ----
  nav_panel(title = "Reporte", icon = icon("file-lines"),
    tags$style(HTML("
      .dl-hero {
        display: grid;
        grid-template-columns: 1fr 1fr;
        gap: 18px;
        margin-bottom: 18px;
      }
      @media (max-width: 900px) { .dl-hero { grid-template-columns: 1fr; } }
      .dl-card {
        position: relative;
        padding: 22px 22px 22px 84px;
        border-radius: 14px;
        color: #fff;
        min-height: 150px;
        box-shadow: 0 6px 18px rgba(0,0,0,.08);
        transition: transform .15s ease, box-shadow .15s ease;
      }
      .dl-card:hover { transform: translateY(-2px);
                        box-shadow: 0 10px 24px rgba(0,0,0,.12); }
      .dl-card .dl-icon {
        position: absolute; left: 22px; top: 22px;
        width: 50px; height: 50px;
        display: flex; align-items: center; justify-content: center;
        font-size: 24px; color: #fff;
        background: rgba(255,255,255,.15); border-radius: 12px;
      }
      .dl-card h4 {
        margin: 0 0 6px 0; font-size: 18px; font-weight: 700; color: #fff;
      }
      .dl-card p {
        margin: 0 0 14px 0; font-size: 13px; line-height: 1.45;
        opacity: .92; color: #fff;
      }
      .dl-txt   { background: linear-gradient(135deg,#18BC9C 0%,#0F8B72 100%); }
      .dl-docx  { background: linear-gradient(135deg,#3498DB 0%,#1E40AF 100%); }
      .dl-card .btn {
        background: #fff !important; color: #1F2937 !important;
        border: none !important; font-weight: 600 !important;
        padding: 8px 16px !important; border-radius: 8px !important;
      }
      .dl-card .btn:hover {
        background: #F3F4F6 !important; color: #111827 !important;
      }
      .dl-tag {
        display: inline-block; font-size: 11px; font-weight: 600;
        padding: 2px 9px; border-radius: 999px;
        background: rgba(255,255,255,.22); color: #fff; margin-left: 6px;
        vertical-align: middle;
      }
    ")),
    div(class = "dl-hero",
      # TXT card
      div(class = "dl-card dl-txt",
        div(class = "dl-icon", icon("file-lines")),
        tags$h4("Log completo (.txt)",
                 tags$span(class = "dl-tag", "Sin API")),
        tags$p("Auditoría paso a paso: consenso multi-método, cargas finales, ",
                "índices por iteración, omega/alpha, validez externa, ",
                "análisis conceptual de la IA y bitácora verbose."),
        downloadButton("dl_full_txt",
                        label = " Descargar TXT",
                        icon  = icon("download"))),
      # DOCX card
      div(class = "dl-card dl-docx",
        div(class = "dl-icon", icon("file-word")),
        tags$h4("Manuscrito Word (.docx)",
                 tags$span(class = "dl-tag", "Requiere IA")),
        tags$p("Secciones 'Análisis de datos' (Método 2.4) y 'Resultados' ",
                "(3.1–3.5) en estilo APA-7, con tablas reales de cargas, ",
                "ajuste, fiabilidad y correlaciones embebidas."),
        downloadButton("dl_manuscript_docx",
                        label = " Descargar Word",
                        icon  = icon("download")))
    ),
    navset_card_tab(
      nav_panel("Reporte de optimización",
        verbatimTextOutput("boost_text")),
      nav_panel("Análisis conceptual (IA)",
        uiOutput("conceptual_ui"))
    )
  ),

  # ---- Panel: Trace (verbose paso a paso) ----
  nav_panel(title = "Trace", icon = icon("terminal"),
    card(card_header(
      div(style = "display:flex; justify-content:space-between; align-items:center;",
        tags$strong("Bitácora paso a paso de efa_boosting()"),
        downloadButton("dl_trace", "Descargar .txt",
                        class = "btn-sm", style = "font-size:11px;"))),
      tags$p(tags$small(class = "text-muted",
        "Cada iteración muestra: estructura factorial actual, índices de ajuste, ítem eliminado y razón. Reproduce exactamente lo que verías corriendo efa_boosting(verbose = TRUE) en consola.")),
      tags$pre(id = "trace_pre",
        style = "background:#1F2937; color:#F9FAFB; padding:18px; border-radius:8px;
                 font-family:'JetBrains Mono','Consolas',monospace; font-size:12px;
                 max-height:680px; overflow-y:auto; line-height:1.45;",
        textOutput("trace_text", inline = FALSE))
    )
  ),

  # ---- Panel: IA Chat ----
  nav_panel(title = "IA Chat", icon = icon("comments"),
    layout_sidebar(
      sidebar = sidebar(width = 320, title = "Contexto",
        tags$p(tags$small(
          "El asistente conoce el dataset cargado, los ítems detectados,
           la estructura factorial final y los ítems eliminados.")),
        uiOutput("chat_status")
      ),
      uiOutput("chat_ui"),
      div(style = "margin-top:12px; display:flex; gap:8px;",
        textAreaInput("chat_input", NULL,
                       placeholder = "Pregunta algo sobre el modelo, los ítems eliminados, las cargas…",
                       width = "100%", rows = 2),
        actionButton("chat_send", icon("paper-plane"),
                      class = "btn-go", style = "align-self:flex-end;"))
    )
  )
)

# -----------------------------------------------------------------------------
# Server
# -----------------------------------------------------------------------------
server <- function(input, output, session) {

  rv <- reactiveValues(
    phase = 0L,           # 0 = not started, 1..5 = phases, 6 = done
    sub   = NULL,         # for phase 2: NULL (initial) | "decide" | for 3: NULL | "review"
    data  = NULL,
    items = character(0),
    config = list(),
    results = list(),
    boost_obj = NULL,
    chat = list(),        # list of list(role, content)
    started = FALSE,
    # Convergent / discriminant instruments (Phase 5b).
    # Each element is a list:
    #   list(id=uuid, label, multi=TRUE|FALSE,
    #        items=chr (when !multi),
    #        subscales=list(list(name, items)) (when multi),
    #        expected="convergent"|"discriminant",
    #        include_total=TRUE|FALSE)
    convergent_instruments = list(),
    convergent_uid = 0L     # incremental id for new cards
  )

  # ---- Data reading ----
  raw_data <- reactive({
    f <- input$data_file
    if (is.null(f)) return(NULL)
    ext <- tools::file_ext(f$name)
    df <- switch(tolower(ext),
      "csv" = utils::read.csv(f$datapath),
      "tsv" = utils::read.delim(f$datapath),
      "xlsx" = , "xls" = if (requireNamespace("readxl", quietly = TRUE))
                            as.data.frame(readxl::read_excel(f$datapath))
                         else stop("Instala readxl para archivos Excel."),
      "rds" = readRDS(f$datapath),
      stop("Tipo de archivo no soportado: ", ext))
    df
  })

  output$data_brief <- renderUI({
    df <- raw_data(); if (is.null(df)) return(tags$small(class = "text-muted",
      "Carga un archivo para previsualizar."))
    tags$div(class = "alert alert-info py-2",
      tags$strong(sprintf("%d filas × %d columnas", nrow(df), ncol(df))),
      tags$br(),
      tags$small(paste(utils::head(colnames(df), 6), collapse = ", "),
                  if (ncol(df) > 6) "...")
    )
  })

  # ---- Item override: ALL columns (like EasyValidation) ----
  observe({
    df <- raw_data()
    if (is.null(df)) {
      updateSelectizeInput(session, "items", choices = character(0),
                            selected = character(0)); return()
    }
    updateSelectizeInput(session, "items",
                          choices = colnames(df), selected = character(0))
  })

  # ===================================================================
  # Helper: phase labels (Spanish primary)
  # ===================================================================
  phase_label <- function(p, lang = "es") {
    es <- identical(lang, "es")
    switch(as.character(p),
      "1" = if (es) "Datos" else "Data",
      "2" = if (es) "Diagnóstico paralelo" else "Parallel diagnostic",
      "3" = if (es) "EFA Boosting" else "EFA Boosting",
      "4" = if (es) "Fiabilidad" else "Reliability",
      "5" = if (es) "Validez externa" else "External validity",
      if (es) "(completado)" else "(done)")
  }

  # Label for the *action* triggered by clicking Confirm/Continue at the
  # current phase p_now. Clicking at phase N actually runs the analysis
  # that advances the wizard *into* phase N+1, so the progress bar should
  # describe what the click is about to do, not the current phase name.
  phase_action_label <- function(p_now, lang = "es") {
    es <- identical(lang, "es")
    switch(as.character(p_now),
      "1" = if (es) "Ejecutando análisis paralelo (Kaiser, PA, MAP, BIC)"
            else    "Running parallel analysis (Kaiser, PA, MAP, BIC)",
      "2" = if (es) "Implementando efa_boosting()..."
            else    "Running efa_boosting()...",
      "3" = if (es) "Calculando fiabilidad (omega + alpha)"
            else    "Computing reliability (omega + alpha)",
      "4" = if (es) "Avanzando a validez externa"
            else    "Advancing to external validity",
      "5" = if (es) "Calculando correlaciones con variables externas"
            else    "Computing correlations with external variables",
      if (es) "Procesando..." else "Processing...")
  }

  # ===================================================================
  # Stepper + stats
  # ===================================================================
  output$stepper_ui <- renderUI({
    if (!rv$started) return(NULL)
    es <- identical(input$language, "es")
    steps <- if (es) c("1 Datos", "2 Diagnóstico", "3 Boosting",
                       "4 Fiabilidad", "5 Validez")
             else    c("1 Data", "2 Diagnostic", "3 Boosting",
                       "4 Reliability", "5 Validity")
    div(class = "stepper",
      lapply(seq_along(steps), function(i) {
        cls <- if (i == rv$phase) "stepper-pill active"
               else if (i < rv$phase) "stepper-pill done"
               else "stepper-pill"
        div(class = cls, steps[i])
      })
    )
  })

  output$stats_row <- renderUI({
    if (!rv$started) return(NULL)
    es <- identical(input$language, "es")
    layout_column_wrap(width = 1/4, fill = FALSE,
      div(class = "stat-card",
        div(class = "stat-num", length(rv$items)),
        div(class = "stat-lbl", if (es) "Ítems" else "Items")),
      div(class = "stat-card",
        div(class = "stat-num", if (!is.null(rv$data)) nrow(rv$data) else 0),
        div(class = "stat-lbl", if (es) "Filas" else "Rows")),
      div(class = "stat-card",
        div(class = "stat-num", phase_label(rv$phase, input$language)),
        div(class = "stat-lbl", if (es) "Fase" else "Phase")),
      div(class = "stat-card",
        div(class = "stat-num",
            if (isTRUE(input$use_ai) && nzchar(input$openai_api_key %||% ""))
              tags$span(style="color:#18BC9C;", "IA")
            else
              tags$span(style="color:#95A5A6;", "—")),
        div(class = "stat-lbl", if (es) "Asistente" else "Assistant"))
    )
  })

  # ===================================================================
  # MAIN step card per phase
  # ===================================================================
  # Wraps the accept button. When autopilot is ON + AI is configured, show
  # a banner with the "Take control" button instead, and arm a timer to
  # auto-click accept after N seconds.
  .render_step_with_autopilot <- function(step_node) {
    autopilot_active <- isTRUE(input$autopilot) &&
                          isTRUE(input$use_ai) &&
                          nzchar(input$openai_api_key %||% "")
    if (!autopilot_active) return(step_node)
    es <- identical(input$language, "es")
    secs <- as.numeric(input$autopilot_read_s %||% 3)
    banner <- div(class = "step-card",
      style = "background:#FFF7E0; border:1px solid #F5C242; margin-bottom:10px;",
      div(style = "display:flex; align-items:center; justify-content:space-between;",
        div(
          tags$span(icon("robot"), style = "color:#B45309; font-size:18px; margin-right:8px;"),
          tags$strong(if (es) "Modo IA — la IA conducirá este paso"
                       else    "AI mode — the AI will drive this step"),
          tags$br(),
          tags$small(class = "text-muted",
            sprintf(if (es) "Confirmará automáticamente en %.1f segundos."
                    else    "Will auto-confirm in %.1f seconds.", secs))),
        actionButton("autopilot_off",
          if (es) HTML('<i class="fa fa-hand"></i>&nbsp;Tomar el control')
          else    HTML('<i class="fa fa-hand"></i>&nbsp;Take control'),
          style = "background:#B45309; color:#fff; border:none; padding:8px 14px; border-radius:6px; font-weight:600;")))
    tagList(banner, step_node)
  }

  output$step_ui <- renderUI({
    if (!rv$started) {
      return(div(class = "step-card",
        div(class = "step-title", "Bienvenido"),
        div(class = "step-subtitle",
          "Carga un archivo, define el prefijo de ítems y presiona Iniciar."),
        tags$ul(
          tags$li("El wizard correrá automáticamente un análisis paralelo (Horn, 1965)."),
          tags$li("Después aplicará efa_boosting() para optimizar la estructura."),
          tags$li("Calculará fiabilidad ω + α por factor."),
          tags$li("Te permitirá explorar validez externa con variables sociodemográficas."),
          tags$li("Si activas IA, recibirás un análisis conceptual de los ítems eliminados.")
        )
      ))
    }
    es <- identical(input$language, "es")
    autopilot_active <- isTRUE(input$autopilot) &&
                          isTRUE(input$use_ai) &&
                          nzchar(input$openai_api_key %||% "")
    # The "resume AI" button is shown when the user has the AI stack ready
    # (use_ai + key) but the autopilot toggle is currently off. This makes
    # the round-trip pause → resume one-click on the same toolbar.
    can_resume_ai <- !isTRUE(input$autopilot) &&
                       isTRUE(input$use_ai) &&
                       nzchar(input$openai_api_key %||% "")
    # Allow going back from any phase 2..6 (including the "completed"
    # pseudo-state at phase 6 — the user may want to revisit results).
    can_go_back <- rv$phase >= 2L && rv$phase <= 6L
    toolbar <- div(
      style = paste(
        "display:flex; gap:8px; align-items:center;",
        "margin-bottom:10px; flex-wrap:wrap;"),
      if (can_go_back)
        actionButton("step_back",
          if (es) HTML('<i class="fa fa-arrow-left"></i>&nbsp;Atrás (fase anterior)')
          else    HTML('<i class="fa fa-arrow-left"></i>&nbsp;Back (previous phase)'),
          class = "btn-back",
          style = "padding:7px 14px; font-weight:600;")
      else NULL,
      if (autopilot_active)
        actionButton("autopilot_off",
          if (es) HTML('<i class="fa fa-pause"></i>&nbsp;Pausar autopiloto')
          else    HTML('<i class="fa fa-pause"></i>&nbsp;Pause autopilot'),
          style = "background:#B45309; color:#fff; border:none; padding:7px 14px; border-radius:6px; font-weight:600;")
      else NULL,
      if (can_resume_ai)
        actionButton("autopilot_resume",
          if (es) HTML('<i class="fa fa-play"></i>&nbsp;Reanudar IA')
          else    HTML('<i class="fa fa-play"></i>&nbsp;Resume AI'),
          style = "background:#18BC9C; color:#fff; border:none; padding:7px 14px; border-radius:6px; font-weight:600;")
      else NULL,
      div(style = "flex:1;"),  # spacer pushes hint right
      tags$small(class = "text-muted",
        if (can_go_back)
          (if (es) "Puedes retroceder en cualquier momento. Los resultados ya calculados se conservan."
           else    "You can go back anytime. Computed results are kept.")
        else "")
    )
    tagList(toolbar,
            .render_step_with_autopilot(.build_step(rv, input)))
  })

  # ===================================================================
  # AI classification of detected instruments for autopilot in Phase 5
  # ===================================================================

  # Ask OpenAI to classify each candidate as "convergent" or "discriminant"
  # based on its label and (when known) the principal instrument name.
  # Returns a named list keyed by candidate label, each with $expected and
  # optional $reason. Fallback to "convergent" when the call fails.
  .ai_classify_instruments <- function(candidates, principal_label,
                                          api_key, model = "gpt-4.1") {
    if (length(candidates) == 0L) return(list())
    if (!nzchar(api_key)) {
      out <- list()
      for (c_i in candidates)
        out[[c_i$label]] <- list(expected = "convergent", reason = "(sin IA)")
      return(out)
    }
    if (!requireNamespace("httr", quietly = TRUE) ||
        !requireNamespace("jsonlite", quietly = TRUE)) {
      out <- list()
      for (c_i in candidates)
        out[[c_i$label]] <- list(expected = "convergent",
                                  reason = "(httr/jsonlite no disponibles)")
      return(out)
    }
    descs <- vapply(candidates, function(c_i) {
      kind <- if (isTRUE(c_i$multi))
        sprintf("multidim (%d subescalas: %s)",
                 length(c_i$subscales),
                 paste(vapply(c_i$subscales, `[[`, character(1), "name"),
                       collapse = ", "))
      else "unidim"
      sprintf("- %s  [%d items, %s]", c_i$label, c_i$n_items, kind)
    }, character(1))
    sys_prompt <- paste0(
      "Eres un psicometra senior. Para una escala psicologica llamada '",
      principal_label %||% "(desconocida)",
      "', clasifica cada instrumento detectado como 'convergent' (mide ",
      "un constructo teoricamente relacionado, se espera correlacion ",
      "moderada o fuerte) o 'discriminant' (mide un constructo distinto, ",
      "se espera correlacion baja). ",
      "Responde EXCLUSIVAMENTE con JSON valido (sin markdown, sin code ",
      "fences) con la forma: {\"items\":[",
      "{\"label\":\"XXX\",\"expected\":\"convergent\",\"reason\":\"...\"},...]}.")
    user_prompt <- paste0(
      "Principal: ", principal_label %||% "(desconocida)", "\n",
      "Instrumentos detectados en los datos:\n",
      paste(descs, collapse = "\n"),
      "\n\nClasifica cada uno. Si solo conoces el acronimo (ej. BAI, DERS, ",
      "PHQ, AAQ, MAAS, RSES) usa tu conocimiento de literatura ",
      "psicometrica para decidir. Si el acronimo es totalmente opaco, ",
      "marcalo como 'convergent' por defecto y explica 'opaco'.")
    out <- tryCatch({
      r <- httr::POST("https://api.openai.com/v1/chat/completions",
        httr::add_headers(Authorization = paste("Bearer", api_key),
                            `Content-Type` = "application/json"),
        httr::timeout(60),
        body = jsonlite::toJSON(list(
          model = model,
          response_format = list(type = "json_object"),
          messages = list(
            list(role = "system", content = sys_prompt),
            list(role = "user",   content = user_prompt)),
          temperature = 0.1), auto_unbox = TRUE))
      if (httr::status_code(r) >= 400)
        stop("HTTP ", httr::status_code(r))
      txt <- httr::content(r, as = "parsed")$choices[[1]]$message$content
      parsed <- jsonlite::fromJSON(txt, simplifyDataFrame = FALSE)
      res <- list()
      for (it in (parsed$items %||% list())) {
        if (is.null(it$label) || !nzchar(it$label)) next
        exp <- tolower(it$expected %||% "convergent")
        if (!(exp %in% c("convergent","discriminant"))) exp <- "convergent"
        res[[it$label]] <- list(expected = exp,
                                  reason = it$reason %||% "")
      }
      # Fill any missing candidates with the default
      for (c_i in candidates)
        if (is.null(res[[c_i$label]]))
          res[[c_i$label]] <- list(expected = "convergent",
                                    reason = "(no clasificado)")
      res
    }, error = function(e) {
      message("[autopilot/ai_classify] fallback (", e$message, ")")
      r <- list()
      for (c_i in candidates)
        r[[c_i$label]] <- list(expected = "convergent",
                                reason = sprintf("(error IA: %s)", e$message))
      r
    })
    out
  }

  # Autopilot timer: every time pending step changes and autopilot is ON,
  # arm a one-shot timer to auto-click the accept button.
  # NOTE: later::later() runs the callback OUTSIDE the reactive context,
  # so we must snapshot every reactive value (input$, rv$, ...) into
  # local variables BEFORE passing them into the deferred closure.
  autopilot_armed_at <- reactiveVal(NULL)
  observe({
    rv$phase  # depend on phase changes
    rv$boost_obj
    rv$results
    if (!isTRUE(input$autopilot) ||
        !isTRUE(input$use_ai) ||
        !nzchar(input$openai_api_key %||% "") ||
        !isTRUE(rv$started) ||
        rv$phase < 1L || rv$phase > 5L) return()
    secs <- as.numeric(input$autopilot_read_s %||% 3)
    armed_at <- Sys.time()
    autopilot_armed_at(armed_at)
    # Capture the session id so the deferred callback can talk back via JS.
    sess <- session
    later::later(function() {
      # We are OUTSIDE a reactive consumer here. Use isolate() to peek
      # at the current value of input$autopilot and the armed timestamp.
      still_on <- isolate(isTRUE(input$autopilot))
      current_armed <- isolate(autopilot_armed_at())
      if (!still_on) return()
      if (is.null(current_armed) ||
          !identical(current_armed, armed_at)) return()  # superseded
      sess$sendCustomMessage("autopilot_click", list())
    }, delay = secs)
  })

  observeEvent(input$autopilot_off, {
    updateCheckboxInput(session, "autopilot", value = FALSE)
    autopilot_armed_at(NULL)
    showNotification("Has tomado el control manual.", type = "message", duration = 3)
  })

  # Resume autopilot: just flip the checkbox back to TRUE. The standing
  # autopilot observer is reactive on input$autopilot + rv$phase, so the
  # next click triggers it and the read-delay countdown begins immediately
  # for the current step.
  observeEvent(input$autopilot_resume, {
    if (!isTRUE(input$use_ai) ||
        !nzchar(input$openai_api_key %||% "")) {
      showNotification(
        "Activa IA y proporciona una API key antes de reanudar.",
        type = "warning", duration = 5)
      return()
    }
    updateCheckboxInput(session, "autopilot", value = TRUE)
    showNotification(
      sprintf("Autopiloto reanudado en Fase %d.", rv$phase),
      type = "message", duration = 3)
  })

  # Step back: rewind to the previous phase AND pause autopilot for safety.
  # Computed results (rv$results$phaseN) are kept; if the user re-confirms a
  # phase they'll be overwritten. The convergent panel pre-fill flag is
  # preserved so the orange banner remains visible when returning to phase 5.
  observeEvent(input$step_back, {
    current <- rv$phase
    if (current < 2L || current > 6L) return()
    # Always pause autopilot when the user manually goes back — they want
    # to inspect something, the AI shouldn't auto-confirm from under them.
    updateCheckboxInput(session, "autopilot", value = FALSE)
    autopilot_armed_at(NULL)
    target <- max(1L, current - 1L)
    # Phase 2 has a sub-state ("review" after parallel diag). Coming back
    # to phase 2 should land on the diag screen (sub = NULL) so the user
    # can change n_factors again.
    rv$sub <- NULL
    rv$phase <- target
    showNotification(sprintf("Volviste a la Fase %d. Autopiloto pausado.",
                              target),
                      type = "message", duration = 4)
  })

  # ===================================================================
  # Autopilot pre-fill of Phase 5: when the wizard reaches phase 5 with
  # autopilot ON, AI key available and NO convergent instruments yet,
  # auto-detect candidates, ask the IA to classify each as convergent
  # or discriminant, and write the resulting cards into rv$convergent_
  # instruments. The user can still review/edit; the regular autopilot
  # tick will fire the Calcular button after the read-delay.
  # ===================================================================
  observeEvent(rv$phase, {
    if (rv$phase != 5L) return()
    if (!isTRUE(input$autopilot)) return()
    if (!isTRUE(input$use_ai)) return()
    if (!nzchar(input$openai_api_key %||% "")) return()
    if (length(rv$convergent_instruments) > 0L) return()  # already filled
    if (isTRUE(rv$.conv_ai_in_progress)) return()
    rv$.conv_ai_in_progress <- TRUE
    on.exit(rv$.conv_ai_in_progress <- FALSE, add = TRUE)

    # 1. Auto-detect
    excl <- unique(c(rv$items,
                      isolate(input$external_vars) %||% character(0)))
    cands <- .detect_convergent_groups(rv$data, exclude_cols = excl)
    if (length(cands) == 0L) {
      rv$.conv_ai_prefilled <- FALSE
      rv$.conv_ai_reasons   <- NULL
      return()
    }

    # 2. Cancel the pending autopilot tick while the AI is thinking, so we
    #    don't auto-click "Calcular" with empty cards.
    autopilot_armed_at(NULL)

    pr <- shiny::Progress$new(session); on.exit(pr$close(), add = TRUE)
    pr$set(message = "Autopiloto: clasificando instrumentos con IA...",
           detail  = sprintf("%d instrumento(s) detectado(s).", length(cands)),
           value   = 0.3)

    # 3. AI classification
    cls <- .ai_classify_instruments(
      candidates = cands,
      principal_label = isolate(input$name_items),
      api_key  = isolate(input$openai_api_key),
      model    = isolate(input$gpt_model %||% "gpt-4.1"))

    # 4. Write into rv$convergent_instruments
    added <- list(); reasons <- list()
    for (c_i in cands) {
      decision <- cls[[c_i$label]] %||%
        list(expected = "convergent", reason = "(default)")
      new_instr <- list(
        id        = .conv_new_id(),
        label     = c_i$label,
        multi     = c_i$multi,
        items     = if (isTRUE(c_i$multi)) character(0) else c_i$items,
        subscales = c_i$subscales,
        expected  = decision$expected,
        include_total = isTRUE(c_i$multi))  # default: include total when multi
      added[[length(added) + 1L]] <- new_instr
      reasons[[c_i$label]] <- list(expected = decision$expected,
                                     reason = decision$reason %||% "")
    }
    rv$convergent_instruments <- added
    rv$.conv_ai_prefilled <- TRUE
    rv$.conv_ai_reasons   <- reasons

    showNotification(sprintf(
      "Autopiloto: pre-cargados %d instrumento(s). Revisa antes de calcular.",
      length(added)), type = "message", duration = 6)

    # 5. Re-arm the autopilot tick: the user has `autopilot_read_s`
    #    seconds to review or take control before "Calcular" fires.
    armed_at <- Sys.time()
    autopilot_armed_at(armed_at)
    sess <- session
    secs <- as.numeric(isolate(input$autopilot_read_s %||% 3))
    # Give the user extra breathing room in phase 5 — they may want to
    # tweak expected direction. Add 4 s on top of the configured delay.
    later::later(function() {
      still_on <- isolate(isTRUE(input$autopilot))
      current_armed <- isolate(autopilot_armed_at())
      if (!still_on) return()
      if (is.null(current_armed) ||
          !identical(current_armed, armed_at)) return()
      sess$sendCustomMessage("autopilot_click", list())
    }, delay = secs + 4)
  }, ignoreInit = TRUE)

  .build_step <- function(rv, input) {
    es <- identical(input$language, "es")
    p <- rv$phase

    # Phase 1 — Data confirmation
    if (p == 1L) {
      n_data <- nrow(rv$data); n_items <- length(rv$items)
      return(div(class = "step-card",
        div(class = "step-title",
          sprintf("Fase 1 — %s", if (es) "Confirmar datos cargados"
                                  else    "Confirm loaded data")),
        div(class = "step-subtitle", sprintf("%d filas × %d ítems detectados",
          n_data, n_items)),
        div(class = "step-body",
          paste(
            sprintf("Filas              : %d", n_data),
            sprintf("Ítems              : %d  (%s)", n_items,
              paste(utils::head(rv$items, 12), collapse = ", ")),
            sprintf("Estimador          : %s", input$estimator),
            sprintf("Rotación           : %s", input$rotation),
            sprintf("Carga mínima |λ|   : %.2f", input$loading_threshold),
            sprintf("Mín. ítems / factor: %d", input$min_items_per_factor),
            sep = "\n")),
        div(class = "action-row",
          div(class = "action-box action-do",
            div(class = "action-label", if (es) "Acción propuesta" else "Proposed action"),
            div(class = "action-text",
              if (es) "Confirmar dataset y correr análisis paralelo"
              else    "Confirm dataset and run parallel analysis"),
            div(class = "action-code", "confirm_data → parallel_analysis")),
          div(class = "action-box action-after",
            div(class = "action-label", if (es) "Qué pasará" else "What will happen"),
            div(class = "action-text",
              if (es) "Se ejecutarán Parallel Analysis, MAP, Kaiser y BIC para sugerir el número de factores."
              else    "Parallel Analysis, MAP, Kaiser and BIC will be computed to suggest the number of factors.")))
        ,
        actionButton("accept", HTML('<i class="fa fa-check"></i>&nbsp;Confirmar'),
                      class = "btn-go")
      ))
    }

    # Phase 2 — Parallel diagnostic (auto runs after Phase 1 accept)
    if (p == 2L) {
      diag <- rv$results$phase2
      if (is.null(diag)) {
        return(div(class = "step-card",
          div(class = "step-title", "Fase 2 — Diagnóstico paralelo"),
          div(class = "step-subtitle", "Ejecutando análisis paralelo + MAP + Kaiser + BIC..."),
          tags$div(class = "spinner-border text-secondary"),
          tags$small(class="text-muted", " Esto puede tomar unos segundos")))
      }
      rec <- diag$recommendation
      return(div(class = "step-card",
        div(class = "step-title",
          sprintf("Fase 2 — %s", if (es) "Recomendación de factores" else "Factor recommendation")),
        div(class = "step-subtitle",
          sprintf(if (es) "Consenso de %d métodos → recomienda %d factor(es)"
                  else    "Consensus of %d methods → recommends %d factor(s)",
                  nrow(diag$consensus), rec)),
        div(class = "step-body", diag$consensus_text),
        tags$hr(),
        div(style = "margin:14px 0;",
          tags$strong(if (es) "¿Cuántos factores extraer?" else "How many factors to extract?"),
          tags$br(),
          radioButtons("nfactors_choice", NULL,
            choices = c(
              setNames(sprintf("auto:%d", rec),
                       sprintf(if (es) "Usar recomendación (consenso): %d factor(es)"
                               else    "Use recommendation (consensus): %d factor(s)", rec)),
              setNames("manual", if (es) "Fijar valor teórico manualmente"
                                  else    "Set theoretical value manually")),
            selected = sprintf("auto:%d", rec)),
          conditionalPanel(condition = "input.nfactors_choice == 'manual'",
            numericInput("nfactors_manual",
              if (es) "Número de factores (teórico)" else "Number of factors (theoretical)",
              value = rec, min = 1L, max = 12L, step = 1L))),
        div(class = "action-row",
          div(class = "action-box action-do",
            div(class = "action-label", if (es) "Acción propuesta" else "Proposed action"),
            div(class = "action-text",
              if (es) "Fijar número de factores y correr EFA boosting"
              else    "Fix number of factors and run EFA boosting"),
            div(class = "action-code", "fix_n_factors → run_efa_boosting")),
          div(class = "action-box action-after",
            div(class = "action-label", if (es) "Qué pasará" else "What will happen"),
            div(class = "action-text",
              if (es) "OptimalFactor::efa_boosting() optimizará iterativamente eliminando ítems con cargas cruzadas o débiles."
              else    "OptimalFactor::efa_boosting() will iteratively remove cross-loaded or weak items.")))
        ,
        actionButton("accept", HTML('<i class="fa fa-play"></i>&nbsp;Aplicar EFA Boosting'),
                      class = "btn-go")
      ))
    }

    # Phase 3 — EFA Boosting (apply / review)
    if (p == 3L) {
      if (is.null(rv$boost_obj)) {
        return(div(class = "step-card",
          div(class = "step-title", "Fase 3 — EFA Boosting"),
          div(class = "step-subtitle", "Ejecutando boosting iterativo..."),
          tags$div(class = "spinner-border text-secondary"),
          tags$small(class="text-muted", " Esto puede tomar varios minutos con muchos ítems")))
      }
      b <- rv$boost_obj
      fit <- if (!is.null(b$bondades_original) && nrow(b$bondades_original) > 0L)
        b$bondades_original[nrow(b$bondades_original), ] else NULL
      cfi <- as.numeric(fit$cfi.scaled %||% NA)
      tli <- as.numeric(fit$tli.scaled %||% NA)
      rmsea <- as.numeric(fit$rmsea.scaled %||% NA)
      srmr <- as.numeric(fit$srmr %||% NA)
      removed <- b$removed_items %||% character(0)
      n_final <- nrow(b$final_structure)
      n_init <- n_final + length(removed)
      stop_reason <- b$stop_reason %||% "unknown"
      fit_line <- function(v, t, op) {
        if (is.null(v) || is.na(v)) return("n/a")
        ok <- if (identical(op, ">=")) v >= t else v <= t
        sprintf("%.3f (criterio %s %.2f) → %s",
                 v, op, t, if (ok) "OK" else "NO CUMPLE")
      }
      removed_pills <- if (length(removed) > 0L)
        lapply(removed, function(r) tags$span(class = "removed-pill", r))
      else tags$em(class = "text-muted",
                    if (es) "Ningún ítem eliminado" else "No items removed")

      return(div(class = "step-card",
        div(class = "step-title",
          sprintf("Fase 3 — %s", if (es) "Revisar resultado del boosting"
                                  else    "Review boosting result")),
        div(class = "step-subtitle",
          sprintf(if (es) "Boosting completado: %d ítems → %d ítems (stop_reason: %s)"
                  else    "Boosting done: %d items → %d items (stop_reason: %s)",
                  n_init, n_final, stop_reason)),

        # Items removed prominently
        tags$h6(class = "mt-3", if (es) "Ítems eliminados:" else "Items removed:"),
        div(style = "margin-bottom:14px;", removed_pills),

        div(class = "step-body",
          paste(
            if (es) "ÍNDICES DE AJUSTE FINALES" else "FINAL FIT INDICES",
            sprintf("  CFI   = %s", fit_line(cfi, 0.95, ">=")),
            sprintf("  TLI   = %s", fit_line(tli, 0.95, ">=")),
            sprintf("  RMSEA = %s", fit_line(rmsea, 0.08, "<=")),
            sprintf("  SRMR  = %s", fit_line(srmr, 0.08, "<=")),
            "",
            sprintf("Iteraciones del boosting: %d", b$iterations %||% 0L),
            sep = "\n")),
        div(class = "action-row",
          div(class = "action-box action-do",
            div(class = "action-label", if (es) "Acción propuesta" else "Proposed action"),
            div(class = "action-text",
              if (es) "Confirmar solución y calcular fiabilidad"
              else    "Confirm solution and compute reliability"),
            div(class = "action-code", "confirm_efa → reliability")),
          div(class = "action-box action-after",
            div(class = "action-label", if (es) "Qué pasará" else "What will happen"),
            div(class = "action-text",
              if (es) "Se calculará ω categórico (semTools::compRelSEM) y α de Cronbach por factor."
              else    "Categorical ω (semTools::compRelSEM) and Cronbach's α per factor will be computed.")))
        ,
        actionButton("accept", HTML('<i class="fa fa-check"></i>&nbsp;Confirmar y continuar'),
                      class = "btn-go")
      ))
    }

    # Phase 4 — Reliability
    if (p == 4L) {
      rel <- rv$results$phase4
      if (is.null(rel)) {
        return(div(class = "step-card",
          div(class = "step-title", "Fase 4 — Fiabilidad"),
          div(class = "step-subtitle", "Calculando ω + α..."),
          tags$div(class = "spinner-border text-secondary")))
      }
      om <- rel$omega; al <- rel$alpha
      lines <- character(0)
      lines <- c(lines, if (es) "OMEGA CATEGÓRICO (semTools::compRelSEM, ord.scale=TRUE)"
                        else    "CATEGORICAL OMEGA (semTools::compRelSEM, ord.scale=TRUE)")
      if (!is.null(om) && length(om) > 0L && !all(is.na(om))) {
        nm <- names(om); if (is.null(nm)) nm <- paste0("f", seq_along(om))
        lines <- c(lines, sprintf("  %-12s = %.3f", nm, as.numeric(om)))
      } else lines <- c(lines, "  (no disponible)")
      lines <- c(lines, "", if (es) "ALFA DE CRONBACH (psych::alpha)"
                            else    "CRONBACH'S ALPHA (psych::alpha)")
      if (!is.null(al) && length(al) > 0L && !all(is.na(al))) {
        nm <- names(al); if (is.null(nm)) nm <- paste0("f", seq_along(al))
        lines <- c(lines, sprintf("  %-12s = %.3f", nm, as.numeric(al)))
      } else lines <- c(lines, "  (no disponible)")
      return(div(class = "step-card",
        div(class = "step-title",
          sprintf("Fase 4 — %s", if (es) "Fiabilidad calculada" else "Reliability computed")),
        div(class = "step-subtitle",
          if (es) "Revisar valores y continuar a validez externa."
          else    "Review values and continue to external validity."),
        div(class = "step-body", paste(lines, collapse = "\n")),
        div(class = "action-row",
          div(class = "action-box action-do",
            div(class = "action-label", if (es) "Acción propuesta" else "Proposed action"),
            div(class = "action-text",
              if (es) "Continuar a validez externa" else "Continue to external validity"),
            div(class = "action-code", "confirm_reliability → external_validity")),
          div(class = "action-box action-after",
            div(class = "action-label", if (es) "Qué pasará" else "What will happen"),
            div(class = "action-text",
              if (es) "Podrás elegir variables sociodemográficas y se computarán correlaciones de Pearson con los puntajes factoriales."
              else    "You can pick demographic variables and Pearson correlations with factor sum scores will be computed.")))
        ,
        actionButton("accept", HTML('<i class="fa fa-arrow-right"></i>&nbsp;Continuar'),
                      class = "btn-go")
      ))
    }

    # Phase 5 — External validity
    if (p == 5L) {
      val <- rv$results$phase5
      if (is.null(val)) {
        ext_pool <- if (!is.null(rv$data))
          setdiff(colnames(rv$data), rv$items) else character(0)
        ext_pool <- ext_pool[vapply(ext_pool,
          function(v) is.numeric(rv$data[[v]]), logical(1))]
        # Banner shown when autopilot pre-filled the convergent panel.
        ai_banner <- NULL
        if (isTRUE(rv$.conv_ai_prefilled) &&
            length(rv$convergent_instruments) > 0L) {
          reasons <- rv$.conv_ai_reasons %||% list()
          rows <- lapply(rv$convergent_instruments, function(it) {
            rinfo <- reasons[[it$label]] %||% list()
            chip <- if (identical(it$expected, "discriminant"))
              tags$span(style = "background:#F59E0B; color:#fff; padding:2px 8px; border-radius:999px; font-size:11px; font-weight:600;",
                         "discriminante")
            else
              tags$span(style = "background:#18BC9C; color:#fff; padding:2px 8px; border-radius:999px; font-size:11px; font-weight:600;",
                         "convergente")
            div(style = "padding:4px 0; font-size:13px;",
              tags$strong(it$label), " · ", chip, " · ",
              tags$small(class = "text-muted",
                rinfo$reason %||% ""))
          })
          ai_banner <- div(
            style = paste(
              "background: linear-gradient(135deg,#FFF7ED 0%,#FED7AA 100%);",
              "border-left: 4px solid #EA580C; padding: 12px 14px;",
              "border-radius: 8px; margin-bottom: 14px;"),
            div(style = "display:flex; justify-content:space-between; align-items:flex-start; gap:10px;",
              div(
                tags$strong(style = "color:#9A3412;",
                  sprintf("🤖 Propuesta de la IA: %d instrumento(s) clasificado(s)",
                          length(rv$convergent_instruments))),
                tags$div(style = "color:#7C2D12; font-size:12px; margin-top:2px;",
                  "Revisa antes de calcular. Puedes editar cada card en (b), o presionar 'Tomar el control' para detener el cronómetro.")),
              actionButton("autopilot_off",
                if (es) "Tomar el control" else "Take control",
                class = "btn-back",
                style = "background:#fff; color:#9A3412; border:1px solid #EA580C; padding:6px 12px; font-size:12px; white-space:nowrap;",
                icon = icon("hand"))),
            div(style = "margin-top:10px;", rows))
        }

        return(div(class = "step-card",
          div(class = "step-title",
            "Fase 5 — Validez basada en relaciones con otras variables"),
          div(class = "step-subtitle",
            if (es) "Variables sueltas (criterio sociodemográfico) e instrumentos de comparación (validez convergente / discriminante)."
            else    "Standalone criterion variables AND comparison instruments (convergent / discriminant validity)."),
          ai_banner,
          # ---- (a) Variables externas sueltas (criterio simple) ----
          div(style = "margin-top:10px;",
            tags$strong(if (es) "(a) Variables criterio (sueltas)"
                         else    "(a) Standalone criterion variables")),
          tags$small(class = "text-muted",
            if (es) "Pearson r de cada factor contra cada variable numérica seleccionada."
            else    "Pearson r of each factor vs each selected numeric variable."),
          selectizeInput("external_vars",
            if (es) "Variables externas (numéricas):" else "External variables (numeric):",
            choices = ext_pool, multiple = TRUE,
            options = list(plugins = list("remove_button"))),

          # ---- (b) Instrumentos de comparación ----
          div(style = "margin-top:18px; border-top:1px solid #E5E7EB; padding-top:14px;",
            div(style = "display:flex; justify-content:space-between; align-items:center;",
              tags$strong(if (es) "(b) Instrumentos de comparación"
                           else    "(b) Comparison instruments"),
              div(
                actionButton("conv_detect", icon("magnifying-glass"),
                  title = if (es) "Detectar automáticamente"
                          else    "Auto-detect",
                  class = "btn-back", style = "padding:4px 10px; font-size:12px;"),
                actionButton("conv_add", icon("plus"),
                  title = if (es) "Añadir manualmente" else "Add manually",
                  class = "btn-go", style = "padding:4px 10px; font-size:12px;")
              )),
            tags$small(class = "text-muted",
              if (es) "Cada instrumento puede ser unidimensional (un total) o multidimensional (subescalas detectadas por sub-prefijo, ej. DERS_AC, DERS_OB)."
              else    "Each instrument may be unidimensional (one total) or multidimensional (subscales detected by sub-prefix)."),
            uiOutput("conv_cards_ui")),

          div(class = "action-row", style = "margin-top:16px;",
            div(class = "action-box action-do",
              div(class = "action-label", if (es) "Acción propuesta" else "Proposed action"),
              div(class = "action-text",
                if (es) "Computar correlaciones + veredicto"
                else    "Compute correlations + verdict"),
              div(class = "action-code", "compute_validity + convergence")),
            div(class = "action-box action-after",
              div(class = "action-label", if (es) "Qué pasará" else "What will happen"),
              div(class = "action-text",
                if (es) "Se construye un total por factor del instrumento principal y para cada instrumento de comparación se calculan totales (o subtotales si es multidimensional). Cada par se correlaciona, se reporta r, IC95%, p, magnitud (Cohen) y veredicto (convergencia/discriminación)."
                else    "Builds factor totals and instrument totals (or sub-totals if multi); correlates each pair, reports r, 95% CI, p, magnitude (Cohen) and verdict.")))
          ,
          actionButton("accept", HTML('<i class="fa fa-calculator"></i>&nbsp;Calcular'),
                        class = "btn-go")
        ))
      }
      # Show results
      conv <- rv$results$phase5b
      conv_summary <- NULL
      if (!is.null(conv) && !isTRUE(conv$skipped) && !is.null(conv$table)) {
        tbl <- conv$table
        # Aggregate verdict counts per instrument.
        verdict_lines <- tapply(tbl$verdict, tbl$instrument, function(v) {
          tab <- table(v)
          paste(sprintf("%s=%d", names(tab), tab), collapse = "; ")
        })
        conv_summary <- div(style = "margin-top:14px;",
          tags$strong("Resumen de instrumentos de comparación"),
          div(class = "step-body",
            paste(sprintf("• %s: %s", names(verdict_lines), verdict_lines),
                  collapse = "\n")),
          tags$small(class = "text-muted",
            sprintf("%d par(es) evaluado(s) sobre %d instrumento(s). Detalle completo en la tab 'Resultados'.",
                     nrow(tbl), length(unique(tbl$instrument)))))
      }
      return(div(class = "step-card",
        div(class = "step-title", "Fase 5 — Validez calculada"),
        div(class = "step-subtitle",
          if (es) "Correlaciones con variables sueltas + validez convergente/discriminante por instrumento."
          else    "Standalone correlations + convergent/discriminant validity per instrument."),
        div(class = "step-body",
          if (is.null(val$corr_df) || nrow(val$corr_df) == 0L)
            "(Sin variables criterio sueltas seleccionadas.)"
          else paste(capture.output(print(val$corr_df, row.names = FALSE)),
                      collapse = "\n")),
        conv_summary,
        tags$p(tags$small(class = "text-muted",
          if (es) "Wizard completado. Revisa Resultados y Reporte."
          else    "Wizard completed. Inspect Results and Report.")),
        actionButton("accept", HTML('<i class="fa fa-flag-checkered"></i>&nbsp;Finalizar'),
                      class = "btn-go")
      ))
    }

    # Done
    div(class = "step-card",
      div(class = "step-title", "✓ Wizard completado"),
      div(class = "step-subtitle",
        "Explora 'Resultados', 'Reporte' o 'IA Chat' para más detalle."),
      actionButton("start_again", HTML('<i class="fa fa-redo"></i>&nbsp;Reiniciar'),
                    class = "btn-back"))
  }

  # ===================================================================
  # Apply action when user clicks Confirmar/Continuar
  # ===================================================================
  observeEvent(input$accept, {
    p_now <- rv$phase
    pr <- shiny::Progress$new(session); on.exit(pr$close(), add = TRUE)
    pr$set(message = phase_action_label(p_now, input$language), value = 0.05)

    withCallingHandlers({
      if (p_now == 1L) {
        rv$phase <- 2L
        # Auto-run parallel diagnostic
        df_items <- rv$data[, rv$items, drop = FALSE]
        diag <- .suggest_factors_consensus(df_items,
          max_factors = min(6L, floor((ncol(df_items) - 1L) / 2L)))
        rv$results$phase2 <- diag
      } else if (p_now == 2L) {
        chosen <- isolate(input$nfactors_choice %||% sprintf("auto:%d",
          rv$results$phase2$recommendation))
        nf <- if (identical(chosen, "manual")) as.integer(isolate(input$nfactors_manual))
              else as.integer(sub("^auto:", "", chosen))
        rv$config$n_factors <- nf
        rv$phase <- 3L
        # Run boosting — capture verbose stdout (cat()) so the Trace tab
        # can show the same step-by-step structure the user sees in R.
        df_items <- rv$data[, rv$items, drop = FALSE]
        use_ai <- isTRUE(input$use_ai) && nzchar(input$openai_api_key %||% "")
        message(sprintf("Running efa_boosting with %d factor(s)...", nf))
        tmpf <- tempfile(fileext = ".log")
        con  <- file(tmpf, open = "wt")
        sink(con, type = "output", split = FALSE)
        b <- tryCatch(
          OptimalFactor::efa_boosting(
            data = df_items, name_items = input$name_items, n_factors = nf,
            thresholds = list(loading = input$loading_threshold,
                               min_items_per_factor = as.integer(input$min_items_per_factor)),
            model_config = list(estimator = input$estimator,
                                 rotation = input$rotation),
            performance = list(max_candidates_eval = 12, smart_pruning = TRUE,
                                emit_progress = TRUE),
            use_ai_analysis = use_ai,
            # efa_boosting() requires ai_config$item_definitions to fire
            # the conceptual analysis. When the user hasn't provided real
            # definitions, fall back to item names so the AI block runs
            # anyway (less context but still documents WHY each item
            # was dropped). Default model = gpt-4.1 to match EasyValidation.
            ai_config = list(
              api_key            = input$openai_api_key %||% "",
              gpt_model          = input$gpt_model %||% "gpt-4.1",
              language           = input$ai_language %||% "spanish",
              generate_names     = use_ai,
              only_removed       = TRUE,
              analysis_detail    = "detailed",
              domain_name        = "Constructo de la escala",
              scale_title        = "Escala bajo análisis",
              construct_definition = "",
              model_name         = "EFA Model",
              item_definitions   = setNames(as.list(rv$items), rv$items)),
            verbose = TRUE),
          finally = { sink(type = "output"); close(con) })
        rv$boost_obj <- b
        rv$boost_console <- tryCatch(
          paste(readLines(tmpf, warn = FALSE), collapse = "\n"),
          error = function(e) "")
        rv$results$phase3 <- list(boost = b,
          report = tryCatch(
            OptimalFactor::report_efa_results(b, show_plot = TRUE, print = FALSE),
            error = function(e) NULL))
      } else if (p_now == 3L) {
        rv$phase <- 4L
        message("Computing omega + alpha...")
        rel <- .compute_reliability(rv)
        rv$results$phase4 <- rel
      } else if (p_now == 4L) {
        rv$phase <- 5L
      } else if (p_now == 5L) {
        if (is.null(rv$results$phase5)) {
          val <- .compute_external_validity(rv,
            ext_vars = isolate(input$external_vars))
          message("Computing convergent / discriminant validity...")
          conv <- .compute_convergent_validity(rv,
            instruments = rv$convergent_instruments)
          rv$results$phase5  <- val
          rv$results$phase5b <- conv
        } else {
          rv$phase <- 6L
        }
      }
    }, message = function(m) {
      txt <- trimws(gsub("[\r\n]+", " ", conditionMessage(m)))
      if (nzchar(txt) && nchar(txt) < 160) pr$set(detail = txt)
    })
    pr$set(value = 1)
  })

  observeEvent(input$start_again, {
    rv$phase <- 0L; rv$started <- FALSE
    rv$results <- list(); rv$boost_obj <- NULL
    rv$chat <- list()
    rv$convergent_instruments <- list()
    rv$.conv_ai_prefilled <- FALSE
    rv$.conv_ai_reasons   <- NULL
  })

  # ===================================================================
  # Helpers (consensus, reliability, validity)
  # ===================================================================
  .suggest_factors_consensus <- function(item_data, max_factors) {
    consensus <- data.frame(method = character(), suggested_k = integer(),
                              details = character(), stringsAsFactors = FALSE)
    message("  [1/4] Computing polychoric correlations & eigenvalues...")
    R <- tryCatch(psych::polychoric(item_data)$rho,
                   error = function(e) cor(item_data, use = "pairwise.complete.obs"))
    eigenvalues <- eigen(R, symmetric = TRUE, only.values = TRUE)$values
    kaiser_k <- sum(eigenvalues > 1)
    consensus <- rbind(consensus, data.frame(
      method = "Kaiser (eigenvalue > 1)", suggested_k = as.integer(kaiser_k),
      details = sprintf("EV > 1: %s",
        paste(round(eigenvalues[eigenvalues > 1], 2), collapse = ", ")),
      stringsAsFactors = FALSE))
    message(sprintf("    Kaiser: %d", kaiser_k))

    message("  [2/4] Parallel Analysis (50 sims)...")
    pa <- tryCatch(psych::fa.parallel(item_data, fa = "fa", plot = FALSE,
                                       n.iter = 50, quant = 0.95),
                    error = function(e) NULL)
    if (!is.null(pa)) {
      consensus <- rbind(consensus, data.frame(
        method = "Parallel Analysis", suggested_k = as.integer(pa$nfact),
        details = "EV above 95th percentile of simulated",
        stringsAsFactors = FALSE))
      message(sprintf("    PA: %d", pa$nfact))
    }

    message("  [3/4] MAP + BIC (psych::nfactors)...")
    nf <- tryCatch(psych::nfactors(item_data, n = max_factors, cor = "poly",
                                     rotate = "oblimin", fm = "minres"),
                    error = function(e) NULL)
    if (!is.null(nf)) {
      if (!is.null(nf$map)) {
        map_vals <- nf$map[!is.na(nf$map)]
        if (length(map_vals) > 0L) {
          map_k <- as.integer(which.min(map_vals))
          consensus <- rbind(consensus, data.frame(
            method = "MAP (Velicer)", suggested_k = map_k,
            details = "Minimum Average Partial correlation",
            stringsAsFactors = FALSE))
          message(sprintf("    MAP: %d", map_k))
        }
      }
      if (!is.null(nf$vss.stats) && "BIC" %in% colnames(nf$vss.stats)) {
        bic_vals <- nf$vss.stats[, "BIC"]; bic_vals <- bic_vals[!is.na(bic_vals)]
        if (length(bic_vals) > 0L) {
          bic_k <- as.integer(which.min(bic_vals))
          consensus <- rbind(consensus, data.frame(
            method = "BIC", suggested_k = bic_k,
            details = "Minimum Bayesian Information Criterion",
            stringsAsFactors = FALSE))
          message(sprintf("    BIC: %d", bic_k))
        }
      }
    }
    votes <- table(consensus$suggested_k)
    recommendation <- as.integer(names(votes)[which.max(votes)])
    n_votes <- max(votes); n_methods <- nrow(consensus)
    message(sprintf("\n  RECOMENDACIÓN: %d factor(es)  (%d/%d métodos coinciden)",
                     recommendation, n_votes, n_methods))
    consensus_text <- paste(
      vapply(seq_len(nrow(consensus)), function(i)
        sprintf("  %-25s → %d  (%s)",
                 consensus$method[i], consensus$suggested_k[i],
                 consensus$details[i]),
        character(1)),
      collapse = "\n")
    consensus_text <- paste0(consensus_text,
      sprintf("\n  %s\n  RECOMENDACIÓN: %d factor(es)  (%d/%d métodos coinciden)",
              strrep("─", 60), recommendation, n_votes, n_methods))
    list(consensus = consensus, consensus_text = consensus_text,
         recommendation = recommendation, eigenvalues = eigenvalues)
  }

  .efa_to_lavaan_syntax <- function(loadings, threshold = 0.30) {
    cn <- colnames(loadings)
    item_col <- intersect(c("Items","Item"), cn)[1]
    fact_cols <- cn[grepl("^[fF][0-9]+$", cn)]
    L <- as.matrix(loadings[, fact_cols, drop = FALSE]); L[is.na(L)] <- 0
    dom <- max.col(abs(L), ties.method = "first")
    parts <- vapply(seq_along(fact_cols), function(fi) {
      its <- as.character(loadings[[item_col]][dom == fi &
        apply(abs(L), 1, max) >= threshold])
      if (length(its) >= 2L) sprintf("%s =~ %s", fact_cols[fi],
                                       paste(its, collapse = " + "))
      else ""
    }, character(1))
    paste(parts[nzchar(parts)], collapse = "\n")
  }

  .compute_reliability <- function(rv) {
    df_items <- rv$data[, rv$items, drop = FALSE]
    syntax <- NULL
    if (!is.null(rv$boost_obj)) {
      retained <- as.character(rv$boost_obj$final_structure$Items %||%
                                 rv$boost_obj$final_structure$Item)
      df_items <- df_items[, retained, drop = FALSE]
      syntax <- .efa_to_lavaan_syntax(rv$boost_obj$final_structure,
                                        threshold = input$loading_threshold)
    }
    omega <- tryCatch({
      if (!is.null(syntax) && nzchar(syntax) &&
          requireNamespace("lavaan", quietly = TRUE) &&
          requireNamespace("semTools", quietly = TRUE)) {
        fit <- lavaan::cfa(syntax, data = df_items, ordered = TRUE,
                            estimator = input$estimator, std.lv = TRUE)
        if (lavaan::lavInspect(fit, "converged")) {
          om <- semTools::compRelSEM(fit, ord.scale = TRUE)
          message(sprintf("ω: %s",
            paste(names(om), "=", round(om, 3), collapse = ", ")))
          om
        } else NA_real_
      } else NA_real_
    }, error = function(e) { message("Omega err: ", e$message); NA_real_ })
    alpha <- tryCatch({
      if (!is.null(rv$boost_obj) && requireNamespace("psych", quietly = TRUE)) {
        ld <- rv$boost_obj$final_structure
        cn <- colnames(ld); item_col <- intersect(c("Items","Item"), cn)[1]
        fact_cols <- cn[grepl("^[fF][0-9]+$", cn)]
        L <- as.matrix(ld[, fact_cols, drop = FALSE]); L[is.na(L)] <- 0
        dom <- max.col(abs(L), ties.method = "first")
        out <- numeric(length(fact_cols))
        for (k in seq_along(fact_cols)) {
          its <- as.character(ld[[item_col]][dom == k])
          its <- its[its %in% colnames(df_items)]
          if (length(its) >= 2L) {
            a <- suppressWarnings(psych::alpha(df_items[, its, drop = FALSE],
                                                 check.keys = TRUE))
            out[k] <- as.numeric(a$total$raw_alpha)
          } else out[k] <- NA_real_
        }
        setNames(out, fact_cols)
      } else NA
    }, error = function(e) NA)
    list(omega = omega, alpha = alpha)
  }

  .compute_external_validity <- function(rv, ext_vars) {
    if (length(ext_vars) == 0L || is.null(rv$boost_obj))
      return(list(corr_df = NULL))
    df <- rv$data
    ld <- rv$boost_obj$final_structure
    cn <- colnames(ld); item_col <- intersect(c("Items","Item"), cn)[1]
    fact_cols <- cn[grepl("^[fF][0-9]+$", cn)]
    L <- as.matrix(ld[, fact_cols, drop = FALSE]); L[is.na(L)] <- 0
    dom <- max.col(abs(L), ties.method = "first")
    scores <- as.data.frame(matrix(NA_real_, nrow = nrow(df),
                                     ncol = length(fact_cols)))
    colnames(scores) <- fact_cols
    for (k in seq_along(fact_cols)) {
      its <- as.character(ld[[item_col]][dom == k])
      its <- its[its %in% colnames(df)]
      if (length(its) >= 1L)
        scores[[k]] <- rowSums(df[, its, drop = FALSE], na.rm = TRUE)
    }
    rows <- list()
    for (ev in ext_vars) {
      v <- df[[ev]]; if (!is.numeric(v)) next
      for (k in seq_along(fact_cols)) {
        ct <- suppressWarnings(stats::cor.test(scores[[k]], v))
        rows[[length(rows) + 1L]] <- data.frame(
          external = ev, factor = fact_cols[k],
          r = round(ct$estimate, 3),
          ci_low = round(ct$conf.int[1], 3),
          ci_up = round(ct$conf.int[2], 3),
          p = signif(ct$p.value, 3),
          stringsAsFactors = FALSE)
      }
    }
    corr_df <- if (length(rows) > 0L) do.call(rbind, rows) else NULL
    rownames(corr_df) <- NULL
    list(corr_df = corr_df, scores = scores)
  }

  # ===================================================================
  # Convergent / discriminant validity helpers
  # ===================================================================

  # Magnitude of a correlation per Cohen (1988): small >=.10, medium >=.30,
  # large >=.50. Sign is irrelevant for size; absolute value is used.
  .cohen_magnitude <- function(r) {
    if (is.na(r)) return(NA_character_)
    a <- abs(r)
    if (a >= 0.50) "grande"
    else if (a >= 0.30) "moderada"
    else if (a >= 0.10) "pequeña"
    else "insignificante"
  }

  # Verdict for one (r, p) pair given expected direction.
  # expected = "convergent": want |r| high, p < .05 in the predicted sign.
  # expected = "discriminant": want |r| low (< .30), regardless of p.
  .verdict_convergence <- function(r, p, expected = "convergent") {
    if (is.na(r) || is.na(p)) return("indeterminado")
    if (identical(expected, "discriminant")) {
      if (abs(r) < 0.30) return("discrimina (|r|<.30)")
      if (abs(r) < 0.50) return("discriminación dudosa")
      return("NO discrimina (|r|>=.50)")
    }
    if (p >= 0.05) return("no significativa")
    if (abs(r) >= 0.50) return("convergencia fuerte")
    if (abs(r) >= 0.30) return("convergencia moderada")
    if (abs(r) >= 0.10) return("convergencia débil")
    "convergencia no respaldada"
  }

  # Auto-detection of candidate instruments from column names.
  # Strategy:
  #   1. Exclude principal items, external_vars and obvious non-numeric cols.
  #   2. Group remaining columns by greedy prefix on letters-only stem.
  #   3. For each group of size >= 3, detect sub-prefix structure with
  #      regex "^(STEM)[._-]?([A-Z]{1,4})\\d+$" → splits into subscales.
  #   4. Return list of candidates: list(label, prefix, multi, subscales,
  #      items, n_items).
  .detect_convergent_groups <- function(data, exclude_cols) {
    pool <- setdiff(colnames(data), exclude_cols)
    pool <- pool[vapply(pool, function(v) is.numeric(data[[v]]), logical(1))]
    if (length(pool) == 0L) return(list())

    # Extract alphabetic stem from each column (everything before the
    # first digit or trailing alphanumeric block).
    extract_stem <- function(x) {
      # capture leading uppercase block, optional separator, optional
      # uppercase block, before digits
      m <- regmatches(x, regexec("^([A-Za-z]{2,})", x))[[1L]]
      if (length(m) < 2L) return(NA_character_)
      toupper(m[2L])
    }
    stems <- vapply(pool, extract_stem, character(1))
    keep <- !is.na(stems) & nzchar(stems)
    pool  <- pool[keep]; stems <- stems[keep]
    if (length(pool) == 0L) return(list())

    candidates <- list()
    for (stem in unique(stems)) {
      grp_cols <- pool[stems == stem]
      if (length(grp_cols) < 3L) next   # too short to be an instrument

      # Detect sub-structure inside this stem. Patterns to try:
      #   STEM_AC1, STEM_AC2, STEM_OB1, ...   (separator _ or . or -)
      #   STEMA1, STEMB2, ...                 (one-letter subscale tag)
      sub_pat1 <- sprintf("^%s[._-]([A-Za-z]{1,5})(\\d+)$", stem)
      sub_pat2 <- sprintf("^%s([A-Za-z]{1,5})(\\d+)$", stem)
      sub_pat3 <- sprintf("^%s(\\d+)$", stem)  # purely numeric (uni)

      sub_tags <- character(0)
      for (cn in grp_cols) {
        m <- regmatches(cn, regexec(sub_pat1, cn))[[1L]]
        if (length(m) >= 2L) { sub_tags <- c(sub_tags, toupper(m[2L])); next }
        m <- regmatches(cn, regexec(sub_pat2, cn))[[1L]]
        if (length(m) >= 2L) { sub_tags <- c(sub_tags, toupper(m[2L])); next }
        sub_tags <- c(sub_tags, NA_character_)
      }
      # If multiple distinct non-NA tags appear (and each tag has >=2 items),
      # treat as multidimensional.
      tag_table <- table(sub_tags[!is.na(sub_tags)])
      tag_table <- tag_table[tag_table >= 2L]
      if (length(tag_table) >= 2L) {
        # multidimensional
        subscales <- lapply(names(tag_table), function(tag) {
          its <- grp_cols[!is.na(sub_tags) & sub_tags == tag]
          list(name = tag, items = its)
        })
        candidates[[length(candidates) + 1L]] <- list(
          label = stem, prefix = stem, multi = TRUE,
          subscales = subscales,
          items = unlist(lapply(subscales, `[[`, "items")),
          n_items = sum(vapply(subscales, function(s) length(s$items),
                                integer(1))))
      } else {
        # unidimensional candidate
        candidates[[length(candidates) + 1L]] <- list(
          label = stem, prefix = stem, multi = FALSE,
          subscales = list(),
          items = grp_cols,
          n_items = length(grp_cols))
      }
    }
    candidates
  }

  # Mini parallel analysis for a block of items. Used to *suggest* whether
  # the regex-detected unidimensional block might actually be multi (or
  # vice-versa) by counting Kaiser eigenvalues > 1 over polychoric corr.
  .quick_dimensionality <- function(data, items) {
    items <- items[items %in% colnames(data)]
    if (length(items) < 3L) return(list(k_kaiser = NA_integer_, ok = FALSE))
    X <- data[, items, drop = FALSE]
    R <- tryCatch(psych::polychoric(X)$rho,
                  error = function(e) cor(X, use = "pairwise.complete.obs"))
    ev <- eigen(R, symmetric = TRUE, only.values = TRUE)$values
    list(k_kaiser = sum(ev > 1, na.rm = TRUE),
         eigen_top3 = head(round(ev, 3), 3),
         ok = TRUE)
  }

  # Build score columns from an instrument definition. Returns a list:
  #   list(scores = named-data.frame, columns_meta = list(label, type))
  .build_instrument_scores <- function(data, instr) {
    scores <- data.frame(row.names = seq_len(nrow(data)))
    meta   <- list()
    add_col <- function(name, items, type) {
      its <- items[items %in% colnames(data)]
      if (length(its) == 0L) return(invisible())
      scores[[name]] <<- rowSums(data[, its, drop = FALSE], na.rm = TRUE)
      meta[[length(meta) + 1L]] <<- list(name = name, type = type,
                                          n_items = length(its))
    }
    if (isTRUE(instr$multi)) {
      for (sub in instr$subscales) {
        col_name <- sprintf("%s_%s", instr$label, sub$name)
        add_col(col_name, sub$items, "subscale")
      }
      if (isTRUE(instr$include_total)) {
        col_name <- sprintf("%s_TOTAL", instr$label)
        all_its <- unlist(lapply(instr$subscales, `[[`, "items"))
        add_col(col_name, all_its, "total")
      }
    } else {
      add_col(sprintf("%s_TOTAL", instr$label), instr$items, "total")
    }
    list(scores = scores, meta = meta)
  }

  # Compute the convergent/discriminant validity panel for ALL instruments.
  # Each row: factor (or grand total of principal) vs one convergent column.
  .compute_convergent_validity <- function(rv, instruments) {
    if (length(instruments) == 0L || is.null(rv$boost_obj))
      return(list(table = NULL, by_instrument = list(), skipped = TRUE))

    df <- rv$data
    ld <- rv$boost_obj$final_structure
    cn <- colnames(ld); item_col <- intersect(c("Items","Item"), cn)[1]
    fact_cols <- cn[grepl("^[fF][0-9]+$", cn)]

    # Factor scores (sum of items with dominant loading).
    L <- as.matrix(ld[, fact_cols, drop = FALSE]); L[is.na(L)] <- 0
    dom <- max.col(abs(L), ties.method = "first")
    f_scores <- as.data.frame(matrix(NA_real_, nrow = nrow(df),
                                       ncol = length(fact_cols)))
    colnames(f_scores) <- fact_cols
    retained <- character(0)
    for (k in seq_along(fact_cols)) {
      its <- as.character(ld[[item_col]][dom == k])
      its <- its[its %in% colnames(df)]
      retained <- c(retained, its)
      if (length(its) >= 1L)
        f_scores[[k]] <- rowSums(df[, its, drop = FALSE], na.rm = TRUE)
    }
    # Grand total of principal instrument (all retained items).
    f_scores$TOTAL_PRINCIPAL <- rowSums(df[, retained, drop = FALSE],
                                          na.rm = TRUE)

    rows <- list()
    by_instr <- list()
    for (instr in instruments) {
      bld <- .build_instrument_scores(df, instr)
      if (length(bld$meta) == 0L) next
      conv_cols <- names(bld$scores)
      for (k in c(fact_cols, "TOTAL_PRINCIPAL")) {
        for (cc in conv_cols) {
          ct <- suppressWarnings(stats::cor.test(f_scores[[k]],
                                                   bld$scores[[cc]]))
          rmag <- .cohen_magnitude(ct$estimate)
          verd <- .verdict_convergence(ct$estimate, ct$p.value,
                                         instr$expected %||% "convergent")
          rows[[length(rows) + 1L]] <- data.frame(
            instrument   = instr$label,
            expected     = instr$expected %||% "convergent",
            principal    = k,
            convergent   = cc,
            r            = round(unname(ct$estimate), 3),
            ci_low       = round(unname(ct$conf.int[1]), 3),
            ci_up        = round(unname(ct$conf.int[2]), 3),
            p            = signif(unname(ct$p.value), 3),
            magnitude    = rmag,
            verdict      = verd,
            stringsAsFactors = FALSE)
        }
      }
      by_instr[[instr$label]] <- list(
        scores_meta = bld$meta,
        n_pairs = length(conv_cols) * (length(fact_cols) + 1L))
    }
    table_df <- if (length(rows) > 0L) do.call(rbind, rows) else NULL
    if (!is.null(table_df)) rownames(table_df) <- NULL
    list(table = table_df, by_instrument = by_instr, skipped = FALSE,
         factor_scores = f_scores)
  }

  # ===================================================================
  # Export helpers: full TXT log + manuscript DOCX (Análisis de datos +
  # Resultados, APA-7 style). Pattern inspired by EasyValidation's
  # export_session_to_text() and generate_manuscript_docx(), adapted to
  # the OptimalFactor wizard (5 phases, EFA-only).
  # ===================================================================

  .export_wizard_to_text <- function(rv, input) {
    hr   <- strrep("=", 70)
    sub  <- strrep("-", 70)
    thin <- strrep(".", 70)
    push <- function(out, ...) c(out, ...)
    out <- character(0)
    out <- push(out,
      hr,
      sprintf("OptimalFactor Wizard %s — Reporte de sesión",
              tryCatch(as.character(utils::packageVersion("OptimalFactor")),
                        error = function(e) "?")),
      hr,
      sprintf("Generado : %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
      sprintf("Muestra  : %d filas × %d ítems iniciales",
              nrow(rv$data %||% data.frame()), length(rv$items)),
      sprintf("Ítems    : %s", paste(rv$items, collapse = ", ")),
      "")

    # ---- Configuración técnica ----
    out <- push(out, sub, "CONFIGURACIÓN TÉCNICA", sub,
      sprintf("estimator           : %s", input$estimator %||% "WLSMV"),
      sprintf("rotation            : %s", input$rotation %||% "oblimin"),
      sprintf("loading_threshold   : %.3f",
              as.numeric(input$loading_threshold %||% 0.30)),
      sprintf("min_items_per_factor: %s",
              as.character(input$min_items_per_factor %||% 3L)),
      sprintf("n_factors usado     : %s",
              as.character(rv$results$phase2$nf_used %||% "n/a")),
      sprintf("uso de IA           : %s",
              if (isTRUE(input$use_ai) &&
                  nzchar(input$openai_api_key %||% "")) "sí" else "no"),
      sprintf("modelo IA           : %s",
              if (isTRUE(input$use_ai)) input$gpt_model %||% "gpt-4.1" else "n/a"),
      "")

    # ---- Fase 2: diagnóstico paralelo ----
    p2 <- rv$results$phase2
    if (!is.null(p2)) {
      out <- push(out, sub, "FASE 2 — DIAGNÓSTICO PARALELO", sub,
        sprintf("Consenso multi-método (k recomendado por método):"))
      if (!is.null(p2$diag$consensus)) {
        cdf <- p2$diag$consensus
        for (i in seq_len(nrow(cdf))) {
          out <- push(out, sprintf("  %-30s -> k = %d   %s",
                                     cdf$method[i], cdf$suggested_k[i],
                                     cdf$details[i] %||% ""))
        }
      }
      out <- push(out,
        sprintf("Recomendación consenso : k = %s",
                as.character(p2$diag$recommendation %||% "n/a")),
        sprintf("Decisión final usuario : k = %s",
                as.character(p2$nf_used %||% "n/a")),
        "")
    }

    # ---- Fase 3: EFA boosting ----
    b <- rv$boost_obj
    if (!is.null(b)) {
      out <- push(out, sub, "FASE 3 — EFA BOOSTING", sub,
        sprintf("Ítems iniciales       : %d", length(rv$items)),
        sprintf("Ítems finales         : %d", nrow(b$final_structure)),
        sprintf("Ítems eliminados      : %s",
                if (length(b$removed_items) > 0L)
                  paste(b$removed_items, collapse = ", ")
                else "(ninguno)"),
        sprintf("stop_reason           : %s", b$stop_reason %||% "?"),
        "")

      # Loadings finales
      if (!is.null(b$final_structure)) {
        out <- push(out, "Estructura factorial final (cargas redondeadas a .01):")
        ld <- b$final_structure
        cn <- colnames(ld)
        item_col <- intersect(c("Items","Item"), cn)[1]
        fact_cols <- cn[grepl("^[fF][0-9]+$", cn)]
        if (length(fact_cols) > 0L && !is.na(item_col)) {
          hdr <- paste(sprintf("%10s", c("Item", fact_cols)), collapse = " ")
          out <- push(out, paste0("  ", hdr))
          for (i in seq_len(nrow(ld))) {
            nums <- vapply(fact_cols, function(c) {
              v <- ld[[c]][i]
              if (is.numeric(v) && !is.na(v) && abs(v) > 0)
                sprintf("%10.2f", v) else sprintf("%10s", ".")
            }, character(1))
            out <- push(out, sprintf("  %10s %s",
                                       ld[[item_col]][i],
                                       paste(nums, collapse = " ")))
          }
        }
        out <- push(out, "")
      }

      # Bondades por iteración
      if (!is.null(b$bondades_original)) {
        out <- push(out, "Índices de ajuste por iteración:",
          paste(utils::capture.output(print(b$bondades_original, row.names = FALSE)),
                collapse = "\n"), "")
      }

      # Análisis conceptual (si la IA estuvo activa)
      if (!is.null(b$conceptual_analysis)) {
        cap <- tryCatch(
          paste(utils::capture.output(
            OptimalFactor::print_conceptual_analysis(b,
              width = 100, show_stats = TRUE)),
            collapse = "\n"),
          error = function(e) "")
        if (nzchar(cap)) {
          out <- push(out, "Análisis conceptual de ítems eliminados (IA):",
            cap, "")
        }
      }
    }

    # ---- Verbose boost (la bitácora del Trace) ----
    if (!is.null(rv$boost_console) && nzchar(rv$boost_console)) {
      out <- push(out, sub,
        "BITÁCORA verbose efa_boosting() (Trace tab)", sub,
        rv$boost_console, "")
    }

    # ---- Fase 4: fiabilidad ----
    p4 <- rv$results$phase4
    if (!is.null(p4)) {
      out <- push(out, sub, "FASE 4 — FIABILIDAD", sub)
      if (length(p4$omega) > 0L) {
        out <- push(out, "Omega categórico (compRelSEM, ord.scale = TRUE):")
        for (nm in names(p4$omega))
          out <- push(out,
            sprintf("  %-10s  omega = %.3f", nm, as.numeric(p4$omega[[nm]])))
      }
      if (length(p4$alpha) > 0L) {
        out <- push(out, "Alpha de Cronbach (psych::alpha):")
        al <- as.numeric(p4$alpha)
        nms <- names(p4$omega) %||% paste0("f", seq_along(al))
        for (i in seq_along(al))
          out <- push(out, sprintf("  %-10s  alpha = %.3f",
                                     nms[i], al[i]))
      }
      if (!is.null(p4$fit_indices)) {
        fm <- p4$fit_indices
        out <- push(out, "Ajuste del CFA EFA-derived (lavaan, WLSMV):",
          sprintf("  CFI = %.3f, TLI = %.3f, RMSEA = %.3f, SRMR = %.3f",
                  fm[["cfi.scaled"]] %||% NA, fm[["tli.scaled"]] %||% NA,
                  fm[["rmsea.scaled"]] %||% NA, fm[["srmr"]] %||% NA))
      }
      out <- push(out, "")
    }

    # ---- Fase 5: validez externa ----
    p5 <- rv$results$phase5
    if (!is.null(p5)) {
      out <- push(out, sub, "FASE 5 — VALIDEZ EXTERNA (variables sueltas)", sub)
      if (isTRUE(p5$skipped) || is.null(p5$corr_df)) {
        out <- push(out, "(omitida — no se seleccionaron variables externas)")
      } else {
        cdf <- p5$corr_df
        for (i in seq_len(nrow(cdf))) {
          p_str <- if (cdf$p[i] < 0.001) "< .001"
                   else sprintf("%.3f", cdf$p[i])
          out <- push(out,
            sprintf("  %-10s × %-12s  r = %6.3f  IC 95%% [%6.3f, %6.3f]  p = %s",
                    cdf$factor[i], cdf$external[i],
                    cdf$r[i], cdf$ci_low[i], cdf$ci_up[i], p_str))
        }
      }
      out <- push(out, "")
    }

    # ---- Fase 5b: validez convergente / discriminante ----
    p5b <- rv$results$phase5b
    if (!is.null(p5b) && !isTRUE(p5b$skipped) && !is.null(p5b$table)) {
      out <- push(out, sub,
        "FASE 5b — VALIDEZ CONVERGENTE / DISCRIMINANTE", sub)
      # Instruments meta
      for (instr in rv$convergent_instruments) {
        kind <- if (isTRUE(instr$multi))
          sprintf("multidim. (%d subescalas: %s)",
                   length(instr$subscales),
                   paste(vapply(instr$subscales, `[[`, character(1), "name"),
                         collapse = ", "))
        else "unidim."
        out <- push(out,
          sprintf("  • %-12s  %-50s  esperado: %s%s",
                  instr$label, kind, instr$expected %||% "convergent",
                  if (isTRUE(instr$include_total)) "  [+ total global]" else ""))
      }
      out <- push(out, "",
        "Correlaciones (principal × convergente):")
      tbl <- p5b$table
      for (i in seq_len(nrow(tbl))) {
        p_str <- if (tbl$p[i] < 0.001) "< .001"
                 else sprintf("%.3f", tbl$p[i])
        out <- push(out,
          sprintf("  [%-8s] %-16s × %-22s  r = %6.3f  IC95%% [%6.3f, %6.3f]  p = %s  · %s · %s",
                  tbl$instrument[i], tbl$principal[i], tbl$convergent[i],
                  tbl$r[i], tbl$ci_low[i], tbl$ci_up[i], p_str,
                  tbl$magnitude[i], tbl$verdict[i]))
      }
      out <- push(out, "")
    }

    out <- push(out, hr,
      sprintf("Fin del reporte — %s",
              format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
      hr)
    paste(out, collapse = "\n")
  }

  # -------------------------------------------------------------------
  # Manuscript DOCX
  # -------------------------------------------------------------------

  .docx_w_fmt_num <- function(df, decimals = 2L) {
    df <- as.data.frame(df, stringsAsFactors = FALSE)
    p_cols <- c("p", "pvalue", "p.value", "P")
    for (cn in colnames(df)) {
      v <- df[[cn]]
      if (!is.numeric(v)) next
      if (cn %in% p_cols) {
        df[[cn]] <- vapply(v, function(x) {
          if (is.na(x)) "" else if (x < 0.001) "< .001"
          else sprintf("%.3f", x)
        }, character(1))
      } else {
        df[[cn]] <- ifelse(is.na(v), "",
                            formatC(v, digits = decimals, format = "f"))
      }
    }
    df
  }

  .docx_w_add_table <- function(doc, df, font_family = "Calibri",
                                  font_size = 11L) {
    if (requireNamespace("flextable", quietly = TRUE)) {
      ft <- flextable::flextable(df)
      ft <- flextable::theme_apa(ft)
      ft <- flextable::font(ft, fontname = font_family, part = "all")
      ft <- flextable::fontsize(ft, size = font_size, part = "all")
      ft <- flextable::align(ft, align = "center", part = "all")
      if (ncol(df) >= 1L)
        ft <- flextable::align(ft, j = 1L, align = "left", part = "all")
      ft <- flextable::autofit(ft)
      doc <- flextable::body_add_flextable(doc, ft, align = "center")
    } else {
      doc <- officer::body_add_table(doc, df,
                                      first_row = TRUE, first_column = FALSE)
    }
    officer::body_add_par(doc, "")
  }

  .docx_w_table_loadings <- function(doc, rv, decimals) {
    b <- rv$boost_obj; if (is.null(b)) return(doc)
    ld <- b$final_structure
    cn <- colnames(ld)
    item_col <- intersect(c("Items","Item"), cn)[1]
    fact_cols <- cn[grepl("^[fF][0-9]+$", cn)]
    if (is.na(item_col) || length(fact_cols) == 0L) return(doc)
    df <- ld[, c(item_col, fact_cols), drop = FALSE]
    .docx_w_add_table(doc, .docx_w_fmt_num(df, decimals))
  }

  .docx_w_table_fit <- function(doc, rv, decimals) {
    p4 <- rv$results$phase4
    fm <- p4$fit_indices
    if (is.null(fm)) return(doc)
    df <- data.frame(
      Modelo  = "EFA-derived CFA",
      Items   = nrow(rv$boost_obj$final_structure %||% data.frame()),
      CFI     = unname(fm[["cfi.scaled"]] %||% NA_real_),
      TLI     = unname(fm[["tli.scaled"]] %||% NA_real_),
      RMSEA   = unname(fm[["rmsea.scaled"]] %||% NA_real_),
      SRMR    = unname(fm[["srmr"]] %||% NA_real_),
      stringsAsFactors = FALSE)
    .docx_w_add_table(doc, .docx_w_fmt_num(df, decimals))
  }

  .docx_w_table_omega <- function(doc, rv, decimals) {
    p4 <- rv$results$phase4; if (is.null(p4)) return(doc)
    om <- as.numeric(p4$omega %||% numeric(0))
    al <- as.numeric(p4$alpha %||% numeric(0))
    if (length(om) == 0L) return(doc)
    df <- data.frame(
      Factor = names(p4$omega) %||% paste0("f", seq_along(om)),
      Omega  = om,
      Alpha  = if (length(al) >= length(om)) al[seq_along(om)] else rep(NA_real_, length(om)),
      stringsAsFactors = FALSE)
    .docx_w_add_table(doc, .docx_w_fmt_num(df, decimals))
  }

  .docx_w_table_external <- function(doc, rv, decimals) {
    p5 <- rv$results$phase5
    if (is.null(p5) || isTRUE(p5$skipped) || is.null(p5$corr_df)) return(doc)
    df <- p5$corr_df[, c("external","factor","r","ci_low","ci_up","p"),
                      drop = FALSE]
    colnames(df) <- c("Variable externa","Factor","r","IC 95% (inf)",
                       "IC 95% (sup)","p")
    .docx_w_add_table(doc, .docx_w_fmt_num(df, decimals))
  }

  .docx_w_table_convergent <- function(doc, rv, decimals) {
    p5b <- rv$results$phase5b
    if (is.null(p5b) || isTRUE(p5b$skipped) || is.null(p5b$table))
      return(doc)
    df <- p5b$table[, c("instrument","principal","convergent","r",
                          "ci_low","ci_up","p","magnitude","verdict"),
                      drop = FALSE]
    colnames(df) <- c("Instrumento","Principal","Convergente","r",
                       "IC inf","IC sup","p","Magnitud","Veredicto")
    .docx_w_add_table(doc, .docx_w_fmt_num(df, decimals))
  }

  .md_inline_to_fpar_w <- function(txt, font_family = "Calibri", font_size = 11L) {
    fp_n <- officer::fp_text(font.size = font_size, font.family = font_family)
    fp_b <- officer::fp_text(font.size = font_size, font.family = font_family,
                              bold = TRUE)
    fp_i <- officer::fp_text(font.size = font_size, font.family = font_family,
                              italic = TRUE)
    parts <- list(); remaining <- txt
    while (nzchar(remaining)) {
      bold_pos   <- regexpr("\\*\\*([^*]+?)\\*\\*", remaining)
      italic_pos <- regexpr("(?<!\\*)\\*([^*]+?)\\*(?!\\*)", remaining, perl = TRUE)
      pos <- c(bold = as.integer(bold_pos), italic = as.integer(italic_pos))
      pos[pos == -1] <- .Machine$integer.max
      if (all(pos == .Machine$integer.max)) {
        if (nzchar(remaining))
          parts[[length(parts) + 1L]] <- officer::ftext(remaining, fp_n)
        break
      }
      type <- names(which.min(pos)); p <- min(pos)
      if (p > 1L)
        parts[[length(parts) + 1L]] <- officer::ftext(
          substr(remaining, 1L, p - 1L), fp_n)
      if (type == "bold") {
        m <- regmatches(remaining, regexpr("\\*\\*([^*]+?)\\*\\*", remaining))
        inner <- gsub("^\\*\\*|\\*\\*$", "", m)
        parts[[length(parts) + 1L]] <- officer::ftext(inner, fp_b)
        remaining <- substr(remaining, p + nchar(m), nchar(remaining))
      } else {
        m <- regmatches(remaining,
                         regexpr("(?<!\\*)\\*([^*]+?)\\*(?!\\*)", remaining,
                                  perl = TRUE))
        inner <- gsub("^\\*|\\*$", "", m)
        parts[[length(parts) + 1L]] <- officer::ftext(inner, fp_i)
        remaining <- substr(remaining, p + nchar(m), nchar(remaining))
      }
    }
    do.call(officer::fpar, parts)
  }

  .docx_w_emit_line <- function(doc, ln, rv, decimals) {
    raw <- trimws(sub("\r$", "", ln), which = "right")
    if (grepl("\\{\\{TABLE_LOADINGS\\}\\}", raw))
      return(.docx_w_table_loadings(doc, rv, decimals))
    if (grepl("\\{\\{TABLE_FIT\\}\\}", raw))
      return(.docx_w_table_fit(doc, rv, decimals))
    if (grepl("\\{\\{TABLE_OMEGA\\}\\}", raw))
      return(.docx_w_table_omega(doc, rv, decimals))
    if (grepl("\\{\\{TABLE_EXTERNAL\\}\\}", raw))
      return(.docx_w_table_external(doc, rv, decimals))
    if (grepl("\\{\\{TABLE_CONVERGENT\\}\\}", raw))
      return(.docx_w_table_convergent(doc, rv, decimals))
    if (grepl("^## ",  raw)) return(officer::body_add_par(doc,
                                       sub("^##\\s+", "", raw),
                                       style = "heading 2"))
    if (grepl("^### ", raw)) return(officer::body_add_par(doc,
                                       sub("^###\\s+", "", raw),
                                       style = "heading 3"))
    if (grepl("^\\*\\*[^*][^*]*\\*\\*\\s*$", raw)) {
      inner <- sub("^\\*\\*", "", sub("\\*\\*\\s*$", "", raw))
      fp_b <- officer::fp_text(font.size = 11L, font.family = "Calibri",
                                bold = TRUE)
      return(officer::body_add_fpar(doc,
                officer::fpar(officer::ftext(inner, fp_b)), style = "Normal"))
    }
    if (!nzchar(raw)) return(officer::body_add_par(doc, ""))
    if (grepl("^\\s*[-*•]\\s+", raw)) {
      txt <- sub("^\\s*[-*•]\\s+", "", raw)
      return(officer::body_add_fpar(doc,
               .md_inline_to_fpar_w(txt), style = "List Bullet"))
    }
    officer::body_add_fpar(doc, .md_inline_to_fpar_w(raw), style = "Normal")
  }

  .generate_wizard_docx <- function(rv, input, file, api_key,
                                       decimals = 2L) {
    if (!requireNamespace("officer", quietly = TRUE))
      stop("Instala 'officer' primero: install.packages('officer').")
    if (is.null(rv$boost_obj))
      stop("Aún no hay boosting completado. Avanza el wizard hasta Fase 3.")

    audit <- .export_wizard_to_text(rv, input)
    has_convergent <- !is.null(rv$results$phase5b) &&
                      !isTRUE(rv$results$phase5b$skipped) &&
                      !is.null(rv$results$phase5b$table)

    system_prompt <- paste0(
      "Eres un psicómetra senior redactando, en español APA-7, las ",
      "secciones 'Análisis de datos' (dentro del Método) y 'Resultados' ",
      "de un manuscrito instrumental sobre VALIDACIÓN PSICOMÉTRICA con ",
      "AFE y boosting de ítems. SOLO usas números del audit trail. ",
      "NUNCA inventas autores, fechas, n ni estadísticos.\n\n",
      "FORMATO ESTRICTO:\n",
      "  - Markdown plano. Sin code fences. Sin JSON.\n",
      "  - Encabezados en su propia línea: '## 2. Método' y '## 3. ",
      "    Resultados' (h2); '### 2.4. Análisis de datos', ",
      "    '### 3.1. Análisis descriptivo de los ítems', ",
      "    '### 3.2. Análisis paralelo y diagnóstico factorial', ",
      "    '### 3.3. EFA boosting', ",
      "    '### 3.4. Fiabilidad', ",
      "    '### 3.5. Evidencia de validez con variables externas'",
      if (has_convergent)
        ", '### 3.6. Evidencia convergente y discriminante' (h3).\n"
      else ".\n",
      "  - Párrafos densos (4-7 oraciones). NO bullets gratis.\n",
      "  - Cita autores inline: (Rosseel, 2012), (Hu & Bentler, 1999), ",
      "    (McDonald, 1999), (Horn, 1965), (Velicer, 1976), (Garrido et ",
      "    al., 2013), (Wu & Estabrook, 2016), (Li, 2016), ",
      "    (Ventura-León, 2023) para OptimalFactor, (R Core Team, 2024)",
      if (has_convergent) ", (Cohen, 1988) para magnitudes de r" else "",
      ".\n",
      "  - Usa cursivas APA con un solo asterisco: *p*, *M*, *SD*, *r*, ",
      "    *N*. Negrita con doble asterisco SOLO para 'Tabla N' y ",
      "    'Figura N'.\n",
      "  - Placeholders permitidos (ponlos en su propia línea, NO inventes ",
      "    otros): {{TABLE_LOADINGS}}, {{TABLE_FIT}}, {{TABLE_OMEGA}}, ",
      "    {{TABLE_EXTERNAL}}",
      if (has_convergent) ", {{TABLE_CONVERGENT}}" else "",
      ". Formato APA:\n",
      "      **Tabla N**\n",
      "      *Título en cursivas*\n",
      "      {{TABLE_xxx}}\n",
      "      *Nota.* explicación.\n",
      "  - NO escribas sección de Referencias.\n",
      "Responde en español.")

    subs_results <- if (has_convergent) "3.1 a 3.6" else "3.1 a 3.5"
    extra_anal <- if (has_convergent) paste0(
      ", (h) evidencia convergente/discriminante mediante correlaciones ",
      "Pearson entre puntajes totales del instrumento principal (por factor ",
      "y total general) y los totales/subtotales de cada instrumento de ",
      "comparación. Cuando una medida de comparación sea multidimensional, ",
      "explica que se conservó su estructura interna sumando ítems por ",
      "subescala antes de correlacionar, y reporta magnitudes según ",
      "Cohen (1988): r>=.50 grande, .30-.49 moderada, .10-.29 pequeña") else ""
    extra_results <- if (has_convergent) paste0(
      "Para la subsección 3.6 describe primero los instrumentos de ",
      "comparación incluidos (con su dimensionalidad), luego reporta los ",
      "coeficientes de correlación con r, IC95% y p, y emite un juicio ",
      "explícito de convergencia (o discriminación, cuando así se haya ",
      "declarado) por par. Embebe la **Tabla N** con {{TABLE_CONVERGENT}}. ")
      else ""
    user_prompt <- paste0(
      "Redacta ÚNICAMENTE las secciones '## 2. Método' (solo con la ",
      "subsección '### 2.4. Análisis de datos') y '## 3. Resultados' ",
      "(con subsecciones ", subs_results, "). NO redactes Introducción, ",
      "Discusión ni Referencias. La sección 'Análisis de datos' debe ",
      "tener 5-7 párrafos cubriendo: (a) software y paquetes (R Core ",
      "Team, 2024; OptimalFactor; psych; semTools; lavaan), (b) ",
      "preprocesamiento y descriptivos, (c) decisión sobre número de ",
      "factores con consenso multi-método (Kaiser, análisis paralelo, ",
      "MAP, BIC), (d) AFE con estimador y rotación reales del audit, ",
      "(e) EFA boosting con umbrales reales, (f) fiabilidad por ",
      "omega categórico (McDonald, 1999) y alpha, (g) validez externa ",
      "con correlaciones Pearson si aplica", extra_anal, ".\n\n",
      "La sección Resultados debe reportar los hallazgos numéricos ",
      "REALES del audit trail y embeber las tablas con los placeholders. ",
      extra_results,
      "Empieza directamente con `## 2. Método`.\n\n",
      "Audit trail (verdad fuente; cita estos números exactos):\n```\n",
      audit, "\n```")

    # Call OpenAI
    if (!requireNamespace("httr", quietly = TRUE) ||
        !requireNamespace("jsonlite", quietly = TRUE))
      stop("httr + jsonlite son requeridos para llamar a OpenAI.")
    r <- httr::POST("https://api.openai.com/v1/chat/completions",
      httr::add_headers(Authorization = paste("Bearer", api_key),
                          `Content-Type` = "application/json"),
      httr::timeout(180),
      body = jsonlite::toJSON(list(
        model = input$gpt_model %||% "gpt-4.1",
        messages = list(
          list(role = "system", content = system_prompt),
          list(role = "user", content = user_prompt)),
        temperature = 0.3), auto_unbox = TRUE))
    if (httr::status_code(r) >= 400)
      stop("OpenAI HTTP ", httr::status_code(r), ": ",
           substr(httr::content(r, as = "text", encoding = "UTF-8"), 1, 400))
    narrative <- httr::content(r, as = "parsed")$choices[[1]]$message$content
    narrative <- gsub("^\\s*```(?:markdown)?\\s*|\\s*```\\s*$", "",
                       trimws(narrative))

    # Build the docx
    doc <- officer::read_docx()
    centred <- officer::fp_par(text.align = "center")
    fp_title <- officer::fp_text(font.size = 16L, bold = TRUE,
                                   font.family = "Calibri")
    title_txt <- sprintf("Validación psicométrica vía EFA boosting — %s",
                          input$name_items %||% "escala bajo análisis")
    doc <- officer::body_add_fpar(doc,
             officer::fpar(officer::ftext(title_txt, fp_title),
                            fp_p = centred))
    doc <- officer::body_add_par(doc, "")
    doc <- officer::body_add_par(doc,
             sprintf("Generado automáticamente con OptimalFactor wizard el %s.",
                     format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
             style = "Normal")
    doc <- officer::body_add_par(doc, "")

    lines <- strsplit(narrative, "\n", fixed = TRUE)[[1]]
    for (ln in lines) {
      doc <- .docx_w_emit_line(doc, ln, rv = rv, decimals = decimals)
    }
    print(doc, target = file)
    invisible(file)
  }

  # Download handlers
  output$dl_full_txt <- downloadHandler(
    filename = function()
      sprintf("OptimalFactor_session_%s.txt",
              format(Sys.time(), "%Y%m%d_%H%M%S")),
    content = function(file) {
      if (!isTRUE(rv$started) || is.null(rv$boost_obj)) {
        writeLines(paste(
          "El wizard aún no se ha ejecutado completamente.",
          "Carga datos, inicia el wizard y avanza al menos hasta la Fase 3",
          "(EFA boosting) para generar el log.",
          sep = "\n"), file)
        return()
      }
      writeLines(.export_wizard_to_text(rv, input), file)
    }
  )

  output$dl_manuscript_docx <- downloadHandler(
    filename = function()
      sprintf("OptimalFactor_manuscript_%s.docx",
              format(Sys.time(), "%Y%m%d_%H%M%S")),
    content = function(file) {
      if (is.null(rv$boost_obj)) {
        showNotification(
          "Aún no hay boosting completado. Avanza el wizard hasta la Fase 3.",
          type = "warning", duration = 6); return()
      }
      api_key <- isolate(input$openai_api_key %||% "")
      if (!nzchar(api_key)) {
        showNotification(
          "Se requiere una API key de OpenAI para redactar el manuscrito.",
          type = "error", duration = 8); return()
      }
      pr <- shiny::Progress$new(session); on.exit(pr$close(), add = TRUE)
      pr$set(message = "Redactando manuscrito con IA...",
             detail  = "Esto puede tardar 30-60 segundos.",
             value   = 0.3)
      tryCatch(
        .generate_wizard_docx(rv = rv, input = input, file = file,
                                api_key = api_key, decimals = 2L),
        error = function(e) {
          showNotification(sprintf("Error generando manuscrito: %s",
                                     conditionMessage(e)),
                            type = "error", duration = 14)
          stop(e)
        })
    }
  )

  # ===================================================================
  # Convergent instruments — CRUD UI
  # ===================================================================

  .conv_new_id <- function() {
    rv$convergent_uid <- rv$convergent_uid + 1L
    sprintf("conv_%d", rv$convergent_uid)
  }

  # Card list shown in the Phase 5 step. Each card summarises a defined
  # instrument and offers Edit / Remove.
  output$conv_cards_ui <- renderUI({
    items <- rv$convergent_instruments
    if (length(items) == 0L)
      return(div(class = "text-muted", style = "padding:8px 4px; font-size:12px;",
        "Sin instrumentos. Usa 'Detectar' o '+' para añadirlos."))
    tagList(lapply(items, function(it) {
      lab_type <- if (isTRUE(it$multi))
        sprintf("multidim. (%d subescalas)", length(it$subscales))
      else "unidim."
      exp_lbl <- if (identical(it$expected, "discriminant"))
        tags$span(class = "dl-tag", style = "background:#F59E0B;", "discriminante")
      else
        tags$span(class = "dl-tag", style = "background:#18BC9C;", "convergente")
      div(class = "step-card",
        style = "padding:10px 12px; margin-bottom:8px; background:#F9FAFB;",
        div(style = "display:flex; justify-content:space-between; align-items:center;",
          div(
            tags$strong(it$label), " ",
            tags$small(class = "text-muted", lab_type), " ",
            exp_lbl),
          div(
            actionLink(paste0("conv_edit_", it$id), icon("pen-to-square"),
                        title = "Editar",
                        style = "color:#3498DB; margin-right:10px;"),
            actionLink(paste0("conv_del_", it$id), icon("trash"),
                        title = "Quitar",
                        style = "color:#DC2626;")
          )),
        div(class = "text-muted", style = "font-size:11px; margin-top:4px;",
          sprintf("%d ítem(s) total: %s",
                  length(unique(if (isTRUE(it$multi))
                                  unlist(lapply(it$subscales, `[[`, "items"))
                                else it$items)),
                  paste(utils::head(if (isTRUE(it$multi))
                                       unlist(lapply(it$subscales, `[[`, "items"))
                                    else it$items, 6),
                        collapse = ", "))))
    }))
  })

  # Per-card observers (edit / remove). Bind dynamically.
  observe({
    for (it in rv$convergent_instruments) {
      local({
        cur_id <- it$id
        observeEvent(input[[paste0("conv_del_", cur_id)]], {
          rv$convergent_instruments <- Filter(
            function(x) !identical(x$id, cur_id),
            rv$convergent_instruments)
          showNotification("Instrumento eliminado.", type = "message",
                            duration = 3)
        }, ignoreInit = TRUE, once = FALSE)
        observeEvent(input[[paste0("conv_edit_", cur_id)]], {
          .open_conv_modal(cur_id)
        }, ignoreInit = TRUE, once = FALSE)
      })
    }
  })

  # Auto-detect button → open modal with checkboxes.
  observeEvent(input$conv_detect, {
    if (is.null(rv$data)) {
      showNotification("Carga datos primero.", type = "warning"); return()
    }
    excl <- unique(c(rv$items, isolate(input$external_vars) %||% character(0)))
    cands <- .detect_convergent_groups(rv$data, exclude_cols = excl)
    if (length(cands) == 0L) {
      showNotification(
        "No se detectaron instrumentos candidatos en las columnas restantes.",
        type = "message", duration = 5); return()
    }
    # Build the modal UI: one row per candidate with a checkbox.
    rows <- lapply(seq_along(cands), function(i) {
      c_i <- cands[[i]]
      kk <- tryCatch(.quick_dimensionality(rv$data, c_i$items),
                      error = function(e) list(k_kaiser = NA, ok = FALSE))
      hint <- if (isTRUE(kk$ok))
        sprintf(" · análisis paralelo sugiere k=%d (eigen top: %s)",
                 kk$k_kaiser,
                 paste(kk$eigen_top3, collapse = ", "))
      else ""
      sub_info <- if (isTRUE(c_i$multi))
        sprintf("multidimensional (%d subescalas: %s)",
                 length(c_i$subscales),
                 paste(vapply(c_i$subscales, `[[`, character(1), "name"),
                       collapse = ", "))
      else "unidimensional"
      div(style = "padding:8px 0; border-bottom:1px solid #E5E7EB;",
        checkboxInput(sprintf("conv_pick_%d", i),
          tags$strong(sprintf("%s — %d ítems · %s%s",
                               c_i$label, c_i$n_items, sub_info, hint)),
          value = TRUE),
        radioButtons(sprintf("conv_exp_%d", i), NULL,
          choices = c("Convergente esperado" = "convergent",
                       "Discriminante esperado" = "discriminant"),
          selected = "convergent", inline = TRUE),
        if (isTRUE(c_i$multi))
          checkboxInput(sprintf("conv_total_%d", i),
            "También calcular el total global del instrumento",
            value = FALSE)
        else NULL
      )
    })
    showModal(modalDialog(
      title = "Instrumentos detectados",
      size = "l", easyClose = TRUE,
      tags$small(class = "text-muted",
        "El wizard agrupó las columnas no usadas como ítems del principal y detectó posibles subescalas por sub-prefijo (ej. DERS_AC, DERS_OB). Verifica antes de añadir."),
      tagList(rows),
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("conv_detect_confirm", "Añadir seleccionados",
          class = "btn-go", icon = icon("check"))
      )))
    # Stash candidates so the confirm handler can read them
    rv$.conv_candidates <- cands
  })

  observeEvent(input$conv_detect_confirm, {
    cands <- rv$.conv_candidates %||% list()
    n_added <- 0L
    for (i in seq_along(cands)) {
      if (!isTRUE(input[[sprintf("conv_pick_%d", i)]])) next
      c_i <- cands[[i]]
      expected <- input[[sprintf("conv_exp_%d", i)]] %||% "convergent"
      include_total <- isTRUE(input[[sprintf("conv_total_%d", i)]])
      new_instr <- list(
        id        = .conv_new_id(),
        label     = c_i$label,
        multi     = c_i$multi,
        items     = if (isTRUE(c_i$multi)) character(0) else c_i$items,
        subscales = c_i$subscales,
        expected  = expected,
        include_total = include_total)
      rv$convergent_instruments[[length(rv$convergent_instruments) + 1L]] <-
        new_instr
      n_added <- n_added + 1L
    }
    rv$.conv_candidates <- NULL
    removeModal()
    showNotification(sprintf("%d instrumento(s) añadido(s).", n_added),
                      type = "message", duration = 4)
  })

  # Manual add button → open empty editor modal.
  observeEvent(input$conv_add, {
    .open_conv_modal(NULL)
  })

  # Open the editor modal. If id is non-NULL, prefill from existing instr.
  .open_conv_modal <- function(id) {
    instr <- if (!is.null(id))
      Filter(function(x) identical(x$id, id), rv$convergent_instruments)[[1]]
    else NULL
    excl <- unique(c(rv$items, isolate(input$external_vars) %||% character(0)))
    pool <- setdiff(colnames(rv$data), excl)
    pool <- pool[vapply(pool,
      function(v) is.numeric(rv$data[[v]]), logical(1))]

    cur_items <- if (!is.null(instr) && !isTRUE(instr$multi)) instr$items
                  else character(0)
    cur_sub_n <- if (!is.null(instr) && isTRUE(instr$multi))
      length(instr$subscales) else 0L
    rv$.conv_editing_id <- id %||% ""
    rv$.conv_editing_pool <- pool
    rv$.conv_editing_n_sub <- max(2L, cur_sub_n)

    showModal(modalDialog(
      title = if (is.null(instr)) "Nuevo instrumento de comparación"
              else                "Editar instrumento",
      size = "l", easyClose = FALSE,
      textInput("conv_modal_label", "Etiqueta (ej. 'BAI', 'DERS-15'):",
                 value = instr$label %||% ""),
      radioButtons("conv_modal_multi", "Tipo:",
        choices = c("Unidimensional (un total)" = "uni",
                     "Multidimensional (subescalas)" = "multi"),
        selected = if (isTRUE(instr$multi)) "multi" else "uni",
        inline = TRUE),
      radioButtons("conv_modal_expected", "Relación esperada:",
        choices = c("Convergente"   = "convergent",
                     "Discriminante" = "discriminant"),
        selected = instr$expected %||% "convergent", inline = TRUE),
      uiOutput("conv_modal_body"),
      checkboxInput("conv_modal_include_total",
        "Calcular también el total global (solo si es multidimensional)",
        value = isTRUE(instr$include_total)),
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("conv_modal_save", "Guardar",
          class = "btn-go", icon = icon("check"))
      )
    ))
  }

  output$conv_modal_body <- renderUI({
    if (is.null(rv$.conv_editing_pool)) return(NULL)
    pool <- rv$.conv_editing_pool
    is_multi <- identical(input$conv_modal_multi, "multi")
    if (!is_multi) {
      cur <- character(0)
      if (nzchar(rv$.conv_editing_id %||% "")) {
        ins <- Filter(function(x) identical(x$id, rv$.conv_editing_id),
                       rv$convergent_instruments)
        if (length(ins) > 0L && !isTRUE(ins[[1]]$multi))
          cur <- ins[[1]]$items
      }
      return(selectizeInput("conv_modal_items", "Ítems del instrumento:",
        choices = pool, selected = cur, multiple = TRUE,
        options = list(plugins = list("remove_button"))))
    }
    # Multi mode: render N subscale editors
    n_sub <- rv$.conv_editing_n_sub %||% 2L
    cur_sub <- list()
    if (nzchar(rv$.conv_editing_id %||% "")) {
      ins <- Filter(function(x) identical(x$id, rv$.conv_editing_id),
                     rv$convergent_instruments)
      if (length(ins) > 0L && isTRUE(ins[[1]]$multi))
        cur_sub <- ins[[1]]$subscales
    }
    div(
      lapply(seq_len(n_sub), function(j) {
        # cur_sub may be shorter than n_sub (creating new instrument or
        # user added rows). Guard the subscript access.
        prev     <- if (j <= length(cur_sub)) cur_sub[[j]] else NULL
        cur_name <- if (!is.null(prev) && nzchar(prev$name %||% ""))
                       prev$name else sprintf("S%d", j)
        cur_its  <- if (!is.null(prev)) (prev$items %||% character(0))
                    else character(0)
        div(style = "padding:8px; margin-bottom:8px; border:1px dashed #D1D5DB; border-radius:6px;",
          tags$small(tags$strong(sprintf("Subescala %d", j))),
          fluidRow(
            column(4, textInput(sprintf("conv_modal_sub_name_%d", j),
                                 "Nombre:", value = cur_name)),
            column(8, selectizeInput(sprintf("conv_modal_sub_items_%d", j),
                                       "Ítems:",
                                       choices = pool, selected = cur_its,
                                       multiple = TRUE,
                                       options = list(plugins = list("remove_button")))))
        )
      }),
      div(style = "display:flex; gap:8px;",
        actionButton("conv_modal_add_sub", "+ subescala",
          class = "btn-back", style = "font-size:12px;"),
        actionButton("conv_modal_rm_sub", "− subescala",
          class = "btn-back", style = "font-size:12px;"))
    )
  })

  observeEvent(input$conv_modal_add_sub, {
    rv$.conv_editing_n_sub <- (rv$.conv_editing_n_sub %||% 2L) + 1L
  })
  observeEvent(input$conv_modal_rm_sub, {
    cur <- rv$.conv_editing_n_sub %||% 2L
    rv$.conv_editing_n_sub <- max(2L, cur - 1L)
  })

  observeEvent(input$conv_modal_save, {
    lab <- trimws(input$conv_modal_label %||% "")
    if (!nzchar(lab)) {
      showNotification("La etiqueta no puede estar vacía.",
                        type = "warning"); return()
    }
    is_multi <- identical(input$conv_modal_multi, "multi")
    expected <- input$conv_modal_expected %||% "convergent"
    if (is_multi) {
      n_sub <- rv$.conv_editing_n_sub %||% 2L
      subs <- list()
      for (j in seq_len(n_sub)) {
        nm  <- trimws(input[[sprintf("conv_modal_sub_name_%d", j)]] %||% "")
        its <- input[[sprintf("conv_modal_sub_items_%d", j)]] %||% character(0)
        if (nzchar(nm) && length(its) > 0L)
          subs[[length(subs) + 1L]] <- list(name = nm, items = its)
      }
      if (length(subs) < 2L) {
        showNotification("Necesitas al menos 2 subescalas con ítems.",
                          type = "warning"); return()
      }
      new_instr <- list(
        id            = if (nzchar(rv$.conv_editing_id %||% ""))
                          rv$.conv_editing_id else .conv_new_id(),
        label         = lab, multi = TRUE,
        items         = character(0),
        subscales     = subs,
        expected      = expected,
        include_total = isTRUE(input$conv_modal_include_total))
    } else {
      its <- input$conv_modal_items %||% character(0)
      if (length(its) < 3L) {
        showNotification("Necesitas al menos 3 ítems.", type = "warning")
        return()
      }
      new_instr <- list(
        id            = if (nzchar(rv$.conv_editing_id %||% ""))
                          rv$.conv_editing_id else .conv_new_id(),
        label         = lab, multi = FALSE,
        items         = its, subscales = list(),
        expected      = expected, include_total = FALSE)
    }
    # Upsert
    cur <- rv$convergent_instruments
    idx <- which(vapply(cur, function(x) identical(x$id, new_instr$id),
                         logical(1)))
    if (length(idx) > 0L) cur[[idx]] <- new_instr
    else                  cur[[length(cur) + 1L]] <- new_instr
    rv$convergent_instruments <- cur
    rv$.conv_editing_id <- NULL; rv$.conv_editing_pool <- NULL
    rv$.conv_editing_n_sub <- NULL
    removeModal()
    showNotification("Instrumento guardado.", type = "message", duration = 3)
  })

  # ===================================================================
  # START button
  # ===================================================================
  observeEvent(input$start, {
    df <- raw_data()
    if (is.null(df)) {
      showNotification("Carga un archivo primero.", type = "warning"); return()
    }
    its <- input$items
    if (length(its) == 0L) {
      prefix <- input$name_items %||% ""
      its <- if (nzchar(prefix)) grep(paste0("^", prefix), colnames(df), value = TRUE)
             else character(0)
    }
    if (length(its) < 3L) {
      showNotification("Se requieren ≥ 3 ítems.", type = "warning"); return()
    }
    rv$data <- df; rv$items <- its
    rv$config <- list()
    rv$phase <- 1L; rv$results <- list(); rv$boost_obj <- NULL
    rv$started <- TRUE
    showNotification("Wizard iniciado.", type = "message", duration = 3)
  })

  # ===================================================================
  # Result panels
  # ===================================================================
  output$tbl_loadings <- DT::renderDT({
    b <- rv$boost_obj; if (is.null(b)) return(NULL)
    df <- b$final_structure
    # Round numeric factor cols to 2 decimals
    fact_cols <- grep("^[fF][0-9]+$", colnames(df), value = TRUE)
    for (cc in fact_cols)
      df[[cc]] <- ifelse(is.na(df[[cc]]) | abs(df[[cc]]) < 1e-3, NA,
                          round(df[[cc]], 2))
    DT::datatable(df, options = list(pageLength = 25), rownames = FALSE) |>
      DT::formatRound(columns = fact_cols, digits = 2)
  })
  output$tbl_fit <- DT::renderDT({
    b <- rv$boost_obj
    if (is.null(b) || is.null(b$bondades_original)) return(NULL)
    DT::datatable(b$bondades_original, options = list(pageLength = 10),
                   rownames = FALSE)
  })
  output$boost_text <- renderText({
    r <- rv$results$phase3$report
    if (is.null(r) || !is.character(r$text))
      return("El reporte aparecerá tras la Fase 3 (boosting).")
    paste(r$text, collapse = "\n")
  })
  output$tbl_reliability <- DT::renderDT({
    rel <- rv$results$phase4; if (is.null(rel)) return(NULL)
    om <- rel$omega %||% NA; al <- rel$alpha %||% NA
    if (length(om) == 0L) return(NULL)
    df <- data.frame(
      factor = names(om) %||% paste0("f", seq_along(om)),
      omega  = round(as.numeric(om), 3),
      alpha  = round(as.numeric(al)[seq_along(om)], 3))
    DT::datatable(df, options = list(pageLength = 10), rownames = FALSE)
  })
  output$tbl_validity <- DT::renderDT({
    val <- rv$results$phase5
    if (is.null(val) || is.null(val$corr_df) || nrow(val$corr_df) == 0L) {
      return(DT::datatable(
        data.frame(Mensaje =
          "Sin variables criterio sueltas. Mira la card 'Validez convergente / discriminante' abajo."),
        options = list(dom = "t", ordering = FALSE),
        rownames = FALSE, class = "compact"))
    }
    DT::datatable(val$corr_df, options = list(pageLength = 15),
                   rownames = FALSE)
  })
  output$plt_validity <- renderPlot({
    val <- rv$results$phase5
    if (is.null(val) || is.null(val$corr_df) || nrow(val$corr_df) == 0L ||
        !requireNamespace("ggplot2", quietly = TRUE))
      return(NULL)
    cd <- val$corr_df
    cd$label <- paste(cd$factor, "vs", cd$external)
    cd$sig   <- ifelse(cd$p < 0.05, "p < .05", "p ≥ .05")
    cd$absr  <- abs(cd$r)
    cd$label <- factor(cd$label, levels = cd$label[order(cd$absr)])
    ggplot2::ggplot(cd, ggplot2::aes(x = absr, y = label, fill = sig)) +
      ggplot2::geom_col(width = 0.7) +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", r)),
                          hjust = -0.15, size = 3.5) +
      ggplot2::scale_fill_manual(values = c("p < .05" = "#18BC9C",
                                              "p ≥ .05" = "#BDC3C7")) +
      ggplot2::scale_x_continuous(
        limits = c(0, max(0.5, max(cd$absr, na.rm = TRUE) * 1.20)),
        expand = c(0, 0)) +
      ggplot2::labs(title = "|r| factor × variable externa",
                    x = "|r|", y = NULL, fill = NULL) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        legend.position = "top",
        plot.title = ggplot2::element_text(face = "bold", size = 13),
        panel.grid.major.y = ggplot2::element_blank(),
        plot.margin = ggplot2::margin(8, 14, 8, 8))
  })

  # ---- Convergent/discriminant validity ----
  output$tbl_convergent <- DT::renderDT({
    conv <- rv$results$phase5b
    if (is.null(conv) || isTRUE(conv$skipped) || is.null(conv$table) ||
        nrow(conv$table) == 0L) {
      return(DT::datatable(
        data.frame(Mensaje =
          "Sin instrumentos de comparación definidos. Usa el botón 🔍 o + en la Fase 5."),
        options = list(dom = "t", ordering = FALSE),
        rownames = FALSE, class = "compact"))
    }
    df <- conv$table
    # Pretty col names
    pretty <- df
    colnames(pretty) <- c("Instrumento","Esperado","Principal",
                           "Convergente","r","IC inf","IC sup","p",
                           "Magnitud","Veredicto")
    DT::datatable(pretty, options = list(pageLength = 20, scrollX = TRUE),
                   rownames = FALSE) |>
      DT::formatStyle("Veredicto",
        backgroundColor = DT::styleEqual(
          c("convergencia fuerte","convergencia moderada",
            "convergencia débil","convergencia no respaldada",
            "no significativa","discrimina (|r|<.30)",
            "discriminación dudosa","NO discrimina (|r|>=.50)"),
          c("#A7F3D0","#D1FAE5","#FEF3C7","#FECACA",
            "#E5E7EB","#A7F3D0","#FEF3C7","#FECACA")))
  })
  output$plt_convergent <- renderPlot({
    conv <- rv$results$phase5b
    if (is.null(conv) || isTRUE(conv$skipped) || is.null(conv$table) ||
        !requireNamespace("ggplot2", quietly = TRUE))
      return(NULL)
    df <- conv$table
    df$principal  <- factor(df$principal,  levels = unique(df$principal))
    df$convergent <- factor(df$convergent, levels = unique(df$convergent))
    df$label <- sprintf("%.2f%s", df$r,
                         ifelse(!is.na(df$p) & df$p < 0.05, "*", ""))
    ggplot2::ggplot(df,
        ggplot2::aes(x = convergent, y = principal, fill = r)) +
      ggplot2::geom_tile(color = "white", linewidth = 0.4) +
      ggplot2::geom_text(ggplot2::aes(label = label,
                                         color = abs(r) > 0.55),
                          size = 3.7, fontface = "bold",
                          show.legend = FALSE) +
      ggplot2::scale_fill_gradient2(
        low = "#DC2626", mid = "#F3F4F6", high = "#1E40AF",
        midpoint = 0, limits = c(-1, 1), name = "r") +
      ggplot2::scale_color_manual(values = c(`TRUE` = "white",
                                               `FALSE` = "#1F2937")) +
      ggplot2::labs(title = "Correlaciones factor × convergente",
                    subtitle = "* p < .05",
                    x = NULL, y = NULL) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 35, hjust = 1, size = 10),
        axis.text.y = ggplot2::element_text(size = 10),
        plot.title  = ggplot2::element_text(face = "bold", size = 13),
        plot.subtitle = ggplot2::element_text(color = "#6B7280", size = 10),
        panel.grid = ggplot2::element_blank(),
        legend.position = "right",
        plot.margin = ggplot2::margin(8, 14, 8, 8))
  })

  # Verbose trace from efa_boosting (captured stdout) + conceptual analysis
  # appended at the bottom when AI was active. The user asked for a single
  # place to see HOW the algorithm depured items.
  output$trace_text <- renderText({
    txt <- rv$boost_console
    if (is.null(txt) || !nzchar(txt))
      return("La bitácora aparecerá tras la Fase 3 (boosting).\nCorre el wizard para verla aquí.")
    # Append OptimalFactor::print_conceptual_analysis() at the end when the
    # boost was run with AI — it explains conceptually WHY each item was
    # dropped, complementing the stats-only verbose above.
    b <- rv$boost_obj
    if (!is.null(b) && !is.null(b$conceptual_analysis)) {
      conceptual <- tryCatch(
        paste(utils::capture.output(
          OptimalFactor::print_conceptual_analysis(b,
            width = 100, show_stats = TRUE)),
          collapse = "\n"),
        error = function(e) "")
      if (nzchar(conceptual)) {
        sep <- paste0("\n\n", strrep("═", 70), "\n",
                       "ANÁLISIS CONCEPTUAL DE ÍTEMS (IA)\n",
                       strrep("═", 70), "\n")
        txt <- paste0(txt, sep, conceptual)
      }
    }
    txt
  })
  output$dl_trace <- downloadHandler(
    filename = function()
      sprintf("efa_boosting_trace_%s.txt", format(Sys.time(), "%Y%m%d_%H%M%S")),
    content = function(file) {
      txt <- rv$boost_console %||% ""
      writeLines(txt, file)
    }
  )

  # Conceptual analysis — driven by OptimalFactor::print_conceptual_analysis(),
  # which is the canonical formatter for the conceptual block. We capture its
  # output and show it verbatim so the wizard mirrors what a code-only user
  # would see in the console.
  output$conceptual_ui <- renderUI({
    b <- rv$boost_obj
    if (is.null(b) || is.null(b$conceptual_analysis))
      return(tags$em(class = "text-muted",
        "Activa IA y vuelve a correr el boosting para ver el análisis conceptual aquí."))
    txt <- tryCatch(
      paste(utils::capture.output(
        OptimalFactor::print_conceptual_analysis(b,
          width = 100, show_stats = TRUE)),
        collapse = "\n"),
      error = function(e) paste("(error rindiendo análisis conceptual:",
                                  e$message, ")"))
    tags$pre(class = "step-body", style = "max-height:680px;", txt)
  })

  # ===================================================================
  # IA Chat
  # ===================================================================
  output$chat_status <- renderUI({
    if (!isTRUE(input$use_ai) || !nzchar(input$openai_api_key %||% "")) {
      return(tags$div(class = "alert alert-warning py-2",
        tags$small("Activa IA y proporciona una API key en el sidebar.")))
    }
    autopiloto <- isTRUE(input$autopilot)
    tags$div(class = "alert alert-success py-2",
      tags$small(if (autopiloto) "Autopiloto ACTIVO. La IA puede iniciar."
                 else "Chat manual. Tú haces la pregunta."))
  })

  # Markdown → HTML for chat replies. The model often returns **bold**,
  # numbered lists and bullets — render them as actual HTML instead of
  # showing raw asterisks.
  .md_to_html <- function(txt) {
    if (is.null(txt) || !nzchar(txt)) return(HTML(""))
    tryCatch(
      HTML(commonmark::markdown_html(txt, smart = TRUE,
                                       extensions = c("table", "strikethrough"))),
      error = function(e) HTML(gsub("\n", "<br>", htmltools::htmlEscape(txt)))
    )
  }
  output$chat_ui <- renderUI({
    msgs <- rv$chat
    if (length(msgs) == 0L) {
      return(div(class = "chat-box",
        tags$em(class = "text-muted",
          "Aún no hay conversación. Escribe abajo y presiona Enviar.")))
    }
    div(class = "chat-box",
      lapply(msgs, function(m) {
        cls <- if (m$role == "user") "chat-msg chat-user" else "chat-msg chat-ai"
        role_lbl <- if (m$role == "user") "Tú" else "IA"
        # User messages: keep verbatim (just \n → <br>) so their original
        # formatting is preserved. AI messages: parse as Markdown.
        body <- if (m$role == "user")
          HTML(gsub("\n", "<br>", htmltools::htmlEscape(m$content)))
        else .md_to_html(m$content)
        div(class = cls,
          div(class = "chat-role", role_lbl),
          body)
      }))
  })

  observeEvent(input$chat_send, {
    txt <- isolate(input$chat_input %||% "")
    if (!nzchar(txt)) return()
    if (!isTRUE(input$use_ai) || !nzchar(input$openai_api_key %||% "")) {
      showNotification("Necesitas activar IA + API key.", type = "warning"); return()
    }
    rv$chat[[length(rv$chat) + 1L]] <- list(role = "user", content = txt)
    updateTextAreaInput(session, "chat_input", value = "")
    # Build context
    ctx <- character(0)
    if (!is.null(rv$boost_obj)) {
      b <- rv$boost_obj
      ctx <- c(ctx,
        sprintf("Dataset: %d filas × %d ítems iniciales (%s).",
          nrow(rv$data), length(rv$items),
          paste(utils::head(rv$items, 8), collapse = ", ")),
        sprintf("Ítems finales tras boosting: %d.",
          nrow(b$final_structure)),
        sprintf("Ítems eliminados: %s.",
          paste(b$removed_items %||% "(ninguno)", collapse = ", ")),
        sprintf("stop_reason: %s.", b$stop_reason %||% "?"))
    }
    sys_msg <- paste(
      "Eres un experto en psicometría y análisis factorial.",
      "Responde a la pregunta del usuario considerando este contexto:",
      paste(ctx, collapse = " "),
      sep = "\n")
    pr <- shiny::Progress$new(session); on.exit(pr$close(), add = TRUE)
    pr$set(message = "Consultando IA...", value = 0.3)
    resp <- tryCatch({
      if (!requireNamespace("httr", quietly = TRUE) ||
          !requireNamespace("jsonlite", quietly = TRUE)) {
        stop("httr + jsonlite requeridos.")
      }
      r <- httr::POST("https://api.openai.com/v1/chat/completions",
        httr::add_headers(Authorization = paste("Bearer", input$openai_api_key),
                            `Content-Type` = "application/json"),
        httr::timeout(60),
        body = jsonlite::toJSON(list(
          model = input$gpt_model %||% "gpt-4o",
          messages = list(
            list(role = "system", content = sys_msg),
            list(role = "user", content = txt)),
          temperature = 0.5), auto_unbox = TRUE))
      if (httr::status_code(r) >= 400) stop("HTTP ", httr::status_code(r))
      out <- httr::content(r, as = "parsed")
      out$choices[[1]]$message$content
    }, error = function(e) paste("(error IA:", e$message, ")"))
    rv$chat[[length(rv$chat) + 1L]] <- list(role = "assistant", content = resp)
  })
}

shinyApp(ui, server)
