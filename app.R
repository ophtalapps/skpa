library(shiny)
library(DT)
library(dplyr)
library(stringr)
library(lubridate)
library(data.table)
library(shinyWidgets)
library(rlist)
library(shinybusy)
library(plotly)
library(shinythemes)
library(purrr)
renv::clean()
css <- "
.nowrap {
  white-space: nowrap;
}
.red {
  background-color: #e34755;
}
table.dataTable tr.selected td.red {
  background-color: #e34755 !important;
}
"

ui <- navbarPage(
  'SKPA',
  theme = shinytheme("yeti"),
  header = (
    add_busy_spinner('fading-circle', position = 'bottom-left')
  ),
  tabPanel(
    ' ', 
    icon = icon(
      "magnifying-glass"),
    tags$head(
      tags$style(".recalculating { opacity: inherit !important; }"),
      tags$style(
        "#table  {white-space: nowrap;  }")),
    column(5,
    column(
      6,
      fluidRow(
        wellPanel(
          searchInput("pnr_input", "Id", btnSearch = icon("search"), btnReset = icon("remove"), width = "100%"),
          pickerInput("select", label = "", choices = NULL, multiple = TRUE, options = list(title = "No tests loaded"))
        )
      )
    ),
    column(2),
    column(2,
      br(),
      br(),
      uiOutput('lat')),
      fluidRow(
        dataTableOutput('table')),
        br(),
        fluidRow(
          column(
            5,
            uiOutput(
              'warning')
          )
      )
    ),
    column(1),
    column(
      6,
      br(),
      fluidRow(
        plotlyOutput(
          'plot')
        )
      )
  )
)

server <- function(input, output, session) {
  
  in_file <- file.path('./data/INDEX.CSV')
  previousSelection <- NULL
  previousPage <- NULL

  tests <- reactiveValues(warning = FALSE)
  picker <- reactiveValues(choices = NULL, title = "Select", action = FALSE, tbl = NULL)

  observeEvent(input$pnr_input_reset, {
    tests$all <- NULL 
    tests$m <- NULL
    tests$lname <- NULL
    tests$fname <- NULL
    set_picker_null()
  })

  observeEvent(input$pnr_input, {
    req(input$pnr_input)
    pnr <- input$pnr_input
  
    if (isTRUE(pnr == "") | isTRUE(str_length(pnr) != 12)) {
      tests$all <- NULL 
      tests$m <- NULL
      tests$lname <- NULL
      tests$fname <- NULL
      set_picker_null()
      sendSweetAlert(
        session = session,
        title = "Error",
        text = "No patients found!",
        type = "error"
      )
    }
    else  {
      dat <- data.table::fread(
      in_file, 
      encoding = 'Latin-1', 
      check.names = TRUE
    ) %>%
      filter(Pat.ID. == pnr)
    
    if (isTRUE(nrow(dat) > 0)) {
      dat <- dat %>%
        select(
          1:5, 7, 12:13, 65, 85, 61:62, 83:84
        ) %>%
        setnames(
          c(
            "lname", "fname", "id", "dob", "date",
            "lat", "k1", "k2", "kmax", "tct", "status", "error",
            "arc", "prc"
          )
        ) %>%
        .[error %in% 0] %>%
        .[, dob := mdy(dob)] %>%
        .[, date := mdy(date)] %>%
        .[, age := round(as.numeric((date - dob) / 365.25), 2)] %>%
        mutate_at(
          c(
            "k1", "k2", "kmax", "arc", "prc", "tct"
          ),
          ~ as.numeric(
            sub(',', '.', .x)
          )
        )
      if (isTRUE(nrow(dat) > 0))
        tests$all <- dat
      # kanske visa varningsmeddelande
    }
  }
  })

  set_picker_null <- function() {
    updatePickerInput(
      session = session,
      inputId = 'select',
      choices = paste(''),
      options = list(
        title = "No tests loaded",
        `actions-box` = FALSE))
  }

  observeEvent(tests$all, {
    if (!is.null(tests$all)) {1
      tbl <- as.data.table(table(isolate(tests$all$date), isolate(tests$all$lat))) 
       print('asdf')
      tbl <- tbl[tbl$N != 0,]
      tbl <- tbl[order(-V2)]

      updatePickerInput(
        session = session,
        inputId = "select",
        choices = paste(
          tbl$V1,
          tbl$V2
          ),
        choicesOpt = list(
          subtext = paste(
            "N",
            tbl$N,
            sep = ":"
          )
          ),
        options = list(
          title = paste0(
            unique(tests$all$lname), ", ",
            unique(tests$all$fname)
            ),
          `actions-box` = FALSE
        )
      )
    }
  })

  output$lat <- renderUI({
    req(tests$all)
    txt <- paste("<h2><b>",unique(substr(input$select, str_locate(input$select, ' ') + 1, str_length(input$select))),"</b></h2>")
    HTML(paste(txt))  
  })

  output$warning <- renderUI({
    req(tests$warning)

    txt <- paste(
      "<b><h3>",
      "Warning!",
      "</h3></b>",
      "Color coded progression analysis is not available when comparing different number of measurements at each occasion"
    )

    HTML(
      as.character(
        icon("exclamation-triangle", 'fa-3x')
      ), 
      txt
    )
  })
  
  means <- eventReactive(input$select, {
    req(input$select)

    d <- as.Date(substr(input$select, 1, str_locate(input$select, ' ') + 1))
    l <- substr(input$select, str_locate(input$select, ' ') + 1, str_length(input$select))
    dt <- isolate(tests$all)[lat %in% l] %>%
      select(c(1, 2, 3, 4, 5, 15, 6, 8, 9, 13, 14, 10))

    tests$tbl <- as.data.table(table(dt$date, dt$lat))
    
    dt.l <- lapply(d, function(x) {
      tmp <- dt[date %in% x]
      m <- as.data.table(lapply(tmp[,-c(1:7)], function(x) {
        round(mean(x), digits = 2)
      }))
      N <- nrow(tmp)
      tmp <- cbind(tmp[1,c(3,4,5,6)], N, m) %>%
        t %>%
        as.data.frame
    })

    list.cbind(dt.l)
    df <- as.data.frame(dt.l)

    df <- df[-c(1,2,3),, drop = FALSE]
    colnames(df) <- d

    if (isTRUE(ncol(df) > 1)) {
      diff.df <- df %>%
        select(1, ncol(df))
      diffs <- get_diffs(diff.df)

      if (diff.df[2,1] == diff.df[2,2]) {
        # om 1 mot 1
        p <- get_progress(diff.df, diffs)

        df <- cbind(df, diffs, p)
        tests$warning <- FALSE
      }
      else {
        df <- cbind(df, diffs)
        tests$warning <- TRUE
      }
    }
    tests$m <- df

    df
  })
  
  get_diffs <- function(diff.df) {
    dates <- as.Date(colnames(diff.df))
    
    age <- as.numeric(abs(difftime(dates[1], dates[2])))
    k2 <- round(diff(as.numeric(diff.df[3,])), digits = 2)
    kmax <- round(diff(as.numeric(diff.df[4,])), digits = 2)
    tct <- round(diff(as.numeric(diff.df[7,])), digits = 2)
    arc <- round(diff(as.numeric(diff.df[5,])), digits = 2)
    prc <- round(diff(as.numeric(diff.df[6,])), digits = 2)
    diffs <- data.table(age = age, N = '', k2 = k2, kmax = kmax, arc = arc, prc = prc, tct = tct) %>%
      t %>%
      as.data.frame
    colnames(diffs) <- 'diff'
    return(diffs)
  }
  
  get_progress <- function(diff.df, diffs) {
    m <- diff.df
    d <- diffs 
    if (diff.df[2,1] == 1) {
      kmax.progress <- ifelse (
        as.numeric(m[4,1]) < 49,
        ifelse (
          as.numeric(d[4,1]) <= 0.67, 0, 1
        ),
        ifelse(
          as.numeric(d[4,1]) <= 1.42, 0, 1
        )
      )
      k2.progress <- ifelse (
        as.numeric(d[3,1]) <= 0.94, 0, 1
      )
      
      tct.progress <- ifelse (
        as.numeric(m[5,1]) < 482.5,
        ifelse (
          as.numeric(d[5,1]) >= -15.4, 0, 1
        ),
        ifelse (
          as.numeric(d[5,1]) >= -9.51, 0, 1
        )
      )
      
      arc.progress <- ifelse (
        as.numeric(m[6,1]) < 7.33,
        ifelse(as.numeric(d[6,1]) >= -0.25, 0, 1
        ),
        ifelse(as.numeric(d[6,1]) >= -0.11, 0, 1
        )
      )
      prc.progress <- ifelse (
        as.numeric(d[7,1]) >= -0.19, 0, 1
      )
      
      p <- data.table(
        age = -1, 
        N = -1, 
        k2 = k2.progress, 
        kmax = kmax.progress,
        tct = tct.progress, 
        arc = arc.progress, 
        prc = prc.progress)  
    }
    else {
      kmax.progress <- ifelse(
        as.numeric(m[4,1]) < 49,
        ifelse(
          as.numeric(d[4,1]) <= 0.27, 0,
          ifelse(
            as.numeric(d[4,1]) <= 0.44, 2, 1
          )
        ),
        ifelse(
          as.numeric(d[4,1]) <= 0.67, 0,
          ifelse(as.numeric(d[4,1]) <= 1.08, 2, 1
          )
        )
      )
      k2.progress <- ifelse(
        as.numeric(d[3,1]) < 0.52, 0,
        ifelse(as.numeric(d[3,1]) <= 0.72, 2, 1
        )
      )
      tct.progress <- ifelse (
        as.numeric(m[5,1]) < 482.5,
        ifelse (
          as.numeric(d[5,1]) >= -6.1, 0,
          ifelse(
            as.numeric(d[5,1]) >= -10.2, 2, 1
          )
        ),
        ifelse(
          as.numeric(d[5,1]) >= -3.54, 0,
          ifelse(
            as.numeric(d[5,1]) >= -5.75,2, 1
          )
        )
      )
      arc.progress <- ifelse (
        as.numeric(m[6,1]) < 7.33,
        ifelse(
          as.numeric(d[6,1]) >= -0.10, 0,
          ifelse(
            as.numeric(d[6,1]) >= -0.17, 2, 1
          )
        ),
        ifelse(
          as.numeric(d[6,1]) >= -0.04, 0,
          ifelse(
            as.numeric(d[6,1]) >= -0.07, 2, 1
          )
        )
      )
      prc.progress <- ifelse (
        as.numeric(d[7,1]) >= -0.088, 0,
        ifelse(
          as.numeric(d[7,1]) >= -0.12, 2, 1
        )
      )
      p <- data.table(
        age = -1, 
        N = -1, 
        k2 = k2.progress, 
        kmax = kmax.progress,
        tct = tct.progress, 
        arc = arc.progress, 
        prc = prc.progress)
    }
    p <- p %>%
      t %>%
      as.data.frame
    colnames(p) <- 'progress'

    return(p)
  }
  
  observeEvent(input$select, {
    if (is.null(input$select)) {
      tests$warning <- FALSE
      tests$m <- NULL
    }

    l <- substr(input$select, str_locate(input$select, ' ') + 1, str_length(input$select))

    dt <- tests$all
    abox <- FALSE

    if (length(l) != 0) {
      dt <- dt[lat %in% l]
      abox <- TRUE
    }
    if (isTRUE(nrow(dt) > 0)) {
      tbl <- as.data.table(
        table(
          dt$date, dt$lat))
      tbl <- tbl[tbl$N != 0,]
      tbl <- tbl[order(-V2)]

      updatePickerInput(
        session = session,
        inputId = 'select',
        selected = input$select,
        choices = paste(
          tbl$V1,
          tbl$V2),
        choicesOpt = list(
          subtext = paste(
            "n",
            tbl$N,
            sep = ":")), 
        options = list(
          `selected-text-format`= "static",
          title = paste0(unique(tests$all$lname), ', ',
            unique(tests$all$fname)),
          `actions-box` = abox))

    }
  }, ignoreNULL = FALSE)

  observeEvent(input$table_columns_selected, {
    sel.cols <- isolate(input$table_columns_selected)
    if (length(sel.cols) < 2)
      return()
   
    m <- isolate(means())

   if (isTRUE(sel.cols[sel.cols == 0] == 0)) {
     sel.cols <- sel.cols[c(sel.cols != 0)]
   }
    if (isTRUE('diff' %in% colnames(m[sel.cols]))) {
      sel.cols <- sel.cols[1:length(sel.cols)-1]
    }

    if (isTRUE(length(sel.cols) == 3)) {
      sel.cols <- sel.cols[c(length(sel.cols) - 1, length(sel.cols))]
    }

    if (isTRUE(length(sel.cols) == 2)) {
      diff.df <- m[sel.cols]
      diff.df <- diff.df[, order(names(diff.df))]
      
      diffs <- get_diffs(diff.df)
      
      if (isTRUE('diff' == colnames(m[ncol(m)]))) {
        df <- cbind(m[,-c(ncol(m)),drop = FALSE], diffs)
        #tests$warning <- TRUE 
      }
      else if (isTRUE('progress' == colnames(m[ncol(m)]))) {
        df <- cbind(m[,-c(ncol(m), ncol(m) -1), drop = FALSE], diffs)
        #tests$warning <- FALSE
      }
      else
        df <- cbind(m, diffs)
      
      if (isTRUE(diff.df[2,1] != diff.df[2,2])) {
        tests$warning <- TRUE
      }
      else if(isTRUE(diff.df[2,1] == diff.df[2,2])) {
        tests$warning <- FALSE

        p <- get_progress(diff.df, diffs)
        df <- cbind(df , p)
      }
      tests$m <- df

    }

    if (is.null(sel.cols)) {
      tests$m <- m

      if (isTRUE('diff' == colnames(m[ncol(m)]))) {
        tests$warning <- TRUE
      }
      else if (isTRUE('progress' == colnames(m[ncol(m)]))) {
        tests$warning <- FALSE
      }
    }
    tests$sel.cols <- sel.cols
    selectColumns(proxy, select = sel.cols)
  }, ignoreNULL = TRUE)
  
  table_data <- reactive({
    req(tests$m)
    t <- isolate(tests$m)
    t[1,] <- as.character(floor(as.numeric(t[1,])))
    t
  })
  
  output$table = renderDT({
    req(tests$m)

    if (isTRUE('progress' == colnames(isolate(tests$m)[ncol(isolate(tests$m))]))) {
      rc <- 2
      t <- ncol(isolate(tests$m))
    }
    else {
      rc <- 1
      t <- NULL
    }
    
    cn <- colnames(isolate(tests$m))
    d <- isolate(tests$m)

    d[1,] <- as.character(floor(as.numeric(d[1,])))
    if (isTRUE(any(grepl('diff', cn)))) {
        cn[cn == 'diff'] <- '&#916;'
       
        colnames(d) <- cn
        d[1,'&#916;'] <- paste0(d[1, '&#916;'], ' days')
    }

    lc <- ifelse(ncol(isolate(tests$m)) == 1 , 0, 1)
    d <- (d[c(1,2,3,4,5,6,7), , drop = FALSE])
    rownames(d) <- c("Age", "n", "K2", "Kmax", "A", "B", "C")

    isolate({
    tbl <- datatable(
        d,
        rownames = TRUE,
        selection = list(
          mode = 'multiple',
         selected = tests$sel.cols,
          target = 'column'),
        extensions = 'FixedColumns',
        options = list(
          rowid = 0,
          ordering = FALSE,
          keys = TRUE,
          resetPaging = FALSE,
          fixedColumns = list(leftColumns = lc, rightColumns = rc),
          columnDefs = list(
            list(
              className = 'dt-left',
              targets = '_all'),
            list(visible = FALSE,
                 targets = t)),
          dom = 't',
          scrollX = TRUE),
        escape = FALSE) %>%
        formatStyle(0, fontWeight = 'bold') 

        if (ncol(isolate(tests$m)) > 2) {
          tbl <- tbl %>%
            formatStyle('&#916;', fontWeight = 'bold') 
          if (isTRUE('progress' == colnames(tests$m[ncol(isolate(tests$m))]))) {
            tbl <- tbl %>%
              formatStyle('&#916;', 'progress', backgroundColor = styleEqual(c(0,1, 2), c('#00ff00', '#ff0000', '#ffff00')))
          }
        }})
      tbl
    }, server = FALSE)
  
  output$plot <- renderPlotly({
    req(tests$all)
    plot.dt <- means()
    if(isTRUE('progress' == colnames(plot.dt)[ncol(plot.dt)]))
      plot.dt <- plot.dt[,-c(ncol(plot.dt)-1, ncol(plot.dt))]
    else if (isTRUE('diff' == colnames(plot.dt)[ncol(plot.dt)]))
      plot.dt <- plot.dt[, -c(ncol(plot.dt))]
    dates <- colnames(plot.dt)
    plot.dt <- plot.dt  %>%
      t %>%
      as.data.frame
    
    if (isTRUE(nrow(plot.dt) > 0)) {
      age_min <- min(as.numeric(plot.dt$age)) - 0.5
      age_max <- max(as.numeric(plot.dt$age)) + 0.5
      
      m <-  list(
        size = 10,
        symbol = 1)
      
      t <- list(
        family = "arial",
        size = 18,
        color = 'black')
      
      p1 <- plot_ly(
        plot.dt,
        x = ~as.numeric(age),
        y = ~as.numeric(k2),
        name = 'K2',
        type = 'scatter',
        mode = 'markers+lines',
        text = ~dates,
        marker = list(
          size = 5,
          symbol = 1,
          color = '#ff00c1'
        ),
        line = list(
          color = '#ff00c1'
        )) %>%
        layout(xaxis = list(title = 'Age', range = c(age_min, age_max), dtick = 0.5),
               yaxis = list(title = list(
                 text = 'D', font=t), range = c(~min(as.numeric(k2)) - 10, ~min(as.numeric(k2)) + 15), dtick = 5),
               showlegend = TRUE) %>%
        add_annotations(
          text = "K2",
          x = 0.5,
          y = 1,
          yref = "paper",
          xref = "paper",
          xanchor = "left",
          yanchor = "top",
          yshift = 20,
          showarrow = FALSE,
          font = list(size = 12)
        )
      
      p5 <- plot_ly(
        plot.dt,
        x = ~as.numeric(age), 
        y =  ~as.numeric(kmax),
        name = 'Kmax',
        type = 'scatter',
        mode = 'markers+lines',
        text = ~dates,
        marker = list(
          size = 5,
          symbol = 1,
          color = '	#9600ff'
        ),
        line = list(
          color = '#9600ff'
        )) %>%
        layout(xaxis = list(title = 'Age', range = c(age_min, age_max), dtick = 0.5),
               yaxis = list(title = list(text = 'D', font = t),
                            range = c(~min(as.numeric(kmax)) - 10, ~max(as.numeric(kmax)) + 15), dtick = 5),
               showlegend = FALSE) %>%
        add_annotations(
          text = "Kmax",
          x = 0.5,
          y = 1,
          yref = "paper",
          xref = "paper",
          xanchor = "left",
          yanchor = "top",
          yshift = 20,
          showarrow = FALSE,
          font = list(size = 12)
        )
      
      p2 <- plot_ly(
        plot.dt,
        x = ~as.numeric(age),
        y = ~as.numeric(arc),
        name = 'A',
        type = 'scatter',
        mode = 'markers+lines',
        text = ~dates,
        marker = list(
          size = 5,
          symbol = 1,
          color = '	#4900ff'
        ),
        line = list(
          color = '#4900ff'
        )) %>%
        layout(xaxis = list(title = 'Age', range = c(age_min, age_max), dtick = 0.5),
               yaxis = list(title = list(text = 'mm', font = t),
                            range = c(~min(as.numeric(arc)) - 1, ~max(as.numeric(arc)) + 1), dtick = 0.25),
               showlegend = FALSE) %>%
        add_annotations(
          text = "A",
          x = 0.5,
          y = 1,
          yref = "paper",
          xref = "paper",
          xanchor = "left",
          yanchor = "top",
          yshift = 20,
          showarrow = FALSE,
          font = list(size = 12)
        )
      
      p3 <- plot_ly(
        plot.dt,
        x = ~as.numeric(age),
        y = ~as.numeric(tct),
        name = 'C',
        type = 'scatter',
        mode = 'markers+lines',
        text = ~dates,
        marker = list(
          size = 5,
          symbol = 1,
          color = '#00b8ff'
        ),
        line = list(
          color = '#00b8ff'
        )) %>%
        layout(xaxis = list(title = list(text = '<b>Age</b>', font = list(size = 14)), range = c(age_min, age_max), dtick = 0.5),
               yaxis = list(title = list(text = 'my', font = t), range = c(~min(as.numeric(tct)) - 100, ~max(as.numeric(tct)) + 100), dtick = 50),
               showlegend = FALSE) %>%
        add_annotations(
          text = "C",
          x = 0.5,
          y = 1,
          yref = "paper",
          xref = "paper",
          xanchor = "left",
          yanchor = "top",
          yshift = 20,
          showarrow = FALSE,
          font = list(size = 12)
        )
      
      p4 <- plot_ly(
        plot.dt,
        x = ~as.numeric(age),
        y = ~as.numeric(prc),
        name = 'B',
        type = 'scatter',
        mode = 'markers+lines',
        text = ~dates,
        marker = list(
          size = 5,
          symbol = 1,
          color = '#00fff9'
        ),
        line = list(
          color = '#00fff9'
        ),
        height = 750) %>%
        layout(xaxis = list(title = '', range = c(age_min, age_max), dtick = 0.5),
               yaxis = list(title = list(text = 'mm', font = t), range = c(~min(as.numeric(prc)) - 1, ~max(as.numeric(prc)) + 1), dtick = 0.25),
               showlegend = FALSE) %>%
        add_annotations(
          text = "B",
          x = 0.5,
          y = 1,
          yref = "paper",
          xref = "paper",
          xanchor = "left",
          yanchor = "top",
          yshift = 20,
          showarrow = FALSE,
          font = list(size = 12)
        )
      
      p <- subplot(p1, p5, p2, p4, p3, nrows = 3, margin = 0.04, titleX = TRUE, shareX = TRUE) %>%
        # layout(legend = list(xanchor = "center", x = 0.5, y = 1.1, orientation = 'h')) %>%
        config(displayModeBar = FALSE) 
      p
    }
    
  })

  proxy = dataTableProxy(outputId = 'table', deferUntilFlush = TRUE)
}

shinyApp(ui = ui, server = server)

