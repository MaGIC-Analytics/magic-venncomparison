# ─── Utilities ─────────────────────────────────────────────────────────────────

`%||%` <- function(a, b) {
    if (is.null(a)) return(b)
    if (length(a) == 0) return(b)
    if (length(a) == 1 && is.character(a) && !nzchar(a)) return(b)
    a
}

read_delim_auto <- function(path) {
    ext <- tolower(tools::file_ext(path))
    if (ext %in% c("tsv", "txt")) {
        fread(path, sep="\t")
    } else {
        fread(path, sep=",")
    }
}

# Sanitize an arbitrary string into a safe Shiny input id fragment.
sanitize_key <- function(x) gsub("[^A-Za-z0-9]+", "_", x)

# ════════════════════════════════════════════════════════════════════════════
#  DE TABLES MODE
# ════════════════════════════════════════════════════════════════════════════

# ─── Reactive Data Stores ─────────────────────────────────────────────────────

FileStore  <- reactiveVal(list())        # named list: key -> data.frame
FileLabels <- reactiveVal(list())        # named list: key -> display label
FileKeys   <- reactiveVal(character(0))  # ordered keys for stable UI rendering

# ─── Multi-File Upload Handler ────────────────────────────────────────────────

observeEvent(input$de_files, {
    req(input$de_files)
    current_store  <- FileStore()
    current_labels <- FileLabels()
    current_keys   <- FileKeys()

    for (i in seq_len(nrow(input$de_files))) {
        fname <- tools::file_path_sans_ext(input$de_files$name[i])
        key   <- sanitize_key(fname)
        # Ensure unique keys
        if (key %in% current_keys) {
            key <- paste0(key, "_", i)
        }
        dat <- tryCatch(
            read_delim_auto(input$de_files$datapath[i]),
            error = function(e) {
                showNotification(paste("Error reading", input$de_files$name[i], ":", e$message),
                    type='error', duration=8)
                NULL
            }
        )
        if (!is.null(dat) && nrow(dat) > 0) {
            current_store[[key]]  <- dat
            current_labels[[key]] <- fname
            current_keys          <- c(current_keys, key)
        }
    }
    FileStore(current_store)
    FileLabels(current_labels)
    FileKeys(current_keys)
})

# ─── Demo Data Generation (DE tables) ─────────────────────────────────────────

generate_demo_data <- function() {
    set.seed(42)

    gene_universe <- paste0("Gene_", sprintf("%04d", 1:3000))
    n_genes <- length(gene_universe)

    # Core shared significant genes (appear in multiple comparisons)
    core_sig <- sample(gene_universe, 150)

    comp_names <- c("Treatment_A_vs_Control", "Treatment_B_vs_Control",
                    "Treatment_C_vs_Control", "Treatment_A_vs_B",
                    "TimePoint_24h_vs_0h")

    comparisons <- list()

    for (comp_name in comp_names) {
        # Each comparison gets 60-100% of core genes + 50-200 unique
        comp_core  <- sample(core_sig, floor(length(core_sig) * runif(1, 0.6, 1.0)))
        unique_sig <- sample(setdiff(gene_universe, core_sig), sample(50:200, 1))
        all_sig    <- c(comp_core, unique_sig)

        baseMean       <- rlnorm(n_genes, meanlog=6, sdlog=1.5)
        log2FoldChange <- numeric(n_genes)
        pvalue         <- numeric(n_genes)

        sig_idx    <- which(gene_universe %in% all_sig)
        nonsig_idx <- which(!gene_universe %in% all_sig)

        # Significant genes: strong fold changes, low p-values
        log2FoldChange[sig_idx] <- rnorm(length(sig_idx), mean=0, sd=2)
        # Ensure |lfc| > 0.5 for significant genes
        too_small <- sig_idx[abs(log2FoldChange[sig_idx]) < 0.5]
        log2FoldChange[too_small] <- sign(runif(length(too_small), -1, 1)) *
            runif(length(too_small), 1, 3)
        pvalue[sig_idx] <- 10^(-runif(length(sig_idx), 2, 10))

        # Non-significant genes: small fold changes, high p-values
        log2FoldChange[nonsig_idx] <- rnorm(length(nonsig_idx), mean=0, sd=0.3)
        pvalue[nonsig_idx] <- runif(length(nonsig_idx), 0.01, 1)

        padj  <- p.adjust(pvalue, method="BH")
        lfcSE <- abs(log2FoldChange / qnorm(pmax(1 - pvalue/2, 0.5001)))
        lfcSE[lfcSE < 0.01 | is.na(lfcSE) | is.infinite(lfcSE)] <- 0.1
        stat  <- log2FoldChange / lfcSE

        comparisons[[comp_name]] <- data.table(
            gene           = gene_universe,
            baseMean       = round(baseMean, 2),
            log2FoldChange = round(log2FoldChange, 4),
            lfcSE          = round(lfcSE, 4),
            stat           = round(stat, 4),
            pvalue         = signif(pvalue, 4),
            padj           = signif(padj, 4)
        )
    }
    comparisons
}

observeEvent(input$demo_submit, {
    demo <- generate_demo_data()
    orig_names <- names(demo)
    keys <- sanitize_key(orig_names)
    names(demo) <- keys
    FileStore(demo)
    FileLabels(as.list(setNames(orig_names, keys)))
    FileKeys(keys)
})

# ─── Column Mapping Selectors ─────────────────────────────────────────────────

output$column_selectors <- renderUI({
    files <- FileStore()
    req(length(files) > 0)
    cols <- colnames(files[[1]])

    # Smart guessing helper
    guess <- function(patterns, fallback_idx) {
        for (pat in patterns) {
            match <- grep(pat, cols, ignore.case=TRUE, value=TRUE)
            if (length(match) > 0) return(match[1])
        }
        if (fallback_idx <= length(cols)) cols[fallback_idx] else cols[1]
    }

    tagList(
        hr(),
        h4("Map Columns", style="color:#0F344C;"),
        selectInput("gene_col", "Gene ID column:", choices=cols,
            selected=guess(c("^gene$", "gene_id", "symbol", "ensembl"), 1)),
        selectInput("lfc_col", "log2FoldChange column:", choices=cols,
            selected=guess(c("log2FoldChange", "logFC", "lfc", "log2fc"), 2)),
        selectInput("padj_col", "padj column:", choices=cols,
            selected=guess(c("padj", "FDR", "adj\\.P", "q\\.value"), 3)),
        selectInput("pval_col", "pvalue column:", choices=cols,
            selected=guess(c("^pvalue$", "^pval$", "PValue", "p\\.value"), 4))
    )
})

# ─── File Management UI (editable labels) ─────────────────────────────────────

output$file_management_ui <- renderUI({
    keys   <- FileKeys()
    files  <- FileStore()
    labels <- FileLabels()
    if (length(keys) == 0) return(p("No files loaded yet.", style="color:#999;"))

    file_rows <- lapply(keys, function(key) {
        label <- labels[[key]] %||% key
        n_row <- nrow(files[[key]])
        n_col <- ncol(files[[key]])
        fluidRow(
            column(5, textInput(paste0("label_", key), label=NULL, value=label)),
            column(3, p(paste(n_row, "rows,", n_col, "cols"), style="margin-top:8px; color:#666;")),
            column(2, actionButton(paste0("remove_", key), label=NULL,
                icon=icon("trash"), class="btn btn-danger btn-sm",
                style="margin-top:4px;")),
            style="margin-bottom:2px;"
        )
    })
    do.call(tagList, file_rows)
})

# ─── Label Update Observer ────────────────────────────────────────────────────

observe({
    keys   <- FileKeys()
    labels <- isolate(FileLabels())
    changed <- FALSE
    for (key in keys) {
        input_id  <- paste0("label_", key)
        new_label <- input[[input_id]]
        if (!is.null(new_label) && nchar(trimws(new_label)) > 0 && new_label != labels[[key]]) {
            labels[[key]] <- new_label
            changed <- TRUE
        }
    }
    if (changed) FileLabels(labels)
})

# ─── File Removal Observers ──────────────────────────────────────────────────

observe({
    keys <- FileKeys()
    lapply(keys, function(key) {
        btn_id <- paste0("remove_", key)
        observeEvent(input[[btn_id]], {
            current_store  <- FileStore()
            current_labels <- FileLabels()
            current_keys   <- FileKeys()
            current_store[[key]]  <- NULL
            current_labels[[key]] <- NULL
            current_keys <- current_keys[current_keys != key]
            FileStore(current_store)
            FileLabels(current_labels)
            FileKeys(current_keys)
        }, ignoreInit=TRUE, once=TRUE)
    })
})

# ─── Data Preview Table (DE) ──────────────────────────────────────────────────

output$file_preview_table <- DT::renderDataTable({
    keys   <- FileKeys()
    files  <- FileStore()
    labels <- FileLabels()
    req(length(keys) > 0)

    # Show first file as preview
    first_key <- keys[1]
    df <- as.data.frame(files[[first_key]])
    DT::datatable(df,
        caption=paste("Preview:", labels[[first_key]] %||% first_key),
        style='bootstrap',
        options=list(pageLength=10, scrollX=TRUE),
        rownames=FALSE)
})

# ════════════════════════════════════════════════════════════════════════════
#  GENERIC MODE  (one wide table; each column is a set, rows are members)
# ════════════════════════════════════════════════════════════════════════════

GenericData   <- reactiveVal(NULL)            # the wide data.frame (original colnames)
GenericKeyMap <- reactiveVal(character(0))    # named vector: key -> original column name
GenericKeys   <- reactiveVal(character(0))    # ordered keys
GenericLabels <- reactiveVal(list())          # key -> display label (default = column name)

# ─── Single-Table Upload Handler ──────────────────────────────────────────────

observeEvent(input$generic_file, {
    req(input$generic_file)
    dat <- tryCatch(
        read_delim_auto(input$generic_file$datapath),
        error = function(e) {
            showNotification(paste("Error reading", input$generic_file$name, ":", e$message),
                type='error', duration=8)
            NULL
        }
    )
    req(!is.null(dat))
    dat  <- as.data.frame(dat)
    cols <- colnames(dat)
    keys <- make.unique(sanitize_key(cols), sep="_")

    GenericData(dat)
    GenericKeyMap(setNames(cols, keys))
    GenericKeys(keys)
    GenericLabels(setNames(as.list(cols), keys))
})

# ─── Per-Column (Set) Management UI: editable labels ──────────────────────────

# Count the non-empty members of one generic column.
generic_members <- function(df, col) {
    vals <- df[[col]]
    vals <- vals[!is.na(vals)]
    vals <- trimws(as.character(vals))
    vals <- vals[nzchar(vals) & !(tolower(vals) %in% c("na", "nan", "null"))]
    unique(vals)
}

output$generic_sets_ui <- renderUI({
    keys   <- GenericKeys()
    df     <- GenericData()
    keymap <- GenericKeyMap()
    labels <- GenericLabels()
    if (length(keys) == 0) return(p("No table loaded yet.", style="color:#999;"))

    set_rows <- lapply(keys, function(key) {
        col   <- keymap[[key]]
        label <- labels[[key]] %||% col
        n     <- length(generic_members(df, col))
        fluidRow(
            column(5, textInput(paste0("glabel_", key), label=NULL, value=label)),
            column(4, p(paste(n, "items"), style="margin-top:8px; color:#666;")),
            style="margin-bottom:2px;"
        )
    })
    do.call(tagList, set_rows)
})

# ─── Generic Label Update Observer ────────────────────────────────────────────

observe({
    keys   <- GenericKeys()
    labels <- isolate(GenericLabels())
    changed <- FALSE
    for (key in keys) {
        new_label <- input[[paste0("glabel_", key)]]
        if (!is.null(new_label) && nchar(trimws(new_label)) > 0 && new_label != labels[[key]]) {
            labels[[key]] <- new_label
            changed <- TRUE
        }
    }
    if (changed) GenericLabels(labels)
})

# ─── Data Preview Table (Generic) ─────────────────────────────────────────────

output$generic_preview_table <- DT::renderDataTable({
    df <- GenericData()
    req(!is.null(df))
    DT::datatable(df,
        caption="Preview: each column becomes a set; non-empty values are its members.",
        style='bootstrap',
        options=list(pageLength=10, scrollX=TRUE),
        rownames=FALSE)
})
