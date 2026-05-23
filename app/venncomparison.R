# ════════════════════════════════════════════════════════════════════════════
#  GENE SET DERIVATION  (two input modes feed one FilteredGeneSets dispatcher)
# ════════════════════════════════════════════════════════════════════════════

# ─── DE Tables Mode: filtered gene sets ───────────────────────────────────────

de_filtered_gene_sets <- reactive({
    files  <- FileStore()
    labels <- FileLabels()
    keys   <- FileKeys()
    req(length(keys) >= 2)

    # Determine which significance column and cutoff to use
    sig_type <- input$sig_column %||% "padj"
    if (sig_type == "pvalue") {
        sig_cut <- input$pval_cutoff %||% 0.01
    } else {
        sig_cut <- input$padj_cutoff %||% 0.05
    }
    lfc_cut   <- input$lfc_cutoff  %||% 1
    direction <- input$direction   %||% "both"

    # Determine column names (demo uses known defaults, custom uses mapped cols)
    gene_col <- input$gene_col %||% "gene"
    lfc_col  <- input$lfc_col  %||% "log2FoldChange"
    if (sig_type == "pvalue") {
        sig_col <- input$pval_col %||% "pvalue"
    } else {
        sig_col <- input$padj_col %||% "padj"
    }

    gene_sets <- list()

    for (key in keys) {
        df    <- as.data.frame(files[[key]])
        label <- labels[[key]] %||% key

        # Validate required columns exist
        if (!all(c(gene_col, lfc_col, sig_col) %in% colnames(df))) next

        # Filter by significance
        sig <- df[!is.na(df[[sig_col]]) & df[[sig_col]] < sig_cut, ]

        if (direction == "up") {
            sig <- sig[sig[[lfc_col]] > lfc_cut, ]
            gene_sets[[label]] <- unique(as.character(sig[[gene_col]]))

        } else if (direction == "down") {
            sig <- sig[sig[[lfc_col]] < -lfc_cut, ]
            gene_sets[[label]] <- unique(as.character(sig[[gene_col]]))

        } else if (direction == "both_sep") {
            up_sig   <- sig[sig[[lfc_col]] > lfc_cut, ]
            down_sig <- sig[sig[[lfc_col]] < -lfc_cut, ]
            gene_sets[[paste0(label, " (Up)")]]   <- unique(as.character(up_sig[[gene_col]]))
            gene_sets[[paste0(label, " (Down)")]] <- unique(as.character(down_sig[[gene_col]]))

        } else {  # "both" combined
            sig <- sig[abs(sig[[lfc_col]]) > lfc_cut, ]
            gene_sets[[label]] <- unique(as.character(sig[[gene_col]]))
        }
    }

    # Remove empty sets
    gene_sets <- gene_sets[sapply(gene_sets, length) > 0]
    validate(need(length(gene_sets) >= 2,
        "At least 2 non-empty gene sets are required after filtering. Try relaxing your cutoffs."))
    gene_sets
})

# ─── Generic Mode: one set per column, no filtering ───────────────────────────

generic_gene_sets <- reactive({
    df     <- GenericData()
    keys   <- GenericKeys()
    keymap <- GenericKeyMap()
    labels <- GenericLabels()
    req(!is.null(df), length(keys) >= 2)

    gene_sets <- list()
    for (key in keys) {
        col   <- keymap[[key]]
        label <- labels[[key]] %||% col
        vals  <- generic_members(df, col)
        if (length(vals) > 0) {
            # Guard against duplicate labels collapsing sets
            if (label %in% names(gene_sets)) label <- paste0(label, " (", key, ")")
            gene_sets[[label]] <- vals
        }
    }
    validate(need(length(gene_sets) >= 2,
        "At least 2 columns with non-empty values are required."))
    gene_sets
})

# ─── Dispatcher: the rest of the app consumes this ────────────────────────────

FilteredGeneSets <- reactive({
    if ((input$input_type %||% "de") == "generic") {
        generic_gene_sets()
    } else {
        de_filtered_gene_sets()
    }
})

# ─── Set Summary Panel ────────────────────────────────────────────────────────

output$set_summary_panel <- renderUI({
    gene_sets <- tryCatch(FilteredGeneSets(), error=function(e) NULL)
    if (is.null(gene_sets)) return(NULL)

    summary_items <- lapply(names(gene_sets), function(name) {
        tags$li(strong(name), paste0(": ", length(gene_sets[[name]]), " items"))
    })

    div(class="control-group-panel",
        p(class="control-group-title", icon("list"), " Set Item Counts"),
        tags$ul(style="padding-left:18px;", summary_items)
    )
})

# ════════════════════════════════════════════════════════════════════════════
#  PER-SET COLORS  (shared by both Venn and UpSet)
# ════════════════════════════════════════════════════════════════════════════

# Short display names for color picker labels
short_label <- function(nm) {
    nm <- gsub("_", " ", nm)
    if (nchar(nm) > 20) nm <- paste0(substr(nm, 1, 18), "..")
    nm
}

# Default qualitative palette of length n (Set1, ramped beyond 9)
set_default_palette <- function(n) {
    if (n <= 0) return(character(0))
    if (n <= 9) {
        RColorBrewer::brewer.pal(max(3, n), "Set1")[seq_len(n)]
    } else {
        colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))(n)
    }
}

# Which sets are currently "active": Venn caps at 5 (with selection); UpSet uses all
active_set_names <- reactive({
    gs <- tryCatch(FilteredGeneSets(), error=function(e) NULL)
    if (is.null(gs)) return(NULL)
    nm <- names(gs)
    if ((input$plot_type %||% "venn") == "venn" && length(nm) > 5) {
        if (!is.null(input$venn_selected_sets) && length(input$venn_selected_sets) >= 1) {
            nm <- input$venn_selected_sets
        }
        nm <- nm[seq_len(min(5, length(nm)))]
    }
    nm
})

# One color picker per active set, defaulted from the palette
output$set_color_pickers_ui <- renderUI({
    set_names <- active_set_names()
    if (is.null(set_names) || length(set_names) == 0) return(NULL)
    pal <- set_default_palette(length(set_names))
    pickers <- lapply(seq_along(set_names), function(i) {
        colourInput(paste0("setcol_", i), short_label(set_names[i]), value=pal[i])
    })
    do.call(tagList, pickers)
})

# Named color vector (set name -> hex), reading pickers, falling back to palette
set_colors <- reactive({
    set_names <- active_set_names()
    if (is.null(set_names) || length(set_names) == 0) return(NULL)
    pal <- set_default_palette(length(set_names))
    cols <- vapply(seq_along(set_names), function(i) {
        input[[paste0("setcol_", i)]] %||% pal[i]
    }, character(1))
    setNames(cols, set_names)
})

# ─── Venn Set Warning / Selector (>5 sets) ────────────────────────────────────

output$venn_set_warning <- renderUI({
    gene_sets <- tryCatch(FilteredGeneSets(), error=function(e) NULL)
    if (is.null(gene_sets)) return(NULL)
    n_sets <- length(gene_sets)

    if (n_sets > 5) {
        tagList(
            div(class="alert alert-warning", style="padding:8px; font-size:13px;",
                icon("exclamation-triangle"),
                paste("You have", n_sets, "sets. Venn diagrams support max 5.",
                      "Select up to 5 below.")),
            selectizeInput("venn_selected_sets", "Select sets (max 5):",
                choices=names(gene_sets),
                selected=names(gene_sets)[1:min(5, n_sets)],
                multiple=TRUE,
                options=list(maxItems=5))
        )
    }
})

# ─── Venn Custom Edge Color Pickers ───────────────────────────────────────────

output$venn_custom_edge_colors_ui <- renderUI({
    set_names <- active_set_names()
    if (is.null(set_names)) return(NULL)
    default_edge_colors <- c("#1B9E77","#D95F02","#7570B3","#E7298A","#66A61E")
    color_inputs <- lapply(seq_along(set_names), function(i) {
        colourInput(paste0("venn_edge_color_", i), short_label(set_names[i]),
            value=default_edge_colors[((i - 1) %% length(default_edge_colors)) + 1])
    })
    do.call(tagList, color_inputs)
})

# Blend member colors for an intersection region
blend_colors <- function(hex_colors) {
    if (length(hex_colors) == 1) return(hex_colors)
    rgb_vals <- col2rgb(hex_colors)
    avg_rgb  <- rowMeans(rgb_vals)
    rgb(avg_rgb[1], avg_rgb[2], avg_rgb[3], maxColorValue=255)
}

# ════════════════════════════════════════════════════════════════════════════
#  PLOTTERS
# ════════════════════════════════════════════════════════════════════════════

# ─── Venn Diagram ─────────────────────────────────────────────────────────────

venn_plotter <- reactive({
    gene_sets <- FilteredGeneSets()
    req(gene_sets, length(gene_sets) >= 2)

    # If >5 sets, use selected subset
    if (length(gene_sets) > 5) {
        req(input$venn_selected_sets)
        gene_sets <- gene_sets[input$venn_selected_sets]
        validate(need(length(gene_sets) >= 2 && length(gene_sets) <= 5,
                      "Select 2-5 sets for the Venn diagram."))
    }

    n_sets     <- length(gene_sets)
    color_mode <- input$venn_color_mode %||% "gradient"
    # Per-set colors take over the fill whenever the "Per-set Colors" switch is on
    use_setcolor <- isTRUE(input$venn_fill) && isTRUE(input$show_set_colors)

    # Capture per-set colors (by name) BEFORE wrapping the display names
    sc            <- set_colors()
    custom_colors <- if (!is.null(sc)) unname(sc[names(gene_sets)]) else set_default_palette(n_sets)
    custom_colors[is.na(custom_colors)] <- "#CCCCCC"

    # Per-set boundary colors
    edge_colors <- rep("black", n_sets)
    edge_size   <- 0.5
    if (isTRUE(input$venn_custom_edges)) {
        edge_colors <- vapply(seq_len(n_sets), function(i) {
            input[[paste0("venn_edge_color_", i)]] %||% "black"
        }, character(1))
        edge_size <- 1.5
    }

    # Wrap long set names for display
    wrap_width <- input$venn_label_wrap %||% 20
    wrapped_names <- sapply(names(gene_sets), function(nm) {
        nm_spaced <- gsub("_", " ", nm)
        paste(strwrap(nm_spaced, width=wrap_width), collapse="\n")
    })
    names(gene_sets) <- wrapped_names

    # Label mode
    show_counts <- isTRUE(input$venn_counts)
    show_pct    <- isTRUE(input$venn_pct)
    label_mode <- "none"
    if (show_counts && show_pct) label_mode <- "both"
    else if (show_counts) label_mode <- "count"
    else if (show_pct) label_mode <- "percent"

    label_size   <- input$venn_label_size   %||% 4
    setname_size <- input$venn_setname_size %||% 5
    opacity_val  <- input$venn_opacity      %||% 0.5

    # Per-set fill: build regions manually with blended colors
    if (use_setcolor) {
        tryCatch({
            venn_obj  <- Venn(gene_sets)
            venn_data <- process_data(venn_obj)

            region_edge_d <- venn_regionedge(venn_data)
            set_edge_d    <- venn_setedge(venn_data)
            setlabel_d    <- venn_setlabel(venn_data)
            regionlabel_d <- venn_regionlabel(venn_data)

            # Map each region (ids like "1", "1/2", "1/2/3") to a blended color
            region_color_map <- sapply(unique(as.character(region_edge_d$id)), function(rid) {
                set_indices <- as.integer(strsplit(rid, "/")[[1]])
                set_indices <- set_indices[!is.na(set_indices) & set_indices >= 1 & set_indices <= n_sets]
                if (length(set_indices) == 0) return("#FFFFFF")
                blend_colors(custom_colors[set_indices])
            })
            region_edge_d$fill_color <- region_color_map[as.character(region_edge_d$id)]

            p <- ggplot() +
                geom_polygon(data=region_edge_d,
                    aes(x=X, y=Y, group=id, fill=fill_color),
                    alpha=opacity_val, color=NA) +
                scale_fill_identity() +
                geom_path(data=set_edge_d,
                    aes(x=X, y=Y, group=id, color=factor(id)),
                    linewidth=edge_size) +
                scale_color_manual(values=setNames(edge_colors, as.character(1:n_sets)), guide="none") +
                geom_text(data=setlabel_d,
                    aes(x=X, y=Y, label=name),
                    size=setname_size, fontface="bold")

            total_genes <- sum(regionlabel_d$count)
            if (label_mode != "none") {
                if (label_mode == "count") {
                    regionlabel_d$lbl <- as.character(regionlabel_d$count)
                } else if (label_mode == "percent") {
                    regionlabel_d$lbl <- paste0(round(regionlabel_d$count / max(total_genes, 1) * 100, 1), "%")
                } else if (label_mode == "both") {
                    pcts <- round(regionlabel_d$count / max(total_genes, 1) * 100, 1)
                    regionlabel_d$lbl <- paste0(regionlabel_d$count, "\n(", pcts, "%)")
                }
                p <- p + geom_label(data=regionlabel_d,
                    aes(x=X, y=Y, label=lbl),
                    size=label_size, fill=NA, label.size=NA)
            }

            return(p + theme_void() + coord_equal())

        }, error = function(e) {
            validate(paste0("Per-set fill error: ", e$message))
        })
    }

    # Gradient / palette modes via ggVennDiagram convenience function
    p <- ggVennDiagram(gene_sets,
        label       = label_mode,
        label_alpha = 0,
        label_size  = label_size,
        set_size    = setname_size,
        edge_size   = edge_size,
        set_color   = edge_colors,
        label_percent_digit = 1)

    if (isTRUE(input$venn_fill)) {
        if (color_mode == "gradient") {
            grad_color <- input$venn_gradient_color %||% "#008cba"
            p <- p + scale_fill_gradient(low="white", high=grad_color, guide="none")
        } else if (color_mode == "palette") {
            pal_name <- input$venn_palette %||% "Blues"
            p <- p + scale_fill_distiller(palette=pal_name, guide="none")
        }
    } else {
        p <- p + scale_fill_gradient(low="white", high="white", guide="none")
    }

    p + theme_void()
})

# ─── UpSet Plot ───────────────────────────────────────────────────────────────

upset_plotter <- reactive({
    gene_sets <- FilteredGeneSets()
    req(gene_sets, length(gene_sets) >= 2)

    sort_by     <- input$upset_sort       %||% "freq"
    top_n       <- input$upset_top_n      %||% 40
    point_size  <- input$upset_point_size %||% 5
    line_size   <- input$upset_line_size  %||% 2
    bar_color   <- input$upset_bar_color  %||% "#3B3B3B"
    dot_color   <- input$upset_dot_color  %||% "#3B3B3B"
    show_empty  <- isTRUE(input$upset_empty)
    show_bars   <- isTRUE(input$upset_set_bars)

    # Per-set colors drive the set-size bars only when the "Per-set Colors" switch is on;
    # otherwise the set bars use the (uniform) intersection bar color.
    sc <- if (isTRUE(input$show_set_colors)) set_colors() else NULL
    set_bar_cols <- if (!is.null(sc)) unname(sc[names(gene_sets)]) else rep(bar_color, length(gene_sets))
    set_bar_cols[is.na(set_bar_cols)] <- bar_color

    upset(fromList(gene_sets),
        sets         = names(gene_sets),
        keep.order   = TRUE,
        order.by     = sort_by,
        nsets        = length(gene_sets),
        nintersects  = top_n,
        empty.intersections = if (show_empty) "on" else NULL,
        point.size   = point_size,
        line.size    = line_size,
        mainbar.y.label = "Intersection Size",
        sets.x.label    = "Set Size",
        show.numbers = "yes",
        mb.ratio     = c(0.6, 0.4),
        sets.bar.color = if (show_bars) set_bar_cols else NA,
        main.bar.color = bar_color,
        matrix.color   = dot_color,
        shade.color    = dot_color
    )
})

# ─── Plot Rendering ───────────────────────────────────────────────────────────

observe({
    output$comparison_plot_out <- renderPlot({
        if (input$plot_type == "venn") {
            venn_plotter()
        } else {
            upset_plotter()
        }
    }, height = input$plot_height %||% 700,
       width  = input$plot_width  %||% 900)
})

# ─── Intersection Table ───────────────────────────────────────────────────────

IntersectionData <- reactive({
    gene_sets <- FilteredGeneSets()
    req(gene_sets, length(gene_sets) >= 2)

    set_names <- names(gene_sets)
    all_items <- unique(unlist(gene_sets))
    if (length(all_items) == 0) return(data.frame(Intersection="None", Item_Count=0, Items=""))

    membership <- sapply(gene_sets, function(s) all_items %in% s)
    rownames(membership) <- all_items

    patterns <- apply(membership, 1, function(row) {
        members <- set_names[row]
        if (length(members) == 0) return("None")
        paste(sort(members), collapse=" & ")
    })

    pattern_list <- split(all_items, patterns)
    result <- data.frame(
        Intersection = names(pattern_list),
        Item_Count   = sapply(pattern_list, length),
        Items        = sapply(pattern_list, function(g) paste(sort(g), collapse=", ")),
        stringsAsFactors = FALSE
    )
    result <- result[order(-result$Item_Count), ]
    rownames(result) <- NULL
    result
})

output$intersection_table <- DT::renderDataTable({
    df <- IntersectionData()
    req(df)
    DT::datatable(df,
        style='bootstrap',
        rownames=FALSE,
        options=list(
            pageLength=20,
            scrollX=TRUE,
            columnDefs=list(
                list(targets=2, render=JS(
                    "function(data, type, row) {
                        if(type === 'display' && data && data.length > 100) {
                            return '<span title=\"' + data + '\">' +
                                   data.substr(0,100) + '...</span>';
                        }
                        return data;
                    }"
                ))
            )
        ),
        escape=FALSE)
})

# ─── Download: Intersection CSV ───────────────────────────────────────────────

output$download_intersections_csv <- downloadHandler(
    filename = function() { "intersections.csv" },
    content = function(file) {
        write.csv(IntersectionData(), file, row.names=FALSE)
    }
)

# ─── Download: Plot ───────────────────────────────────────────────────────────

output$download_plot <- downloadHandler(
    filename = function() {
        paste0("set_comparison.", input$download_format)
    },
    content = function(file) {
        fmt <- input$download_format
        h   <- input$plot_height %||% 700
        w   <- input$plot_width  %||% 900

        if (input$plot_type == "venn") {
            # ggplot-based: use ggsave
            p <- venn_plotter()
            ggsave(file, plot=p, device=fmt, height=h/96, width=w/96, units="in", dpi=150)
        } else {
            # UpSetR: base graphics, need device open/close
            if (fmt == "png") {
                png(file, height=h, width=w, res=150)
            } else if (fmt == "pdf") {
                pdf(file, height=h/96, width=w/96)
            } else if (fmt == "jpeg") {
                jpeg(file, height=h, width=w, res=150)
            } else if (fmt == "tiff") {
                tiff(file, height=h, width=w, res=150)
            } else if (fmt == "svg") {
                svg(file, height=h/96, width=w/96)
            } else if (fmt == "eps") {
                setEPS()
                postscript(file, height=h/96, width=w/96)
            }
            print(upset_plotter())
            dev.off()
        }
    }
)

# ─── Help Modal ───────────────────────────────────────────────────────────────

show_help_modal_ui <- function() {
    showModal(modalDialog(
        title = tagList(icon("circle-question"), " Set Comparison Tool Help"),
        size = "l", easyClose = TRUE, footer = modalButton("Close"),
        tabsetPanel(
            tabPanel("Overview",
                br(),
                h4("What is Set Comparison?"),
                p("This tool visualizes how lists overlap. Compare differentially expressed
                  gene lists across contrasts, or any generic lists (genes, proteins, GO terms,
                  metabolites, etc.) to see what is shared and what is unique."),
                h4("Two input modes"),
                tags$ul(
                    tags$li(strong("Generic lists:"), " upload one wide table where each column
                        is a set and the rows are its members. Empty cells are ignored, no
                        filtering is applied."),
                    tags$li(strong("DE tables:"), " upload one or more differential-expression
                        results tables. Map the gene / log2FC / p-value columns and filter by
                        significance, fold change, and direction to define each set.")
                ),
                h4("Workflow"),
                tags$ol(
                    tags$li("Pick an input mode on the Data Input tab and load your data (or demo)."),
                    tags$li("Rename sets inline if you like."),
                    tags$li("Choose Venn Diagram or UpSet Plot and adjust colors/options."),
                    tags$li("Explore the intersection table and download plots and lists.")
                )
            ),
            tabPanel("Venn Diagrams",
                br(),
                h4("About Venn Diagrams"),
                p("Venn diagrams show overlapping circular regions for each set. Best for 2-5 sets."),
                tags$ul(
                    tags$li(strong("Fill mode:"), " gradient, a Brewer palette, or per-set colors."),
                    tags$li(strong("Per-set colors:"), " each set gets its own color; intersection
                        regions blend the colors of their member sets."),
                    tags$li(strong("Opacity / counts / percentages / fonts / label wrap:"), " fine-tune appearance."),
                    tags$li(strong("Custom boundary colors:"), " set per-set circle outline colors.")
                ),
                p(strong("Limit:"), " max 5 sets — if you have more, pick which 5 to display.")
            ),
            tabPanel("UpSet Plots",
                br(),
                h4("About UpSet Plots"),
                p("UpSet plots show intersections as a dot matrix with bars for intersection size,
                  and scale to any number of sets."),
                tags$ul(
                    tags$li(strong("Min intersection size / Top N / Sort:"), " control which intersections show."),
                    tags$li(strong("Per-set colors:"), " color the set-size bars per set."),
                    tags$li(strong("Bar / dot colors, point & line size, set size bars:"), " style the matrix.")
                )
            ),
            tabPanel("Controls",
                br(),
                h4("DE filter controls (DE tables mode only)"),
                tags$ul(
                    tags$li(strong("Significance column:"), " toggle between padj and pvalue."),
                    tags$li(strong("Cutoff:"), " features below this significance threshold pass."),
                    tags$li(strong("abs(log2FC) cutoff:"), " features above this absolute fold change pass."),
                    tags$li(strong("Direction:"), " up only, down only, both separately (two sets per
                        comparison), or both combined.")
                ),
                h4("Download"),
                p("Download the current plot as PNG, PDF, JPEG, TIFF, SVG, or EPS, and the
                  intersection lists as CSV.")
            )
        )
    ))
}

observeEvent(input$show_help_float, { show_help_modal_ui() })
