library(shiny)
require(shinyjs)
library(shinythemes)
require(shinycssloaders)
library(shinyWidgets)

library(DT)
library(tidyverse)
library(data.table)
library(colourpicker)
library(RColorBrewer)
library(ggVennDiagram)
library(UpSetR)

tagList(
    tags$head(
        includeHTML(("www/GA.html")),
        tags$style(type = 'text/css','.navbar-brand{display:none;}'),
        tags$style(HTML("
            .control-group-panel {
                border: 1px solid #ddd;
                border-radius: 6px;
                padding: 10px 12px;
                margin-bottom: 10px;
                background-color: #f9f9f9;
            }
            .control-group-title {
                font-weight: bold;
                font-size: 14px;
                color: #0F344C;
                margin-bottom: 8px;
            }
            #show_help_float {
                position: fixed;
                bottom: 28px;
                right: 28px;
                z-index: 9999;
                border-radius: 50%;
                width: 46px;
                height: 46px;
                font-size: 20px;
                padding: 0;
                box-shadow: 0 3px 8px rgba(0,0,0,0.25);
            }
        "))
    ),
    ## Global floating help button
    actionButton("show_help_float", label=NULL,
        icon=icon("circle-question"),
        title="Help & documentation",
        class="btn btn-info"
    ),
    fluidPage(theme = shinytheme('yeti'),
            windowTitle = "MaGIC Set Comparison Tool",
            useShinyjs(),
            titlePanel(
                fluidRow(
                column(2, tags$a(href='http://www.bioinformagic.io/', tags$img(height=75, src="MaGIC_Icon_0f344c.svg")), align='center'),
                column(10, fluidRow(
                    column(10, h1(strong('MaGIC Set Comparison Tool'), align='center', style="color:#0F344C;"))
                ))
                ),
                windowTitle = "MaGIC Set Comparison Tool"),
                tags$style(type='text/css', '.navbar{font-size:20px;}'),
                tags$style(type='text/css', '.nav-tabs{padding-bottom:20px;}'),
                tags$style(type='text/css', '.navbar-default{background-color:#0F344C;}'),
                tags$style(type='text/css', HTML('.navbar { background-color: #0F344C;}
                          .tab-panel{ background-color: #0F344C;}
                          .navbar-default .navbar-nav > .active > a,
                           .navbar-default .navbar-nav > .active > a:focus,
                           .navbar-default .navbar-nav > .active > a:hover {
                                color: white;
                                background-color: #008cba;
                            }')
                          ),
                tags$head(tags$style(".modal-dialog{ width:1300px}")),

        navbarPage(title="", id='NAVTABS',

        ## Introduction Tab
##########################################################################################################################################################
            tabPanel('Introduction',
                fluidRow(
                    column(2),
                    column(8,
                        column(12, align="center",
                            style="margin-bottom:25px;",
                            h3(markdown("Welcome to the **Set Comparison Tool** by the
                            [Molecular and Genomics Informatics Core (MaGIC)](http://www.bioinformagic.io)."))),
                        hr(),
                        h4(strong("How to Use This Tool"), style="color:#0F344C;"),
                        tags$ol(
                            tags$li("Go to the ", strong("Data Input"), " tab and choose an input mode."),
                            tags$li(strong("Generic lists:"), " upload one wide table where each column is a set."),
                            tags$li(strong("DE tables:"), " upload one or more DE results tables (or load the demo)."),
                            tags$li("Optionally rename each set using the editable label fields."),
                            tags$li("For DE tables, map your columns and set significance / fold-change filters."),
                            tags$li("Switch between Venn Diagram and UpSet Plot on the ", strong("Set Comparison"), " tab."),
                            tags$li("Explore the intersection table and download results.")
                        ),
                        hr(),
                        h4(strong("Two Input Modes"), style="color:#0F344C;"),
                        fluidRow(
                            column(6,
                                h5(strong("Generic Lists")),
                                tags$ul(
                                    tags$li("One wide table (CSV/TSV); each ", strong("column"), " is a set."),
                                    tags$li("Rows are list items — genes, proteins, GO terms, anything."),
                                    tags$li("Empty cells / NAs are ignored; no filtering is applied."),
                                    tags$li("Column headers become set labels (editable).")
                                )
                            ),
                            column(6,
                                h5(strong("DE Tables")),
                                tags$ul(
                                    tags$li("One or more differential-expression results tables."),
                                    tags$li("Each file is one comparison/contrast (one set)."),
                                    tags$li("Map gene ID / log2FC / p-value / padj columns."),
                                    tags$li("Filter by significance, fold change, and direction.")
                                )
                            )
                        ),
                        hr(),
                        h4(strong("Venn Diagrams vs UpSet Plots"), style="color:#0F344C;"),
                        fluidRow(
                            column(6,
                                h5(strong("Venn Diagrams")),
                                tags$ul(
                                    tags$li("Best for ", strong("2 to 5"), " sets."),
                                    tags$li("Shows exact overlap regions as circles."),
                                    tags$li("Intuitive for small numbers of comparisons."),
                                    tags$li("Becomes cluttered with more than 5 sets.")
                                )
                            ),
                            column(6,
                                h5(strong("UpSet Plots")),
                                tags$ul(
                                    tags$li("Ideal for ", strong("any number"), " of sets (especially >5)."),
                                    tags$li("Displays intersections as a matrix with size bars."),
                                    tags$li("Clearly shows the largest / most complex intersections."),
                                    tags$li("Scales well to dozens of comparisons.")
                                )
                            )
                        ),
                        hr()
                    ),
                    column(2)
                )
            ),

        ## Data Input Tab
##########################################################################################################################################################
            tabPanel('Data Input',
                fluidRow(
                    column(3,
                        wellPanel(
                            h2('Input Data', align='center', style="color:#0F344C;"),
                            radioGroupButtons("input_type", label="Input type:",
                                choices=c("DE tables"="de", "Generic lists"="generic"),
                                selected="de", justified=TRUE, status="info"),
                            hr(),

                            ## ── DE TABLES MODE ──
                            conditionalPanel("input.input_type == 'de'",
                                materialSwitch("DemoData", label="Upload custom data",
                                    value=FALSE, right=TRUE, status='info'),
                                conditionalPanel("input.DemoData",
                                    fileInput('de_files', 'Upload DE Results Tables',
                                        accept=c('.csv','.tsv','.txt'),
                                        multiple=TRUE),
                                    uiOutput('column_selectors'),
                                    actionButton('submit', 'Submit Data',
                                        class='btn btn-info btn-block',
                                        icon=icon('check'))
                                ),
                                conditionalPanel("!input.DemoData",
                                    p("Load synthetic demo data with 5 DE comparisons
                                      featuring partially overlapping significant gene sets."),
                                    actionButton('demo_submit', 'Load Demo Data',
                                        class='btn btn-success btn-block',
                                        icon=icon('flask'))
                                )
                            ),

                            ## ── GENERIC LISTS MODE ──
                            conditionalPanel("input.input_type == 'generic'",
                                fileInput('generic_file', 'Upload Wide Table (one column per set)',
                                    accept=c('.csv','.tsv','.txt'),
                                    multiple=FALSE),
                                p("Each column becomes a set; its non-empty values are the members.
                                  Empty cells and NAs are ignored."),
                                actionButton('generic_submit', 'Submit Data',
                                    class='btn btn-info btn-block',
                                    icon=icon('check'))
                            )
                        )
                    ),
                    column(9,
                        ## DE mode management + preview
                        conditionalPanel("input.input_type == 'de'",
                            h4("Uploaded Files", style="color:#0F344C;"),
                            uiOutput('file_management_ui'),
                            hr(),
                            h4("Data Preview", style="color:#0F344C;"),
                            withSpinner(DT::dataTableOutput('file_preview_table'))
                        ),
                        ## Generic mode management + preview
                        conditionalPanel("input.input_type == 'generic'",
                            h4("Sets (columns)", style="color:#0F344C;"),
                            uiOutput('generic_sets_ui'),
                            hr(),
                            h4("Data Preview", style="color:#0F344C;"),
                            withSpinner(DT::dataTableOutput('generic_preview_table'))
                        )
                    )
                )
            ),

        ## Set Comparison Tab
##########################################################################################################################################################
            tabPanel('Set Comparison',
                fluidRow(
                    column(3,
                        wellPanel(
                            ## Plot Type Toggle (always visible at top)
                            radioGroupButtons("plot_type", label=NULL,
                                choices=c("Venn Diagram"="venn", "UpSet Plot"="upset"),
                                selected="venn", justified=TRUE, status="info",
                                size="normal"),
                            hr(),

                            ## Filter Controls (DE tables mode only)
                            conditionalPanel("input.input_type == 'de'",
                                materialSwitch("show_filters", label="Filter Controls",
                                    value=TRUE, right=TRUE, status='info'),
                                conditionalPanel("input.show_filters",
                                    div(class="control-group-panel",
                                        radioButtons("sig_column", "Significance column:",
                                            choices=c("padj"="padj", "pvalue"="pvalue"),
                                            selected="padj", inline=TRUE),
                                        conditionalPanel("input.sig_column == 'padj'",
                                            sliderInput("padj_cutoff", "padj cutoff:",
                                                min=0.001, max=0.1, step=0.001, value=0.05)
                                        ),
                                        conditionalPanel("input.sig_column == 'pvalue'",
                                            sliderInput("pval_cutoff", "pvalue cutoff:",
                                                min=0.0001, max=0.05, step=0.0001, value=0.01)
                                        ),
                                        sliderInput("lfc_cutoff", "abs(log2FC) cutoff:",
                                            min=0, max=5, step=0.25, value=1),
                                        radioButtons("direction", "Direction:",
                                            choices=c("Up-regulated only"="up",
                                                      "Down-regulated only"="down",
                                                      "Both (separately)"="both_sep",
                                                      "Both (combined)"="both"),
                                            selected="both")
                                    )
                                )
                            ),

                            ## Dynamic set summary
                            uiOutput("set_summary_panel"),
                            hr(),

                            ## Per-set Colors (shared by Venn + UpSet)
                            materialSwitch("show_set_colors", label="Per-set Colors",
                                value=FALSE, right=TRUE, status='info'),
                            conditionalPanel("input.show_set_colors",
                                div(class="control-group-panel",
                                    p("One color per set. When on, these drive the Venn fills (intersection
                                      regions blend their member colors) and the UpSet set-size bars.",
                                      style="font-size:12px; color:#666;"),
                                    uiOutput("set_color_pickers_ui")
                                )
                            ),

                            ## Venn Options
                            conditionalPanel("input.plot_type == 'venn'",
                                ## Always visible in Venn mode: max-5-set selector when needed
                                uiOutput("venn_set_warning"),
                                materialSwitch("show_venn_colors", label="Venn Fill Options",
                                    value=FALSE, right=TRUE, status='info'),
                                conditionalPanel("input.show_venn_colors",
                                    div(class="control-group-panel",
                                        materialSwitch("venn_fill", label="Fill colors",
                                            value=TRUE, right=TRUE, status='info'),
                                        conditionalPanel("input.venn_fill",
                                            sliderInput("venn_opacity", "Fill opacity:",
                                                min=0, max=1, step=0.05, value=0.5),
                                            ## When Per-set Colors is on, the Venn uses them automatically
                                            conditionalPanel("input.show_set_colors",
                                                p("Using ", strong("Per-set Colors"), " (set above) for fills; ",
                                                  "intersection regions blend their member colors. Turn off ",
                                                  strong("Per-set Colors"), " to use a gradient or palette instead.",
                                                  style="font-size:12px; color:#666;")
                                            ),
                                            conditionalPanel("!input.show_set_colors",
                                                radioButtons("venn_color_mode", "Region fill:",
                                                    choices=c("Gradient"="gradient", "Palette"="palette"),
                                                    selected="gradient", inline=TRUE),
                                                conditionalPanel("input.venn_color_mode == 'gradient'",
                                                    colourInput("venn_gradient_color", "Gradient color:", value="#008cba")
                                                ),
                                                conditionalPanel("input.venn_color_mode == 'palette'",
                                                    selectInput("venn_palette", "Color palette:",
                                                        choices=c("Blues","Reds","Greens","Purples","Oranges",
                                                                  "YlOrRd","YlGnBu","RdYlBu","Spectral","BuPu"),
                                                        selected="Blues")
                                                )
                                            )
                                        ),
                                        materialSwitch("venn_custom_edges", label="Custom boundary colors",
                                            value=FALSE, right=TRUE, status='info'),
                                        conditionalPanel("input.venn_custom_edges",
                                            uiOutput("venn_custom_edge_colors_ui")
                                        )
                                    )
                                ),

                                ## Venn Font & Label Options
                                materialSwitch("show_venn_fonts", label="Venn Font & Labels",
                                    value=FALSE, right=TRUE, status='info'),
                                conditionalPanel("input.show_venn_fonts",
                                    div(class="control-group-panel",
                                        materialSwitch("venn_pct", label="Show percentages",
                                            value=FALSE, right=TRUE, status='info'),
                                        materialSwitch("venn_counts", label="Show counts",
                                            value=TRUE, right=TRUE, status='info'),
                                        sliderInput("venn_label_size", "Count/pct font size:",
                                            min=2, max=20, step=1, value=4),
                                        sliderInput("venn_setname_size", "Set name font size:",
                                            min=2, max=20, step=1, value=5),
                                        sliderInput("venn_label_wrap", "Label wrap width (chars):",
                                            min=5, max=50, step=1, value=20)
                                    )
                                )
                            ),

                            ## UpSet-specific controls
                            conditionalPanel("input.plot_type == 'upset'",
                                materialSwitch("show_upset_opts", label="UpSet Plot Options",
                                    value=FALSE, right=TRUE, status='info'),
                                conditionalPanel("input.show_upset_opts",
                                    div(class="control-group-panel",
                                        p("Set-size bars use Per-set Colors when that option (above) is on;
                                          otherwise the intersection bar color.",
                                          style="font-size:12px; color:#666;"),
                                        sliderInput("upset_min_size", "Min intersection size:",
                                            min=0, max=50, step=1, value=1),
                                        sliderInput("upset_top_n", "Top N intersections:",
                                            min=5, max=100, step=5, value=40),
                                        radioButtons("upset_sort", "Sort by:",
                                            choices=c("Frequency"="freq", "Degree"="degree"),
                                            selected="freq", inline=TRUE),
                                        materialSwitch("upset_empty", label="Show empty intersections",
                                            value=FALSE, right=TRUE, status='info'),
                                        colourInput("upset_bar_color", "Intersection bar color:", value="#3B3B3B"),
                                        colourInput("upset_dot_color", "Matrix dot color:", value="#3B3B3B"),
                                        sliderInput("upset_point_size", "Matrix dot size:",
                                            min=1, max=10, step=0.5, value=5),
                                        sliderInput("upset_line_size", "Matrix line size:",
                                            min=0.5, max=5, step=0.5, value=2),
                                        materialSwitch("upset_set_bars", label="Show set size bars",
                                            value=TRUE, right=TRUE, status='info')
                                    )
                                )
                            ),

                            ## Resize controls
                            materialSwitch("show_resize", label="Resize Plot",
                                value=FALSE, right=TRUE, status='info'),
                            conditionalPanel("input.show_resize",
                                sliderInput("plot_height", "Plot height (px):",
                                    min=200, max=2000, step=50, value=700),
                                sliderInput("plot_width", "Plot width (px):",
                                    min=200, max=2000, step=50, value=900)
                            )
                        )
                    ),
                    column(9,
                        tabsetPanel(id='ComparisonTabs',
                            tabPanel(title='Plot',
                                hr(),
                                withSpinner(type=6, color='#5bc0de',
                                    plotOutput("comparison_plot_out", height='100%')
                                ),
                                div(style="margin-top:30px; text-align:center; padding-bottom:50px;",
                                    div(style="display:inline-block; width:250px; margin-bottom:10px;",
                                        selectInput("download_format", "Download format:",
                                            choices=c('png','pdf','svg','tiff','jpeg','eps'))
                                    ),
                                    br(),
                                    downloadButton('download_plot', 'Download Plot')
                                )
                            ),
                            tabPanel(title='Intersection Table',
                                hr(),
                                withSpinner(DT::dataTableOutput('intersection_table')),
                                br(),
                                downloadButton('download_intersections_csv',
                                    'Download Intersections (CSV)',
                                    class='btn btn-info')
                            )
                        )
                    )
                )
            ),

        ## Footer
##########################################################################################################################################################
            tags$footer(
                wellPanel(
                    fluidRow(
                        column(4, align='center',
                        tags$a(href="https://github.com/MaGIC-Analytics/magic-venncomparison", icon("github", "fa-3x")),
                        tags$h4('GitHub to submit issues/requests')
                        ),
                        column(4, align='center',
                        tags$a(href="http://www.bioinformagic.io/", icon("magic", "fa-3x")),
                        tags$h4('MaGIC Home Page')
                        ),
                        column(4, align='center',
                        tags$a(href="https://github.com/MaGIC-Analytics", icon("address-card", "fa-3x")),
                        tags$h4("Developer's Page")
                        )
                    ),
                    fluidRow(
                        column(12, align='center',
                            HTML('<p>&copy;
                                <script language="javascript" type="text/javascript">
                                var today = new Date()
                                var year = today.getFullYear()
                                document.write(year)
                                </script>
                            </p>
                            ')
                        )
                    )
                )
            )
        )#Ends navbarPage
    )#Ends fluidpage
)#Ends tagList
