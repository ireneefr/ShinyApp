
# Upload app to shinyapps.io
# library(rsconnect)
# deployApp("/mnt/ElRaid/ifernandez/R/shinyepico_input")

# AVAILABLE METHODS
norm_options <- c(
    "Raw",
    "Illumina",
    "Funnorm",
    "Noob",
    "SWAN",
    "Quantile",
    "Noob+Quantile"
)

hclust_methods <- c(
    "single",
    "complete",
    "average",
    "mcquitty",
    "median",
    "centroid"
)


# Load packages
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyWidgets)
library(dplyr)


# Define UI for application that draws a histogram
shinyUI(dashboardPage( 
    
    # HEADER
    dashboardHeader(title = "SHINY APP", titleWidth = 200), #title
    
    # SIDEBAR
    dashboardSidebar( width = 200, 
        
        # MENU
        sidebarMenu(id = "menu", 
            menuItem("Input", tabName = "input", icon = icon("table")), #input
            menuItem("Normalization", tabName = "normalization", icon = icon("bar-chart-o")),
            menuItem("DMPS", tabName = "dmps", icon = icon("signal")),
            menuItem("DMRS", tabName = "dmrs", icon = icon("signal"))) #plot
    ),
    
    # BODY
    dashboardBody(
        
        # SECTIONS
        tabItems(
        
        # INPUT 
        tabItem(tabName = "input",
            fluidRow( shinythemes::themeSelector(),

              # Box 1: Upload file
              sidebarPanel(width = 12, 
                           fileInput("fileinput_input",
                                     "Introducir input",
                                     multiple = FALSE,
                                     accept = ".zip"
                           ), 
                           uiOutput("ui_button_input_load")),
              
              # Box 3: Select input
              conditionalPanel("typeof output.samples_table != 'undefined'",
                               box(width = 3,
                               selectInput("select_input_samplenamevar", "", c()),
                               selectInput("select_input_groupingvar", "", c()),
                               selectInput("select_input_donorvar", "", c()),
                               pickerInput(
                                   inputId = "selected_samples",
                                   label = "",
                                   choices = c(),
                                   options = list(
                                       `actions-box` = TRUE,
                                       size = 10,
                                       `selected-text-format` = "count > 3"
                                   ),
                                   multiple = TRUE
                               ),
                               actionButton("button_input_next", "Continue"))),
              
              # Box 2: Table
              box(width = 9,
                  withSpinner(DT::DTOutput("samples_table")))
              
            )
        ),
        
        # NORMALIZATION
        tabItem(tabName = "normalization", 
            fluidPage(
                    # Box1
                    sidebarPanel(width = 3,
                        selectInput("select_minfi_norm", "Select Normalization", norm_options),
                        div(
                            margin_left = "50px",
                            switchInput(
                                inputId = "select_minfi_dropcphs",
                                label = "Drop CpHs",
                                labelWidth = "fit",
                                value = TRUE,
                                inline = TRUE
                            ),
                            
                            switchInput(
                                inputId = "select_minfi_dropsnps",
                                label = "Drop SNPs",
                                labelWidth = "fit",
                                value = TRUE,
                                inline = TRUE
                            )
                        ),
                        
                        conditionalPanel(
                            "input.select_minfi_dropsnps",
                            sliderInput(
                                inputId = "slider_minfi_maf",
                                label = "Minimum MAF to filter",
                                min = 0,
                                max = 1,
                                step = 0.01,
                                value = 0,
                                width = "75%"
                            )
                        ),
                        
                        switchInput(
                            inputId = "select_minfi_chromosomes",
                            label = "Drop X/Y Chr.",
                            labelWidth = "fit",
                            value = FALSE
                        ),
                        
                        shinyjs::disabled(actionButton("button_minfi_select", "Select")),
                        h4(),
                        textOutput("text_minfi_probes")
                    ),
                    
                    
                    mainPanel(
                        width = 9,
                        tabsetPanel(
                            tabPanel(
                                "Quality Control",
                                h4("Overall Signal"),
                                withSpinner(plotly::plotlyOutput("graph_minfi_qcraw")),
                                h4("Bisulfite Conversion II"),
                                withSpinner(plotly::plotlyOutput("graph_minfi_bisulfiterawII"))
                            ),
                            
                            tabPanel(
                                "Density plot",
                                h4("Raw"),
                                withSpinner(plotly::plotlyOutput("graph_minfi_densityplotraw")),
                                h4("Processed"),
                                withSpinner(plotly::plotlyOutput("graph_minfi_densityplot"))
                            ),
                            
                            tabPanel(
                                "Boxplot",
                                h4("Raw"),
                                withSpinner(plotOutput("graph_minfi_boxplotraw")),
                                h4("Processed"),
                                withSpinner(plotOutput("graph_minfi_boxplot"))
                            ),
                            
                            tabPanel(
                                "SNPs Heatmap",
                                h4("SNPs beta-values (Raw)"),
                                withSpinner(plotly::plotlyOutput("graph_minfi_snps"))
                            ),
                            
                            tabPanel(
                                "Sex prediction",
                                h4("X vs Y chromosomes signal plot"),
                                withSpinner(plotly::plotlyOutput("graph_minfi_sex")),
                                withSpinner(DT::DTOutput("table_minfi_sex"))
                            ),
                            
                            tabPanel(
                                "Principal Component Analysis",
                                h4("Processed"),
                                withSpinner(plotly::plotlyOutput("graph_minfi_pcaplot")),
                                withSpinner(DT::DTOutput("table_minfi_pcaplot")),
                                column(
                                    6,
                                    selectInput(
                                        inputId = "select_minfi_pcaplot_pcx",
                                        choices = c(),
                                        label = "Select x variable"
                                    ),
                                    
                                    selectInput(
                                        inputId = "select_minfi_pcaplot_color",
                                        choices = c(),
                                        label = "Select color variable"
                                    )
                                ),
                                column(
                                    6,
                                    selectInput(
                                        inputId = "select_minfi_pcaplot_pcy",
                                        choices = c(),
                                        label = "Select y variable"
                                    )
                                ),
                                actionButton("button_pca_update", "Update")
                            ),
                            
                            tabPanel(
                                "Correlations",
                                h4("Processed"),
                                withSpinner(plotly::plotlyOutput("graph_minfi_corrplot")),
                                selectInput("select_minfi_typecorrplot", "Select data to plot", choices = c("p.value", "correlation value"), selected = "correlation value"),
                                withSpinner(DT::DTOutput("table_minfi_corrplot"))
                            )
                        )
                    )
            )
        ),
        
        # DMPS
        tabItem(tabName = "dmps", 
                fluidPage(
                        # Box1
                    sidebarPanel(
                        width = 3,
                        h4("Linear Model Options"),
                        
                        pickerInput(
                            inputId = "select_limma_voi",
                            label = "Select Variable of Interest",
                            choices = c(),
                            multiple = FALSE
                        ),
                        
                        pickerInput(
                            inputId = "checkbox_limma_covariables",
                            label = "Select linear model covariables",
                            choices = c(),
                            multiple = TRUE,
                            options = list(
                                `actions-box` = TRUE,
                                size = 10,
                                `selected-text-format` = "count > 3"
                            )
                        ),
                        
                        pickerInput(
                            inputId = "checkbox_limma_interactions",
                            label = "Select linear model interactions",
                            choices = c(),
                            multiple = TRUE,
                            options = list(
                                `actions-box` = TRUE,
                                size = 10,
                                `selected-text-format` = "count > 3"
                            )
                        ),
                        
                        switchInput(
                            inputId = "select_limma_weights",
                            label = "Array Weights",
                            labelWidth = "80px",
                            value = FALSE
                        ),
                        
                        
                        shinyjs::disabled(
                            actionButton("button_limma_calculatemodel", "Generate Model")
                        ),
                        tags$br(),
                        uiOutput("button_limma_calculatedifs_container")
                    ),
                    mainPanel(
                        width = 9,
                        tabsetPanel(
                            id = "tabset_limma",
                            tabPanel(
                                "Model diagnosis",
                                value = "model_diagnosis",
                                h4("Sigma vs A plot"),
                                plotOutput("graph_limma_plotSA") %>% shinycssloaders::withSpinner(),
                                h4("Design matrix"),
                                DT::DTOutput("table_limma_design") %>% shinycssloaders::withSpinner()
                            ),
                            tabPanel(
                                "Heatmap",
                                value = "differential_cpgs",
                                div(
                                    style = "max-width:800px;margin:auto;",
                                    fluidPage(
                                        h4("Heatmap"),
                                        textOutput("text_limma_heatmapcount"),
                                        uiOutput("graph_limma_heatmapcontainer"),
                                        h4("DMP counts in each contrast"),
                                        tableOutput("table_limma_difcpgs") %>% shinycssloaders::withSpinner(),
                                        
                                        fluidRow(
                                            column(
                                                6,
                                                h4("Group options"),
                                                
                                                selectizeInput(
                                                    "select_limma_groups2plot",
                                                    "Groups to plot",
                                                    c(),
                                                    multiple = TRUE,
                                                    options = list(plugins = list("remove_button", "drag_drop"))
                                                ),
                                                
                                                selectizeInput(
                                                    "select_limma_contrasts2plot",
                                                    "Contrasts to plot",
                                                    c(),
                                                    multiple = TRUE,
                                                    options = list(plugins = list("remove_button", "drag_drop"))
                                                ),
                                                
                                                h4("Data options"),
                                                
                                                switchInput(
                                                    inputId = "select_limma_removebatch",
                                                    label = "Remove Batch Effect",
                                                    labelWidth = "100px",
                                                    value = FALSE,
                                                    disabled = TRUE
                                                ),
                                            ),
                                            
                                            column(
                                                6,
                                                h4("Filtering options"),
                                                sliderInput("slider_limma_deltab", "Min. DeltaBeta", 0, 1, 0.2),
                                                sliderInput("slider_limma_adjpvalue", "Max. FDR", 0, 1, 0.05),
                                                sliderInput("slider_limma_pvalue", "Max. p-value", 0, 1, 1)
                                            )
                                        ),
                                        
                                        h4("Clustering options",
                                           align =
                                               "left"
                                        ),
                                        
                                        fluidRow(
                                            column(
                                                5,
                                                selectInput(
                                                    "select_limma_clusteralg",
                                                    "Clustering algorithm",
                                                    c(
                                                        "single",
                                                        "complete",
                                                        "average",
                                                        "mcquitty",
                                                        "median",
                                                        "centroid"
                                                    ),
                                                    "average"
                                                ),
                                                
                                                selectInput(
                                                    "select_limma_clusterdist",
                                                    "Distance Function",
                                                    c("pearson", "spearman", "kendall", "euclidean"),
                                                    "pearson"
                                                ),
                                                
                                                selectInput("select_limma_scale", "Scale", c("row", "none"), "row"),
                                                tags$br()
                                            ),
                                            
                                            column(
                                                3,
                                                offset = 1,
                                                tags$br(),
                                                
                                                switchInput(
                                                    inputId = "select_limma_graphstatic",
                                                    label = "Static Graph",
                                                    labelWidth = "100px",
                                                    value = TRUE
                                                ),
                                                
                                                switchInput(
                                                    inputId = "select_limma_colv",
                                                    label = "Column Dendro.",
                                                    labelWidth = "100px",
                                                    value = TRUE
                                                ),
                                                
                                                switchInput(
                                                    inputId = "select_limma_colsidecolors",
                                                    label = "Column Colors",
                                                    labelWidth = "100px",
                                                    value = FALSE
                                                )
                                            ),
                                            
                                            column(
                                                3,
                                                
                                                tags$br(),
                                                
                                                switchInput(
                                                    inputId = "select_limma_rowsidecolors",
                                                    label = "Row Colors",
                                                    labelWidth = "100px",
                                                    value = FALSE
                                                ),
                                                
                                                conditionalPanel(
                                                    "input.select_limma_rowsidecolors",
                                                    numericInput(
                                                        "select_limma_knumber",
                                                        "Clusters number",
                                                        value = 2,
                                                        min = 1,
                                                        max = Inf,
                                                        step = 1
                                                    )
                                                ),
                                                
                                                
                                                shinyjs::disabled(actionButton("button_limma_heatmapcalc", "Update"))
                                            )
                                        )
                                    )
                                )
                            ),
                            
                            tabPanel(
                                "DMPs Annotation",
                                h4("DMP Boxplot"),
                                plotOutput("graph_limma_indboxplot") %>% shinycssloaders::withSpinner(),
                                h4("DMPs Annotation"),
                                br(),
                                DT::DTOutput("table_limma_ann") %>% shinycssloaders::withSpinner(),
                                selectInput(inputId = "select_limma_anncontrast", label = "", choices = "", selected = ""),
                                actionButton(inputId = "button_limma_indboxplotcalc", label = "Plot")
                            )                                     
                        )
                )
        )),
        
        # DMRS
        tabItem(tabName = "dmrs", 
                fluidPage(
                        # Box1
                    sidebarPanel(
                        width = 3,
                        
                        pickerInput(
                            inputId = "select_dmrs_contrasts",
                            label = "Contrasts to calculate",
                            choices = c(),
                            options = list(
                                `actions-box` = TRUE,
                                size = 10,
                                `selected-text-format` = "count > 3"
                            ),
                            multiple = TRUE
                        ),
                        
                        pickerInput(
                            inputId = "select_dmrs_regions",
                            label = "Type of DMRs",
                            choices = c("promoters", "genes", "CGI"),
                            selected = c("promoters", "genes", "CGI"),
                            options = list(
                                `actions-box` = TRUE,
                                size = 10,
                                `selected-text-format` = "count > 3"
                            ),
                            multiple = TRUE
                        ),
                        
                        pickerInput(
                            inputId = "select_dmrs_platform",
                            label = "Array platform",
                            choices = c("450k", "EPIC"),
                            selected = c("EPIC"),
                            multiple = FALSE
                        ),
                        
                        sliderInput(
                            "slider_dmrs_cpgs",
                            label = "Min. CpGs in DMR",
                            min = 2,
                            max = 50,
                            value = 5
                        ),
                        
                        sliderInput(
                            "slider_dmrs_permutations",
                            label = "Number of permutations",
                            min = 1000,
                            max = 100000,
                            value = 50000
                        ),
                        
                        shinyjs::disabled(actionButton("button_dmrs_calculate", "Calculate"))
                    ),
                    
                    mainPanel(
                        width = 9,
                        tabsetPanel(
                            id = "tabset_dmrs",
                            
                            tabPanel(
                                "Heatmap",
                                div(
                                    style = "max-width:800px;margin:auto;",
                                    fluidPage(
                                        h4("DMRs Heatmap"),
                                        textOutput("text_dmrs_heatmapcount"),
                                        uiOutput("graph_dmrs_heatmapcontainer"),
                                        h4("DMRs counts in each contrast"),
                                        tableOutput("table_dmrs_count") %>% shinycssloaders::withSpinner(),
                                        
                                        fluidRow(
                                            column(
                                                6,
                                                h4("Group options"),
                                                
                                                selectizeInput(
                                                    "select_dmrs_groups2plot",
                                                    "Groups to plot",
                                                    c(),
                                                    multiple = TRUE,
                                                    options = list(plugins = list("remove_button", "drag_drop"))
                                                ),
                                                
                                                selectizeInput(
                                                    "select_dmrs_contrasts2plot",
                                                    "Contrasts to plot",
                                                    c(),
                                                    multiple = TRUE,
                                                    options = list(plugins = list("remove_button", "drag_drop"))
                                                ),
                                                
                                                selectizeInput(
                                                    "select_dmrs_regions2plot",
                                                    "Regions to plot",
                                                    c(),
                                                    multiple = TRUE,
                                                    options = list(plugins = list("remove_button", "drag_drop"))
                                                ),
                                                
                                                h4("Data options"),
                                                
                                                switchInput(
                                                    inputId = "select_dmrs_removebatch",
                                                    label = "Remove Batch Effect",
                                                    labelWidth = "100px",
                                                    value = FALSE,
                                                    disabled = TRUE
                                                ),
                                            ),
                                            
                                            column(
                                                6,
                                                h4("Filtering options"),
                                                sliderInput("slider_dmrs_deltab", "Min. DeltaBeta", 0, 1, 0),
                                                sliderInput("slider_dmrs_adjpvalue", "Max. FDR", 0, 1, 0.05),
                                                sliderInput("slider_dmrs_pvalue", "Max. p-value", 0, 1, 1)
                                            )
                                        ),
                                        
                                        h4("Clustering options",
                                           align =
                                               "left"
                                        ),
                                        
                                        fluidRow(
                                            column(
                                                5,
                                                selectInput(
                                                    "select_dmrs_clusteralg",
                                                    "Clustering algorithm",
                                                    c(
                                                        "single",
                                                        "complete",
                                                        "average",
                                                        "mcquitty",
                                                        "median",
                                                        "centroid"
                                                    ),
                                                    "average"
                                                ),
                                                
                                                selectInput(
                                                    "select_dmrs_clusterdist",
                                                    "Distance Function",
                                                    c("pearson", "spearman", "kendall", "euclidean"),
                                                    "pearson"
                                                ),
                                                
                                                selectInput("select_dmrs_scale", "Scale", c("row", "none"), "row"),
                                                tags$br()
                                            ),
                                            
                                            column(
                                                3,
                                                offset = 1,
                                                tags$br(),
                                                
                                                switchInput(
                                                    inputId = "select_dmrs_graphstatic",
                                                    label = "Static Graph",
                                                    labelWidth = "100px",
                                                    value = TRUE
                                                ),
                                                
                                                switchInput(
                                                    inputId = "select_dmrs_colv",
                                                    label = "Column Dendro.",
                                                    labelWidth = "100px",
                                                    value = TRUE
                                                ),
                                                
                                                switchInput(
                                                    inputId = "select_dmrs_colsidecolors",
                                                    label = "Column Colors",
                                                    labelWidth = "100px",
                                                    value = FALSE
                                                )
                                            ),
                                            
                                            column(
                                                3,
                                                
                                                tags$br(),
                                                
                                                switchInput(
                                                    inputId = "select_dmrs_rowsidecolors",
                                                    label = "Row Colors",
                                                    labelWidth = "100px",
                                                    value = FALSE
                                                ),
                                                
                                                conditionalPanel(
                                                    "input.select_dmrs_rowsidecolors",
                                                    numericInput(
                                                        "select_dmrs_knumber",
                                                        "Clusters number",
                                                        value = 2,
                                                        min = 1,
                                                        max = Inf,
                                                        step = 1
                                                    )
                                                ),
                                                
                                                shinyjs::disabled(actionButton("button_dmrs_heatmapcalc", "Update"))
                                            )
                                        )
                                    )
                                )
                            ),
                            
                            tabPanel(
                                "Single DMR plot",
                                
                                h4("Genomic graph"),
                                plotOutput("graph_dmrs_singledmr") %>% shinycssloaders::withSpinner(),
                                # h4("GSEA graph"),
                                # plotOutput("graph_dmrs_singlegsea") %>% shinycssloaders::withSpinner(),
                                h4("DMRs table"),
                                
                                
                                div(
                                    style = "display:inline-block",
                                    
                                    selectInput(
                                        "select_dmrs_selcont",
                                        label = "Contrast",
                                        choices = c()
                                    )
                                ),
                                
                                div(
                                    style = "display:inline-block",
                                    selectInput("select_dmrs_selreg", label = "Region", choices = c())
                                ),
                                
                                
                                DT::DTOutput("table_dmrs_table") %>% shinycssloaders::withSpinner(),
                                
                                br(),
                                
                                actionButton("button_dmrs_graphsingle", "Plot")
                            )
                        )                                     
                        )
                )
        )
        
        
        
        
    ))))
