#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    # Max size
    options(shiny.maxRequestSize = 100*1024^2) #5MB getShinyOption("shiny.maxRequestSize") | 30*1024^2 = 30MB
    
    # INITIALIZE REACTIVE VARIABLES
    rval_generated_limma_model <- reactiveVal(value = FALSE)
    rval_analysis_finished <- reactiveVal(value = FALSE)
    rval_filteredlist2heatmap_valid <- reactiveVal(value = FALSE)
    rval_filteredmcsea2heatmap_valid <- reactiveVal(value = FALSE)
    rval_dmrs_finished <- reactiveVal(value = FALSE)
    rval_dmrs_ready2heatmap <- reactiveVal(value = FALSE)
    rval_dmrs_ready2mcsea <- reactiveVal(value = FALSE)
    
    
    # Load button
    output$ui_button_input_load <- renderUI({
        if (!is.null(input$fileinput_input$datapath)) {
            return(actionButton("button_input_load", "Load Data"))
        } else {
            return()
        }
    })
    
    observeEvent(input$fileinput_input, shinyjs::enable("button_input_load"))
    
    # When you press button_input_load, the data is unzipped and the metharray sheet is loaded
    rval_sheet <- eventReactive(input$button_input_load, {
        
        # Check if updated file is .zip
        validate(need(tools::file_ext(input$fileinput_input$datapath) == "zip", "File extension should be .zip"))
        
        shinyjs::disable("button_input_load") # disable the load button to avoid multiple clicks
        
        if (dir.exists(paste0(tempdir(), "/experiment_data"))) {
            unlink(paste0(tempdir(), "/experiment_data"), recursive = TRUE) # remove current files in target directory
        }
        
        zip::unzip(input$fileinput_input$datapath,
                   exdir = paste0(tempdir(), "/experiment_data")
        ) # extracting zip
        
        sheet <- minfi::read.metharray.sheet(paste0(tempdir(), "/experiment_data"))
        
        # We check if sheet is correct
        # This is to prevent app crashes when zip updated is not correct.
        validate(
            need(
                is.data.frame(sheet) &
                    any(colnames(sheet) %in% "Slide") &
                    any(colnames(sheet) %in% "Array"),
                "SampleSheet is not correct. Please, check your samplesheet and your zip file."
            )
        )
        
        validate(
            need(
                anyDuplicated(colnames(sheet)) == 0,
                "Repeated variable names are not allowed. Please, modify your sample sheet."
            )
        )
        
        colnames(sheet) <- make.names(colnames(sheet)) # fix possible not-valid colnames
        
        sheet
        #print(colnames(sheet))
    })
    

    output$samples_table <- DT::renderDT(
        rval_sheet(),
        rownames = FALSE,
        selection = "single",
        style = "bootstrap",
        options = list(
            pageLength = 10,
            autoWidth = TRUE,
            scrollX = TRUE,
            columnDefs = list(list(
                targets = match("Basename", colnames(rval_sheet())) - 1, visible = FALSE
            ))
        )
    )
    

    
    
    rval_sheet_target <- eventReactive(
        input$button_input_next,
        rval_sheet()[rval_sheet()[[input$select_input_samplenamevar]] %in% input$selected_samples, ],
    )
    
    
    rval_clean_sheet_target <- eventReactive(rval_gset(), {
        generate_clean_samplesheet(target_samplesheet = minfi::pData(rval_gset()),
                                   donorvar = input$select_input_donorvar)
        
    })
    
    
    # When you press button_input_load, the form options are updated
    observeEvent(input$button_input_load, {
        updateSelectInput(
            session,
            "select_input_samplenamevar",
            label = "Select Sample Names Column:",
            choices = colnames(rval_sheet()),
        )
        
        updateSelectInput(
            session,
            "select_input_groupingvar",
            label = "Select Variable of Interest:",
            choices = colnames(rval_sheet())
        )
        updateSelectInput(
            session,
            "select_input_donorvar",
            label = "Select Donor Variable:",
            choices = colnames(rval_sheet())
        )
        
       shinyjs::enable("button_input_next") # Enable button continue
    })
    
    # The checkbox of samples to process is updated when samplenamevar changes
    observeEvent(
        {
            input$select_input_samplenamevar
            input$select_input_groupingvar
        },
        updatePickerInput(
            session,
            "selected_samples",
            label = "Select Samples to Process:",
            selected = rval_sheet()[, input$select_input_samplenamevar],
            choices = rval_sheet()[, input$select_input_samplenamevar],
            choicesOpt = list(subtext = paste("Group: ", rval_sheet()[, input$select_input_groupingvar]))
        )
    )
    
    # when samples selected are changed, continue button is enabled again
    observeEvent(input$selected_samples, shinyjs::enable("button_input_next"))
    
    
    
    
    # rval_rgset loads RGSet using read.metharray.exp and the sample sheet (rval_sheet())
    rval_rgset <- eventReactive(input$button_input_next, ignoreNULL = FALSE, {
        validate(need(input$fileinput_input != "", "Data has not been uploaded yet"))
        
        # Prior check to test variable selection
        if (anyDuplicated(rval_sheet_target()[, input$select_input_samplenamevar]) > 0 |
            anyDuplicated(rval_sheet_target()[, input$select_input_groupingvar]) == 0) {
            showModal(
                modalDialog(
                    title = "Variable error",
                    "Check if selected variables are correct. Sample Name Variable should not have duplicated values
          and the variable of interest should have groups greater than 1.",
                    easyClose = TRUE,
                    footer = NULL
                )
            )
        }
        
        # Check prior conditions to read data
        validate(need(
            anyDuplicated(rval_sheet_target()[, input$select_input_samplenamevar]) == 0,
            "Sample Name Variable should not have duplicated values"
        ))
        validate(need(
            anyDuplicated(rval_sheet_target()[, input$select_input_groupingvar]) > 0,
            "Grouping variable should have groups greater than 1"
        ))
        
        # disable button to avoid multiple clicks
        shinyjs::disable("button_input_next")
        
        
        # We need to check if this step works
        withProgress(
            message = "Reading array data...",
            value = 2,
            max = 5,
            {
                try({
                    RGSet <- read_idats(
                        targets = rval_sheet_target())
                })
                
                if (!exists("RGSet", inherits = FALSE)) {
                    showModal(
                        modalDialog(
                            title = "reading error",
                            "Minfi can't read arrays specified in your samplesheet. Please, check your zipfile and your sampleSheet",
                            easyClose = TRUE,
                            footer = NULL
                        )
                    )
                    shinyjs::disable("button_minfi_select")
                }
                
                validate(
                    need(
                        exists("RGSet", inherits = FALSE),
                        "Minfi can't read arrays specified in your samplesheet. Please, check your zipfile and your sampleSheet"
                    )
                )
                
                #Checking array type and annotation
                nProbes = length(minfi::featureNames(RGSet))
                
                if(nProbes >= 622000 & nProbes <= 623000){
                    
                    if (!requireNamespace("IlluminaHumanMethylation450kanno.ilmn12.hg19", quietly = TRUE) |
                        !requireNamespace("IlluminaHumanMethylation450kmanifest", quietly = TRUE))
                    {
                        showModal(
                            modalDialog(
                                title = "Missing package(s)",
                                "450k annotation or manifest packages are not available. Please, install IlluminaHumanMethylation450kmanifest and IlluminaHumanMethylation450kanno.ilmn12.hg19 packages and restart the application.",
                                easyClose = TRUE,
                                footer = NULL
                            )
                        )
                    }
                    
                    validate(
                        need(
                            requireNamespace("IlluminaHumanMethylation450kanno.ilmn12.hg19", quietly = TRUE) &
                                requireNamespace("IlluminaHumanMethylation450kmanifest", quietly = TRUE),
                            "450k annotation or manifest packages are not available. Please, install IlluminaHumanMethylation450kmanifest and IlluminaHumanMethylation450kanno.ilmn12.hg19 packages."
                        )
                    )
                }
                else if (nProbes >= 1032000 & nProbes <= 1053000){
                    
                    if (!requireNamespace("IlluminaHumanMethylationEPICanno.ilm10b4.hg19", quietly = TRUE) |
                        !requireNamespace("IlluminaHumanMethylationEPICmanifest", quietly = TRUE))
                    {
                        showModal(
                            modalDialog(
                                title = "Missing package(s)",
                                "EPIC annotation or manifest packages are not available. Please, install IlluminaHumanMethylationEPICmanifest and IlluminaHumanMethylationEPICanno.ilm10b4.hg19 packages and restart the application.",
                                easyClose = TRUE,
                                footer = NULL
                            )
                        )
                    }
                    
                    validate(
                        need(
                            requireNamespace("IlluminaHumanMethylationEPICanno.ilm10b4.hg19", quietly = TRUE) &
                                requireNamespace("IlluminaHumanMethylationEPICmanifest", quietly = TRUE),
                            "EPIC annotation or manifest packages are not available. Please, install IlluminaHumanMethylationEPICmanifest and IlluminaHumanMethylationEPICanno.ilm10b4.hg19 packages."
                        )
                    )
                }
                
                # analysis restarted
                rval_analysis_finished(FALSE)
                rval_dmrs_finished(FALSE)
                
                # we return RGSet
                RGSet
            }
        )
    })
    

    
    
    
    
    # We change the page to the next one
    observeEvent(input$button_input_next, {
        # check if rgset is loaded
        req(rval_rgset())
        
        # update PCA parameters
        updateSelectInput(
            session,
            "select_minfi_pcaplot_pcx",
            choices = paste0("PC", seq_len(nrow(rval_sheet_target()))),
            selected = "PC1"
        )
        
        updateSelectInput(
            session,
            "select_minfi_pcaplot_pcy",
            choices = paste0("PC", seq_len(nrow(rval_sheet_target()))),
            selected = "PC2"
        )
        
        updateSelectInput(
            session,
            "select_minfi_pcaplot_color",
            choices = c(
                colnames(rval_sheet_target()),
                "xMed",
                "yMed",
                "predictedSex"
            ),
            
            selected = input$select_input_groupingvar
        )
        
        shinyjs::enable("button_minfi_select")
        updateTabsetPanel(session, "menu", "normalization")
    })
    
    
    
    
    
    # MINFI NORMALIZATION
    
    # Calculation of minfi normalized data
    rval_gset <- eventReactive(input$button_minfi_select, {
        validate(need(
            !is.null(rval_rgset()),
            "Raw data has not been loaded yet."
        ))
        
        shinyjs::disable("button_minfi_select") # disable button to avoid repeat clicking
        
        withProgress(
            message = "Normalization in progress...",
            value = 1,
            max = 4,
            {
                try({
                    gset <- normalize_rgset(
                        rgset = rval_rgset(), normalization_mode = input$select_minfi_norm,
                        detectP = 0.01, dropSNPs = input$select_minfi_dropsnps, maf = input$slider_minfi_maf,
                        dropCpHs = input$select_minfi_dropcphs, dropSex = input$select_minfi_chromosomes
                    )
                })
                
                # check if normalization has worked
                
                if (!exists("gset", inherits = FALSE)) {
                    showModal(
                        modalDialog(
                            title = "Normalization failure",
                            "An unexpected error has occurred during minfi normalization. Please, notify the error to the package maintainer.",
                            easyClose = TRUE,
                            footer = NULL
                        )
                    )
                    shinyjs::enable("button_minfi_select")
                }
                
                validate(
                    need(
                        exists("gset", inherits = FALSE),
                        "An unexpected error has occurred during minfi normalization. Please, notify the error to the package maintainer."
                    )
                )
                
                # enable button
                shinyjs::enable("button_minfi_select")
                
                # return gset
                gset
            }
        )
    })
    
    


    


})
