#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(gt)
title <- "Evaluating automated selection of outcome negative controls for estimating the total error distribution of self-controlled cohort study designs"

dataSources <- data.frame(sourceId = c(11, 12, 13, 10),
                          sourceName = c("IBM CCAE", "IBM MDCD", "IBM MDCR", "Optum SES"),
                          persons = c(152963555, 28917265, 10115120, 84310086),
                          years = c("Jan 1, 2000 to Feb 29, 2020", "Jan 1, 2006 to June 30, 2019", "Jan 1, 2000 to Jan 31, 2020", "May 1, 2000 to Dec 31, 2019")
)

data("ohdsiNegativeControls", package = 'MethodEvaluation')
ohdsiNegativeControls <- ohdsiNegativeControls[ohdsiNegativeControls$type != "Exposure control",]
manualControlData <- readRDS("data/outcomeControlEvaluationManualControlsDt.rds")

automatedControlsData <- readRDS("data/outcomeControlEvaluationAutomatedControlsDt.rds")
automatedControlsData <- automatedControlsData[automatedControlsData$tCases + automatedControlsData$cCases > 10,]

exposureNames <- readRDS("data/exposureNames.rds")

# Precomputed results
evaluationResults <- readRDS("data/outcomeControlEvaluationTable.rds")
fullDataSet <- readRDS("data/outcomeControlEvaluationFullData.rds")
errorRateData <- readRDS("data/computedOutcomeControllError.rds")
nullDists <- readRDS("data/nullDistOutput.rds")

calibrationPlotTab <- fluidPage(h3("Calibration plots"),
                                p("The plots below compare manual to automated sets of controls"),
                                fluidRow(column(6, div(h4("Manual controls"),
                                                       shinycssloaders::withSpinner(plotly::plotlyOutput("manualCalibrationPlot")))),
                                         column(6, div(h4("Automated controls"),
                                                       shinycssloaders::withSpinner(plotly::plotlyOutput("automatedCalibrationPlot"))))),
                                p("Estimates below the dashed line (gray area) have p<0.05 using traditional p-value calculation. Estimates in the orange areas have p<0.05 using the calibrated p-value calculation and the red band indicates the 95% credible interval of the boundary of the orange area. Blue dots indicate negative controls. The shown absolute systematic error summarises the computed null distributions in a single statistic that captures the mean and variance of the distributions")
)
aboutText <- fluidPage(
  h3("About this study"),
  p("Authors: James P. Gilbert, Rachel E. Teneralli, Erica A. Voss, Christopher A. Knoll, David M. Kern, M Soledad Cepeda, Patrick B. Ryan"),
  h4("Purpose"),
  p("The use of negative controls for empirical calibration of p-values and confidence intervals (Cis) of effect estimates is an important tool to improve the quality, reproducibility, and robustness of population effect estimation studies.",
    "However, sets of negative controls are challenging to select, requiring significant manual curation",
    "This has proven to be a rate limiting factor in high-throughput studies conducted previously;",
    "without calibrated effect estimates, a very large number of false positive results are generated.",
    "In this work, we evaluated a fully automated method of generating outcome controls using the, previously presented, Common Evidence Model (CEM) applied to a series of self-controlled cohort (SCC) designs."
  ),

  h4("Methods"),
  p("We applied the self controlled cohort design at the level of all exposures (defined as new users of RX-norm ingredients) outcome concepts (defined by 2 diagnosis SNOMED codes within one year).",
    "Patients required at least one year of prior observation.",
    "We considered exposure windows to be continuous periods of drug exposure, with 30 days windows.",
    "For the evaluation of controls we picked 4 exposures from the OHDSI MethodEvaluation package and used the 25 negative control outcomes as the manual gold standard benchmark",
    "Automated Negative controls were selected using the common evidence model, with the CEM connector package.",
    "The OHDSI empirical calibration package was used to calculate adjusted confidence intervals, p-values and effect estimates"
  ),
  h4("Data sources"),
  p("We applied these methods to 4 US insurance claims databases that conform to the OMOP Common Data Model:"),
  gt::gt_output("dataSourceTable")
)

systematicErrorTab <- fluidPage(
  h3("Difference of systematic error distributions"),
  p("The absolute systematic error is a single measure of the error captured in the null distributions."),
  p("The table below displays the observed mean, standard deviation and measured absolute error of the null distributions for the manually selected outcome controls
  and automatically selected outcome controls."),
  br(),
  gt::gt_output("resultTable")
)

performanceComparissonTab <- fluidPage(
  h3("Performance of Calibration"),
  p("Comparing the performance of calibration is measuring the relative number of true positives for statistically significant effect estimates.",
    "Results compare calbirated and uncalibrated effect estiamtes generated by the SCC analysis at a signficance level of p < 0.05 with a confidence interval that is does not cross 1 (the line of no effect)."),
  gt::gt_output("performance"),
  h5("Table Legend:"),
  p("FDR = False Discovery Rate"),
  p("PPV = Positive Predictive Value"),
  p("NPV = Negative Predictive Value"),
  p("FNR = False Negative Rate"),
  p("FP = False Positives"),
  p("TP = True Positives")
)

negativeControlsTab <- fluidPage(
  h3("Cem Connector Controls"),
  p("The following are control outcomes that are collected using the cem connector package.",
    "Though, in this study, all potential controls were used, the ranking here could allow the selection of a subset that are based on co-occurence accross the OHDSI network of CDMS."),
  CemConnector::negativeControlSelectorUi("controls"))

tEE <- fluidRow(
  h3("Total Expected Systematic Error"),
  p("Below is the expected total systematic error, computed with automated negative controls, for all Rx-Norm ingrerdient exposures"),
  shinycssloaders::withSpinner(plotOutput("teeDist")),
  p("Violin plots of the distribution of expected absolute systematic error across null distributions generated from outcome controls for all exposures in each data source.
   Though 2 standard deviations of error are below 0.5 for all the datasets, in some cases systematic error appears to be extreme.
   Many of these can be explained by small counts for exposure, outcome pairs.")
)

referencesTab <- fluidRow(
  h4("OHDSI R packages"),
  p(a("CemConnector:", href = "https://github.com/OHDSI/CemConnector"), " to generate automatic sets of negative controls"),
  p(a("SelfControlledCohort:", href = "https://github.com/OHDSI/SelfControlledCohort"), " to generate effect estimates"),
  p(a("EmpiricalCalibration:", href = "https://github.com/OHDSI/EmpiricalCalibration"), " to calibrate effect estimates"),
  p(a("MethodEvaluation:", href = "https://github.com/OHDSI/MethodEvaluation"), " for benchmark cohorts and manual negative controls"),
  h4("References"),
  p("1.  Schuemie MJ, Hripcsak G, Ryan PB, Madigan D, Suchard MA. Empirical confidence interval calibration for population-level effect estimation studies in observational healthcare data. Proc Natl Acad Sci U S A. 2018 Mar 13;115(11):2571–7."),
  p("2. 	Schuemie MJ, Ryan PB, Hripcsak G, Madigan D, Suchard MA. Improving reproducibility by using high-throughput observational studies with empirical calibration. Philos Trans R Soc A Math Phys Eng Sci. 2018;376(2128)."),
  p("3. 	Lipsitch M, Tchetgen Tchetgen E, Cohen T. Negative Controls: A tool for detecting confounding and bias in observational studies. Epidemiology [Internet]. 2010 May [cited 2020 May 14];21(3):383–8. Available from: http://journals.lww.com/00001648-201005000-00017"),
  p("4. 	Teneralli RE, Kern DM, Cepeda MS, Gilbert JP, Drevets WC. Exploring Real-World Evidence to Uncover Unknown Drug Benefits and Support the Discovery of New Treatment Targets for Depressive and Bipolar Disorders Exploring Real-World Evidence to Uncover Unknown Drug Benefits and Support the Discovery of New Treatment Targets for. J Affect Disord [Internet]. 2021 [cited 2021 May 12]; Available from: https://doi.org/10.1016/j.jad.2021.04.096"),
  p("5. 	Kern DM, Cepeda MS, Lovestone S, Seabrook GR. Aiding the discovery of new treatments for dementia by uncovering unknown benefits of existing medications. Alzheimer’s Dement Transl Res Clin Interv. 2019 Jan 1;5:862–70."),
  p("6. 	Cepeda MS, Kern DM, Seabrook GR, Lovestone S. Comprehensive Real-World Assessment of Marketed Medications to Guide Parkinson’s Drug Discovery. Clin Drug Investig. 2019 Nov 1;39(11):1067–75."),
  p("7. 	Voss EA, Boyce RD, Ryan PB, van der Lei J, Rijnbeek PR, Schuemie MJ. Accuracy of an automated knowledge base for identifying drug adverse reactions. J Biomed Inform. 2017 Feb 1;66:72–81."),
  p("8. 	J. Schuemie M, Soledad Cepede M, A. Suchard M, Yang J, Tian Alejandro Schuler Y, B. Ryan P, et al. How Confident Are We About Observational Findings in Health Care: A Benchmark Study. Harvard Data Sci Rev. 2020 Jan 31;2(1)."),
)

# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel(title),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    sidebarPanel(
      shiny::selectInput("databaseSel", "Data source", dataSources$sourceName),
      shiny::selectInput("exposureSel", "RxNorm Ingredient", exposureNames$exposureName),
    ),

    # Main panel for displaying outputs ----
    mainPanel(

      tabsetPanel(type = "tabs",
                  tabPanel("About", aboutText),
                  tabPanel("Calibration Plots", calibrationPlotTab),
                  tabPanel("Systematic Error", systematicErrorTab),
                  tabPanel("Performance comparisson", performanceComparissonTab),
                  tabPanel("Negative Controls", negativeControlsTab),
                  tabPanel("Total expected error", tEE),
                  tabPanel("References", referencesTab)
      ),
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {

  output$summary <- renderText("summary")

  selectedExposureId <- reactive({
    exposureNames %>%
      filter(exposureName == input$exposureSel) %>%
      select(exposureId) %>%
      pull()
  })

  selectedDataSource <- reactive({
    dataSources %>%
      filter(sourceName == input$databaseSel) %>%
      select(sourceId) %>%
      pull()
  })

  output$manualCalibrationPlot <- plotly::renderPlotly({

    exposureId <- selectedExposureId()
    sourceId <- selectedDataSource()
    mData <- manualControlData[manualControlData$sourceId == sourceId & manualControlData$exposureConceptId == exposureId,]
    positives <- fullDataSet[fullDataSet$sourceId == sourceId & fullDataSet$exposureConceptId == exposureId,]
    EmpiricalCalibration::plotCalibrationEffect(log(mData$rr),
                                                mData$seLogRr,
                                                showExpectedAbsoluteSystematicError = TRUE)
  })

  output$automatedCalibrationPlot <- plotly::renderPlotly({
    exposureId <- selectedExposureId()
    sourceId <- selectedDataSource()
    autData <- automatedControlsData[automatedControlsData$sourceId == sourceId & automatedControlsData$exposureConceptId == exposureId,]
    EmpiricalCalibration::plotCalibrationEffect(log(autData$rr),
                                                autData$seLogRr,
                                                showExpectedAbsoluteSystematicError = TRUE)
  })


  output$resultTable <- gt::render_gt({
    outputTable <- evaluationResults %>%
      inner_join(dataSources, by = "sourceId") %>%
      filter(sourceId == selectedDataSource()) %>%
      inner_join(exposureNames, by = "exposureId") %>%
      select(sourceName, exposureName, manualMean, manualSd, automatedMean, automatedSd, manualAbsErr, automatedAbsErr, absErrorDiff, z, p) %>%
      mutate(z = abs(z)) %>%
      gt(groupname_col = "sourceName") %>%
      fmt_number(3:11, decimals = 3) %>%
      cols_label(
        manualMean = "Manual Mean",
        automatedMean = "Automated Mean",
        manualSd = "Manual Sd",
        automatedSd = "Automated Sd",
        manualAbsErr = "Manual Abs Error",
        automatedAbsErr = "Automated Abs Error",
        absErrorDiff = "Error difference",
        z = "Z-score",
        exposureName = "",
        p = "P value"
      ) %>%
      tab_options(row_group.background.color = "lightgrey",
                  heading.title.font.size = "small",
                  row_group.font.size = "small",
                  container.width = pct(100),
                  table.font.size = "small",
                  heading.subtitle.font.size = "small")
  })

  output$performance <- gt::render_gt({
    res <- errorRateData %>%
      inner_join(dataSources, by = "sourceId") %>%
      unique() %>%
      filter(sourceId == selectedDataSource() & exposureId == selectedExposureId()) %>%
      inner_join(exposureNames, by = "exposureId") %>%
      mutate(FDR = (1 - ppv) * 100) %>%
      select(sourceName, exposureName, type, sensetivity, specificity, FDR, ppv, npv, fnr, fp, tp)

    res %>%
      gt(groupname_col = "sourceName") %>%
      fmt_number(4:9, decimals = 3) %>%
      cols_label(
        exposureName = "",
        type = "Type",
        sensetivity = "Sensetivity",
        specificity = "Specificity",
        ppv = "PPV",
        npv = "NPV",
        fnr = "FNR",
        fp = "FP",
        tp = "TP"
      ) %>%
      tab_options(row_group.background.color = "lightgrey",
                  heading.title.font.size = "small",
                  row_group.font.size = "small",
                  container.width = pct(100),
                  table.font.size = "small",
                  heading.subtitle.font.size = "small")
  })

  output$dataSourceTable <- gt::render_gt({
    dataSources %>%
      select(sourceName, persons, years) %>%
      gt() %>%
      cols_label(
        sourceName = "Source",
        persons = "Person count",
        years = "Period covered"
      ) %>%
      fmt_number(2, decimals = 0) %>%
      tab_options(row_group.background.color = "lightgrey",
                  heading.title.font.size = "small",
                  row_group.font.size = "small",
                  container.width = pct(100),
                  table.font.size = "small",
                  heading.subtitle.font.size = "small")
  })

  cemBackend <- CemConnector::CemWebApiBackend$new("http://cem.ohdsi.org:8080/")
  conceptInputNc <- shiny::reactive({ data.frame(conceptId = selectedExposureId(), includeDescendants = 1, isExcluded = 0) })
  ncModuleServer <- CemConnector::negativeControlSelectorModule("controls",
                                                                cemBackend,
                                                                conceptInput = conceptInputNc,
                                                                isOutcomeSearch = reactive(TRUE),
                                                                nControls = reactive(5000),
                                                                siblingLookupLevelsInput = reactive({ 0 }))

  output$teeDist <- renderPlot({
    ggplot2::ggplot(nullDists[nullDists$sourceId !=15,], ggplot2::aes(x = sourceName, y = absoluteError, color = sourceName)) +
      ggplot2::geom_violin() +
      ggplot2::geom_boxplot(width = 0.2) +
      ggplot2::xlab("Data Source") +
      ggplot2::ylab("Expected Absolute Systematic Error") +
      ggplot2::theme(legend.position = "none", text = ggplot2::element_text(size = 11))
  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)