library(renv)
# library(usethis)
library(tidyverse)
library(shiny)
library(DT)
library(tableHTML)
library(formattable)
library(dplyr)
library(writexl)
library(openxlsx)

# Assumptions(roughly) - Deposit increase: 6% (see Deposit increase in contents). 
# Income increase: 5.3% (see Income increase in contents). 
# Property inflation: 5% (See Property inflation for each typology in contents) 
# Interest rate: 6.2% (see Contents). Min share: 70% (Now an input)

# renv::init()
# renv::activate()
# renv::snapshot()

# Convert annual interest rate to monthly rate
calculateMonthlyPayment <- function(loanAmount, annualInterestRate, loanTermYears) {
  # Convert annual interest rate to monthly rate
  monthlyInterestRate <- annualInterestRate / 12 / 100
  
  # Calculate total number of payments
  totalPayments <- loanTermYears * 12
  
  # Calculate monthly mortgage payment
  monthlyPayment <- (loanAmount * monthlyInterestRate) / (1 - (1 + monthlyInterestRate)^(-totalPayments))
  
  return(monthlyPayment)
}

mortgageTerm <- function(AgeInput) {
  LoanTerm <- min(70 - AgeInput,30)

  return(LoanTerm)
}

# Define UI for application ####
ui <- fluidPage(
  fluidRow(
    column(1, img(src = "TRC-logo.png", width = "100", length = "105")),
    column(11, h1("Mortgage Readiness Tool", style = "display: inline-block; margin-left: 20px;"))
  ),
  ## Inputs ####
  fluidRow(
    column(2,
           numericInput("AgeType",
                        h5("Age"),
                        value = 0,
                        min = 18,
                        max = 50)),
    
    column(2,
           numericInput("Incometype",
                        h5("Income"),
                        value = 0,
                        min = 0,
                        max = 205000)),
    
    column(2,
           numericInput("SavingsType",
                        h5("Savings"),
                        value = 0,
                        min = 0,
                        max = 200000)),
    
    column(2,
           numericInput("KiwiSaverType",
                        h5("KiwiSaver"),
                        value = 0,
                        min = 0,
                        max = 200000)),
    
    column(2,
           numericInput("DebtType",
                        h5("Debt"),
                        value = 0,
                        min = 0,
                        max = 150000))
  ),
  
  fluidRow(
    column(2,
           numericInput("Income_Change_1",
                        h5("Extra income year 1"),
                        value = 0,
                        min = 0,
                        max = 50000)),
    
    column(2,
           numericInput("Income_Change_2",
                        h5("Extra income year 2"),
                        value = 0,
                        min = 0,
                        max = 50000)),
    
    column(2,
           numericInput("Income_Change_3",
                        h5("Extra income year 3"),
                        value = 0,
                        min = 0,
                        max = 50000)),
    
    column(2,
           numericInput("Income_Change_4",
                        h5("Extra income year 4"),
                        value = 0,
                        min = 0,
                        max = 50000)),
    
    column(2,
           numericInput("Income_Change_5",
                        h5("Extra income year 5"),
                        value = 0,
                        min = 0,
                        max = 50000)),
  ),
  
  fluidRow(
    column(2,
           numericInput("Savings_Change_1",
                        h5("Extra savings year 1"),
                        value = 0,
                        min = 0,
                        max = 50000)),
    
    column(2,
           numericInput("Savings_Change_2",
                        h5("Extra savings year 2"),
                        value = 0,
                        min = 0,
                        max = 50000)),
    
    column(2,
           numericInput("Savings_Change_3",
                        h5("Extra savings year 3"),
                        value = 0,
                        min = 0,
                        max = 50000)),
    
    column(2,
           numericInput("Savings_Change_4",
                        h5("Extra savings year 4"),
                        value = 0,
                        min = 0,
                        max = 50000)),
    
    column(2,
           numericInput("Savings_Change_5",
                        h5("Extra savings year 5"),
                        value = 0,
                        min = 0,
                        max = 50000)),
  ),
  
  fluidRow(
    column(2,
           numericInput("Two_bedroom_value", "2 bedroom median value", value = 775000)),
    column(2,
           numericInput("Three_bedroom_value", "3 bedroom median value", value = 890000)),
    column(2,
           numericInput("Four_bedroom_value", "4 bedroom median value", value = 1000000)),
    column(2,
           numericInput("Five_bedroom_value", "5 bedroom median value", value = 1250000)),
  ),
  
  fluidRow(
    column(2,
           numericInput("InterestRate_1", "Interest rate (%)", value = 6.2)), ## Interest Rate ####
    column(2,
           numericInput("WhanauShare", "Whānau share", max = 0.70, min = 0.60, step = 0.05, value = 0.70)),
    column(2,
           numericInput("DebtIncome_Ratio", "Debt to Income ratio (%)", max = 35, min = 30, step = 2.5, value = 30)),
  ),
  
  fluidRow(
    column(2,
           numericInput("Inflation_1","1st year property inflation", max=25,min=0,step=0.5,value = 5.0)),
    column(2,
           numericInput("Inflation_2","2nd year property inflation", max=25,min=0,step=0.5,value = 5.0)),
    column(2,
           numericInput("Inflation_3","3rd year property inflation", max=25,min=0,step=0.5,value = 5.0)),
    column(2,
           numericInput("Inflation_4","4th year property inflation", max=25,min=0,step=0.5,value = 5.0)),
    column(2,
           numericInput("Inflation_5","5th year property inflation", max=25,min=0,step=0.5,value = 5.0)),
    ),
  
  fluidRow(
    actionButton("calculate", "Calculate"),
    downloadButton("download_xlsx", "Download as XLSX"),
  ),
  
  mainPanel(
    # Three bedroom
    # textOutput("AgeType"),
    # textOutput("AgeType_1"),
    # textOutput("AgeType_2"),
    # textOutput("AgeType_3"),
    # textOutput("AgeType_4"),
    # textOutput("AgeType_5"),
    # textOutput("Three_bedroom_value"),
    # textOutput("Three_bedroom_value_1"),
    # textOutput("Three_bedroom_value_2"),
    # textOutput("Three_bedroom_value_3"),
    # textOutput("Three_bedroom_value_4"),
    # textOutput("Three_bedroom_value_5"),
    # textOutput("Three_bedroom_min_share"),
    # textOutput("Three_bedroom_min_share_1"),
    # textOutput("Three_bedroom_min_share_2"),
    # textOutput("Three_bedroom_min_share_3"),
    # textOutput("Three_bedroom_min_share_4"),
    # textOutput("Three_bedroom_min_share_5"),
    # textOutput("Loan_ThreeBed"),
    # textOutput("Loan_ThreeBed_1"),
    # textOutput("Loan_ThreeBed_2"),
    # textOutput("Loan_ThreeBed_3"),
    # textOutput("Loan_ThreeBed_4"),
    # textOutput("Loan_ThreeBed_5"),
    # textOutput("ExpectedDeposit_ThreeBed"),
    # textOutput("ExpectedDeposit_ThreeBed_1"),
    # textOutput("ExpectedDeposit_ThreeBed_2"),
    # textOutput("ExpectedDeposit_ThreeBed_3"),
    # textOutput("ExpectedDeposit_ThreeBed_4"),
    # textOutput("ExpectedDeposit_ThreeBed_5"),
    # textOutput("Minimum_Income_ThreeBed"),
    # textOutput("Minimum_Income_ThreeBed_1"),
    # textOutput("Minimum_Income_ThreeBed_2"),
    # textOutput("Minimum_Income_ThreeBed_3"),
    # textOutput("Minimum_Income_ThreeBed_4"),
    # textOutput("Minimum_Income_ThreeBed_5"),
    # textOutput("ActualIncome_ThreeBed"),
    # textOutput("ActualIncome_ThreeBed_1"),
    # textOutput("ActualIncome_ThreeBed_2"),
    # textOutput("ActualIncome_ThreeBed_3"),
    # textOutput("ActualIncome_ThreeBed_4"),
    # textOutput("ActualIncome_ThreeBed_5"),
    # textOutput("Deposit_calculation"),
    # textOutput("Deposit_calculation_1"),
    # textOutput("Deposit_calculation_2"),
    # textOutput("Deposit_calculation_3"),
    # textOutput("Deposit_calculation_4"),
    # textOutput("Deposit_calculation_5"),
    # Three bedroom
    # textOutput("Three_bedroom_value"),
    # textOutput("Three_bedroom_value_1"),
    # textOutput("Three_bedroom_value_2"),
    # textOutput("Three_bedroom_value_3"),
    # textOutput("Three_bedroom_value_4"),
    # textOutput("Three_bedroom_value_5"),
    # textOutput("Three_bedroom_min_share"),
    # textOutput("Three_bedroom_min_share_1"),
    # textOutput("Three_bedroom_min_share_2"),
    # textOutput("Three_bedroom_min_share_3"),
    # textOutput("Three_bedroom_min_share_4"),
    # textOutput("Three_bedroom_min_share_5"),
    # textOutput("Loan_ThreeBed"),
    # textOutput("Loan_ThreeBed_1"),
    # textOutput("Loan_ThreeBed_2"),
    # textOutput("Loan_ThreeBed_3"),
    # textOutput("Loan_ThreeBed_4"),
    # textOutput("Loan_ThreeBed_5"),
    # textOutput("ExpectedDeposit_ThreeBed"),
    # textOutput("ExpectedDeposit_ThreeBed_1"),
    # textOutput("ExpectedDeposit_ThreeBed_2"),
    # textOutput("ExpectedDeposit_ThreeBed_3"),
    # textOutput("ExpectedDeposit_ThreeBed_4"),
    # textOutput("ExpectedDeposit_ThreeBed_5"),
    # textOutput("Minimum_Income_ThreeBed"),
    # textOutput("Minimum_Income_ThreeBed_1"),
    # textOutput("Minimum_Income_ThreeBed_2"),
    # textOutput("Minimum_Income_ThreeBed_3"),
    # textOutput("Minimum_Income_ThreeBed_4"),
    # textOutput("Minimum_Income_ThreeBed_5"),
    # Four bedroom
    # textOutput("Four_bedroom_value"),
    # textOutput("Four_bedroom_value_1"),
    # textOutput("Four_bedroom_value_2"),
    # textOutput("Four_bedroom_value_3"),
    # textOutput("Four_bedroom_value_4"),
    # textOutput("Four_bedroom_value_5"),
    # textOutput("Four_bedroom_min_share"),
    # textOutput("Four_bedroom_min_share_1"),
    # textOutput("Four_bedroom_min_share_2"),
    # textOutput("Four_bedroom_min_share_3"),
    # textOutput("Four_bedroom_min_share_4"),
    # textOutput("Four_bedroom_min_share_5"),
    # textOutput("Loan_FourBed"),
    # textOutput("Loan_FourBed_1"),
    # textOutput("Loan_FourBed_2"),
    # textOutput("Loan_FourBed_3"),
    # textOutput("Loan_FourBed_4"),
    # textOutput("Loan_FourBed_5"),
    # textOutput("ExpectedDeposit_FourBed"),
    # textOutput("ExpectedDeposit_FourBed_1"),
    # textOutput("ExpectedDeposit_FourBed_2"),
    # textOutput("ExpectedDeposit_FourBed_3"),
    # textOutput("ExpectedDeposit_FourBed_4"),
    # textOutput("ExpectedDeposit_FourBed_5"),
    # textOutput("Minimum_Income_FourBed"),
    # textOutput("Minimum_Income_FourBed_1"),
    # textOutput("Minimum_Income_FourBed_2"),
    # textOutput("Minimum_Income_FourBed_3"),
    # textOutput("Minimum_Income_FourBed_4"),
    # textOutput("Minimum_Income_FourBed_5"),
    # Five bedroom
    # textOutput("Five_bedroom_value"),
    # textOutput("Five_bedroom_value_1"),
    # textOutput("Five_bedroom_value_2"),
    # textOutput("Five_bedroom_value_3"),
    # textOutput("Five_bedroom_value_4"),
    # textOutput("Five_bedroom_value_5"),
    # textOutput("Five_bedroom_min_share"),
    # textOutput("Five_bedroom_min_share_1"),
    # textOutput("Five_bedroom_min_share_2"),
    # textOutput("Five_bedroom_min_share_3"),
    # textOutput("Five_bedroom_min_share_4"),
    # textOutput("Five_bedroom_min_share_5"),
    # textOutput("Loan_FiveBed"),
    # textOutput("Loan_FiveBed_1"),
    # textOutput("Loan_FiveBed_2"),
    # textOutput("Loan_FiveBed_3"),
    # textOutput("Loan_FiveBed_4"),
    # textOutput("Loan_FiveBed_5"),
    # textOutput("ExpectedDeposit_FiveBed"),
    # textOutput("ExpectedDeposit_FiveBed_1"),
    # textOutput("ExpectedDeposit_FiveBed_2"),
    # textOutput("ExpectedDeposit_FiveBed_3"),
    # textOutput("ExpectedDeposit_FiveBed_4"),
    # textOutput("ExpectedDeposit_FiveBed_5"),
    # textOutput("Minimum_Income_FiveBed"),
    # textOutput("Minimum_Income_FiveBed_1"),
    # textOutput("Minimum_Income_FiveBed_2"),
    # textOutput("Minimum_Income_FiveBed_3"),
    # textOutput("Minimum_Income_FiveBed_4"),
    # textOutput("Minimum_Income_FiveBed_5"),
    # Display the results in a table
    tableHTML_output("results_table_2b"),
    tableHTML_output("results_table_3b"),
    tableHTML_output("results_table_4b"),
    tableHTML_output("results_table_5b")
  )
)

# Define server logic ----
server <- function(input, output, session) {
  # Create a reactiveValues object to store the results
  values <- reactiveValues()
  
  observeEvent(input$calculate, {
    # Check if input$IncomeType is NULL or has zero length
    if (is.null(input$Incometype) || length(input$Incometype) == 0) {
      # Handle the case when input$IncomeType is NULL or has zero length
      # Show a warning message
      showModal(
        modalDialog(
          title = strong("Warning: There is no income value entered.", style = "font-size:16px; color: red;"),
          footer = modalButton("Close")
        )
      )
      return(NULL) # Return early if input$IncomeType is NULL or has zero length
    }
    
    if (input$Incometype > 205000 || input$Incometype < 0) {
      showModal(
        modalDialog(
          title = strong("Warning: Input value exceeds the limits", style = "font-size:16px; color: red;"),
          footer = modalButton("Close")
        )
      )
      updateNumericInput(session, "IncomeType", value = 0)
    }
    
    ## Age, income & deposit ####
    AgeType <- input$AgeType
    
    AgeType_1 <- AgeType + 1
    
    AgeType_2 <- AgeType + 2
    
    AgeType_3 <- AgeType + 3
    
    AgeType_4 <- AgeType + 4
    
    AgeType_5 <- AgeType + 5
    
    # Income increase ####
    ActualIncome <- input$Incometype
    
    ActualIncome_1 <- (input$Incometype * 1.053) + input$Income_Change_1
    
    ActualIncome_2 <- (ActualIncome_1 * 1.053) + input$Income_Change_2
    
    ActualIncome_3 <- (ActualIncome_2 * 1.053) + input$Income_Change_3
    
    ActualIncome_4 <- (ActualIncome_3 * 1.053) + input$Income_Change_4
    
    ActualIncome_5 <- (ActualIncome_4 * 1.053) + input$Income_Change_5
    
    # Deposit increase ####
    Deposit_calculation <- input$SavingsType + input$KiwiSaverType - input$DebtType
    
 #  Deposit_calculation_1 <- input$SavingsType + input$Savings_Change_1 + (input$KiwiSaverType * 1.03 + input$IncomeType*1.06 + 521) 
    
    Deposit_calculation_1 <- (Deposit_calculation + 0.06*ActualIncome_1) + input$Savings_Change_1
    
    Deposit_calculation_2 <- (Deposit_calculation_1 + 0.06*ActualIncome_2) + input$Savings_Change_2
    
    Deposit_calculation_3 <- (Deposit_calculation_2 + 0.06*ActualIncome_3) + input$Savings_Change_3
    
    Deposit_calculation_4 <- (Deposit_calculation_3 + 0.06*ActualIncome_4) + input$Savings_Change_4
    
    Deposit_calculation_5 <- (Deposit_calculation_4 + 0.06*ActualIncome_5) + input$Savings_Change_5
    
    ## Two bedroom values ####

    ## Property inflation 2 bed ####
    Two_bedroom_value <- input$Two_bedroom_value
    
    Two_bedroom_value_1 <- Two_bedroom_value * (1 + (input$Inflation_1/100))
    
    Two_bedroom_value_2 <- Two_bedroom_value_1 * (1 + (input$Inflation_2/100))
    
    Two_bedroom_value_3 <- Two_bedroom_value_2 * (1 + (input$Inflation_3/100))
    
    Two_bedroom_value_4 <- Two_bedroom_value_3 * (1 + (input$Inflation_4/100))
    
    Two_bedroom_value_5 <- Two_bedroom_value_4 * (1 + (input$Inflation_5/100))
    
    ## Min share ####
    Two_bedroom_min_share <- input$Two_bedroom_value * input$WhanauShare
    
    Two_bedroom_min_share_1 <- Two_bedroom_value_1 * input$WhanauShare
    
    Two_bedroom_min_share_2 <- Two_bedroom_value_2 * input$WhanauShare
    
    Two_bedroom_min_share_3 <- Two_bedroom_value_3 * input$WhanauShare
    
    Two_bedroom_min_share_4 <- Two_bedroom_value_4 * input$WhanauShare
    
    Two_bedroom_min_share_5 <- Two_bedroom_value_5 * input$WhanauShare
    
    ## Deposit ####
    ExpectedDeposit_TwoBed <- Two_bedroom_value * 0.05
    
    ExpectedDeposit_TwoBed_1 <- Two_bedroom_value_1 * 0.05
    
    ExpectedDeposit_TwoBed_2 <- Two_bedroom_value_2 * 0.05
    
    ExpectedDeposit_TwoBed_3 <- Two_bedroom_value_3 * 0.05
    
    ExpectedDeposit_TwoBed_4 <- Two_bedroom_value_4 * 0.05
    
    ExpectedDeposit_TwoBed_5 <- Two_bedroom_value_5 * 0.05
    
    # For Two Bedroom
    Effective_Deposit_TwoBed <- case_when(
      Deposit_calculation > ExpectedDeposit_TwoBed ~ ExpectedDeposit_TwoBed,
      TRUE ~ Deposit_calculation)
    
    Effective_Deposit_TwoBed_1 <- case_when(
      Deposit_calculation_1 > ExpectedDeposit_TwoBed_1 ~ ExpectedDeposit_TwoBed_1,
      TRUE ~ Deposit_calculation_1)
    
    Effective_Deposit_TwoBed_2 <- case_when(
      Deposit_calculation_2 > ExpectedDeposit_TwoBed_2 ~ ExpectedDeposit_TwoBed_2,
      TRUE ~ Deposit_calculation_2)
    
    Effective_Deposit_TwoBed_3 <- case_when(
      Deposit_calculation_3 > ExpectedDeposit_TwoBed_3 ~ ExpectedDeposit_TwoBed_3,
      TRUE ~ Deposit_calculation_3)
    
    Effective_Deposit_TwoBed_4 <- case_when(
      Deposit_calculation_4 > ExpectedDeposit_TwoBed_4 ~ ExpectedDeposit_TwoBed_4,
      TRUE ~ Deposit_calculation_4)
    
    Effective_Deposit_TwoBed_5 <- case_when(
      Deposit_calculation_5 > ExpectedDeposit_TwoBed_5 ~ ExpectedDeposit_TwoBed_5,
      TRUE ~ Deposit_calculation_5)
    
    ## Loan for 2 bed ####
    Loan_TwoBed <- case_when(
      Deposit_calculation > ExpectedDeposit_TwoBed ~ Two_bedroom_min_share - Deposit_calculation,
      TRUE ~ Two_bedroom_min_share - ExpectedDeposit_TwoBed)
    
    Loan_TwoBed_1 <- case_when(
      Deposit_calculation_1 > ExpectedDeposit_TwoBed_1 ~ Two_bedroom_min_share_1 - Deposit_calculation_1,
      TRUE ~ Two_bedroom_min_share_1 - ExpectedDeposit_TwoBed_1)
    
    Loan_TwoBed_2 <- case_when(
      Deposit_calculation_2 > ExpectedDeposit_TwoBed_2 ~ Two_bedroom_min_share_2 - Deposit_calculation_2,
      TRUE ~ Two_bedroom_min_share_2 - ExpectedDeposit_TwoBed_2)
    
    Loan_TwoBed_3 <- case_when(
      Deposit_calculation_3 > ExpectedDeposit_TwoBed_3 ~ Two_bedroom_min_share_3 - Deposit_calculation_3,
      TRUE ~ Two_bedroom_min_share_3 - ExpectedDeposit_TwoBed_3)
    
    Loan_TwoBed_4 <- case_when(
      Deposit_calculation_4 > ExpectedDeposit_TwoBed_4 ~ Two_bedroom_min_share_4 - Deposit_calculation_4,
      TRUE ~ Two_bedroom_min_share_4 - ExpectedDeposit_TwoBed_4)
    
    Loan_TwoBed_5 <- case_when(
      Deposit_calculation_5 > ExpectedDeposit_TwoBed_5 ~ Two_bedroom_min_share_5 - Deposit_calculation_5,
      TRUE ~ Two_bedroom_min_share_5 - ExpectedDeposit_TwoBed_5)
    
    # Display the results
    monthlyPayment_TwoBed <- calculateMonthlyPayment(Loan_TwoBed, input$InterestRate_1, mortgageTerm(input$AgeType))
    
    monthlyPayment_TwoBed_1 <- calculateMonthlyPayment(Loan_TwoBed_1, input$InterestRate_1, mortgageTerm(AgeType_1))
    
    monthlyPayment_TwoBed_2 <- calculateMonthlyPayment(Loan_TwoBed_2, input$InterestRate_1, mortgageTerm(AgeType_2))
    
    monthlyPayment_TwoBed_3 <- calculateMonthlyPayment(Loan_TwoBed_3, input$InterestRate_1, mortgageTerm(AgeType_3))
    
    monthlyPayment_TwoBed_4 <- calculateMonthlyPayment(Loan_TwoBed_4, input$InterestRate_1, mortgageTerm(AgeType_4))
    
    monthlyPayment_TwoBed_5 <- calculateMonthlyPayment(Loan_TwoBed_5, input$InterestRate_1, mortgageTerm(AgeType_5))
    
    ## Minimum income for Two Bed ####
    Minimum_Income_TwoBed <- monthlyPayment_TwoBed * 100/input$DebtIncome_Ratio *12
    
    Minimum_Income_TwoBed_1 <- monthlyPayment_TwoBed_1 * 100/input$DebtIncome_Ratio *12
    
    Minimum_Income_TwoBed_2 <- monthlyPayment_TwoBed_2 * 100/input$DebtIncome_Ratio *12
    
    Minimum_Income_TwoBed_3 <- monthlyPayment_TwoBed_3 * 100/input$DebtIncome_Ratio *12
    
    Minimum_Income_TwoBed_4 <- monthlyPayment_TwoBed_4 * 100/input$DebtIncome_Ratio *12
    
    Minimum_Income_TwoBed_5 <- monthlyPayment_TwoBed_5 * 100/input$DebtIncome_Ratio *12
    
    # Three bedroom values ############
    
    ## Property inflation 3 bed ####
    Three_bedroom_value <- input$Three_bedroom_value
    
    Three_bedroom_value_1 <- Three_bedroom_value * (1 + (input$Inflation_1/100))
    
    Three_bedroom_value_2 <- Three_bedroom_value_1 * (1 + (input$Inflation_2/100))
    
    Three_bedroom_value_3 <- Three_bedroom_value_2 * (1 + (input$Inflation_3/100))
    
    Three_bedroom_value_4 <- Three_bedroom_value_3 * (1 + (input$Inflation_4/100))
    
    Three_bedroom_value_5 <- Three_bedroom_value_4 * (1 + (input$Inflation_5/100))
    
    ## Min share ####
    Three_bedroom_min_share <- input$Three_bedroom_value * input$WhanauShare
    
    Three_bedroom_min_share_1 <- Three_bedroom_value_1 * input$WhanauShare
    
    Three_bedroom_min_share_2 <- Three_bedroom_value_2 * input$WhanauShare
   
    Three_bedroom_min_share_3 <- Three_bedroom_value_3 * input$WhanauShare
    
    Three_bedroom_min_share_4 <- Three_bedroom_value_4 * input$WhanauShare
    
    Three_bedroom_min_share_5 <- Three_bedroom_value_5 * input$WhanauShare
    
    ## Deposit ####
    ExpectedDeposit_ThreeBed <- input$Three_bedroom_value * 0.05
    
    ExpectedDeposit_ThreeBed_1 <- Three_bedroom_value_1 * 0.05
    
    ExpectedDeposit_ThreeBed_2 <- Three_bedroom_value_2 * 0.05
    
    ExpectedDeposit_ThreeBed_3 <- Three_bedroom_value_3 * 0.05
    
    ExpectedDeposit_ThreeBed_4 <- Three_bedroom_value_4 * 0.05
    
    ExpectedDeposit_ThreeBed_5 <- Three_bedroom_value_5 * 0.05
    
    # For Three Bedroom
    Effective_Deposit_ThreeBed <- case_when(
      Deposit_calculation > ExpectedDeposit_ThreeBed ~ ExpectedDeposit_ThreeBed,
      TRUE ~ Deposit_calculation)
    
    Effective_Deposit_ThreeBed_1 <- case_when(
      Deposit_calculation_1 > ExpectedDeposit_ThreeBed_1 ~ ExpectedDeposit_ThreeBed_1,
      TRUE ~ Deposit_calculation_1)
    
    Effective_Deposit_ThreeBed_2 <- case_when(
      Deposit_calculation_2 > ExpectedDeposit_ThreeBed_2 ~ ExpectedDeposit_ThreeBed_2,
      TRUE ~ Deposit_calculation_2)
    
    Effective_Deposit_ThreeBed_3 <- case_when(
      Deposit_calculation_3 > ExpectedDeposit_ThreeBed_3 ~ ExpectedDeposit_ThreeBed_3,
      TRUE ~ Deposit_calculation_3)
    
    Effective_Deposit_ThreeBed_4 <- case_when(
      Deposit_calculation_4 > ExpectedDeposit_ThreeBed_4 ~ ExpectedDeposit_ThreeBed_4,
      TRUE ~ Deposit_calculation_4)
    
    Effective_Deposit_ThreeBed_5 <- case_when(
      Deposit_calculation_5 > ExpectedDeposit_ThreeBed_5 ~ ExpectedDeposit_ThreeBed_5,
      TRUE ~ Deposit_calculation_5)
    
    ## Loan for 3 bed ####
    Loan_ThreeBed <- case_when(
      Deposit_calculation > ExpectedDeposit_ThreeBed ~ Three_bedroom_min_share - Deposit_calculation,
      TRUE ~ Three_bedroom_min_share - ExpectedDeposit_ThreeBed)
    
    Loan_ThreeBed_1 <- case_when(
      Deposit_calculation_1 > ExpectedDeposit_ThreeBed_1 ~ Three_bedroom_min_share_1 - Deposit_calculation_1,
      TRUE ~ Three_bedroom_min_share_1 - ExpectedDeposit_ThreeBed_1)
    
    Loan_ThreeBed_2 <- case_when(
      Deposit_calculation_2 > ExpectedDeposit_ThreeBed_2 ~ Three_bedroom_min_share_2 - Deposit_calculation_2,
      TRUE ~ Three_bedroom_min_share_2 - ExpectedDeposit_ThreeBed_2)
    
    Loan_ThreeBed_3 <- case_when(
      Deposit_calculation_3 > ExpectedDeposit_ThreeBed_3 ~ Three_bedroom_min_share_3 - Deposit_calculation_3,
      TRUE ~ Three_bedroom_min_share_3 - ExpectedDeposit_ThreeBed_3)
    
    Loan_ThreeBed_4 <- case_when(
      Deposit_calculation_4 > ExpectedDeposit_ThreeBed_4 ~ Three_bedroom_min_share_4 - Deposit_calculation_4,
      TRUE ~ Three_bedroom_min_share_4 - ExpectedDeposit_ThreeBed_4)
    
    Loan_ThreeBed_5 <- case_when(
      Deposit_calculation_5 > ExpectedDeposit_ThreeBed_5 ~ Three_bedroom_min_share_5 - Deposit_calculation_5,
      TRUE ~ Three_bedroom_min_share_5 - ExpectedDeposit_ThreeBed_5)
    
    # Display the results
    monthlyPayment_ThreeBed <- calculateMonthlyPayment(Loan_ThreeBed, input$InterestRate_1, mortgageTerm(input$AgeType))
    
    monthlyPayment_ThreeBed_1 <- calculateMonthlyPayment(Loan_ThreeBed_1, input$InterestRate_1, mortgageTerm(AgeType_1))
    
    monthlyPayment_ThreeBed_2 <- calculateMonthlyPayment(Loan_ThreeBed_2, input$InterestRate_1, mortgageTerm(AgeType_2))
    
    monthlyPayment_ThreeBed_3 <- calculateMonthlyPayment(Loan_ThreeBed_3, input$InterestRate_1, mortgageTerm(AgeType_3))
    
    monthlyPayment_ThreeBed_4 <- calculateMonthlyPayment(Loan_ThreeBed_4, input$InterestRate_1, mortgageTerm(AgeType_4))
    
    monthlyPayment_ThreeBed_5 <- calculateMonthlyPayment(Loan_ThreeBed_5, input$InterestRate_1, mortgageTerm(AgeType_5))
    
    ## Minimum income for Three Bed ####
    Minimum_Income_ThreeBed <- monthlyPayment_ThreeBed * 100/input$DebtIncome_Ratio *12

    Minimum_Income_ThreeBed_1 <- monthlyPayment_ThreeBed_1 * 100/input$DebtIncome_Ratio *12

    Minimum_Income_ThreeBed_2 <- monthlyPayment_ThreeBed_2 * 100/input$DebtIncome_Ratio *12

    Minimum_Income_ThreeBed_3 <- monthlyPayment_ThreeBed_3 * 100/input$DebtIncome_Ratio *12

    Minimum_Income_ThreeBed_4 <- monthlyPayment_ThreeBed_4 * 100/input$DebtIncome_Ratio *12

    Minimum_Income_ThreeBed_5 <- monthlyPayment_ThreeBed_5 * 100/input$DebtIncome_Ratio *12

    # Four bedroom values ####
    ## Property inflation 4 bed ####
    Four_bedroom_value <- input$Four_bedroom_value
    
    Four_bedroom_value_1 <- Four_bedroom_value * (1 + (input$Inflation_1/100))
    
    Four_bedroom_value_2 <- Four_bedroom_value_1 * (1 + (input$Inflation_2/100))
    
    Four_bedroom_value_3 <- Four_bedroom_value_2 * (1 + (input$Inflation_3/100))

    Four_bedroom_value_4 <- Four_bedroom_value_3 * (1 + (input$Inflation_4/100))

    Four_bedroom_value_5 <- Four_bedroom_value_4 * (1 + (input$Inflation_5/100))
    
    ## Min share ####
    Four_bedroom_min_share <- input$Four_bedroom_value * input$WhanauShare
    
    Four_bedroom_min_share_1 <- Four_bedroom_value_1 * input$WhanauShare

    Four_bedroom_min_share_2 <- Four_bedroom_value_2 * input$WhanauShare

    Four_bedroom_min_share_3 <- Four_bedroom_value_3 * input$WhanauShare

    Four_bedroom_min_share_4 <- Four_bedroom_value_4 * input$WhanauShare

    Four_bedroom_min_share_5 <- Four_bedroom_value_5 * input$WhanauShare
    
    ## Deposit increase ####
    ExpectedDeposit_FourBed <- input$Four_bedroom_value * 0.05

    ExpectedDeposit_FourBed_1 <- Four_bedroom_value_1 * 0.05

    ExpectedDeposit_FourBed_2 <- Four_bedroom_value_2 * 0.05

    ExpectedDeposit_FourBed_3 <- Four_bedroom_value_3 * 0.05

    ExpectedDeposit_FourBed_4 <- Four_bedroom_value_4 * 0.05

    ExpectedDeposit_FourBed_5 <- Four_bedroom_value_5 * 0.05
    
    # For Four Bedroom
    Effective_Deposit_FourBed <- case_when(
      Deposit_calculation > ExpectedDeposit_FourBed ~ ExpectedDeposit_FourBed,
      TRUE ~ Deposit_calculation)
    
    Effective_Deposit_FourBed_1 <- case_when(
      Deposit_calculation_1 > ExpectedDeposit_FourBed_1 ~ ExpectedDeposit_FourBed_1,
      TRUE ~ Deposit_calculation_1)
    
    Effective_Deposit_FourBed_2 <- case_when(
      Deposit_calculation_2 > ExpectedDeposit_FourBed_2 ~ ExpectedDeposit_FourBed_2,
      TRUE ~ Deposit_calculation_2)
    
    Effective_Deposit_FourBed_3 <- case_when(
      Deposit_calculation_3 > ExpectedDeposit_FourBed_3 ~ ExpectedDeposit_FourBed_3,
      TRUE ~ Deposit_calculation_3)
    
    Effective_Deposit_FourBed_4 <- case_when(
      Deposit_calculation_4 > ExpectedDeposit_FourBed_4 ~ ExpectedDeposit_FourBed_4,
      TRUE ~ Deposit_calculation_4)
    
    Effective_Deposit_FourBed_5 <- case_when(
      Deposit_calculation_5 > ExpectedDeposit_FourBed_5 ~ ExpectedDeposit_FourBed_5,
      TRUE ~ Deposit_calculation_5)
    
    ## Loan for 4 bed ####
    Loan_FourBed <- case_when(
      Deposit_calculation > ExpectedDeposit_FourBed ~ Four_bedroom_min_share - Deposit_calculation,
      TRUE ~ Four_bedroom_min_share - ExpectedDeposit_FourBed)
    
    Loan_FourBed_1 <- case_when(
      Deposit_calculation_1 > ExpectedDeposit_FourBed_1 ~ Four_bedroom_min_share_1 - Deposit_calculation_1,
      TRUE ~ Four_bedroom_min_share_1 - ExpectedDeposit_FourBed_1)
    
    Loan_FourBed_2 <- case_when(
      Deposit_calculation_2 > ExpectedDeposit_FourBed_2 ~ Four_bedroom_min_share_2 - Deposit_calculation_2,
      TRUE ~ Four_bedroom_min_share_2 - ExpectedDeposit_FourBed_2)
    
    Loan_FourBed_3 <- case_when(
      Deposit_calculation_3 > ExpectedDeposit_FourBed_3 ~ Four_bedroom_min_share_3 - Deposit_calculation_3,
      TRUE ~ Four_bedroom_min_share_3 - ExpectedDeposit_FourBed_3)
    
    Loan_FourBed_4 <- case_when(
      Deposit_calculation_4 > ExpectedDeposit_FourBed_4 ~ Four_bedroom_min_share_4 - Deposit_calculation_4,
      TRUE ~ Four_bedroom_min_share_4 - ExpectedDeposit_FourBed_4)
    
    Loan_FourBed_5 <- case_when(
      Deposit_calculation_5 > ExpectedDeposit_FourBed_5 ~ Four_bedroom_min_share_5 - Deposit_calculation_5,
      TRUE ~ Four_bedroom_min_share_5 - ExpectedDeposit_FourBed_5)
    
    # Display the results
    monthlyPayment_FourBed <- calculateMonthlyPayment(Loan_FourBed, input$InterestRate_1, mortgageTerm(input$AgeType))
    
    monthlyPayment_FourBed_1 <- calculateMonthlyPayment(Loan_FourBed_1, input$InterestRate_1, mortgageTerm(AgeType_1))
    
    monthlyPayment_FourBed_2 <- calculateMonthlyPayment(Loan_FourBed_2, input$InterestRate_1, mortgageTerm(AgeType_2))
    
    monthlyPayment_FourBed_3 <- calculateMonthlyPayment(Loan_FourBed_3, input$InterestRate_1, mortgageTerm(AgeType_3))
    
    monthlyPayment_FourBed_4 <- calculateMonthlyPayment(Loan_FourBed_4, input$InterestRate_1, mortgageTerm(AgeType_4))
    
    monthlyPayment_FourBed_5 <- calculateMonthlyPayment(Loan_FourBed_5, input$InterestRate_1, mortgageTerm(AgeType_5))
    
    ## Minimum income for Four Bed ####
    Minimum_Income_FourBed <- monthlyPayment_FourBed * 100/input$DebtIncome_Ratio *12
    
    Minimum_Income_FourBed_1 <- monthlyPayment_FourBed_1 * 100/input$DebtIncome_Ratio *12
    
    Minimum_Income_FourBed_2 <- monthlyPayment_FourBed_2 * 100/input$DebtIncome_Ratio *12
    
    Minimum_Income_FourBed_3 <- monthlyPayment_FourBed_3 * 100/input$DebtIncome_Ratio *12
    
    Minimum_Income_FourBed_4 <- monthlyPayment_FourBed_4 * 100/input$DebtIncome_Ratio *12
    
    Minimum_Income_FourBed_5 <- monthlyPayment_FourBed_5 * 100/input$DebtIncome_Ratio *12
    
    # Five bedroom values ####
    ## Property inflation 5 bed ####
    Five_bedroom_value <- input$Five_bedroom_value
    
    Five_bedroom_value_1 <- Five_bedroom_value * (1 + (input$Inflation_1/100))
    
    Five_bedroom_value_2 <- Five_bedroom_value_1 * (1 + (input$Inflation_2/100))
    
    Five_bedroom_value_3 <- Five_bedroom_value_2 * (1 + (input$Inflation_3/100))
    
    Five_bedroom_value_4 <- Five_bedroom_value_3 * (1 + (input$Inflation_4/100))
    
    Five_bedroom_value_5 <- Five_bedroom_value_4 * (1 + (input$Inflation_5/100))
    
    ## Min share ####
    Five_bedroom_min_share <- input$Five_bedroom_value * input$WhanauShare
    
    Five_bedroom_min_share_1 <- Five_bedroom_value_1 * input$WhanauShare
    
    Five_bedroom_min_share_2 <- Five_bedroom_value_2 * input$WhanauShare
   
    Five_bedroom_min_share_3 <- Five_bedroom_value_3 * input$WhanauShare
    
    Five_bedroom_min_share_4 <- Five_bedroom_value_4 * input$WhanauShare
    
    Five_bedroom_min_share_5 <- Five_bedroom_value_5 * input$WhanauShare
    
    ## Deposit ####
    ExpectedDeposit_FiveBed <- input$Five_bedroom_value * 0.05
    
    ExpectedDeposit_FiveBed_1 <- Five_bedroom_value_1 * 0.05
    
    ExpectedDeposit_FiveBed_2 <- Five_bedroom_value_2 * 0.05
    
    ExpectedDeposit_FiveBed_3 <- Five_bedroom_value_3 * 0.05
    
    ExpectedDeposit_FiveBed_4 <- Five_bedroom_value_4 * 0.05
    
    ExpectedDeposit_FiveBed_5 <- Five_bedroom_value_5 * 0.05
    
    # For Three Bedroom
    Effective_Deposit_FiveBed <- case_when(
      Deposit_calculation > ExpectedDeposit_FiveBed ~ ExpectedDeposit_FiveBed,
      TRUE ~ Deposit_calculation)
    
    Effective_Deposit_FiveBed_1 <- case_when(
      Deposit_calculation_1 > ExpectedDeposit_FiveBed_1 ~ Deposit_calculation_1,
      TRUE ~ ExpectedDeposit_FiveBed_1)
    
    Effective_Deposit_FiveBed_2 <- case_when(
      Deposit_calculation_2 > ExpectedDeposit_FiveBed_2 ~ Deposit_calculation_2,
      TRUE ~ ExpectedDeposit_FiveBed_2)
    
    Effective_Deposit_FiveBed_3 <- case_when(
      Deposit_calculation_3 > ExpectedDeposit_FiveBed_3 ~ Deposit_calculation_3,
      TRUE ~ ExpectedDeposit_FiveBed_3)
    
    Effective_Deposit_FiveBed_4 <- case_when(
      Deposit_calculation_4 > ExpectedDeposit_FiveBed_4 ~ Deposit_calculation_4,
      TRUE ~ ExpectedDeposit_FiveBed_4)
    
    Effective_Deposit_FiveBed_5 <- case_when(
      Deposit_calculation_5 > ExpectedDeposit_FiveBed_5 ~ Deposit_calculation_5, 
      TRUE ~ ExpectedDeposit_FiveBed_5)
    
    ## Loan for 5 bed ####
    Loan_FiveBed <- case_when(
      Deposit_calculation > ExpectedDeposit_FiveBed ~ Five_bedroom_min_share - Deposit_calculation,
      TRUE ~ Five_bedroom_min_share - ExpectedDeposit_FiveBed)
    
    Loan_FiveBed_1 <- case_when(
      Deposit_calculation_1 > ExpectedDeposit_FiveBed_1 ~ Five_bedroom_min_share_1 - Deposit_calculation_1,
      TRUE ~ Five_bedroom_min_share_1 - ExpectedDeposit_FiveBed_1)
    
    Loan_FiveBed_2 <- case_when(
      Deposit_calculation_2 > ExpectedDeposit_FiveBed_2 ~ Five_bedroom_min_share_2 - Deposit_calculation_2,
      TRUE ~ Five_bedroom_min_share_2 - ExpectedDeposit_FiveBed_2)
    
    Loan_FiveBed_3 <- case_when(
      Deposit_calculation_3 > ExpectedDeposit_FiveBed_3 ~ Five_bedroom_min_share_3 - Deposit_calculation_3,
      TRUE ~ Five_bedroom_min_share_3 - ExpectedDeposit_FiveBed_3)
    
    Loan_FiveBed_4 <- case_when(
      Deposit_calculation_4 > ExpectedDeposit_FiveBed_4 ~ Five_bedroom_min_share_4 - Deposit_calculation_4,
      TRUE ~ Five_bedroom_min_share_4 - ExpectedDeposit_FiveBed_4)
    
    Loan_FiveBed_5 <- case_when(
      Deposit_calculation_5 > ExpectedDeposit_FiveBed_5 ~ Five_bedroom_min_share_5 - Deposit_calculation_5,
      TRUE ~ Five_bedroom_min_share_5 - ExpectedDeposit_FiveBed_5)
    
    # Display the results
    monthlyPayment_FiveBed <- calculateMonthlyPayment(Loan_FiveBed, input$InterestRate_1, mortgageTerm(input$AgeType))
    monthlyPayment_FiveBed_1 <- calculateMonthlyPayment(Loan_FiveBed_1, input$InterestRate_1, mortgageTerm(AgeType_1))
    monthlyPayment_FiveBed_2 <- calculateMonthlyPayment(Loan_FiveBed_2, input$InterestRate_1, mortgageTerm(AgeType_2))
    monthlyPayment_FiveBed_3 <- calculateMonthlyPayment(Loan_FiveBed_3, input$InterestRate_1, mortgageTerm(AgeType_3))
    monthlyPayment_FiveBed_4 <- calculateMonthlyPayment(Loan_FiveBed_4, input$InterestRate_1, mortgageTerm(AgeType_4))
    monthlyPayment_FiveBed_5 <- calculateMonthlyPayment(Loan_FiveBed_5, input$InterestRate_1, mortgageTerm(AgeType_5))
    
    ## Minimum income for Five Bed ####
    Minimum_Income_FiveBed <- monthlyPayment_FiveBed * 100/input$DebtIncome_Ratio * 12
    
    Minimum_Income_FiveBed_1 <- monthlyPayment_FiveBed_1 * 100/input$DebtIncome_Ratio *12
    
    Minimum_Income_FiveBed_2 <- monthlyPayment_FiveBed_2 * 100/input$DebtIncome_Ratio *12
    
    Minimum_Income_FiveBed_3 <- monthlyPayment_FiveBed_3 * 100/input$DebtIncome_Ratio *12
    
    Minimum_Income_FiveBed_4 <- monthlyPayment_FiveBed_4 * 100/input$DebtIncome_Ratio *12
    
    Minimum_Income_FiveBed_5 <- monthlyPayment_FiveBed_5 * 100/input$DebtIncome_Ratio *12
    
    # Store the calculated values in the reactiveValues object / May need to add the Three decimal places to each of these calculations
    
    # Define a function to pad vectors with NA values
    pad_with_na <- function(vector, target_length) {
      if (length(vector) < target_length) {
        additional_na <- rep(NA, target_length - length(vector))
        return(c(vector, additional_na))
      } else {
        return(vector)
      }
    }
    
    # Two bedroom values / Example of replacing each of your variables with a call to pad_with_na function
    values$results_df_2b <- data.frame(
      'Two bedroom value' = paste0('$', scales::comma(pad_with_na(c(Two_bedroom_value, Two_bedroom_value_1, Two_bedroom_value_2, Two_bedroom_value_3, Two_bedroom_value_4, Two_bedroom_value_5), 6))),
      'Two bedroom min share' = paste0('$', scales::comma(pad_with_na(round(c(Two_bedroom_min_share, Two_bedroom_min_share_1, Two_bedroom_min_share_2, Two_bedroom_min_share_3, Two_bedroom_min_share_4, Two_bedroom_min_share_5), 2), 6))),
      'Two bedroom minimum deposit' = paste0('$', scales::comma((c(ExpectedDeposit_TwoBed, ExpectedDeposit_TwoBed_1, ExpectedDeposit_TwoBed_2, ExpectedDeposit_TwoBed_3, ExpectedDeposit_TwoBed_4, ExpectedDeposit_TwoBed_5)), 6)),
      'Minimum loan' = paste0('$', scales::comma(pad_with_na(round(c(Loan_TwoBed, Loan_TwoBed_1, Loan_TwoBed_2, Loan_TwoBed_3, Loan_TwoBed_4, Loan_TwoBed_5), 2), 6))),
      'Minimum GHI' = paste0('$', scales::comma(pad_with_na(round(c(Minimum_Income_TwoBed, Minimum_Income_TwoBed_1, Minimum_Income_TwoBed_2, Minimum_Income_TwoBed_3, Minimum_Income_TwoBed_4, Minimum_Income_TwoBed_5), 2), 6))),
      Age = pad_with_na(c(AgeType, AgeType_1, AgeType_2, AgeType_3, AgeType_4, AgeType_5), 6),
      'GHI' = paste0('$', scales::comma(pad_with_na(round(c(ActualIncome, ActualIncome_1, ActualIncome_2, ActualIncome_3, ActualIncome_4, ActualIncome_5), 2), 6))),
      'Deposit calculation' = paste0('$', scales::comma(pad_with_na(round(c(Deposit_calculation, Deposit_calculation_1, Deposit_calculation_2, Deposit_calculation_3, Deposit_calculation_4, Deposit_calculation_5), 2), 6)))
    )
    
    # Three bedroom values
    values$results_df_3b <- data.frame(
      'Three bedroom value' = paste0('$', scales::comma(pad_with_na(c(Three_bedroom_value, Three_bedroom_value_1, Three_bedroom_value_2, Three_bedroom_value_3, Three_bedroom_value_4, Three_bedroom_value_5), 6))),
      'Three bedroom min share' = paste0('$', scales::comma(pad_with_na(round(c(Three_bedroom_min_share, Three_bedroom_min_share_1, Three_bedroom_min_share_2, Three_bedroom_min_share_3, Three_bedroom_min_share_4, Three_bedroom_min_share_5), 2), 6))),
      'Three bedroom minimum deposit' = paste0('$', scales::comma(pad_with_na(round(c(ExpectedDeposit_ThreeBed, ExpectedDeposit_ThreeBed_1, ExpectedDeposit_ThreeBed_2, ExpectedDeposit_ThreeBed_3, ExpectedDeposit_ThreeBed_4, ExpectedDeposit_ThreeBed_5), 2), 6))),
      'Minimum loan' = paste0('$', scales::comma(pad_with_na(round(c(Loan_ThreeBed, Loan_ThreeBed_1, Loan_ThreeBed_2, Loan_ThreeBed_3, Loan_ThreeBed_4, Loan_ThreeBed_5), 2), 6))),
      'Minimum GHI' = paste0('$', scales::comma(pad_with_na(round(c(Minimum_Income_ThreeBed, Minimum_Income_ThreeBed_1, Minimum_Income_ThreeBed_2, Minimum_Income_ThreeBed_3, Minimum_Income_ThreeBed_4, Minimum_Income_ThreeBed_5), 2), 6))),
      Age = pad_with_na(c(AgeType, AgeType_1, AgeType_2, AgeType_3, AgeType_4, AgeType_5), 6),
      'GHI' = paste0('$', scales::comma(pad_with_na(round(c(ActualIncome, ActualIncome_1, ActualIncome_2, ActualIncome_3, ActualIncome_4, ActualIncome_5), 2), 6))),
      'Deposit calculation' = paste0('$', scales::comma(pad_with_na(round(c(Deposit_calculation, Deposit_calculation_1, Deposit_calculation_2, Deposit_calculation_3, Deposit_calculation_4, Deposit_calculation_5), 2), 6)))
    )
    
    # Four bedroom values
    values$results_df_4b <- data.frame(
      'Four bedroom value' = paste0('$', scales::comma(pad_with_na(c(Four_bedroom_value, Four_bedroom_value_1, Four_bedroom_value_2, Four_bedroom_value_3, Four_bedroom_value_4, Four_bedroom_value_5), 6))),
      'Four bedroom min share' = paste0('$', scales::comma(pad_with_na(round(c(Four_bedroom_min_share, Four_bedroom_min_share_1, Four_bedroom_min_share_2, Four_bedroom_min_share_3, Four_bedroom_min_share_4, Four_bedroom_min_share_5), 2), 6))),
      'Four bedroom minimum deposit' = paste0('$', scales::comma(pad_with_na(round(c(ExpectedDeposit_FourBed, ExpectedDeposit_FourBed_1, ExpectedDeposit_FourBed_2, ExpectedDeposit_FourBed_3, ExpectedDeposit_FourBed_4, ExpectedDeposit_FourBed_5), 2), 6))),
      'Minimum loan' = paste0('$', scales::comma(pad_with_na(round(c(Loan_FourBed, Loan_FourBed_1, Loan_FourBed_2, Loan_FourBed_3, Loan_FourBed_4, Loan_FourBed_5), 2), 6))),
      'Minimum GHI' = paste0('$', scales::comma(pad_with_na(round(c(Minimum_Income_FourBed, Minimum_Income_FourBed_1, Minimum_Income_FourBed_2, Minimum_Income_FourBed_3, Minimum_Income_FourBed_4, Minimum_Income_FourBed_5), 2), 6))),
      Age = pad_with_na(c(AgeType, AgeType_1, AgeType_2, AgeType_3, AgeType_4, AgeType_5), 6),
      'GHI' = paste0('$', scales::comma(pad_with_na(round(c(ActualIncome, ActualIncome_1, ActualIncome_2, ActualIncome_3, ActualIncome_4, ActualIncome_5), 2), 6))),
      'Deposit calculation' = paste0('$', scales::comma(pad_with_na(round(c(Deposit_calculation, Deposit_calculation_1, Deposit_calculation_2, Deposit_calculation_3, Deposit_calculation_4, Deposit_calculation_5), 2), 6)))
    )
    
    # Five bedroom values
    values$results_df_5b <- data.frame(
      'Five bedroom value' = paste0('$', scales::comma(pad_with_na(c(Five_bedroom_value, Five_bedroom_value_1, Five_bedroom_value_2, Five_bedroom_value_3, Five_bedroom_value_4, Five_bedroom_value_5), 6))),
      'Five bedroom min share' = paste0('$', scales::comma(pad_with_na(round(c(Five_bedroom_min_share, Five_bedroom_min_share_1, Five_bedroom_min_share_2, Five_bedroom_min_share_3, Five_bedroom_min_share_4, Five_bedroom_min_share_5), 2), 6))),
      'Five bedroom minimum deposit' = paste0('$', scales::comma(pad_with_na(round(c(ExpectedDeposit_FiveBed, ExpectedDeposit_FiveBed_1, ExpectedDeposit_FiveBed_2, ExpectedDeposit_FiveBed_3, ExpectedDeposit_FiveBed_4, ExpectedDeposit_FiveBed_5), 2), 6))),
      'Minimum loan' = paste0('$', scales::comma(pad_with_na(round(c(Loan_FiveBed, Loan_FiveBed_1, Loan_FiveBed_2, Loan_FiveBed_3, Loan_FiveBed_4, Loan_FiveBed_5), 2), 6))),
      'Minimum GHI' = paste0('$', scales::comma(pad_with_na(round(c(Minimum_Income_FiveBed, Minimum_Income_FiveBed_1, Minimum_Income_FiveBed_2, Minimum_Income_FiveBed_3, Minimum_Income_FiveBed_4, Minimum_Income_FiveBed_5), 2), 6))),
      Age = pad_with_na(c(AgeType, AgeType_1, AgeType_2, AgeType_3, AgeType_4, AgeType_5), 6),
      'GHI' = paste0('$', scales::comma(pad_with_na(round(c(ActualIncome, ActualIncome_1, ActualIncome_2, ActualIncome_3, ActualIncome_4, ActualIncome_5), 2), 6))),
      'Deposit calculation' = paste0('$', scales::comma(pad_with_na(round(c(Deposit_calculation, Deposit_calculation_1, Deposit_calculation_2, Deposit_calculation_3, Deposit_calculation_4, Deposit_calculation_5), 2), 6)))
    )
    
    # Transpose the 2 bedroom dataframe
    values$results_df_2b <- as.data.frame(t(values$results_df_2b))
    # Rename the columns
    colnames(values$results_df_2b) <- c('Now', '1 year', '2 years', '3 years', '4 years', '5 years')
    # Set row names
    rownames(values$results_df_2b) <- c('Two bedroom value', 'Two bedroom min share', 'Expected deposit', 
                                        'Minimum loan', 'Minimum GHI', 'Age', 'Actual GHI', 'Deposit calculation')
    
    # Transpose the 3 bedroom dataframe
    values$results_df_3b <- as.data.frame(t(values$results_df_3b))
    # Rename the columns
    colnames(values$results_df_3b) <- c('Now', '1 year', '2 years', '3 years', '4 years', '5 years')
    # Set row names
    rownames(values$results_df_3b) <- c('Three bedroom value', 'Three bedroom min share', 'Expected deposit', 
                                        'Minimum loan', 'Minimum GHI', 'Age', 'Actual GHI', 'Deposit calculation')
    
    # Transpose the 4 bedroom dataframe
    values$results_df_4b <- as.data.frame(t(values$results_df_4b))
    # Rename the columns
    colnames(values$results_df_4b) <- c('Now', '1 year', '2 years', '3 years', '4 years', '5 years')
    # Set row names
    rownames(values$results_df_4b) <- c('Four bedroom value', 'Four bedroom min share', 'Expected deposit', 
                                        'Minimum loan', 'Minimum GHI', 'Age', 'Actual GHI', 'Deposit calculation')
    
    # Transpose the 5 bedroom dataframe
    values$results_df_5b <- as.data.frame(t(values$results_df_5b))
    # Rename the columns
    colnames(values$results_df_5b) <- c('Now', '1 year', '2 years', '3 years', '4 years', '5 years')
    # Set row names
    rownames(values$results_df_5b) <- c('Five bedroom value', 'Five bedroom min share', 'Expected deposit', 
                                        'Minimum loan', 'Minimum GHI', 'Age', 'Actual GHI', 'Deposit calculation')
    
    # Helper function to create a styled table
    createStyledTable <- function(df, header, cell_width, table_title) {
      # Convert the list to a matrix
      css_matrix <- matrix(
        "background-color: white; color: black;",
        ncol = ncol(df),
        nrow = nrow(df)
      )
      
      # Apply custom styles to cells in the "Age" row based on the conditions
      age_row_index <- which(rownames(df) == "Age")
      css_matrix[age_row_index, ] <- ifelse(df["Age", ] > 40, "background-color: red; color: white;", "background-color: green; color: white;")
      
      # Apply custom styles to cells in other rows
      css_matrix[-age_row_index, ] <- "background-color: white; color: black;"
      
      # Apply custom styles to control column widths
      css_column_widths <- rep(cell_width, ncol(df))
      
      # Set row names from the original data frame
      rownames(css_matrix) <- rownames(df)
      
      # Apply custom styles to cells in the "Actual GHI" row based on the conditions
      Actual_GHI_row_index <- which(rownames(df) == "Actual GHI")
      
      # Extract numerical values without '$' and ',' for Actual GHI and Minimum GHI
      actual_ghi_value <- as.numeric(gsub("[$,]", "", df["Actual GHI", ]))
      minimum_ghi_value <- as.numeric(gsub("[$,]", "", df["Minimum GHI", ]))
      
      # Apply custom styles based on comparison
      css_matrix[Actual_GHI_row_index, ] <- ifelse(actual_ghi_value >= minimum_ghi_value, 
                                                   "background-color: green; color: white;", 
                                                   "background-color: red; color: white;")
      
      # Apply custom styles to cells in the "Deposit calculation" row based on the conditions
      deposit_calculation_row_index <- which(rownames(df) == "Deposit calculation")
      deposit_values <- as.numeric(gsub("[$,]", "", df["Deposit calculation", ]))  # Extract numerical values without '$' and ','
      expected_deposit_values <- as.numeric(gsub("[$,]", "", df["Expected deposit", ]))  # Extract numerical values without '$' and ','
      
      css_matrix[deposit_calculation_row_index, ] <- ifelse(deposit_values >= expected_deposit_values, "background-color: green; color: white;", "background-color: red; color: white;")
      
      # Apply custom styles to cells in the "Age" row
      conditional_table <- htmlTable::htmlTable(
        df,
        header = header,  # Specify the header
        caption = paste0("<b>", table_title, "</b>"),  # Bold the table title
        css.cell = css_matrix,
        width.cell = css_column_widths
      )
      
      return(tagList(HTML(conditional_table)))
    }
    
    # Render results for 2 bedroom table
    output$results_table_2b <- renderUI({
      createStyledTable(
        values$results_df_2b,
        c('Now', '1 year', '2 years', '3 years', '4 years', '5 years'),
        "100px",
        "Two bedroom typology"  # Specify the table title
      )
    })
    
    # Render results for 3 bedroom table
    output$results_table_3b <- renderUI({
      createStyledTable(
        values$results_df_3b,
        c('Now', '1 year', '2 years', '3 years', '4 years', '5 years'),
        "100px",
        "Three bedroom typology"  # Specify the table title
      )
    })
    
    # Render results for 4 bedroom table
    output$results_table_4b <- renderUI({
      createStyledTable(
        values$results_df_4b,
        c('Now', '1 year', '2 years', '3 years', '4 years', '5 years'),
        "100px",
        "Four bedroom typology"  # Specify the table title
      )
    })
    
    # Render results for 5 bedroom table
    output$results_table_5b <- renderUI({
      createStyledTable(
        values$results_df_5b,
        c('Now', '1 year', '2 years', '3 years', '4 years', '5 years'),
        "100px",
        "Five bedroom typology"  # Specify the table title
      )
    })
    
    createExcelFile <- function(df_list, file_name) {
      wb <- createWorkbook()
      
      # Merge the dataframes into one
      merged_df <- do.call(rbind, df_list)
      
      # Ensure row names are preserved
      merged_df <- cbind(RowNames = rownames(merged_df), merged_df)
      
      # Add a new worksheet
      addWorksheet(wb, "Merged Results")
      
      # Write the merged dataframe to the sheet
      writeData(wb, sheet = 1, x = merged_df, startCol = 1, startRow = 1, rowNames = FALSE)
      
      # Apply conditional formatting for rows starting with "Age"
      age_row_indices <- grep("^Age", rownames(merged_df))  # Find rows starting with "Age"
      if (length(age_row_indices) > 0) {
        for (age_row_index in age_row_indices) {  # Apply to all rows starting with "Age"
          age_values <- as.numeric(gsub("[$,]", "", merged_df[age_row_index, -1]))  # Exclude RowNames column
          age_values[is.na(age_values)] <- 0  # Replace NA values with 0
          
          for (col in 2:ncol(merged_df)) {  # Start from column 2 to skip RowNames
            color <- ifelse(age_values[col-1] > 40, "#FF0000", "#00FF00")  # Red if > 40, Green otherwise
            addStyle(wb, sheet = 1, style = createStyle(fgFill = color), rows = age_row_index + 1, cols = col)
          }
        }
      }
      
      # Apply conditional formatting for "Actual GHI" and compare with "Minimum GHI"
      
      # 2-Bedroom (GHI, Minimum GHI)
      actual_ghi_2b_row <- grep("^Actual GHI", rownames(merged_df))
      minimum_ghi_2b_row <- grep("^Minimum GHI", rownames(merged_df))
      if (length(actual_ghi_2b_row) > 0 && length(minimum_ghi_2b_row) > 0) {
        actual_ghi_values_2b <- as.numeric(gsub("[$,]", "", merged_df[actual_ghi_2b_row, -1]))  # Exclude RowNames column
        minimum_ghi_values_2b <- as.numeric(gsub("[$,]", "", merged_df[minimum_ghi_2b_row, -1]))  # Exclude RowNames column
        actual_ghi_values_2b[is.na(actual_ghi_values_2b)] <- 0
        minimum_ghi_values_2b[is.na(minimum_ghi_values_2b)] <- 0
        
        for (col in 2:ncol(merged_df)) {  # Start from column 2 to skip RowNames
          color <- ifelse(actual_ghi_values_2b[col-1] >= minimum_ghi_values_2b[col-1], "#00FF00", "#FF0000")  # Green if >= Minimum GHI, Red otherwise
          addStyle(wb, sheet = 1, style = createStyle(fgFill = color), rows = actual_ghi_2b_row + 1, cols = col)
        }
      }
      
      # 3-Bedroom (GHI1, Minimum GHI1)
      actual_ghi_3b_row <- grep("^Actual GHI1", rownames(merged_df))
      minimum_ghi_3b_row <- grep("^Minimum GHI1", rownames(merged_df))
      if (length(actual_ghi_3b_row) > 0 && length(minimum_ghi_3b_row) > 0) {
        actual_ghi_values_3b <- as.numeric(gsub("[$,]", "", merged_df[actual_ghi_3b_row, -1]))  # Exclude RowNames column
        minimum_ghi_values_3b <- as.numeric(gsub("[$,]", "", merged_df[minimum_ghi_3b_row, -1]))  # Exclude RowNames column
        actual_ghi_values_3b[is.na(actual_ghi_values_3b)] <- 0
        minimum_ghi_values_3b[is.na(minimum_ghi_values_3b)] <- 0
        
        for (col in 2:ncol(merged_df)) {  # Start from column 2 to skip RowNames
          color <- ifelse(actual_ghi_values_3b[col-1] >= minimum_ghi_values_3b[col-1], "#00FF00", "#FF0000")  # Green if >= Minimum GHI, Red otherwise
          addStyle(wb, sheet = 1, style = createStyle(fgFill = color), rows = actual_ghi_3b_row + 1, cols = col)
        }
      }
      
      # 4-Bedroom (GHI2, Minimum GHI2)
      actual_ghi_4b_row <- grep("^Actual GHI2", rownames(merged_df))
      minimum_ghi_4b_row <- grep("^Minimum GHI2", rownames(merged_df))
      if (length(actual_ghi_4b_row) > 0 && length(minimum_ghi_4b_row) > 0) {
        actual_ghi_values_4b <- as.numeric(gsub("[$,]", "", merged_df[actual_ghi_4b_row, -1]))  # Exclude RowNames column
        minimum_ghi_values_4b <- as.numeric(gsub("[$,]", "", merged_df[minimum_ghi_4b_row, -1]))  # Exclude RowNames column
        actual_ghi_values_4b[is.na(actual_ghi_values_4b)] <- 0
        minimum_ghi_values_4b[is.na(minimum_ghi_values_4b)] <- 0
        
        for (col in 2:ncol(merged_df)) {  # Start from column 2 to skip RowNames
          color <- ifelse(actual_ghi_values_4b[col-1] >= minimum_ghi_values_4b[col-1], "#00FF00", "#FF0000")  # Green if >= Minimum GHI, Red otherwise
          addStyle(wb, sheet = 1, style = createStyle(fgFill = color), rows = actual_ghi_4b_row + 1, cols = col)
        }
      }
      
      # 5-Bedroom (GHI3, Minimum GHI3)
      actual_ghi_5b_row <- grep("^Actual GHI3", rownames(merged_df))
      minimum_ghi_5b_row <- grep("^Minimum GHI3", rownames(merged_df))
      if (length(actual_ghi_5b_row) > 0 && length(minimum_ghi_5b_row) > 0) {
        actual_ghi_values_5b <- as.numeric(gsub("[$,]", "", merged_df[actual_ghi_5b_row, -1]))  # Exclude RowNames column
        minimum_ghi_values_5b <- as.numeric(gsub("[$,]", "", merged_df[minimum_ghi_5b_row, -1]))  # Exclude RowNames column
        actual_ghi_values_5b[is.na(actual_ghi_values_5b)] <- 0
        minimum_ghi_values_5b[is.na(minimum_ghi_values_5b)] <- 0
        
        for (col in 2:ncol(merged_df)) {  # Start from column 2 to skip RowNames
          color <- ifelse(actual_ghi_values_5b[col-1] >= minimum_ghi_values_5b[col-1], "#00FF00", "#FF0000")  # Green if >= Minimum GHI, Red otherwise
          addStyle(wb, sheet = 1, style = createStyle(fgFill = color), rows = actual_ghi_5b_row + 1, cols = col)
        }
      }
      
      # Apply conditional formatting for "Deposit Calculation" and compare with "Expected deposit"
      
      # 2-Bedroom (Deposit calculation, Expected deposit)
      deposit_2b_row <- grep("^Deposit calculation", rownames(merged_df))
      expected_deposit_2b_row <- grep("^Expected deposit", rownames(merged_df))
      if (length(deposit_2b_row) > 0 && length(expected_deposit_2b_row) > 0) {
        deposit_values_2b <- as.numeric(gsub("[$,]", "", merged_df[deposit_2b_row, -1]))  # Exclude RowNames column
        expected_deposit_values_2b <- as.numeric(gsub("[$,]", "", merged_df[expected_deposit_2b_row, -1]))  # Exclude RowNames column
        deposit_values_2b[is.na(deposit_values_2b)] <- 0
        expected_deposit_values_2b[is.na(expected_deposit_values_2b)] <- 0
        
        for (col in 2:ncol(merged_df)) {  # Start from column 2 to skip RowNames
          color <- ifelse(deposit_values_2b[col-1] >= expected_deposit_values_2b[col-1], "#00FF00", "#FF0000")  # Green if >= Expected Deposit, Red otherwise
          addStyle(wb, sheet = 1, style = createStyle(fgFill = color), rows = deposit_2b_row + 1, cols = col)
        }
      }
      
      # 3-Bedroom (Deposit calculation1, Expected deposit1)
      deposit_3b_row <- grep("^Deposit calculation1", rownames(merged_df))
      expected_deposit_3b_row <- grep("^Expected deposit1", rownames(merged_df))
      if (length(deposit_3b_row) > 0 && length(expected_deposit_3b_row) > 0) {
        deposit_values_3b <- as.numeric(gsub("[$,]", "", merged_df[deposit_3b_row, -1]))  # Exclude RowNames column
        expected_deposit_values_3b <- as.numeric(gsub("[$,]", "", merged_df[expected_deposit_3b_row, -1]))  # Exclude RowNames column
        deposit_values_3b[is.na(deposit_values_3b)] <- 0
        expected_deposit_values_3b[is.na(expected_deposit_values_3b)] <- 0
        
        for (col in 2:ncol(merged_df)) {  # Start from column 2 to skip RowNames
          color <- ifelse(deposit_values_3b[col-1] >= expected_deposit_values_3b[col-1], "#00FF00", "#FF0000")  # Green if >= Expected Deposit, Red otherwise
          addStyle(wb, sheet = 1, style = createStyle(fgFill = color), rows = deposit_3b_row + 1, cols = col)
        }
      }
      
      # 4-Bedroom (Deposit calculation2, Expected deposit2)
      deposit_4b_row <- grep("^Deposit calculation2", rownames(merged_df))
      expected_deposit_4b_row <- grep("^Expected deposit2", rownames(merged_df))
      if (length(deposit_4b_row) > 0 && length(expected_deposit_4b_row) > 0) {
        deposit_values_4b <- as.numeric(gsub("[$,]", "", merged_df[deposit_4b_row, -1]))  # Exclude RowNames column
        expected_deposit_values_4b <- as.numeric(gsub("[$,]", "", merged_df[expected_deposit_4b_row, -1]))  # Exclude RowNames column
        deposit_values_4b[is.na(deposit_values_4b)] <- 0
        expected_deposit_values_4b[is.na(expected_deposit_values_4b)] <- 0
        
        for (col in 2:ncol(merged_df)) {  # Start from column 2 to skip RowNames
          color <- ifelse(deposit_values_4b[col-1] >= expected_deposit_values_4b[col-1], "#00FF00", "#FF0000")  # Green if >= Expected Deposit, Red otherwise
          addStyle(wb, sheet = 1, style = createStyle(fgFill = color), rows = deposit_4b_row + 1, cols = col)
        }
      }
      
      # 5-Bedroom (Deposit calculation3, Expected deposit3)
      deposit_5b_row <- grep("^Deposit calculation3", rownames(merged_df))
      expected_deposit_5b_row <- grep("^Expected deposit3", rownames(merged_df))
      if (length(deposit_5b_row) > 0 && length(expected_deposit_5b_row) > 0) {
        deposit_values_5b <- as.numeric(gsub("[$,]", "", merged_df[deposit_5b_row, -1]))  # Exclude RowNames column
        expected_deposit_values_5b <- as.numeric(gsub("[$,]", "", merged_df[expected_deposit_5b_row, -1]))  # Exclude RowNames column
        deposit_values_5b[is.na(deposit_values_5b)] <- 0
        expected_deposit_values_5b[is.na(expected_deposit_values_5b)] <- 0
        
        for (col in 2:ncol(merged_df)) {  # Start from column 2 to skip RowNames
          color <- ifelse(deposit_values_5b[col-1] >= expected_deposit_values_5b[col-1], "#00FF00", "#FF0000")  # Green if >= Expected Deposit, Red otherwise
          addStyle(wb, sheet = 1, style = createStyle(fgFill = color), rows = deposit_5b_row + 1, cols = col)
        }
      }
      
      # Save the workbook
      saveWorkbook(wb, file_name, overwrite = TRUE)
    }
    
    # Download handler for the combined Excel file with merged sheet
    output$download_xlsx <- downloadHandler(
      current_date <- format(Sys.Date(), "%d_%m_%Y"),
      filename = function() {
        paste0("MRT_results_", current_date, ".xlsx")
      },
      content = function(file) {
        # Create a list of dataframes with corresponding sheet names
        df_list <- list(
          values$results_df_2b,
          values$results_df_3b,
          values$results_df_4b,
          values$results_df_5b
        )
        
        # Call the function to create and save the Excel file
        createExcelFile(df_list, file)
      }
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
