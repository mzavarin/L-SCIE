#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Contact Nicholas Parham, Elliot Chang, or Mavrik Zavarin at chang58@llnl.gov or zavarin1@llnl.gov for comments or corrections.

library(shiny)
library(DT)
library(dplyr)
library(PeriodicTable)
library(shinycssloaders)
library(shinythemes)
library(sjmisc)

options(shiny.maxRequestSize = 60*1024^2) # max file size set at 30 MB

# Define UI for application that draws a histogram
navbarPage(title = tags$div(img(src='llnl-logo.png', height = 25, width = 150), 'SCDC'), theme = shinytheme('readable'),

  tabPanel(title = 'Home',
           
    # Page title
    titlePanel('LLNL Surface Complexation Database Converter'),
    br(),
    h4('Welcome'),
    p('The SCDC (Surface Complexation Database Converter) is a tool 
      for creating a uniform version of the LLNL surface complexation 
      database.  This allows ease-of-use when formatting the data for 
      programs such as PHREEQC and PEST. This is v2.0'),
    br(),
    h4('User Guide'),
    em('The Unifier'),
    p('This tab serves to create uniformity in units and overall 
      organization of the database.  It requires twenty-one user inputs:'),
    tags$ol(tags$li(strong('Dataset.csv'), '- the Dataset tab exported as a .csv from Access'), 
            tags$li(strong('Data.csv'), '- the Data tab exported as a .csv from Access'),
            tags$li(strong('mineral-ref.xlsx'), '- the mineral-ref tab/file that contains mineral names, 
                    molar masses, and densities'),
            tags$li(strong('SD1'), '- the estimated standard deviation for', em('temperature'), 'values'),
            tags$li(strong('SD2'), '- the estimated standard deviation for', em('electrolyte linear'), 'values'),
            tags$li(strong('SD3'), '- the estimated standard deviation for', em('electrolyte log'), 'values'),
            tags$li(strong('SD4'), '- the estimated standard deviation for', em('pH'), 'values'),
            tags$li(strong('SD5'), '- the estimated standard deviation for', em('mineral'), 'values'),
            tags$li(strong('SD6'), '- the estimated standard deviation for', em('mineral surface area'), 'values'),
            tags$li(strong('SD7'), '- the estimated standard deviation for', em('mineral site'), 'values'),
            tags$li(strong('SD8'), '- the estimated standard deviation for', em('CEC'), 'values'),
            tags$li(strong('SD9'), '- the estimated standard deviation for', em('gas'), 'values'),
            tags$li(strong('SD10'), '- the estimated standard deviation for', em('sorbate'), 'values'),
            tags$li(strong('SD11'), '- the estimated standard deviation for', em('surface charge linear'), 'values'),
            tags$li(strong('SD12'), '- the estimated standard deviation for', em('surface charge log'), 'values'),
            tags$li(strong('SD13'), '- the estimated standard deviation for', em('sorbed % or fraction'), 'values'),
            tags$li(strong('SD14'), '- the estimated standard deviation for', em('sorbed Kd/Rd linear'), 'values'),
            tags$li(strong('SD15'), '- the estimated standard deviation for', em('sorbed Kd/Rd log'), 'values'),
            tags$li(strong('SD16'), '- the estimated standard deviation for', em('sorbed linear'), 'values'),
            tags$li(strong('SD17'), '- the estimated standard deviation for', em('sorbed log'), 'values'),
            tags$li(strong('Sites'), '- choice between only filling in missing values or replacing all with
                    the mineral-ref.xlsx surface sites info')),
    
    br(),
    em('The Filterer'),
    p('This tab serves to subset the output from the Unifier.  It requires three user inputs:'),
    tags$ol(tags$li(strong('sc.dataset.csv'), '- output from', em('the Unifier')),
            tags$li(strong('Minerals'), '- minerals desired in the subset'),
            tags$li(strong('Sorbates'), '- sorbates desired in the subset')),
    
    br(),
    em('The Formatter'),
    p('This tab serves to subset and format the output from the Unifier so that it is ready-to-use in PHREEQC 
      or PEST.  It requires four user inputs:'),
    tags$ol(tags$li(strong('sc.subset.csv'), '- output from', em('the Filterer')),
            tags$li(strong('sample.txt'), '- user specified output format, denoting information placement
                    with !column_name!'),
            tags$li(strong('Program'), '- choice between PHREEQC or PEST (control/instruction) 
                    formats based on the sample.txt used'),
            tags$li(strong('Weighting'), '- user specified weighting formula in R syntax, using the letter ', 
                    em('s'), ' as the placeholder for SD')),
    
    br(),
    h4('Scan Tool'),
    p('Scan your Dataset.csv file for missing minerals from mineral-ref.xlsx below:'),
    fileInput('dataset.test', label = 'Select Dataset.csv file'),
    fileInput('mineral.test', label = 'Select mineral-ref.xlsx file'),
    actionButton('scan', 'Scan'),
    br(),
    tableOutput('missing'),
    br(),
    p('Note: Densities are g/cm3, molar masses are g/mol, sites are sites/nm2, and names 
    are case sensitive.  If nothing shows, there are no minerals or compounds missing.')
  ),    
           
  tabPanel(title = 'Unifier',
  
    # Page title
    titlePanel('LLNL Surface Complexation Database Converter'),
  
    # Sidebar area with user inputs
    sidebarPanel(
      
      # User inputs here
      fileInput('sc.dataset', label = h4('Select Dataset.csv file')),
      hr(),
      fileInput('sc.data', label = h4('Select Data.csv file')),
      hr(),
      fileInput('sc.minerals', label = h4('Select mineral-ref.xlsx file')),
      hr(),
      numericInput('sd1', label = h4('SD1 - Temperature - Celsius'), 5, min = 0, max = 50, step = 1),
      hr(),
      numericInput('sd2', label = h4('SD2 - [Electrolyte] (linear scale) - % mol/L'), 5, min = 0, max = 100, step = 1),
      hr(),
      numericInput('sd3', label = h4('SD3 - [Electrolyte] (log scale)'), 0.2, min = 0, max = 1, step = 1),
      hr(),
      numericInput('sd4', label = h4('SD4 - pH'), 0.1, min = 0, max = 7, step = 0.1),
      hr(),
      numericInput('sd5', label = h4('SD5 - Mineral Solid:Solution Ratio - % g/L'), 10, min = 0, max = 100, step = 1),
      hr(),
      numericInput('sd6', label = h4('SD6 - Mineral Surface Area - % m2/g'), 10, min = 0, max = 100, step = 1),
      hr(),
      numericInput('sd7', label = h4('SD7 - Mineral Site Density - % sites/m2'), 10, min = 0, max = 100, step = 1),
      hr(),
      numericInput('sd8', label = h4('SD8 - Cation Exchange Capacity - % meq/100g'), 10, min = 0, max = 100, step = 1),
      hr(),
      numericInput('sd9', label = h4('SD9 - [Gas] - % bar'), 10, min = 0, max = 100, step = 1),
      hr(),
      numericInput('sd10', label = h4('SD10 - [Sorbate] - % mol/L'), 5, min = 0, max = 100, step = 1),
      hr(),
      numericInput('sd11', label = h4('SD11 - [Surface Charge] (linear scale) - % microC/m2'), 10, min = 0, max = 100, step = 1),
      hr(),
      numericInput('sd12', label = h4('SD12 - [Surface Charge] (log scale)'), 0.2, min = 0, max = 1, step = 1),
      hr(),
      numericInput('sd13', label = h4('SD13 - % or Fraction Sorbed'), 5, min = 0, max = 100, step = 1),
      hr(),
      numericInput('sd14', label = h4('SD14 - Kd/Rd (linear scale) - % Kd/Rd (error propagated to [Aqueous] and [Sorbed])'), 50, min = 0, max = 100, step = 1),
      hr(),
      numericInput('sd15', label = h4('SD15 - Kd/Rd (log scale) (error propagated to [Aqueous] and [Sorbed])'), 0.2, min = 0, max = 1, step = 1),
      hr(),
      numericInput('sd16', label = h4('SD16 - [Aqueous] & [Sorbed] (linear scale) - % mol/L'), 5, min = 0, max = 100, step = 1),
      hr(),
      numericInput('sd17', label = h4('SD17 - [Aqueous] & [Sorbed] (log scale)'), 0.2, min = 0, max = 2, step = 1),
      hr(),
      selectInput('sites', label = h4('Sites'), c('Fill', 'Replace'), selected = 'Fill'),
      hr(),
      actionButton('unify', 'Unify'),
      hr(),
      downloadButton('downloadData', 'Download')
    ),
  
    # Main display area
    mainPanel(
      withSpinner(DT::dataTableOutput('sc.cleaned'), size = 2, proxy.height = '500px')
    )
  ),
  
  tabPanel(title = 'Filterer',
           
           # Page title
           titlePanel('LLNL Surface Complexation Database Converter'),
           
           # Sidebar area with user inputs
           sidebarPanel(
             
             # User inputs here
             fileInput('clean.dataset', label = h4('Select sc.dataset file')),
             hr(),
             uiOutput('mineral.dropdown'),
             hr(),
             uiOutput('sorbate.dropdown'),
             hr(),
             actionButton('filter', 'Filter'),
             hr(),
             downloadButton('downloadSubset', 'Download')
           ),
           
           # Main display area
           mainPanel(
             withSpinner(DT::dataTableOutput('sc.subset'), size = 2, proxy.height = '500px')
           )
  ),
  
  tabPanel(title = 'Formatter',
           
           # Page title
           titlePanel('LLNL Surface Complexation Database Converter'),
           
           # Sidebar area with user inputs
           sidebarPanel(
             
             # User inputs here
             fileInput('sub.dataset', label = h4('Select sc.subset file')),
             hr(),
             fileInput('sample', label = h4('Select sample.txt file')),
             hr(),
             selectInput('program', label = h4('Select program format'), 
                         choices = c('phreeqc', 'pest-control', 'pest-instruction'), selected = NULL),
             hr(),
             textInput('weighting', label = h4('Write weighting formula'), value = "s ^ -1"),
             hr(),
             actionButton('format', 'Format'),
             hr(),
             downloadButton('downloadOutfile', 'Download')
           ),
           
           # Main display area
           mainPanel(
             withSpinner(verbatimTextOutput('outfile'), size = 2, proxy.height = '500px')
           )
  ),
  
  tabPanel(title = 'Contact',
           
           # Page title
           titlePanel('LLNL Surface Complexation Database Converter v2.0 testing'),
           br(),
           h4('Contact Info'),
           p('Please contact Nicholas Parham at ', strong('(305) 877 8223'), ' or ', strong('nick-99@att.net'), 
           ' with questions, comments, or corrections.  This application was sponsored by Mavrik Zavarin, PhD.')
           )
)  


