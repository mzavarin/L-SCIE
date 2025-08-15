
#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Contact Nicholas Parham (NP) at nick-99@att.net for comments or corrections.

library(shiny)
library(readxl)
library(dplyr)
library(DT)
library(sjmisc)
library(PeriodicTable)
library(data.table)

#########################
###### FILE DEPENDENCIES ######
#########################

#####
#####  This makes a connection between the name of an element and its symbol
#####

elements.list = read.csv('elementlist.csv') # should be a dependency
elements = elements.list$symbols

#################
###### FUNCTIONS ######
#################

#####
#####  This section converts something like Na(+) to just Na for downstream calculations...
#####
s_element = function(element){ # isolate element name for use in mass() function
    if (unlist(stringr::str_split(element, '[(]'), use.names = F)[1] %in% elements){
        if (length(unlist(stringr::str_split(element, '[+]'), use.names = F)) > 1){
            symbol = unlist(stringr::str_split(element, '[(]'), use.names = F)[1]
        }else if (length(unlist(stringr::str_split(element, '[-]'), use.names = F)) > 1){
            symbol = unlist(stringr::str_split(element, '[(]'), use.names = F)[1]
        }else{
            symbol = 'H' # default if element not found
        }
        if (is.null(symbol) | is.na(symbol)){
            symbol = 'H'
        }
    }else{
        symbol = 'H'
    }
    return(symbol)
}
#####
##### This section attempts to define the conversion process...blank cells are by default "NA". this convert function is used in most unit conversions downstream
##### separate function made for log scale
convert = function(val = NA, val.sd = NA, new.units = NA, user.sd = NA, new.axis = NA, conversion = 'z',
                   molar.mass = NA, d = NA, area = NA, pH = NA, min.val = NA){
# this links names used in unit conversion to values in the function
    if (!is.na(molar.mass)){
        conversion = stringr::str_replace_all(conversion, 'molar.mass', as.character(molar.mass))
    }
    if (!is.na(d)){
        conversion = stringr::str_replace_all(conversion, 'd', as.character(d))
    }
    if (!is.na(area)){
        conversion = stringr::str_replace_all(conversion, 'area', as.character(area))
    }
    if (!is.na(pH)){
        conversion = stringr::str_replace_all(conversion, 'pH', as.character(pH))
    }
    if (!is.na(min.val)){
        conversion = stringr::str_replace_all(conversion, 'min.val', as.character(min.val))
    }
# this defines how the conversion is done...during val.conversion, Z is replaced with val
    val.conversion = stringr::str_replace_all(conversion, 'z', as.character(val))
    new.val = as.numeric(eval(parse(text = val.conversion)))
# this defines how the conversion of standard deviations is performed. if SD is reported, then calculation is performed in the same way as value
    if (!is.na(val.sd) & val.sd != ''){   
        sd.conversion = stringr::str_replace_all(conversion, 'z', as.character(val.sd))
        new.sd = as.numeric(eval(parse(text = sd.conversion)))
# if standard deviation is not reported, then you estimate it by multiplying user.sd...typically the estimated SD from this code...by the new value.  so if SD is 5% then SD is 0.05 * value
    }else{
        new.sd = new.val * user.sd
    }
#####  This little piece of code simple states that if new.units is not an empty cell and if it's pH, %, percent, fraction, or C, then new.sd is simply the estimated SD (not multiplied by value!) I AM NOT SURE IF THIS IS ALWAYS CORRECT!   
    if (!is.na(new.units)){
        if (new.units %in% c('pH', '%', 'percent', 'fraction', 'C')){
            new.sd = user.sd
        }
    }
##### this defines the new.vals string which is critical to moving data from one column to another!  note that new.vals and new.val are two different things!   
    new.vals = c(new.val, new.sd, new.units, new.axis)
    return(new.vals)
}


convertlog = function(val = NA, val.sd = NA, new.units = NA, user.sd = NA, new.axis = NA, conversion = 'z',
                   molar.mass = NA, d = NA, area = NA, pH = NA, min.val = NA){
  # this links names used in unit conversion to values in the function
  if (!is.na(molar.mass)){
    conversion = stringr::str_replace_all(conversion, 'molar.mass', as.character(molar.mass))
  }
  if (!is.na(d)){
    conversion = stringr::str_replace_all(conversion, 'd', as.character(d))
  }
  if (!is.na(area)){
    conversion = stringr::str_replace_all(conversion, 'area', as.character(area))
  }
  if (!is.na(pH)){
    conversion = stringr::str_replace_all(conversion, 'pH', as.character(pH))
  }
  if (!is.na(min.val)){
    conversion = stringr::str_replace_all(conversion, 'min.val', as.character(min.val))
  }
  # this defines how the conversion is done...during val.conversion, Z is replaced with val
  val.conversion = stringr::str_replace_all(conversion, 'z', as.character(val))
  new.val = as.numeric(eval(parse(text = val.conversion)))
  # this defines how the conversion of standard deviations is performed. if SD is reported, then calculation is performed in the same way as value
  if (!is.na(val.sd) & val.sd != ''){   
    sd.conversion = stringr::str_replace_all(conversion, 'z', as.character(val.sd))
    new.sd = as.numeric(eval(parse(text = sd.conversion)))
    # if standard deviation is not reported, then you estimate it by multiplying user.sd...typically the estimated SD from this code...by the new value.  so if SD is 5% then SD is 0.05 * value
  }else{
    new.sd = user.sd
  }

  ##### this defines the new.vals string which is critical to moving data from one column to another!  note that new.vals and new.val are two different things!   
  new.vals = c(new.val, new.sd, new.units, new.axis)
  return(new.vals)
}


####################
###### SERVER LOGIC ######
####################

shinyServer(function(input, output) {
    
################
###### HOME TAB ######
################
  
#####
#####  mz - this section simply checks to make sure there are no elements or minerals in the database that can't be found in the elements file or mineral-ref file
#####
    missingInput = eventReactive(input$scan, {
        filepath = input$dataset.test
        dataset = read.csv(filepath$datapath)
        filepath2 = input$mineral.test
        mineral.ref = read_excel(filepath2$datapath)
        
        minerals = unique(c(dataset$Mineral, 
                            dataset$Electrolyte1,
                            dataset$Electrolyte2,
                            dataset$Electrolyte3,
                            dataset$Electrolyte4,
                            dataset$Electrolyte5,
                            dataset$Electrolyte6,
                            dataset$Electrolyte7,
                            dataset$Sorbate))
        ref.minerals = unique(mineral.ref$minerals)
        elements.list = read.csv('elementlist.csv') # should be a dependency
        elements = elements.list$symbols
        
        # filter out elements
        for (i in c(1:length(minerals))){
            if (unlist(stringr::str_split(minerals[i], '[(]'), use.names = F)[1] %in% elements){
                if (length(unlist(stringr::str_split(minerals[i], '[+]'), use.names = F)) > 1){
                    minerals[i] = 'Element'
                }else if (length(unlist(stringr::str_split(minerals[i], '[-]'), use.names = F)) > 1){
                    minerals[i] = 'Element'
                }else{
                    # not element
                }
            }
        }

        missing = minerals[!(minerals %in% ref.minerals) & !(minerals == '') & !(is.na(minerals)) 
                           & !(minerals == 'Element') & !(minerals == 'pH')]
        missing = data.frame(missing = missing) # output missing from mineral-ref
        missing
    })
    
    output$missing = renderTable({
        missing = missingInput()
        missing
    })

###################
###### UNIFIER TAB ######
###################
    
    sc.datasetInput = reactive({ # read in Dataset.xlsx as dataframe
        filepath = input$sc.dataset
        sc.dataset = read.csv(filepath$datapath)
        sc.dataset
    })
    
    sc.dataInput = reactive({ # read in Data.xlsx as dataframe
        filepath = input$sc.data
        sc.data = read.csv(filepath$datapath)
        sc.data
    })
    
    sc.mineralsInput = reactive({ # read in mineral-ref.xlsx as dataframe
        filepath = input$sc.minerals
        sc.minerals = read_excel(filepath$datapath, 1)
        sc.minerals
    })
    
    sc.dataset = eventReactive(input$unify, { # create uniform dataset
        
#########################
### READ IN USER SD's ###
#########################
      
        sd1 = input$sd1       # temp SD
        sd2 = input$sd2 / 100 # elyte linear SD
        sd3 = input$sd3 # elyte log SD
        sd4 = input$sd4       # pH SD
        sd5 = input$sd5 / 100 # mineral SD
        sd6 = input$sd6 / 100 # mineralSA SD
        sd7 = input$sd7 / 100# mineral sites SD
        sd8 = input$sd8 / 100 # CEC SD
        sd9 = input$sd9 / 100 # gas SD
        sd10 = input$sd10 / 100 # sorbate SD
        sd11 = input$sd11 / 100 # charge linear SD
        sd12 = input$sd12 # charge log SD
        sd13 = input$sd13 / 100 # sorbed %/frac SD
        sd14 = input$sd14 / 100 # sorbed Kd/Rd linear SD
        sd15 = input$sd15 # sorbed Kd/Rd log SD
        sd16 = input$sd16 / 100 # sorbed linear SD
        sd17 = input$sd17 # sorbed log SD
        
###############################
### READ IN USER DATA FILES ###
###############################
        
        dataset = sc.datasetInput()
        dat = sc.dataInput()
        mineral.ref = sc.mineralsInput()
        dataset = right_join(dataset, dat, by = 'Set') # join Dataset and Data on Set column
        dataset = dataset[dataset$Mineral != '' & !is.na(dataset$Mineral),] # remove incomplete data
# this defines p as the total number of rows
        p = nrow(dataset) # get rows to create empty dataframes and control loops
# I DON'T UNDERSTAND WHAT THIS DOES!!!!
        for (i in c(1:ncol(dataset))){ # fix formatting assumption of logical for numeric columns
            if (is.logical(dataset[,i])){
                dataset[,i] = as.numeric(dataset[,i])
            }
        }
        
########################################
### OUTPUT DATAFRAME STRUCTURE SETUP ###
########################################
        refs = dataset$Reference # transfer without conversion
        sets = dataset$Set # transfer without conversion
        datasource = dataset$Source # source of data without conversion
        setIDs = dataset$number # transfer without conversion
        minerals = dataset$Mineral # transfer without conversion
        formulas = dataset$Mineral_Formula # transfer without conversion
        sources = dataset$Mineral_source # transfer without conversion
        temps = dataset$Temp # transfer without conversion
#####  this makes a new column called sd.temps and fills it with NA        
      sd.temps = c(rep(NA,p))
        
###############################
### HANDLE TEMP ESTIMATIONS ###
###############################
         
# This adds a temperature SD to the dataframe if there is a reported temperature
       for (i in c(1:p)){
           if (temps[i] != '' & !is.na(temps[i])){
               new.vals = convert(val = temps[i], val.sd = sd.temps[i], user.sd = sd1, new.units = 'C')
               sd.temps[i] = as.numeric(new.vals[2])
           }
       }
        
##################################################################
### HANDLE ELECTROLYTE CONVERSIONS, TRANSFERS, AND ESTIMATIONS ###
##################################################################
      
# create an electrolytes dataframe with everything listed as Na
        electrolytes = data.frame(Electrolyte1 = rep(NA,p), Electrolyte1_val = rep(NA,p), 
                                  Electrolyte1_SD = rep(NA,p), Electrolyte1_units = rep(NA,p),
                                  Electrolyte2 = rep(NA,p), Electrolyte2_val = rep(NA,p),
                                  Electrolyte2_SD = rep(NA,p), Electrolyte2_units = rep(NA,p),
                                  Electrolyte3 = rep(NA,p), Electrolyte3_val = rep(NA,p),
                                  Electrolyte3_SD = rep(NA,p), Electrolyte3_units = rep(NA,p),
                                  Electrolyte4 = rep(NA,p), Electrolyte4_val = rep(NA,p),
                                  Electrolyte4_SD = rep(NA,p), Electrolyte4_units = rep(NA,p),
                                  Electrolyte5 = rep(NA,p), Electrolyte5_val = rep(NA,p),
                                  Electrolyte5_SD = rep(NA,p), Electrolyte5_units = rep(NA,p),
                                  Electrolyte6 = rep(NA,p), Electrolyte6_val = rep(NA,p),
                                  Electrolyte6_SD = rep(NA,p), Electrolyte6_units = rep(NA,p),
                                  Electrolyte7 = rep(NA,p), Electrolyte7_val = rep(NA,p),
                                  Electrolyte7_SD = rep(NA,p), Electrolyte7_units = rep(NA,p),
                                  pH = rep(NA,p), 
                                  pH_SD = rep(NA,p))
# makes a column called pHs and a column called sd.pHs...they start off by just being NA        
        pHs = electrolytes$pH # read values
        sd.pHs = electrolytes$pH_SD # read values
        
# this pulls in all the dataset electrolyte data into columns called elytes, val.elytes, sd.elytes, and units.elytes
        for (n in c(1:7)){
            elyte = paste('Electrolyte', n, sep = '')
            elyte.val = paste('Electrolyte', n, '_val', sep = '')
            elyte.sd = paste('Electrolyte', n, '_SD', sep = '')
            elyte.unit = paste('Electrolyte', n, '_units', sep = '')
            elytes = as.vector(dataset[,elyte]) # read values
            val.elytes = as.vector(dataset[,elyte.val]) # read values
            sd.elytes = as.vector(dataset[,elyte.sd]) # read values
            units.elytes = as.vector(dataset[,elyte.unit]) # read values
            
            for (i in c(1:p)){
                if (!is.na(val.elytes[i])){
# pulls pH data out of the electrolytes dataframe, updates the SD based on user input estimates, then puts the info into pHs and sd.pHs files
                    if (units.elytes[i] == 'pH'){
                        new.vals = convert(val = val.elytes[i], val.sd = sd.elytes[i], user.sd = sd4, new.units = 'pH')
                        pHs[i] = as.numeric(new.vals[1])
                        sd.pHs[i] = as.numeric(new.vals[2])
# removes pH from the electrolyte dataframe???
                        elytes[i] = NA
                        val.elytes[i] = NA
                        sd.elytes[i] = NA
                        units.elytes[i] = NA
# converts electrolytes from mg/L to mol/L  
                    }else if (units.elytes[i] == 'mg/L'){
                        if (elytes[i] %in% mineral.ref$minerals){
                            molar.mass = as.numeric(mineral.ref[mineral.ref$minerals == elytes[i], 'masses'])
                        }else{
                            molar.mass = mass(s_element(elytes[i]))
                        }
                        new.vals = convert(val = val.elytes[i], val.sd = sd.elytes[i], user.sd = sd2, new.units = 'mol/L',
                                           conversion = 'z / (1000 * molar.mass)', molar.mass = molar.mass)
                        val.elytes[i] = as.numeric(new.vals[1])
                        sd.elytes[i] = as.numeric(new.vals[2])
                        units.elytes[i] = new.vals[3]
# converts electrolytes from mmol/L to mol/L   
                    }else if (units.elytes[i] == 'mmol/L'){
                        new.vals = convert(val = val.elytes[i], val.sd = sd.elytes[i], user.sd = sd2, new.units = 'mol/L',
                                           conversion = 'z / 1000')
                        val.elytes[i] = as.numeric(new.vals[1])
                        sd.elytes[i] = as.numeric(new.vals[2])
                        units.elytes[i] = new.vals[3]
# converts electrolyes from mol/kg to mol/L (no density correction factors!!!) 
                     }else if (units.elytes[i] == 'mol/kg'){
                        new.vals = convert(val = val.elytes[i], val.sd = sd.elytes[i], user.sd = sd2, new.units = 'mol/L')
                        val.elytes[i] = as.numeric(new.vals[1])
                        sd.elytes[i] = as.numeric(new.vals[2])
                        units.elytes[i] = new.vals[3]
# "converts electrolytes from mol/L to mol/L ...this is just to move the data and SDs correctly into the electrolytes dataframe  
                    }else if (units.elytes[i] == 'mol/L'){
                        new.vals = convert(val = val.elytes[i], val.sd = sd.elytes[i], user.sd = sd2, new.units = 'mol/L')
                        val.elytes[i] = as.numeric(new.vals[1])
                        sd.elytes[i] = as.numeric(new.vals[2])
                        units.elytes[i] = new.vals[3]
                    }
                }
            }
            
# updates all electrolyte data into the electrolytes dataframe...
            electrolytes[,elyte] = elytes # update values
            electrolytes[,elyte.val] = val.elytes # update values
            electrolytes[,elyte.sd] = sd.elytes # update values
            electrolytes[,elyte.unit] = units.elytes # update values
        }
# updates electrolyte dataframe with pH values...not sure if this is needed...       
            electrolytes$pH = pHs # update values
            electrolytes$pH_SD = sd.pHs # update values
        
##############################################################
### HANDLE SORBATE CONVERSIONS, TRANSFERS, AND ESTIMATIONS ###
##############################################################
     
##### pull in the relevant sources from the dataframe for use in this section of the code       
        sorbates = dataset$Sorbate  # read values
        val.sorbate = dataset$Sorbate_val  # read values
        sd.sorbate = dataset$Sorbate_SD  # read values
        units.sorbate = dataset$Sorbate_units  # read values
        axis.x = dataset$X_axis # read values
        val.x = dataset$X_val # read values
        sd.x = dataset$X_SD # read values
        units.x = dataset$X_units # read values
        
        for (i in c(1:p)){
#####  first move any Sorbate data that is in the X axis to the Sorbate column and convert to mol/L
          if (axis.x[i] == 'Sorbate'){
# CONVERT mol/L to mol/L                  
            if (units.x[i] == 'mol/L'){
              new.vals = convert(val = val.x[i], val.sd = sd.x[i], new.units = 'mol/L', new.axis = 'Sorbate',
                                 user.sd = sd10)
              val.sorbate[i] = as.numeric(new.vals[1])
              sd.sorbate[i] = as.numeric(new.vals[2])
              units.sorbate[i] = new.vals[3]
              dataset$Sorbate_val[i] = val.sorbate[i]# write values
              dataset$Sorbate_SD[i] = sd.sorbate[i] # write values
              dataset$Sorbate_units[i] = units.sorbate[i]# write values
              axis.x[i] = NA
              val.x[i] = NA
              sd.x[i] = NA
              units.x[i] = NA
# convert -log(mol/L) to mol/L
            }else if (units.x[i] == '-log(mol/L)'){
              new.vals = convertlog(val = val.x[i], val.sd = sd.x[i], new.units = 'mol/L', new.axis = 'Sorbate',
                                 user.sd = sd17)
              val.sorbate[i] = 10^(-1 * (as.numeric(new.vals[1])))
              sd.sorbate[i] = 10^(-1*(as.numeric(new.vals[1]) - as.numeric(new.vals[2]))) - 10^(-1*(as.numeric(new.vals[1])))
              units.sorbate[i] = new.vals[3]
              dataset$Sorbate_val[i] = val.sorbate[i]# write values
              dataset$Sorbate_SD[i] = sd.sorbate[i] # write values
              dataset$Sorbate_units[i] = units.sorbate[i]# write values
              axis.x[i] = NA
              val.x[i] = NA
              sd.x[i] = NA
              units.x[i] = NA
# convert micromol/L to mol/L
            }else if (units.x[i] == 'micromol/L'){
              new.vals = convert(val = val.x[i], val.sd = sd.x[i], new.units = 'mol/L', new.axis = 'Sorbate',
                                 user.sd = sd16, conversion = 'z / 1000000')
              val.sorbate[i] = as.numeric(new.vals[1])
              sd.sorbate[i] = as.numeric(new.vals[2])
              units.sorbate[i] = new.vals[3]
              dataset$Sorbate_val[i] = val.sorbate[i]# write values
              dataset$Sorbate_SD[i] = sd.sorbate[i] # write values
              dataset$Sorbate_units[i] = units.sorbate[i]# write values
              axis.x[i] = NA
              val.x[i] = NA
              sd.x[i] = NA
              units.x[i] = NA
            }
          }
        }
        
        for (i in c(1:p)){
#####  now convert what is in the sorbate column to the correct units
            if (!is.na(val.sorbate[i])){
                if (sorbates[i] %in% mineral.ref$minerals){
                    molar.mass = as.numeric(mineral.ref[mineral.ref$minerals == sorbates[i], 'masses'])
                }else{
                    molar.mass = mass(s_element(sorbates[i]))
                }
# convert ppb and ug/L to mol/L  
              if (units.sorbate[i] == 'ppb' | units.sorbate[i] == 'ug/L'){
                    new.vals = convert(val = val.sorbate[i], val.sd = sd.sorbate[i], user.sd = sd10, new.units = 'mol/L',
                                       conversion = '(z * 0.001) / (1000 * molar.mass)', molar.mass = molar.mass)
                    val.sorbate[i] = as.numeric(new.vals[1])
                    sd.sorbate[i] = as.numeric(new.vals[2])
                    units.sorbate[i] = new.vals[3]
# convert ppm and mg/L to mol/L                     
                }else if (units.sorbate[i] == 'ppm' | units.sorbate[i] == 'mg/L'){
                    new.vals = convert(val = val.sorbate[i], val.sd = sd.sorbate[i], user.sd = sd10, new.units = 'mol/L',
                                       conversion = 'z / (1000 * molar.mass)', molar.mass = molar.mass)
                    val.sorbate[i] = as.numeric(new.vals[1])
                    sd.sorbate[i] = as.numeric(new.vals[2])
                    units.sorbate[i] = new.vals[3]
# convert mol/kg to mol/L                    
                }else if (units.sorbate[i] == 'mol/kg'){
                    new.vals = convert(val = val.sorbate[i], val.sd = sd.sorbate[i], user.sd = sd10, new.units = 'mol/L')
                    val.sorbate[i] = as.numeric(new.vals[1])
                    sd.sorbate[i] = as.numeric(new.vals[2])
                    units.sorbate[i] = new.vals[3]
# convert mol/L to mol/L  ...this is just to move all the data over                  
                }else if (units.sorbate[i] == 'mol/L'){
                    new.vals = convert(val = val.sorbate[i], val.sd = sd.sorbate[i], user.sd = sd10, new.units = 'mol/L')
                    val.sorbate[i] = as.numeric(new.vals[1])
                    sd.sorbate[i] = as.numeric(new.vals[2])
                    units.sorbate[i] = new.vals[3]
# convert mmol/L to mol/L                     
                }else if (units.sorbate[i] == 'mmol/L'){
                    new.vals = convert(val = val.sorbate[i], val.sd = sd.sorbate[i], user.sd = sd10, new.units = 'mol/L',
                                       conversion = 'z / 1000')
                    val.sorbate[i] = as.numeric(new.vals[1])
                    sd.sorbate[i] = as.numeric(new.vals[2])
                    units.sorbate[i] = new.vals[3]
# convert micromol/L to mol/L                     
                }else if (units.sorbate[i] == 'micromol/L'){
                  new.vals = convert(val = val.sorbate[i], val.sd = sd.sorbate[i], user.sd = sd10, new.units = 'mol/L',
                                     conversion = 'z / 1000000')
                  val.sorbate[i] = as.numeric(new.vals[1])
                  sd.sorbate[i] = as.numeric(new.vals[2])
                  units.sorbate[i] = new.vals[3]
# convert m/L to mol/L                 
                }else {
                  val.sorbate[i] = NA
                  sd.sorbate[i] = NA
                  units.sorbate[i] = NA
              }
              
            }
        }
# create dataframe with all the sorbate info
        sorbate = data.frame(Sorbate = sorbates, Sorbate_val = val.sorbate, Sorbate_SD = sd.sorbate, 
                             Sorbate_units = units.sorbate) # update values and create sub-dataframe for future use
# update all the values in the main dataframe - NOT SURE WHY BUT X_AXIS CAN'T BE UPDATED...GIVES AN ERROR.  THERE IS SOME KIND OF LOGICAL PROBLEM HERE
        # dataset$Sorbate = sorbates  # update values
        # dataset$Sorbate_val = val.sorbate   # update values
        # dataset$Sorbate_SD = sd.sorbate   # update values
        # dataset$Sorbate_units = units.sorbate   # update values
        # dataset$X_axis = axis.x  # update values
        # dataset$X_val = val.x  # update values
        # dataset$X_SD = sd.x  # update values
        # dataset$X_units = units.x # update values
        
# create dataframe with all the aqueous concentration info
        aqueous = data.frame(Aq_val = rep(NA,p), Aq_SD = rep(NA,p), 
                             Aq_units = rep(NA,p)) # create aqueous sub-dataframe for future use
# create dataframe with all the sorbed concentration info        
        sorbed = data.frame(Sorbed_val = rep(NA,p), Sorbed_SD = rep(NA,p), 
                            Sorbed_units = rep(NA,p)) # create sorbed sub-dataframe for future use
        
        ##############################################################
        ### HANDLE MINERAL CONVERSIONS, TRANSFERS, AND ESTIMATIONS ###
        ##############################################################
        val.mineral = dataset$Mineral_val # read values
        sd.mineral = dataset$Mineral_SD # read values
        units.mineral = dataset$Mineral_units # read values

# note here that this converts g/L of mineral to mol/L of mineral....which is quite strange as it is rarely used in this form, but, will keep it for now
        for (i in c(1:p)){
            if (!is.na(units.mineral[i])){
                if (units.mineral[i] == 'g/l' | units.mineral[i] == 'g/L'){
                    if (minerals[i] %in% mineral.ref$minerals){
                        molar.mass = as.numeric(mineral.ref[mineral.ref$minerals == minerals[i], 'masses'])
                        new.vals = convert(val = val.mineral[i], val.sd = sd.mineral[i], user.sd = sd5, new.units = 'mol/L',
                                           conversion = 'z / molar.mass', molar.mass = molar.mass)
                        val.mineral[i] = as.numeric(new.vals[1])
                        sd.mineral[i] = as.numeric(new.vals[2])
                        units.mineral[i] = new.vals[3]
                    }
                }
            }
        }
# create mineral dataframe and update it with conversions        
        mineral = data.frame(Mineral_val = val.mineral, Mineral_SD = sd.mineral, 
                             Mineral_units = units.mineral) # update values and create sub-dataframe for future use
        
        #################################################################
        ### HANDLE MINERAL SA CONVERSIONS, TRANSFERS, AND ESTIMATIONS ###
        #################################################################
        val.mineralSA = mineralSA = dataset$MineralSA # read values
        sd.mineralSA = dataset$MineralSA_SD # read values
        units.mineralSA = dataset$MineralSA_units # read values
        
        for (i in c(1:p)){
            if (!is.na(units.mineralSA[i])){ # perform mineral SA conversions and SD estimations
# convert mm to surface area
                if (units.mineralSA[i] == 'mm'){
                    if(minerals[i] %in% mineral.ref$minerals){
                        d = as.numeric(mineral.ref[mineral.ref$minerals == minerals[i], 'densities'])
                        new.vals = convert(val = val.mineralSA[i], val.sd = sd.mineralSA[i], user.sd = sd6, new.units = 'm2/g',
                                           conversion = '((6 * z^2) / 1000000 ) / ((z^3 * d) / (1000))', d = d)
                        val.mineralSA[i] = as.numeric(new.vals[1])
                        sd.mineralSA[i] = as.numeric(new.vals[2])
                        units.mineralSA[i] = new.vals[3]
# convert nanometers to surface area                        
                    }
                }else if (units.mineralSA[i] == 'nm'){
                  if(minerals[i] %in% mineral.ref$minerals){
                    d = as.numeric(mineral.ref[mineral.ref$minerals == minerals[i], 'densities'])
                    new.vals = convert(val = val.mineralSA[i], val.sd = sd.mineralSA[i], user.sd = sd6, new.units = 'm2/g',
                                       conversion = '((6 * z^2) / 1e18 ) / ((z^3 * d) / 1e21)', d = d)
                    val.mineralSA[i] = as.numeric(new.vals[1])
                    sd.mineralSA[i] = as.numeric(new.vals[2])
                    units.mineralSA[i] = new.vals[3]
                  }
# transfer over the m2/g surface area
                }else if (units.mineralSA[i] == 'm2/g'){
                    new.vals = convert(val = val.mineralSA[i], val.sd = sd.mineralSA[i], user.sd = sd6, new.units = 'm2/g')
                    val.mineralSA[i] = as.numeric(new.vals[1])
                    sd.mineralSA[i] = as.numeric(new.vals[2])
                    units.mineralSA[i] = new.vals[3]
                }
            }
        }
# create mineralSA dataframe and update with unit conversion data
        mineralSA = data.frame(MineralSA = val.mineralSA, MineralSA_SD = sd.mineralSA, 
                               MineralSA_units = units.mineralSA) # update values and create sub-dataframe for future use
# create sites dataframe
        sites = dataset[,c('Mineralsites', 'Mineralsites_SD', 'Mineralsites_units')] # transfer without conversion
# create cec dataframe
        cec = dataset[,c('CEC', 'CEC_SD', 'CEC_units')] # transfer without conversion
# create gases dataframe
        gases = dataset[,c('Gas1', 'Gas1_val', 'Gas1_SD', 'Gas1_units', # transfer without conversion
                           'Gas2', 'Gas2_val', 'Gas2_SD', 'Gas2_units', # transfer without conversion
                           'Gas3', 'Gas3_val', 'Gas3_SD', 'Gas3_units')] # transfer without conversion
# create charges dataframe
        charges = data.frame(SurfCharge_val = rep(NA,p), SurfCharge_SD = rep(NA,p), SurfCharge_units = rep(NA,p))
        
        ###########################
        ### HANDLE SITES VALUES ###
        ###########################
        val.sites = dataset$Mineralsites # read values
        user.input = 'Fill'
        
        # insert code based off of user input
        if (input$sites == 'Replace'){
            for (i in c(1:p)){
                if (minerals[i] %in% mineral.ref$minerals){
                    val.sites[i] = as.numeric(mineral.ref[mineral.ref$minerals == minerals[i], 'sites'])
                }
            }
        }else if (input$sites == 'Fill'){
            for (i in c(1:p)){
                if (!is.na(val.sites[i])){
                    # good
                }else{
                    if (minerals[i] %in% mineral.ref$minerals){
                        val.sites[i] = as.numeric(mineral.ref[mineral.ref$minerals == minerals[i], 'sites'])
                    }
                }
            }
        }
# move updates into main dataset dataframe and sites dataframe        
        dataset$Mineralsites = val.sites # update values
        sites$Mineralsites = val.sites # update values
        
        ##############################
        ### HANDLE CEC ESTIMATIONS ###
        ##############################
        val.cec = dataset$CEC # read values
        sd.cec = dataset$CEC_SD # read values
# this pulls in the CEC SD values from the user input...but no other converstion done bc all values are already in meq/100g
        for (i in c(1:p)){
            if (!is.na(val.cec[i])){
                new.vals = convert(val = val.cec[i], val.sd = sd.cec[i], user.sd = sd8, new.units = 'meq/100g')
                val.cec[i] = as.numeric(new.vals[1])
                sd.cec[i] = as.numeric(new.vals[2])
            }
        }
# move updates into cec dataframe        
        cec$CEC_SD = sd.cec # update values
        
        ########################################
        ### HANDLE MINERAL SITES ESTIMATIONS ###
        ########################################
        val.sites = dataset$Mineralsites # read values
        sd.sites = dataset$Mineralsites_SD # read values
        units.sites = dataset$Mineralsites_units # read values
# this pulls in the site density values and SD from user input but no conversions are done bc all data already in sites/nm2        
        for (i in c(1:p)){
            if (!is.na(val.sites[i])){
                new.vals = convert(val = val.sites[i], val.sd = sd.sites[i], user.sd = sd7, new.units = 'sites/nm2')
                val.sites[i] = as.numeric(new.vals[1])
                sd.sites[i] = as.numeric(new.vals[2])
                units.sites[i] = new.vals[3]
            }
        }
# move updates into sites dataframe        
        sites$Mineralsites_SD = sd.sites # update values
        sites$Mineralsites_units = units.sites # update values
        
        ##############################
        ### HANDLE GAS ESTIMATIONS ###
        ##############################
        for (n in c(1:3)){
            gas.val = paste('Gas', n, '_val', sep = '')
            gas.sd = paste('Gas', n, '_SD', sep = '')
            val.gases = as.vector(gases[,gas.val]) # read values
            sd.gases = as.vector(gases[,gas.sd]) # read values
            
            for (i in c(1:p)){
                new.vals = convert(val = val.gases[i], val.sd = sd.gases[i], user.sd = sd9, new.units = 'bar')
                val.gases[i] = as.numeric(new.vals[1])
                sd.gases[i] = as.numeric(new.vals[2])
            }
            gases[,gas.sd] = sd.gases # update values
        }
        
        ############################################
        ### BIND AND MERGE INTO OUTPUT STRUCTURE ###
        ############################################
        a = data.frame(Reference = refs, Source = datasource, Set = sets, SetID = setIDs, Mineral = minerals, Mineral_formula = formulas, 
                       Mineral_source = sources, Temp = temps)
#####
#####  , Temp_SD = sd.temps  removed this from the above description of a bc temperature SD is not used anywhere
#####        
        b = cbind(a, electrolytes, sorbate, aqueous, sorbed, charges, mineral, mineralSA, sites, cec, gases)
        c = cbind(b, dataset[,c('X_axis', 'X_val', 'X_SD', 'X_units', 'Y_axis', 'Y_val', 'Y_SD', 'Y_units')])
        dataset = c # re-define dataset as output
        
        ##################################################
        ### HANDLE X/Y-AXIS pH CONVERSIONS & TRANSFERS ###
        ##################################################
        axis.y = dataset$Y_axis # read values
        val.y = dataset$Y_val # read values
        sd.y = dataset$Y_SD # read values
        units.y = dataset$Y_units # read values

        axis.x = dataset$X_axis # read values
        val.x = dataset$X_val # read values
        sd.x = dataset$X_SD # read values
        units.x = dataset$X_units # read values

        pHs = dataset$pH # read values
        sd.pHs = dataset$pH_SD # read values

        for (i in c(1:p)){
                if (axis.y[i] == 'pH'){
                    new.vals = convert(val = val.y[i], val.sd = sd.y[i], user.sd = sd4, new.units = 'pH')
                    pHs[i] = as.numeric(new.vals[1])
                    sd.pHs[i] = as.numeric(new.vals[2])
                    dataset$pH[i] = pHs[i]# write values
                    dataset$pH_SD[i] = sd.pHs[i] # write values
             axis.y[i] = NA
             val.y[i] = NA
             sd.y[i] = NA
             units.y[i] = NA
            }
    
        if (axis.x[i] == 'pH'){ # 19JUL20
            new.vals = convert(val = val.x[i], val.sd = sd.x[i], user.sd = sd4, new.units = 'pH')
            pHs[i] = as.numeric(new.vals[1])
            sd.pHs[i] = as.numeric(new.vals[2])
            dataset$pH[i] = pHs[i]# write values
            dataset$pH_SD[i] = sd.pHs[i] # write values
            axis.x[i] = NA
            val.x[i] = NA
            sd.x[i] = NA
            units.x[i] = NA
        }
         else if (axis.x[i] == 'H(+1)' & units.x[i] == '-log(mol/L)'){ # 19JUL20
            new.vals = convert(val = val.x[i], val.sd = sd.x[i], user.sd = sd4, new.units = 'pH')
            pHs[i] = as.numeric(new.vals[1])
            sd.pHs[i] = as.numeric(new.vals[2])
            dataset$pH[i] = pHs[i]# write values
            dataset$pH_SD[i] = sd.pHs[i] # write values
            axis.x[i] = NA
            val.x[i] = NA
            sd.x[i] = NA
            units.x[i] = NA
          }
    
      }

dataset$Y_axis = axis.y # update values
dataset$Y_val = val.y # update values
dataset$Y_SD = sd.y  # update values
dataset$Y_units = units.y # update values

dataset$X_axis = axis.x # update values
dataset$X_val = val.x # update values
dataset$X_SD = sd.x # update values
dataset$X_units = units.x # update values

# dataset$pH = pHs
# dataset$pH_SD = sd.pHs

#############################################################
### HANDLE Y-AXIS CONVERSIONS, TRANSFERS, AND ESTIMATIONS ###
#############################################################
axis.y = dataset$Y_axis # read values
val.y = dataset$Y_val # read values
sd.y = dataset$Y_SD # read values
units.y = dataset$Y_units # read values

areas = dataset$MineralSA # read values
sorbates = dataset$Sorbate # read values
val.mineral = dataset$Mineral_val # read values
val.sorbate = dataset$Sorbate_val # read values
sd.sorbate = dataset$Sorbate_SD # read values
units.sorbate = dataset$Sorbate_units # read values

pHs = dataset$pH # read values
sd.pHs = dataset$pH_SD # read values

val.charges = dataset$SurfCharge_val # read values
sd.charges = dataset$SurfCharge_SD # read values
units.charges = dataset$SurfCharge_units # read values

val.sorbed = dataset$Sorbed_val # read values
sd.sorbed = dataset$Sorbed_SD # read values
units.sorbed = dataset$Sorbed_units # read values

val.aq = dataset$Aq_val # read values
sd.aq = dataset$Aq_SD # read values
units.aq = dataset$Aq_units # read values

for (i in c(1:p)){
    if (units.y[i] != '' & !is.na(units.y[i])){
#####
##### SURFACE CHARGE CONVERSIONS AND TRANSFERS
#####            
# convert C/m2 to microC/cm2
            if (axis.y[i] == 'charge' & units.y[i] == 'C/m2'){
                    new.vals = convert(val = val.y[i], val.sd = sd.y[i], new.units = 'microC/cm2', new.axis = 'charge',
                                       user.sd = sd11, conversion = 'z * 1000000 * (1 / 10000)')
                    val.charges[i] = as.numeric(new.vals[1])
                    sd.charges[i] = as.numeric(new.vals[2])
                    units.charges[i] = new.vals[3]
                    dataset$SurfCharge_val[i] = val.charges[i]# write values
                    dataset$SurfCharge_SD[i] = sd.charges[i] # write values
                    dataset$SurfCharge_units[i] = units.charges[i]# write values
                    axis.y[i] = NA
                    val.y[i] = NA
                    sd.y[i] = NA
                    units.y[i] = NA
# convert C/g to microC/cm2
            }else if (axis.y[i] == 'charge' & units.y[i] == 'C/g'){
                    new.vals = convert(val = val.y[i], val.sd = sd.y[i], new.units = 'microC/cm2', new.axis = 'charge',
                                       user.sd = sd11, conversion = 'z * (1 / area) * 1000000 * (1 / 10000)', area = areas[i])
                    val.charges[i] = as.numeric(new.vals[1])
                    sd.charges[i] = as.numeric(new.vals[2])
                    units.charges[i] = new.vals[3]
                    dataset$SurfCharge_val[i] = val.charges[i]# write values
                    dataset$SurfCharge_SD[i] = sd.charges[i] # write values
                    dataset$SurfCharge_units[i] = units.charges[i]# write values
                    axis.y[i] = NA
                    val.y[i] = NA
                    sd.y[i] = NA
                    units.y[i] = NA
# convert eq/g to microC/cm2
            }else if (axis.y[i] == 'charge' & units.y[i] == 'eq/g'){
                    new.vals = convert(val = val.y[i], val.sd = sd.y[i], new.units = 'microC/cm2', new.axis = 'charge',
                                       user.sd = sd11, conversion = 'z * 96485 * (1 / area) * 1000000 * (1 / 10000)', area = areas[i])
                    val.charges[i] = as.numeric(new.vals[1])
                    sd.charges[i] = as.numeric(new.vals[2])
                    units.charges[i] = new.vals[3]
                    dataset$SurfCharge_val[i] = val.charges[i]# write values
                    dataset$SurfCharge_SD[i] = sd.charges[i] # write values
                    dataset$SurfCharge_units[i] = units.charges[i]# write values
                    axis.y[i] = NA
                    val.y[i] = NA
                    sd.y[i] = NA
                    units.y[i] = NA
# convert meq/kg to microC/cm2
            }else if (axis.y[i] == 'charge' & units.y[i] == 'meq/kg'){
                    new.vals = convert(val = val.y[i], val.sd = sd.y[i], new.units = 'microC/cm2', new.axis = 'charge',
                                       user.sd = sd11, conversion = 'z * 96485 * (1 / area) * (1 / 10000)', area = areas[i])
                    val.charges[i] = as.numeric(new.vals[1])
                    sd.charges[i] = as.numeric(new.vals[2])
                    units.charges[i] = new.vals[3]
                    dataset$SurfCharge_val[i] = val.charges[i]# write values
                    dataset$SurfCharge_SD[i] = sd.charges[i] # write values
                    dataset$SurfCharge_units[i] = units.charges[i]# write values
                    axis.y[i] = NA
                    val.y[i] = NA
                    sd.y[i] = NA
                    units.y[i] = NA
# convert cmol/kg to microC/cm2
            }else if (axis.y[i] == 'charge' & units.y[i] == 'cmol/kg'){
                    new.vals = convert(val = val.y[i], val.sd = sd.y[i], new.units = 'microC/cm2', new.axis = 'charge',
                                       user.sd = sd11, conversion = 'z * (1 / 100) * (1 / 1000) * 96485 *
                                       (1 / area) * (1000000 / 10000)', area = areas[i])
                    val.charges[i] = as.numeric(new.vals[1])
                    sd.charges[i] = as.numeric(new.vals[2])
                    units.charges[i] = new.vals[3]
                    dataset$SurfCharge_val[i] = val.charges[i]# write values
                    dataset$SurfCharge_SD[i] = sd.charges[i] # write values
                    dataset$SurfCharge_units[i] = units.charges[i]# write values
                    axis.y[i] = NA
                    val.y[i] = NA
                    sd.y[i] = NA
                    units.y[i] = NA
                    
# convert atoms/nm2 to microC/cm2
            }else if (axis.y[i] == 'charge' & units.y[i] == 'atoms/nm2'){ # 19JUL20
              new.vals = convert(val = val.y[i], val.sd = sd.y[i], new.units = 'microC/cm2', new.axis = 'charge',
                                 user.sd = sd11, conversion = 'z / 6.02E23 * 1000000 * 1E18 * 96485 * (1 / 10000)')
              val.charges[i] = as.numeric(new.vals[1])
              sd.charges[i] = as.numeric(new.vals[2])
              units.charges[i] = new.vals[3]
              dataset$SurfCharge_val[i] = val.charges[i]# write values
              dataset$SurfCharge_SD[i] = sd.charges[i] # write values
              dataset$SurfCharge_units[i] = units.charges[i]# write values
              axis.y[i] = NA
              val.y[i] = NA
              sd.y[i] = NA
              units.y[i] = NA
                    
# convert micromol/m2 to microC/cm2
            }else if (axis.y[i] == 'charge' & units.y[i] == 'micromol/m2'){ # 19JUL20
                    new.vals = convert(val = val.y[i], val.sd = sd.y[i], new.units = 'microC/cm2', new.axis = 'charge',
                                       user.sd = sd11, conversion = 'z * 96485 * (1 / 10000)')
                    val.charges[i] = as.numeric(new.vals[1])
                    sd.charges[i] = as.numeric(new.vals[2])
                    units.charges[i] = new.vals[3]
                    dataset$SurfCharge_val[i] = val.charges[i]# write values
                    dataset$SurfCharge_SD[i] = sd.charges[i] # write values
                    dataset$SurfCharge_units[i] = units.charges[i]# write values
                    axis.y[i] = NA
                    val.y[i] = NA
                    sd.y[i] = NA
                    units.y[i] = NA
# convert micromC/m2 to microC/cm2
            }else if (axis.y[i] == 'charge' & units.y[i] == 'microC/m2'){
              new.vals = convert(val = val.y[i], val.sd = sd.y[i], new.units = 'microC/cm2', new.axis = 'charge',
                                 user.sd = sd11, conversion = 'z * (1 / 10000)')
              val.charges[i] = as.numeric(new.vals[1])
              sd.charges[i] = as.numeric(new.vals[2])
              units.charges[i] = new.vals[3]
              dataset$SurfCharge_val[i] = val.charges[i]# write values
              dataset$SurfCharge_SD[i] = sd.charges[i] # write values
              dataset$SurfCharge_units[i] = units.charges[i]# write values
              axis.y[i] = NA
              val.y[i] = NA
              sd.y[i] = NA
              units.y[i] = NA
# convert micromC/cm2 to microC/cm2
            }else if (axis.y[i] == 'charge' & units.y[i] == 'microC/cm2'){
              new.vals = convert(val = val.y[i], val.sd = sd.y[i], new.units = 'microC/cm2', new.axis = 'charge',
                                 user.sd = sd11)
              val.charges[i] = as.numeric(new.vals[1])
              sd.charges[i] = as.numeric(new.vals[2])
              units.charges[i] = new.vals[3]
              dataset$SurfCharge_val[i] = val.charges[i]# write values
              dataset$SurfCharge_SD[i] = sd.charges[i] # write values
              dataset$SurfCharge_units[i] = units.charges[i]# write values
              axis.y[i] = NA
              val.y[i] = NA
              sd.y[i] = NA
              units.y[i] = NA
# convert mol/L to microC/cm2
            }else if (axis.y[i] == 'charge' & units.y[i] == 'mol/L'){
                  if (minerals[i] %in% mineral.ref$minerals){
                    molar.mass = as.numeric(mineral.ref[mineral.ref$minerals == minerals[i], 'masses'])
                  }else{
                  molar.mass = mass(s_element(minerals[i]))
                  }
              min.val = val.mineral[i] * molar.mass
              new.vals = convert(val = val.y[i], val.sd = sd.y[i], new.units = 'microC/cm2', new.axis = 'charge',
                                   user.sd = sd11, conversion = 'z * 96485 * (1 / min.val) * (1 / area) *
                                           (1000000 / 10000)', area = areas[i], min.val = min.val)
              val.charges[i] = as.numeric(new.vals[1])
              sd.charges[i] = as.numeric(new.vals[2])
              units.charges[i] = new.vals[3]
              dataset$SurfCharge_val[i] = val.charges[i]# write values
              dataset$SurfCharge_SD[i] = sd.charges[i] # write values
              dataset$SurfCharge_units[i] = units.charges[i]# write values
              axis.y[i] = NA
              val.y[i] = NA
              sd.y[i] = NA
              units.y[i] = NA
# convert mol/kg to microC/cm2
            }else if (axis.y[i] == 'charge' & units.y[i] == 'mol/kg'){
                new.vals = convert(val = val.y[i], val.sd = sd.y[i], new.units = 'microC/cm2', new.axis = 'charge',
                                   user.sd = sd11, conversion = 'z * 96485 * (1 / 1000) * (1 / area) *
                                           (1000000 / 10000)', area = areas[i])
                val.charges[i] = as.numeric(new.vals[1])
                sd.charges[i] = as.numeric(new.vals[2])
                units.charges[i] = new.vals[3]
                dataset$SurfCharge_val[i] = val.charges[i]# write values
                dataset$SurfCharge_SD[i] = sd.charges[i] # write values
                dataset$SurfCharge_units[i] = units.charges[i]# write values
                axis.y[i] = NA
                val.y[i] = NA
                sd.y[i] = NA
                units.y[i] = NA
# convert mmol/kg to microC/cm2
            }else if (axis.y[i] == 'charge' & units.y[i] == 'mmol/kg'){
              new.vals = convert(val = val.y[i], val.sd = sd.y[i], new.units = 'microC/cm2', new.axis = 'charge',
                                 user.sd = sd11, conversion = 'z * 96485 * (1 / 1000000) * (1 / area) *
                                           (1000000 / 10000)', area = areas[i])
              val.charges[i] = as.numeric(new.vals[1])
              sd.charges[i] = as.numeric(new.vals[2])
              units.charges[i] = new.vals[3]
              dataset$SurfCharge_val[i] = val.charges[i]# write values
              dataset$SurfCharge_SD[i] = sd.charges[i] # write values
              dataset$SurfCharge_units[i] = units.charges[i]# write values
              axis.y[i] = NA
              val.y[i] = NA
              sd.y[i] = NA
              units.y[i] = NA
# convert mmol/g to microC/cm2
            }else if (axis.y[i] == 'charge' & units.y[i] == 'mmol/g'){
             new.vals = convert(val = val.y[i], val.sd = sd.y[i], new.units = 'microC/cm2', new.axis = 'charge',
                             user.sd = sd11, conversion = 'z * 96485 * (1 / 1000) * (1 / area) *
                                           (1000000 / 10000)', area = areas[i])
              val.charges[i] = as.numeric(new.vals[1])
              sd.charges[i] = as.numeric(new.vals[2])
              units.charges[i] = new.vals[3]
              dataset$SurfCharge_val[i] = val.charges[i]# write values
              dataset$SurfCharge_SD[i] = sd.charges[i] # write values
              dataset$SurfCharge_units[i] = units.charges[i]# write values
              axis.y[i] = NA
              val.y[i] = NA
              sd.y[i] = NA
              units.y[i] = NA
#####            
##### CALCULATIONS FOR TOTAL H to microC/cm2      
#####      
            }else if (axis.y[i] == 'total H(+)'){
# convert mmol/L to microC/cm2
                    if (units.y[i] == 'mmol/L'){
                      if (minerals[i] %in% mineral.ref$minerals){
                        molar.mass = as.numeric(mineral.ref[mineral.ref$minerals == minerals[i], 'masses'])
                      }else{
                        molar.mass = mass(s_element(minerals[i]))
                      }
                      min.val = val.mineral[i] * molar.mass
                        new.vals = convert(val = val.y[i], val.sd = sd.y[i], new.units = 'microC/cm2', new.axis = 'charge',
                                           user.sd = sd11, conversion = '(z - (10^(-pH)) + (10^(-1*(14-pH)))) *
                                           (1/min.val) * (1/area) * (1000000/10000) * 96485 * (1/1000)', area = areas[i],
                                           pH = pHs[i], min.val = min.val)
                        val.charges[i] = as.numeric(new.vals[1])
                        sd.charges[i] = as.numeric(new.vals[2])
                        units.charges[i] = new.vals[3]
                        dataset$SurfCharge_val[i] = val.charges[i]# write values
                        dataset$SurfCharge_SD[i] = sd.charges[i] # write values
                        dataset$SurfCharge_units[i] = units.charges[i]# write values
                        axis.y[i] = NA
                        val.y[i] = NA
                        sd.y[i] = NA
                        units.y[i] = NA
# convert mol/L to microC/cm2
                    }else if (units.y[i] == 'mol/L'){
                      if (minerals[i] %in% mineral.ref$minerals){
                        molar.mass = as.numeric(mineral.ref[mineral.ref$minerals == minerals[i], 'masses'])
                      }else{
                        molar.mass = mass(s_element(minerals[i]))
                      }
                      min.val = val.mineral[i] * molar.mass
                        new.vals = convert(val = val.y[i], val.sd = sd.y[i], new.units = 'microC/cm2', new.axis = 'charge',
                                           user.sd = sd11, conversion = '(z - (10^(-pH)) + (10^(-1*(14-pH)))) *
                                           (1/min.val) * (1/area) * (1000000/10000) * 96485', area = areas[i],
                                           pH = pHs[i], min.val = min.val)
                        val.charges[i] = as.numeric(new.vals[1])
                        sd.charges[i] = as.numeric(new.vals[2])
                        units.charges[i] = new.vals[3]
                        dataset$SurfCharge_val[i] = val.charges[i]# write values
                        dataset$SurfCharge_SD[i] = sd.charges[i] # write values
                        dataset$SurfCharge_units[i] = units.charges[i]# write values
                        axis.y[i] = NA
                        val.y[i] = NA
                        sd.y[i] = NA
                        units.y[i] = NA
                    }
##### 
##### this section converts all sorbed data to mol/L
##### 
#This converts sorbed percent to mol/L
             }else if (axis.y[i] == 'sorbed'){
                    if (units.y[i] == '%' | units.y[i] == 'percent'){
                        new.vals = convert(val = val.y[i], val.sd = sd.y[i], new.units = '%', new.axis = 'sorbed',
                                           user.sd = (sd13 * 100))
                        val.sorbed[i] = as.numeric(new.vals[1]) * val.sorbate[i] / 100
                        sd.sorbed[i] = as.numeric(new.vals[2]) * val.sorbate[i] / 100
                        units.sorbed[i] = 'mol/L'
                        dataset$Sorbed_val[i] = val.sorbed[i]# write values
                        dataset$Sorbed_SD[i] = sd.sorbed[i] # write values
                        dataset$Sorbed_units[i] = units.sorbed[i]# write values
                        axis.y[i] = NA
                        val.y[i] = NA
                        sd.y[i] = NA
                        units.y[i] = NA
# this converts mol/kg and mmol/g to mol/L
                    }else if (units.y[i] == 'mol/kg' | units.y[i] == 'mmol/g'){
                              if (minerals[i] %in% mineral.ref$minerals){
                                molar.mass = as.numeric(mineral.ref[mineral.ref$minerals == minerals[i], 'masses'])
                              }else{
                                molar.mass = mass(s_element(minerals[i]))
                              }
                      min.val = val.mineral[i] * molar.mass  
                      new.vals = convert(val = val.y[i], val.sd = sd.y[i], new.units = 'mol/L', new.axis = 'sorbed',
                                           user.sd = sd16, conversion = 'z * 0.001 * min.val', min.val = min.val)
                        val.sorbed[i] = as.numeric(new.vals[1])
                        sd.sorbed[i] = as.numeric(new.vals[2])
                        units.sorbed[i] = new.vals[3]
                        dataset$Sorbed_val[i] = val.sorbed[i]# write values
                        dataset$Sorbed_SD[i] = sd.sorbed[i] # write values
                        dataset$Sorbed_units[i] = units.sorbed[i]# write values
                        axis.y[i] = NA
                        val.y[i] = NA
                        sd.y[i] = NA
                        units.y[i] = NA
# this carries mol/L through to the new values...not sure why that is needed
                    }else if (units.y[i] == 'mol/L'){
                        new.vals = convert(val = val.y[i], val.sd = sd.y[i], new.units = 'mol/L', new.axis = 'sorbed',
                                           user.sd = sd16)
                        val.sorbed[i] = as.numeric(new.vals[1])
                        sd.sorbed[i] = as.numeric(new.vals[2])
                        units.sorbed[i] = new.vals[3]
                        dataset$Sorbed_val[i] = val.sorbed[i]# write values
                        dataset$Sorbed_SD[i] = sd.sorbed[i] # write values
                        dataset$Sorbed_units[i] = units.sorbed[i]# write values
                        axis.y[i] = NA
                        val.y[i] = NA
                        sd.y[i] = NA
                        units.y[i] = NA
# this converts nanomol/L to mol/L
                    }else if (units.y[i] == 'nanomol/L'){
                      new.vals = convert(val = val.y[i], val.sd = sd.y[i], new.units = 'mol/L', new.axis = 'sorbed',
                                         user.sd = sd16, conversion = 'z * (1 / 1000000000)')
                      val.sorbed[i] = as.numeric(new.vals[1])
                      sd.sorbed[i] = as.numeric(new.vals[2])
                      units.sorbed[i] = new.vals[3]
                      dataset$Sorbed_val[i] = val.sorbed[i]# write values
                      dataset$Sorbed_SD[i] = sd.sorbed[i] # write values
                      dataset$Sorbed_units[i] = units.sorbed[i]# write values
                      axis.y[i] = NA
                      val.y[i] = NA
                      sd.y[i] = NA
                      units.y[i] = NA
# this converts micromol/L to mol/L
                    }else if (units.y[i] == 'micromol/L'){
                      new.vals = convert(val = val.y[i], val.sd = sd.y[i], new.units = 'mol/L', new.axis = 'sorbed',
                                         user.sd = sd16, conversion = 'z * (1 / 1000000)')
                      val.sorbed[i] = as.numeric(new.vals[1])
                      sd.sorbed[i] = as.numeric(new.vals[2])
                      units.sorbed[i] = new.vals[3]
                      dataset$Sorbed_val[i] = val.sorbed[i]# write values
                      dataset$Sorbed_SD[i] = sd.sorbed[i] # write values
                      dataset$Sorbed_units[i] = units.sorbed[i]# write values
                      axis.y[i] = NA
                      val.y[i] = NA
                      sd.y[i] = NA
                      units.y[i] = NA
# this converts mmol/L to mol/L
                    }else if (units.y[i] == 'mmol/L'){
                      new.vals = convert(val = val.y[i], val.sd = sd.y[i], new.units = 'mol/L', new.axis = 'sorbed',
                                         user.sd = sd16, conversion = 'z * (1 / 1000)')
                      val.sorbed[i] = as.numeric(new.vals[1])
                      sd.sorbed[i] = as.numeric(new.vals[2])
                      units.sorbed[i] = new.vals[3]
                      dataset$Sorbed_val[i] = val.sorbed[i]# write values
                      dataset$Sorbed_SD[i] = sd.sorbed[i] # write values
                      dataset$Sorbed_units[i] = units.sorbed[i]# write values
                      axis.y[i] = NA
                      val.y[i] = NA
                      sd.y[i] = NA
                      units.y[i] = NA
# This converts mmol/kg and micromol/g to mol/L
                    }else if (units.y[i] == 'mmol/kg' | units.y[i] == 'micromol/g'){
                      if (minerals[i] %in% mineral.ref$minerals){
                        molar.mass = as.numeric(mineral.ref[mineral.ref$minerals == minerals[i], 'masses'])
                      }else{
                        molar.mass = mass(s_element(minerals[i]))
                      }
                      min.val = val.mineral[i] * molar.mass 
                      new.vals = convert(val = val.y[i], val.sd = sd.y[i], new.units = 'mol/L', new.axis = 'sorbed',
                                           user.sd = sd16 , conversion = 'z * .000001 * min.val', min.val = min.val)
                        val.sorbed[i] = as.numeric(new.vals[1]) 
                        sd.sorbed[i] = as.numeric(new.vals[2]) 
                        units.sorbed[i] = new.vals[3]
                        dataset$Sorbed_val[i] = val.sorbed[i]# write values
                        dataset$Sorbed_SD[i] = sd.sorbed[i] # write values
                        dataset$Sorbed_units[i] = units.sorbed[i]# write values
                        axis.y[i] = NA
                        val.y[i] = NA
                        sd.y[i] = NA
                        units.y[i] = NA
# This converts mol/m2 to mol/L
                    }else if (units.y[i] == 'mol/m2'){
                      if (minerals[i] %in% mineral.ref$minerals){
                        molar.mass = as.numeric(mineral.ref[mineral.ref$minerals == minerals[i], 'masses'])
                      }else{
                        molar.mass = mass(s_element(minerals[i]))
                      }
                      min.val = val.mineral[i] * molar.mass 
                      new.vals = convert(val = val.y[i], val.sd = sd.y[i], new.units = 'mol/L', new.axis = 'sorbed',
                                           user.sd = sd16, conversion = 'z * area * min.val', area = areas[i], min.val = min.val)
                        val.sorbed[i] = as.numeric(new.vals[1])
                        sd.sorbed[i] = as.numeric(new.vals[2])
                        units.sorbed[i] = new.vals[3]
                        dataset$Sorbed_val[i] = val.sorbed[i]# write values
                        dataset$Sorbed_SD[i] = sd.sorbed[i] # write values
                        dataset$Sorbed_units[i] = units.sorbed[i]# write values
                        axis.y[i] = NA
                        val.y[i] = NA
                        sd.y[i] = NA
                        units.y[i] = NA
# this converts mol/g to mol/L
                    }else if (units.y[i] == 'mol/g'){
                      if (minerals[i] %in% mineral.ref$minerals){
                        molar.mass = as.numeric(mineral.ref[mineral.ref$minerals == minerals[i], 'masses'])
                      }else{
                        molar.mass = mass(s_element(minerals[i]))
                      }
                      min.val = val.mineral[i] * molar.mass 
                      new.vals = convert(val = val.y[i], val.sd = sd.y[i], new.units = 'mol/L', new.axis = 'sorbed',
                                           user.sd = sd16, conversion = 'z * min.val', min.val = min.val)
                        val.sorbed[i] = as.numeric(new.vals[1])
                        sd.sorbed[i] = as.numeric(new.vals[2])
                        units.sorbed[i] = new.vals[3]
                        dataset$Sorbed_val[i] = val.sorbed[i]# write values
                        dataset$Sorbed_SD[i] = sd.sorbed[i] # write values
                        dataset$Sorbed_units[i] = units.sorbed[i]# write values
                        axis.y[i] = NA
                        val.y[i] = NA
                        sd.y[i] = NA
                        units.y[i] = NA
# this converts log(mol/g) to mol/L
                    }else if (units.y[i] == 'log(mol/g)'){
                      if (minerals[i] %in% mineral.ref$minerals){
                        molar.mass = as.numeric(mineral.ref[mineral.ref$minerals == minerals[i], 'masses'])
                      }else{
                        molar.mass = mass(s_element(minerals[i]))
                      }
                      min.val = val.mineral[i] * molar.mass 
                      new.vals = convertlog(val = val.y[i], val.sd = sd.y[i], new.units = 'mol/L', new.axis = 'sorbed',
                                         user.sd = sd17)
                      val.sorbed[i] = 10^(as.numeric(new.vals[1])) * min.val
                      sd.sorbed[i] = (10^(as.numeric(new.vals[1]) + as.numeric(new.vals[2])) - 10^(as.numeric(new.vals[1]))) * min.val
                      units.sorbed[i] = new.vals[3]
                      dataset$Sorbed_val[i] = val.sorbed[i]# write values
                      dataset$Sorbed_SD[i] = sd.sorbed[i] # write values
                      dataset$Sorbed_units[i] = units.sorbed[i]# write values
                      axis.y[i] = NA
                      val.y[i] = NA
                      sd.y[i] = NA
                      units.y[i] = NA
# this converts log(mol/kg) to mol/L
                    }else if (units.y[i] == 'log(mol/kg)'){
                      if (minerals[i] %in% mineral.ref$minerals){
                        molar.mass = as.numeric(mineral.ref[mineral.ref$minerals == minerals[i], 'masses'])
                      }else{
                        molar.mass = mass(s_element(minerals[i]))
                      }
                      min.val = val.mineral[i] * molar.mass 
                      new.vals = convertlog(val = val.y[i], val.sd = sd.y[i], new.units = 'mol/L', new.axis = 'sorbed',
                                         user.sd = sd17)
                      val.sorbed[i] = 10^(as.numeric(new.vals[1])) / 1000 * min.val
                      sd.sorbed[i] = (10^(as.numeric(new.vals[1]) + as.numeric(new.vals[2])) - 10^(as.numeric(new.vals[1]))) / 1000 * min.val
                      units.sorbed[i] = new.vals[3]
                      dataset$Sorbed_val[i] = val.sorbed[i]# write values
                      dataset$Sorbed_SD[i] = sd.sorbed[i] # write values
                      dataset$Sorbed_units[i] = units.sorbed[i]# write values
                      axis.y[i] = NA
                      val.y[i] = NA
                      sd.y[i] = NA
                      units.y[i] = NA
# this converts log(mol/m2) to mol/L
                    }else if (units.y[i] == 'log(mol/m2)'){
                      if (minerals[i] %in% mineral.ref$minerals){
                        molar.mass = as.numeric(mineral.ref[mineral.ref$minerals == minerals[i], 'masses'])
                      }else{
                        molar.mass = mass(s_element(minerals[i]))
                      }
                      min.val = val.mineral[i] * molar.mass 
                      area = areas[i]
                      new.vals = convertlog(val = val.y[i], val.sd = sd.y[i], new.units = 'mol/L', new.axis = 'sorbed',
                                         user.sd = sd17)
                      val.sorbed[i] = 10^(as.numeric(new.vals[1])) * area * min.val
                      sd.sorbed[i] = (10^(as.numeric(new.vals[1]) + as.numeric(new.vals[2])) - 10^(as.numeric(new.vals[1])))  * area * min.val
                      units.sorbed[i] = new.vals[3]
                      dataset$Sorbed_val[i] = val.sorbed[i]# write values
                      dataset$Sorbed_SD[i] = sd.sorbed[i] # write values
                      dataset$Sorbed_units[i] = units.sorbed[i]# write values
                      axis.y[i] = NA
                      val.y[i] = NA
                      sd.y[i] = NA
                      units.y[i] = NA
# this converts fraction to mol/L
                    }else if (units.y[i] == 'fraction'){
                        new.vals = convert(val = val.y[i], val.sd = sd.y[i], new.units = 'fraction', new.axis = 'sorbed',
                                           user.sd = sd13)
                        val.sorbed[i] = as.numeric(new.vals[1]) * val.sorbate[i] 
                        sd.sorbed[i] = as.numeric(new.vals[2]) * val.sorbate[i] 
                        units.sorbed[i] = 'mol/L'
                        dataset$Sorbed_val[i] = val.sorbed[i]# write values
                        dataset$Sorbed_SD[i] = sd.sorbed[i] # write values
                        dataset$Sorbed_units[i] = units.sorbed[i]# write values
                        axis.y[i] = NA
                        val.y[i] = NA
                        sd.y[i] = NA
                        units.y[i] = NA
# this converts millimol/m2 to mol/L
                    }else if (units.y[i] == 'mmol/m2'){ # 19JUL20
                      if (minerals[i] %in% mineral.ref$minerals){
                        molar.mass = as.numeric(mineral.ref[mineral.ref$minerals == minerals[i], 'masses'])
                      }else{
                        molar.mass = mass(s_element(minerals[i]))
                      }
                      min.val = val.mineral[i] * molar.mass
                      new.vals = convert(val = val.y[i], val.sd = sd.y[i], new.units = 'mol/L', new.axis = 'sorbed',
                                         user.sd = sd16, conversion = 'z * (1 / 1000) * area * min.val', area = areas[i],
                                         min.val = min.val)
                      val.sorbed[i] = as.numeric(new.vals[1])
                      sd.sorbed[i] = as.numeric(new.vals[2])
                      units.sorbed[i] = new.vals[3]
                      dataset$Sorbed_val[i] = val.sorbed[i]# write values
                      dataset$Sorbed_SD[i] = sd.sorbed[i] # write values
                      dataset$Sorbed_units[i] = units.sorbed[i]# write values
                      axis.y[i] = NA
                      val.y[i] = NA
                      sd.y[i] = NA
                      units.y[i] = NA
# this converts micromol/m2 to mol/L
                    }else if (units.y[i] == 'micromol/m2'){ # 19JUL20
                        if (minerals[i] %in% mineral.ref$minerals){
                            molar.mass = as.numeric(mineral.ref[mineral.ref$minerals == minerals[i], 'masses'])
                        }else{
                            molar.mass = mass(s_element(minerals[i]))
                        }
                        min.val = val.mineral[i] * molar.mass
                        new.vals = convert(val = val.y[i], val.sd = sd.y[i], new.units = 'mol/L', new.axis = 'sorbed',
                                           user.sd = sd16, conversion = 'z * (1 / 1000000) * area * min.val', area = areas[i],
                                           min.val = min.val)
                        val.sorbed[i] = as.numeric(new.vals[1])
                        sd.sorbed[i] = as.numeric(new.vals[2])
                        units.sorbed[i] = new.vals[3]
                        dataset$Sorbed_val[i] = val.sorbed[i]# write values
                        dataset$Sorbed_SD[i] = sd.sorbed[i] # write values
                        dataset$Sorbed_units[i] = units.sorbed[i]# write values
                        axis.y[i] = NA
                        val.y[i] = NA
                        sd.y[i] = NA
                        units.y[i] = NA
# this converts nanomol/m2 to mol/L
                    }else if (units.y[i] == 'nanomol/m2'){ # 19JUL20
                      if (minerals[i] %in% mineral.ref$minerals){
                        molar.mass = as.numeric(mineral.ref[mineral.ref$minerals == minerals[i], 'masses'])
                      }else{
                        molar.mass = mass(s_element(minerals[i]))
                      }
                      min.val = val.mineral[i] * molar.mass
                      new.vals = convert(val = val.y[i], val.sd = sd.y[i], new.units = 'mol/L', new.axis = 'sorbed',
                                         user.sd = sd16, conversion = 'z * (1 / 1000000000) * area * min.val', area = areas[i],
                                         min.val = min.val)
                      val.sorbed[i] = as.numeric(new.vals[1])
                      sd.sorbed[i] = as.numeric(new.vals[2])
                      units.sorbed[i] = new.vals[3]
                      dataset$Sorbed_val[i] = val.sorbed[i]# write values
                      dataset$Sorbed_SD[i] = sd.sorbed[i] # write values
                      dataset$Sorbed_units[i] = units.sorbed[i]# write values
                      axis.y[i] = NA
                      val.y[i] = NA
                      sd.y[i] = NA
                      units.y[i] = NA
# this converts picomol/m2 to mol/L
                    }else if (units.y[i] == 'picomol/m2'){ # 19JUL20
                      if (minerals[i] %in% mineral.ref$minerals){
                        molar.mass = as.numeric(mineral.ref[mineral.ref$minerals == minerals[i], 'masses'])
                      }else{
                        molar.mass = mass(s_element(minerals[i]))
                      }
                      min.val = val.mineral[i] * molar.mass
                      new.vals = convert(val = val.y[i], val.sd = sd.y[i], new.units = 'mol/L', new.axis = 'sorbed',
                                         user.sd = sd16, conversion = 'z * (1 / 1000000000000) * area * min.val', area = areas[i],
                                         min.val = min.val)
                      val.sorbed[i] = as.numeric(new.vals[1])
                      sd.sorbed[i] = as.numeric(new.vals[2])
                      units.sorbed[i] = new.vals[3]
                      dataset$Sorbed_val[i] = val.sorbed[i]# write values
                      dataset$Sorbed_SD[i] = sd.sorbed[i] # write values
                      dataset$Sorbed_units[i] = units.sorbed[i]# write values
                      axis.y[i] = NA
                      val.y[i] = NA
                      sd.y[i] = NA
                      units.y[i] = NA
# this converts atoms/nm2 to mol/L                    
                    }else if (units.y[i] == 'atoms/nm2'){
                      if (minerals[i] %in% mineral.ref$minerals){
                        molar.mass = as.numeric(mineral.ref[mineral.ref$minerals == minerals[i], 'masses'])
                      }else{
                        molar.mass = mass(s_element(minerals[i]))
                      }
                      min.val = val.mineral[i] * molar.mass
                      new.vals = convert(val = val.y[i], val.sd = sd.y[i], new.units = 'mol/L', new.axis = 'sorbed',
                                         user.sd = sd16, conversion = 'z / 6.02E23 * 1E18 * area * min.val', area = areas[i], min.val = min.val)
                      val.sorbed[i] = as.numeric(new.vals[1])
                      sd.sorbed[i] = as.numeric(new.vals[2])
                      units.sorbed[i] = new.vals[3]
                      dataset$Sorbed_val[i] = val.sorbed[i]# write values
                      dataset$Sorbed_SD[i] = sd.sorbed[i] # write values
                      dataset$Sorbed_units[i] = units.sorbed[i]# write values
                      axis.y[i] = NA
                      val.y[i] = NA
                      sd.y[i] = NA
                      units.y[i] = NA
# this converts mg/g to mol/L NOTE THAT YOU NEED THE MOLAR MASS FOR BOTH MINERAL AND THE SORBATE! it's tricky
                    }else if (units.y[i] == 'mg/g'){
#this part determines min.val which is the g/L of mineral phase
                      if (minerals[i] %in% mineral.ref$minerals){
                        molar.mass = as.numeric(mineral.ref[mineral.ref$minerals == minerals[i], 'masses'])
                      }else{
                        molar.mass = mass(s_element(minerals[i]))
                      }
                      min.val = val.mineral[i] * molar.mass
#                      min.val = 1.0
#this part calculates the molar mass of the sorbate
                      if (!is.na(sorbates[i])){
                        if (sorbates[i] %in% mineral.ref$minerals){
                          molar.mass = as.numeric(mineral.ref[mineral.ref$minerals == sorbates[i], 'masses'])
                        }else{
                          molar.mass = mass(s_element(sorbates[i]))
                        }
                      }
                      new.vals = convert(val = val.y[i], val.sd = sd.y[i], new.units = 'mol/L', new.axis = 'sorbed',
                                         user.sd = sd16, conversion = 'z / (1000 * molar.mass) * min.val', molar.mass = molar.mass, min.val = min.val)
                      val.sorbed[i] = as.numeric(new.vals[1]) 
                      sd.sorbed[i] = as.numeric(new.vals[2])
                      units.sorbed[i] = new.vals[3]
                      dataset$Sorbed_val[i] = val.sorbed[i]# write values
                      dataset$Sorbed_SD[i] = sd.sorbed[i] # write values
                      dataset$Sorbed_units[i] = units.sorbed[i]# write values
                      axis.y[i] = NA
                      val.y[i] = NA
                      sd.y[i] = NA
                      units.y[i] = NA
# this converts microg/m2 to mol/L NOTE THAT YOU NEED THE MOLAR MASS FOR BOTH MINERAL AND THE SORBATE! it's tricky
                    }else if (units.y[i] == 'microg/m2'){
                      #this part calculates the molar mass of the sorbate
                      if (!is.na(sorbates[i])){
                        if (sorbates[i] %in% mineral.ref$minerals){
                          molar.mass = as.numeric(mineral.ref[mineral.ref$minerals == sorbates[i], 'masses'])
                        }else{
                          molar.mass = mass(s_element(sorbates[i]))
                        }
                      }
                      new.vals = convert(val = val.y[i], val.sd = sd.y[i], new.units = 'mol/L', new.axis = 'sorbed',
                                         user.sd = sd16, conversion = 'z / (1000000 * molar.mass) * min.val * area', molar.mass = molar.mass, min.val = min.val, area = areas[i])
                      val.sorbed[i] = as.numeric(new.vals[1]) 
                      sd.sorbed[i] = as.numeric(new.vals[2])
                      units.sorbed[i] = new.vals[3]
                      dataset$Sorbed_val[i] = val.sorbed[i]# write values
                      dataset$Sorbed_SD[i] = sd.sorbed[i] # write values
                      dataset$Sorbed_units[i] = units.sorbed[i]# write values
                      axis.y[i] = NA
                      val.y[i] = NA
                      sd.y[i] = NA
                      units.y[i] = NA

                      
                      
# this converts mg/m2 to mol/L NOTE THAT YOU NEED THE MOLAR MASS FOR BOTH MINERAL AND THE SORBATE! it's tricky
                    }else if (units.y[i] == 'mg/m2'){
                      #this part calculates the molar mass of the sorbate
                      if (!is.na(sorbates[i])){
                        if (sorbates[i] %in% mineral.ref$minerals){
                          molar.mass = as.numeric(mineral.ref[mineral.ref$minerals == sorbates[i], 'masses'])
                        }else{
                          molar.mass = mass(s_element(sorbates[i]))
                        }
                      }
                      new.vals = convert(val = val.y[i], val.sd = sd.y[i], new.units = 'mol/L', new.axis = 'sorbed',
                                         user.sd = sd16, conversion = 'z / (1000 * molar.mass) * min.val * area', molar.mass = molar.mass, min.val = min.val, area = areas[i])
                      val.sorbed[i] = as.numeric(new.vals[1]) 
                      sd.sorbed[i] = as.numeric(new.vals[2])
                      units.sorbed[i] = new.vals[3]
                      dataset$Sorbed_val[i] = val.sorbed[i]# write values
                      dataset$Sorbed_SD[i] = sd.sorbed[i] # write values
                      dataset$Sorbed_units[i] = units.sorbed[i]# write values
                      axis.y[i] = NA
                      val.y[i] = NA
                      sd.y[i] = NA
                      units.y[i] = NA               
                      
                      
                      
                      
# this converts ppm to mol/L NOTE THAT YOU NEED THE MOLAR MASS FOR BOTH MINERAL AND THE SORBATE! it's tricky
                    }else if (units.y[i] == 'ppm' | units.y[i] == 'mg/kg' ){
                      #this part determines min.val which is the g/L of mineral phase
                      if (minerals[i] %in% mineral.ref$minerals){
                        molar.mass = as.numeric(mineral.ref[mineral.ref$minerals == minerals[i], 'masses'])
                      }else{
                        molar.mass = mass(s_element(minerals[i]))
                      }
                      min.val = val.mineral[i] * molar.mass
                      #this part calculates the molar mass of the sorbate
                      if (!is.na(sorbates[i])){
                        if (sorbates[i] %in% mineral.ref$minerals){
                          molar.mass = as.numeric(mineral.ref[mineral.ref$minerals == sorbates[i], 'masses'])
                        }else{
                          molar.mass = mass(s_element(sorbates[i]))
                        }
                      }
                      new.vals = convert(val = val.y[i], val.sd = sd.y[i], new.units = 'mol/L', new.axis = 'sorbed',
                                         user.sd = sd16, conversion = 'z / (1000000 * molar.mass) * min.val', molar.mass = molar.mass, min.val = min.val)
                      val.sorbed[i] = as.numeric(new.vals[1]) 
                      sd.sorbed[i] = as.numeric(new.vals[2])
                      units.sorbed[i] = new.vals[3]
                      dataset$Sorbed_val[i] = val.sorbed[i]# write values
                      dataset$Sorbed_SD[i] = sd.sorbed[i] # write values
                      dataset$Sorbed_units[i] = units.sorbed[i]# write values
                      axis.y[i] = NA
                      val.y[i] = NA
                      sd.y[i] = NA
                      units.y[i] = NA
# this converts mol/molFe to mol/L
                    }else if (units.y[i] == 'mol/molFe'){
                      new.vals = convert(val = val.y[i], val.sd = sd.y[i], new.units = 'mol/L', new.axis = 'sorbed',
                                         user.sd = sd16)
                      val.sorbed[i] = as.numeric(new.vals[1]) * val.mineral[i]
                      sd.sorbed[i] = as.numeric(new.vals[2]) * val.mineral[i]
                      units.sorbed[i] = new.vals[3]
                      dataset$Sorbed_val[i] = val.sorbed[i]# write values
                      dataset$Sorbed_SD[i] = sd.sorbed[i] # write values
                      dataset$Sorbed_units[i] = units.sorbed[i]# write values
                      axis.y[i] = NA
                      val.y[i] = NA
                      sd.y[i] = NA
                      units.y[i] = NA
# this converts nanomol/micromolFe to mol/L
                    }else if (units.y[i] == 'nanomol/micromolFe'){
                      new.vals = convert(val = val.y[i], val.sd = sd.y[i], new.units = 'mol/L', new.axis = 'sorbed',
                                         user.sd = sd16)
                      val.sorbed[i] = as.numeric(new.vals[1]) * val.mineral[i] / 1000
                      sd.sorbed[i] = as.numeric(new.vals[2]) * val.mineral[i] / 1000
                      units.sorbed[i] = new.vals[3]
                      dataset$Sorbed_val[i] = val.sorbed[i]# write values
                      dataset$Sorbed_SD[i] = sd.sorbed[i] # write values
                      dataset$Sorbed_units[i] = units.sorbed[i]# write values
                      axis.y[i] = NA
                      val.y[i] = NA
                      sd.y[i] = NA
                      units.y[i] = NA
# this converts log(mol/molFe) to mol/L
                    }else if (units.y[i] == 'log(mol/molFe)'){
                      new.vals = convertlog(val = val.y[i], val.sd = sd.y[i], new.units = 'mol/L', new.axis = 'sorbed',
                                         user.sd = sd17)
                      val.sorbed[i] = 10^(as.numeric(new.vals[1])) * val.mineral[i]
                      sd.sorbed[i] = (10^(as.numeric(new.vals[1]) + as.numeric(new.vals[2])) - 10^(as.numeric(new.vals[1]))) * val.mineral[i]
                      units.sorbed[i] = new.vals[3]
                      dataset$Sorbed_val[i] = val.sorbed[i]# write values
                      dataset$Sorbed_SD[i] = sd.sorbed[i] # write values
                      dataset$Sorbed_units[i] = units.sorbed[i]# write values
                      axis.y[i] = NA
                      val.y[i] = NA
                      sd.y[i] = NA
                      units.y[i] = NA
# this converts -log(mol/molFe) to mol/L
                    }else if (units.y[i] == '-log(mol/molFe)'){
                      new.vals = convertlog(val = val.y[i], val.sd = sd.y[i], new.units = 'mol/L', new.axis = 'sorbed',
                                         user.sd = sd17)
                      val.sorbed[i] = 10^(-1*(as.numeric(new.vals[1]))) * val.mineral[i]
                      sd.sorbed[i] = (10^(-1*(as.numeric(new.vals[1]) - as.numeric(new.vals[2]))) - 10^(-1*(as.numeric(new.vals[1])))) * val.mineral[i]
                      units.sorbed[i] = new.vals[3]
                      dataset$Sorbed_val[i] = val.sorbed[i]# write values
                      dataset$Sorbed_SD[i] = sd.sorbed[i] # write values
                      dataset$Sorbed_units[i] = units.sorbed[i]# write values
                      axis.y[i] = NA
                      val.y[i] = NA
                      sd.y[i] = NA
                      units.y[i] = NA
                    }
#####                
##### AQUEOUS Y AXIS CALCULATIONS      
#####      
             }else if (axis.y[i] == 'aqueous'){
# convert ppb to mol/L
                    if (units.y[i] == 'ppb'){
# calculate molar mass of sorbate first                    
                      if (!is.na(sorbates[i])){
                        if (sorbates[i] %in% mineral.ref$minerals){
                          molar.mass = as.numeric(mineral.ref[mineral.ref$minerals == sorbates[i], 'masses'])
                        }else{
                          molar.mass = mass(s_element(sorbates[i]))
                        }
                      }
                        new.vals = convert(val = val.y[i], val.sd = sd.y[i], new.units = 'mol/L', new.axis = 'aqueous',
                                           user.sd = sd16, conversion = '(z * 0.001) / (1000 * molar.mass)',
                                           molar.mass = molar.mass)
                        val.aq[i] = as.numeric(new.vals[1])
                        sd.aq[i] = as.numeric(new.vals[2])
                        units.aq[i] = new.vals[3]
                        dataset$Aq_val[i] = val.aq[i]# write values
                        dataset$Aq_SD[i] = sd.aq[i] # write values
                        dataset$Aq_units[i] = units.aq[i]# write values
                        axis.y[i] = NA
                        val.y[i] = NA
                        sd.y[i] = NA
                        units.y[i] = NA
# convert ppm to mol/L
                    }else if (units.y[i] == 'ppm'){
# calculate molar mass of sorbate first                    
                      if (!is.na(sorbates[i])){
                        if (sorbates[i] %in% mineral.ref$minerals){
                          molar.mass = as.numeric(mineral.ref[mineral.ref$minerals == sorbates[i], 'masses'])
                        }else{
                          molar.mass = mass(s_element(sorbates[i]))
                        }
                      }
                        new.vals = convert(val = val.y[i], val.sd = sd.y[i], new.units = 'mol/L', new.axis = 'aqueous',
                                           user.sd = sd16, conversion = 'z / (1000 * molar.mass)', molar.mass = molar.mass)
                        val.aq[i] = as.numeric(new.vals[1])
                        sd.aq[i] = as.numeric(new.vals[2])
                        units.aq[i] = new.vals[3]
                        dataset$Aq_val[i] = val.aq[i]# write values
                        dataset$Aq_SD[i] = sd.aq[i] # write values
                        dataset$Aq_units[i] = units.aq[i]# write values
                        axis.y[i] = NA
                        val.y[i] = NA
                        sd.y[i] = NA
                        units.y[i] = NA
# convert mol/L to mol/L
                    }else if (units.y[i] == 'mol/L'){
                        new.vals = convert(val = val.y[i], val.sd = sd.y[i], new.units = 'mol/L', new.axis = 'aqueous',
                                           user.sd = sd16)
                        val.aq[i] = as.numeric(new.vals[1])
                        sd.aq[i] = as.numeric(new.vals[2])
                        units.aq[i] = new.vals[3]
                        dataset$Aq_val[i] = val.aq[i]# write values
                        dataset$Aq_SD[i] = sd.aq[i] # write values
                        dataset$Aq_units[i] = units.aq[i]# write values
                        axis.y[i] = NA
                        val.y[i] = NA
                        sd.y[i] = NA
                        units.y[i] = NA
# convert log(mol/L) to mol/L
                    }else if (units.y[i] == 'log(mol/L)'){
                      new.vals = convertlog(val = val.y[i], val.sd = sd.y[i], new.units = 'mol/L', new.axis = 'aqueous',
                                         user.sd = sd17)
                      val.aq[i] = 10^(as.numeric(new.vals[1]))
                      sd.aq[i] = 10^(as.numeric(new.vals[1]) + as.numeric(new.vals[2])) - 10^(as.numeric(new.vals[1]))
                      units.aq[i] = new.vals[3]
                      dataset$Aq_val[i] = val.aq[i]# write values
                      dataset$Aq_SD[i] = sd.aq[i] # write values
                      dataset$Aq_units[i] = units.aq[i]# write values
                      axis.y[i] = NA
                      val.y[i] = NA
                      sd.y[i] = NA
                      units.y[i] = NA

#convert aqueous percent to mol/L
                    }else if (units.y[i] == '%'){
                      new.vals = convert(val = val.y[i], val.sd = sd.y[i], new.units = '%', new.axis = 'aqueous',
                                            user.sd = sd13*100)
                      val.aq[i] = as.numeric(new.vals[1]) * val.sorbate[i] / 100
                      sd.aq[i] = as.numeric(new.vals[2]) * val.sorbate[i] / 100
                      units.aq[i] = 'mol/L'
                      dataset$Aq_val[i] = val.aq[i]# write values
                      dataset$Aq_SD[i] = sd.aq[i] # write values
                      dataset$Aq_units[i] = units.aq[i]# write values
                      axis.y[i] = NA
                      val.y[i] = NA
                      sd.y[i] = NA
                      units.y[i] = NA
                    }

               #####                
##### Kd ON THE Y AXIS CALCULATIONS   
##### This is intended to convert everything to mL/g
#####
                  }else if (axis.y[i] == 'Kd' | axis.y[i] == 'Rd'){
# convert mL/g to mL/g
                    if (units.y[i] == 'mL/g' | units.y[i] == 'ml/g' | units.y[i] == 'L/kg'){
                      new.vals = convert(val = val.y[i], val.sd = sd.y[i], new.units = 'mL/g', new.axis = 'Kd',
                                         user.sd = sd14)
                      val.y[i] = as.numeric(new.vals[1])
                      sd.y[i] = as.numeric(new.vals[2])
                      units.y[i] = new.vals[3]
                      axis.y[i] = new.vals[4]
# convert log(mL/g) and log(L/kg) to mL/g
                    }else if (units.y[i] == 'log(mL/g)' | units.y[i] == 'log(L/kg)'  | units.y[i] == 'log(ml/g)'){
                      new.vals = convertlog(val = val.y[i], val.sd = sd.y[i], new.units = 'mL/g', new.axis = 'Kd',
                                         user.sd = sd15)
                      val.y[i] = 10^(as.numeric(new.vals[1]))
                      sd.y[i] = 10^(as.numeric(new.vals[1]) + as.numeric(new.vals[2])) - 10^(as.numeric(new.vals[1]))
                      units.y[i] = new.vals[3]
                      axis.y[i] = new.vals[4]
# convert log(L/g) to mL/g
                    }else if (units.y[i] == 'log(L/g)'){
                      new.vals = convertlog(val = val.y[i], val.sd = sd.y[i], new.units = 'mL/g', new.axis = 'Kd',
                                            user.sd = sd15)
                      val.y[i] = 10^(as.numeric(new.vals[1]))*1000
                      sd.y[i] = (10^(as.numeric(new.vals[1]) + as.numeric(new.vals[2])) - 10^(as.numeric(new.vals[1])))*1000
                      units.y[i] = new.vals[3]
                      axis.y[i] = new.vals[4]                     
# convert m3/kg to mL/g
                    }else if (units.y[i] == 'm3/kg'){
                        new.vals = convert(val = val.y[i], val.sd = sd.y[i], new.units = 'mL/g', new.axis = 'Kd',
                                    user.sd = sd14)
                        val.y[i] = as.numeric(new.vals[1]) * 1000
                        sd.y[i] = as.numeric(new.vals[2]) * 1000
                        units.y[i] = new.vals[3]
                        axis.y[i] = new.vals[4]
                    }
#####
##### convert Ka to Kd mL/g
#####
              }else if (axis.y[i] == 'Ka'){
# convert L/m2 to mL/g
                    if (units.y[i] == 'L/m2'){
                    new.vals = convert(val = val.y[i], val.sd = sd.y[i], new.units = 'mL/g', new.axis = 'Kd',
                             user.sd = sd14, conversion = 'z * 1000 * area', area = areas[i])
                      val.y[i] = as.numeric(new.vals[1])
                      sd.y[i] = as.numeric(new.vals[2])
                      units.y[i] = new.vals[3]
                      axis.y[i] = new.vals[4]
# convert mL/m2 to Kd mL/g
                    }else if (units.y[i] == 'mL/m2'){
                      new.vals = convert(val = val.y[i], val.sd = sd.y[i], new.units = 'mL/g', new.axis = 'Kd',
                             user.sd = sd14, conversion = 'z * area', area = areas[i])
                      val.y[i] = as.numeric(new.vals[1])
                      sd.y[i] = as.numeric(new.vals[2])
                      units.y[i] = new.vals[3]
                      axis.y[i] = new.vals[4]
                    }
              }
          }
}
#####
#####
#####  THIS IS AN ADJUSTMENT TO CONVERT ALL CHARGE INFORMATION TO H+ SURFACE EXCESS
#####
#####
for (i in c(1:p)){
  if (units.charges[i] != '' & !is.na(units.charges[i])){
    #####
    ##### converting surface charge from microC/cm2 to mol/L H+ surface excess
    #####
    if (units.charges[i] == 'microC/cm2'){
      if (minerals[i] %in% mineral.ref$minerals){
              molar.mass = as.numeric(mineral.ref[mineral.ref$minerals == minerals[i], 'masses'])
              }else{
              molar.mass = mass(s_element(minerals[i]))
              }
              min.val = val.mineral[i] * molar.mass
      new.vals = convert(val = val.charges[i], val.sd = sd.charges[i], new.units = 'mol/L', new.axis = 'charge',
                         user.sd = sd11, conversion = 'z / 96485 /1000000 * 10000 * min.val * area', area = areas[i], min.val = min.val)
      val.charges[i] = as.numeric(new.vals[1])
      sd.charges[i] = as.numeric(new.vals[2])
      units.charges[i] = new.vals[3]
      dataset$SurfCharge_val[i] = val.charges[i]# write values
      dataset$SurfCharge_SD[i] = sd.charges[i] # write values
      dataset$SurfCharge_units[i] = units.charges[i]# write values
    }
  }
}
        dataset$Y_axis = axis.y # update values
        dataset$Y_val = val.y # update values
        dataset$Y_SD = sd.y  # update values
        dataset$Y_units = units.y # update values

        #############################################################
        ### HANDLE X-AXIS CONVERSIONS, TRANSFERS, AND ESTIMATIONS ### 
        #############################################################
        axis.x = dataset$X_axis # read values
        val.x = dataset$X_val # read values
        sd.x = dataset$X_SD # read values
        units.x = dataset$X_units # read values
        axis.y = dataset$Y_axis # read values
        val.y = dataset$Y_val # read values
        sd.y = dataset$Y_SD # read values
        units.y = dataset$Y_units # read values
        minerals = dataset$Mineral # read values
        val.mineral = dataset$Mineral_val # read values
        sd.mineral = dataset$Mineral_SD # read values
        units.mineral = dataset$Mineral_units # read values
        val.aq = dataset$Aq_val
        val.sorbate = dataset$Sorbate_val
        val.sorbed = dataset$Sorbed_val
        sd.aq = dataset$Aq_SD
        sd.sorbate = dataset$Sorbate_SD
        sd.sorbed = dataset$Sorbed_SD
        units.aq = dataset$Aq_units
        units.sorbate = dataset$Sorbate_units
        units.sorbed = dataset$Sorbed_units
        
        for (i in c(1:p)){
            if (units.x[i] != '' & !is.na(units.x[i])){
                if (axis.x[i] == 'aqueous'){
#####
#####  converting aqueous units
#####
# log(mol/L) converstion to mol/L
                    if (units.x[i] == 'log(mol/L)'){
                        new.vals = convertlog(val = val.x[i], val.sd = sd.x[i], new.units = 'mol/L', new.axis = 'aqueous',
                                           user.sd = sd17)
                        val.aq[i] = 10^(as.numeric(new.vals[1]))
                        sd.aq[i] = 10^(as.numeric(new.vals[1]) + as.numeric(new.vals[2])) - 10^(as.numeric(new.vals[1]))
                        units.aq[i] = new.vals[3]
                        dataset$Aq_val[i] = val.aq[i]# write values
                        dataset$Aq_SD[i] = sd.aq[i] # write values
                        dataset$Aq_units[i] = units.aq[i]# write values
                        axis.x[i] = NA
                        val.x[i] = NA
                        sd.x[i] = NA
#  NO IDEA WHY BUT i CAN IDENTIFY UNITS.X AT NA...THE CODE CRASHES...
                        units.x[i] = ''
# -log(mol/L) converstion to mol/L                        
                    }else if (units.x[i] == '-log(mol/L)'){
                      new.vals = convertlog(val = val.x[i], val.sd = sd.x[i], new.units = 'mol/L', new.axis = 'aqueous', 
                                         user.sd = sd17)
                      val.aq[i] = 10^(-1 * (as.numeric(new.vals[1])))
                      sd.aq[i] = 10^(-1 * (as.numeric(new.vals[1]) - as.numeric(new.vals[2]))) - 10^(-1 * (as.numeric(new.vals[1])))
                      units.aq[i] = new.vals[3]
                      dataset$Aq_val[i] = val.aq[i]# write values
                      dataset$Aq_SD[i] = sd.aq[i] # write values
                      dataset$Aq_units[i] = units.aq[i]# write values
                      axis.x[i] = NA
                      val.x[i] = NA
                      sd.x[i] = NA
                      units.x[i] = ''
# mol/L converstion to mol/L                        
                    }else if (units.x[i] == 'mol/L'){
                        new.vals = convert(val = val.x[i], val.sd = sd.x[i], new.units = 'mol/L', new.axis = 'aqueous', 
                                           user.sd = sd16)
                        val.aq[i] = as.numeric(new.vals[1])
                        sd.aq[i] = as.numeric(new.vals[2])
                        units.aq[i] = new.vals[3]
                        dataset$Aq_val[i] = val.aq[i]# write values
                        dataset$Aq_SD[i] = sd.aq[i] # write values
                        dataset$Aq_units[i] = units.aq[i]# write values
                        axis.x[i] = NA
                        val.x[i] = NA
                        sd.x[i] = NA
                        units.x[i] = ''
# mmol/L converstion to mol/L   
                    }else if (units.x[i] == 'mmol/L'){
                        new.vals = convert(val = val.x[i], val.sd = sd.x[i], new.units = 'mol/L', new.axis = 'aqueous', 
                                           user.sd = sd16, conversion = 'z / 1000')
                        val.aq[i] = as.numeric(new.vals[1])
                        sd.aq[i] = as.numeric(new.vals[2])
                        units.aq[i] = new.vals[3]
                        dataset$Aq_val[i] = val.aq[i]# write values
                        dataset$Aq_SD[i] = sd.aq[i] # write values
                        dataset$Aq_units[i] = units.aq[i]# write values
                        axis.x[i] = NA
                        val.x[i] = NA
                        sd.x[i] = NA
                        units.x[i] = ''
# micromol/L converstion to mol/L                
                    }else if (units.x[i] == 'micromol/L'){
                      new.vals = convert(val = val.x[i], val.sd = sd.x[i], new.units = 'mol/L', new.axis = 'aqueous', 
                                         user.sd = sd16, conversion = 'z / 1000000')
                      val.aq[i] = as.numeric(new.vals[1])
                      sd.aq[i] = as.numeric(new.vals[2])
                      units.aq[i] = new.vals[3]
                      dataset$Aq_val[i] = val.aq[i]# write values
                      dataset$Aq_SD[i] = sd.aq[i] # write values
                      dataset$Aq_units[i] = units.aq[i]# write values
                      axis.x[i] = NA
                      val.x[i] = NA
                      sd.x[i] = NA
                      units.x[i] = ''
# convert ppb to mol/L                      
                    }else if (units.x[i] == 'ppb'){
                    # calculate molar mass of sorbate first                    
                    if (!is.na(sorbates[i])){
                      if (sorbates[i] %in% mineral.ref$minerals){
                        molar.mass = as.numeric(mineral.ref[mineral.ref$minerals == sorbates[i], 'masses'])
                      }else{
                        molar.mass = mass(s_element(sorbates[i]))
                      }
                    }
                    new.vals = convert(val = val.x[i], val.sd = sd.x[i], new.units = 'mol/L', new.axis = 'aqueous',
                                       user.sd = sd16, conversion = '(z * 0.001) / (1000 * molar.mass)',
                                       molar.mass = molar.mass)
                    val.aq[i] = as.numeric(new.vals[1])
                    sd.aq[i] = as.numeric(new.vals[2])
                    units.aq[i] = new.vals[3]
                    dataset$Aq_val[i] = val.aq[i]# write values
                    dataset$Aq_SD[i] = sd.aq[i] # write values
                    dataset$Aq_units[i] = units.aq[i]# write values
                    axis.x[i] = NA
                    val.x[i] = NA
                    sd.x[i] = NA
                    units.x[i] = ''
# convert ppm to mol/L
                  }else if (units.x[i] == 'ppm' | units.x[i] == 'mg/L'){
                    # calculate molar mass of sorbate first                    
                    if (!is.na(sorbates[i])){
                      if (sorbates[i] %in% mineral.ref$minerals){
                        molar.mass = as.numeric(mineral.ref[mineral.ref$minerals == sorbates[i], 'masses'])
                      }else{
                        molar.mass = mass(s_element(sorbates[i]))
                      }
                    }
                    new.vals = convert(val = val.x[i], val.sd = sd.x[i], new.units = 'mol/L', new.axis = 'aqueous',
                                       user.sd = sd16, conversion = 'z / (1000 * molar.mass)', molar.mass = molar.mass)
                    val.aq[i] = as.numeric(new.vals[1])
                    sd.aq[i] = as.numeric(new.vals[2])
                    units.aq[i] = new.vals[3]
                    dataset$Aq_val[i] = val.aq[i]# write values
                    dataset$Aq_SD[i] = sd.aq[i] # write values
                    dataset$Aq_units[i] = units.aq[i]# write values
                    axis.x[i] = NA
                    val.x[i] = NA
                    sd.x[i] = NA
                    units.x[i] = ''
                  }
#####
##### Mineral value converstion to g/L
#####
                }else if (axis.x[i] == 'Mineral_val'){
# converting mineral_val mg/L to g/L                    
                    if (units.x[i] == 'mg/L'){ # 19JUL20
                    new.vals = convert(val = val.x[i], val.sd = sd.x[i], new.units = 'g/L', new.axis = 'Mineral_val', 
                                     user.sd = sd16, conversion = 'z / 1000')
                    val.mineral[i] = as.numeric(new.vals[1])
                    sd.mineral[i] = as.numeric(new.vals[2])
                    units.mineral[i] = new.vals[3]
                    dataset$Mineral_val[i] = val.mineral[i]# write values
                    dataset$Mineral_SD[i] = sd.mineral[i] # write values
                    dataset$Mineral_units[i] = units.mineral[i]# write values
                    axis.x[i] = NA
                    val.x[i] = NA
                    sd.x[i] = NA
                    units.x[i] = ''
                    
                    
                    
                    if (minerals[i] %in% mineral.ref$minerals){
                      molar.mass = as.numeric(mineral.ref[mineral.ref$minerals == minerals[i], 'masses'])
                      new.vals = convert(val = val.mineral[i], val.sd = sd.mineral[i], user.sd = sd5, new.units = 'mol/L',
                                         conversion = 'z / molar.mass', molar.mass = molar.mass)
                      val.mineral[i] = as.numeric(new.vals[1])
                      sd.mineral[i] = as.numeric(new.vals[2])
                      units.mineral[i] = new.vals[3]
                    }
                    
                    
# converting mineral_val g/l to g/l
                    }else if (units.x[i] == 'g/L'){ # 19JUL20
                    new.vals = convert(val = val.x[i], val.sd = sd.x[i], new.units = 'g/L', new.axis = 'Mineral_val', 
                                       user.sd = sd16)
                    val.mineral[i] = as.numeric(new.vals[1])
                    sd.mineral[i] = as.numeric(new.vals[2])
                    units.mineral[i] = new.vals[3]
                    dataset$Mineral_val[i] = val.mineral[i]# write values
                    dataset$Mineral_SD[i] = sd.mineral[i] # write values
                    dataset$Mineral_units[i] = units.mineral[i]# write values
                    axis.x[i] = NA
                    val.x[i] = NA
                    sd.x[i] = NA
                    units.x[i] = ''
                    
                    if (minerals[i] %in% mineral.ref$minerals){
                      molar.mass = as.numeric(mineral.ref[mineral.ref$minerals == minerals[i], 'masses'])
                      new.vals = convert(val = val.mineral[i], val.sd = sd.mineral[i], user.sd = sd5, new.units = 'mol/L',
                                         conversion = 'z / molar.mass', molar.mass = molar.mass)
                      val.mineral[i] = as.numeric(new.vals[1])
                      sd.mineral[i] = as.numeric(new.vals[2])
                      units.mineral[i] = new.vals[3]
                    }
                    
                    }  
#####                  
##### very special case where solid solution ratio CAN be calculated USING Kd and sorbed data                   
#####
                 }else if (axis.x[i] == 'sorbed' & units.x[i] == 'mmol/kg' & axis.y[i] == 'Kd' & units.y[i] == 'mL/g' & is.na(val.mineral[i])){ # 19JUL20
                 val.aq[i] = val.x[i]/1000/val.y[i]
                 sd.aq[i] = (sd16^2 + sd14^2)^0.5 * val.aq[i]
                 units.aq[i] = 'mol/L'
                 val.mineral[i] = (val.sorbate[i] - val.aq[i]) / (val.x[i]/1000000) # update values
                 sd.mineral[i] = ((((sd.sorbate[i])^2 + (sd.aq[i])^2)^0.5 / (val.sorbate[i] - val.aq[i]))^2 + sd16^2)^0.5 * val.mineral[i]
                 units.mineral[i] = 'g/L' 
                 val.sorbed[i] = val.sorbate[i] - val.aq[i]
                 sd.sorbed[i] = ((sd.sorbate[i])^2 + (sd.aq[i])^2)^0.5
                 units.sorbed[i] = 'mol/L'   
#
                 dataset$Aq_val[i] = val.aq[i]
                 dataset$Aq_SD[i] = sd.aq[i]
                 dataset$Aq_units[i] = units.aq[i]
                 dataset$Mineral_val[i] = val.mineral[i]
                 dataset$Mineral_SD = sd.mineral[i]
                 dataset$Mineral_units[i] = units.mineral[i]
                 dataset$Sorbed_val[i] = val.sorbed[i]
                 dataset$Sorbed_SD[i] = sd.sorbed[i]
                 dataset$Sorbed_units[i] = units.sorbed[i]
                 axis.x[i] = NA
                 val.x[i] = NA
                 sd.x[i] = NA
                 units.x[i] = ''
                 
                 if (minerals[i] %in% mineral.ref$minerals){
                   molar.mass = as.numeric(mineral.ref[mineral.ref$minerals == minerals[i], 'masses'])
                   new.vals = convert(val = val.mineral[i], val.sd = sd.mineral[i], user.sd = sd5, new.units = 'mol/L',
                                      conversion = 'z / molar.mass', molar.mass = molar.mass)
                   val.mineral[i] = as.numeric(new.vals[1])
                   sd.mineral[i] = as.numeric(new.vals[2])
                   units.mineral[i] = new.vals[3]
                 }
                 
                 
                 
#####
#####  converstion of sorbed values to mol/L                 
#####                 
                }else if (axis.x[i] == 'sorbed'){
# sorbed conversion mol/kg to mol/L 
                    if (units.x[i] == 'mol/kg'){
                      if (minerals[i] %in% mineral.ref$minerals){
                        molar.mass = as.numeric(mineral.ref[mineral.ref$minerals == minerals[i], 'masses'])
                      }else{
                        molar.mass = mass(s_element(minerals[i]))
                      }
                      min.val = val.mineral[i] * molar.mass
                        new.vals = convert(val = val.x[i], val.sd = sd.x[i], new.units = 'mol/L', new.axis = 'sorbed',
                                           user.sd = sd16, conversion = '(z / 1000) * min.val', min.val = min.val)
                        val.sorbed[i] = as.numeric(new.vals[1])
                        sd.sorbed[i] = as.numeric(new.vals[2])
                        units.sorbed[i] = new.vals[3]
                        dataset$Sorbed_val[i] = val.sorbed[i]# write values
                        dataset$Sorbed_SD[i] = sd.sorbed[i] # write values
                        dataset$Sorbed_units[i] = units.sorbed[i]# write values
                        axis.x[i] = NA
                        val.x[i] = NA
                        sd.x[i] = NA
                        units.x[i] = ''
# sorbed conversion mmol/kg to mol/L 
                      }else if (units.x[i] == 'mmol/kg'){
                        if (minerals[i] %in% mineral.ref$minerals){
                          molar.mass = as.numeric(mineral.ref[mineral.ref$minerals == minerals[i], 'masses'])
                        }else{
                          molar.mass = mass(s_element(minerals[i]))
                        }
                        min.val = val.mineral[i] * molar.mass
                      new.vals = convert(val = val.x[i], val.sd = sd.x[i], new.units = 'mol/L', new.axis = 'sorbed',
                                         user.sd = sd16, conversion = '(z / 1000000) * min.val', min.val = min.val)
                      val.sorbed[i] = as.numeric(new.vals[1])
                      sd.sorbed[i] = as.numeric(new.vals[2])
                      units.sorbed[i] = new.vals[3]
                      dataset$Sorbed_val[i] = val.sorbed[i]# write values
                      dataset$Sorbed_SD[i] = sd.sorbed[i] # write values
                      dataset$Sorbed_units[i] = units.sorbed[i]# write values
                      axis.x[i] = NA
                      val.x[i] = NA
                      sd.x[i] = NA
                      units.x[i] = ''
                    }
#####                    
##### SURFACE CHARGE CONVERSIONS AND TRANSFERS ###
#####
                }else if (axis.x[i] == 'total H(+)'){ # 8JUL20
                  # convert total mol/L H+ to microC/cm2                 
                  if (units.x[i] == 'mol/L'){
                    
                    if (minerals[i] %in% mineral.ref$minerals){
                      molar.mass = as.numeric(mineral.ref[mineral.ref$minerals == minerals[i], 'masses'])
                    }else{
                      molar.mass = mass(s_element(minerals[i]))
                    }
                    min.val = val.mineral[i] * molar.mass
                    
                    
                    new.vals = convert(val = val.x[i], val.sd = sd.x[i], new.units = 'microC/cm2', new.axis = 'charge',
                                       user.sd = sd11, conversion = '(z - (10^(-pH)) + (10^(-1*(14-pH)))) *
                                           (1/min.val) * (1/area) * (1000000/10000) * 96485', area = areas[i], 
                                       pH = pHs[i], min.val = min.val)
                    val.charges[i] = as.numeric(new.vals[1])
                    sd.charges[i] = as.numeric(new.vals[2])
                    units.charges[i] = new.vals[3]
                    axis.x[i] = NA
                    val.x[i] = NA
                    sd.x[i] = NA
                    units.x[i] = ''
                    # convert total mmol/L H+ to microC/cm2         
                  }else if (units.x[i] == 'mmol/L'){ # 19JUL20
                    if (minerals[i] %in% mineral.ref$minerals){
                      molar.mass = as.numeric(mineral.ref[mineral.ref$minerals == minerals[i], 'masses'])
                    }else{
                      molar.mass = mass(s_element(minerals[i]))
                    }
                    min.val = val.mineral[i] * molar.mass
                    new.vals = convert(val = val.x[i], val.sd = sd.x[i], new.units = 'microC/cm2', new.axis = 'charge',
                                       user.sd = sd11, conversion = '((z / 1000) - (10^(-pH)) + (10^(-1*(14-pH)))) *
                                           (1/min.val) * (1/area) * (1000000/10000) * 96485', area = areas[i], # changed 16JUL20
                                       pH = pHs[i], min.val = min.val)
                    val.charges[i] = as.numeric(new.vals[1])
                    sd.charges[i] = as.numeric(new.vals[2])
                    units.charges[i] = new.vals[3]
                    axis.x[i] = NA
                    val.x[i] = NA
                    sd.x[i] = NA
                    units.x[i] = ''
                  }
#####
##### convert ratio to electrolyte concentration  
#####                  
                }else if (axis.x[i] == 'ratio'){ # 8JUL20
# convert C(+4)/Sorbate to C(+4)                 
                    if (units.x[i] == 'C(+4)/Sorbate'){
                      min.val = val.x[i]  
                      axis.x[i] = 'C(+4)'
                        val.x[i] = min.val * dataset$Sorbate_val[i]
                        sd.x[i] = sd16 * dataset$Sorbate_val[i]
                        units.x[i] = 'mol/L'
# convert Citrate(-3)/Sorbate to Citrate(-3)         
                    }else if (units.x[i] == 'Citrate(-3)/Sorbate'){ # 19JUL20
                      min.val = val.x[i]  
                      axis.x[i] = 'Citrate(-3)'
                      val.x[i] = min.val * dataset$Sorbate_val[i]
                      sd.x[i] = sd16 * dataset$Sorbate_val[i]
                      units.x[i] = 'mol/L'
# convert F(-1)/Sorbate to F(-1) 
                    }else if (units.x[i] == 'F(-1)/Sorbate'){ # 19JUL20
                      min.val = val.x[i]
                      axis.x[i] = 'F(-1)'
                      val.x[i] = min.val * dataset$Sorbate_val[i]
                      sd.x[i] = sd16 * dataset$Sorbate_val[i]
                      units.x[i] = 'mol/L'
# convert Mo(+6)/Sorbate to Mo(+6)
                    }else if (units.x[i] == 'Mo(+6)/Sorbate'){ # 19JUL20
                      min.val = val.x[i]
                      axis.x[i] = 'Mo(+6)'
                      val.x[i] = min.val * dataset$Sorbate_val[i]
                      sd.x[i] = sd16 * dataset$Sorbate_val[i]
                      units.x[i] = 'mol/L'
# convert Oxalate(-2)/Sorbate to Oxalate(-2)
                    }else if (units.x[i] == 'Oxalate(-2)/Sorbate'){ # 19JUL20
                      min.val = val.x[i]
                      axis.x[i] = 'Oxalate(-2)'
                      val.x[i] = min.val * dataset$Sorbate_val[i]
                      sd.x[i] = sd16 * dataset$Sorbate_val[i]
                      units.x[i] = 'mol/L'
# convert P(+5)/Sorbate to P(+5)
                    }else if (units.x[i] == 'P(+5)/Sorbate'){ # 19JUL20
                      min.val = val.x[i]
                      axis.x[i] = 'P(+5)'
                      val.x[i] = min.val * dataset$Sorbate_val[i]
                      sd.x[i] = sd16 * dataset$Sorbate_val[i]
                      units.x[i] = 'mol/L'
# convert S(+6)/Sorbate to S(+6)
                    }else if (units.x[i] == 'S(+6)/Sorbate'){ # 19JUL20
                      min.val = val.x[i]
                      axis.x[i] = 'S(+6)'
                      val.x[i] = min.val * dataset$Sorbate_val[i]
                      sd.x[i] = sd16 * dataset$Sorbate_val[i]
                      units.x[i] = 'mol/L'
# convert Si(+4)/Sorbate to Si(+4)
                    }else if (units.x[i] == 'Si(+4)/Sorbate'){ # 19JUL20
                      min.val = val.x[i]
                      axis.x[i] = 'Si(+4)'
                      val.x[i] = min.val * dataset$Sorbate_val[i]
                      sd.x[i] = sd16 * dataset$Sorbate_val[i]
                      units.x[i] = 'mol/L'
                    }
#####
##### convert OH- to surface charge 
#####
                }else if (axis.x[i] == 'total OH(-)'){
# convert total mol/L OH- to microC/cm2  
                    if (units.x[i] == 'mol/L'){
                      if (minerals[i] %in% mineral.ref$minerals){
                        molar.mass = as.numeric(mineral.ref[mineral.ref$minerals == minerals[i], 'masses'])
                      }else{
                        molar.mass = mass(s_element(minerals[i]))
                      }
                      min.val = val.mineral[i] * molar.mass
                        new.vals = convert(val = val.x[i], val.sd = sd.x[i], new.units = 'microC/cm2', new.axis = 'charge',
                                           user.sd = sd11, conversion = '(-1 * z - (10^(-pH)) + (10^(-1*(14-pH)))) *
                                           (1/min.val) * (1/area) * (1000000/10000) * 96485', area = areas[i], pH = pHs[i],
                                           min.val = min.val)
                        val.charges[i] = as.numeric(new.vals[1])
                        sd.charges[i] = as.numeric(new.vals[2])
                        units.charges[i] = new.vals[3]
                        axis.x[i] = NA
                        val.x[i] = NA
                        sd.x[i] = NA
                        units.x[i] = ''
                    }
                }

#####                              
##### transfer electrolyte info to electrolytes columns
#####
# transfer alkalinity in meq/L to electrolytes and convert to mol/L              
                if (axis.x[i] == 'alkalinity' & units.x[i] == 'meq/L'){
                    for (n in c(1:6)){
                        elyte = paste('Electrolyte', n, sep = '')
                        elyte.val = paste('Electrolyte', n, '_val', sep = '')
                        elyte.sd = paste('Electrolyte', n, '_SD', sep = '')
                        elyte.unit = paste('Electrolyte', n, '_units', sep = '')
                        elytes = as.vector(dataset[,elyte])
                        val.elytes = as.vector(dataset[,elyte.val])
                        sd.elytes = as.vector(dataset[,elyte.sd])
                        units.elytes = as.vector(dataset[,elyte.unit])
                        
                        if (is.na(elytes[i]) | elytes[i] == ''){
                            new.vals = convert(val = val.x[i], val.sd = sd.x[i], new.units = 'mol/L',user.sd = sd2,
                                               conversion = 'z / 1000')
                            elytes[i] = 'C(+4)' # transfer elyte name
                            val.elytes[i] = as.numeric(new.vals[1])
                            sd.elytes[i] = as.numeric(new.vals[2])
                            units.elytes[i] = new.vals[3]
                            axis.x[i] = NA
                            val.x[i] = NA
                            sd.x[i] = NA
                            units.x[i] = NA
                            dataset[,elyte] = elytes
                            dataset[,elyte.val] = val.elytes
                            dataset[,elyte.sd] = sd.elytes
                            dataset[,elyte.unit] = units.elytes
                            break
                        }
                    }
                    
                }else{
                    for (n in c(1:6)){
                        elyte = paste('Electrolyte', n, sep = '')
                        elyte.val = paste('Electrolyte', n, '_val', sep = '')
                        elyte.sd = paste('Electrolyte', n, '_SD', sep = '')
                        elyte.unit = paste('Electrolyte', n, '_units', sep = '')
                        elytes = as.vector(dataset[,elyte])
                        val.elytes = as.vector(dataset[,elyte.val])
                        sd.elytes = as.vector(dataset[,elyte.sd])
                        units.elytes = as.vector(dataset[,elyte.unit])
                        
                        if (is.na(elytes[i]) | elytes[i] == ''){
                            if (units.x[i] == 'mg/L'){ # 7JUL20
                                if (axis.x[i] %in% mineral.ref$minerals){
                                    molar.mass = as.numeric(mineral.ref[mineral.ref$minerals == axis.x[i], 'masses'])
                                }else{
                                    molar.mass = mass(s_element(minerals[i]))
                                }
                                new.vals = convert(val = val.x[i], val.sd = sd.x[i], new.units = 'mol/L',user.sd = sd2,
                                                   conversion = 'z / (1000 * molar.mass)', molar.mass = molar.mass)
                                elytes[i] = axis.x[i]
                                val.elytes[i] = as.numeric(new.vals[1])
                                sd.elytes[i] = as.numeric(new.vals[2])
                                units.elytes[i] = new.vals[3]
                                
                            }
                            else if (units.x[i] == 'mol/L'){
                                new.vals = convert(val = val.x[i], val.sd = sd.x[i], new.units = 'mol/L', user.sd = sd2)
                                elytes[i] = axis.x[i]
                                val.elytes[i] = as.numeric(new.vals[1])
                                sd.elytes[i] = as.numeric(new.vals[2])
                                units.elytes[i] = new.vals[3]
                            }
                            axis.x[i] = NA
                            val.x[i] = NA
                            sd.x[i] = NA
                            units.x[i] = NA
                            dataset[,elyte] = elytes
                            dataset[,elyte.val] = val.elytes
                            dataset[,elyte.sd] = sd.elytes
                            dataset[,elyte.unit] = units.elytes
                            break
                            
                        }
                    }
                }
            }
        }

        
        
        #####
        #####
        #####  THIS IS AN ADJUSTMENT TO CONVERT ALL CHARGE INFORMATION TO H+ SURFACE EXCESS
        #####
        #####
        for (i in c(1:p)){
          if (units.charges[i] != '' & !is.na(units.charges[i])){
            #####
            ##### converting surface charge from microC/cm2 to mol/L H+ surface excess
            #####
            if (units.charges[i] == 'microC/cm2'){
              if (minerals[i] %in% mineral.ref$minerals){
                molar.mass = as.numeric(mineral.ref[mineral.ref$minerals == minerals[i], 'masses'])
              }else{
                molar.mass = mass(s_element(minerals[i]))
              }
              min.val = val.mineral[i] * molar.mass
              new.vals = convert(val = val.charges[i], val.sd = sd.charges[i], new.units = 'mol/L', new.axis = 'charge',
                                 user.sd = sd11, conversion = 'z / 96485 /1000000 * 10000 * min.val * area', area = areas[i], min.val = min.val)
              val.charges[i] = as.numeric(new.vals[1])
              sd.charges[i] = as.numeric(new.vals[2])
              units.charges[i] = new.vals[3]
              dataset$SurfCharge_val[i] = val.charges[i]# write values
              dataset$SurfCharge_SD[i] = sd.charges[i] # write values
              dataset$SurfCharge_units[i] = units.charges[i]# write values
            }
          }
        }        
        
        
# updates all the relevant information into the main dataframe
        dataset$X_axis = axis.x # update values
        dataset$X_val = val.x # update values
        dataset$X_SD = sd.x # update values
        dataset$X_units = units.x # update values
        
        dataset$Mineral_val = val.mineral # update values
        dataset$Mineral_SD = sd.mineral # update values
        dataset$Mineral_units = units.mineral # update values
        
        dataset$SurfCharge_val = val.charges # update values
        dataset$SurfCharge_SD = sd.charges # update values
        dataset$SurfCharge_units = units.charges # update values
        
        dataset$pH = pHs # update values
        dataset$pH_SD = sd.pHs # update values
        
        #####################################################################
        ### HANDLE SORBED/AQUEOUS CONVERSIONS, TRANSFERS, AND ESTIMATIONS ###
        #####################################################################
        sorbates = dataset$Sorbate # read values
        val.sorbate = dataset$Sorbate_val # read values
        sd.sorbate = dataset$Sorbate_SD # read values
        units.sorbate = dataset$Sorbate_units # read values
        
        val.sorbed = dataset$Sorbed_val # read values
        sd.sorbed = dataset$Sorbed_SD # read values
        units.sorbed = dataset$Sorbed_units # read values
        
        val.aq = dataset$Aq_val # read values
        sd.aq = dataset$Aq_SD # read values
        units.aq = dataset$Aq_units # read values
        
        axis.x = dataset$X_axis # read values
        axis.x = replace(axis.x, c(axis.x == ''), NA)
        val.x = dataset$X_val # read values
        sd.x = dataset$X_SD # read values
        units.x = dataset$X_units # read values
        
        axis.y = dataset$Y_axis # read values
        axis.y = replace(axis.y, c(axis.y == ''), NA)
        val.y = dataset$Y_val # read values
        sd.y = dataset$Y_SD # read values
        units.y = dataset$Y_units # read values
        
        minerals = dataset$Mineral # read values
        val.mineral = dataset$Mineral_val # read values
        
        for (i in c(1:p)){
#####
##### use available Kds to populate the table with aqueous, sorbed, and sorbate concentrations
##### ideally this would only pind the y axes with kds
#####  I DON'T KNOW WHY I HAVE TO USE NA HERE...I SHOULD JUST BE ABLE TO LOOK FOR KD VALUES!!!
              if (!is.na(axis.y[i])){
                    if (minerals[i] %in% mineral.ref$minerals){
                        molar.mass = as.numeric(mineral.ref[mineral.ref$minerals == minerals[i], 'masses'])
                    }else{
                        molar.mass = mass(s_element(minerals[i]))
                    }
                    min.val = val.mineral[i] * molar.mass
# conversion when sorbate data available
                    if (val.sorbate[i] != '' & !is.na(val.sorbate[i])){
                    val.aq[i] = val.sorbate[i] / (val.y[i] / 1000 * min.val + 1)
                    sd.aq[i] = ((((sd.y[i]) / 1000 * min.val)/(val.y[i] / 1000 * min.val + 1))^2 + (sd.sorbate[i]/val.sorbate[i])^2)^(0.5) * val.aq[i]
                    units.aq[i] = 'mol/L'
                    val.sorbed[i] = (val.sorbate[i] * min.val)/((1000 / val.y[i]) + min.val)
                    sd.sorbed[i] = ((sd.y[i]/1000 / (val.y[i] / 1000 + min.val * (val.y[i] / 1000)^2))^2 + (sd.sorbate[i]/val.sorbate[i])^2)^0.5 * val.sorbed[i]
#                    val.sorbed[i] = val.sorbate[i] - val.aq[i]
#                    sd.sorbed[i] = (sd.sorbate[i]^2 + sd.aq[i]^2)^(0.5)
                    units.sorbed[i] = 'mol/L'
# conversion when sorbed data available
                    }else if (val.sorbed[i] != '' & !is.na(val.sorbed[i])){
                    val.aq[i] = val.sorbed[i] / min.val / val.y[i] * 1000
                    sd.aq[i] = ((sd.y[i]/val.y[i])^2 + (sd.sorbed[i]/val.sorbed[i])^2)^0.5 * val.aq[i]
                    units.aq[i] = 'mol/L'
                    val.sorbate[i] = val.sorbed[i] * (1 + 1000 / val.y[i] / min.val) 
                    sd.sorbate[i] = ((sd.sorbed[i]/val.sorbed[i])^2 + (sd.y[i] / 1000 / ((val.y[i]/1000)^2 * min.val + val.y[i]/1000))^2)^0.5 * val.sorbate[i]
#                    val.sorbate[i] = val.sorbed[i] + val.aq[i]
#                    sd.sorbate[i] = (sd.sorbed[i]^2 + sd.aq[i]^2)^(0.5)
                    units.sorbate[i] = 'mol/L'
# conversion when aq data available
                    }else if (val.aq[i] != '' & !is.na(val.aq[i])){
                      val.sorbed[i] = val.y[i] / 1000 * val.aq[i] * min.val
                      sd.sorbed[i] = ((sd.y[i]/val.y[i])^2 + (sd.aq[i]/val.aq[i])^2)^0.5 * val.sorbed[i]
                      units.sorbed[i] = 'mol/L'
                      val.sorbate[i] = val.aq[i] * (val.y[i] / 1000 * min.val + 1)
                      sd.sorbate[i] =  ((sd.aq[i]/val.aq[i])^2 + (sd.y[i]/1000 * min.val / (1 + val.y[i]/1000 * min.val))^2)^0.5 * val.sorbate[i]
#                      val.sorbate[i] = val.sorbed[i] + val.aq[i]
#                      sd.sorbate[i] = (sd.sorbed[i]^2 + sd.aq[i]^2)^(0.5)
                      units.sorbate[i] = 'mol/L'
                    }
#####
#####  here we need to fill in any blanks in the aqueous, sorbed, or sorbate values
#####                    
                }
          
# I THINK WE RUN THIS AS A SEPARATE IF STATEMENT.
          
          if (is.na(val.aq[i])){
                  val.aq[i] = val.sorbate[i] - val.sorbed[i]
                  sd.aq[i] = (sd.sorbate[i]^2 + sd.sorbed[i]^2)^(0.5)
                  units.aq[i] = 'mol/L'
                }else if (is.na(val.sorbed[i])){
                  val.sorbed[i] = val.sorbate[i] - val.aq[i]
                  sd.sorbed[i] = (sd.sorbate[i]^2 + sd.aq[i]^2)^(0.5)
                  units.sorbed[i] = 'mol/L'
                }else if (is.na(val.sorbate[i])){
                  val.sorbate[i] = val.aq[i] + val.sorbed[i]
                  sd.sorbate[i] = (sd.aq[i]^2 + sd.sorbed[i]^2)^(0.5)
                  units.sorbate[i] = 'mol/L'
                }
            
        }

      
        for (i in c(1:p)){
          #####
          ##### remove negative and zero values, set to SD of the particular AQ, S, and Total sorbate concentration.
          ##### 

         if (!is.na(val.aq[i]) & !is.na(sd.aq[i])){
#             if (val.aq[i] <= sd.aq[i]){
              if (val.aq[i] <= 0){
           val.aq[i] = sd.aq[i]
         }
        }
          if (!is.na(val.sorbed[i]) & !is.na(sd.sorbed[i])){
#            if (val.sorbed[i] <= sd.sorbed[i]){
            if (val.sorbed[i] <= 0){
              val.sorbed[i] = sd.sorbed[i] 
          }
          }
          if (!is.na(val.sorbate[i]) & !is.na(sd.sorbate[i])){
#          if (val.sorbate[i] <= sd.sorbate[i]){
            if (val.sorbate[i] <= 0){
              val.sorbate[i] = sd.sorbate[i]
           }
          }
        }     

                  
        dataset$Y_axis = axis.y # read values
        dataset$Sorbed_val = val.sorbed # update values
        dataset$Sorbed_SD = sd.sorbed # update values
        dataset$Sorbed_units = units.sorbed # update values
        
        dataset$Aq_val = val.aq # update values
        dataset$Aq_SD = sd.aq # update values
        dataset$Aq_units = units.aq # update values
        
        dataset$Sorbate_val = val.sorbate # update values
        dataset$Sorbate_SD = sd.sorbate # update values
        dataset$Sorbate_units = units.sorbate
        
        ########################################
        ### CONVERT MINERAL INFO BACK TO g/L ###
        ########################################
        val.mineral = dataset$Mineral_val # read values
        sd.mineral = dataset$Mineral_SD # read values
        units.mineral = dataset$Mineral_units # read values
        
        for (i in c(1:p)){
            if (units.mineral[i] != '' & !is.na(units.mineral[i])){
                if (units.mineral[i] == 'mol/L'){
                    if (minerals[i] %in% mineral.ref$minerals){
                        molar.mass = as.numeric(mineral.ref[mineral.ref$minerals == minerals[i], 'masses'])
                    }else{
                        molar.mass = mass(s_element(minerals[i]))
                    }
                    new.vals = convert(val = val.mineral[i], val.sd = sd.mineral[i], user.sd = sd5, new.units = 'g/L',
                                       conversion = 'z * molar.mass', molar.mass = molar.mass)
                    val.mineral[i] = as.numeric(new.vals[1])
                    sd.mineral[i] = as.numeric(new.vals[2])
                    units.mineral[i] = new.vals[3]
                }
            }
        }
        
        dataset$Mineral_val = val.mineral # update values
        dataset$Mineral_SD = sd.mineral # update values
        dataset$Mineral_units = units.mineral # update values
        
        ######################################
        ### REMOVE X/Y COLUMNS FROM OUTPUT ###
        ######################################
        #col.keep = c('X_axis', 'X_val', 'X_SD', 'X_units', 'Y_axis', 'Y_val', 'Y_SD', 'Y_units') # Elliot Chang Commented Out #
        #dataset = dataset[,!(colnames(dataset) %in% col.keep)]
        
        ####################################
        ### CONVERT MISSING VALUES TO NA ###
        ####################################

# changed the way you fill in blanks with NA - Mavrik Zavarin 4/6/2023                
        dataset[dataset == "" | dataset == " "] <- NA  # Replace blank & space by NA

# old way you fill in blanks with NA - Mavrik Zavarin 4/6/2023
#        for (i in c(1:ncol(dataset))){
#            dirty = as.vector(dataset[,i])
#            # clean = na_if(dirty, 0)
#            cleanest = na_if(dirty, ' ')
#            dataset[,i] = cleanest
#        }
        
        ###############################################
        ### ROUND ALL NUMERIC COLUMNS TO 6 SIG FIGS ###
        ###############################################
        for (i in c(1:ncol(dataset))){
            if (is.double(dataset[,i])){
                values = as.vector(dataset[,i])
                dataset[,i] = signif(values, 6)
            }
        }
        
        ##############
        ### OUTPUT ###
        ##############
        dataset
    })
    
    output$sc.cleaned = DT::renderDataTable({ # output datatable to UI
        sc.cleaned = sc.dataset()
        sc.cleaned
    })
    
    output$downloadData = downloadHandler(
        filename = function() {
            paste('sc.dataset', '.csv', sep = '')
        },
        content = function(file) {
            write.csv(sc.dataset(), file, row.names = F) # user must import in Excel, not just open
    })
    
       ##################
    ###### FILTERER TAB ######
       ##################
    
    clean.datasetInput = reactive({ # read in Clean-Dataset.xlsx as dataframe
        filepath = input$clean.dataset
        clean.dataset = read.csv(filepath$datapath)
        clean.dataset
    })
    
    output$mineral.dropdown = renderUI({
        if (is.null(input$clean.dataset)){
            clean.dataset = data.frame(Mineral = character(), Sorbate = character())
        }else{
            clean.dataset = clean.datasetInput()
        }
        minerals = unique(clean.dataset$Mineral)
        selectInput('mineral.select', label = h4('Mineral(s)'), choices = minerals, multiple = T)
    })
    
    output$sorbate.dropdown = renderUI({
        if (is.null(input$clean.dataset)){
            clean.dataset = data.frame(Mineral = character(), Sorbate = character())
        }else{
            clean.dataset = clean.datasetInput()
        }
        sorbates = unique(clean.dataset$Sorbate)
        selectInput('sorbate.select', label = h4('Sorbate(s)'), choices = sorbates, multiple = T)
    })
    
    sc.subset = eventReactive(input$filter, {
        clean.dataset = clean.datasetInput()
        minerals = input$mineral.select
        sorbates = input$sorbate.select
        sc.subset = clean.dataset[clean.dataset$Mineral %in% minerals & clean.dataset$Sorbate %in% sorbates,]
        sc.subset
    })

    output$sc.subset = DT::renderDataTable({ # output subset to UI
        sc.subset = sc.subset()
        sc.subset
    })
    
    output$downloadSubset = downloadHandler(
        filename = function() {
            paste('sc.subset', '.csv', sep = '')
        },
        content = function(file) {
            write.csv(sc.subset(), file, row.names = F) # user must import in Excel, not just open
        })
    
       #####################
    ###### FORMATTER TAB ######
       #####################
    
    sub.datasetInput = reactive({ # read in sc.subset.xlsx as dataframe
        filepath = input$sub.dataset
        sub.dataset = read.csv(filepath$datapath)
        sub.dataset
    })
    
    sample.formatInput = reactive({ # read in sample.txt as dataframe
        filepath = input$sample
        sample.format = readChar(filepath$datapath, file.info(filepath$datapath)$size)
        sample.format = stringr::str_remove_all(sample.format, '\r')
        sample.format
    })
    
    outfile = eventReactive(input$format, {
        subset.data = sub.datasetInput()
        format.sample = sample.formatInput()
        format.output = data.frame()
        
        for (k in c(1:nrow(subset.data))){
            chunk = format.sample
            chunk = unlist(stringr::str_split(chunk, '\n'), use.names = F)
            locators = c('Num', 'Obs', 'Weighting', colnames(subset.data))
            for (i in c(1:length(chunk))){
                for (n in c(1:length(locators))){
                    if (stringr::str_detect(chunk[i], locators[n])){
                        if (locators[n] == 'Num'){
                            val = k
                        }else if (locators[n] == 'Obs'){
                            val = paste('Obs', k, sep = '')
                        }else if(locators[n] == 'Weighting'){
                            s = subset.data[k, 'Aq_SD']
                            val = signif(eval(parse(text = input$weighting)), 6)
                        }else{
                            val = subset.data[k, locators[n]]
                        }
                        
                        locator = paste('!', locators[n], '!', sep = '')
                        
                        if (is.null(val)){
                            chunk[i] = ''
                            next
                        }else if(is.na(val)){
                            chunk[i] = ''
                            next
                        }
                        
                        if (input$program == 'phreeqc'){ # special vase added 16JUL20 # 19JUL20
                            if ((locators[n] %like% 'Gas') & ((locators[n] %like% '_val') | (locators[n] %like% '_SD'))){
                                val = log(val, base = 10)
                            }
                        }
                        
                        if (input$program == 'pest-instruction'){ # special case added 16JUL20
                            val = paste('!', val, '!', sep = '')
                        }else if(locators[n] != 'Obs' & locators[n] != 'Num' & is.numeric(val)){
                            val = formatC(val, digits = 3, format = 'E')
                        }
                        
                        val = as.character(val)
                        chunk[i] = stringr::str_replace_all(chunk[i], locator, val)
                    }
                }
            }
            
            chunk = chunk[chunk != '']
            chunk = do.call(paste, c(as.list(chunk), sep = '\n'))
            format.output = c(format.output, chunk)
            format.output = do.call(paste, c(as.list(format.output), sep = '\n'))
        }
        
        format.output
    })
    
    output$outfile = renderText({
        outfile = outfile()
        outfile
    })
    
    output$downloadOutfile = downloadHandler(
        filename = function() {
            paste(input$program, '-format', '.txt', sep = '')
        },
        content = function(file) {
            write.table(outfile(), file, row.names = F, col.names = F, quote = F)
        })
})
