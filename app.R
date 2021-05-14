suppressPackageStartupMessages({
  library(shiny)
  library(shinyWidgets)
  library(plotly)
  library(knitr)
  library(rmarkdown)
  library(squire)
  library(tidyverse) 
  library(lubridate)
  library(viridis)
  library(tidycovid19)
  library(ggdark)
  library(future)
  })

temo <- function(r)
{
  # Deaths
  
  deaths <- format_output(x = r, var_select = "deaths") %>%
    mutate(replicate = factor(replicate))
  
  deaths1<-deaths %>% group_by(compartment, t) %>% summarize(deaths=mean(y))
  
  # Infections
  
  infections <- format_output(x = r, var_select = "infections") %>%
    mutate(replicate = factor(replicate))
  
  infections1<-infections %>% group_by(compartment, t) %>% summarize(infections=mean(y))
  
  # Occupied hospital beds
  
  hosp_bed <- format_output(x = r, var_select = "hospital_occupancy") %>%
    mutate(replicate = factor(replicate))
  
  hosp_bed1<-hosp_bed %>% group_by(compartment, t) %>% summarize(hosp_bed=mean(y))
  
  # Require hospital beds 
  
  hospital_demand <- format_output(x = r, var_select = "hospital_demand") %>%
    mutate(replicate = factor(replicate))
  
  hospital_demand1<-hospital_demand %>% group_by(compartment, t) %>% summarize(hospital_demand=mean(y))
  
  # Required ICU beds
  
  ICU_demand <- format_output(x = r, var_select = "ICU_demand") %>%
    mutate(replicate = factor(replicate))
  
  ICU_demand1<-ICU_demand %>% group_by(compartment, t) %>% summarize(ICU_demand=mean(y))
  
  # Occupied ICU Beds
  
  ICU_bed <- format_output(x = r, var_select = "ICU_occupancy") %>%
    mutate(replicate = factor(replicate))
  
  ICU_bed1<-ICU_bed %>% group_by(compartment, t) %>% summarize(ICU_bed=mean(y))
  
  
  # Get Oxygen and Live
  
  IOxGetLive <- format_output(x = r, var_select = "IOxGetLive") %>%
    mutate(replicate = factor(replicate))
  
  IOxGetLive1<-IOxGetLive %>% group_by(compartment, t) %>% summarize(IOxGetLive=mean(y))
  
  # Get Oxygen and Die
  
  IOxGetDie <- format_output(x = r, var_select = "IOxGetDie") %>%
    mutate(replicate = factor(replicate))
  
  IOxGetDie1<-IOxGetDie %>% group_by(compartment, t) %>% summarize(IOxGetDie=mean(y))
  
  # Not Get Oxygen and Live
  
  IOxNotGetLive<- format_output(x = r, var_select = "IOxGetLive") %>%
    mutate(replicate = factor(replicate))
  
  IOxGetLive1<-IOxNotGetLive%>% group_by(compartment, t) %>% summarize(IOxGetLive=mean(y))
  
  
  # Not Get Oxygen and Die
  
  IOxNotGetDie<- format_output(x = r, var_select = "IOxGetLive") %>%
    mutate(replicate = factor(replicate))
  
  IOxGetLive1<-IOxNotGetDie%>% group_by(compartment, t) %>% summarize(IOxGetLive=mean(y))
  
  
  # Get Ventilator and Live
  
  IMVGetLive<- format_output(x = r, var_select = "IOxGetLive") %>%
    mutate(replicate = factor(replicate))
  
  IOxGetLive1<-IMVGetLive%>% group_by(compartment, t) %>% summarize(IOxGetLive=mean(y))
  
  # Get Ventilator and Die
  
  IMVGetDie<- format_output(x = r, var_select = "IOxGetLive") %>%
    mutate(replicate = factor(replicate))
  
  IOxGetLive1<-IMVGetDie%>% group_by(compartment, t) %>% summarize(IOxGetLive=mean(y))
  
  # Not Get Ventilator and Die
  
  IMVNotGetDie<- format_output(x = r, var_select = "IOxGetLive") %>%
    mutate(replicate = factor(replicate))
  
  IOxGetLive1<-IMVNotGetDie%>% group_by(compartment, t) %>% summarize(IOxGetLive=mean(y))
  
  # Not Get Ventilator and Live
  
  IMVNotGetLive<- format_output(x = r, var_select = "IOxGetLive") %>%
    mutate(replicate = factor(replicate))
  
  IOxGetLive1<-IMVNotGetLive%>% group_by(compartment, t) %>% summarize(IOxGetLive=mean(y))
  
  #----------------------------------------------------------------
  
  
  
  data1<-data.frame(time=deaths1$t,deaths=deaths1$deaths,infections=infections1$infections,
                    hosp_bed=hosp_bed1$hosp_bed,hospital_demand=hospital_demand1$hospital_demand,
                    ICU_demand=ICU_demand1$ICU_demand,ICU_bed=ICU_bed1$ICU_bed,
                    IOxGetLive=IOxGetLive1$IOxGetLive,
                    IOxGetDie=IOxGetDie1$IOxGetDie,
                    IOxNotGetLive=IOxGetLive1$IOxGetLive,
                    IOxNotGetDie=IOxGetLive1$IOxGetLive,
                    IMVGetLive=IOxGetLive1$IOxGetLive,
                    IMVGetDie=IOxGetLive1$IOxGetLive,
                    IMVNotGetDie=IOxGetLive1$IOxGetLive,
                    IMVNotGetLive=IOxGetLive1$IOxGetLive)
  
  
}

# Get the population

npop = 47563609 # From the 2019 Census

f= c(0.13,0.13,0.13,0.11,0.09,0.08,0.08,0.06,0.05,
     0.04,0.03,0.02,0.02,0.01,0.01,0.01) # Age proportions Based on the 2019 Census
population =  npop*f      # number in each age class

pop <- get_population("Kenya")
population <- pop$n

population<-round(47563609*population/sum(population))
# Rescale population to the population of Kenya

# Get the mixing matrix

contact_matrix <- get_mixing_matrix("Kenya")
# Different from the Prem et al (POLYMOD) paper 

# Unmitigate scenario
# model using baseline parameters and no control interventions
r <- run_explicit_SEEIR_model(population = population,
                              contact_matrix_set = contact_matrix,
                              country = "Kenya",dt = 1)

unmitigated<-temo(r) %>%
  mutate(scenario_type = "unmitigated")


# Intervention scenarios

r1 <- run_explicit_SEEIR_model(population = population, 
                               tt_contact_matrix = c(0, 2),
                               contact_matrix_set = list(contact_matrix,
                                                         contact_matrix*0.2),
                               country = "Kenya",dt = 1)

scenario1<-temo(r) %>%
  mutate(scenario_type = "scenario1")

#merging the scenarios simulated datasets
scenarios_data <- data.frame(Reduce(function(x, y) merge(x, y, all=TRUE), 
                                    list(unmitigated,scenario1)))

# Calibrate
#*******************************************************************************
#Data importing and cleaning

suppressWarnings({
  covdata <-
    download_jhu_csse_covid19_data()
})

#subset for Kenya
covdataKE <- covdata %>%
  filter(country == "Kenya" & date >= "2020-03-13") 

#converting cumulative count to daily count 
covdataKE1<-covdataKE

covdataKE1$confirmed<-  c(covdataKE$confirmed[1],diff(covdataKE$confirmed))
covdataKE1$deaths<-  c(covdataKE$deaths[1],diff(covdataKE$deaths))
covdataKE1$recovered<-  c(covdataKE$recovered[1],diff(covdataKE$recovered))

#"avoid UNRELIABLE VALUE warning" from the running lie 260-random gen of values
#turn off scientific numbers

options(future.rng.onMisuse="ignore",
        scipen = 999)

# Set up for parallelisation

future::plan(future::multisession())

# Fit model
#taking the beds in ICU and hospital as 38 and 1893 respectively

out <- calibrate(data = covdataKE1,  R0_min = 1.0,  R0_max = 1.5, 
                 R0_step = 0.5,
first_start_date = "2020-03-10",  last_start_date = "2020-03-11",
day_step = 1,  replicates = 10, n_particles = 20,
forecast = 30,
country = "Kenya",
baseline_ICU_bed_capacity = 38,
baseline_hosp_bed_capacity = 1893)

scenario0<-temo(out) %>%
mutate(scenario_type = "scenario0")

# interventions
change_date<-c("2020-06-06") #first dusk to dawn curfew imposed
change_senario1<-c(0.8) # An 20% decline in contacts
# Scenario 1. 20% decline in contact
out1 <- calibrate( data = covdataKE,R0_min = 1.0,R0_max = 1.5,R0_step = 0.5,
                   first_start_date = "2020-03-10",
last_start_date = "2020-03-11",day_step = 1,replicates = 10,
n_particles = 20,
forecast = 30, country = "Kenya",R0_change = change_senario1,
date_R0_change = change_date,
baseline_ICU_bed_capacity = 38,
baseline_hosp_bed_capacity = 1893)

scenario_1<-temo(out1)%>%
mutate(scenario_type = "scenario_1")

#merging the scenarios datasets after calibrating the model

scenarios_data_calibrated <- data.frame(Reduce(function(x, y) merge(x, y, all=TRUE),
list(scenario0,scenario_1)))

#grouping the calibrated data by the invention scenarios
plots_data <- scenarios_data_calibrated %>%
group_by(scenario_type) %>%
ungroup()




# Define UI for application that draws a histogram
ui <- fluidPage(
  
  #call the rmd file with the documentation
    withMathJax(includeMarkdown(file.path("./seir_covid19ke.Rmd"))),
    
    fluidRow(
      h3("Explorative Graphs"),
      splitLayout(
        plotlyOutput("cov19_cases"),
        plotlyOutput("cov19_deaths"),
        plotlyOutput("cov19_recovered")
      )) ,
    
    fluidRow(column(3,
                    h3("Different interventions"),
                    helpText("Unmitigated scenario: model using baseline parameters and no control"),
                    helpText("Scenario 1: Set a 20% reduction in the contact matrix after 2 days"))
             ,
        # Sidebar with a slider input for number of bins
        column(8,
               sidebarPanel(
                   pickerInput(inputId = "scenario_type",
                               label = "Choose the interventions",
                               choices = unique(scenarios_data_calibrated$scenario_type),
                               multiple = TRUE)
               ) 
    ),
    # Show a plot of the generated distribution
    mainPanel(
        plotlyOutput("deathsPlot"),
        plotlyOutput("infectionsPlot"))),
    
  fluidRow(
    h3("Model Diagnostic Plots"),
    splitLayout(
    plotlyOutput("diagnostic_plot_un"),
    plotlyOutput("diagnostic_plot_sc1")))
    
)

# Define server logic required to scenario plots
server <- function(input, output) {
  
  #deaths daily reported confirmed cases
  output$cov19_cases <- renderPlotly({
    ggplot(covdataKE1,
           aes(x = date, y = confirmed)) +
      geom_line(colour = "black")+
      labs(y = "Daily Count",  x = "Date",
           title = "Covid-19 Reported Confirmed Cases") +
      theme_minimal()
  })
  
  #daily reported deaths
  output$cov19_deaths <- renderPlotly({
    ggplot(covdataKE1,
           aes(x = date, y = deaths)) +
      geom_line(colour = "purple")+
      labs(y = "Daily Reported Count",  x = "Date",
           title = "Covid-19 Reported Deaths") +
      dark_theme_minimal()
    
  })
  
  #daily reported recovered patients
  output$cov19_recovered <- renderPlotly({
    ggplot(covdataKE1,
           aes(x = date, y = recovered)) +
      geom_line(colour = "red")+
      labs(y = "Daily Reported Count",  x = "Date",
           title = "Covid-19 Reported Recovered Patients") +
      theme_bw()
  })
  
  
 output$deathsPlot <- renderPlotly({
     
     validate(need(input$scenario_type != "","Choose scenario(s)"))
 
         ggplot(
     plots_data %>% filter(scenario_type == input$scenario_type),
         aes(x = time, y = deaths, colour = scenario_type)) +
        geom_point()+
        geom_line()+
        labs(y = "Daily Deaths",  x = "Time",
             title = "Simulated Daily Deaths") +
        theme_minimal() +
        scale_color_viridis(discrete = TRUE,
                             name = "Scenarios",
                             labels = c("Scenario1","Unmitigated"))
    })
 output$infectionsPlot <- renderPlotly({
     validate(need(input$scenario_type != "",
                   "Please, choose scenario(S)."))
     
     ggplot(
         plots_data %>% filter(scenario_type == input$scenario_type),
         aes(x = time, y = deaths, colour = scenario_type)) +
         geom_line()+
         labs(y = "Infections",  x = "Time",
              title = "Simulated Daily Infections") +
         theme_minimal() +
         scale_color_viridis(discrete = TRUE,
                             name = "Scenarios",
                             labels = c("Scenario1","Unmitigated"))
 })
 
 output$diagnostic_plot_un <- renderPlotly({
   plot(out, particle_fit = TRUE) + 
     ggplot2::xlim(as.Date(c("2020-03-10","2021-05-01"))) + 
     ggplot2::ylim(c(0,15))
 })
 
 output$diagnostic_plot_sc1 <- renderPlotly({
   plot(out1, particle_fit = TRUE) + 
     ggplot2::xlim(as.Date(c("2020-03-10","2021-05-01"))) + 
     ggplot2::ylim(c(0,15))
 })
 
}
    
# Run the application 
shinyApp(ui = ui, server = server)
