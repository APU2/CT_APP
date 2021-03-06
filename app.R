## app.R ##
library(shinydashboard)

# read lookup table
CT_LKUP<-read.csv("CT_LKUP.csv")

# format Temperature
CT_LKUP$Temp<-as.numeric(CT_LKUP$Temp)




ui <- dashboardPage(
          dashboardHeader(title = "Theoretical Ct calculation"),
          dashboardSidebar(title = h3("Site data input" ), 
                           sliderInput("cl",
                                       "Free chlorine dose (mg/l):",
                                       min = 0,
                                       max = 3,
                                       round = -2,
                                       value = 1,
                                       step = 0.025),
                           sliderInput("ph",
                                       "Select pH",
                                       min = 5,
                                       max = 9,
                                       value = 6,
                                       round = -2,
                                       step = 0.1),
                           radioButtons("temp",
                                        "Select closest temperature (DegC)",
                                        choices = c(0,5,10,15,20), selected = 15),
                           numericInput("flow", 
                                        "Select flow (MLD)",
                                        min = 1,
                                        max = 400,
                                        #round = TRUE,
                                        value = 50),
                           numericInput("vol",
                                        "Select contact tank volume (M3):",
                                        min = 5,
                                        max = 1000,
                                        value = 250),
                           radioButtons("source_type",
                                        "Select source type:",
                                        choices = c("Groundwater", "Surface water"),
                                        selected = "Surface water")
          ),
          dashboardBody(
                    h1("Key theoretical disinfection parameters"),
                    h3("Select the dose, pH, temperature, flow, tank volume and source type using the menu on the left. Key disinfection parameters will be calculated and the pH-adjusted CT value will be displayed below"),
                    # Boxes need to be put in a row (or column)
                    fluidRow(
                              valueBoxOutput("rtime"),
                              valueBoxOutput("HOCL_PCT"),
                              valueBoxOutput("min_ct")),
                    h3("Theoretical Ct value"),
                    fluidRow(
                              #valueBoxOutput("CT"),
                              valueBoxOutput("ph_cor_ct")
                    ),
                    h4("This application is provided to illustrate the impact of pH variation on disinfection performance. Theoretical residence time is not a substitute for the calculation of the emprical residence time distribution as short-circuiting may occur within the contact tank."),
                    h4("Contact:"),
                    h5("Any questions regarding the use of this application please contact a.upton@cranfield.ac.uk"),
                    h5("Any questions regarding chlorine disinfection r.a.cheswick@cranfield.ac.uk"),
                    h3("References"),
                    h5("White’s Handbook of Chlorination and Alternative Disinfectants. (2009). Hoboken, NJ, USA: John Wiley & Sons, Inc. doi:10.1002/9780470561331"),
                    h5(citation("shiny")),
                    h5(citation("shinydashboard"))
          )
)

server <- function(input, output)  {
          
          rt <- reactive({retention<-input$vol / ((input$flow/24/60)*1000) 
          return(retention)})
          HOCL <- reactive({hocl<- CT_LKUP[ CT_LKUP$pH == input$ph &  CT_LKUP$Temp == input$temp, "value"]
          return(hocl)})
          Ct <- reactive({contact <- input$cl*rt()
          return(contact)})
          pH_adj_ct <- reactive({ ph_ct <- Ct()*(HOCL()/100)
          return(ph_ct)})
          target_ct <- reactive({ target<- ifelse(input$source_type == "Groundwater", 5, 10)
          return(target)})
          
          
          output$rtime <- renderValueBox({ valueBox(rt() , "Theoretical contact time (minutes)", icon = icon("clock-o"))})
          output$HOCL_PCT <- renderValueBox({valueBox( HOCL() , "HOCl %", icon = icon("line-chart"))})
          output$min_ct <- renderValueBox({valueBox( target_ct(), "Target minimum Ct (mg.min/l)", icon = icon("bullseye"))})
          output$CT <- renderValueBox({valueBox( Ct(), "Theoretical Ct value (mg.min/l)", icon = icon("line-chart"))})
          output$ph_cor_ct <- renderValueBox({valueBox( round(pH_adj_ct(),1) , "pH adjusted Ct value (mg.min/l)" , 
                                                        icon = icon(ifelse( pH_adj_ct() > target_ct(), "thumbs-up", "thumbs-down")),
                                                        color = ifelse( pH_adj_ct() > target_ct(), "green", "red"))})
          
          
          
          
}

shinyApp(ui, server)