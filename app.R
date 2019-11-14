#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(replyr)
library(magrittr)
library(gridExtra)
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Micro Brewery Analysis"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        
        sidebarPanel(
            
            #file Input
            ####################################################################################
            fileInput("beerData", label = h3("Input Beer Data")),
            fileInput("breweryData", label = h3("Input Brewery Data")),

            #ABV or IBU
            ####################################################################################
            selectInput("beerchar", label = h3("IBU or ABV"), 
                        choices = list("IBU" = 1, "ABV" = 2, "Both" = 7), 
                        selected = 1),
            
            
             #Histogram or Boxplot Option
            ####################################################################################
            selectInput("histBox", label = h3("Distribution Plots"),
                        choices = list("Histogram" = 3, "Boxplot" = 4), 
                        selected = 1),

            
            #State Selection
            ####################################################################################
            selectInput("stateInput", label = h3("State"),
                        choices = c("Choose state",
                                    "All States",
                                    "AL ",
                                    "AK",
                                    "AZ",
                                    "AR",
                                    "CA",
                                    "CO",
                                    "CT",
                                    "DE",
                                    "FL",
                                    "GA",
                                    "HI",
                                    "ID",
                                    "IL",
                                    "IN",
                                    "IA",
                                    "KA",
                                    "KY",
                                    "LA",
                                    "ME",
                                    "MD",
                                    "MA",
                                    "MI",
                                    "MN",
                                    "MS",
                                    "MO",
                                    "MT",
                                    "NE",
                                    "NV",
                                    "NH",
                                    "NJ",
                                    "NM",
                                    "NY",
                                    "NC",
                                    "ND",
                                    "OH",
                                    "OK",
                                    "OR",
                                    "PA",
                                    "RI",
                                    "SC",
                                    "SD",
                                    "TN",
                                    "TX",
                                    "UT",
                                    "VT",
                                    "VA",
                                    "WA",
                                    "WV",
                                    "WI",
                                    "WY"
                        ),
                        selected = 1),    
            #Regression Line Selection
            ####################################################################################
            radioButtons("regressionLine", label = h3("Include Regression Line?"), 
                        choices = list("Yes" = 5, "No" = 6), 
                        selected = 1)),


        # Show a plot of the generated distribution
        ####################################################################################
        ####################################################################################
        mainPanel(
           print("Please import Beers.CSV and Breweries.CSV to see plots."),
           plotOutput("distPlot"),
           print("\nPlease select a state to view."),
           plotOutput("scatterPlot")
        )
        
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #Assigning files to variables----
    ####################################################################################
    Beers = reactive({
        inFile1 = input$beerData
        if (is.null(inFile1)) {return(NULL)}
        
        return(read.csv(inFile1$datapath))
        })
    
    Breweries = reactive({
        inFile2 = input$breweryData
        if (is.null(inFile2)) {return(NULL)}
        read.csv(inFile2$datapath)
        })
    
    
    #Clean and Join the data----
    ####################################################################################
    # Fix duplicate Breweries, Just assign all Beers to one of the duplicates
    #Breweries %>% filter(Breweries$Name=="Summit Brewing Company")
    cleanJoin = reactive({
    Beers = Beers()
    Breweries = Breweries()
    Beers[Beers$Brewery_id == 139,]$Brewery_id <- 59
    
    #Breweries %>% filter(Breweries$Name=="Sly Fox Brewing Company")
    Beers[Beers$Brewery_id == 164,]$Brewery_id <- 372
    
    # Remove duplicate brewery
    Breweries <- Breweries[!(Breweries$Brew_ID == 139 | Breweries$Brew_ID == 164),]
    
    BeerAndBrewery <- Breweries %>% inner_join(Beers, by = c("Brew_ID" = "Brewery_id"))
    
    ABVByStyle <- Beers %>% 
        dplyr::group_by(Style) %>% 
        dplyr::summarize(ABVStyle = mean(ABV, na.rm = TRUE)) %>% 
        select(Style, ABVStyle) 
    
    ### Compute Mean IBU by Style
    IBUByStyle <- Beers %>% 
        dplyr::group_by(Style) %>% 
        dplyr::summarize(IBUStyle = as.integer(round(mean(IBU, na.rm = TRUE), 0))) %>% 
        select(Style, IBUStyle) 
    
    BeerAndBreweryImproved <- BeerAndBrewery %>% 
        inner_join(ABVByStyle, by = "Style") %>% 
        inner_join(IBUByStyle, by = "Style") %>% 
        mutate(ABV = coalesce(ABV, ABVStyle)) %>%
        mutate(IBU = coalesce(IBU, IBUStyle)) %>%
        mutate(State = str_trim(State, side = c("both")))
    
    })
    
    #State Selection----
    ####################################################################################
    selectedState <- reactive({
        beerData = cleanJoin()
        if(input$stateInput == "All States"){
            return(beerData)
        }
        
        else if(input$stateInput != "All States"){
            beerData = filter(beerData,State == input$stateInput)
            return(beerData)
            
        }
       
    })
    
    
    
    #Distribution of Data----
    ####################################################################################
    output$distPlot <- renderPlot({
        beerData = cleanJoin()
        
        
        if(input$histBox == 3 & input$beerchar == 1){
            ggplot(data = beerData, aes(beerData$IBU)) +
                geom_histogram()+
                xlab("IBU") + 
                ggtitle("IBU Distribution")      
        } else if(input$histBox == 3 & input$beerchar == 2){
            ggplot(data = beerData, aes(beerData$ABV)) +
                geom_histogram()+
                xlab("ABV") + 
                ggtitle("ABV Distribution")      
        } else if(input$histBox == 4 & input$beerchar == 1){
            ggplot(data = beerData, mapping = aes(x = State, y = IBU, fill = State)) +
                geom_boxplot() +
                xlab("State") + ylab("IBU Rating") + 
                ggtitle("IBU Distribution by State")     
        }  else if(input$histBox == 4 & input$beerchar == 2){
            ggplot(data = beerData, mapping = aes(x = State, y = ABV, fill = State)) +
                geom_boxplot() +
                xlab("State") + ylab("ABV Rating") + 
                ggtitle("ABV Distribution by State")  
        } else if(input$histBox == 3 & input$beerchar == 7){
            a = ggplot(data = beerData, aes(beerData$IBU)) +
                geom_histogram()+
                xlab("IBU") + 
                ggtitle("IBU Distribution")
            
            b = ggplot(data = beerData, aes(beerData$ABV)) +
                geom_histogram()+
                xlab("ABV") + 
                ggtitle("ABV Distribution")  
            grid.arrange(a,b,ncol = 2)
        } else if(input$histBox == 4 & input$beerchar == 7){
            a=ggplot(data = beerData, mapping = aes(x = State, y = IBU, fill = State)) +
                geom_boxplot() +
                xlab("State") + ylab("IBU Rating") + 
                ggtitle("IBU Distribution by State")
            
            b=ggplot(data = beerData, mapping = aes(x = State, y = ABV, fill = State)) +
                geom_boxplot() +
                xlab("State") + ylab("ABV Rating") + 
                ggtitle("ABV Distribution by State")  
            grid.arrange(a,b,ncol = 2)
        }
    })
    
    
    #Scatterplot----
    ####################################################################################
    output$scatterPlot <- renderPlot({
        beerData = selectedState()
        if(input$regressionLine == 5){
            ggplot(data = beerData, mapping = aes(x = ABV * 100, y = IBU)) +
                geom_point(position = "dodge") + geom_smooth(method = "lm") +
                xlab("Percent ABV") + ylab("IBU Rating") + 
                scale_x_continuous(breaks = c(2,3,4,5,6,7,8,9,10), limits = c(2.5,10)) +
                labs(caption = "ABV values of over 10 percent were left off due to sparseness of the data")+
                ggtitle("ABV and IBU Scatterplot")
        
           
        }
        else{
            ggplot(data = beerData, mapping = aes(x = ABV * 100, y = IBU)) +
                geom_point(position = "dodge") +
                xlab("Percent ABV") + ylab("IBU Rating") + 
                scale_x_continuous(breaks = c(2,3,4,5,6,7,8,9,10), limits = c(2.5,10)) +
                labs(caption = "ABV values of over 10 percent were left off due to sparseness of the data")+
                ggtitle("ABV and IBU Scatterplot")
        }
            #Scatter Plot of Data
      
    })
    

        # You can access the value of the widget with input$file, e.g.
        # You can access the value of the widget with input$select, e.g.
        # You can access the values of the widget (as a vector)
        # with input$radio, e.g.
        output$value <- renderPrint({str(input$file)})
            output$value <- renderPrint({input$select })
            output$value <- renderPrint({input$radio})

}

# Run the application 
shinyApp(ui = ui, server = server)
