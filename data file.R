# -------------------------------
# Load required packages
# -------------------------------
library(tidyverse)

# -------------------------------
#  Read the dataset
# -------------------------------
data <- read.csv("API_JPN_DS2_en_csv_v2_9334.csv", skip = 4)


# -------------------------------
#  Define required indicators
# -------------------------------
indicators <- c(
  "Population, total",                             
  "Life expectancy at birth, total (years)",        
  "Fertility rate, total (births per woman)",       
  "Infant mortality rate (per 1,000 live births)",  
  "Hospital beds (per 1,000 people)",             
  "Current health expenditure (% of GDP)" , 
  "Mortality rate, infant (per 1,000 live births)" ,
  "Mortality rate, infant, female (per 1,000 live births)",
  "Mortality rate, infant, male (per 1,000 live births)",
  "Population, male",
  "Population, female",
  "Population growth (annual %)",
   "Cause of death, by communicable diseases and maternal, prenatal and nutrition conditions (% of total)",
   "Prevalence of undernourishment (% of population)",
  "Sex ratio at birth (male births per female births)",
  "Immunization, DPT (% of children ages 12-23 months)",
  "Probability of dying among adolescents ages 10-14 years (per 1,000)"
  
)

# -------------------------------
#  Filter for Japan and selected indicators
# -------------------------------
japan_data <- data %>%
  filter(
    Country.Name == "Japan",
    Indicator.Name %in% indicators
  )

# -------------------------------
# Select only relevant columns (1990–2024)
# -------------------------------
japan_selected <- japan_data %>%
  select(Country.Name, Indicator.Name, `X1990`:`X2024`)

# -------------------------------
#  Convert to long format for analysis
# -------------------------------
japan_long <- japan_selected %>%
  pivot_longer(
    cols = `X1990`:`X2024`,
    names_to = "Year",
    values_to = "Value"
  )

# -------------------------------
#  Spread to wide format per year for comparison
# -------------------------------
japan_wide <- japan_long %>%
  pivot_wider(
    names_from = Indicator.Name,
    values_from = Value
  )

# -------------------------------
#  View the result
# -------------------------------
head(japan_wide)
view(japan_wide)


# -------------------------------
#  Convert Year to numeric
# -------------------------------
japan_wide$Year <- as.numeric(gsub("X", "", japan_wide$Year))

# -------------------------------
#  Basic summary statistics
# -------------------------------
summary(japan_wide)

# Correlation matrix between numeric indicators
japan_cor <- japan_wide %>%
  select(-Country.Name, -Year) %>%
  cor(use = "pairwise.complete.obs")

print(japan_cor)

library(reshape2)
library(ggplot2)

# Convert matrix to long format for ggplot
japan_cor_long <- melt(japan_cor)

# Plot
ggplot(japan_cor_long, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1),
                       name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(title = "Correlation Heatmap of Japan Health & Demographic Indicators")

#population trend
ggplot(japan_wide, aes(x = Year, y = `Population, total`)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Population Growth in Japan (1990–2024)",
       y = "Population", x = "Year") +
  theme_minimal()

# life expectancy over time
ggplot(japan_wide, aes(x = Year, y = `Life expectancy at birth, total (years)`)) +
  geom_line(color = "darkgreen", size = 1) +
  labs(title = "Life Expectancy in Japan (1990–2024)",
       y = "Years", x = "Year") +
  theme_minimal()

#health expenditure vs life expectancy
ggplot(japan_wide, aes(x = `Current health expenditure (% of GDP)`,
                       y = `Life expectancy at birth, total (years)`)) +
  geom_point(color = "purple") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Health Expenditure vs Life Expectancy",
       x = "Health Expenditure (% of GDP)", y = "Life Expectancy (years)") +
  theme_minimal()

#population growth and life expectancy
ggplot(japan_wide, aes(x = `Population, total`, y = `Life expectancy at birth, total (years)`)) +
  geom_point(color = "darkblue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Population vs. Life Expectancy in Japan (1990–2024)",
       x = "Population (Total)",
       y = "Life Expectancy (Years)") +
  theme_minimal()

#male and female population trend(compare  gender population over time)
ggplot(japan_wide) +
  geom_line(aes(x = Year, y = `Population, male`, color = "Male"), size = 1) +
  geom_line(aes(x = Year, y = `Population, female`, color = "Female"), size = 1) +
  labs(title = "Male vs Female Population in Japan (1990–2024)",
       y = "Population", x = "Year", color = "Gender") +
  theme_minimal()

# compare infant mortallity trends by gender

ggplot(japan_wide) +
  geom_line(aes(x = Year, y = `Mortality rate, infant (per 1,000 live births)`, color = "Total"), size = 1) +
  geom_line(aes(x = Year, y = `Mortality rate, infant, male (per 1,000 live births)`, color = "Male"), size = 1) +
  geom_line(aes(x = Year, y = `Mortality rate, infant, female (per 1,000 live births)`, color = "Female"), size = 1) +
  labs(title = "Infant Mortality Rates in Japan (1990–2024)",
       y = "Deaths per 1,000 live births", x = "Year", color = "Category") +
  theme_minimal()

# hospital beds vs life expectancy (test if better health care access links to longer lives)
ggplot(japan_wide, aes(x = `Hospital beds (per 1,000 people)`,
                       y = `Life expectancy at birth, total (years)`)) +
  geom_point(color = "darkgreen", size = 2) +
  geom_smooth(method = "lm", color = "orange", se = FALSE) +
  labs(title = "Hospital Beds vs. Life Expectancy in Japan",
       x = "Hospital Beds (per 1,000 people)", y = "Life Expectancy (Years)") +
  theme_minimal()

# health expenditure over time
ggplot(japan_wide, aes(x = Year, y = `Current health expenditure (% of GDP)`)) +
  geom_line(color = "purple", size = 1) +
  labs(title = "Health Expenditure (% of GDP) in Japan (1990–2024)",
       y = "Health Expenditure (% of GDP)", x = "Year") +
  theme_minimal()

# population growth rate over time

ggplot(japan_wide, aes(x = Year, y = `Population growth (annual %)`)) +
  geom_line(color = "steelblue", size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Population Growth Rate in Japan (1990–2024)",
       y = "Annual Growth (%)", x = "Year") +
  theme_minimal()

# Gender population ratio over time
# Gender population ratio (Male-to-Female)
japan_wide <- japan_wide %>%
  mutate(Gender_Ratio = (`Population, male` / `Population, female`) * 100)

ggplot(japan_wide, aes(x = Year, y = Gender_Ratio)) +
  geom_line(color = "darkred", size = 1) +
  labs(title = "Gender Ratio (Male per 100 Female) in Japan (1990–2024)",
       y = "Males per 100 Females", x = "Year") +
  theme_minimal()

#fertility rate and infant mortality
ggplot(japan_wide, aes(x = `Fertility rate, total (births per woman)`,
                       y = `Mortality rate, infant (per 1,000 live births)`)) +
  geom_point(color = "darkorange", size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Fertility Rate vs Infant Mortality in Japan",
       x = "Fertility Rate (births per woman)",
       y = "Infant Mortality (per 1,000)") +
  theme_minimal()

#health infrastructure and undernourishment
ggplot(japan_wide, aes(x = `Hospital beds (per 1,000 people)`,
                       y = `Prevalence of undernourishment (% of population)`)) +
  geom_point(color = "darkblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Hospital Beds vs Undernourishment in Japan",
       x = "Hospital Beds (per 1,000 people)",
       y = "Undernourishment (% of population)") +
  theme_minimal()

#Immunization
ggplot(japan_wide, aes(x = Year, y = `Immunization, DPT (% of children ages 12-23 months)`)) +
  geom_line(color = "seagreen", size = 1) +
  labs(title = "Immunization (DPT) Rate in Japan (1990–2024)",
       y = "Immunization (% of children)", x = "Year") +
  theme_minimal()
#communicable diseases mortality trend
ggplot(japan_wide, aes(x = Year,
                       y = `Cause of death, by communicable diseases and maternal, prenatal and nutrition conditions (% of total)`)) +
  geom_line(color = "firebrick", size = 1) +
  labs(title = "Communicable Disease-related Deaths in Japan (1990–2024)",
       y = "% of Total Deaths", x = "Year") +
  theme_minimal()
# infant mortality by gender
japan_infant_long <- japan_wide %>%
  select(Year, 
         Female = `Mortality rate, infant, female (per 1,000 live births)`,
         Male = `Mortality rate, infant, male (per 1,000 live births)`) %>%
  pivot_longer(-Year, names_to = "Gender", values_to = "Mortality")

ggplot(japan_infant_long, aes(x = Year, y = Mortality, fill = Gender)) +
  geom_area(alpha = 0.7) +
  labs(title = "Infant Mortality by Gender (1990–2024)",
       y = "Deaths per 1,000", x = "Year") +
  theme_minimal()



library(patchwork)

p1 <- ggplot(japan_wide, aes(Year, `Population, total`)) + geom_line(color="blue") + labs(title="Population")
p2 <- ggplot(japan_wide, aes(Year, `Life expectancy at birth, total (years)`)) + geom_line(color="green") + labs(title="Life Expectancy")
p3 <- ggplot(japan_wide, aes(Year, `Current health expenditure (% of GDP)`)) + geom_line(color="purple") + labs(title="Health Expenditure")
p4 <- ggplot(japan_wide, aes(Year, `Fertility rate, total (births per woman)`)) + geom_line(color="red") + labs(title="Fertility Rate")

(p1 | p2) / (p3 | p4) + plot_annotation(title = "Japan Health & Demographic Overview (1990–2024)")

#shiny

library(shiny)
library(plotly)
library(bslib)
library(tidyverse)
library(patchwork)
library(reshape2)
library(corrplot)
library(shinydashboard)





# ------------------------------------------------------------
# UI Layout with multiple tabs
# ------------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "Japan Health"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("chart-line")),
      menuItem("Demographics", tabName = "demographics", icon = icon("users")),
      menuItem("Health Indicators", tabName = "health", icon = icon("heartbeat")),
      menuItem("Mortality", tabName = "mortality", icon = icon("baby")),
      menuItem("Correlation", tabName = "correlation", icon = icon("th"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # ---------------- Overview ----------------
      tabItem(tabName = "overview",
              fluidRow(
                valueBox(format(tail(na.omit(japan_wide$`Population, total`), 1), big.mark = ","), 
                         subtitle = "Total Population (2024)", icon = icon("users"), color = "blue"),
                valueBox(round(tail(na.omit(japan_wide$`Life expectancy at birth, total (years)`), 1), 1),
                         subtitle = "Life Expectancy (Years,Latest available)", icon = icon("heartbeat"), color = "green"),
                valueBox(round(tail(na.omit(japan_wide$`Current health expenditure (% of GDP)`), 1), 1),
                         subtitle = "Health Expenditure (% of GDP,Latest Available)", icon = icon("dollar-sign"), color = "purple")
              ),
              
              fluidRow(
                box(width = 6, plotlyOutput("popTrend", height = "300px")),
                box(width = 6, plotlyOutput("lifeTrend", height = "300px"))
              ),
              
              fluidRow(
                box(width = 6, plotlyOutput("fertilityTrend", height = "300px")),
                box(width = 6, plotlyOutput("expenditureTrend", height = "300px"))
              )
      ),
      
      # ---------------- Demographics ----------------
      tabItem(tabName = "demographics",
              fluidRow(
                box(width = 8, plotlyOutput("demoPlot", height = "350px")),
                box(width = 4,
                    selectInput("demo_indicator", "Select Indicator:",
                                choices = c("Population, total",
                                            "Population, male",
                                            "Population, female",
                                            "Population growth (annual %)"),
                                selected = "Population, total"),
                    sliderInput("year_gender", "Select Year for Gender Chart:",
                                min = min(japan_wide$Year),
                                max = max(japan_wide$Year),
                                value = max(japan_wide$Year),
                                step = 1, sep = ""),
                    plotlyOutput("genderPie", height = "250px")
                )
              )
      ),
      
      # ---------------- Health Indicators ----------------
      tabItem(tabName = "health",
              fluidRow(
                box(width = 6, plotlyOutput("hospitalPlot", height = "300px")),
                box(width = 6, plotlyOutput("immunizationPlot", height = "300px"))
              ),
              fluidRow(
                box(width = 6, plotlyOutput("undernourishmentPlot", height = "300px")),
                box(width = 6, plotlyOutput("diseasePlot", height = "300px"))
              )
      ),
      
      # ---------------- Mortality ----------------
      tabItem(tabName = "mortality",
              fluidRow(
                box(width = 6, plotlyOutput("mortalityPlot", height = "350px")),
                box(width = 6, plotlyOutput("adolescentPlot", height = "350px"))
              )
      ),
      
      
      # ---------------- Correlation ----------------
      tabItem(tabName = "correlation",
              fluidRow(
                box(width = 6, plotOutput("corrHeatmap", height = "1000px")),
                
              )
      )
    )
  )
)

# ------------------------------------------------------------
# SERVER
# ------------------------------------------------------------
server <- function(input, output) {
  
  # --- Overview plots
  output$popTrend <- renderPlotly({
    plot_ly(japan_wide, x = ~Year, y = ~`Population, total`, type = 'scatter', mode = 'lines', name = 'Population',
            line = list(color = 'blue')) %>%
      layout(title = "Population Trend (1990–2024)")
  })
  
  output$lifeTrend <- renderPlotly({
    plot_ly(japan_wide, x = ~Year, y = ~`Life expectancy at birth, total (years)`, type = 'scatter', mode = 'lines', 
            line = list(color = 'green')) %>%
      layout(title = "Life Expectancy Over Time")
  })
  
  output$fertilityTrend <- renderPlotly({
    plot_ly(japan_wide, x = ~Year, y = ~`Fertility rate, total (births per woman)`, type = 'scatter', mode = 'lines', 
            line = list(color = 'red')) %>%
      layout(title = "Fertility Rate in Japan (1990–2024)")
  })
  
  output$expenditureTrend <- renderPlotly({
    plot_ly(japan_wide, x = ~Year, y = ~`Current health expenditure (% of GDP)`, type = 'scatter', mode = 'lines', 
            line = list(color = 'purple')) %>%
      layout(title = "Health Expenditure (% of GDP)")
  })
  
  # --- Demographics
  output$demoPlot <- renderPlotly({
    plot_ly(japan_wide, x = ~Year,
            y = as.formula(paste0("~`", input$demo_indicator, "`")),
            type = 'scatter', mode = 'lines+markers',
            line = list(color = 'blue')) %>%
      layout(title = paste(input$demo_indicator, "in Japan (1990–2024)"))
  })
  
  output$genderPie <- renderPlotly({
    df <- japan_wide %>% filter(Year == input$year_gender)
    gender_data <- data.frame(
      Gender = c("Male", "Female"),
      Population = c(df$`Population, male`, df$`Population, female`)
    )
    
    plot_ly(gender_data,
            labels = ~Gender, values = ~Population, type = 'pie',
            textinfo = 'label+percent', insidetextorientation = 'radial') %>%
      layout(title = paste("Gender Composition -", input$year_gender))
  })
  
  # --- Health
  output$hospitalPlot <- renderPlotly({
    plot_ly(japan_wide, x = ~Year, y = ~`Hospital beds (per 1,000 people)`,
            type = 'scatter', mode = 'lines', line = list(color = 'orange')) %>%
      layout(title = "Hospital Beds per 1,000 People")
  })
  
  output$immunizationPlot <- renderPlotly({
    plot_ly(japan_wide, x = ~Year, y = ~`Immunization, DPT (% of children ages 12-23 months)`,
            type = 'scatter', mode = 'lines', line = list(color = 'seagreen')) %>%
      layout(title = "Immunization (DPT) Rate (1990–2024)")
  })
  
  output$undernourishmentPlot <- renderPlotly({
    df <- japan_wide %>%
      filter(!is.na(`Hospital beds (per 1,000 people)`),
             !is.na(`Prevalence of undernourishment (% of population)`))
    
    plot_ly(df, x = ~`Hospital beds (per 1,000 people)`,
            y = ~`Prevalence of undernourishment (% of population)`,
            type = 'scatter', mode = 'markers+lines', color = I("darkblue")) %>%
      layout(title = "Hospital Beds vs Undernourishment",
             xaxis = list(title = "Hospital Beds (per 1,000 people)"),
             yaxis = list(title = "Undernourishment (% of population)"))
  })
  
  
  output$diseasePlot <- renderPlotly({
    df <- japan_wide %>%
      filter(!is.na(`Prevalence of undernourishment (% of population)`))
    
    plot_ly(df, x = ~Year,
            y = ~`Prevalence of undernourishment (% of population)`,
            type = 'scatter', mode = 'lines+markers',
            line = list(color = 'firebrick')) %>%
      layout(title = "Undernourishment in Japan (2001–Latest Available)",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Undernourishment (% of population)"))
  })
  
  # --- Mortality
  output$mortalityPlot <- renderPlotly({
    plot_ly(japan_wide, x = ~Year, y = ~`Mortality rate, infant (per 1,000 live births)`, name = "Total", type = 'scatter', mode = 'lines', line = list(color = 'black')) %>%
      add_trace(y = ~`Mortality rate, infant, male (per 1,000 live births)`, name = "Male", line = list(color = 'blue')) %>%
      add_trace(y = ~`Mortality rate, infant, female (per 1,000 live births)`, name = "Female", line = list(color = 'red')) %>%
      layout(title = "Infant Mortality Trends (1990–2024)",
             yaxis = list(title = "Deaths per 1,000 live births"))
  })
  output$adolescentPlot <- renderPlotly({
    df <- japan_wide %>%
      filter(!is.na(`Probability of dying among adolescents ages 10-14 years (per 1,000)`))
    
    plot_ly(df, x = ~Year,
            y = ~`Probability of dying among adolescents ages 10-14 years (per 1,000)`,
            type = 'scatter', mode = 'lines+markers',
            line = list(color = 'darkgreen')) %>%
      layout(title = "Adolescent Mortality (Ages 10–14, per 1,000)",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Deaths per 1,000"),
             hovermode = "x unified")
  })
  
  # --- Correlation
  output$corrHeatmap <- renderPlot({
    japan_cor <- japan_wide %>%
      select(-Country.Name, -Year) %>%
      cor(use = "pairwise.complete.obs")
    japan_cor_long <- melt(japan_cor)
    
    ggplot(japan_cor_long, aes(x = Var1, y = Var2, fill = value)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                           midpoint = 0, limit = c(-1,1), name = "Correlation") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            axis.title = element_blank()) +
      labs(title = "Correlation Heatmap of Indicators")
  })
  
  
}

# ------------------------------------------------------------
# Run App
# ------------------------------------------------------------
shinyApp(ui, server)