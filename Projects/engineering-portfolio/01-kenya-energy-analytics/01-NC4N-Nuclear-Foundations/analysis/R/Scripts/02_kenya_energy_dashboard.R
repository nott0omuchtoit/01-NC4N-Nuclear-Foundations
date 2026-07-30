# ============================================
# app.R - Kenya Energy Analytics Dashboard
# NC4N Nuclear Foundations
# ============================================

# 1. Load Libraries ---------------------------
library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(scales)
library(ggthemes)
library(DT)  # For interactive tables
library(markdown)  # For including markdown files

# 2. Load Data -------------------------------
df <- read_csv("C:/Users/user/Desktop/Programming/Projects/engineering-portfolio/01-kenya-energy-analytics/01-NC4N-Nuclear-Foundations/data/raw/electricity-prod-source-stacked.csv")

# Clean and transform (reuse your existing code)
df_clean <- df %>%
  rename(
    country = Entity,
    iso_code = Code,
    year = Year,
    other_renewables = `Other renewables`,
    bioenergy = Bioenergy,
    solar = Solar,
    wind = Wind,
    hydro = Hydropower,
    nuclear = Nuclear,
    oil = Oil,
    gas = Gas,
    coal = Coal
  ) %>%
  mutate(
    across(c(other_renewables, bioenergy, solar, wind, hydro, 
             nuclear, oil, gas, coal),
           ~ . * 1000)
  ) %>%
  mutate(
    total_generation = rowSums(across(c(coal, gas, oil, nuclear, hydro, 
                                        wind, solar, bioenergy, other_renewables)), 
                               na.rm = TRUE),
    renewables = rowSums(across(c(hydro, wind, solar, bioenergy, other_renewables)), 
                         na.rm = TRUE),
    fossil = rowSums(across(c(coal, gas, oil)), na.rm = TRUE)
  ) %>%
  mutate(
    nuclear_share = (nuclear / total_generation) * 100,
    renewables_share = (renewables / total_generation) * 100,
    fossil_share = (fossil / total_generation) * 100
  )

# Filter Kenya
kenya <- df_clean %>% filter(country == "Kenya")
current_year <- max(kenya$year)

# Calculate current mix with proper leading source identification
current_mix <- kenya %>%
  filter(year == current_year) %>%
  pivot_longer(
    cols = c(coal, gas, oil, nuclear, hydro, wind, solar, bioenergy, other_renewables),
    names_to = "source",
    values_to = "generation_gwh"
  ) %>%
  filter(generation_gwh > 0) %>%
  mutate(
    share = generation_gwh / sum(generation_gwh) * 100,
    source = case_when(
      source == "hydro" ~ "Hydro",
      source == "wind" ~ "Wind",
      source == "solar" ~ "Solar",
      source == "bioenergy" ~ "Bioenergy",
      source == "other_renewables" ~ "Other Renewables",
      source == "coal" ~ "Coal",
      source == "gas" ~ "Gas",
      source == "oil" ~ "Oil",
      source == "nuclear" ~ "Nuclear",
      TRUE ~ source
    )
  )

# Dynamic leading source
leading_idx <- which.max(current_mix$share)
leading_source <- current_mix$source[leading_idx]
leading_share <- round(current_mix$share[leading_idx], 1)


# 3. UI --------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "Kenya Energy Analytics"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Generation Trends", tabName = "trends", icon = icon("chart-line")),
      menuItem("Energy Mix", tabName = "mix", icon = icon("pie-chart")),
      menuItem("Nuclear Comparison", tabName = "nuclear", icon = icon("atom")),
      menuItem("Scenario Analysis", tabName = "scenarios", icon = icon("microscope")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f5f6fa;
        }
        .box {
          border-radius: 8px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        .value-box {
          border-radius: 8px;
        }
      "))
    ),
    
    tabItems(
      # Overview Tab
      tabItem(
        tabName = "overview",
        fluidRow(
          valueBoxOutput("total_generation"),
          valueBoxOutput("leading_source_box"),
          valueBoxOutput("renewable_share")
        ),
        fluidRow(
          box(
            title = "Kenya's Electricity Mix Overview",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("overview_plot")
          )
        ),
        fluidRow(
          box(
            title = "Key Insights",
            status = "info",
            width = 12,
            htmlOutput("key_insights")
          )
        )
      ),
      
      # Trends Tab
      tabItem(
        tabName = "trends",
        fluidRow(
          box(
            title = "Generation Trends (1985-2025)",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("trend_plot")
          )
        ),
        fluidRow(
          box(
            title = "Growth Rate",
            status = "info",
            width = 12,
            plotlyOutput("growth_plot")
          )
        )
      ),
      
      # Energy Mix Tab
      tabItem(
        tabName = "mix",
        fluidRow(
          box(
            title = paste("Current Mix -", current_year),
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("mix_bar")
          ),
          box(
            title = "Share Breakdown",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("mix_pie")
          )
        ),
        fluidRow(
          box(
            title = "Evolution of Energy Mix",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("mix_evolution")
          )
        ),
        fluidRow(
          box(
            title = "Detailed Data",
            status = "info",
            width = 12,
            DTOutput("mix_table")
          )
        )
      ),
      
      # Nuclear Comparison Tab
      tabItem(
        tabName = "nuclear",
        fluidRow(
          box(
            title = "Kenya vs Nuclear-Operating Countries",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("comparison_plot")
          )
        ),
        fluidRow(
          box(
            title = "Nuclear Share Comparison",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("nuclear_share_plot")
          )
        )
      ),
      
      # Scenarios Tab
      tabItem(
        tabName = "scenarios",
        fluidRow(
          box(
            title = "Nuclear Scenario Impact",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("scenario_plot")
          )
        ),
        fluidRow(
          box(
            title = "Scenario Controls",
            status = "info",
            width = 4,
            sliderInput("nuclear_capacity", 
                        "Nuclear Capacity (MW):",
                        min = 0, max = 3000, value = 1000, step = 100),
            numericInput("capacity_factor", 
                         "Capacity Factor (%):",
                         min = 50, max = 95, value = 90, step = 1)
          ),
          box(
            title = "Scenario Results",
            status = "success",
            width = 8,
            htmlOutput("scenario_results")
          )
        )
      ),
      
      # About Tab
      tabItem(
        tabName = "about",
        fluidRow(
          box(
            title = "About This Dashboard",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            includeMarkdown("about.Rmd")
          )
        )
      )
    )
  )
)


# 4. Server ----------------------------------
server <- function(input, output, session) {
  
  # Overview Value Boxes
  output$total_generation <- renderValueBox({
    total <- kenya %>%
      filter(year == current_year) %>%
      pull(total_generation) %>%
      round(0)
    
    valueBox(
      value = paste0(format(total, big.mark = ","), " GWh"),
      subtitle = paste("Total Generation (", current_year, ")", sep = ""),
      icon = icon("bolt"),
      color = "blue"
    )
  })
  
  output$leading_source_box <- renderValueBox({
    valueBox(
      value = paste0(leading_source, " (", leading_share, "%)"),
      subtitle = "Leading Energy Source",
      icon = icon("chart-pie"),
      color = "green"
    )
  })
  
  output$renewable_share <- renderValueBox({
    renew_share <- kenya %>%
      filter(year == current_year) %>%
      pull(renewables_share) %>%
      round(1)
    
    valueBox(
      value = paste0(renew_share, "%"),
      subtitle = "Renewables Share",
      icon = icon("leaf"),
      color = "yellow"
    )
  })
  
  # Overview Plot
  output$overview_plot <- renderPlotly({
    p <- current_mix %>%
      plot_ly(
        x = ~reorder(source, -share),
        y = ~share,
        type = "bar",
        text = ~paste(source, "<br>", round(share, 1), "%"),
        textposition = "outside",
        marker = list(color = c("#3498db", "#2ecc71", "#f39c12", "#e74c3c", 
                                "#9b59b6", "#1abc9c", "#e67e22", "#34495e"))
      ) %>%
      layout(
        title = paste("Kenya Electricity Mix -", current_year),
        xaxis = list(title = "Source"),
        yaxis = list(title = "Share (%)", range = c(0, 55)),
        hovermode = "closest"
      )
    p
  })
  
  # Key Insights (using your fixed leading source)
  output$key_insights <- renderUI({
    HTML(paste0("
      <ul>
        <li><strong>", leading_source, "</strong> is the leading source at <strong>", leading_share, "%</strong> of generation</li>
        <li>Total generation reached <strong>", 
                format(round(kenya %>% filter(year == current_year) %>% pull(total_generation), 0), 
                       big.mark = ","), " GWh</strong> in ", current_year, "</li>
        <li>Renewables account for <strong>", 
                round(kenya %>% filter(year == current_year) %>% pull(renewables_share), 1), "%</strong> of the mix</li>
        <li>Fossil fuels make up <strong>", 
                round(kenya %>% filter(year == current_year) %>% pull(fossil_share), 1), "%</strong></li>
        <li>Nuclear currently contributes <strong>0%</strong> to Kenya's grid</li>
      </ul>
    "))
  })
  
  # Trends Plot
  output$trend_plot <- renderPlotly({
    p <- kenya %>%
      plot_ly(
        x = ~year,
        y = ~total_generation,
        type = "scatter",
        mode = "lines+markers",
        line = list(color = "#2c3e50", width = 2),
        marker = list(color = "#3498db", size = 8),
        text = ~paste("Year:", year, "<br>Generation:", 
                      format(round(total_generation, 0), big.mark = ","), "GWh"),
        hoverinfo = "text"
      ) %>%
      layout(
        title = "Kenya's Total Electricity Generation (1985-2025)",
        xaxis = list(title = "Year"),
        yaxis = list(title = "Generation (GWh)")
      )
    p
  })
  
  # Growth Rate Plot
  output$growth_plot <- renderPlotly({
    growth_data <- kenya %>%
      arrange(year) %>%
      mutate(growth_rate = (total_generation - lag(total_generation)) / 
               lag(total_generation) * 100) %>%
      filter(!is.na(growth_rate))
    
    p <- growth_data %>%
      plot_ly(
        x = ~year,
        y = ~growth_rate,
        type = "bar",
        text = ~paste(round(growth_rate, 1), "%"),
        textposition = "outside",
        marker = list(color = ~ifelse(growth_rate >= 0, "#27ae60", "#e74c3c"))
      ) %>%
      layout(
        title = "Annual Growth Rate of Kenya's Electricity Generation",
        xaxis = list(title = "Year"),
        yaxis = list(title = "Growth Rate (%)")
      )
    p
  })
  
  # Mix Bar Plot
  output$mix_bar <- renderPlotly({
    p <- current_mix %>%
      plot_ly(
        x = ~reorder(source, share),
        y = ~generation_gwh,
        type = "bar",
        text = ~paste0(round(share, 1), "%"),
        textposition = "outside",
        marker = list(color = c("#3498db", "#2ecc71", "#f39c12", "#e74c3c", 
                                "#9b59b6", "#1abc9c", "#e67e22", "#34495e")),
        name = ~source
      ) %>%
      layout(
        title = paste("Generation by Source -", current_year),
        xaxis = list(title = "Source"),
        yaxis = list(title = "Generation (GWh)")
      )
    p
  })
  
  # Mix Pie Chart
  output$mix_pie <- renderPlotly({
    p <- current_mix %>%
      plot_ly(
        labels = ~source,
        values = ~share,
        type = "pie",
        textinfo = "label+percent",
        insidetextorientation = "radial",
        hoverinfo = "text",
        text = ~paste(source, "<br>", round(share, 1), "%<br>", 
                      format(round(generation_gwh, 0), big.mark = ","), " GWh")
      ) %>%
      layout(
        title = "Share Breakdown"
      )
    p
  })
  
  # Mix Evolution
  output$mix_evolution <- renderPlotly({
    evolution_data <- kenya %>%
      pivot_longer(
        cols = c(coal, gas, oil, nuclear, hydro, wind, solar, bioenergy, other_renewables),
        names_to = "source",
        values_to = "generation_gwh"
      ) %>%
      filter(generation_gwh > 0) %>%
      mutate(source = case_when(
        source == "hydro" ~ "Hydro",
        source == "wind" ~ "Wind",
        source == "solar" ~ "Solar",
        source == "bioenergy" ~ "Bioenergy",
        source == "other_renewables" ~ "Other Renewables",
        source == "coal" ~ "Coal",
        source == "gas" ~ "Gas",
        source == "oil" ~ "Oil",
        source == "nuclear" ~ "Nuclear",
        TRUE ~ source
      ))
    
    p <- evolution_data %>%
      plot_ly(
        x = ~year,
        y = ~generation_gwh,
        color = ~source,
        type = "scatter",
        mode = "none",
        stackgroup = "one",
        fillcolor = ~source,
        text = ~paste(source, "<br>", round(generation_gwh, 0), "GWh"),
        hoverinfo = "text"
      ) %>%
      layout(
        title = "Evolution of Kenya's Electricity Mix (1985-2025)",
        xaxis = list(title = "Year"),
        yaxis = list(title = "Generation (GWh)"),
        hovermode = "x unified"
      )
    p
  })
  
  # Mix Table
  output$mix_table <- renderDT({
    current_mix %>%
      select(source, generation_gwh, share) %>%
      mutate(
        generation_gwh = round(generation_gwh, 0),
        share = round(share, 1)
      ) %>%
      arrange(desc(share)) %>%
      datatable(
        colnames = c("Source", "Generation (GWh)", "Share (%)"),
        options = list(
          pageLength = 10,
          dom = "Bfrtip"
        ),
        rownames = FALSE
      )
  })
  
  # Comparison Plot
  output$comparison_plot <- renderPlotly({
    nuclear_countries <- c("France", "United States", "China", "Russia", 
                           "South Korea", "India", "Canada", "United Kingdom",
                           "Germany", "South Africa", "Japan", "Ukraine")
    
    comparison_data <- df_clean %>%
      filter(country %in% c("Kenya", nuclear_countries)) %>%
      filter(year == max(year)) %>%
      select(country, nuclear_share, renewables_share, fossil_share)
    
    comparison_long <- comparison_data %>%
      pivot_longer(
        cols = c(nuclear_share, renewables_share, fossil_share),
        names_to = "source_type",
        values_to = "share"
      ) %>%
      mutate(
        source_type = case_when(
          source_type == "nuclear_share" ~ "Nuclear",
          source_type == "renewables_share" ~ "Renewables",
          source_type == "fossil_share" ~ "Fossil"
        )
      )
    
    p <- comparison_long %>%
      plot_ly(
        x = ~country,
        y = ~share,
        color = ~source_type,
        type = "bar",
        text = ~paste0(round(share, 1), "%"),
        textposition = "inside"
      ) %>%
      layout(
        title = "Electricity Mix Comparison",
        xaxis = list(title = "Country"),
        yaxis = list(title = "Share (%)", tickformat = ".0%"),
        barmode = "stack",
        legend = list(orientation = "h", y = -0.2)
      )
    p
  })
  
  # Nuclear Share Plot
  output$nuclear_share_plot <- renderPlotly({
    nuclear_countries <- c("France", "United States", "China", "Russia", 
                           "South Korea", "India", "Canada", "United Kingdom",
                           "Germany", "South Africa", "Japan", "Ukraine")
    
    comparison_data <- df_clean %>%
      filter(country %in% c("Kenya", nuclear_countries)) %>%
      filter(year == max(year)) %>%
      select(country, nuclear_share) %>%
      mutate(
        is_kenya = ifelse(country == "Kenya", "Kenya", "Other"),
        country = reorder(country, nuclear_share)
      )
    
    p <- comparison_data %>%
      plot_ly(
        x = ~nuclear_share,
        y = ~country,
        type = "bar",
        orientation = "h",
        color = ~is_kenya,
        colors = c("Kenya" = "#e74c3c", "Other" = "#3498db"),
        text = ~paste0(round(nuclear_share, 1), "%"),
        textposition = "outside"
      ) %>%
      layout(
        title = "Nuclear Share of Electricity Generation",
        xaxis = list(title = "Nuclear Share (%)"),
        yaxis = list(title = "Country"),
        showlegend = FALSE
      )
    p
  })
  
  # Scenario Plot
  output$scenario_plot <- renderPlotly({
    # Current generation
    current_gen <- kenya %>%
      filter(year == current_year) %>%
      summarise(total = sum(total_generation)) %>%
      pull(total)
    
    # Nuclear parameters from slider
    nuclear_capacity_mw <- input$nuclear_capacity
    capacity_factor <- input$capacity_factor / 100
    hours_per_year <- 8760
    nuclear_gwh <- nuclear_capacity_mw * hours_per_year * capacity_factor / 1000
    
    # Prepare current mix
    current_mix_data <- kenya %>%
      filter(year == current_year) %>%
      pivot_longer(
        cols = c(coal, gas, oil, hydro, wind, solar, bioenergy, other_renewables),
        names_to = "source",
        values_to = "gwh"
      ) %>%
      mutate(
        source = case_when(
          source == "hydro" ~ "Hydro",
          source == "wind" ~ "Wind",
          source == "solar" ~ "Solar",
          source == "bioenergy" ~ "Bioenergy",
          source == "other_renewables" ~ "Other Renewables",
          source == "coal" ~ "Coal",
          source == "gas" ~ "Gas",
          source == "oil" ~ "Oil",
          TRUE ~ source
        ),
        scenario = "Current Mix"
      )
    
    # With nuclear scenario
    nuclear_scenario <- current_mix_data %>%
      bind_rows(
        data.frame(
          source = "Nuclear",
          gwh = nuclear_gwh,
          scenario = "With Nuclear"
        )
      ) %>%
      group_by(scenario) %>%
      mutate(share = gwh / sum(gwh) * 100)
    
    # Replicate for current mix (for grouping)
    current_with_share <- current_mix_data %>%
      group_by(scenario) %>%
      mutate(share = gwh / sum(gwh) * 100)
    
    combined <- bind_rows(current_with_share, nuclear_scenario)
    
    p <- combined %>%
      plot_ly(
        x = ~scenario,
        y = ~gwh,
        color = ~source,
        type = "bar",
        text = ~paste0(round(share, 1), "%"),
        textposition = "inside"
      ) %>%
      layout(
        title = paste("Impact of", nuclear_capacity_mw, "MW Nuclear Plant"),
        xaxis = list(title = "Scenario"),
        yaxis = list(title = "Generation (GWh)"),
        barmode = "stack",
        legend = list(orientation = "h", y = -0.2)
      )
    p
  })
  
  # Scenario Results
  output$scenario_results <- renderUI({
    current_gen <- kenya %>%
      filter(year == current_year) %>%
      summarise(total = sum(total_generation)) %>%
      pull(total)
    
    nuclear_capacity_mw <- input$nuclear_capacity
    capacity_factor <- input$capacity_factor / 100
    hours_per_year <- 8760
    nuclear_gwh <- nuclear_capacity_mw * hours_per_year * capacity_factor / 1000
    
    nuclear_share <- (nuclear_gwh / (current_gen + nuclear_gwh)) * 100
    co2_reduction <- round(nuclear_share * 0.3, 1)  # Rough estimate
    
    HTML(paste0("
      <div style='padding: 15px;'>
        <h4>Scenario Results</h4>
        <table style='width: 100%;'>
          <tr>
            <td><strong>Nuclear Capacity:</strong></td>
            <td>", format(nuclear_capacity_mw, big.mark = ","), " MW</td>
          </tr>
          <tr>
            <td><strong>Capacity Factor:</strong></td>
            <td>", input$capacity_factor, "%</td>
          </tr>
          <tr>
            <td><strong>Annual Generation:</strong></td>
            <td>", format(round(nuclear_gwh, 0), big.mark = ","), " GWh</td>
          </tr>
          <tr>
            <td><strong>Nuclear Share of Mix:</strong></td>
            <td><strong style='color: #e74c3c;'>", round(nuclear_share, 1), "%</strong></td>
          </tr>
          <tr>
            <td><strong>Estimated CO₂ Reduction:</strong></td>
            <td><strong style='color: #27ae60;'>", co2_reduction, "%</strong></td>
          </tr>
        </table>
        <br>
        <div style='background: #f8f9fa; padding: 10px; border-radius: 5px;'>
          <p style='margin: 0;'><strong>💡 Insight:</strong> A ", format(nuclear_capacity_mw, big.mark = ","), 
                " MW nuclear plant at ", input$capacity_factor, "% capacity factor would provide ", 
                round(nuclear_share, 1), "% of Kenya's current electricity needs.</p>
        </div>
      </div>
    "))
  })
}


# 5. Run the App ------------------------------
shinyApp(ui, server)