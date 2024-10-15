# Required Libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggiraph)
library(dplyr)
library(leaflet)
library(readxl)
library(tidyr)
library(DT)
library(dashboardthemes)
library(sf)
library(httr)
library(jsonlite)

# Load Data
data_file <- "DataProject.xlsx"
data_ahh <- read_excel(data_file, sheet = "AHH")
data_pdb <- read_excel(data_file, sheet = "PDB")
data_population <- read_excel(data_file, sheet = "Populasi")
data_participation <- read_excel(data_file, sheet = "Partisipasi")
data_map <- read_excel(data_file, sheet = "LetakProvinsi")
data_ahh <- merge(data_ahh, data_map, by = "Provinsi")
load_geojson <- function(url) {
  req <- GET(url)
  stop_for_status(req)
  content(req, "text") %>% st_read()
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Korelasi Produktif, Kesehatan, dan Pertumbuhan Ekonomi", titleWidth = 600),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Populasi Usia Produktif", tabName = "populasi", icon = icon("users")),
      menuItem("Angka Harapan Hidup", tabName = "ahh", icon = icon("heartbeat")),
      menuItem("Tingkat Kesehatan", tabName = "kesehatan", icon = icon("map")),
      menuItem("Pertumbuhan Ekonomi", tabName = "ekonomi", icon = icon("chart-line")),
      menuItem("Produktivitas vs. Kesehatan", tabName = "produktifitas", icon = icon("balance-scale"))
    ),
    selectInput("selected_prov", "Pilih Provinsi:", choices = c("Semua Provinsi", unique(data_ahh$Provinsi))),
    selectInput("selected_year", "Pilih Tahun:",
                choices = sort(unique(data_ahh$Tahun)),
                selected = max(data_ahh$Tahun)),
    selectInput("selected_usaha", "Pilih Lapangan Usaha:", choices = unique(data_pdb$LapanganUsahaPDB))
  ),
  dashboardBody(
    shinyDashboardThemes(
      theme = "blue_gradient"
    ),
    tabItems(
      tabItem(tabName = "populasi",
              fluidRow(
                box(title = "Populasi Usia Produktif (15-64 Tahun)", status = "primary", solidHeader = TRUE, 
                    width = 12, plotOutput("pop_plot", height = "500px"))
              )
      ),
      tabItem(tabName = "ahh",
              fluidRow(
                box(title = textOutput("dynamicTitleBar"), status = "primary", solidHeader = TRUE, 
                    width = 12, girafeOutput("ahh_bar", height = "500px"))
              )
      ),
      tabItem(tabName = "kesehatan",
              fluidRow(
                box(title = textOutput("dynamicTitleMap"), status = "primary", solidHeader = TRUE, 
                    width = 12, leafletOutput("choromap", height = "500px"))
              ),
      ),
      tabItem(tabName = "ekonomi",
              fluidRow(
                box(title = "Pertumbuhan Ekonomi (PDB)", status = "primary", solidHeader = TRUE, 
                    width = 12, plotOutput("pdb_line", height = "500px"))
              )
      ),
      tabItem(tabName = "produktifitas",
              fluidRow(
                box(title = textOutput("dynamicTitleScatterLk"), status = "primary", solidHeader = TRUE, 
                    width = 12, girafeOutput("scatter_plot_laki", height = "500px")),
              ),
              fluidRow(
                box(title = textOutput("dynamicTitleScatterPr"), status = "primary", solidHeader = TRUE, 
                    width = 12, girafeOutput("scatter_plot_perempuan", height = "500px"))
              )
      )
    )
  )
)

# Server
server <- function(input, output) {
  output$pop_plot <- renderPlot({
    ggplot(data_population, aes(x = Tahun, y = Populasi)) +
      geom_line(color = "turquoise", size = 1.25) +
      geom_point(color = "navy", size = 3) +
      xlab("Tahun") + ylab("Populasi (ribuan)") +
      scale_x_continuous(breaks = seq(min(data_population$Tahun), max(data_population$Tahun), by = 1)) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 20),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)
      )
  })
  
  dynamicTitleBar <- reactive({
    paste("Angka Harapan Hidup Tahun", input$selected_year)
  })
  
  output$ahh_bar <- renderGirafe({
    data_filtered <- data_ahh %>% filter(Tahun == input$selected_year)
    
    if (input$selected_prov != "Semua Provinsi") {
      data_filtered <- data_filtered %>% filter(Provinsi == input$selected_prov)
    }
    
    data_filtered <- data_filtered %>%
      pivot_longer(cols = c(AHH_Laki, AHH_Perempuan), names_to = "Jenis_Kelamin", values_to = "Angka_Harapan_Hidup") %>%
      mutate(Jenis_Kelamin = recode(Jenis_Kelamin, "AHH_Laki" = "Laki-Laki", "AHH_Perempuan" = "Perempuan"))
    
    data_filtered <- data_filtered %>%
      group_by(Provinsi) %>%
      mutate(Avg_Angka_Harapan_Hidup = mean(Angka_Harapan_Hidup, na.rm = TRUE)) %>%
      arrange(Avg_Angka_Harapan_Hidup)
    
    gg <- ggplot(data_filtered, aes(x = reorder(Provinsi, -Avg_Angka_Harapan_Hidup), y = Angka_Harapan_Hidup, fill = Jenis_Kelamin, tooltip = Angka_Harapan_Hidup)) +
      geom_bar_interactive(stat = "identity", position = position_dodge(width = -0.5), alpha = 1) +
      geom_bar_interactive(stat = "identity", position = position_dodge(width = 0.8), alpha = 1, width = 0.8) +
      xlab("Provinsi") + ylab("Angka Harapan Hidup") +
      scale_fill_manual(name = "Keterangan", values = c("Laki-Laki" = "turquoise", "Perempuan" = "navy"), labels = c("Laki-Laki", "Perempuan")) +
      theme_minimal() +
      theme(plot.title = element_text(size = 20),
            axis.title.x = element_text(size = 15),
            axis.title.y = element_text(size = 15),
            axis.text.x = element_text(size = 12, angle = 75, hjust = 1),
            axis.text.y = element_text(size = 12),
            legend.title = element_text(size = 15),
            legend.text = element_text(size = 12)
      ) +
      guides(fill = guide_legend(title = "Keterangan", title.position = "top", title.theme = element_text(size = 15)))
    girafe(ggobj = gg, width_svg = 15, height_svg = 6, options = list(opts_tooltip(use_fill=TRUE), opts_toolbar(saveaspng = FALSE)))
  })
  output$dynamicTitleBar <- renderText({
    dynamicTitleBar()
  })
  
  
  # Load GeoJSON data from GitHub
  geojson_url <- "https://raw.githubusercontent.com/eppofahmi/geojson-indonesia/master/provinsi/all_maps_state_indo.geojson"
  geo_data <- load_geojson(geojson_url)
  
  # Dynamic Title
  dynamicTitleMap <- reactive({
    paste("Tingkat Kesehatan Rata-Rata Tiap Provinsi Tahun", input$selected_year)
  })
  
  # fixed min and max range for AHH_rata
  min_range <- min(data_ahh$AHH_rata, na.rm = TRUE)
  max_range <- max(data_ahh$AHH_rata, na.rm = TRUE)
  
  # Render leaflet map for AHH rata rata
  output$choromap <- renderLeaflet({
    data_map <- data_ahh %>% filter(Tahun == input$selected_year)
    
    geo_data_provinsi <- geo_data %>% 
      left_join(data_map, by = c("name" = "Provinsi"))
    
    pal <- colorBin("RdYlGn", domain = c(min_range,max_range), bins = 5)
    
    leaflet(geo_data_provinsi) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(AHH_rata),
        color = "black",
        weight = 1,
        opacity = 1,
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "gray",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = ~paste(name, ": ", AHH_rata),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addLegend(pal = pal, 
                values = c(min_range,max_range), 
                opacity = 0.7, 
                title = "Angka Harapan Hidup",
                position = "bottomright")
  })
  output$dynamicTitleMap <- renderText({
    dynamicTitleMap()
  })
  
  #pdb_line
  output$pdb_line <- renderPlot({
    data_filtered <- data_pdb %>% filter(LapanganUsahaPDB == input$selected_usaha)
    ggplot(data_filtered, aes(x = Tahun, y = LajuPertumbuhanPDB)) +
      geom_line(color = "turquoise", size = 1.25) +
      geom_point(color = "navy", size = 3) +
      ggtitle(paste(input$selected_usaha)) +
      xlab("Tahun") + ylab("Laju Pertumbuhan PDB (%)") +
      scale_x_continuous(breaks = seq(min(data_filtered$Tahun), max(data_filtered$Tahun), by = 1)) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 20),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)
      )
  })
  
  dynamicTitleScatterLk <- reactive({
    paste("Produktivitas vs. Kesehatan (Laki-Laki) Tahun ", input$selected_year)
  })
  output$scatter_plot_laki <- renderGirafe({
    data_combined <- merge(data_participation, data_ahh, by = c("Provinsi", "Tahun"))
    data_scatter_lk <- data_combined %>% filter(Tahun == input$selected_year)
    gg <- ggplot(data_scatter_lk, aes(x = Partisipasi_Laki, y = AHH_Laki)) +
      geom_point_interactive(aes(tooltip = Provinsi, color = Provinsi), size = 3) +
      xlab("Partisipasi Angkatan Kerja Laki-Laki") + ylab("Angka Harapan Hidup Laki-Laki") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 20),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12)
      )
    girafe(ggobj = gg, width_svg = 15, height_svg = 6, options = list(opts_tooltip(use_fill=TRUE), opts_toolbar(saveaspng = FALSE)))
  })
  output$dynamicTitleScatterLk <- renderText({
    dynamicTitleScatterLk()
  })
  
  
  dynamicTitleScatterPr <- reactive({
    paste("Produktivitas vs. Kesehatan (Perempuan) Tahun ", input$selected_year)
  })
  output$scatter_plot_perempuan <- renderGirafe({
    data_combined <- merge(data_participation, data_ahh, by = c("Provinsi", "Tahun"))
    data_scatter_pr <- data_combined %>% filter(Tahun == input$selected_year)
    gg <- ggplot(data_scatter_pr, aes(x = Partisipasi_Perempuan, y = AHH_Perempuan)) +
      geom_point_interactive(aes(tooltip = Provinsi, color = Provinsi), size = 2.5) +
      xlab("Partisipasi Angkatan Kerja Perempuan") + ylab("Angka Harapan Hidup Perempuan") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 20),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12)
      )
    girafe(ggobj = gg, width_svg = 15, height_svg = 6, options = list(opts_tooltip(use_fill=TRUE), opts_toolbar(saveaspng = FALSE)))
  })
  output$dynamicTitleScatterPr <- renderText({
    dynamicTitleScatterPr()
  })
  
}

# Run App
shinyApp(ui = ui, server = server)
