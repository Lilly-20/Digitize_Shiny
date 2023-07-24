# Load necessary libraries
library(shiny)
library(SurvdigitizeR)
library(here)
library(magrittr)  # for pipe operator
library(ggplot2)   # for plot generation
library(DT)
library(plotly)
library(shinydashboard)

header <- dashboardHeader(title = "Survival Digitize App")
header$children[[3]] <- tags$a(href='https://www.sickkids.ca/',
                               tags$img(src="https://www.sickkids.ca/contentassets/232e33f4208c495eb192e035ee7ba6e5/sickkids-logo-desktop-140x42.png"),
                               style = "padding-top:10px; padding-right:10px; float:right;")
header$children[[4]] <- tags$a(href='https://www.cadth.ca/',
                               tags$img(src="https://www.cadth.ca/themes/custom/cadth/images/cadth-logo.svg",
                                        height = '48', width = '250'),
                               style = "padding-top:10px; padding-right:10px; float:right;")


# Define UI for application
ui <- dashboardPage(
  header,
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "about"),
      menuItem("Analysis", tabName = "analysis")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "about",
              h2("About the Survival Digitize App"),
              p("In the sphere of biomedical research, Kaplan-Meier (KM) survival curves are pivotal in visualizing and interpreting survival data. However, these curves often exist merely in the form of published images, necessitating a reliable means of extracting and digitizing the valuable underlying data. To address this need, we present our Shiny app, a user-friendly, web-based tool that leverages the capabilities of the SurvdigitizeR package developed in R. Our Shiny app provides an interactive platform to digitize KM curves from image files, aiding researchers and clinicians in their data analysis efforts."),
              h3("How does Digitization Shiny App work?"),
              p("Our Shiny app offers a user-friendly interface to digitize Kaplan-Meier survival curves. Users upload an image of the curve and set certain parameters, such as the number of curves, axis scales, and the presence of line censoring markers. The app, using the SurvdigitizeR package's survival_digitize function, then processes the image, extracts the survival curve data, and presents an interactive plot. The resulting data can be conveniently downloaded from the app."),
              h3("Here's a step-by-step guide on how to use our Shiny app:"),
              tags$ul(
                tags$li("Upload an image file of the KM survival curve using the file upload button."),
                tags$li("Input the number of survival curves present in the image."),
                tags$li("Specify the start and end points and increments of the x (time) and y (survival probability) axes."),
                tags$li("Indicate whether the y-axis text is vertical and whether there is a line censoring marker in the plot."),
                tags$li("Click the 'Submit' button. The app will then process the image and display the digitized survival curve."),
                tags$li("Review the digitized plot, and if satisfied with the results, download the digitized data and/or the plot image using the respective download buttons.")
              ),
              h3("Acknowledgements"),
              p("The Shiny app was developed through a collaborative effort, and we wish to express our deepest appreciation to [           ]. This project is an example of how technology can be harnessed to aid and simplify complex data analysis tasks in the realm of medical and life sciences research.")
      ),
      tabItem(tabName = "analysis",
              sidebarLayout(
                sidebarPanel(
                  fileInput("img_path", "Upload Image"),
                  numericInput("num_curves", "Number of Curves:", "", min = 1, max = 10),
                  numericInput("x_start", "X start:", ""),
                  numericInput("x_end", "X end:", ""),
                  numericInput("x_increment", "X increment:", ""),
                  numericInput("y_start", "Y start:", ""),
                  numericInput("y_end", "Y end:", ""),
                  numericInput("y_increment", "Y increment:", ""),
                  checkboxInput("y_text_vertical", "Y Text Vertical", TRUE),
                  checkboxInput("line_censoring", "Line Censoring Marker", FALSE),
                  actionButton("submit", "Submit"),
                  downloadButton("download_plot", "Download Plot")
    ),
                mainPanel(
                  imageOutput("loaded_img"),
                  plotlyOutput("digitized_plot"),
                  DTOutput("table_head"),
                  downloadButton("download_data", "Download Data")
                )
              )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {

  data_out <- reactiveValues(df = NULL)

  output$loaded_img <- renderImage({
    req(input$img_path)
    list(src = input$img_path$datapath,
         alt = "Your Image",
         width = 400,
         height = 300)
  }, deleteFile = FALSE)

  observeEvent(input$submit, {
    req(input$img_path) # Ensure that the file is uploaded
    data_out$df <- survival_digitize(
      img_path = input$img_path$datapath,
      num_curves = input$num_curves,
      x_start = input$x_start,
      x_end = input$x_end,
      x_increment = input$x_increment,
      y_start = input$y_start,
      y_end = input$y_end,
      y_increment = input$y_increment,
      y_text_vertical = input$y_text_vertical,
      line_censoring = input$line_censoring
    )

    output$digitized_plot <- renderPlotly({
      req(data_out$df)
      g = data_out$df %>%
        ggplot(aes(x = time, y= St, color = as.factor(curve), group = curve)) +
        geom_step() + theme_bw() + labs(color='Curves') +
        scale_color_manual(values = c("1" = "red", "2" = "blue")) +
        scale_x_continuous(limits = c(input$x_start, input$x_end),
                           breaks = seq(input$x_start, input$x_end, by = input$x_increment))+
        scale_y_continuous(limits = c(input$y_start, input$y_end),
                           breaks = seq(input$y_start, input$y_end, by = input$y_increment))+
        ggtitle(paste0(input$img_path$name,"_Digitized"))

      plotly::ggplotly(g)
    })
  })

  output$table_head <- renderDT({
    req(data_out$df)
    data_out$df
  })

  output$download_data <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      req(data_out$df)
      write.csv(data_out$df, file, row.names = FALSE)
    }
  )

  output$download_plot <- downloadHandler(
    filename = function() {
      paste(input$img_path$name,"_Auto_Digitized.png", sep="")
    },
    content = function(file) {
      req(data_out$df)
      img <- data_out$df %>%
        ggplot(aes(x = time, y= St, color = as.factor(curve), group = curve)) +
        geom_step() + theme_bw() + labs(color='Curves') +
        scale_color_manual(values = c("1" = "red", "2" = "blue")) +
        scale_x_continuous(limits = c(input$x_start, input$x_end),
                           breaks = seq(input$x_start, input$x_end, by = input$x_increment))+
        scale_y_continuous(limits = c(input$y_start, input$y_end),
                           breaks = seq(input$y_start, input$y_end, by = input$y_increment))+
        ggtitle(paste0(input$img_path$name,"_Digitized"))
      ggsave(file, img, width = 10, height = 8)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
