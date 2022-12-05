library(shiny)
library(shinydashboard)

dashboard_box_size <- if (is.null(TOPIC$full_community)) "3" else "4 col-lg-2"
observationXData <- c(
  "Marital_status",
  "Application_mode",
  "Course",
  "Daytime_evening_attendance",
  "Previous_qualification",
  "Nationality",
  "Mother_qualification",
  "Father_qualification",
  "Mother_occupation",
  "Father_occupation",
  "Displaced",
  "Educational_special_needs",
  "Debtor",
  "Tuition_fees_up_to_date",
  "Gender",
  "Scholarship_holder",
  "Target",  
  "Application_order",
  "Previous_qualification_grade",
  "Admission_grade",
  "Age_at_enrollment",
  "International",
  "Curricular_units_1st_sem_credited",
  "Curricular_units_1st_sem_enrolled",
  "Curricular_units_1st_sem_evaluations",
  "Curricular_units_1st_sem_approved",
  "Curricular_units_1st_sem_grade",
  "Curricular_units_1st_sem_without_evaluations",
  "Curricular_units_2nd_sem_credited",
  "Curricular_units_2nd_sem_enrolled",
  "Curricular_units_2nd_sem_evaluations",
  "Curricular_units_2nd_sem_approved",
  "Curricular_units_2nd_sem_grade",
  "Curricular_units_2nd_sem_without_evaluations",
  "Unemployment_rate",
  "Inflation_rate",
  "GDP"
)
observationYData <- c(
  "Application_order",
  "Previous_qualification_grade",
  "Admission_grade",
  "Age_at_enrollment",
  "International",
  "Curricular_units_1st_sem_credited",
  "Curricular_units_1st_sem_enrolled",
  "Curricular_units_1st_sem_evaluations",
  "Curricular_units_1st_sem_approved",
  "Curricular_units_1st_sem_grade",
  "Curricular_units_1st_sem_without_evaluations",
  "Curricular_units_2nd_sem_credited",
  "Curricular_units_2nd_sem_enrolled",
  "Curricular_units_2nd_sem_evaluations",
  "Curricular_units_2nd_sem_approved",
  "Curricular_units_2nd_sem_grade",
  "Curricular_units_2nd_sem_without_evaluations",
  "Unemployment_rate",
  "Inflation_rate",
  "GDP"
)

dashboardPage(
  # Dashboard Page Setup ----------------------------------------------------
  # title = META$name,
  # skin  = META$skin_color,
  # theme = c(META$theme_css, "custom.css"),
  # sidebar_mini = TRUE,
  
  # Dashboard Header --------------------------------------------------------
  dashboardHeader(
    title = HTML(glue::glue(
      '<span class="logo-mini">{META$logo_mini}</span>
      <span class="logo-lg">{META$logo_lg}</span>'
    )),
    dropdownMenu(
      type = "message",
      messageItem(
        from = "dummy-email@example.com",
        message =  "Please give feedback to us.",
        icon = icon("envelope"),   
        href = "mailto:dummy-email@example.com"
      ),
      icon = icon("comment")
    ),
    dropdownMenu(
      type = "message",
      messageItem(
        from = "northeastern",
        message =  "",
        icon = icon("twitter"),
        href = "https://twitter.com/intent/tweet"
      ),
      icon = icon("share-alt")
    )
  ),
  
  # Dashboard Sidebar -------------------------------------------------------
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "tab_dashboard", icon = icon("dashboard")),
      menuItem("Observation", tabName = "tab_observation", icon = icon("info")),
      #menuItem("Analysis", tabName = "tab_analysis", icon = icon("stream")),
      menuItem("Data", tabName = "tab_data", icon = icon("compass"))
    )
  ),
  
  # Dashboard Body ----------------------------------------------------------
  dashboardBody(
    tabItems(
      # DASHBOARD ---------------
      tabItem(
        "tab_dashboard",
        # 1st row -> donut chart ---------------
        fluidRow(
          column(12, align="center",
            box(
              title = div(class="charts-title", h2("Academic Data for Enrolment Summary")), 
              solidHeader = TRUE,
              plotOutput("enrolment_summary_plot", height=250),
              width=12
            )
          )
        ),
        # 2nd row -> drop down target ---------------
        fluidRow(
          column(
            12, align="center",
            box(
              selectInput("targetEnrolment", "Enrolment Class",
                          choices = c("Dropout", "Graduate", "Enrolled"),
                          selected = c("Dropout"),
                          multiple = FALSE),
              width=6 
            )
          )
        ),
        # 3rd row -> bar chart and plot ---------------
        fluidRow(
          column(12, align="center",
            box(
              title = div(class="charts-title", h2("Course")), 
              solidHeader = TRUE,
              plotOutput("course_summary", height=250, width = 500),
              width=6
            ),
            box(
              title = div(class="charts-title", h2("Application Order")), 
              solidHeader = TRUE,
              plotOutput("application_order_summary", height=250, width = 500),
              width=6
            )
          )
        ),
        # 4th row -> pie chart and box plot chart ---------------
        fluidRow(
          column(12, align="center",
            box(
              title = div(class="charts-title", h2("Daytime/evening Attendance")), 
              solidHeader = TRUE,
              plotOutput("daytime_attendance_summary", height=250, width = 500),
              width=6
            ),
            box(
              title = div(class="charts-title", h2("Previous Qualification via Grade")), 
              solidHeader = TRUE,
              plotOutput("previous_qualification", height=250, width = 500),
              width=6
            )
          )
        ),
        # 5th row -> bar chart ---------------
        fluidRow(
         column(12, align="center",
          box(
            title = div(class="charts-title", h2("Application Mode")), 
            solidHeader = TRUE,
            plotOutput("application_mode", height=250, width = 500),
            width=6
          )
         )
        )
      ),
      # OBSERVATIONS ---------------
      tabItem(
        "tab_observation",
        fluidRow(
          column(12, align="center",
           box(
             selectInput("obX", "Observation X-axis",
                         choices = observationXData,
                         selected = observationXData[1],
                         multiple = FALSE),
             width=6
           ),
           box(
             selectInput("obY", "Observation Y-axis",
                         choices = observationYData,
                         selected = observationYData[1],
                         multiple = FALSE),
             width=6
           )
          )
        ),
        fluidRow(
          column(12, align="center",
            box(
              title =  div(class="charts-title", h2("Observations")),
              solidHeader = TRUE,
              plotOutput("observation", height=500, width = 1000),
              width = 12
            ) 
          )
        )
      ),
      # ANALYSIS ---------------
      #tabItem(
       # "tab_analysis",
       # h2("TODO: Add analysis items here")
     # ),
      # DATA ---------------
      tabItem(
        "tab_data",
        numericInput("maxrows", "Rows to show", 25),
        verbatimTextOutput("rawtable"),
        downloadButton("downloadCsv", "Download as CSV"),tags$br(),tags$br(),
      )
    )
  )
)