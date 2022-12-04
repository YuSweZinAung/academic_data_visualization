# ---- Metadata ----
META <- list(
  # Name of the app, used in the browser/tab title
  name        = "Academic Data",
  # A description of the app
  description = "A Shiny Dashboard showing academic data",
  # Link to the app, used in social media cards
  app_url     = "https://apps.garrickadenbuie.com/rstudioconf-2019/",
  # Link to app icon image, used in social media cards
  app_icon    = "https://garrickadenbuie.com/images/2019/rstudioconf-2019-icon.png",
  # The name of the conference or organization
  conf_org    = "rstudio::conf",
  # App title, long, shown when sidebar is open, HTML is valid
  logo_lg     = "<em>Data Visualization</em>",
  # App title, short, shown when sidebar is collapsed, HTML is valid
  logo_mini   = "<em>ADV</em>",
  # Icon for box with count of conference-related tweets
  topic_icon  = "comments",
  # Icon for box with count of "community"-related tweets
  topic_icon_full = "r-project",
  # AdminLTE skin color for the dashboard
  skin_color  = "blue-light",
  # AdminLTE theme CSS files
  theme_css   = c("ocean-next/AdminLTE.css", "ocean-next/_all-skins.css")
)

TOPIC <- list(
  # Name of the conference or topic, for use in descriptive text
  name             = "rstudio::conf",
  # Name of the full Twitter community, for use in descriptive text
  full_community   = "#rstats",
  # Terms related to the topic that must be included in topical tweet text
  terms            = c("rstudioconf", "rstudio conf", "rstudio::conf", "rstudiconf", "rstduioconf"),
  # Hashtags to exclude from the Top 10 Hashtags list (because they're implied by the topic)
  hashtag_exclude  = "rstudio?conf|rstduioconf|rstats|rstudio conf",
  # Words to exclude from the Top 10 Words list (because they're implied by the topic)
  wordlist_exclude = "rstudio|conf|rstats"
)
