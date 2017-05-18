## app.R ## - Dashboard - Production
library(shiny)
library(shinydashboard)
library(RODBC)
library(ggplot2)
library(dplyr)
library(plotly)
library(scales)
library(stringr)
library(DT)
library(RColorBrewer)

list.of.packages <- c("ggplot2", "plotly", "RColorBrewer")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


channel <- odbcConnect("NZSQL", believeNRows = FALSE)
check.connect <- function(channel) {
  connected <- FALSE;
  while (!connected) {
    tryCatch(
      {
        sqlQuery(channel, "select * from edwvehicle..status_d limit 1;");
        connected <- TRUE
        print("Connected!")
      },
      error=function(e) {
        print("Reconnecting to DB...");
        channel <- odbcConnect("NZSQL")
      })
  }
}

segment.list <- sqlQuery(channel, sprintf("select distinct v.SEGMENT
                                          from EDWVEHICLE..VALUATION_VEHICLE_D v
                                          where v.NO_VALUE_FLG = 'N'
                                          and v.KBB_PUBLISHED_FLG = 'Y'
                                          and v.SEGMENT is not null
                                          order by 1 asc"))

make.list <- sqlQuery(channel, sprintf("select distinct v.MAKE_NAME
                                       from EDWVEHICLE..VALUATION_VEHICLE_D v
                                       where v.NO_VALUE_FLG = 'N'
                                       and v.KBB_PUBLISHED_FLG = 'Y'
                                       order by 1 asc"))

model.list <- sqlQuery(channel, sprintf("select distinct v.MODEL_NAME, v.MAKE_NAME
                                        from EDWVEHICLE..VALUATION_VEHICLE_D v
                                        --where v.NO_VALUE_FLG = 'N'
                                        --and v.KBB_PUBLISHED_FLG = 'Y'
                                        order by 1 asc"))

year.list <- sqlQuery(channel, sprintf("select distinct v.MODEL_YEAR
                                       from EDWVEHICLE..VALUATION_VEHICLE_D v
                                       --where v.NO_VALUE_FLG = 'N'
                                       where v.KBB_PUBLISHED_FLG = 'Y'
                                       order by 1 desc"))

fte_theme <- function() {
  
  # Generate the colors for the chart procedurally with RColorBrewer
  palette <- brewer.pal("Greys", n=9)
  color.background = palette[1]
  color.grid.major = palette[3]
  color.axis.text = palette[6]
  color.axis.title = palette[7]
  color.title = palette[9]
  
  # Begin construction of chart
  theme_bw(base_size=9) +
    
    # Set the entire chart region to a light gray color
    theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border=element_rect(color=color.background)) +
    
    # Format the grid
    theme(panel.grid.major=element_line(color=color.grid.major,size=.25)) +
    theme(panel.grid.minor=element_blank()) +
    theme(axis.ticks=element_blank()) +
    
    # Format the legend, but hide by default
    #theme(legend.position="none") +
    theme(legend.background = element_rect(fill=color.background)) +
    theme(legend.text = element_text(size=7,color=color.axis.title)) +
    
    # Set title and axis labels, and format these and tick marks
    theme(plot.title=element_text(color=color.title, size=15, vjust=1.25)) +
    theme(axis.text.x=element_text(size=10,color=color.axis.text)) +
    theme(axis.text.y=element_text(size=10,color=color.axis.text)) +
    theme(axis.title.x=element_text(size=11,color=color.axis.title, vjust=0)) +
    theme(axis.title.y=element_text(size=11,color=color.axis.title, vjust=1.25)) +
    
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}
# av.bias.query <- sqlQuery(channel, sprintf("SELECT
#                                            b.PUBLISH_WEEK
#                                            ,b.VIMSVEHICLEID as VIMSID
#                                            ,b.COUNT_4WK as CNT_4WK
#                                            ,round(b.AV_BIAS_4WK, 2) as BIAS_4WK
#                                            ,round(b.FCAST_BIAS_4WK, 2) as M_BIAS_4Wk
#                                            ,round(b.AV_DLR_BIAS_4WK,0) as BIAS_$_4WK
#                                            ,round(b.FCAST_DLR_BIAS_4WK, 0) as M_BIAS_$_4WK
#                                            ,b.COUNT_CW as CNT_CW
#                                            ,round(b.AV_BIAS_CW, 2) as BIAS_CW
#                                            ,round(b.FCAST_BIAS_CW, 2) as M_BIAS_CW
#                                            ,round(b.AV_DLR_BIAS_CW, 0) as BIAS_$_CW
#                                            ,round(b.FCAST_DLR_BIAS_CW, 0) as M_BIAS_$_CW
#                                            FROM
#                                            SAS_STAGE_VAL..BS_BIAS_BY_VEHICLEID b
#                                            ORDER BY
#                                            b.PUBLISH_WEEK desc"))
# 
# ucfpp.bias.query <- sqlQuery(channel, sprintf("SELECT
#                                               bf.publish_week
#                                               ,bf.VIMSVEHICLEID as VIMSID
#                                               ,bf.COUNT_4WK
#                                               ,round(bf.PUBLISHED_BIAS_4WK, 2) as BIAS_4WK
#                                               ,round(bf.FCAST_BIAS_4WK, 2) as M_BIAS_4wk
#                                               ,round(bf.PUBLISHED_DLR_BIAS_4WK, 0) as BIAS_$_4WK
#                                               ,round(bf.FCAST_DLR_BIAS_4WK, 0) as M_BIAS_$_4WK
#                                               ,bf.COUNT_CW
#                                               ,round(bf.PUBLISHED_BIAS_CW,2) as BIAS_CW
#                                               ,round(bf.FCAST_BIAS_CW, 2) as M_BIAS_CW
#                                               ,round(bf.PUBLISHED_DLR_BIAS_CW, 0) as BIAS_$_CW
#                                               ,round(bf.FCAST_DLR_BIAS_CW, 0) as M_BIAS_$_CW
#                                               FROM SAS_STAGE_VAL..BS_FPP_BIAS_BY_VEHICLEID bf
#                                               order by bf.PUBLISH_WEEK desc"))
# 
# srp.bias.query <- sqlQuery(channel, sprintf("SELECT
#                                             bs.publish_week
#                                             ,bs.VIMSVEHICLEID as VIMSID
#                                             ,bs.COUNT_4WK
#                                             ,round(bs.PUBLISHED_BIAS_4WK, 2) as BIAS_4WK
#                                             ,round(bs.FCAST_BIAS_4WK, 2) as M_BIAS_4wk
#                                             ,round(bs.PUBLISHED_DLR_BIAS_4WK, 0) as BIAS_$_4WK
#                                             ,round(bs.FCAST_DLR_BIAS_4WK, 0) as M_BIAS_$_4WK
#                                             ,bs.COUNT_CW
#                                             ,round(bs.PUBLISHED_BIAS_CW,2) as BIAS_CW
#                                             ,round(bs.FCAST_BIAS_CW, 2) as M_BIAS_CW
#                                             ,round(bs.PUBLISHED_DLR_BIAS_CW, 0) as BIAS_$_CW
#                                             ,round(bs.FCAST_DLR_BIAS_CW, 0) as M_BIAS_$_CW
#                                             FROM SAS_STAGE_VAL..BS_SRP_BIAS_BY_VEHICLEID bs
#                                             order by bs.PUBLISH_WEEK desc"))


# segment.list <- c('TEST')
# make.list <- c('TEST')
# model.list <- c('TEST')
# year.list <- c('TEST')
loadingMessages <- function()
{
  funny.messages <- c(
    "The Funnies",
    "Reticulating splines...",
    "Generating witty dialog...",
    "Swapping time and space...",
    "Spinning violently around the y-axis...",
    "Tokenizing real life...",
    "Bending the spoon...",
    "Filtering morale...",
    "Don't think of purple hippos...",
    "We need a new fuse...",
    "Have a good day.",
    "Upgrading Windows, your PC will restart several times. Sit back and relax.",
    "640K ought to be enough for anybody",
    "The architects are still drafting",
    "The bits are breeding",
    "We're building the buildings as fast as we can",
    "Would you prefer chicken, steak, or tofu?",
    "(Pay no attention to the man behind the curtain)",
    "...and enjoy the elevator music...",
    "Please wait while the little elves draw your map",
    "Don't worry - a few bits tried to escape, but we caught them",
    "Would you like fries with that?",
    "Checking the gravitational constant in your locale...",
    "Go ahead -- hold your breath!",
    "...at least you're not on hold...",
    "Hum something loud while others stare",
    "You're not in Kansas any more",
    "The server is powered by a lemon and two electrodes.",
    "Please wait while a larger software vendor in Seattle takes over the world",
    "We're testing your patience",
    "As if you had any other choice",
    "Follow the white rabbit",
    "Why don't you order a sandwich?",
    "While the satellite moves into position",
    "keep calm and npm install",
    "The bits are flowing slowly today",
    "Dig on the 'X' for buried treasure... ARRR!",
    "It's still faster than you could draw it",
    "The last time I tried this the monkey didn't survive. Let's hope it works better this time.",
    "I should have had a V8 this morning.",
    "My other loading screen is much faster.",
    "Testing on Timmy... We're going to need another Timmy.",
    "Reconfoobling energymotron...",
    "(Insert quarter)",
    "Are we there yet?",
    "Have you lost weight?",
    "Just count to 10",
    "Why so serious?",
    "It's not you. It's me.",
    "Counting backwards from Infinity",
    "Don't panic...",
    "Embiggening Prototypes",
    "Do not run! We are your friends!",
    "Do you come here often?",
    "Warning: Don't set yourself on fire.",
    "We're making you a cookie.",
    "Creating time-loop inversion field",
    "Spinning the wheel of fortune...",
    "Loading the enchanted bunny...",
    "Computing chance of success",
    "I'm sorry Dave, I can't do that.",
    "Looking for exact change",
    "All your web browser are belong to us",
    "All I really need is a kilobit.",
    "I feel like im supposed to be loading something. . .",
    "What do you call 8 Hobbits? A Hobbyte.",
    "Should have used a compiler language...",
    "Is this Windows?",
    "Adjusting flux capacitor...",
    "Please wait until the sloth starts moving.",
    "Don't break your screen yet!",
    "I swear it's almost done.",
    "Let's take a mindfulness minute...",
    "Unicorns are at the end of this road, I promise.",
    "Listening for the sound of one hand clapping...",
    "Keeping all the 1's and removing all the 0's...",
    "Putting the icing on the cake. The cake is not a lie...",
    "Cleaning off the cobwebs...",
    "Making sure all the i's have dots...",
    "Connecting Neurotoxin Storage Tank...",
    "Granting wishes...",
    "We are not liable for any broken screens as a result of waiting.",
    "Time flies when you're having fun.",
    "Get some coffee and come back in ten minutes..",
    "Spinning the hamster.",
    "99 bottles of beer on the wall..",
    "Stay awhile and listen..",
    "Be careful not to step in the git-gui",
    "You shall not pass! yet..",
    "Load it and they will come",
    "Convincing AI not to turn evil..",
    "There is no spoon. Because we are not done loading it",
    "Your left thumb points to the right and your right thumb points to the left.",
    "How did you get here?",
    "Wait, do you smell something burning?",
    "Computing the secret to life, the universe, and everything.",
    "When nothing is going right, Go left!!...",
    "I love my job only when I'm on vacation...",
    "i'm not lazy, I'm just relaxed!!",
    "Never steal. The government hates competition....",
    "Why are they called apartments if they are all stuck together?",
    "Life is Short - Talk Fast!!!!",
    "Optimism - is a lack of information.....",
    "Save water and shower together",
    "Whenever I find the key to success, someone changes the lock.",
    "Sometimes I think war is God's way of teaching us geography.",
    "I've got problem for your solution...",
    "Where there's a will, there's a relative.",
    "User: the word computer professionals use when they mean !!idiot!!",
    "Adults are just kids with money.",
    "I think I am, therefore, I am. I think.",
    "A kiss is like a fight, with mouths.",
    "You don't pay taxes-they take taxes.",
    "Coffee, Chocolate, Men. The richer the better!",
    "I am free of all prejudices. I hate everyone equally.",
    "git happens",
    "May the forks be with you",
    "A commit a day keeps the mobs away",
    "This is not a joke, it's a commit.",
    "Making America... great... again...",
    "Constructing additional pylons...",
    "Roping some seaturtles...",
    "Locating Jebediah Kerman...",
    "We are not liable for any broken screens as a result of waiting.",
    "Hello IT, have you tried turning it off and on again?",
    "If you type Google into Google you can break the internet",
    "Well, this is embarrassing.",
    "Didn't know paint dried so quickly.",
    "What is the airspeed velocity of an unladen swallow?",
    "Hello, IT... Have you tried forcing an unexpected reboot?",
    "They just toss us away like yesterday's jam.",
    "They're fairly regular, the beatings, yes. I'd say we're on a bi-weekly beating.",
    "The Elders of the Internet would never stand for it.",
    "Space is invisible mind dust, and stars are but wishes.",
    "Didn't know paint dried so quickly.",
    "Everything sounds the same",
    "I'm going to walk the dog",
    "I didn't choose the engineering life. The engineering life chose me.",
    "Dividing by zero...",
    "Spawn more Overlord!",
    "If I'm not back in five minutes, just wait longer.",
    "Some days, you just can't get rid of a bug!",
    "We're going to need a bigger boat.",
    "Chuck Norris never git push. The repo pulls before.",
    "Web developers do it with <style>",
    "I need to git pull --my-life-together",
    "Java developers never RIP. They just get Garbage Collected.",
    "Cracking military-grade encryption...",
    "Simulating traveling salesman...",
    "Proving P=NP...",
    "Entangling superstrings...",
    "Twiddling thumbs...",
    "Searching for plot device...",
    "Trying to sort in O(n)...",
    "Laughing at your pictures-i mean, loading...",
    "Sending data to NS-i mean, our servers.",
    "Looking for sense of humour, please hold on.",
    "Please wait while the intern refills his coffee.",
    "A different error message? Finally, some progress!",
    "Hold on while we wrap up our git together...sorry",
    "Please hold on as we reheat our coffee",
    "Kindly hold on as we convert this bug to a feature...",
    "Kindly hold on as our intern quits vim...",
    "Winter is coming...",
    "Installing dependencies",
    "Switching to the latest JS framework",
    "Distracted by cat gifs",
    "Finding someone to hold my beer",
    "BRB, working on my side project",
    "@todo Insert witty loading message",
    "Let's hope it's worth the wait",
    "Aw, snap! Not..",
    "Ordering 1s and 0s...",
    "Updating dependencies...",
    "Whatever you do, don't look behind you...",
    "Please wait... Consulting the manual...",
    "It is dark. You're likely to be eaten by a grue.",
    "Loading funny message...",
    "It's 10:00pm. Do you know where your children are?",
    "Waiting Daenerys say all her titles...",
    "Feel free to spin in your chair",
    "What the what?",
    "format C: ...",
    "Forget you saw that password I just typed into the IM ...",
    "What's under there?",
    "Your computer has a virus, it's name is Windows!",
    "Go ahead, hold your breath and do ironman plank till loading complete",
    "Bored of slow loading spinner, buy more RAM!",
    "Help, I'm trapped in a loader!"
  )
  
  return(sample(funny.messages, 1))
}

ui <- dashboardPage(
  dashboardHeader(title = "SHINY"),
  
  dashboardSidebar(
    img(src="ctab.png", height=300), 
    sidebarMenu(
      id = "tabs",
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Used Car", tabName = "usedcar", icon = icon("car"),
               menuSubItem("Scatter Plot", tabName = "usedcar1"),
               menuSubItem("Comparison Plot", tabName = "usedcar4"),
               menuSubItem("Data Table", tabName = "usedcar2"),
               menuSubItem("Data Summary", tabName = "usedcar3")
      ),
      menuItem("New Car", tabName = "newcar", icon = icon("gift"),
               menuSubItem("Scatter Plot", tabName = "newcar1"),
               menuSubItem("Data Table", tabName = "newcar2"),
               menuSubItem("Data Summary", tabName = "newcar3")
      ),
      menuItem("Mass Scatters", tabName = "mass-scatters", icon=icon("area-chart"))
    )
  ),
  
  dashboardBody(
    # tags$head(
    #   tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")
    # ),
    tags$head(tags$style(HTML('
                              .main-header .logo {
                              font-family: "Georgia", Times, "Times New Roman", serif;
                              font-weight: bold;
                              font-size: 30px;
                              }
                              '))),
    
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                column(width = 6,
                       shinydashboard::box(width = NULL, solidHeader = TRUE, status = "primary",
                                           title = "Select the Date Range",
                                           dateRangeInput("dateRange", "", start = Sys.Date() - 120, end = Sys.Date() + 14)
                       ),
                       valueBoxOutput("nrowUsedBox", width = NULL),
                       valueBoxOutput("nrowUCFPPBox", width = NULL),
                       valueBoxOutput("nrowSRPBox", width = NULL),
                       valueBoxOutput("nrowMassBox", width = NULL),
                       valueBoxOutput("nrowNewBox", width = NULL)
                       
                       
                ),
                column(width = 6,
                       shinydashboard::box(width = NULL, solidHeader = TRUE, status = "primary",
                                           title = "Select one or more of the following:",
                                           selectizeInput("analyst", "Analyst:", c("","Anthony","Janice", "Martin", "Tarek", "Will", "Joseph", "Sandra"), options = list(placeholder = 'Select an Analyst')),
                                           selectizeInput("segment", "Segment:", c("", as.character(segment.list$SEGMENT)), options=list(placeholder = 'Select a Segment', selectOnTab = TRUE)),
                                           #selectizeInput("segment", "Segment:", c("", segment.list), options=list(placeholder = 'Select a Segment', selectOnTab = TRUE)),
                                           selectizeInput("make", "Make:", c("", as.character(make.list$MAKE_NAME)), options=list(placeholder = 'Select a Make', selectOnTab = TRUE)),
                                           #selectizeInput("make", "Make:", c("", make.list), options=list(placeholder = 'Select a Make', selectOnTab = TRUE)),
                                           #selectizeInput("model", "Model:", c("", as.character(model.list$MODEL_NAME)), multiple = FALSE, options=list(placeholder = 'Select a Model', selectOnTab = TRUE)),
                                           uiOutput("model"),
                                           selectizeInput("year", "Year:", c("", as.character(year.list$MODEL_YEAR)), options=list(placeholder = 'Select a Year', selectOnTab = TRUE)),
                                           #selectizeInput("year", "Year:", c("", year.list), options=list(placeholder = 'Select a Year', selectOnTab = TRUE)),
                                           textAreaInput("idSearch", "VIMSID:", placeholder = "Please enter 1 or more IDs separated with commas"),
                                           actionButton("usedButton", "Pull Used Transactions", icon("car")),
                                           actionButton("newButton", "Pull New Transactions", icon("paper-plane")),
                                           actionButton("mass.scatters.button", "Pull Mass Scatters", icon = icon("search")),
                                           checkboxInput("getUCFPP", "Get UCFPP Transactions", value = FALSE),
                                           checkboxInput("getListings", "Get Retail Listings", value = FALSE)
                                           
                       )
                )
              )
      ),
      tabItem(tabName = 'mass-scatters',
              fluidRow(box(
                width = 12,
                status = "primary",
                plotOutput("mass.scatters.preview", height = "600px")
              )),
              fluidRow(box(
                width = 3,
                status = "primary",
                div(class = "row",
                    div(class = "col-md-6", checkboxGroupInput("scatter.options", "Scatter Options:",c("KBB Values" = "kbb","Forecast" = "fc","Smoother" = "smoother"), selected = c("kbb", "fc", "smoother")), downloadButton("download.scatters", "Download", icon("arrow-circle-right")))
                    #div(class = "col-md-6", textAreaInput("scatter.ids", "VIMSIDs:", placeholder = "Enter IDs here"), textInput("filename", "Filename:", placeholder = "Enter filename here"))
                    #div(class = "col-md-3", actionButton("mass.scatters.button", "Pull Mass Scatters", icon = icon("search")))
                )
              )
              )
      ),  
      tabItem(tabName = "usedcar1",
              fluidRow(
                column(width = 3,
                       shinydashboard::box(width = NULL, status = "primary",
                                           uiOutput("year.used"), 
                                           uiOutput("make.used"),
                                           uiOutput("model.used"),
                                           uiOutput("trim.used"),
                                           strong("Vehicle Map: "),
                                           textOutput("vehicleMap.used"),
                                           strong("VIMSID:"),
                                           textOutput("vimsid.used"),
                                           br(),
                                           uiOutput("dates.used")
                                           # p(
                                           #   downloadButton('downloadDataUsed','.CSV'),
                                           #   downloadButton('downloadPlotUsed','.PNG')
                                           #   downloadButton('downloadPlotUsedPDF','.PDF')
                                           # )
                       )
                ),
                column(width = 9,
                       shinydashboard::box(width = NULL, status = "primary", plotlyOutput("plot.used"), textOutput("nrowUsedBox.sub")),
                       uiOutput("engine.used")
                       # tabBox(
                       #   width = NULL,
                       #   tabPanel("Plotly", plotlyOutput("plot.used"), textOutput("nrowUsedBox.sub")),
                       #   tabPanel("ggplot", plotOutput("plot.used.ggplot"), textOutput("nrowUsedBox.sub.ggplot"))
                       # )
                )
              ),
              fluidRow(
                column(width = 6,
                       # div(class="row",
                       #     div(class = "col-xs-4", checkboxInput("yaxis.used", "Y-axis", FALSE)),
                       #     div(class = "col-xs-4", numericInput("ymin.used", "ymin", 0, min = 0, step = 1000)),
                       #     div(class = "col-xs-4", numericInput("ymax.used", "ymax", 30000, step = 1000))
                       # ) ,
                       
                       # div(class="row",
                       #     div(class = "col-xs-4", checkboxInput("xaxis.used", "X-axis (mileage)", FALSE)),
                       #     div(class = "col-xs-4", numericInput("xmin.used", "xmin", 0, min = 0, step = 10000)),
                       #     div(class = "col-xs-4", numericInput("xmax.used", "xmax", 150000, step = 10000))
                       # ),
                       div(class = "row",
                           div(class = "col-md-3", checkboxInput("vin.used.checkbox", "VIN Search", FALSE)),
                           div(class = "col-md-3", textInput("vin.used", "VIN"), strong(textOutput("vin.used.text"))),
                           div(class = "col-md-3", numericInput("size.used", "Size: ", 1.25, step = 0.25))
                       ),
                       div(class = "row",
                           div(class = "col-md-2", checkboxInput("mileage.filter", "Mileage Filter", FALSE)),
                           div(class = "col-md-10", valueBoxOutput("under.box"), valueBoxOutput("over.box"), valueBoxOutput("buckets.box"))
                           #div(class = "col-md-5", valueBoxOutput("over.box"))
                       )
                       #valueBoxOutput("under.box"), valueBoxOutput("over.box")
                       
                ),
                column(width = 2,
                       checkboxInput("kbb.used","KBB values",TRUE),
                       checkboxInput("forecast.used","Forecast",TRUE),
                       checkboxInput("forecast.raw.used","Raw Forecast",FALSE),
                       checkboxInput("engine", "Engine", FALSE) 
                       #checkboxInput("bias.marker", "Bias Marker", FALSE)
                ),
                column(width = 2,
                       checkboxInput("smooth.used","Smoother",TRUE),
                       checkboxInput("weekly.median.used", "Weekly Median", FALSE),
                       checkboxInput("weekly.forecast.used", "Weekly Forecast", FALSE),
                       # conditionalPanel(
                       #   "input.smooth.used == 'TRUE'",
                       #   checkboxInput("error.used","Error bars",FALSE)),
                       #checkboxInput("fill.used","Fill", TRUE),
                       checkboxInput("buckets.used", "Buckets", FALSE),
                       uiOutput("certified")
                ),
                column(width = 2,
                       radioButtons("plotType", "Plot Type:",
                                    choices = c("AV Plot" = "avplot",
                                                "Raw AV Plot" = "rawavplot",
                                                "Mileage Plot" = "mileageplot",
                                                "UCFPP Plot" = "ucfppplot",
                                                "Retail Plot" = "retailplot"),
                                    selected = "avplot")
                       #,numericInput("size.used", "Size: ", 1.25, step = 0.25)
                )
              ),
              fluidRow(
                dataTableOutput("bias")
              )
      ),
      #Comparison Scatter UI
      tabItem(tabName = "usedcar4",
              fluidRow(
                column(width = 3,
                       shinydashboard::box(width = NULL, status = "primary",
                                           h4("Vehicle 1"),
                                           uiOutput("year.used.comparison1"), 
                                           uiOutput("make.used.comparison1"),
                                           uiOutput("model.used.comparison1"),
                                           uiOutput("trim.used.comparison1"),
                                           uiOutput("dates.used.comparison"),
                                           h4("Vehicle 2"),
                                           uiOutput("year.used.comparison2"), 
                                           uiOutput("make.used.comparison2"),
                                           uiOutput("model.used.comparison2"),
                                           uiOutput("trim.used.comparison2")
                                           
                       )),
                column(width = 9,
                       shinydashboard::box(width = NULL, status = "primary", plotOutput("plot.used.comparison"),valueBoxOutput("nrowUsedBox.sub.comparison")),
                       column(width = 2,
                              checkboxInput("kbb.used.comparison1","KBB values 1",TRUE),
                              checkboxInput("kbb.used.comparison2", "KBB values 2", FALSE),
                              
                              #Forecast comparison haven't been implemented yet
                              checkboxInput("forecast.used.comparison1","Forecast 1",TRUE),
                              checkboxInput("forecast.used.comparison2","Forecast 2",FALSE)
                       ),
                       column(width = 2,
                              checkboxInput("smooth.used.comparison1","Smoother 1",TRUE),
                              checkboxInput("smooth.used.comparison2","Smoother 2",FALSE)
                       ),
                       column(width = 2,
                              radioButtons("plotType.comparison", "Plot Type:",
                                           choices = c("AV Plot" = "avplot.comparison"
                                                       
                                                       #These plots haven't been implemented yet
                                                       #"Mileage Plot" = "mileageplot.comparison",
                                                       #"UCFPP Plot" = "ucfppplot.comparison",
                                                       #"Retail Plot" = "retailplot.comparison"
                                           ),
                                           selected = "avplot.comparison"),
                              numericInput("size.used.comparison", "Size: ", 1.25, step = 0.25)
                       )
                )
                
                
              )#,
              # fluidRow(
              #   column(width = 2,
              #          checkboxInput("kbb.used.comparison1","KBB values 1",TRUE),
              #          checkboxInput("kbb.used.comparison2", "KBB values 2", FALSE),
              #          checkboxInput("forecast.used.comparison1","Forecast 1",TRUE),
              #          checkboxInput("forecast.used.comparison2","Forecast 2",FALSE)
              #   ),
              #   column(width = 2,
              #          checkboxInput("smooth.used.comparison1","Smoother 1",TRUE),
              #          checkboxInput("smooth.used.comparison2","Smoother 2",FALSE)
              #   ),
              #   column(width = 2,
              #          radioButtons("plotType.comparison", "Plot Type:",
              #                       choices = c("AV Plot" = "avplot.comparison",
              #                                   "Mileage Plot" = "mileageplot.comparison",
              #                                   "UCFPP Plot" = "ucfppplot.comparison",
              #                                   "Retail Plot" = "retailplot.comparison"),
              #                       selected = "avplot.comparison"),
              #          numericInput("size.used.comparison", "Size: ", 1.25, step = 0.25)
              #   )
              # )
      ),
      tabItem(tabName = "usedcar2",
              dataTableOutput("table.used")
      ),
      tabItem(tabName = "usedcar3",
              tabBox(id="tabset1", height="500px", width = "500px",
                     tabPanel("AV", verbatimTextOutput("summary.used")),
                     tabPanel("UCFPP", verbatimTextOutput("summary.ucfpp")),
                     tabPanel("Retail", verbatimTextOutput("summary.srp"))
              )
      ),
      tabItem(tabName = "newcar1",
              fluidRow(
                column(width = 3,
                       shinydashboard::box(width = NULL, status = "primary",
                                           uiOutput("year.new"), 
                                           uiOutput("make.new"),
                                           uiOutput("model.new"),
                                           uiOutput("trim.new"),
                                           strong("VIMSID:"),
                                           textOutput("vimsid.new"),
                                           uiOutput("dates.new")
                       )
                ),
                column(width = 9,
                       shinydashboard::box(width = NULL, status = "primary",
                                           plotlyOutput("plot.new")
                       )
                )
              ),
              fluidRow(
                # column(width = 4,
                #        checkboxInput("yaxis.new", "Y-axis", FALSE),
                #        div(class="row",
                #            div(class = "col-xs-6", numericInput("ymin.new", "ymin", -0.2, min = -0.2, step = 0.01)),
                #            div(class = "col-xs-6", numericInput("ymax.new", "ymax", 0, step = 0.01))
                #        )
                # ),
                column(width = 4
                       #checkboxInput("fill.new","Fill", TRUE),
                       
                ),
                
                column(width = 2,
                       checkboxInput("kbb.new","KBB values",TRUE),
                       checkboxInput("fpprange","FPP range",TRUE),
                       checkboxInput("forecast.new","Forecast",TRUE)
                ),
                column(width = 2,
                       checkboxInput("smooth.new","Smoother",TRUE),
                       # conditionalPanel(
                       #   condition = "input.smooth.new == true",
                       #   checkboxInput("error.new","Error bars",FALSE)),
                       checkboxInput("msrp","Show MSRP",FALSE)                      
                ),
                column(width = 4,
                       numericInput("size.new", "Size:", 1.25, step = 0.25))
                # sliderInput("point_alpha","Opacity:",
                #             min = 0.0, max = 1, value = 1, step = 0.1))
              ),
              fluidRow(
                valueBoxOutput("nrowNewBox.sub", width = 3),
                valueBoxOutput("nrowNewBox.sub.low", width = 3),
                valueBoxOutput("nrowNewBox.sub.inrange", width = 3),
                valueBoxOutput("nrowNewBox.sub.high", width = 3)
              ),
              fluidRow(
                valueBoxOutput("error.new.fc", width = 3),
                valueBoxOutput("error.new.pub", width = 3)
              )
      ),
      tabItem(tabName = "newcar2",
              dataTableOutput("table.new")
      ),
      tabItem(tabName = "newcar3",
              verbatimTextOutput("summary.new")
      )
    )
    )  
    )

server <- function(input, output, session) {
  
  segmentText <- reactive({
    if (input$analyst == "") {
      if (input$segment == "") {
        segment <- ""
      } else {
        segment <- paste("AND v.SEGMENT in ('",input$segment,"')",sep="")
      }
    } else {
      if (input$segment == "") {
        segment <- switch(input$analyst,
                          "Anthony" = paste("AND v.SEGMENT in ('Truck')"),
                          "Tarek" = paste("AND v.SEGMENT in ('Compact Car', 'Compact Sports Car', 'Pony Car', 'Import Sports Car')"),
                          "Martin" = paste("AND v.SEGMENT in ('Compact Luxury Car', 'Exotic', 'High End Luxury Car', 'Midsize Luxury Car', 'High Performance', 'Luxury Roadster')"),
                          #"Sean" = paste("AND v.SEGMENT in ('')"),
                          "Sandra" = paste("AND v.SEGMENT in ('Midsize Car', 'Fullsize Car', 'Van')"),
                          "Janice" = paste("AND v.SEGMENT in ('Subcompact Car', 'Sports Car', 'Minivan')"),
                          "Will" = paste("AND V.SEGMENT in ('Compact SUV Crossover','Midsize Truck', 'Near Luxury Car')"),
                          "Joseph" = paste("AND V.SEGMENT in ('Fullsize SUV Crossover', 'Midsize SUV Crossover')"))
      } else {
        segment <- paste("AND v.SEGMENT in ('", input$segment,"')",sep="")
      }
    }
  })
  
  makeText <- reactive ({
    if (input$make == "") {
      make <- ""
    } else {
      make <- paste("AND v.MAKE_NAME = '", input$make, "'", sep="")
    }
  })
  
  modelText <- reactive ({
    if (input$model.filter == "") {
      model <- ""
    } else {
      model <- paste("AND v.MODEL_NAME = '", input$model.filter, "'", sep="")
    }
  })
  
  yearText <- reactive({
    if (input$year == "") {
      year <- ""
    } else {
      year <- paste("AND v.MODEL_YEAR = ", input$year, sep="")
    }
  })
  
  vimsidText <- reactive({
    if (input$idSearch == "") {
      vimsid <- ""
    } else {
      id.input <- str_replace_all(input$idSearch, "[\\r\\n\\s\\t]","")
      id.input.sub <- str_replace_all(id.input, ",", "','")
      vimsid <- paste("AND v.VIMS_VEHICLE_ID in ('", id.input.sub, "')", sep = "")
    }
  })
  
  mass.scatters.vvid <- reactive({
    if (input$scatter.id == "") {
      vimsid <- ""
    } else {
      id.input <- str_replace_all(input$scatter.id, "[\\r\\n\\s\\t]","")
      id.input.sub <- str_replace_all(id.input, ",", "','")
      vimsid <- paste("AND v.VIMS_VEHICLE_ID in ('", id.input.sub, "')", sep = "")
    }
  })
  
  # UI Output for model select input
  output$model <- renderUI({
    if(input$make == ""){
      model <- model.list
    } else {
      model <- model.list %>%
        filter(MAKE_NAME == input$make)
    }
    selectizeInput("model.filter", "Model:", c("",as.character(model$MODEL_NAME)), options=list(placeholder = 'Select a Model'))
  })
  
  # UI Output for certified input
  output$certified <- renderUI({
    if (input$plotType == "retailplot") {
      checkboxInput("certified", "Include Certified", FALSE)
    } else {
      NULL
    }
  })
  
  ## Used Car Scatter functions ##
  
  query.used <- reactive ({
    
    sprintf(
      "select
      d.CALENDARDATE as SALE_DATE
      ,v.VIMS_VEHICLE_ID
      , v.MODEL_YEAR
      , v.MAKE_NAME
      , v.MODEL_NAME
      , v.VEHICLE_NAME
      , v.VEHICLE_MAP
      , v.SEGMENT
      , a.VIN
      , a.MILEAGE
      , case when a.CONFIGURED_AV_AMOUNT is not null then a.SALE_PRICE_AMOUNT - (a.CONFIGURED_AV_AMOUNT - a.BASE_VALUATION_AMOUNT) - (ow.CURR_CONFIG_WORKING_OPTION_ADJUSTMENT_AMOUNT - a.AV_OPTION_ADJUSTMENT_AMOUNT)- coalesce(cgb.VALUE_ADJUSTMENT_AMOUNT,0) else a.SALE_PRICE_AMOUNT END as SALE_PRICE_BASE
      , a.SALE_PRICE_AMOUNT
      , round(fc.FINALFCAST_BASE) as FORECAST
      , fc.SALEDATE as FORECAST_DATE
      , ss.MNHM_BUCKET
      , ss.MNHM_PRICE
      , ss.MNHM_COUNT
      ,case when vvov.PART_OF_BASE_VALUE_FLAG = 'Y' then ve.ENGINE_DESCRIPTION || ' (default)' else ve.ENGINE_DESCRIPTION end as engine_description
      from edwvehicle..AUCTION_TRANSACTION_F a
      join edwvehicle..VEHICLE_CONDITION_D vc on vc.VEHICLE_CONDITION_ID = a.PRICED_VEHICLE_CONDITION_ID
      join edwvehicle..DATE_D d on d.DATEID = a.SALE_DATE_ID
      join edwvehicle..AGG_AUCTION_TRANSACTION_CURR_CONFIG_WORKING ow on ow.AUCTION_TRANSACTION_ID = a.AUCTION_TRANSACTION_ID
      join edwvehicle..VALUATION_VEHICLE_D v on v.VALUATION_VEHICLE_ID = a.VALUATION_VEHICLE_ID
      join edwvehicle..GEOGRAPHY_D g on g.GEOGRAPHY_ID = a.AUCTION_GEOGRAPHY_ID
      join edwvehicle..PUBLICATION_VERSION_D pv
      on d.calendardate between pv.EFFECTIVE_PRODUCT_VERSION_START_DATE and pv.effective_product_version_end_date and pv.PUBLICATION_VERSION_DESC = 'UC Internet'
      join edwvehicle..VALUATION_ENGINE_D ve on ve.VALUATION_ENGINE_ID = a.VALUATION_ENGINE_ID
      join (select vvov.* from edwvehicle..VALUATION_VEHICLE_OPTION_VALUE_F vvov join edwvehicle..PUBLICATION_VERSION_D p on p.publication_version_id = vvov.publication_version_id
      where current_date between p.EFFECTIVE_PRODUCT_VERSION_START_DATE and p.effective_product_version_end_date and p.product_desc = 'UC Internet') vvov     
      on vvov.VALUATION_OPTION_ID = a.VALUATION_ENGINE_ID and vvov.VALUATION_VEHICLE_ID = a.VALUATION_VEHICLE_ID
      left join (select va.VALUATION_VEHICLE_ID,vc.VEHICLE_CONDITION_DESC ,va.PUBLICATION_VERSION_ID,va.VALUE_ADJUSTMENT_AMOUNT
      from edwvehicle..VALUATION_VEHICLE_VALUE_ADJUSTMENT_F va
      join edwvehicle..VEHICLE_CONDITION_D vc on va.VEHICLE_CONDITION_ID = vc.VEHICLE_CONDITION_ID) cgb on cgb.vehicle_condition_desc = vc.vehicle_Condition_desc
      and cgb.valuation_vehicle_id = v.valuation_vehicle_id
      and cgb.publication_version_id = pv.publication_version_id
      join SAS_STAGE_VAL..SS_VALUATION_TOOL_DATA ss on ss.VIMSID = v.VIMS_VEHICLE_ID
      left join SAS_STAGE_VAL..CL_VIMSOUTPUT_PUBLISHED fc on fc.VIMSVEHICLEID = v.VIMS_VEHICLE_ID and fc.FORECASTUPDATED = (select max(FORECASTUPDATED) from SAS_STAGE_VAL..CL_VIMSOUTPUT_PUBLISHED) and fc.I=3
      where a.OUTLIER_flag = 'N'
      and d.CALENDARDATE between '%s' and '%s'
      %s
      %s
      %s
      %s
      %s
      and a.PRICED_FLAG = 'Y'
      and vc.VEHICLE_CONDITION_DESC in ('Good','Very Good','Fair','Excellent')
      and a.SALE_PRICE_AMOUNT is not null
      and g.STATE <> 'PR'
      and g.STATE <> 'HI'
      order by SALE_DATE DESC
      ",input$dateRange[1], input$dateRange[2], segmentText(), makeText(), modelText(), yearText(), vimsidText()
    )
  })
  
  
  
  query.values <- reactive ({
    
    sprintf("select v.VIMS_VEHICLE_ID
            ,cast(pv.EFFECTIVE_PRODUCT_VERSION_END_DATE as date) as SALE_DATE
            ,vvv.ADJUSTED_BASE_AV_GOOD_AMOUNT as BASE_VALUATION_AMOUNT
            ,vvv.BASE_RETAIL_AMOUNT as RETAIL
            ,vvv.BASE_UC_FPP_AMOUNT as UCFPP
            ,vvv.BASE_UC_FPP_LOWER_LIMIT_AMOUNT as UCFPP_LOWER
            ,vvv.BASE_UC_FPP_UPPER_LIMIT_AMOUNT as UCFPP_UPPER
            ,case when VVV.FORECAST_BASE_AMOUNT = 0 then null else (vvv.FORECAST_BASE_AMOUNT + (vvv.ADJUSTED_BASE_AV_GOOD_AMOUNT - vvv.BASE_VALUATION_AMOUNT)) end as FORECAST_BASE_AMOUNT
            ,m.FINALFCAST_BASE as FORECAST_RAW_AMOUNT
            ,CASE WHEN FP.FORECAST_PRICE_TYPE_FACTOR_PERCENT=0 THEN NULL ELSE FP.FORECAST_PRICE_TYPE_FACTOR_PERCENT END AS UCFPP_FST
            ,CASE WHEN SR.FORECAST_PRICE_TYPE_FACTOR_PERCENT=0 THEN NULL ELSE SR.FORECAST_PRICE_TYPE_FACTOR_PERCENT END AS SR_FST
            from EDWVEHICLE..VALUATION_VEHICLE_VALUE_CALCULATED_F vvv
            --from EDW_TEST_BACKUPS..VALUATION_VEHICLE_VALUE_CALCULATED_F vvv
            join EDWVEHICLE..VALUATION_VEHICLE_D v using (VALUATION_VEHICLE_ID)
            join EDWVEHICLE..PUBLICATION_VERSION_D pv using (PUBLICATION_VERSION_ID)
            join EDWVEHICLE..DATE_D d on pv.EFFECTIVE_PRODUCT_VERSION_END_DATE = d.CALENDARDATE 
            JOIN EDWVEHICLE..VALUATION_VEHICLE_FACTOR_F FP USING(VALUATION_VEHICLE_ID, PUBLICATION_VERSION_ID, PUBLICATION_TYPE_ID)
            JOIN EDWVEHICLE..VALUATION_VEHICLE_FACTOR_F SR USING(VALUATION_VEHICLE_ID, PUBLICATION_VERSION_ID, PUBLICATION_TYPE_ID)
            left join SAS_STAGE_VAL..BS_MAIN_MODEL_FORECAST m on m.VIMSVEHICLEID = vvv.VIMS_VEHICLE_ID and m.PUBLISH_WEEK between pv.EFFECTIVE_PRODUCT_VERSION_START_DATE and pv.EFFECTIVE_PRODUCT_VERSION_END_DATE
            where
            d.CALENDARDATE between '%s' and '%s'
            %s
            %s
            %s
            %s
            %s
            and pv.PUBLICATION_VERSION_DESC = ('UC Internet')
            and fp.VEHICLE_VALUE_TYPE_ID=6
            AND SR.VEHICLE_VALUE_TYPE_ID=2 
            ORDER BY SALE_DATE DESC
            ",input$dateRange[1], input$dateRange[2], segmentText(), makeText(), modelText(), yearText(),vimsidText()
    )
  })
  
  query.ucfpp <- reactive ({
    
    sprintf("SELECT 
            D.CALENDARDATE AS SALE_DATE
            ,v.VIMS_VEHICLE_ID
            ,V.MODEL_YEAR
            ,V.MAKE_NAME
            ,V.MODEL_NAME
            ,V.VEHICLE_NAME
            ,V.SEGMENT
            ,R.VIN
            ,R.SALE_PRICE_AMOUNT - (R.PRICED_CONFIGURED_FPP_AMOUNT - R.PRICED_BASE_FPP_AMOUNT) AS SALE_PRICE
            ,CASE WHEN SALE_PRICE IS NULL THEN R.SALE_PRICE_AMOUNT ELSE SALE_PRICE END AS RETAIL_PRICE
            FROM EDWVEHICLE..RETAIL_TRANSACTION_F R
            JOIN VALUATION_VEHICLE_D V ON V.VALUATION_VEHICLE_ID = R.VALUATION_VEHICLE_ID
            JOIN VEHICLE_CONDITION_D VC ON VC.VEHICLE_CONDITION_ID = R.PRICED_VEHICLE_CONDITION_ID
            JOIN DATE_D D ON D.DATEID = R.SALE_DATE_ID
            JOIN EDWVEHICLE..VEHICLE_TRANSACTION_TYPE_D VT ON VT.VEHICLE_TRANSACTION_TYPE_ID = R.VEHICLE_TRANSACTION_TYPE_ID
            WHERE
            R.OUTLIER_FLAG = 'N'
            AND R.PRICED_FLAG = 'Y'
            AND VT.VEHICLE_NEW_USED = 'Used'
            AND SALE_DATE BETWEEN '%s' and '%s'
            %s
            %s
            %s
            %s
            %s
            ORDER BY SALE_DATE DESC
            ",input$dateRange[1], input$dateRange[2], segmentText(), makeText(), modelText(), yearText(), vimsidText())
  })
  
  query.srp <- reactive ({
    
    sprintf("select
            d.CALENDARDATE as LIST_DATE
            ,v.VIMS_VEHICLE_ID
            ,v.MODEL_YEAR
            ,v.MAKE_NAME
            ,v.MODEL_NAME
            ,v.VEHICLE_NAME
            ,v.SEGMENT
            ,vauto.VIN
            ,vauto.certified_flag
            ,vauto.AD_LIST_PRICE_AMOUNT as LISTINGS_PRICE
            ,vauto.AD_LIST_PRICE_AMOUNT - (vauto.PRICED_CONFIGURED_RETAIL_ASKING_AMOUNT - vauto.PRICED_BASE_RETAIL_ASKING_AMOUNT) as BASE_LISTINGS_PRICE
            
            from edwvehicle..V_VAUTO_LISTING_INVENTORY_F vauto
            join edwvehicle..VALUATION_VEHICLE_D v on v.VALUATION_VEHICLE_ID = vauto.VALUATION_VEHICLE_ID
            join edwvehicle..VEHICLE_TRANSACTION_TYPE_D vtt on vtt.VEHICLE_TRANSACTION_TYPE_ID = vauto.VEHICLE_TRANSACTION_TYPE_ID
            join edwvehicle..DATE_D d on d.DATEID = vauto.AD_LAST_MODIFIED_DATE_ID
            
            where 
            vtt.VEHICLE_NEW_USED = 'Used'
            and v.VALUATION_VEHICLE_ID <> 0
            and vauto.AD_LIST_PRICE_AMOUNT is not null
            and vauto.PRICED_FLAG = 'Y'
            and vauto.AD_LIST_PRICE_AMOUNT between 200 and (2 * vauto.PRICED_MSRP_AMOUNT)
            and vauto.MILEAGE between 200 and 300000
            and vauto.MILEAGE <> 0
            and vauto.ENGINE_IS_UNCERTAIN_FLAG = 'N'
            and vauto.TRANSMISSION_IS_UNCERTAIN_FLAG = 'N'
            and vauto.DRIVETRAIN_IS_UNCERTAIN_FLAG = 'N'
            and LIST_DATE between '%s' and '%s'
            %s
            %s
            %s
            %s
            %s
            order by LIST_DATE DESC
            ",input$dateRange[1], input$dateRange[2], segmentText(), makeText(), modelText(), yearText(), vimsidText())
  })
  
  
  # Bias queries
  av.bias.query <- reactive({
    withProgress(message = loadingMessages(), value = 0, {
      if(input$usedButton){
        setProgress(value = 1)
        print(paste("AV bias query beginning... ", Sys.time()))
        data <- sqlQuery(channel, isolate(sprintf("SELECT
                                                  b.PUBLISH_WEEK
                                                  ,b.VIMSVEHICLEID as VIMSID
                                                  ,b.COUNT_4WK as CNT_4WK
                                                  ,round(b.AV_BIAS_4WK, 2) as BIAS_4WK
                                                  ,round(b.FCAST_BIAS_4WK, 2) as M_BIAS_4Wk
                                                  ,round(b.AV_DLR_BIAS_4WK,0) as BIAS_$_4WK
                                                  ,round(b.FCAST_DLR_BIAS_4WK, 0) as M_BIAS_$_4WK
                                                  ,b.COUNT_CW as CNT_CW
                                                  ,round(b.AV_BIAS_CW, 2) as BIAS_CW
                                                  ,round(b.FCAST_BIAS_CW, 2) as M_BIAS_CW
                                                  ,round(b.AV_DLR_BIAS_CW, 0) as BIAS_$_CW
                                                  ,round(b.FCAST_DLR_BIAS_CW, 0) as M_BIAS_$_CW
                                                  FROM
                                                  SAS_STAGE_VAL..BS_BIAS_BY_VEHICLEID b
                                                  ORDER BY
                                                  b.PUBLISH_WEEK desc")), believeNRows = FALSE)
        print(paste("AV bias query complete!    ", Sys.time()))
        data$PUBLISH_WEEK <- as.Date(data$PUBLISH_WEEK)
        invisible(data)
      }
    })
  })  
  
  ucfpp.bias.query <- reactive({
    withProgress(message = loadingMessages(), value = 0, {
      if(input$usedButton){
        setProgress(value = 1)
        print(paste("UCFPP bias query beginning... ", Sys.time()))
        data <- sqlQuery(channel, isolate(sprintf("SELECT
                                                  bf.publish_week
                                                  ,bf.VIMSVEHICLEID as VIMSID
                                                  ,bf.COUNT_4WK
                                                  ,round(bf.PUBLISHED_BIAS_4WK, 2) as BIAS_4WK
                                                  ,round(bf.FCAST_BIAS_4WK, 2) as M_BIAS_4wk
                                                  ,round(bf.PUBLISHED_DLR_BIAS_4WK, 0) as BIAS_$_4WK
                                                  ,round(bf.FCAST_DLR_BIAS_4WK, 0) as M_BIAS_$_4WK
                                                  ,bf.COUNT_CW
                                                  ,round(bf.PUBLISHED_BIAS_CW,2) as BIAS_CW
                                                  ,round(bf.FCAST_BIAS_CW, 2) as M_BIAS_CW
                                                  ,round(bf.PUBLISHED_DLR_BIAS_CW, 0) as BIAS_$_CW
                                                  ,round(bf.FCAST_DLR_BIAS_CW, 0) as M_BIAS_$_CW
                                                  FROM SAS_STAGE_VAL..BS_FPP_BIAS_BY_VEHICLEID bf
                                                  order by bf.PUBLISH_WEEK desc")), believeNRows = FALSE)
        print(paste("UCFPP bias query complete!    ", Sys.time()))
        data$PUBLISH_WEEK <- as.Date(data$PUBLISH_WEEK)
        invisible(data)
      }
    })
  })
  
  
  srp.bias.query <- reactive({
    withProgress(message = loadingMessages(), value = 0, {
      if(input$usedButton){
        setProgress(value = 1)
        print(paste("SRP bias query beginning... ", Sys.time()))
        data <- sqlQuery(channel, isolate(sprintf("SELECT
                                                  bs.publish_week
                                                  ,bs.VIMSVEHICLEID as VIMSID
                                                  ,bs.COUNT_4WK
                                                  ,round(bs.PUBLISHED_BIAS_4WK, 2) as BIAS_4WK
                                                  ,round(bs.FCAST_BIAS_4WK, 2) as M_BIAS_4wk
                                                  ,round(bs.PUBLISHED_DLR_BIAS_4WK, 0) as BIAS_$_4WK
                                                  ,round(bs.FCAST_DLR_BIAS_4WK, 0) as M_BIAS_$_4WK
                                                  ,bs.COUNT_CW
                                                  ,round(bs.PUBLISHED_BIAS_CW,2) as BIAS_CW
                                                  ,round(bs.FCAST_BIAS_CW, 2) as M_BIAS_CW
                                                  ,round(bs.PUBLISHED_DLR_BIAS_CW, 0) as BIAS_$_CW
                                                  ,round(bs.FCAST_DLR_BIAS_CW, 0) as M_BIAS_$_CW
                                                  FROM SAS_STAGE_VAL..BS_SRP_BIAS_BY_VEHICLEID bs
                                                  order by bs.PUBLISH_WEEK desc")), believeNRows = FALSE)
        print(paste("SRP bias query complete!    ", Sys.time()))
        data$PUBLISH_WEEK <- as.Date(data$PUBLISH_WEEK)
        invisible(data)
      }
    })                        
  })
  
  
  
  # Value Boxes on the homescreen
  output$nrowUsedBox <- renderValueBox({ 
    rows <- rowCountUsed()
    if(rows == 0) boxColor <- "red" else boxColor <- "green"
    if(rows == 0) boxIcon <- "ban" else boxIcon <- "thumbs-o-up"
    valueBox(
      rows, "Auction Transactions", icon = icon(boxIcon), color = boxColor
    )
  })
  
  output$nrowUCFPPBox <- renderValueBox({
    rows <- rowCountUCFPP()
    if(rows == 0) boxColor <- "red" else boxColor <- "green"
    if(rows == 0) boxIcon <-"ban" else boxIcon <- "thumbs-o-up"
    valueBox(
      rows, "UCFPP Transactions", icon = icon(boxIcon), color = boxColor
    )
  })
  
  output$nrowSRPBox <- renderValueBox({
    rows <- rowCountSRP()
    if(rows == 0) boxColor <- "red" else boxColor <- "green"
    if(rows == 0) boxIcon <-"ban" else boxIcon <- "thumbs-o-up"
    valueBox(
      rows, "Retail Listings", icon = icon(boxIcon), color = boxColor
    )
  })
  
  output$nrowMassBox <- renderValueBox({
    rows <- rowCountMass()
    if(rows == 0) boxColor <- "red" else boxColor <- "green"
    if(rows == 0) boxIcon <-"ban" else boxIcon <- "thumbs-o-up"
    valueBox(
      rows, "Mass Scatters", icon = icon(boxIcon), color = boxColor
    )
  })
  
  rowCountUsed <- reactive({
    rows <- nrow(data.used())
    if(is.null(rows)) rows <- 0
    format(rows, big.mark = ",")
  })
  
  rowCountUCFPP <- reactive({
    rows <- nrow(data.ucfpp())
    if(is.null(rows)) rows <- 0
    format(rows, big.mark = ",")
  })
  
  rowCountSRP <- reactive({
    rows <- nrow(data.srp())
    if(is.null(rows)) rows <- 0
    format(rows, big.mark = ",")
  })
  
  rowCountMass <- reactive({
    if(is.null(data.used.mass())){
      rows <- 0
    } else {
      rows <- nrow(data.used.mass() %>% distinct(VIMS_VEHICLE_ID, .keep_all = TRUE))
    }
    #rows <- nrow(data.used.mass() %>% distinct(VIMS_VEHICLE_ID))
    #if(is.null(rows)) rows <- 0
    data.values.mass()
    format(rows, big.mark = ",")
  })
  
  # Query AV transactions
  data.used <- reactive({
    withProgress(message = loadingMessages(), value = 0, {
      if(input$usedButton >= 1) {
        setProgress(value = 1)
        check.connect(channel)
        print(paste("AV query beginning... ", Sys.time()))
        data <- sqlQuery(channel, isolate(query.used()), believeNRows = FALSE)
        print(paste("AV query complete!    ", Sys.time()))
        #str(data)
        data$SALE_DATE <- as.Date(data$SALE_DATE)
        invisible(data)
      }
    })
  })
  
  data.used.mass <- reactive({
    withProgress(message = loadingMessages(), value = 0, {
      if(input$mass.scatters.button >= 1){
        setProgress(value = 1)
        check.connect(channel)
        print(paste("Mass Scatters query beginning... ", Sys.time()))
        data <- sqlQuery(channel, isolate(query.used()), believeNRows = FALSE)
        print(paste("Mass Scatters Ready!    ", Sys.time()))
        data$SALE_DATE <- as.Date(data$SALE_DATE)
        #str(data)
        invisible(data)
      }
    })
  })
  
  # Query KBB values
  data.values <- reactive ({
    if(input$usedButton) {
      check.connect(channel)
      print(paste("KBB Query beginning... ", Sys.time()))
      data <- sqlQuery(channel, isolate(query.values()), believeNRows = FALSE)
      print(paste("KBB Query complete!    ", Sys.time()))
      #str(data)
      data$SALE_DATE <- as.Date(data$SALE_DATE)
      invisible(data)
    }
  })
  
  data.values.mass <- reactive ({
    withProgress(message = loadingMessages(), value = 0, {
      if(input$mass.scatters.button){
        setProgress(value = 1)
        check.connect(channel)
        print(paste("Mass KBB Query beginning... ", Sys.time()))
        data <- sqlQuery(channel, isolate(query.values()), believeNRows = FALSE)
        print(paste("Mass KBB Query complete!    ", Sys.time()))
        #str(data)
        data$SALE_DATE <- as.Date(data$SALE_DATE)
        invisible(data)
      }
    })
  })
  
  
  # Query UCFPP transactions
  data.ucfpp <- reactive({
    if(input$usedButton & input$getUCFPP) {
      check.connect(channel)
      print(paste("UCFPP query beginning... ", Sys.time()))
      data <- sqlQuery(channel, isolate(query.ucfpp()), believeNRows = FALSE)
      print(paste("UCFPP query complete!    ", Sys.time()))
      #str(data)
      data$SALE_DATE <- as.Date(data$SALE_DATE)
      invisible(data)
    }
  })
  
  
  # Query Retail listings
  data.srp <- reactive({
    if(input$usedButton & input$getListings) {
      check.connect(channel)
      print(paste("Retail query beginning... ", Sys.time()))
      data <- sqlQuery(channel, isolate(query.srp()), believeNRows = FALSE)
      print(paste("Retail query complete!    ", Sys.time()))
      #str(data)
      data$LIST_DATE <- as.Date(data$LIST_DATE)
      invisible(data)
    }
  })
  
  
  
  # UI Outputs for scatter
  output$year.used <- renderUI({
    # If no data, initialize with current year
    if (rowCountUsed() == 0)
      initial <- as.numeric(format(Sys.Date(), "%Y"))
    else
      initial <- max(data.used()$MODEL_YEAR)
    year.list <- data.used() %>%
      group_by(MODEL_YEAR) %>%
      distinct(MODEL_YEAR, .keep_all = TRUE) %>%
      select(MODEL_YEAR) %>%
      arrange(MODEL_YEAR, desc(as.numeric(MODEL_YEAR)))
    selectInput("year.used", "Year: ", c("",year.list$MODEL_YEAR), selected = initial)
    #numericInput("year.used", "Year: ", initial, step = 1)
  })
  
  output$make.used <- renderUI({
    req(input$year.used)
    make.list <- data.used() %>%
      filter(MODEL_YEAR == input$year.used) %>%
      group_by(MAKE_NAME) %>%
      distinct(MAKE_NAME, .keep_all = TRUE) %>%
      select(MAKE_NAME) %>%
      arrange(MAKE_NAME)
    selectInput("make.used", "Make:", as.character(make.list$MAKE_NAME), selected = input$make.used)
  })
  
  output$model.used <- renderUI({
    req(input$make.used)
    model.list <- data.used() %>%
      filter(MODEL_YEAR == input$year.used, MAKE_NAME == input$make.used) %>%
      group_by(MODEL_NAME) %>%
      distinct(MODEL_NAME, .keep_all = TRUE) %>%
      select(MODEL_NAME) %>%
      arrange(MODEL_NAME)
    selectInput("model.used", "Model:", as.character(model.list$MODEL_NAME), selected = input$model.used)
  })
  
  output$trim.used <- renderUI({
    req(input$model.used)
    trim.list <- data.used() %>%
      filter(MODEL_YEAR == input$year.used, MAKE_NAME == input$make.used, MODEL_NAME == input$model.used)  %>%
      group_by(VEHICLE_NAME) %>%
      distinct(VEHICLE_NAME, .keep_all = TRUE) %>%
      select(VEHICLE_NAME) %>%
      arrange(VEHICLE_NAME)
    selectInput("trim.used", "Trim:", as.character(trim.list$VEHICLE_NAME), selected = input$trim.used)
  })
  
  output$engine.used <- renderUI({
    if(input$engine){
      req(input$trim.used)
      engine.list <- data.used() %>%
        filter(MODEL_YEAR == input$year.used, MAKE_NAME == input$make.used, MODEL_NAME == input$model.used, VEHICLE_NAME == input$trim.used) %>%
        group_by(ENGINE_DESCRIPTION) %>%
        distinct(ENGINE_DESCRIPTION, .keep_all = TRUE) %>%
        select(ENGINE_DESCRIPTION) %>%
        arrange(ENGINE_DESCRIPTION)
      selectInput("engine.used", "Engine:", as.character(engine.list$ENGINE_DESCRIPTION), multiple = TRUE)
    } else {
      NULL
    }
    
  })
  
  # VIMSID UI Output - Not working correctly
  # output$vimsid.ui.used <- renderUI({
  #   req(input$trim.used)
  #   vimsid.list <- data.used() %>%
  #     group_by(VIMS_VEHICLE_ID) %>%
  #     distinct(VIMS_VEHICLE_ID) %>%
  #     select(VIMS_VEHICLE_ID) %>%
  #     arrange(VIMS_VEHICLE_ID)
  #   selectInput("vimsid.ui.used", "VIMSID:", c("",as.character(vimsid.list$VIMS_VEHICLE_ID)), selected = "")
  # })
  
  output$vehicleMap.used <- renderText({
    req(input$trim.used)
    map.list <- data.used() %>%
      filter(MODEL_YEAR == input$year.used, MAKE_NAME == input$make.used, MODEL_NAME == input$model.used, VEHICLE_NAME == input$trim.used)  %>%
      group_by(VEHICLE_MAP) %>%
      distinct(VEHICLE_MAP, .keep_all = TRUE) %>%
      select(VEHICLE_MAP) %>%
      arrange(VEHICLE_MAP)
    paste(map.list$VEHICLE_MAP)
  })
  
  output$vimsid.used <- renderText({
    req(input$trim.used)
    vimsid.list <- data.used() %>%
      filter(MODEL_YEAR == input$year.used, MAKE_NAME == input$make.used, MODEL_NAME == input$model.used, VEHICLE_NAME == input$trim.used) %>%
      group_by(VIMS_VEHICLE_ID) %>%
      distinct(VIMS_VEHICLE_ID, .keep_all = TRUE) %>%
      select(VIMS_VEHICLE_ID) %>%
      arrange(VIMS_VEHICLE_ID)
    paste(vimsid.list$VIMS_VEHICLE_ID)
  })
  
  #Comparison Year UIOutput
  output$year.used.comparison1 <- renderUI({
    # If no data, initialize with current year
    if(rowCountUsed() == 0) 
      initial <- as.numeric(format(Sys.Date(), "%Y")) 
    else 
      initial <- max(data.used()$MODEL_YEAR)
    selectInput("year.used.comparison1", "Year: ", c("", as.character(year.list$MODEL_YEAR)), selected = initial)
  })
  
  output$year.used.comparison2 <- renderUI({
    # If no data, initialize with current year
    if(rowCountUsed() == 0) 
      initial <- as.numeric(format(Sys.Date(), "%Y")) 
    else 
      initial <- max(data.used()$MODEL_YEAR)
    selectInput("year.used.comparison2", "Year: ", c("", as.character(year.list$MODEL_YEAR)), selected = initial)
  })
  
  #Comparison Make UIOutput
  output$make.used.comparison1 <- renderUI({
    make.list <- data.used() %>%
      filter(MODEL_YEAR == input$year.used.comparison1) %>%
      group_by(MAKE_NAME) %>%
      distinct(MAKE_NAME, .keep_all = TRUE) %>%
      select(MAKE_NAME) %>%
      arrange(MAKE_NAME)
    selectInput("make.used.comparison1", "Make:", as.character(make.list$MAKE_NAME), selected = input$make.used)
  })
  
  output$make.used.comparison2 <- renderUI({
    make.list <- data.used() %>%
      filter(MODEL_YEAR == input$year.used.comparison2) %>%
      group_by(MAKE_NAME) %>%
      distinct(MAKE_NAME, .keep_all = TRUE) %>%
      select(MAKE_NAME) %>%
      arrange(MAKE_NAME)
    selectInput("make.used.comparison2", "Make:", as.character(make.list$MAKE_NAME))
  })
  
  #Comparison Model UIOutput
  output$model.used.comparison1 <- renderUI({
    model.list <- data.used() %>%
      filter(MODEL_YEAR == input$year.used.comparison1, MAKE_NAME == input$make.used.comparison1) %>%
      group_by(MODEL_NAME) %>%
      distinct(MODEL_NAME, .keep_all = TRUE) %>%
      select(MODEL_NAME) %>%
      arrange(MODEL_NAME)
    selectInput("model.used.comparison1", "Model:", as.character(model.list$MODEL_NAME), selected = input$model.used.comparison1)
  })
  
  output$model.used.comparison2 <- renderUI({
    model.list <- data.used() %>%
      filter(MODEL_YEAR == input$year.used.comparison2, MAKE_NAME == input$make.used.comparison2) %>%
      group_by(MODEL_NAME) %>%
      distinct(MODEL_NAME, .keep_all = TRUE) %>%
      select(MODEL_NAME) %>%
      arrange(MODEL_NAME)
    selectInput("model.used.comparison2", "Model:", as.character(model.list$MODEL_NAME))
  })
  
  #Comparison Trim UIoutput
  output$trim.used.comparison1 <- renderUI({
    trim.list <- data.used() %>%
      filter(MODEL_YEAR == input$year.used.comparison1, MAKE_NAME == input$make.used.comparison1, MODEL_NAME == input$model.used.comparison1)  %>%
      group_by(VEHICLE_NAME) %>%
      distinct(VEHICLE_NAME, .keep_all = TRUE) %>%
      select(VEHICLE_NAME) %>%
      arrange(VEHICLE_NAME)
    selectInput("trim.used.comparison1", "Trim:", as.character(trim.list$VEHICLE_NAME), selected = input$trim.used.comparison1)
  })
  
  output$trim.used.comparison2 <- renderUI({
    trim.list <- data.used() %>%
      filter(MODEL_YEAR == input$year.used.comparison2, MAKE_NAME == input$make.used.comparison2, MODEL_NAME == input$model.used.comparison2)  %>%
      group_by(VEHICLE_NAME) %>%
      distinct(VEHICLE_NAME, .keep_all = TRUE) %>%
      select(VEHICLE_NAME) %>%
      arrange(VEHICLE_NAME)
    selectInput("trim.used.comparison2", "Trim:", as.character(trim.list$VEHICLE_NAME))
  })
  
  output$dates.used.comparison <- renderUI({
    dateRangeInput("inDates1.comparison", "Date range:", start = input$dateRange[1], end = input$dateRange[2], min = input$dateRange[1], max = input$dateRange[2])
  })
  
  output$dates.used <- renderUI({
    req(input$dateRange)
    dateRangeInput("inDates1", "Date range:", start = input$dateRange[1], end = input$dateRange[2], min = input$dateRange[1], max = input$dateRange[2])
  })
  
  # Clean up AV transactions
  data.used.sub <- reactive ({
    if(input$engine){
      m <- data.used() %>%
        filter(MODEL_YEAR == input$year.used, 
               MAKE_NAME == input$make.used, 
               MODEL_NAME == input$model.used, 
               VEHICLE_NAME == input$trim.used,
               ENGINE_DESCRIPTION %in% input$engine.used,
               SALE_DATE >= input$inDates1[1],
               SALE_DATE <= input$inDates1[2])
      return(m)
    } else {
      m <- data.used() %>%
        filter(MODEL_YEAR == input$year.used, 
               MAKE_NAME == input$make.used, 
               MODEL_NAME == input$model.used, 
               VEHICLE_NAME == input$trim.used,
               #ENGINE_DESCRIPTION == input$engine.used,
               SALE_DATE >= input$inDates1[1],
               SALE_DATE <= input$inDates1[2])
      return(m)
    }
  })
  
  # Clean up AV transactions 1 for comparison
  data.used.sub.comparison1 <- reactive ({
    m <- data.used() %>%
      filter(MODEL_YEAR == input$year.used.comparison1, 
             MAKE_NAME == input$make.used.comparison1, 
             MODEL_NAME == input$model.used.comparison1, 
             VEHICLE_NAME == input$trim.used.comparison1,
             SALE_DATE >= input$inDates1.comparison[1],
             SALE_DATE <= input$inDates1.comparison[2])
    return(m)
  })
  
  # Clean up AV transactions 2 for comparison
  data.used.sub.comparison2 <- reactive ({
    m <- data.used() %>%
      filter(MODEL_YEAR == input$year.used.comparison2, 
             MAKE_NAME == input$make.used.comparison2, 
             MODEL_NAME == input$model.used.comparison2, 
             VEHICLE_NAME == input$trim.used.comparison2,
             SALE_DATE >= input$inDates1.comparison[1],
             SALE_DATE <= input$inDates1.comparison[2])
    return(m)
  })
  
  # Clean up UCFPP transactions
  data.ucfpp.sub <- reactive ({
    m <- data.ucfpp() %>%
      filter(MODEL_YEAR == input$year.used, 
             MAKE_NAME == input$make.used, 
             MODEL_NAME == input$model.used, 
             VEHICLE_NAME == input$trim.used,
             #VIMS_VEHICLE_ID == input$vimsid.ui.used,
             SALE_DATE >= input$inDates1[1],
             SALE_DATE <= input$inDates1[2])
    return(m)
  })
  
  # Clean up Retail Listings
  data.srp.sub <- reactive ({
    m <- data.srp() %>%
      filter(MODEL_YEAR == input$year.used, 
             MAKE_NAME == input$make.used, 
             MODEL_NAME == input$model.used, 
             VEHICLE_NAME == input$trim.used,
             #VIMS_VEHICLE_ID == input$vimsid.ui.used,
             LIST_DATE >= input$inDates1[1],
             LIST_DATE <= input$inDates1[2])
    if (input$certified == FALSE) {
      m <- m %>%
        filter(CERTIFIED_FLAG == 0)
    }
    return(m)
  })
  
  # Clean up KBB values
  data.values.sub <- reactive ({
    vvid <- data.used.sub()$VIMS_VEHICLE_ID[1]
    m <- data.values() %>%
      filter(VIMS_VEHICLE_ID == vvid,
             SALE_DATE >= input$inDates1[1],
             SALE_DATE <= input$inDates1[2])
    return(m)
  })
  
  # Clean up KBB values for comparison 1
  data.values.sub.comparison1 <- reactive ({
    vvid <- data.used.sub.comparison1()$VIMS_VEHICLE_ID[1]
    m <- data.values() %>%
      filter(VIMS_VEHICLE_ID == vvid,
             SALE_DATE >= input$inDates1.comparison[1],
             SALE_DATE <= input$inDates1.comparison[2])
    return(m)
  })
  
  # Clean up KBB values for comparison 2
  data.values.sub.comparison2 <- reactive ({
    vvid <- data.used.sub.comparison2()$VIMS_VEHICLE_ID[1]
    m <- data.values() %>%
      filter(VIMS_VEHICLE_ID == vvid,
             SALE_DATE >= input$inDates1.comparison[1],
             SALE_DATE <= input$inDates1.comparison[2])
    return(m)
  })
  
  # Fill plots
  # fillTextUsed <- reactive({
  #   if(input$fill.used) paste("black")
  #   else NA 
  # })
  
  # VIN color
  vin.color <- reactive({
    if (input$plotType == "avplot") {
      if (input$vin.used %in% data.used.sub()$VIN) {
        paste("purple")
      } else NA
    }
    else if (input$plotType == "ucfppplot") {
      if (input$vin.used %in% data.ucfpp.sub()$VIN) {
        paste("purple")
      } else NA
    }
    else if (input$plotType == "retailplot") {
      if (input$vin.used %in% data.srp.sub()$VIN) {
        paste("purple")
      } else NA
    }
    # if(input$vin.used %in% data.used.sub()$VIN || input$vin.used %in% data.srp.sub()$VIN || input$vin.used %in% data.ucfpp.sub()$VIN){
    #   paste("purple")
    # } else {NA}
  })
  
  # VIN Highlight data
  vin.highlight <- reactive({
    if (input$plotType == "avplot"){
      if (input$vin.used %in% data.used.sub()$VIN) {
        subset(data.used.sub(), VIN == input$vin.used)
      } else {
        NULL
      }
    }
    else if (input$plotType == "ucfppplot"){
      if (input$vin.used %in% data.ucfpp.sub()$VIN) {
        subset(data.ucfpp.sub(), VIN == input$vin.used)
      } else {
        NULL
      }
    }
    else if (input$plotType == "retailplot"){
      if (input$vin.used %in% data.srp.sub()$VIN) {
        subset(data.srp.sub(), VIN == input$vin.used)
      } else {
        NULL
      }
    }
    # if(input$vin.used %in% data.used.sub()$VIN){
    #   subset(data.used.sub(), VIN == input$vin.used)
    # } else if (input$vin.used %in% data.srp.sub()$VIN){
    #   subset(data.srp.sub(), VIN == input$vin.used)
    # } else if (input$vin.used %in% data.ucfpp.sub()$VIN){
    #   subset(data.ucfpp.sub(), VIN == input$vin.used)
    # } else {
    #   NULL
    # }
  })
  
  # VIN text
  output$vin.used.text <- renderText({
    if(input$vin.used.checkbox && input$vin.used != "") {
      if (input$plotType == "avplot") {
        if (input$vin.used %in% data.used.sub()$VIN) {
          paste("VIN exists in AV data")
        } else {
          paste("VIN doesn't exist in AV data")
        }
      }
      else if (input$plotType == "ucfppplot") {
        if (input$vin.used %in% data.ucfpp.sub()$VIN) {
          paste("VIN exists in UCFPP data")
        } else {
          paste("VIN doesn't exist in UCFPP data")
        }
      }
      else if (input$plotType == "retailplot") {
        if (input$vin.used %in% data.srp.sub()$VIN) {
          paste("VIN exists in Retail data")
        } else {
          paste("VIN doesn't exist in Retail data")
        }
      }
    }
  })
  
  # Over under highlight
  over.under.highlight <- reactive({
    cnt <- data.used.sub()$MNHM_COUNT[1]
    kbb <- data.values.sub()$BASE_VALUATION_AMOUNT[1]
    subset(data.used.sub(), kbb >= SALE_PRICE_BASE | kbb < SALE_PRICE_BASE) %>% top_n(cnt, wt=SALE_DATE)
  })
  
  # Mileage Filter
  mileage.filter <- reactive({
    min.mileage <- input$mileage.filter.main - input$mileage.filter.plusMinus
    max.mileage <- input$mileage.filter.main + input$mileage.filter.plusMinus
    if (input$mileage.filter.main != "") {
      subset(data.used.sub(), MILEAGE <= min.mileage | MILEAGE >= max.mileage)
    }
  })
  
  #USED SCATTER PLOTLY OUTPUT
  plotInputUsed.plotly <- reactive({
    req(input$trim.used)
    if(input$plotType == "avplot") {
      p <- ggplot() + fte_theme() + geom_point(data=data.used.sub(),aes(SALE_DATE,SALE_PRICE_BASE),size=input$size.used) + ggtitle(paste(input$year.used,input$make.used,input$model.used,input$trim.used,sep=" ")) + labs(x = "Sale Date", y = "Auction Value") + scale_y_continuous(labels = comma) #theme(plot.title=element_text(face="bold")) + 
      if(input$engine)
        p <- p + geom_point(data=data.used.sub(), aes(SALE_DATE, SALE_PRICE_BASE,color=ENGINE_DESCRIPTION), size=input$size.used)
      if(input$buckets.used)
        p <- p + geom_point(data=over.under.highlight(), aes(SALE_DATE, SALE_PRICE_BASE), color="#fa9fb5", size=input$size.used)# + geom_point(data=over.under.highlight()[2], aes(SALE_DATE, SALE_PRICE_BASE), color="#fee8c8", size=input$size.used, shape=21, fill=fillTextUsed())
      if(input$mileage.filter)
        p <- p + geom_point(data=data.used.sub(), aes(col = MILEAGE), size = input$size.used)# + scale_colour_gradientn(colours = c("red", "violet", "blue")) #color = "#A86F59"
      if(input$kbb.used)
        p <- p + geom_line(data=data.values.sub(),aes(SALE_DATE,BASE_VALUATION_AMOUNT),color="blue",size=1)
      if(input$forecast.used)
        p <- p + geom_line(data=data.values.sub(), aes(SALE_DATE,FORECAST_BASE_AMOUNT),color="orange",size=1)
      if(input$forecast.raw.used)
        p <- p + geom_line(data=data.values.sub(), aes(SALE_DATE,FORECAST_RAW_AMOUNT),color="orange",size=1, linetype="longdash")
      if(input$smooth.used)
        p <- p + geom_smooth(data=data.used.sub(), aes(SALE_DATE, SALE_PRICE_BASE), se=FALSE,colour="red",size=1)
      if(input$weekly.median.used)
        p <- p + geom_point(data=data.used.sub(), aes(FORECAST_DATE,MNHM_PRICE), color="#2ca25f", size=3, shape=19)
      #p <- p + geom_hline(yintercept=data.used.sub()$MNHM_PRICE,color="blue", size=0.75, linetype=4) + geom_hline(yintercept=data.used.sub()$FORECAST, size=0.75, color="orange", linetype=4)
      if(input$weekly.forecast.used)
        p <- p + geom_point(data=data.used.sub(), aes(FORECAST_DATE,FORECAST), color="orange", size=3, shape=19)
      #if(input$bias.marker)
        #p <- p + geom_vline(data=av.bias.query.sub(), aes(xintercept = PUBLISH_WEEK))
      if(input$vin.used.checkbox)
        p <- p + geom_point(data = vin.highlight(), aes(SALE_DATE, SALE_PRICE_BASE), colour = vin.color(), size = 3, shape = 18)
    } else if(input$plotType == "rawavplot") {
      p <- ggplot(data.used.sub(),aes(SALE_DATE, SALE_PRICE_AMOUNT)) + theme_gray() + geom_point(size=input$size.used) + ggtitle(paste(input$year.used,input$make.used,input$model.used,input$trim.used,sep=" ")) + theme(plot.title=element_text(face="bold")) + labs(x = "Sale Date", y = "Auction Value")
      # if(input$yaxis.used)
      #   p <- p + scale_y_continuous(limits = c(input$ymin.used, input$ymax.used))
      if(input$kbb.used)
        p <- p + geom_line(data=data.values.sub(),aes(SALE_DATE,BASE_VALUATION_AMOUNT),color="blue",size=1)
      if(input$forecast.used)
        p <- p + geom_line(data=data.values.sub(), aes(SALE_DATE,FORECAST_BASE_AMOUNT),color="orange",size=1)
      if(input$smooth.used)
        p <- p + geom_smooth(se=FALSE,colour="red",size=1)
      if(input$vin.used.checkbox)
        p <- p + geom_point(data = vin.highlight(), aes(SALE_DATE, SALE_PRICE_AMOUNT), colour = vin.color(), size = 3, shape = 18)
    } else if (input$plotType == 'mileageplot') {
      p <- ggplot(data.used.sub(),aes(MILEAGE,SALE_PRICE_AMOUNT)) + fte_theme() + geom_point(size=input$size.used) + scale_x_continuous(name = "Mileage") + scale_y_continuous(name="Sale Price", label=comma) + ggtitle(paste(input$year.used,input$make.used,input$model.used,input$trim.used,sep=" "))
      # if(input$yaxis.used)
      #   p <- p + scale_y_continuous(limits = c(input$ymin.used, input$ymax.used), name = "Sale Price")
      # if(input$xaxis.used)
      #   p <- p + scale_x_continuous(limits = c(input$xmin.used, input$xmax.used), name = "Mileage")
      if(input$smooth.used)
        p <- p + stat_smooth(se=FALSE,colour="red",size=1)
    } else if (input$plotType == 'ucfppplot') {
      p <- ggplot(data.ucfpp.sub(),aes(SALE_DATE,RETAIL_PRICE)) + theme_gray() + geom_point(size=input$size.used) + scale_x_date(name="Sale Date") + scale_y_continuous(name="UCFPP") + ggtitle(paste(input$year.used,input$make.used,input$model.used,input$trim.used,sep=" ")) + theme(plot.title=element_text(face="bold"))
      # if(input$yaxis.used)
      #   p <- p + scale_y_continuous(limits = c(input$ymin.used, input$ymax.used), name = "UCFPP")
      if(input$kbb.used)
        p <- p + geom_line(data=data.values.sub(), aes(SALE_DATE,UCFPP),color="blue",size=1) + geom_line(data=data.values.sub(),aes(SALE_DATE,UCFPP_LOWER), color="blue", size = 1, linetype = "longdash") + geom_line(data=data.values.sub(),aes(SALE_DATE, UCFPP_UPPER), color = "blue", size = 1, linetype = "longdash")
      if(input$forecast.used)
        p <- p + geom_line(data=data.values.sub(), aes(SALE_DATE,UCFPP_FST),color="orange",size=1)
      if(input$smooth.used)
        p <- p + geom_smooth(se=FALSE,colour="red",size=1)
      if(input$vin.used.checkbox)
        p <- p + geom_point(data = vin.highlight(), aes(SALE_DATE, RETAIL_PRICE), colour = vin.color(), size = 3, shape = 18)
    } else if (input$plotType == 'retailplot') {
      p <- ggplot(data.srp.sub(), aes(LIST_DATE, BASE_LISTINGS_PRICE)) + theme_gray() + geom_point(size=input$size.used) + scale_x_date(name="List Date") + scale_y_continuous(name="List Price") + ggtitle(paste(input$year.used,input$make.used,input$model.used,input$trim.used,sep=" ")) + theme(plot.title=element_text(face="bold"))
      # if(input$yaxis.used)
      #   p <- p + scale_y_continuous(limits = c(input$ymin.used, input$ymax.used), name = "List Price")
      if(input$kbb.used)
        p <- p + geom_line(data=data.values.sub(), aes(SALE_DATE,RETAIL),color="blue",size=1)
      if(input$forecast.used)
        p <- p + geom_line(data=data.values.sub(), aes(SALE_DATE,SR_FST),color="orange",size=1)
      if(input$smooth.used)
        p <- p + geom_smooth(se=FALSE,colour="red",size=1)
      if(input$vin.used.checkbox)
        p <- p + geom_point(data = vin.highlight(), aes(LIST_DATE, BASE_LISTINGS_PRICE), colour = vin.color(), size = 3, shape = 18)
    }
    if (names(dev.cur()) != "null device") dev.off()
    return(ggplotly(p))
  })
  
  # Plotly Output
  output$plot.used <- renderPlotly({
    withProgress(message = loadingMessages(), value = 0, {
      setProgress(value = 1)
      print(plotInputUsed.plotly())
    })
  })
  # Mass Scatters output
  mass.scatters.input <- reactive({
    #if(input$plotType == "avplot") {
    p <- ggplot(data.used.mass(),aes(SALE_DATE,SALE_PRICE_BASE)) + theme_gray() + geom_point()+ labs(x = "Sale Date", y = "Auction Value") + scale_x_date(date_labels = "%b %y", date_breaks = "1 month") + scale_y_continuous(labels = dollar) + # + ggtitle(paste(data.used.mass$MODEL_YEAR, data.used.mass$MAKE_NAME, data.used.mass$MODEL_NAME, data.used.mass$VEHICLE_NAME, sep=" ")) + theme(plot.title=element_text(face="bold")) +
      facet_wrap( ~ VIMS_VEHICLE_ID, scales = "free", ncol = 4)
    if('kbb' %in% input$scatter.options)
      p <- p + geom_line(data=data.values.mass(),aes(SALE_DATE,BASE_VALUATION_AMOUNT),color="blue",size=1)
    if('fc' %in% input$scatter.options)
      p <- p + geom_line(data=data.values.mass(), aes(SALE_DATE,FORECAST_BASE_AMOUNT),color="orange",size=1)
    # if(input$forecast.raw.used)
    #   p <- p + geom_line(data=data.values.sub(), aes(SALE_DATE,FORECAST_RAW_AMOUNT),color="orange",size=1, linetype="longdash")
    if('smoother' %in% input$scatter.options)
      p <- p + geom_smooth(se=FALSE,colour="red",size=1)
    # if(input$vin.used.checkbox)
    #   p <- p + geom_point(data = vin.highlight(), aes(SALE_DATE, SALE_PRICE_BASE), colour = vin.color(), size = 3, shape = 18)
    # if(input$mileage.filter)
    #   p <- p + geom_point(aes(col = MILEAGE), size = input$size.used)# + scale_colour_gradientn(colours = c("red", "violet", "blue")) #color = "#A86F59"
    #} else if(input$plotType == "rawavplot") {
    #p <- ggplot(data.used.sub(),aes(SALE_DATE, SALE_PRICE_AMOUNT)) + theme_gray() + geom_point(size=input$size.used, shape = 21, fill=fillTextUsed()) + ggtitle(paste(input$year.used,input$make.used,input$model.used,input$trim.used,sep=" ")) + theme(plot.title=element_text(face="bold")) + labs(x = "Sale Date", y = "Auction Value")
    # if(input$yaxis.used)
    #   p <- p + scale_y_continuous(limits = c(input$ymin.used, input$ymax.used))
    #if(input$kbb.used)
    #  p <- p + geom_line(data=data.values.sub(),aes(SALE_DATE,BASE_VALUATION_AMOUNT),color="blue",size=1)
    #if(input$forecast.used)
    #  p <- p + geom_line(data=data.values.sub(), aes(SALE_DATE,FORECAST_BASE_AMOUNT),color="orange",size=1)
    #if(input$smooth.used)
    #  p <- p + geom_smooth(se=input$error.used,colour="red",size=1)
    #if(input$vin.used.checkbox)
    #  p <- p + geom_point(data = vin.highlight(), aes(SALE_DATE, SALE_PRICE_AMOUNT), colour = vin.color(), size = 3, shape = 18)
    #} else if (input$plotType == 'mileageplot') {
    #p <- ggplot(data.used.sub(),aes(MILEAGE,SALE_PRICE_AMOUNT)) + theme_gray() + geom_point(size=input$size.used, fill=fillTextUsed()) + scale_x_continuous(name = "Mileage") + scale_y_continuous(name="Sale Price") + ggtitle(paste(input$year.used,input$make.used,input$model.used,input$trim.used,sep=" ")) + theme(plot.title=element_text(face="bold"))
    # if(input$yaxis.used)
    #   p <- p + scale_y_continuous(limits = c(input$ymin.used, input$ymax.used), name = "Sale Price")
    # if(input$xaxis.used)
    #   p <- p + scale_x_continuous(limits = c(input$xmin.used, input$xmax.used), name = "Mileage")
    #   if(input$smooth.used)
    #     p <- p + stat_smooth(se=input$error.used,colour="red",size=1)
    # } else if (input$plotType == 'ucfppplot') {
    #   p <- ggplot(data.ucfpp.sub(),aes(SALE_DATE,RETAIL_PRICE)) + theme_gray() + geom_point(size=input$size.used, fill=fillTextUsed()) + scale_x_date(name="Sale Date") + scale_y_continuous(name="UCFPP") + ggtitle(paste(input$year.used,input$make.used,input$model.used,input$trim.used,sep=" ")) + theme(plot.title=element_text(face="bold"))
    #   # if(input$yaxis.used)
    #   #   p <- p + scale_y_continuous(limits = c(input$ymin.used, input$ymax.used), name = "UCFPP")
    #   if(input$kbb.used)
    #     p <- p + geom_line(data=data.values.sub(), aes(SALE_DATE,UCFPP),color="blue",size=1) + geom_line(data=data.values.sub(),aes(SALE_DATE,UCFPP_LOWER), color="blue", size = 1, linetype = "longdash") + geom_line(data=data.values.sub(),aes(SALE_DATE, UCFPP_UPPER), color = "blue", size = 1, linetype = "longdash")
    #   if(input$forecast.used)
    #     p <- p + geom_line(data=data.values.sub(), aes(SALE_DATE,UCFPP_FST),color="orange",size=1)
    #   if(input$smooth.used)
    #     p <- p + geom_smooth(se=input$error.used,colour="red",size=1)
    #   if(input$vin.used.checkbox)
    #     p <- p + geom_point(data = vin.highlight(), aes(SALE_DATE, RETAIL_PRICE), colour = vin.color(), size = 3, shape = 18)
    # } else if (input$plotType == 'retailplot') {
    #   p <- ggplot(data.srp.sub(), aes(LIST_DATE, BASE_LISTINGS_PRICE)) + theme_gray() + geom_point(size=input$size.used, fill=fillTextUsed()) + scale_x_date(name="List Date") + scale_y_continuous(name="List Price") + ggtitle(paste(input$year.used,input$make.used,input$model.used,input$trim.used,sep=" ")) + theme(plot.title=element_text(face="bold"))
    #   # if(input$yaxis.used)
    #   #   p <- p + scale_y_continuous(limits = c(input$ymin.used, input$ymax.used), name = "List Price")
    #   if(input$kbb.used)
    #     p <- p + geom_line(data=data.values.sub(), aes(SALE_DATE,RETAIL),color="blue",size=1)
    #   if(input$forecast.used)
    #     p <- p + geom_line(data=data.values.sub(), aes(SALE_DATE,SR_FST),color="orange",size=1)
    #   if(input$smooth.used)
    #     p <- p + geom_smooth(se=input$error.used,colour="red",size=1)
    #   if(input$vin.used.checkbox)
    #     p <- p + geom_point(data = vin.highlight(), aes(LIST_DATE, BASE_LISTINGS_PRICE), colour = vin.color(), size = 3, shape = 18)
    # }
    
    return(p)
  })
  
  # Creates empty list to store mass scatters
  plot_list <- list()
  
  # Download handler for mass scatters
  output$download.scatters <- downloadHandler(
    filename = function() {paste("Plot Output.pdf")},
    content = function(file) {
      unique_ids <- as.list(data.used.mass() %>% distinct(VIMS_VEHICLE_ID, .keep_all = TRUE) %>% select(VIMS_VEHICLE_ID))
      for(i in unique_ids$VIMS_VEHICLE_ID){
        d.used <- data.used.mass() %>% filter(VIMS_VEHICLE_ID == i)
        d.values <- data.values.mass() %>% filter(VIMS_VEHICLE_ID == i)
        p <- ggplot(data=d.used,aes(SALE_DATE,SALE_PRICE_BASE)) + theme_gray() + geom_point()+ labs(x = "Sale Date", y = "Auction Value") + scale_x_date(date_labels = "%b %y", date_breaks = "1 month") + scale_y_continuous(labels = dollar) + ggtitle(paste(d.used$MODEL_YEAR, d.used$MAKE_NAME, d.used$MODEL_NAME, d.used$VEHICLE_NAME, sep=" ")) + theme(plot.title=element_text(face="bold"))
        if('kbb' %in% input$scatter.options)
          p <- p + geom_line(data=d.values,aes(SALE_DATE,BASE_VALUATION_AMOUNT),color="blue",size=1)
        if('fc' %in% input$scatter.options)
          p <- p + geom_line(data=d.values, aes(SALE_DATE,FORECAST_BASE_AMOUNT),color="orange",size=1)
        if('smoother' %in% input$scatter.options)
          p <- p + geom_smooth(se=FALSE,colour="red",size=1)
        plot_list[[i]] <- p
      }
      pdf(file, width = 15)
      for(i in seq(length(plot_list))){
        print(plot_list[[i]])
      }
      dev.off()
    }
  )
  
  output$mass.scatters.preview <- renderPlot({
    withProgress(message = loadingMessages(), value = 0, {
      setProgress(value = 1)
      print(mass.scatters.input())
    })
  })
  
  # Used scatter ggplot2 Output
  plotInputUsed.ggplot <- reactive({
    if(input$plotType == "avplot") {
      p <- ggplot(data.used.sub(),aes(SALE_DATE,SALE_PRICE_BASE)) + theme_gray() + geom_point(size=input$size.used) + ggtitle(paste(input$year.used,input$make.used,input$model.used,input$trim.used,sep=" ")) + theme(plot.title=element_text(face="bold")) + labs(x = "Sale Date", y = "Auction Value")
      # if(input$yaxis.used)
      #   p <- p + scale_y_continuous(limits = c(input$ymin.used, input$ymax.used))
      if(input$vin.used.checkbox)
        p <- p + geom_point(data = vin.highlight(), aes(SALE_DATE, SALE_PRICE_BASE), colour = vin.color(), size = 3, shape = 18)
      if(input$kbb.used)
        p <- p + geom_line(data=data.values.sub(),aes(SALE_DATE,BASE_VALUATION_AMOUNT),color="blue",size=1)
      if(input$forecast.used)
        p <- p + geom_line(data=data.values.sub(), aes(SALE_DATE,FORECAST_BASE_AMOUNT),color="orange",size=1)
      if(input$smooth.used)
        p <- p + geom_smooth(se=FALSE,colour="red",size=1)
    } else if (input$plotType == 'mileageplot') {
      p <- ggplot(data.used.sub(),aes(MILEAGE,SALE_PRICE_AMOUNT)) + theme_gray() + geom_point(size=input$size.used) + scale_x_continuous(name = "Mileage") + scale_y_continuous(name="Sale Price") + ggtitle(paste(input$year.used,input$make.used,input$model.used,input$trim.used,sep=" ")) + theme(plot.title=element_text(face="bold"))
      # if(input$yaxis.used)
      #   p <- p + scale_y_continuous(limits = c(input$ymin.used, input$ymax.used), name = "Sale Price")
      # if(input$xaxis.used)
      #   p <- p + scale_x_continuous(limits = c(input$xmin.used, input$xmax.used), name = "Mileage")
      if(input$smooth.used)
        p <- p + stat_smooth(se=FALSE,colour="red",size=1)
    } else if (input$plotType == 'ucfppplot') {
      p <- ggplot(data.ucfpp.sub(),aes(SALE_DATE,RETAIL_PRICE)) + theme_gray() + geom_point(size=input$size.used) + scale_x_date(name="Sale Date") + scale_y_continuous(name="UCFPP") + ggtitle(paste(input$year.used,input$make.used,input$model.used,input$trim.used,sep=" ")) + theme(plot.title=element_text(face="bold"))
      # if(input$yaxis.used)
      #   p <- p + scale_y_continuous(limits = c(input$ymin.used, input$ymax.used), name = "UCFPP")
      if(input$kbb.used)
        p <- p + geom_line(data=data.values.sub(), aes(SALE_DATE,UCFPP),color="blue",size=1) + geom_line(data=data.values.sub(),aes(SALE_DATE,UCFPP_LOWER), color="blue", size = 1, linetype = "longdash") + geom_line(data=data.values.sub(),aes(SALE_DATE, UCFPP_UPPER), color = "blue", size = 1, linetype = "longdash")
      if(input$forecast.used)
        p <- p + geom_line(data=data.values.sub(), aes(SALE_DATE,UCFPP_FST),color="orange",size=1)
      if(input$smooth.used)
        p <- p + geom_smooth(se=FALSE,colour="red",size=1)
    } else if (input$plotType == 'retailplot') {
      p <- ggplot(data.srp.sub(), aes(LIST_DATE, BASE_LISTINGS_PRICE)) + theme_gray() + geom_point(size=input$size.used) + scale_x_date(name="List Date") + scale_y_continuous(name="List Price") + ggtitle(paste(input$year.used,input$make.used,input$model.used,input$trim.used,sep=" ")) + theme(plot.title=element_text(face="bold"))
      # if(input$yaxis.used)
      #   p <- p + scale_y_continuous(limits = c(input$ymin.used, input$ymax.used), name = "List Price")
      if(input$kbb.used)
        p <- p + geom_line(data=data.values.sub(), aes(SALE_DATE,RETAIL),color="blue",size=1)
      if(input$forecast.used)
        p <- p + geom_line(data=data.values.sub(), aes(SALE_DATE,SR_FST),color="orange",size=1)
      if(input$smooth.used)
        p <- p + geom_smooth(se=FALSE,colour="red",size=1)
    }
    
    return(p)
  })
  
  # ggplot2 Output
  output$plot.used.ggplot <- renderPlot({
    print(plotInputUsed.ggplot())
  })
  
  # UCFPP Range Count
  #ucfpp.range.counts <- reactive({
  
  #low <- nrow(filter(data.ucfpp.sub()$RETAIL_PRICE < data.values.sub()$UCFPP_LOWER))
  #low <- nrow(left_join(data.ucfpp.sub(), data.values.sub(), by = c("SALE_DATE" = "SALE_DATE")) %>% filter(RETAIL_PRICE < UCFPP_LOWER))
  #low2 <- nrow(full_join(data.ucfpp.sub(), data.values.sub(), by = c("SALE_DATE" = "SALE_DATE")) %>% filter(RETAIL_PRICE < UCFPP_LOWER))
  #high <- nrow(data.new.sub() %>% filter(DISCOUNT > KBBHIGH))
  #inrange <- nrow(data.new.sub() %>% filter(DISCOUNT >= KBBLOW, DISCOUNT <= KBBHIGH))
  #total <- low + high + inrange
  #c(low, low2)#, inrange, high, total)
  #})
  
  
  # output$nrowUcfppBox.sub.low <- renderValueBox({
  #   req(ucfpp.range.counts())
  #   if (input$plotType == 'ucfppplot'){
  #     rows <- ucfpp.range.counts()[1]
  #     valueBox(
  #       format(rows, big.mark = ","), "Below range", color = "navy"
  #     )} else {
  #       NULL
  #     }
  # })
  
  # output$nrowUcfppBox.sub.low2 <- renderValueBox({
  #   req(ucfpp.range.counts())
  #   if (input$plotType == 'ucfppplot'){
  #     rows <- ucfpp.range.counts()[2]
  #     valueBox(
  #       format(rows, big.mark = ","), "Below range", color = "navy"
  #     )} else {
  #       NULL
  #     }
  # })
  
  #USED COMPARISON SCATTER OUTPUT
  plotInputUsed.comparison <- reactive({
    if(input$plotType.comparison == "avplot.comparison") {
      p <- ggplot() + geom_point(data=data.used.sub.comparison1(),aes(SALE_DATE,SALE_PRICE_BASE, color="Vehicle 1"),size=input$size.used.comparison) + geom_point(data=data.used.sub.comparison2(), aes(SALE_DATE, SALE_PRICE_BASE, color="Vehicle 2"), size=input$size.used.comparison) + labs(x = "Sale Date", y = "Auction Value") + ggtitle(paste(input$year.used.comparison1,input$make.used.comparison1,input$model.used.comparison1,input$trim.used.comparison1,"vs",input$year.used.comparison2,input$make.used.comparison2,input$model.used.comparison2,input$trim.used.comparison2,sep=" ")) + theme(plot.title=element_text(face="bold")) + scale_color_manual(name="", values=c("Vehicle 1"="black", "Vehicle 2"="firebrick"), guide="legend") + guides(color=guide_legend(override.aes = list(shape=c(16,16)))) + theme(panel.background=element_rect(fill="grey75")) 
      if(input$kbb.used.comparison1)
        p <- p + geom_line(data=data.values.sub.comparison1(),aes(SALE_DATE,BASE_VALUATION_AMOUNT),color="blue",size=1)
      if(input$kbb.used.comparison2)
        p <- p + geom_line(data=data.values.sub.comparison2(),aes(SALE_DATE,BASE_VALUATION_AMOUNT),color="blue",size=1)
      if(input$forecast.used.comparison1)
        p <- p + geom_line(data=data.values.sub.comparison1(), aes(SALE_DATE,FORECAST_BASE_AMOUNT),color="orange",size=1)
      if(input$forecast.used.comparison2)
        p <- p + geom_line(data=data.values.sub.comparison2(), aes(SALE_DATE,FORECAST_BASE_AMOUNT),color="orange",size=1)
      if(input$smooth.used.comparison1)
        p <- p + stat_smooth(data=data.used.sub.comparison1(),aes(SALE_DATE,SALE_PRICE_BASE),se=FALSE,color="red",size=1)
      if(input$smooth.used.comparison2)
        p <- p + stat_smooth(data=data.used.sub.comparison2(),aes(SALE_DATE,SALE_PRICE_BASE),se=FALSE, color="red", size=1)
    }# } else if (input$plotType == 'mileageplot') {
    #   p <- ggplot(data.used.sub(),aes(MILEAGE,SALE_PRICE_AMOUNT)) + theme_gray() + geom_point(size=input$size.used, fill=fillTextUsed()) + scale_x_continuous(name = "Mileage") + scale_y_continuous(name="Sale Price") + ggtitle(paste(input$year.used,input$make.used,input$model.used,input$trim.used,sep=" ")) + theme(plot.title=element_text(face="bold"))
    #   if(input$yaxis.used)
    #     p <- p + scale_y_continuous(limits = c(input$ymin.used, input$ymax.used), name = "Sale Price")
    #   if(input$xaxis.used)
    #     p <- p + scale_x_continuous(limits = c(input$xmin.used, input$xmax.used), name = "Mileage")
    #   if(input$smooth.used)
    #     p <- p + stat_smooth(se=input$error.used,colour="red",size=1)
    # } else if (input$plotType == 'ucfppplot') {
    #   p <- ggplot(data.ucfpp.sub(),aes(SALE_DATE,RETAIL_PRICE)) + theme_gray() + geom_point(size=input$size.used, fill=fillTextUsed()) + scale_x_date(name="Sale Date") + scale_y_continuous(name="UCFPP") + ggtitle(paste(input$year.used,input$make.used,input$model.used,input$trim.used,sep=" ")) + theme(plot.title=element_text(face="bold"))
    #   if(input$yaxis.used)
    #     p <- p + scale_y_continuous(limits = c(input$ymin.used, input$ymax.used), name = "UCFPP")
    #   if(input$kbb.used)
    #     p <- p + geom_line(data=data.values.sub(), aes(SALE_DATE,UCFPP),color="blue",size=1) + geom_line(data=data.values.sub(),aes(SALE_DATE,UCFPP_LOWER), color="blue", size = 1, linetype = "longdash") + geom_line(data=data.values.sub(),aes(SALE_DATE, UCFPP_UPPER), color = "blue", size = 1, linetype = "longdash")
    #   if(input$forecast.used)
    #     p <- p + geom_line(data=data.values.sub(), aes(SALE_DATE,UCFPP_FST),color="orange",size=1)
    #   if(input$smooth.used)
    #     p <- p + geom_smooth(se=input$error.used,colour="red",size=1)
    # } else if (input$plotType == 'retailplot') {
    #   p <- ggplot(data.srp.sub(), aes(LIST_DATE,LIST_PRICE)) + theme_gray() + geom_point(size=input$size.used, fill=fillTextUsed()) + scale_x_date(name="List Date") + scale_y_continuous(name="List Price") + ggtitle(paste(input$year.used,input$make.used,input$model.used,input$trim.used,sep=" ")) + theme(plot.title=element_text(face="bold"))
    #   if(input$yaxis.used)
    #     p <- p + scale_y_continuous(limits = c(input$ymin.used, input$ymax.used), name = "List Price")
    #   if(input$kbb.used)
    #     p <- p + geom_line(data=data.values.sub(), aes(SALE_DATE,RETAIL),color="blue",size=1)
    #   if(input$forecast.used)
    #     p <- p + geom_line(data=data.values.sub(), aes(SALE_DATE,SR_FST),color="orange",size=1)
    #   if(input$smooth.used)
    #     p <- p + geom_smooth(se=input$error.used,colour="red",size=1)
    # }
    
    return(p)
  })
  
  output$plot.used.comparison <- renderPlot({
    print(plotInputUsed.comparison())
  })
  
  
  # Download buttons - Not functioning or not used
  
  # output$downloadDataUsed <- downloadHandler(
  #   filename = function () {
  #     paste(input$make.used, input$model.used, input$trim.used, input$year.used, Sys.Date(), 'csv', sep='.')
  #   },
  #   content = function(file) {
  #     write.csv(data.used.sub(),file)
  #   }
  # )
  # 
  
  # Download single used car plot
  # output$downloadPlotUsed <- downloadHandler (
  #   filename = function () {
  #     paste(input$make.used, input$model.used, input$trim.used, input$year.used, Sys.Date(), 'png', sep='.')
  #   },
  #   content = function(file) {
  #     png(file, width=800)
  #     #ggsave(file, plot = plotInputUsed.plotly(), device = "png", width = 15)
  #     print(plotInputUsed.ggplot())
  #     dev.off()
  #   }
  # )
  # 
  # output$downloadPlotUsedPDF <- downloadHandler (
  #   filename = function () { 
  #     paste(input$make.used, input$model.used, input$trim.used, input$year.used, Sys.Date(), 'pdf', sep='.')
  #   },
  #   content = function(file) {
  #     pdf(file, width=6, height=4)
  #     print(plotInputUsed())
  #     dev.off()
  #   }
  # )
  
  output$table.used <- renderDataTable(
    if(input$plotType == "avplot" | input$plotType == "mileageplot")
      select(data.used.sub(), SALE_DATE:SALE_PRICE_BASE)
    else if(input$plotType =="ucfppplot")
      data.ucfpp.sub()
    else if(input$plotType=="retailplot")
      data.srp.sub(),
    extensions = list("ColReorder"=NULL, "RowReorder"=NULL, "Buttons"=NULL),
    options = list(pageLength=50, dom="Bfrtip", fixedHeader=TRUE, colReorder = TRUE, rowReorder=TRUE, buttons=I("colvis"))
  )
  
  av.bias.query.sub <- reactive({
    vvid <- data.used.sub()$VIMS_VEHICLE_ID[1]
    m <- av.bias.query() %>% filter(VIMSID == vvid) %>% select(-VIMSID) %>% top_n(3, wt=PUBLISH_WEEK)
    m$PUBLISH_WEEK <- as.Date(m$PUBLISH_WEEK)
    return(m)
  })
  
  ucfpp.bias.query.sub <- reactive({
    vvid <- data.ucfpp.sub()$VIMS_VEHICLE_ID[1]
    m <- ucfpp.bias.query() %>% filter(VIMSID == vvid) %>% select(-VIMSID) %>% top_n(3, wt=PUBLISH_WEEK)
    return(m)
  })
  
  srp.bias.query.sub <- reactive({
    vvid <- data.srp.sub()$VIMS_VEHICLE_ID[1]
    m <- srp.bias.query() %>% filter(VIMSID == vvid) %>% select(-VIMSID) %>% top_n(3, wt=PUBLISH_WEEK)
    return(m)
  })
  
  output$bias <- renderDataTable(
    if(input$plotType == "avplot"){
      av.bias.query.sub()
    } else if (input$plotType == "ucfppplot"){
      ucfpp.bias.query.sub()
    } else if(input$plotType == "retailplot"){
      srp.bias.query.sub()
    },
    extensions = list("ColReorder"=NULL, "RowReorder"=NULL, "Buttons"=NULL),
    options = list(pageLength=50, dom="Bt", fixedHeader=TRUE, colReorder = TRUE, rowReorder=TRUE, buttons=I("colvis"))
  )
  
  output$summary.used <- renderPrint(
    summary(data.used.sub() %>% select(SALE_PRICE_BASE, MILEAGE))
  )
  output$summary.ucfpp <- renderPrint(
    summary(data.ucfpp.sub())
  )
  output$summary.srp <- renderPrint(
    summary(data.srp.sub())
  )
  
  # Total Transaction Count
  output$nrowUsedBox.sub <- renderText({
    req(input$trim.used)
    if(input$plotType == "avplot" | input$plotType == "mileageplot" | input$plotType == "rawavplot"){
      rows <- nrow(data.used.sub())
      paste("Total Transactions: ", rows)
      #valueBox(rows, "Total Transactions")
    }else if (input$plotType == "ucfppplot"){
      rows <- nrow(data.ucfpp.sub())
      #valueBox(rows, "Total Transactions")
      paste("Total Transactions: ", rows)
    }else if (input$plotType == "retailplot"){
      rows <- nrow(data.srp.sub())
      #valueBox(rows, "Total Listings")
      paste("Total Listings: ", rows)}
  })
  
  output$nrowUsedBox.sub.ggplot <- renderText({
    if(input$plotType == "avplot" | input$plotType == "mileageplot"){
      rows <- nrow(data.used.sub())
      paste("Total Transactions: ", rows)
      #valueBox(rows, "Total Transactions")
    }else if (input$plotType == "ucfppplot"){
      rows <- nrow(data.ucfpp.sub())
      #valueBox(rows, "Total Transactions")
      paste("Total Transactions: ", rows)
    }else if (input$plotType == "retailplot"){
      rows <- nrow(data.srp.sub())
      #valueBox(rows, "Total Listings")
      paste("Total Listings: ", rows)}
  })
  
  # Over/Under value boxes
  output$under.box <- renderValueBox({
    req(input$trim.used)
    kbb <- data.values.sub()$BASE_VALUATION_AMOUNT[1]
    cnt <- data.used.sub()$MNHM_COUNT[1]
    #row <- nrow(data.used.sub() %>% filter(kbb > SALE_PRICE_BASE))
    row <- nrow(data.used.sub() %>% top_n(cnt, wt=SALE_DATE) %>% filter(kbb < SALE_PRICE_BASE))
    valueBox(
      paste(row), "AV Under", icon = icon("chevron-circle-down"), color = "olive", width = NULL
    )
  })
  
  output$over.box <- renderValueBox({
    req(input$trim.used)
    kbb <- data.values.sub()$BASE_VALUATION_AMOUNT[1]
    cnt <- data.used.sub()$MNHM_COUNT[1]
    #row <- nrow(data.used.sub() %>% filter(kbb < SALE_PRICE_BASE))
    row <- nrow(data.used.sub() %>% top_n(cnt, wt=SALE_DATE) %>% filter(kbb >= SALE_PRICE_BASE))
    valueBox(
      paste(row), "AV Over", icon = icon("chevron-circle-up"), color = "olive", width = NULL
    )
  })
  
  output$buckets.box <- renderValueBox({
    req(input$trim.used)
    valueBox(
      paste(data.used.sub()$MNHM_BUCKET), "Week", icon = icon("calendar"), color = "purple", width = NULL
    )
  })
  
  #   output$error.used.fc <- renderValueBox({
  #     valueBox(
  #       percent(error.used()$fc), "forecast error", color = "red"
  #     )
  #   })
  #   
  #   output$error.used.pub <- renderValueBox({
  #     valueBox(
  #       percent(error.used()$pub), "published error", color = "red"
  #     )
  #   })
  
  #   error.used <- reactive({
  #     fc <- data.used.sub() %>%
  #       filter(FORECAST_BASE_AMOUNT > 0) %>%
  #       summarise(fc = mean(abs(FORECAST_BASE_AMOUNT/SALE_PRICE_BASE - 1)))
  #     pub <- data.used.sub() %>%
  #       filter(BASE_VALUATION_AMOUNT > 0) %>%
  #       summarise(fc = mean(abs(FORECAST_BASE_AMOUNT/SALE_PRICE_BASE - 1)), pub = mean(abs(BASE_VALUATION_AMOUNT/SALE_PRICE_BASE - 1)))
  #     c(fc, pub)
  #     })
  
  
  ### New car scatter functions ##
  
  query.new <- reactive ({
    sprintf("select distinct 
            v.VIMS_VEHICLE_ID
            ,a.VIN
            , d.CALENDARDATE as SALE_DATE
            , v.MODEL_YEAR
            , v.MAKE_NAME
            , v.MODEL_NAME
            , v.VEHICLE_NAME
            , v.SEGMENT
            --, g.STATE
            , a.MILEAGE
            --, a.SALE_PRICE_AMOUNT
            --, a.MSRP_AMOUNT
            , ROUND(a.SALE_PRICE_AMOUNT / a.MSRP_AMOUNT - 1, 4) as DISCOUNT
            , fpp.FPP_PERCENT/100 - 1 as KBBFPP
            , fpp.FPP_PERCENT * fpp.FPP_RANGE_LOWER_LIMIT / 100 - 1 as KBBLOW
            , fpp.FPP_PERCENT * fpp.FPP_RANGE_UPPER_LIMIT / 100 - 1 as KBBHIGH
            , fpp.FORECAST_FPP_PERCENT/100 - 1 as FORECAST
            , case when DISCOUNT >= 0 then 1 else 0 end as MSRPFLAG
            
            from EDWVEHICLE..RETAIL_TRANSACTION_F a
            join EDWVEHICLE..DATE_D d ON a.SALE_DATE_ID = d.DATEID
            join EDWVEHICLE..VALUATION_VEHICLE_D v ON a.VALUATION_VEHICLE_ID = v.VALUATION_VEHICLE_ID
            join EDWVEHICLE..PUBLICATION_VERSION_D pv on d.CALENDARDATE between pv.EFFECTIVE_PRODUCT_VERSION_START_DATE and pv.EFFECTIVE_PRODUCT_VERSION_END_DATE
            --join EDWVEHICLE..FAIR_PURCHASE_PRICE_FORECAST_VALUE_F fpp on (a.VALUATION_DRIVETRAIN_ID = fpp.VALUATION_DRIVETRAIN_ID and a.VALUATION_ENGINE_ID = fpp.VALUATION_ENGINE_ID and a.VALUATION_TRANSMISSION_ID = fpp.VALUATION_TRANSMISSION_ID and a.VALUATION_VEHICLE_ID = fpp.VALUATION_VEHICLE_ID and pv.PUBLICATION_VERSION_ID = fpp.PUBLICATION_VERSION_ID)
            join EDWVEHICLE..FAIR_PURCHASE_PRICE_FORECAST_VALUE_F fpp on (a.VALUATION_VEHICLE_ID = fpp.VALUATION_VEHICLE_ID and pv.PUBLICATION_VERSION_ID = fpp.PUBLICATION_VERSION_ID)
            
            
            WHERE 1=1
            and a.VEHICLE_TRANSACTION_TYPE_ID = 2
            and DISCOUNT between -0.2 and 0.2
            --and (a.MILEAGE <= 1000 or a.MILEAGE is NULL)
            --and a.MSRP_AMOUNT > 10000
            and a.OUTLIER_FLAG = 'N'
            and d.CALENDARDATE between '%s' and '%s'
            %s
            %s
            %s
            ORDER BY SALE_DATE DESC
            ",input$dateRange[1], input$dateRange[2], segmentText(), makeText(), modelText()
    )
  })
  
  query.new.values <- reactive({
    sprintf("select
            v.VIMS_VEHICLE_ID
            ,d.CALENDARDATE as SALE_DATE
            ,fpp.FPP_PERCENT/100 - 1 as KBBFPP
            ,fpp.FPP_PERCENT * fpp.FPP_RANGE_LOWER_LIMIT / 100 - 1 as KBBLOW
            ,fpp.FPP_PERCENT * fpp.FPP_RANGE_UPPER_LIMIT / 100 - 1 as KBBHIGH
            ,fpp.FORECAST_FPP_PERCENT/100 - 1 as FORECAST
            
            from 	edwvehicle..FAIR_PURCHASE_PRICE_FORECAST_VALUE_F fpp
            join edwvehicle..VALUATION_VEHICLE_D v
            on v.VALUATION_VEHICLE_ID = fpp.VALUATION_VEHICLE_ID
            join edwvehicle..PUBLICATION_VERSION_D pv
            on pv.PUBLICATION_VERSION_ID = fpp.PUBLICATION_VERSION_ID
            join edwvehicle..DATE_D d
            on d.CALENDARDATE = pv.EFFECTIVE_PRODUCT_VERSION_END_DATE
            
            where
            pv.PUBLICATION_VERSION_DESC = 'NCU'
            and fpp.FPP_RULE_ID <> 5
            and d.CALENDARDATE between '%s' and '%s'
            %s
            %s
            %s
            
            order by
            SALE_DATE DESC
            ",input$dateRange[1], input$dateRange[2], segmentText(), makeText(), modelText()
    )
  })
  
  output$nrowNewBox <- renderValueBox({
    rows <- rowCountNew()
    if(rows == 0) boxColor <- "red" else boxColor <- "green"
    if(rows == 0) boxIcon <- "ban" else boxIcon <- "thumbs-o-up"
    valueBox(
      format(rows, big.mark = ","), "New Car Transactions", icon = icon(boxIcon), color = boxColor
    )
  })
  
  rowCountNew <- reactive({
    rows <- nrow(data.new())
    if(is.null(rows)) rows <- 0
    format(rows, big.mark = ",")
  })
  
  data.new <- reactive({
    withProgress(message = loadingMessages(), value = 0, {
      if(input$newButton >= 1) {
        setProgress(value = 1)
        check.connect(channel)
        print(paste("Query beginning... ", Sys.time()))
        data <- sqlQuery(channel, isolate(query.new()), believeNRows = FALSE)
        print(paste("Query complete!    ", Sys.time()))
        data$SALE_DATE <- as.Date(data$SALE_DATE)
        invisible(data)
      }
    })
  })
  
  data.new.values <- reactive({
    withProgress(message = loadingMessages(), value = 0, {
      if(input$newButton >= 1) {
        setProgress(value=1)
        check.connect(channel)
        print(paste("Query beginning... ", Sys.time()))
        data <- sqlQuery(channel, isolate(query.new.values()), believeNRows = FALSE)
        print(paste("Query complete!    ", Sys.time()))
        data$SALE_DATE <- as.Date(data$SALE_DATE)
        invisible(data)
      }
    })
  })
  
  output$year.new <- renderUI({
    # If no data, initialize with current year
    if(rowCountNew() == 0) 
      initial <- as.numeric(format(Sys.Date(), "%Y")) 
    else 
      initial <- max(data.new()$MODEL_YEAR)
    
    selectInput("year.new", "Year: ", c("", as.character(year.list$MODEL_YEAR)), selected = initial)
  })
  
  output$make.new <- renderUI({
    req(input$year.new)
    make.list <- data.new() %>%
      filter(MODEL_YEAR == input$year.new) %>%
      group_by(MAKE_NAME) %>%
      distinct(MAKE_NAME, .keep_all = TRUE) %>%
      select(MAKE_NAME) %>%
      arrange(MAKE_NAME)
    selectInput("make.new", "Make:", as.character(make.list$MAKE_NAME), selected = input$make.new)
  })
  
  output$model.new <- renderUI({
    req(input$make.new)
    model.list <- data.new() %>%
      filter(MODEL_YEAR == input$year.new, MAKE_NAME == input$make.new) %>%
      group_by(MODEL_NAME) %>%
      distinct(MODEL_NAME, .keep_all = TRUE) %>%
      select(MODEL_NAME) %>%
      arrange(MODEL_NAME)
    selectInput("model.new", "Model:", as.character(model.list$MODEL_NAME), selected = input$model.new)
  })
  
  output$trim.new <- renderUI({
    req(input$model.new)
    trim.list <- data.new() %>%
      filter(MODEL_YEAR == input$year.new, MAKE_NAME == input$make.new, MODEL_NAME == input$model.new)  %>%
      group_by(VEHICLE_NAME) %>%
      distinct(VEHICLE_NAME, .keep_all = TRUE) %>%
      select(VEHICLE_NAME) %>%
      arrange(VEHICLE_NAME)
    selectInput("trim.new", "Trim:", as.character(trim.list$VEHICLE_NAME), selected = input$trim.new)
  })
  
  output$vimsid.new <- renderText({
    req(input$trim.new)
    vimsid.list <- data.new() %>%
      filter(MODEL_YEAR == input$year.new, MAKE_NAME == input$make.new, MODEL_NAME == input$model.new, VEHICLE_NAME == input$trim.new) %>%
      distinct(VIMS_VEHICLE_ID, .keep_all = TRUE) %>%
      select(VIMS_VEHICLE_ID)
    paste(vimsid.list$VIMS_VEHICLE_ID)
  })
  
  #   output$stateFilter <- renderUI({
  #     state.list <- data.new() %>%
  #       filter(MODEL_YEAR == input$year.new, MAKE_NAME == input$make.new, MODEL_NAME == input$model.new, VEHICLE_NAME == input$trim.new)  %>%
  #       group_by(STATE) %>%
  #       distinct(STATE) %>%
  #       select(STATE) %>%
  #       arrange(STATE)
  #     selectInput("state", "State:", as.character(state.list$STATE), selected = input$state)
  #   }) 
  
  output$dates.new <- renderUI({
    dateRangeInput("inDates2", "Date range:", start = input$dateRange[1], end = input$dateRange[2], min = input$dateRange[1], max = input$dateRange[2])
  })
  
  data.new.sub <- reactive ({
    m <- data.new() %>%
      filter(MODEL_YEAR == input$year.new, 
             MAKE_NAME == input$make.new, 
             MODEL_NAME == input$model.new, 
             VEHICLE_NAME == input$trim.new,
             SALE_DATE >= input$inDates2[1],
             SALE_DATE <= input$inDates2[2])
    if(!input$msrp) m <- m %>% filter(MSRPFLAG == 0)
    return(m)
  })
  
  data.new.values.sub <- reactive({
    vvid <- data.new.sub()$VIMS_VEHICLE_ID[1]
    m <- data.new.values() %>%
      filter(VIMS_VEHICLE_ID == vvid,
             SALE_DATE >= input$inDates2[1],
             SALE_DATE <= input$inDates2[2])
    return(m)
  })
  
  fillTextNew <- reactive(
    if(input$fill.new) paste("black") 
    else NA
  )
  
  # New Car Plot Scatter Output
  plotInputNew <- reactive({    
    p <- ggplot(data.new.sub(),aes(SALE_DATE,DISCOUNT)) + fte_theme() + geom_point(colour="black", size=input$size.new) + scale_x_date(name="Sale Date") + scale_y_continuous(name="Discount from MSRP") + ggtitle(paste(input$year.new,input$make.new,input$model.new,input$trim.new,sep=" "))
    if(input$kbb.new)
      p <- p + geom_line(data = data.new.values.sub(), aes(SALE_DATE,KBBFPP),color="blue",size=1) 
    if(input$fpprange) {
      p <- p + geom_line(data = data.new.values.sub(), aes(SALE_DATE,KBBLOW), color= "blue", size = 1, linetype = "longdash")
      p <- p + geom_line(data = data.new.values.sub(), aes(SALE_DATE,KBBHIGH), color= "blue", size = 1, linetype = "longdash")
    }
    if(input$forecast.new)
      p <- p + geom_line(data = data.new.values.sub(), aes(SALE_DATE,FORECAST),color="orange",size=1)
    if(input$smooth.new) 
      p <- p + geom_smooth(se = FALSE ,colour="red",size=1)
    return(ggplotly(p))
  })
  
  output$plot.new <- renderPlotly({
    withProgress(message = loadingMessages(), value = 0, {
      setProgress(value=1)
      print(plotInputNew())
    })
  })
  
  output$downloadDataNew <- downloadHandler(
    filename = function () {
      paste(input$make.new, input$model.new, input$trim.new, input$year.new, Sys.Date(), 'csv', sep='.')
    },
    content = function(file) {
      write.csv(data.new.sub(),file)
    }
  )
  
  output$downloadPlotNew <- downloadHandler (
    filename = function () { 
      paste(input$make.new, input$model.new, input$trim.new, input$year.new, Sys.Date(), 'png', sep='.')
    },
    content = function(file) {
      png(file, width=600, height=400, type='cairo-png')
      print(plotInputNew())
      dev.off()
    }
  )
  
  output$downloadPlotNewPDF <- downloadHandler (
    filename = function () { 
      paste(input$make.new, input$model.new, input$trim.new, input$year.new, Sys.Date(), 'pdf', sep='.')
    },
    content = function(file) {
      pdf(file, width=6, height=4)
      print(plotInputNew())
      dev.off()
    }
  )
  
  
  
  output$table.new <- renderDataTable(select(data.new.sub(), -KBBLOW, -KBBHIGH, -SEGMENT, -MSRPFLAG), 
                                      options=list(iDisplayLength=50))
  
  output$summary.new <- renderPrint(summary(data.new.sub() %>% select(KBBFPP, MILEAGE, FORECAST)))
  
  output$nrowNewBox.sub <- renderValueBox({
    rows <- new.range.counts()[4]
    valueBox(
      format(rows, big.mark = ","), "transactions"
    )
  })
  
  output$nrowNewBox.sub.low <- renderValueBox({
    rows <- new.range.counts()[1]
    valueBox(
      format(rows, big.mark = ","), "Below range", color = "navy"
    )
  })
  
  output$nrowNewBox.sub.inrange <- renderValueBox({
    rows <- new.range.counts()[2]
    valueBox(
      format(rows, big.mark = ","), "In range", color = "green"
    )
  })
  
  output$nrowNewBox.sub.high <- renderValueBox({
    rows <- new.range.counts()[3]  
    valueBox(
      format(rows, big.mark = ","), "Above range", color = "red"
    )
  })
  
  output$error.new.fc <- renderValueBox({
    valueBox(
      percent(error.new()$fc), "forecast error", color = "red"
    )
  })
  
  output$error.new.pub <- renderValueBox({
    valueBox(
      percent(error.new()$pub), "published error", color = "red"
    )
  })
  
  new.range.counts <- reactive({
    low <- nrow(data.new.sub() %>% filter(DISCOUNT < KBBLOW))
    high <- nrow(data.new.sub() %>% filter(DISCOUNT > KBBHIGH))
    inrange <- nrow(data.new.sub() %>% filter(DISCOUNT >= KBBLOW, DISCOUNT <= KBBHIGH))
    total <- low + high + inrange
    c(low, inrange, high, total)
  })
  
  error.new <- reactive({
    data.new.sub() %>%
      filter(KBBLOW < 0) %>%
      summarise(fc = mean(abs(FORECAST - DISCOUNT)), pub = mean(abs(KBBFPP - DISCOUNT)))
  })
  
  # Automatically stops Shiny when closing the browser tab
  session$onSessionEnded(stopApp)
  }

shinyApp(ui, server)