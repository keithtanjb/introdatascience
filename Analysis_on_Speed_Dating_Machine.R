library(shiny, warn.conflicts = FALSE)
library(shinydashboard, warn.conflicts = FALSE)
library(broom, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(corrplot, warn.conflicts = FALSE)
library(RColorBrewer, warn.conflicts = FALSE)
library(slickR, warn.conflicts = FALSE)
library(ggthemes, warn.conflicts = FALSE)
library(tidyverse, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(knitr, warn.conflicts = FALSE)

#Data Cleaning
Speed_Dating <- read.csv("Speed Dating Data.csv")
Clean_Speed_Dating <- Speed_Dating[Speed_Dating$partner==1, ]
names(Speed_Dating)
Clean_Speed_Dating <- subset(Clean_Speed_Dating, select = c("gender", "age", "field_cd", "mn_sat", "tuition", "race", "imprace", "imprelig", "income", "goal", "date", "go_out", "career_c", "sports", "tvsports", "exercise", "dining", "museums", "art", "hiking", "gaming", "clubbing", "reading", "tv", "theater", "movies", "concerts", "music", "shopping", "yoga", "exphappy", "expnum", "attr1_1", "sinc1_1", "intel1_1", "fun1_1", "amb1_1", "shar1_1", "attr4_1", "sinc4_1", "intel4_1", "fun4_1", "amb4_1", "shar4_1", "attr2_1", "sinc2_1", "intel2_1", "fun2_1", "amb2_1", "shar2_1", "attr3_1", "sinc3_1", "intel3_1", "fun3_1", "amb3_1", "attr5_1", "sinc5_1", "intel5_1", "fun5_1", "amb5_1"))
str(Clean_Speed_Dating)
Clean_Speed_Dating$gender = as.factor(Clean_Speed_Dating$gender)
Clean_Speed_Dating$field_cd = as.factor(Clean_Speed_Dating$field_cd)
Clean_Speed_Dating$race = as.factor(Clean_Speed_Dating$race)
Clean_Speed_Dating$goal = as.factor(Clean_Speed_Dating$goal)
Clean_Speed_Dating$date = as.factor(Clean_Speed_Dating$date)
Clean_Speed_Dating$go_out = as.factor(Clean_Speed_Dating$go_out)
Clean_Speed_Dating$career_c = as.factor(Clean_Speed_Dating$career_c)
Clean_Speed_Dating$mn_sat = as.integer(Clean_Speed_Dating$mn_sat)
Clean_Speed_Dating$tuition = as.integer(Clean_Speed_Dating$tuition)
Clean_Speed_Dating$income = as.integer(Clean_Speed_Dating$income)
str(Clean_Speed_Dating)
levels(Clean_Speed_Dating$gender)
levels(Clean_Speed_Dating$field_cd)
levels(Clean_Speed_Dating$race)
levels(Clean_Speed_Dating$goal)
levels(Clean_Speed_Dating$date)
levels(Clean_Speed_Dating$go_out)
levels(Clean_Speed_Dating$career_c)
levels(Clean_Speed_Dating$gender) <- c("Female", "Male")
levels(Clean_Speed_Dating$field_cd) <- c("Law", "Math", "Social Science, Psychologist", "Medical Science, Pharmaceuticals, and Bio Tech", "Engineering", "English/Creative Writing/ Journalism", "History/Religion/Philosophy", "Business/Econ/Finance", "Education, Academia", "Biological Sciences/Chemistry/Physics", "Social Work", "Undergrad/Undecided", "Political Science/International Affairs", "Film", "Fine Arts/Arts Administration", "Languages", "Architecture", "Others")
levels(Clean_Speed_Dating$race) <- c("Black/African American", "European/Caucasian-American", "Latino/Hispanic American", "Asian/Pacific Islander/Asian-American", "Native American", "Others")
levels(Clean_Speed_Dating$goal) <- c("Seemed like a fun night out", "To meet new people", "To get a date", "Looking for a serious relationship", "To say I did it", "Others")
levels(Clean_Speed_Dating$date) <- c("Several times a week", "Twice a week", "Once a week", "Twice a month", "Once a month", "Several times a year", "Almost never")
levels(Clean_Speed_Dating$go_out) <- c("Several times a week", "Twice a week", "Once a week", "Twice a month", "Once a month", "Several times a year", "Almost never")
levels(Clean_Speed_Dating$career_c) <- c("Lawyer", "Academic/Research", "Psychologist", "Doctor/Medicine", "Engineer", "Creative Arts/Entertainment", "Banking/Consulting/Finance/Marketing/Business/CEO/Entrepreneur/Admin", "Real Estate", "International/Humanitarian Affairs", "Undecided", "Social Work", "Speech Pathology", "Politics", "Pro sports/Athletics", "Others", "Journalism", "Architecture")
str(Clean_Speed_Dating)
Clean_Speed_Dating$attr1_1 <- as.integer(Clean_Speed_Dating$attr1_1)
Clean_Speed_Dating$sinc1_1 <- as.integer(Clean_Speed_Dating$sinc1_1)
Clean_Speed_Dating$intel1_1 <- as.integer(Clean_Speed_Dating$intel1_1)
Clean_Speed_Dating$fun1_1 <- as.integer(Clean_Speed_Dating$fun1_1)
Clean_Speed_Dating$amb1_1 <- as.integer(Clean_Speed_Dating$amb1_1)
Clean_Speed_Dating$shar1_1 <- as.integer(Clean_Speed_Dating$shar1_1)
Clean_Speed_Dating$attr2_1 <- as.integer(Clean_Speed_Dating$attr2_1)
Clean_Speed_Dating$sinc2_1 <- as.integer(Clean_Speed_Dating$sinc2_1)
Clean_Speed_Dating$intel2_1 <- as.integer(Clean_Speed_Dating$intel2_1)
Clean_Speed_Dating$fun2_1 <- as.integer(Clean_Speed_Dating$fun2_1)
Clean_Speed_Dating$amb2_1 <- as.integer(Clean_Speed_Dating$amb2_1)
Clean_Speed_Dating$shar2_1 <- as.integer(Clean_Speed_Dating$shar2_1)
str(Clean_Speed_Dating)
dim(Clean_Speed_Dating)
colSums(is.na(Clean_Speed_Dating))
Clean_Speed_Dating <- subset(Clean_Speed_Dating, select = -c(mn_sat, tuition, income, expnum, attr4_1, sinc4_1, intel4_1, fun4_1, amb4_1, shar4_1, attr5_1, sinc5_1, intel5_1, fun5_1, amb5_1))
missing_row <- Clean_Speed_Dating[!complete.cases(Clean_Speed_Dating),]
Clean_Speed_Dating <- na.omit(Clean_Speed_Dating)
all(complete.cases(Clean_Speed_Dating))
summary(Clean_Speed_Dating)
Clean_Speed_Dating$attr3_1_100 <- as.integer(Clean_Speed_Dating$attr3_1*10)
Clean_Speed_Dating$sinc3_1_100 <- as.integer(Clean_Speed_Dating$sinc3_1*10)
Clean_Speed_Dating$intel3_1_100 <- as.integer(Clean_Speed_Dating$intel3_1*10)
Clean_Speed_Dating$fun3_1_100 <- as.integer(Clean_Speed_Dating$fun3_1*10)
Clean_Speed_Dating$amb3_1_100 <- as.integer(Clean_Speed_Dating$amb3_1*10)
Clean_Speed_Dating <- subset(Clean_Speed_Dating, select = -c(attr3_1, sinc3_1, intel3_1, fun3_1, amb3_1))
sapply(Clean_Speed_Dating,class)
summary(Clean_Speed_Dating)

#Data Preprocessing
a <- Clean_Speed_Dating
b <- a %>% select(gender) %>% group_by(gender) %>% count() 
maximum <- max(b[,'n'])

#ui
ui <- dashboardPage(title = "Speed Dating App",
    dashboardHeader(title = span("NLTK", style = "font-family: Tahoma, Helvetica, sans-serif; font-weight: bold; font-size: 130%;")),
    dashboardSidebar(sidebarMenu(
        menuItem("Introduction", tabName = "Introduction", icon = icon("heart")),
        menuItem("Variable Analytics", tabName = "Variable", icon = icon("chart-line")),
        menuItem("Test Match", tabName = "Match", icon = icon("equals"))
    )),
    dashboardBody(
        tags$head(tags$style(HTML(".main-sidebar { font-size: 16px; }"))),
        tabItems(
            tabItem(tabName = "Introduction",
                    fluidRow(
                        column(width =3,
                               box(title = tags$p("Welcome to NLTK", style = "font-family: Helvetica; font-weight: bold; font-size: 30px;"),
                                   width = 12,
                                   height = 460,
                                   background = "light-blue",
                                   br(),
                                   img(src = "NLTK Logo.png", height = 60, width = 60),
                                   br(),
                                   br(),
                                   br(),
                                   tags$p("NLTK is established in 2021 by 4 passionate young men. It is a free and simple platform to meet new people. Anyone could find their best date by browsing simple dating information.",
                                          style = "font-size: 120%;"),
                                   align = "justify"),
                               box(title = tags$p("User Manual", style = "font-family: Helvetica; font-weight: bold; font-size: 30px;"),
                                   width = 12,
                                   height = 490,
                                   background = "teal",
                                   tags$li("Introduction tabs: A brief introduction of NLTK speed dating application. All reviews and testimonials are fictitious and are used for assignment purposes only.",
                                           style = "font-size: 100%;"),
                                   br(),
                                   tags$li("Variable Analytics: Exploratory data analysis allows users to better understand the current trend of well-known high-match date characteristics.",
                                           style = "font-size: 100%;"),
                                   br(),
                                   tags$li("Test Match: Adoption of random forest model to predict high-accuracy results for high-match date prediction.",
                                           style = "font-size: 100%;"),
                                   align = "justify"),
                        ),
                        box(title = tags$b("Meet your Life Partner in NLTK"), 
                            status = "info",
                            width = 4,
                            height = 460,
                            collapsible = TRUE,
                            solidHeader = TRUE,
                            br(),
                            slickROutput("slickr", width="300px"),
                            align = "center"),
                        box(title = tags$b("Rating & Review"), 
                            status = "info",
                            width = 4,
                            height = 460,
                            collapsible = TRUE,
                            solidHeader = TRUE,
                            img(src = "Rate&Review.png", height = 400, width = 300),
                            align = "left"),
                        box(title = tags$b("Testimonials from Past Customers"), 
                            status = "info",
                            width = 8,
                            height = 490,
                            collapsible = TRUE,
                            solidHeader = TRUE,
                            br(),
                            img(src = "Testimonials.png", height = 380, width = 670),
                            align = "center"),
                    )),
            tabItem(tabName = "Variable",
                    Info<- sidebarLayout(
                        sidebarPanel( width = 4, style = "font-size: 90%;", style = "color:blue",
                                      radioButtons("q", "Please select your desire exploratory descriptive analysis: ", 
                                                   list("Gender" = "a1", "Age" = "a2", "Race" = "a3", "Field of Study" = "a4", "Career" = "a5",
                                                        "Importance of Same Race" = "a6", "Importance of Same Religion" = "a7", "Goal" = "a8",
                                                        "Frequency of Date" = "a9", "Frequency of Go out" = "a10", "Preferred Activity" = "a11",
                                                        "Expectancy of Happiness with People during Date" = "a12",
                                                        "Score of Attributes of Opposite Sex that You are Looking for" = "a13",
                                                        "Score of Attributes which You Think Your Opposite Sex Looking for" = "a14",
                                                        "Self Score of Attributes" = "a15"))
                        ),
                        box(title = tags$b("Exploratory Data Analysis"), 
                            status = "info",
                            width = 8,
                            height = 470,
                            collapsible = TRUE,
                            solidHeader = TRUE,
                            br(),
                            plotOutput("plot"),
                            align = "center"),
                    )),
            tabItem(tabName = "Match",
                    fluidRow(
                        box(title = tags$b("Correlationship Analysis"),
                            status = "info",
                            width = 12,
                            height = 900,
                            collapsible = TRUE,
                            solidHeader = TRUE,
                            br(),
                            plotOutput("corr", height = 800, width = 900),
                            align = "center"),
                        box(title = tags$b("Predictive Model Results using Random Forest Algorithm"),
                            status = "info",
                            width = 12,
                            height = 700,
                            collapsible = TRUE,
                            solidHeader = TRUE,
                            img(src = "predict.PNG", height = 480, width = 670),
                            br(),
                            img(src = "cm.PNG", height = 150, width = 670),
                            align = "center"),
                    ))
                )
        ),
)    

#server
server <- function(input, output) {
     output$corr <- renderPlot({
        df <- data.frame(data.matrix(Clean_Speed_Dating))
        res <- cor(df)
        round(res, 2)
        corr <- corrplot(res, order = "hclust", tl.col = "black", tl.cex=0.7)
    })
    output$slickr <- renderSlickR({
        imgs <- list.files("A", pattern=".PNG", full.names = TRUE)
        slickR(imgs)
    })
    output$plot<- renderPlot({
        if(input$q=='a1'){
            a %>% select(gender) %>% 
                ggplot(aes(x=gender, fill=gender))+
                geom_bar(col='black')+
                geom_hline(yintercept = maximum, lty='dashed', col='sienna', lwd=1)+
                labs(title='Gender', x='Gender', y='Number of People')+
                scale_fill_manual(values = c('oldlace', 'navy'))+
                guides(fill=guide_legend('Gender'))+
                theme_classic()
        }
        else if(input$q=='a2'){
            a %>% 
                ggplot(aes(x=age, y=..count.., fill=gender))+
                geom_density(alpha=0.3)+
                labs(title='Age', x='Age', y='Number of People')+
                scale_fill_manual(values = c('tomato3', 'royalblue1'))+
                guides(fill=guide_legend('Gender'))+
                theme_classic()
        }
        else if(input$q=='a3'){
            a %>%
                ggplot(aes(x=race, fill=gender))+
                geom_bar(col='black', position = 'dodge')+
                labs(title='Race', x='Race', y='Number of People')+
                scale_fill_manual(values = c('violetred', 'royalblue'))+
                coord_flip()+
                guides(fill=guide_legend('Gender'))+
                theme_classic()
        }
        else if(input$q=='a4'){
            a %>%
                ggplot(aes(x=field_cd, fill=gender))+
                geom_bar(col='black', position = 'dodge')+
                labs(title='Field of study', x='Field', y='Number of People')+
                scale_fill_manual(values = c('powderblue', 'peachpuff'))+
                coord_flip()+
                guides(fill=guide_legend('Gender'))+
                theme_classic()
        }
        else if(input$q=='a5'){
            a %>%
                ggplot(aes(x=career_c, fill=gender))+
                geom_bar(col='black', position = 'dodge')+
                labs(title = 'Career', x = 'Career', y='Number of People')+
                scale_fill_manual(values = c('yellow', 'navy'))+
                coord_flip()+
                guides(fill=guide_legend('Gender'))+
                theme_classic()
        }
        else if(input$q=='a6'){
            a %>% 
                ggplot(aes(x=imprace, y=..count.., fill=gender))+
                geom_density(alpha=0.3)+
                labs(title='Importance of Same Race', x='Score (1-10)', 
                     y='Number of People')+
                scale_fill_manual(values = c('darkblue', 'orangered2'))+
                guides(fill=guide_legend('Gender'))+
                theme_classic()
        }
        else if(input$q=='a7'){
            a %>% 
                ggplot(aes(x=imprelig, y=..count.., fill=gender))+
                geom_density(alpha=0.3)+
                labs(title='Importance of Same Religion', x='Score (1-10)', 
                     y='Number of People')+
                scale_fill_manual(values = c('khaki1', 'deepskyblue1'))+
                guides(fill=guide_legend('Gender'))+
                theme_classic()
        }
        else if(input$q=='a8'){
            a %>% 
                ggplot(aes(x=goal, fill=gender)) +
                geom_bar(col='black', position = 'dodge')+
                labs(title = 'Goal', x='Goal', y='Number of People')+
                scale_fill_manual(values = c('thistle', 'firebrick2'))+
                coord_flip()+
                guides(fill=guide_legend('Gender'))+
                theme_classic()
        }
        else if(input$q=='a9'){
            a %>% 
                ggplot(aes(x=date, fill=gender)) +
                geom_bar(col='black', position = 'dodge')+
                labs(title = 'Date', x='Date', y='Number of People')+
                scale_fill_manual(values = c('rosybrown', 'royalblue'))+
                coord_flip()+
                guides(fill=guide_legend('Gender'))+
                theme_classic()
        }
        else if(input$q=='a10'){
            a %>% 
                ggplot(aes(x=go_out, fill=gender)) +
                geom_bar(col='black', position = 'dodge')+
                labs(title = 'Go Out', x='Go Out', y='Number of People')+
                scale_fill_manual(values = c('darkorchid2', 'darkorange2'))+
                coord_flip()+
                guides(fill=guide_legend('Gender'))+
                theme_classic()
        }
        else if(input$q=='a11'){
            activities <- c('sports', 'tvsports', 'exercise', 'dining', 'museums',
                            'art', 'hiking', 'gaming', 'clubbing', 'reading', 'tv', 
                            'theater', 'movies', 'concerts', 'music', 'shopping', 
                            'yoga')
            a %>% select(gender, activities) %>% group_by(gender) %>% 
                summarise(across(activities, mean)) %>%  
                pivot_longer(activities, names_to = 'activities', values_to = 'mean_score') %>% 
                ggplot(aes(x=activities,y =mean_score, fill=gender)) +
                geom_col(col='black', position = 'dodge')+
                labs(title = 'Preferred Activity', x='Activity', y= 'Mean Score')+
                scale_fill_manual(values = c('red3', 'darkblue'))+
                coord_flip()+
                guides(fill=guide_legend('Gender'))+
                theme_classic()
        }
        else if(input$q=='a12'){
            a %>% 
                ggplot(aes(x=exphappy, y=..count.., fill=gender))+
                geom_density(alpha=0.3)+
                scale_fill_manual(values = c('gold', 'firebrick4'))+
                labs(title = 'Expectancy of Hapiness with People during Date (Score 1-10)', 
                     x = 'Score (1-10)', y='Number of People')+
                guides(fill=guide_legend('Gender'))+
                theme_classic()
        }
        else if(input$q=='a13'){
            attr1 <- c('attr1_1', 'sinc1_1', 'intel1_1', 'fun1_1', 'amb1_1', 'shar1_1')
            c <- a %>% select(gender, attr1) %>% group_by(gender) %>% 
                summarise(across(attr1, mean)) %>%  
                pivot_longer(attr1, names_to = 'attributes', values_to = 'mean_score')
            c$attributes <- recode(c$attributes, attr1_1='attractive', sinc1_1='sincere', 
                                   intel1_1='intelligent', fun1_1 = 'fun', amb1_1='ambitious',
                                   shar1_1='shared interests/hobbies')
            c %>%  
                ggplot(aes(x=attributes, y =mean_score, fill=gender)) +
                geom_col(col='black', position = 'dodge')+
                labs(title = 'Score of Attributes of Opposite Sex that You are Looking For',
                     x='Attributes', y='Mean Score')+
                scale_fill_manual(values = c('mediumpurple3', 'lightcyan1'))+
                coord_flip()+
                guides(fill=guide_legend('Gender'))+
                theme_classic()
        }
        else if(input$q=='a14'){
            attr2 <- c('attr2_1', 'sinc2_1', 'intel2_1', 'fun2_1', 'amb2_1', 'shar2_1')
            d <- a %>% select(gender, attr2) %>% group_by(gender) %>% 
                summarise(across(attr2, mean)) %>%  
                pivot_longer(attr2, names_to = 'attributes', values_to = 'mean_score')
            d$attributes <- recode(d$attributes, attr2_1='attractive', sinc2_1='sincere', 
                                   intel2_1='intelligent', fun1_1 = 'fun', amb2_1='ambitious',
                                   shar2_1='shared interest')
            d %>% 
                ggplot(aes(x=attributes, y =mean_score, fill=gender)) +
                geom_col(col='black', position = 'dodge')+
                labs(title = 'Score of Attributes which You Think Your Opposite Sex
                     Looking For', x='Attributes', y='Mean Score')+
                scale_fill_manual(values = c('maroon3', 'dodgerblue3'))+
                coord_flip()+
                guides(fill=guide_legend('Gender'))+
                theme_classic()
        }
        else if(input$q=='a15'){
            attr3 <- c('attr3_1_100', 'sinc3_1_100', 'intel3_1_100', 'fun3_1_100', 
                       'amb3_1_100')
            e <- a %>% select(gender, attr3) %>% group_by(gender) %>% 
                summarise(across(attr3, mean)) %>%  
                pivot_longer(attr3, names_to = 'attributes', values_to = 'mean_score')
            
            e$attributes <- recode(e$attributes, attr3_1_100='attractive', sinc3_1_100=
                                       'sincere',intel3_1_100='intelligent', 
                                   fun3_1_100 = 'fun', amb3_1_100='ambitious')
            e %>% 
                ggplot(aes(x=attributes, y =mean_score, fill=gender)) +
                geom_col(col='black', position = 'dodge')+
                labs(title = 'Self Score of Attributes', x='Attributes', y='Mean Score')+
                scale_fill_manual(values = c('hotpink2', 'deepskyblue2'))+
                coord_flip()+
                guides(fill=guide_legend('Gender'))+
                theme_classic()
        }
    })
    
}

shinyApp(ui = ui, server = server)
