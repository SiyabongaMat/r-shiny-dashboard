library(tidyverse)

# Question 1

df <- read_csv("graduate_survey.csv")

# 1.a)

df2 <- df %>%
  select(Campus, StudyField, Branch, Role, EduLevel, ProgLang, Databases, Platform,
         WebFramework, Industry, AISearch, AITool, Employment)

# 1. b)

data <- na.omit(df2)

# 1. c)

data$Campus <- gsub("Umhlanga", "Durban", data$Campus)

# 1. d)

final_data <- data %>%
  count(Campus, sort = TRUE) %>%
  head(5)

subset_df <- data %>%
  filter(Campus %in% final_data$Campus)



# Question 2

library(ggplot2)

# 1.i

dev_tools = subset_df %>%
  select(ProgLang, Databases, AISearch, AITool, WebFramework, Platform)

# Programming languages

prog_langs <- sapply(dev_tools$ProgLang, function(x) strsplit(x, split = ";")[[1]])

langs <- unlist(prog_langs)

langs_count <- table(langs)

langs_df <- as.data.frame(langs_count)

pop_lang <- ggplot(langs_df, aes(x = langs, y = Freq)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme(axis.text.x = element_text(angle = 70, size = 8, vjust = 0.5)) +
  labs(y = "", x = "Programming Languages", title = "Programming Languages Popularity")

# Databases

databases <- sapply(dev_tools$Databases, function(x) strsplit(x, split = ";")[[1]])

database <- unlist(databases)

langs_count <- table(database)

databases_df <- as.data.frame(langs_count)

pop_databases <- ggplot(databases_df, aes(x = database, y = Freq)) +
  geom_bar(stat = "identity", fill = "red") +
  theme(axis.text.x = element_text(angle = 70, size = 8, vjust = 0.5)) +
  labs(y = "", x = "Databases", title = "Databases Popularity")

# AI Search

ai_search <- sapply(dev_tools$AISearch, function(x) strsplit(x, split = ";")[[1]])

ai_s <- unlist(ai_search)

ai_search_count <- table(ai_s)

ai_s_df <- as.data.frame(ai_search_count)

pop_aisearch <- ggplot(ai_s_df, aes(x = ai_s, y = Freq)) +
  geom_bar(stat = "identity", fill = "cyan") +
  theme(axis.text.x = element_text(angle = 70, size = 8, vjust = 0.5)) +
  labs(y = "", x = "AI Search Tools", title = "AI Search Tools Popularity")

# AI Tools

ai_tools <- sapply(dev_tools$AITool, function(x) strsplit(x, split = ";")[[1]])

ai_t <- unlist(ai_tools)

ai_tools_count <- table(ai_t)

ai_t_df <- as.data.frame(ai_tools_count)

pop_aitools <- ggplot(ai_t_df, aes(x = ai_t, y = Freq)) +
  geom_bar(stat = "identity", fill = "violet") +
  theme(axis.text.x = element_text(angle = 70, size = 8, vjust = 0.5)) +
  labs(y = "", x = "AI Tools", title = "AI Tools Popularity")

# Platforms

platforms <- sapply(dev_tools$Platform, function(x) strsplit(x, split = ";")[[1]])

platform <- unlist(platforms)

platform_count <- table(platform)

platforms_df <- as.data.frame(platform_count)

pop_platforms <- ggplot(platforms_df, aes(x = platform, y = Freq)) +
  geom_bar(stat = "identity", fill = "gold") +
  theme(axis.text.x = element_text(angle = 70, size = 8, vjust = 0.5)) +
  labs(y = "", x = "Platforms", title = "Platforms Popularity")


# Web Frameworks

web_frameworks <- sapply(dev_tools$WebFramework, function(x) strsplit(x, split = ";")[[1]])

frameworks <- unlist(web_frameworks)

framework_count <- table(frameworks)

framework_df <- as.data.frame(framework_count)

pop_web <- ggplot(framework_df, aes(frameworks, Freq)) +
  geom_bar(stat = "identity", fill = "yellow") +
  theme(axis.text.x = element_text(angle = 70, size = 8, vjust = 0.5)) +
  labs(y = "", x = "Web Frameworks", title = "Web Frameworks Popularity")

# 1. ii

# Industry

industry <- subset_df %>%
  select(Industry)

industries <- sapply(industry$Industry, function(x) strsplit(x, split = ",")[[1]])

ind <- unlist(industries)

industry_count <- table(ind)

industry_df <- as.data.frame(industry_count)

pop_industry <- ggplot(industry_df, aes(ind, Freq)) +
  geom_bar(stat = "identity", fill = "green") +
  theme(axis.text.x = element_text(angle = 70, size = 8, vjust = 0.5)) +
  labs(y = "", x = "Industries", title = "Graduate Industries")

# 1. iii

# Roles

role <- subset_df %>%
  select(Role)

role <- sapply(role$Role, function(x) strsplit(x, split = ",")[[1]])

role_ <- unlist(role)

role_count <- table(role_)

roles_df <- as.data.frame(role_count)

pop_roles <- ggplot(roles_df, aes(role_, Freq)) +
  geom_bar(stat = "identity", fill = "grey") +
  theme(axis.text.x = element_text(angle = 70, size = 8, vjust = 0.5)) +
  labs(y = "", x = "Roles", title = "Graduate Roles")

# 1. iv

# Employment vs Unemployment

subset_df$Employment <- sapply(subset_df$Employment, function(x) gsub(",", 
                                                                      ";", x))

employment_field <- subset_df %>%
  select(StudyField, Employment)

employment_status <- employment_field %>%
  mutate(Employment = strsplit(Employment, ";")) %>%
  unnest(Employment)

grouped_employment_studyfield <- employment_status %>%
  filter(Employment == "Employed" | Employment == "Not employed") %>%
  group_by(Employment, StudyField) %>%
  count()

emp_unemp <- ggplot(grouped_employment_studyfield, aes(StudyField, n, fill = Employment)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Field of Study", title = "Employed vs Unemployed in each study field")



# Question 3

library(shiny)
library(shinydashboard)
library(shinythemes)

ui <- fluidPage(theme = shinytheme("readable"),
                navbarPage(
                  "Developer Tools Graphs",
                  tabPanel("Languages",
                           mainPanel(
                             h3("Popular programming languages among graduates"),
                             plotOutput("langPlot")
                           )
                  ),
                  tabPanel("Databases",
                           mainPanel(
                             h3("Popular databases among graduates"),
                             plotOutput("dataPlot")
                           )
                  ),
                  tabPanel("AI Search",
                           mainPanel(
                             h3("Popular AI Search among graduates"),
                             plotOutput("aiSPlot")
                           )
                  ),
                  tabPanel("AI Tools",
                           mainPanel(
                             h3("Popular AI Tools among graduates"),
                             plotOutput("aiTPlot")
                           )
                  ),
                  tabPanel("Platforms",
                           mainPanel(
                             h3("Popular platforms among graduates"),
                             plotOutput("platfPlot")
                           )
                  ),
                  tabPanel("Frameworks",
                           mainPanel(
                             h3("Popular web frameworks among graduates"),
                             plotOutput("webPlot")
                           )
                  ),
                  tabPanel("Industries",
                           mainPanel(
                             h3("Popular industries among graduates"),
                             plotOutput("indPlot")
                           )
                  ),
                  tabPanel("Roles",
                           mainPanel(
                             h3("Popular roles among graduates"),
                             plotOutput("rolesPlot")
                           )
                  ),
                  tabPanel("Employment vs Unemployment",
                           mainPanel(
                             h3("Employability among study fields"),
                             plotOutput("vsPlot")
                           )
                  ),
                )
                
)

server <- function(input, output) {
  output$langPlot <- renderPlot({
    pop_lang
  })
  
  output$dataPlot <- renderPlot({
    pop_databases
  })
  
  output$aiSPlot <- renderPlot({
    pop_aisearch
  })
  
  output$aiTPlot <- renderPlot({
    pop_aitools
  })
  
  output$platfPlot <- renderPlot({
    pop_platforms
  })
  
  output$webPlot <- renderPlot({
    pop_web
  })
  
  output$indPlot <- renderPlot({
    pop_industry
  })
  
  output$rolesPlot <- renderPlot({
    pop_roles
  })
  
  output$vsPlot <- renderPlot({
    emp_unemp
  })
}

shinyApp(ui, server)
