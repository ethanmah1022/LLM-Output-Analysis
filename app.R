library(tidyverse)
library(tidytext)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(quanteda.corpora)
library(quanteda.sentiment)
library(ggplot2)
library(shiny)
library(shinydashboard)

#Parameters
LLMs <- c("ChatGPT 3.5", "Microsoft Copilot", "Google Gemini",
          "Meta Llama 3", "Claude 3", "Le Chat Mistral")

scenarios <- c("Resolving Coworker Conflict","Applying For Newgrad Jobs")

dictionaries <- c("AFINN","ANEW","geninqposneg","HuLiu","LoughranMcDonald",
                  "LSD2015","NRC")

analysis_types <- c("Sentiment Analysis", "Frequency Analysis", "Lexical Diversity", "Author Similarity")

#LLM Texts
origin1 <- "I am extremely disappointed and frustrated that your part of the project remains incomplete. Your lack of progress has caused significant delays, impacting not only my work but also the entire team's progress. Your responsibilities were clear, and deadlines were agreed upon. Yet, you have failed to meet them. This is unacceptable and unprofessional. I demand that you take immediate action to complete your tasks and provide me with a detailed update on your progress by today."
origin2 <- "Hi, I'm a computer science grad looking for a role that puts my coding skills to good use. I've got a solid grasp of the fundamentals and a passion for problem-solving. I'm a bit quirky, but I'm also a hard worker who's always looking to improve. If you're looking for someone who's excited about tech and eager to learn, let's chat!"
gpt1 <- "Hi, I hope you are well. I wanted to touch base regarding your portion of the project. It appears that some tasks remain unfinished, which has affected our timeline and the progress of the entire team. I understand that challenges can arise, and I am here to support in any way I can. However, it's crucial that we adhere to our deadlines to meet our collective goals. Could you please provide an update on your current status and let me know how I can assist you in moving forward? Your timely response and action on this matter would be greatly appreciated. Thank you for your attention to this. Best regards,"
gpt2 <- "Hi, I hope this message finds you well. My name is, and I am a recent graduate with a degree in Computer Science. I am excited to apply for a software development role at where I can leverage my coding skills and passion for technology. Throughout my academic career, I have built a strong foundation in computer science principles and honed my programming abilities through various projects and internships. I am particularly skilled in, and I thrive on solving complex problems. I am enthusiastic about continuous learning and self-improvement, and I bring a unique, creative approach to my work. I am confident that my dedication and eagerness to contribute to innovative projects make me a valuable addition to your team. I would love the opportunity to discuss how my background and skills align with the needs of. Thank you for considering my application. I look forward to the possibility of contributing to your team. Best regards,"
copilot1 <- "Dear, I hope this email finds you well. I wanted to discuss the status of our project. It appears that there have been delays in completing your assigned tasks, which is impacting our overall progress. Could you please provide an update on your progress and any challenges you’re facing? Let’s work together to find a solution and ensure the project stays on track. Thank you for your attention to this matter. Best regards,"
copilot2 <- "Dear, I hope this email finds you well. As a recent computer science graduate, I am enthusiastic about leveraging my coding skills to contribute to a dynamic team. My solid grasp of fundamental concepts and passion for problem-solving drive my commitment to excellence. I thrive on challenges and am always eager to learn and improve. I would love the opportunity to discuss how my background aligns with your organization’s goals. If you’re open to it, I’d be grateful for a conversation. Thank you for considering my application. Best regards,"
gemini1 <- "Hi, I'm reaching out to follow up on your progress with for the project. As you know, completing this task on schedule is crucial for the overall timeline. I wanted to see if you've encountered any obstacles that might be causing delays. Is there anything I can do to assist you in finalizing your part? Perhaps a quick discussion could help streamline the process. It would be helpful if you could share an update on your progress and an estimated completion timeframe for your tasks. Thanks,"
gemini2 <- "Dear, I am writing to express my strong interest in positions at, as advertised on. As a recent graduate with a Bachelor of Science in Computer Science, I am eager to leverage my coding skills and problem-solving abilities in a fast-paced environment. During my studies, I gained a solid foundation in. I am particularly passionate about. In my academic projects, I. This experience allowed me to develop my . I am a highly motivated and quick learner who thrives in collaborative settings. I am confident that I can quickly adapt to new technologies and contribute meaningfully to your team. My resume is attached, which provides more details about my qualifications. Thank you for your time and consideration. I am available for an interview at your earliest convenience. Sincerely,"
llama1 <- "I wanted to follow up with you regarding your part of the project, which remains incomplete. I'm concerned that the delays are impacting not only my work but also the team's progress. As we previously discussed, the deadlines and responsibilities were clear. I understand that unexpected challenges can arise, but I would appreciate it if you could provide me with a detailed update on your progress and a plan to complete your tasks as soon as possible. Let's work together to get the project back on track."
llama2 <- "Dear Hiring Manager, I am excited to apply for a role at as a recent computer science graduate. With a strong foundation in programming principles and a passion for problem-solving, I am confident in my ability to make a valuable contribution to your team. Throughout my academic journey, I have developed a solid grasp of computer science fundamentals, including. My passion for technology and drive to continuously improve have led me to. I am eager to bring my skills and enthusiasm to a dynamic team and contribute to innovative projects. My strong work ethic and willingness to learn make me an excellent fit for a company that values growth and collaboration. Thank you for considering my application. I look forward to discussing my qualifications further. Best regards, "
claude1 <- "I want to follow up regarding your portion of the project. As you're aware, the agreed-upon deadline has passed, and your incomplete tasks are causing delays that impact the entire team's progress. I understand that unexpected challenges can arise, but it's crucial that we maintain open communication and adhere to our commitments as much as possible. Could you please provide me with an update on your current status and the remaining work needed to complete your assigned responsibilities? Additionally, I'd appreciate a realistic timeline for when you expect to have everything finished. Collaborating effectively is essential for our team's success, and I'm happy to discuss any support you might need to get back on track. I kindly request that you prioritize this matter and work diligently to resolve any outstanding issues. Please let me know if you foresee any further obstacles that could hinder your progress. Thank you for your prompt attention to this matter."
claude2 <- "Hi there, Thank you for reaching out regarding potential employment opportunities. Please allow me to introduce myself as a highly motivated and passionate recent graduate in Computer Science. Throughout my academic journey, I have acquired a strong foundation in coding and problem-solving skills, which I am eager to apply in a professional setting. I am particularly drawn to roles that challenge me intellectually and enable me to continuously enhance my technical expertise. My coursework and personal projects have honed my proficiency in various programming languages, data structures, and algorithms. Additionally, I possess excellent analytical and critical thinking abilities, which allow me to approach challenges with a solutions-oriented mindset. Beyond my technical qualifications, I am a dedicated and diligent individual who thrives in collaborative environments. I am always eager to learn from experienced professionals and contribute my unique perspectives to the team. My ability to adapt and think creatively has proven invaluable in tackling complex problems. I am confident that my skills, enthusiasm, and commitment to excellence would make me a valuable asset to your organization. I would welcome the opportunity to further discuss my qualifications and how I can contribute to your team's success. Thank you for your consideration, and I look forward to the possibility of an interview. Best regards, "
mistral1 <- "Dear, I hope this message finds you well. I am writing to discuss the status of our project, specifically the section that has been assigned to you. I have noticed that there seems to be some delay in the completion of your part. This has inadvertently affected the overall progress of the project, including my tasks and those of other team members, which are interconnected with yours. I understand that we all have our unique challenges and work paces, and it's possible that some unforeseen circumstances might have arisen. However, as we had previously agreed upon the deadlines and responsibilities, I believe it's crucial for us to stay on track to ensure the success of our project. To help us move forward, could you please provide me with an update on your progress by the end of the day? If there are any obstacles or issues that you're currently facing, I'm more than willing to discuss them with you and explore potential solutions. Our goal is to support each other and work together as a team. I appreciate your understanding and prompt attention to this matter. Let's continue to work diligently and professionally to meet our project objectives. Best regards,"
mistral2 <- "Dear, I hope this message finds you well. I am writing to express my interest in exploring potential job opportunities within your organization. As a recent graduate in Computer Science, I am eager to apply my strong foundation in programming and problem-solving to a dynamic and innovative team. During my academic career, I have honed my skills in various programming languages and gained a deep understanding of computer science fundamentals. I have consistently been recognized for my dedication, creativity, and ability to approach challenges from unique perspectives. In addition to my technical skills, I bring a passion for continuous learning and improvement. I am confident that my strong work ethic, adaptability, and enthusiasm for technology would make me a valuable asset to your team. I would greatly appreciate the opportunity to further discuss how my background and skills align with your organization's goals and needs. Please let me know if there is a suitable time for us to connect. Thank you for considering my application. I look forward to the possibility of contributing to your team's success. Best regards,"

corp1 <- corpus(c(origin1,gpt1,copilot1,gemini1,llama1,claude1,mistral1),docvars = data.frame(LLM = c("Original","GPT-4", "Copilot", "Gemini", "Llama3", "Claude3", "Mistral")))
corp2 <- corpus(c(origin2,gpt2,copilot2,gemini2,llama2,claude2,mistral2),docvars = data.frame(LLM = c("Original","GPT-4", "Copilot", "Gemini", "Llama3", "Claude3", "Mistral")))

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "LLM Output Analysis"),
  dashboardSidebar(
    sidebarMenu(
      selectInput("task_select", "Select Scenario:", choices = scenarios),
      menuItem("Text Outputs", tabName = "text_tab", icon = icon("file-alt")),
      menuItem("Analysis", tabName = "analysis_tab", icon = icon("chart-bar"))
    ),
    selectInput("dictionary_select", "Select Lexicon Dictionary:", choices = dictionaries),
    selectInput("analysis_select", "Select Analysis Type:", choices = analysis_types),
    actionButton("analyze_button", "Analyze")
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "analysis_tab",
              uiOutput("dynamic_ui")
      ),
      tabItem(tabName = "text_tab",
              uiOutput("text_boxes_ui")
      )
    )
  )
)


# Define server logic
server <- function(input, output, session) {
  
  # Frequency Analysis
  frequency_analysis <- function(dfmat) {
    req(dfmat)
    tstat_freq <- textstat_frequency(dfmat, n = 10)
    
    list(
      freq_table = renderTable({ head(tstat_freq, 20) }),
      freq_plot = renderPlot({
        dfmat %>% 
          textstat_frequency(n = 15) %>% 
          ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
          geom_point() +
          coord_flip() +
          labs(x = NULL, y = "Frequency") +
          theme_minimal()
      }),
      wordcloud_plot = renderPlot({
        set.seed(132)
        textplot_wordcloud(dfmat, max_words = 100)
      })
    )
  }
  
  # Lexical Diversity Analysis
  lexical_diversity_analysis <- function(dfmat) {
    req(dfmat)
    tstat_lexdiv <- textstat_lexdiv(dfmat)
    
    renderPlot({
      plot(tstat_lexdiv$TTR, type = "l", xaxt = "n", xlab = NULL, ylab = "TTR")
      grid()
      axis(1, at = seq_len(nrow(tstat_lexdiv)), labels = dfmat$LLM)
    })
  }
  
  # Author Similarity Analysis
  author_similarity_analysis <- function(dfmat) {
    req(dfmat)
    tstat_dist <- as.dist(textstat_dist(dfmat))
    clust <- hclust(tstat_dist)
    
    renderPlot({
      plot(clust, xlab = "Distance", ylab = NULL, labels = dfmat$LLM, main = "LLM Output Similarity")
    })
  }
  
  # Sentiment Analysis
  sentiment_analysis <- function(dfmat,dictionary){
    req(dfmat)
    if (dictionary == "AFINN" | dictionary == "ANEW"){
      sentiment_scores <- textstat_valence(dfmat, dictionary = get(paste0("data_dictionary_",dictionary)))
    } else {
      sentiment_scores <- textstat_polarity(dfmat, dictionary = get(paste0("data_dictionary_",dictionary)))
    }
    sentiment_df <- data.frame(
      LLM = docvars(dfmat, "LLM"),
      sentiment = sentiment_scores$sentiment
    )
    renderPlot({
      ggplot(sentiment_df, aes(x = LLM, y = sentiment, fill = LLM)) +
        geom_bar(stat = "identity") +
        theme_minimal() +
        labs(title = "Sentiment Analysis of LLM Outputs",
             x = "LLM",
             y = "Sentiment Score") +
        scale_fill_brewer(palette = "Set3")
    })
    
  }
  
  # Observe event when the Analyze button is clicked in Analysis
  observeEvent(input$analyze_button, {
    req(input$task_select, input$analysis_select)
    
    # Select corpus based on scenario
    corp <- if (input$task_select == "Resolving Coworker Conflict") {
      corp1
    } else if (input$task_select == "Applying For Newgrad Jobs") {
      corp2
    }
    req(corp)
    toks <- tokens(corp, remove_punct = TRUE) %>%
      tokens_remove(pattern = stopwords("en")) %>%
      tokens_remove("I")
    dfmat <- dfm(toks)
    
    if (input$analysis_select == "Frequency Analysis") {
        results <- frequency_analysis(dfmat)
        output$dynamic_ui <- renderUI({
          tabsetPanel(
            tabPanel("Frequency Table", tableOutput("tstat_freq_table")),
            tabPanel("Frequency Plot", plotOutput("freq_plot")),
            tabPanel("Wordcloud", plotOutput("wordcloud_plot"))
          )
        })
        output$tstat_freq_table <- results$freq_table
        output$freq_plot <- results$freq_plot
        output$wordcloud_plot <- results$wordcloud_plot
    } else if (input$analysis_select == "Lexical Diversity") {
          output$dynamic_ui <- renderUI({
            tabsetPanel(
              tabPanel("TTR Line Plot", plotOutput("ttr_plot"))
            )
          })
          output$ttr_plot <- lexical_diversity_analysis(dfmat)
    } else if (input$analysis_select == "Author Similarity"){
          output$dynamic_ui <- renderUI({
            tabsetPanel(
              tabPanel("Author Similarity Plot", plotOutput("author_similarity_plot"))
            )
          })
          output$author_similarity_plot <- author_similarity_analysis(dfmat)
    } else if (input$analysis_select == "Sentiment Analysis") {
      output$dynamic_ui <- renderUI({
        tabsetPanel(
          tabPanel("Sentiment Analysis", plotOutput("sentiment_plot"))
        )
      })
      output$sentiment_plot <- sentiment_analysis(corp, input$dictionary_select)
    }
  })
  
  # Observe event when looking at text outputs
  observeEvent(input$task_select, {
    req(input$task_select)
    
    # Select corpus based on scenario
    corp <- if (input$task_select == "Resolving Coworker Conflict") {
      corp1
    } else if (input$task_select == "Applying For Newgrad Jobs") {
      corp2
    }
    req(corp)
    
    # Generate text in individual text boxes for original and each LLM
    # Original text box is not collapsed
    output$text_boxes_ui <- renderUI({
      lapply(seq_along(corp), function(i) {
        box(
          title = docvars(corp)[i, "LLM"],
          textOutput(paste0("text_", i)),
          width = 12,
          collapsible = TRUE,
          collapsed = docvars(corp)[i, "LLM"] != "Original"
        )
      })
    })
    
    lapply(seq_along(corp), function(i) {
      output[[paste0("text_", i)]] <- renderText({
        as.character(corp)[i]
      })
    })
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

