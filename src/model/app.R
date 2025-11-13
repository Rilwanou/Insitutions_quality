library(shiny)
library(readxl)
library(dplyr)
library(AER)
library(modelsummary)
library(gt)

ui <- fluidPage(

  titlePanel("Résulats de l'étude d'Acemoglu"),
  
  sidebarLayout(
    
    sidebarPanel(
      h3("Contrôles"),
      p("Utilisez ce menu pour sélectionner les résulats de la table de régression que vous souhaitez afficher."),
      
      selectInput("table_choice", 
                  label = "Choisir la table à afficher:",
                  choices = c(
                    "Table 4 IV-Regression of log gdp per capita",
                    "Table OLS 1995",
                    "Table First Stage 1995",
                    "Panel A: Two-Stage Least Squares 1995",
                    "Extension Rapport 2019",
                    "Table OLS 2019",
                    "Table 2SLS (IV) 2019"
                  ),
                  selected = "Table 2SLS 1995")
    ),
    

    mainPanel(
      h2("Résultats de la régression"),
      p("Le tableau ci-dessous recrée le style de l'article (coefficients, erreurs-types entre parenthèses, et étoiles)."),
      
      gt::gt_output(outputId = "model_table")
    )
  )
)

server <- function(input, output, session) {
  
  list_ols_1995 <- list(
    "(1)" = ols_c1, "(2)" = ols_c2, "(3)" = ols_c3, "(4)" = ols_c4,
    "(5)" = ols_c5, "(6)" = ols_c6, "(7)" = ols_c7, "(8)" = ols_c8,
    "(9)" = ols_c9
  )
  
  list_fs_1995 <- list(
    "(1)" = fs_b1, "(2)" = fs_b2, "(3)" = fs_b3, "(4)" = fs_b4,
    "(5)" = fs_b5, "(6)" = fs_b6, "(7)" = fs_b7, "(8)" = fs_b8,
    "(9)" = fs_b9
  )
  
  list_iv_1995 <- list(
    "Base_sample (1)" = iv_a1, "Base_sample (2)" = iv_a2, "Basesample without Neo-Europes (3)" = iv_a3, "Basesample without Neo-Europes (4)" = iv_a4,
    "Base sample without Africa (5)" = iv_a5, "Base sample without Africa (6)" = iv_a6, "Base sample with continent dummies (7)" = iv_a7, "Base sample with continent dummies (8)" = iv_a8,
    "Base sample, dependent variableis logoutput perworker (9)" = iv_a9
  )
  
  # Listes pour 2019
  list_ols_2019 <- list(
    "(1)" = ols_2019_1, "(2)" = ols_2019_2, "(3)" = ols_2019_3, "(4)" = ols_2019_4,
    "(5)" = ols_2019_5, "(6)" = ols_2019_6, "(7)" = ols_2019_7, "(8)" = ols_2019_8,
    "(9)" = ols_2019_9
  )
  
  list_iv_2019 <- list(
    "(1)" = iv_2019_1, "(2)" = iv_2019_2, "(3)" = iv_2019_3, "(4)" = iv_2019_4,
    "(5)" = iv_2019_5, "(6)" = iv_2019_6, "(7)" = iv_2019_7, "(8)" = iv_2019_8,
    "(9)" = iv_2019_9
  )
  
  output$model_table <- gt::render_gt({
    
    table_to_show <- switch(input$table_choice,
                            "Table OLS 1995 (Panel C)" = list_ols_1995,
                            "Table First Stage 1995 (Panel B)" = list_fs_1995,
                            "Table 2SLS (IV) 1995 (Panel A)" = list_iv_1995,
                            "Table OLS 2019" = list_ols_2019,
                            "Table 2SLS (IV) 2019" = list_iv_2019,
                            list_iv_1995
    )
    dynamic_title <- input$table_choice

    modelsummary(
      table_to_show,
      output = "gt",
      stars = TRUE,
      statistic = "std.error",
      gof_map = c("nobs", "r.squared"),
      title = dynamic_title
    ) 
  })
}
shinyApp(ui = ui, server = server)