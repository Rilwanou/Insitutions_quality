# --- UI (Interface Utilisateur) ---
# Définit l'apparence de la page web
ui <- fluidPage(
  # Titre de l'application
  titlePanel("Réplication de l'étude Acemoglu et al. (2001)"),
  
  # Layout avec une barre latérale
  sidebarLayout(
    
    # Panneau de la barre latérale (pour les contrôles)
    sidebarPanel(
      h3("Contrôles"),
      p("Utilisez ce menu pour sélectionner la table de régression que vous souhaitez afficher."),
      
      # Menu déroulant pour choisir la table
      selectInput("table_choice", 
                  label = "Choisir la table à afficher:",
                  choices = c(
                    "--- Table 4 (Article 1995) ---",
                    "Table OLS 1995 (Panel C)",
                    "Table First Stage 1995 (Panel B)",
                    "Table 2SLS (IV) 1995 (Panel A)",
                    "--- Extension (Rapport 2019) ---",
                    "Table OLS 2019",
                    "Table 2SLS (IV) 2019"
                  ),
                  selected = "Table 2SLS (IV) 1995 (Panel A)") # Sélection par défaut
    ),
    
    # Panneau principal (pour afficher les résultats)
    mainPanel(
      h2("Résultats de la régression"),
      p("Le tableau ci-dessous recrée le style de l'article (coefficients, erreurs-types entre parenthèses, et étoiles)."),
      
      # La sortie de la table sera rendue ici
      gt::gt_output(outputId = "model_table")
    )
  )
)

# --- Server (Logique du Backend) ---
# Définit comment l'application réagit aux actions de l'utilisateur
server <- function(input, output, session) {
  
  # 1. Mettre tous les modèles dans des listes (pour 'modelsummary')
  # Ces objets (ols_c1, iv_a1, etc.) sont chargés automatiquement 
  # depuis le fichier global.R
  
  # Listes pour 1995
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
    "(1A)" = iv_a1, "(2)" = iv_a2, "(3)" = iv_a3, "(4)" = iv_a4,
    "(5)" = iv_a5, "(6)" = iv_a6, "(7)" = iv_a7, "(8)" = iv_a8,
    "(9)" = iv_a9
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
  
  # 2. Réagir au choix de l'utilisateur
  output$model_table <- gt::render_gt({
    
    # 'switch' est une façon propre de faire un 'if/else'
    # En fonction du choix, il sélectionne la bonne liste de modèles
    table_to_show <- switch(input$table_choice,
                            "Table OLS 1995 (Panel C)" = list_ols_1995,
                            "Table First Stage 1995 (Panel B)" = list_fs_1995,
                            "Table 2SLS (IV) 1995 (Panel A)" = list_iv_1995,
                            "Table OLS 2019" = list_ols_2019,
                            "Table 2SLS (IV) 2019" = list_iv_2019,
                            list_iv_1995 # (Sécurité en cas de choix non valide)
    )
    dynamic_title <- input$table_choice
    # 3. Générer la table
    # On force la sortie en "gt" ET on ajoute le style "stargazer"
    modelsummary(
      table_to_show, 
      output = "gt",
      stars = TRUE,                 # <-- Ajoute les étoiles (***)
      statistic = "std.error",      # <-- Met les (erreurs-types) en dessous
      gof_map = c("nobs", "r.squared"), # <-- Nettoie les stats du bas
      title = dynamic_title
    ) 
  })
}

# --- Lancement de l'Application ---
# Combine l'UI et le Server pour lancer l'application
shinyApp(ui = ui, server = server)