################################################################################
#
#             Application d'analyse de données
#
################################################################################

Sys.setlocale("LC_ALL", "French")

################################################################################
#
#  Utilitaire.R
# Ce Bloc contient les fonctions utilitaires, les interprétations 
# et les helpers pour l'application.
#
################################################################################


# ---- Gestion des packages ----
install_and_load <- function(packages) {
  installed_packages <- installed.packages()[, "Package"]
  to_install <- packages[!packages %in% installed_packages]
  if (length(to_install) > 0) install.packages(to_install, repos = "https://cran.r-project.org")
  for (pkg in packages) suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

required_packages <- c(
  "shiny", "shinydashboard", "DT", "ggplot2", "haven", "openxlsx",
  "car", "agricolae", "RColorBrewer", "dplyr", "qqplotr", "tidyr", "forcats",
  "shinyjs", "shinyWidgets", "bslib", "shinyalert", "plotly", "corrplot",
  "rmarkdown", "knitr", "factoextra", "cluster", "GGally", "psych", "nortest",
  "lmtest", "multcomp", "emmeans", "performance", "see", "report", "FactoMineR",
  "stats", "purrr", "stringr", "ggdendro", "reshape2", "MASS", "PMCMRplus",
  "multcompView", "ggrepel", "FSA", "rcompanion", "plotrix", "tibble","questionr"
)

install_and_load(required_packages)

# Petit utilitaire
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# Fonction d'interprétation des p-values
interpret_p_value <- function(p_value) {
  if (is.na(p_value)) {
    return("NA")
  } else if (p_value < 0.001) {
    return("Hautement significatif (p < 0.001)")
  } else if (p_value < 0.01) {
    return("Très significatif (p < 0.01)")
  } else if (p_value < 0.05) {
    return("Significatif (p < 0.05)")
  } else {
    return("Non significatif (p >= 0.05)")
  }
}

# Fonction pour interpréter les résultats statistiques
interpret_test_results <- function(test_type, p_value, test_object = NULL) {
  if (is.na(p_value)) return("Résultat non disponible")
  
  significance <- ifelse(p_value < 0.05, "significative", "non significative")
  
  switch(test_type,
         "t.test" = paste0("Le test t montre une différence ", significance, " entre les groupes (p = ", round(p_value, 8), ")"),
         "wilcox.test" = paste0("Le test de Wilcoxon montre une différence ", significance, " entre les groupes (p = ", round(p_value, 8), ")"),
         "anova" = paste0("L'ANOVA montre une différence ", significance, " entre les groupes (p = ", round(p_value, 8), ")"),
         "kruskal.test" = paste0("Le test de Kruskal-Wallis montre une différence ", significance, " entre les groupes (p = ", round(p_value, 8), ")"),
         "scheirerRayHare" = paste0("Le test de Scheirer-Ray-Hare montre une différence ", significance, " entre les groupes (p = ", round(p_value, 8), ")"),
         "chisq.test" = paste0("Le test du chi² montre une association ", significance, " entre les variables (p = ", round(p_value, 8), ")"),
         "cor.test" = paste0("La corrélation est ", significance, " (p = ", round(p_value, 8), ")"),
         paste0("Le test ", test_type, " montre un résultat ", significance, " (p = ", round(p_value, 8), ")")
  )
}

# Fonctions d'interprétation pour la normalité et l'homogénéité
interpret_normality <- function(p_value) {
  if (is.na(p_value)) return("Résultat non disponible")
  if (p_value >= 0.05) {
    return(paste0("La distribution est normale (p = ", round(p_value, 8), " >= 0.05)"))
  } else {
    return(paste0("La distribution n'est pas normale (p = ", round(p_value, 8), " < 0.05)"))
  }
}

interpret_homogeneity <- function(p_value) {
  if (is.na(p_value)) return("Résultat non disponible")
  if (p_value >= 0.05) {
    return(paste0("Les variances sont homogènes (p = ", round(p_value, 8), " >= 0.05)"))
  } else {
    return(paste0("Les variances ne sont pas homogènes (p = ", round(p_value, 8), " < 0.05)"))
  }
}

# Fonction pour interpréter la normalité des résidus
interpret_normality_resid <- function(p_value) {
  if (is.na(p_value)) return("Test non applicable")
  if (p_value > 0.05) {
    return("Les résidus suivent une distribution normale (p > 0.05). Les conditions pour les tests paramétriques sont respectées.")
  } else {
    return("Les résidus ne suivent pas une distribution normale (p < 0.05). Considérez l'utilisation de tests non-paramétriques.")
  }
}

# Fonction pour interpréter l'homogénéité des résidus
interpret_homogeneity_resid <- function(p_value) {
  if (is.na(p_value)) return("Test non applicable")
  if (p_value > 0.05) {
    return("Les variances sont homogènes (p > 0.05). Les conditions pour les tests paramétriques sont respectées.")
  } else {
    return("Les variances ne sont pas homogènes (p < 0.05). Utilisez des tests robustes à l'hétérogénéité des variances.")
  }
}

# Fonction pour filtrage croisé complet (2 facteurs)
filter_complete_cross <- function(df, A, B, reqA = TRUE, reqB = FALSE) {
  df <- df[, , drop = FALSE]
  if (!is.factor(df[[A]])) df[[A]] <- factor(df[[A]])
  if (!is.factor(df[[B]])) df[[B]] <- factor(df[[B]])
  changed <- TRUE
  while (changed) {
    tab <- table(droplevels(df[[A]]), droplevels(df[[B]]))
    keepA <- rownames(tab)[apply(tab > 0, 1, all)]
    keepB <- colnames(tab)[apply(tab > 0, 2, all)]
    old_n <- nrow(df)
    if (reqA) df <- df[df[[A]] %in% keepA, , drop = FALSE]
    if (reqB) df <- df[df[[B]] %in% keepB, , drop = FALSE]
    df <- droplevels(df)
    changed <- nrow(df) < old_n
  }
  return(df)
}

# Fonction pour filtrage croisé complet (N facteurs)
filter_complete_cross_n <- function(df, factors) {
  if (length(factors) < 2) return(df)
  dfx <- df
  for (f in factors) if (!is.factor(dfx[[f]])) dfx[[f]] <- factor(dfx[[f]])
  changed <- TRUE
  while (changed) {
    cnt <- dfx %>% dplyr::count(dplyr::across(dplyr::all_of(factors)), name = "n", .drop = FALSE)
    full <- tidyr::complete(cnt, tidyr::nesting(!!!rlang::syms(factors)))
    miss <- full[is.na(full$n), , drop = FALSE]
    if (nrow(miss) == 0) break
    levels_to_drop <- lapply(factors, function(f) unique(miss[[f]]))
    names(levels_to_drop) <- factors
    old_n <- nrow(dfx)
    for (f in factors) {
      dfx <- dfx[!(dfx[[f]] %in% levels_to_drop[[f]]), , drop = FALSE]
    }
    dfx <- droplevels(dfx)
    changed <- nrow(dfx) < old_n
    if (nrow(dfx) == 0) break
  }
  dfx
}

# Fonction pour calcul du CV
calc_cv <- function(x) sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) * 100

################################################################################
#
# Interface UI de l'application Shiny
#
#
################################################################################


ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = span(icon("flask"), "Analyse agronomique"), 
    titleWidth = 300,
    dropdownMenu(
      type = "notifications", 
      badgeStatus = "info",
      notificationItem(
        text = "L'application est prête",
        icon = icon("check-circle"),
        status = "success"
      )
    )
  ),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "tabs",
      menuItem("Chargement", tabName = "load", icon = icon("upload")),
      menuItem("Exploration", tabName = "explore", icon = icon("binoculars")),
      menuItem("Nettoyage", tabName = "clean", icon = icon("broom")),
      menuItem("Filtrage", tabName = "filter", icon = icon("filter")),
      menuItem("Analyses descriptives", tabName = "descriptive", icon = icon("chart-bar")),
      menuItem("Analyses multivariées", tabName = "multivariate", icon = icon("project-diagram")),
      menuItem("Réalisation des tests statistiques", tabName = "tests", icon = icon("calculator")),
      menuItem("Comparaisons multiples PostHoc", tabName = "multiple", icon = icon("sort-amount-down")),
      menuItem("Tableaux croisés", tabName = "crosstab", icon = icon("table")),
      menuItem("Visualisation des données", tabName = "visualization", icon = icon("chart-line")),
      menuItem("Rapport de synthèse des analyses", tabName = "report", icon = icon("file-alt")),
      hr(),
      actionButton("helpBtn", "Aide", icon = icon("question-circle"), class = "btn-info"),
      actionButton("resetBtn", "Réinitialiser", icon = icon("redo"), class = "btn-warning")
    )
  ),
  dashboardBody(
    useShinyjs(),
    useShinyalert(force = TRUE),
    tags$head(
      tags$style(HTML("
        .box-title { font-weight: bold; }
        .btn { margin-right: 5px; }
        .progress-bar { background-color: #3c8dbc; }
        .nav-tabs-custom > .nav-tabs > li.active { border-top-color: #3c8dbc; }
        .info-box { min-height: 80px; }
        .info-box-icon { height: 80px; line-height: 80px; }
        .info-box-content { padding: 10px; }
        .small-box { border-radius: 5px; }
        .main-header .logo { font-weight: bold; }
        .interpretation-box { background-color: #f9f9f9; border-left: 4px solid #3c8dbc; padding: 10px; margin-top: 10px; }
      "))
    ),
    tabItems(
      # ---- Chargement ----
      tabItem(tabName = "load",
              fluidRow(
                box(title = "Charger données", status = "primary", width = 12, solidHeader = TRUE,
                    fileInput("file", "Choisir un fichier",
                              accept = c(".csv", ".xlsx", ".xls", ".txt", ".sav", ".dta", ".rds")),
                    uiOutput("sheetUI"),
                    radioButtons("sep", "Séparateur (CSV/TXT)",
                                 choices = c(Virgule = ",", `Point-virgule` = ";", Tab = "\t"), selected = ","),
                    checkboxInput("header", "Avec en-têtes", TRUE),
                    actionButton("loadData", "Charger", class = "btn-primary", icon = icon("upload")),
                    hr(),
                    h4("Exemple de données"),
                    p("Vous pouvez utiliser vos propres données ou télécharger un exemple pour tester l'application:"),
                    downloadButton("downloadExample", "Télécharger exemple (CSV)", class = "btn-info")
                )
              ),
              fluidRow(
                valueBoxOutput("nrowBox", width = 3),
                valueBoxOutput("ncolBox", width = 3),
                valueBoxOutput("naBox", width = 3),
                valueBoxOutput("memBox", width = 3)
              ),
              fluidRow(
                box(title = "Aperçu des données", status = "info", width = 12, solidHeader = TRUE,
                    DTOutput("preview"))
              )
      ),
      
      # ---- Exploration ----
      tabItem(tabName = "explore",
              fluidRow(
                box(title = "Structure des données", status = "info", width = 6, solidHeader = TRUE,
                    verbatimTextOutput("dataStructure")
                ),
                box(title = "Résumé statistique", status = "info", width = 6, solidHeader = TRUE,
                    verbatimTextOutput("dataSummary")
                )
              ),
              fluidRow(
                box(title = "Matrice de corrélation", status = "success", width = 6, solidHeader = TRUE,
                    uiOutput("corrVarSelect"),
                    hr(),
                    h5("Options graphiques - Corrélation", style = "font-weight: bold; color: #27ae60;"),
                    fluidRow(
                      column(4,
                             selectInput("corrMethod", "Méthode de corrélation:", 
                                         choices = list("Pearson" = "pearson", 
                                                        "Spearman" = "spearman", 
                                                        "Kendall" = "kendall"),
                                         selected = "pearson"),
                             selectInput("corrDisplay", "Mode d'affichage:", 
                                         choices = list("Nombres" = "number", 
                                                        "Cercles" = "circle", 
                                                        "Carrés" = "square",
                                                        "Ellipses" = "ellipse",
                                                        "Couleurs" = "color",
                                                        "Graphique à secteurs" = "pie"),
                                         selected = "number")
                      ),
                      column(4,
                             sliderInput("corrTextSize", "Taille des valeurs:", min = 0.3, max = 2, value = 0.8, step = 0.1),
                             sliderInput("corrLabelSize", "Taille labels variables:", min = 0.3, max = 2, value = 0.8, step = 0.1)
                      ),
                      column(4,
                             textInput("corrTitle", "Titre personnalisé:", placeholder = "Matrice de corrélation"),
                             checkboxInput("corrCenterTitle", "Centrer le titre", value = TRUE),
                             selectInput("corrType", "Type d'affichage:", 
                                         choices = list("Complet" = "full", 
                                                        "Triangulaire supérieur" = "upper", 
                                                        "Triangulaire inférieur" = "lower"),
                                         selected = "upper")
                      )
                    ),
                    fluidRow(
                      column(6,
                             actionButton("updateCorrPlot", "Mettre à jour", class = "btn-success btn-sm", icon = icon("refresh"))
                      ),
                      column(6,
                             div(style = "text-align: right;",
                                 numericInput("corrDPI", "DPI image:", value = 300, min = 300, max = 20000, step = 50, width = "120px"),
                                 downloadButton("downloadCorrPlot", "Télécharger PNG", class = "btn-info btn-sm", icon = icon("download"))
                             )
                      )
                    ),
                    hr(),
                    plotOutput("corrPlot", height = "500px")
                ),
                box(title = "Distribution des variables", status = "success", width = 6, solidHeader = TRUE,
                    uiOutput("distVarSelect"),
                    hr(),
                    h5("Options graphiques - Distribution", style = "font-weight: bold; color: #337ab7;"),
                    fluidRow(
                      column(4,
                             sliderInput("distTitleSize", "Taille titre:", min = 8, max = 24, value = 14),
                             sliderInput("distAxisTitleSize", "Taille titres axes:", min = 8, max = 20, value = 12)
                      ),
                      column(4,
                             sliderInput("distAxisTextSize", "Taille texte axes:", min = 6, max = 16, value = 10),
                             sliderInput("distLegendTextSize", "Taille légende:", min = 6, max = 16, value = 10)
                      ),
                      column(4,
                             textInput("distTitle", "Titre personnalisé:", placeholder = "Distribution de [variable]"),
                             checkboxInput("distCenterTitle", "Centrer le titre", value = TRUE),
                             checkboxInput("distShowDensity", "Afficher courbe densité", value = TRUE)
                      )
                    ),
                    fluidRow(
                      column(6,
                             actionButton("updateDistPlot", "Mettre à jour", class = "btn-primary btn-sm", icon = icon("refresh"))
                      ),
                      column(6,
                             div(style = "text-align: right;",
                                 numericInput("distDPI", "DPI image:", value = 300, min = 300, max = 20000, step = 50, width = "120px"),
                                 downloadButton("downloadDistPlot", "Télécharger PNG", class = "btn-info btn-sm", icon = icon("download"))
                             )
                      )
                    ),
                    hr(),
                    plotOutput("distPlot", height = "500px")
                )
              ),
              fluidRow(
                box(title = "Analyse des valeurs manquantes", status = "warning", width = 12, solidHeader = TRUE,
                    hr(),
                    h5("Options graphiques - Valeurs manquantes", style = "font-weight: bold; color: #f39c12;"),
                    fluidRow(
                      column(3,
                             sliderInput("missingTitleSize", "Taille titre:", min = 8, max = 24, value = 14),
                             sliderInput("missingAxisTitleSize", "Taille titres axes:", min = 8, max = 20, value = 12)
                      ),
                      column(3,
                             sliderInput("missingAxisTextSize", "Taille texte axes:", min = 6, max = 16, value = 10),
                             checkboxInput("missingRotateLabels", "Incliner labels X", value = TRUE)
                      ),
                      column(3,
                             textInput("missingTitle", "Titre personnalisé:", placeholder = "Analyse des valeurs manquantes"),
                             checkboxInput("missingCenterTitle", "Centrer le titre", value = TRUE)
                      ),
                      column(3,
                             div(style = "text-align: right;",
                                 numericInput("missingDPI", "DPI image:", value = 300, min = 300, max = 20000, step = 50, width = "120px"),
                                 br(),
                                 downloadButton("downloadMissingPlot", "Télécharger PNG", class = "btn-info btn-sm", icon = icon("download"))
                             )
                      )
                    ),
                    actionButton("updateMissingPlot", "Mettre à jour", class = "btn-warning btn-sm", icon = icon("refresh")),
                    hr(),
                    plotOutput("missingPlot", height = "300px")
                )
              )
      ),
      # ---- Nettoyage ----
      tabItem(tabName = "clean",
              fluidRow(
                box(title = "Types des variables", status = "warning", width = 6, solidHeader = TRUE,
                    uiOutput("varTypeUI"),
                    actionButton("applyTypes", "Appliquer types", class = "btn-warning", icon = icon("check"))
                ),
                box(title = "Gestion des variables", status = "warning", width = 6, solidHeader = TRUE,
                    uiOutput("removeVarUI"),
                    actionButton("removeVar", "Supprimer", class = "btn-danger", icon = icon("trash")),
                    hr(),
                    textInput("newVarName", "Nom nouvelle variable (constante) :"),
                    numericInput("newVarValue", "Valeur par défaut :", 0),
                    actionButton("addVar", "Ajouter constante", class = "btn-success", icon = icon("plus")),
                    hr(),
                    textInput("calcVarName", "Nom variable calculée :"),
                    fluidRow(
                      column(6, uiOutput("colPicker")),
                      column(6,
                             actionButton("insertPlus", "+", class = "btn-info btn-sm"),
                             actionButton("insertMoins", "-", class = "btn-info btn-sm"),
                             actionButton("insertMult", "*", class = "btn-info btn-sm"),
                             actionButton("insertDiv", "/", class = "btn-info btn-sm"),
                             actionButton("insertLog", "log()", class = "btn-info btn-sm"),
                             actionButton("insertSqrt", "sqrt()", class = "btn-info btn-sm")
                      )
                    ),
                    textInput("calcFormula", "Formule (ex: log(Var1) ou (Var1+Var2)/2) :"),
                    actionButton("addCalcVar", "Créer variable calculée", class = "btn-primary", icon = icon("calculator"))
                )
              ),
              fluidRow(
                box(title = "Gestion des valeurs manquantes", status = "info", width = 12, solidHeader = TRUE,
                    uiOutput("naVarSelect"),
                    radioButtons("naMethod", "Méthode de traitement :",
                                 choices = c("Supprimer les lignes" = "remove", 
                                             "Remplacer par la moyenne" = "mean",
                                             "Remplacer par la médiane" = "median",
                                             "Remplacer par une valeur spécifique" = "value"),
                                 selected = "remove"),
                    conditionalPanel(
                      condition = "input.naMethod == 'value'",
                      numericInput("naValue", "Valeur de remplacement :", 0)
                    ),
                    actionButton("applyNA", "Appliquer le traitement", class = "btn-info", icon = icon("eraser"))
                )
              ),
              fluidRow(
                box(title = "Données nettoyées", status = "info", width = 12, solidHeader = TRUE, 
                    DTOutput("cleanedData"))
              )
      ),
      
      # ---- Filtrage ----
      tabItem(tabName = "filter",
              fluidRow(
                box(title = "Filtre croisement complet (2 facteurs)", status = "primary", width = 6, solidHeader = TRUE,
                    uiOutput("filterFactorA"),
                    uiOutput("filterFactorB"),
                    checkboxInput("requireA", "Garder niveaux de A présents pour tous les niveaux de B", TRUE),
                    checkboxInput("requireB", "Garder niveaux de B présents pour tous les niveaux de A", FALSE),
                    actionButton("applyCrossFilter", "Appliquer (2 facteurs)", class = "btn-primary", icon = icon("filter"))
                ),
                box(title = "Filtre croisement complet (N facteurs)", status = "primary", width = 6, solidHeader = TRUE,
                    uiOutput("filterFactorsN"),
                    helpText("Garde uniquement les niveaux qui forment un croisement complet entre tous les facteurs sélectionnés."),
                    actionButton("applyCrossFilterN", "Appliquer (N facteurs)", class = "btn-primary", icon = icon("filter")),
                    actionButton("resetFilter", "Réinitialiser", class = "btn-warning", icon = icon("redo")),
                    hr(),
                    h5("Résumé :"),
                    verbatimTextOutput("filterSummary")
                )
              ),
              fluidRow(
                valueBoxOutput("originalRows", width = 3),
                valueBoxOutput("filteredRows", width = 3),
                valueBoxOutput("removedRows", width = 3),
                valueBoxOutput("completeCases", width = 3)
              ),
              fluidRow(
                box(title = "Données filtrées", status = "info", width = 12, solidHeader = TRUE, 
                    DTOutput("filteredData"))
              )
      ),
      
      # ---- Descriptives ----
      tabItem(tabName = "descriptive",
              fluidRow(
                box(title = "Sélection", status = "success", width = 4, solidHeader = TRUE,
                    uiOutput("numVarSelect"),
                    checkboxInput("selectAllNum", "Sélectionner toutes les variables numériques", FALSE),
                    checkboxGroupInput("descStats", "Statistiques :",
                                       choices = list("Moyenne" = "mean", "Médiane" = "median", "Écart-type" = "sd", 
                                                      "Variance" = "var", "CV (%)" = "cv", "Minimum" = "min", 
                                                      "Maximum" = "max", "1er Quartile" = "q1", "3ème Quartile" = "q3"),
                                       selected = c("mean", "median", "sd", "cv")
                    ),
                    uiOutput("descFactorUI"),
                    actionButton("calcDesc", "Calculer", class = "btn-success", icon = icon("calculator"))
                ),
                box(title = "Résultats", status = "success", width = 8, solidHeader = TRUE,
                    DTOutput("descResults"),
                    downloadButton("downloadDesc", "Télécharger en Excel", class = "btn-info")
                )
              ),
              fluidRow(
                box(title = "Visualisation des descriptives", status = "info", width = 12, solidHeader = TRUE,
                    uiOutput("descPlotVarSelect"),
                    uiOutput("descPlotFactorSelect"),
                    plotlyOutput("descPlot", height = "400px")
                )
              )
      ),
      
      
      # ---- Analyses multivariées ----
      tabItem(tabName = "multivariate",
              fluidRow(
                box(title = "Analyse en Composantes Principales (ACP)", status = "info", width = 6, solidHeader = TRUE,
                    uiOutput("pcaVarSelect"),
                    checkboxInput("pcaScale", "Standardiser les variables", TRUE),
                    # Option pour analyses avec moyennes
                    checkboxInput("pcaUseMeans", "Utiliser les moyennes par groupe", FALSE),
                    conditionalPanel(
                      condition = "input.pcaUseMeans == true",
                      uiOutput("pcaMeansGroupSelect")
                    ),
                    uiOutput("pcaQualiSupSelect"),
                    uiOutput("pcaIndSupSelect"),
                    uiOutput("pcaLabelSourceSelect"),
                    hr(),
                    radioButtons("pcaPlotType", "Type de visualisation:",
                                 choices = c("Variables" = "var", "Individus" = "ind", "Biplot" = "biplot"),
                                 selected = "var", inline = TRUE),
                    numericInput("pcaComponents", "Nombre de composantes:", value = 2, min = 2, max = 10),
                    # Options de personnalisation graphique
                    conditionalPanel(
                      condition = "input.runPCA > 0",
                      hr(),
                      h5("Personnalisation graphique:", style = "font-weight: bold; color: #337ab7;"),
                      textInput("pcaPlotTitle", "Titre du graphique:", 
                                value = "ACP - Analyse en Composantes Principales"),
                      textInput("pcaXLabel", "Label axe X:", value = ""),
                      textInput("pcaYLabel", "Label axe Y:", value = ""),
                      checkboxInput("pcaCenterAxes", "Centrer sur (0,0)", TRUE)
                    ),
                    hr(),
                    actionButton("runPCA", "Exécuter ACP", class = "btn-info", icon = icon("project-diagram"), width = "48%"),
                    downloadButton("downloadPCA", "Télécharger", class = "btn-outline-info", width = "48%")
                ),
                box(title = "Visualisation ACP", status = "info", width = 6, solidHeader = TRUE,
                    plotlyOutput("pcaPlot", height = "550px"),
                    downloadButton("downloadPcaPlot", "Télécharger graphique ACP"),
                    hr(),
                    div(style = "max-height: 300px; overflow-y: auto; font-size: 12px;",
                        verbatimTextOutput("pcaSummary"))
                )
              ),
              fluidRow(
                box(title = "Classification Hiérarchique sur Composantes Principales (HCPC)", status = "success", width = 12, solidHeader = TRUE,
                    p("Cette analyse combine l'ACP avec une classification hiérarchique automatique."),
                    numericInput("hcpcClusters", "Nombre de clusters:", value = 3, min = 2, max = 10),
                    # Options de personnalisation pour HCPC
                    conditionalPanel(
                      condition = "input.runHCPC > 0",
                      hr(),
                      h5("Personnalisation graphique:", style = "font-weight: bold; color: #5cb85c;"),
                      fluidRow(
                        column(6,
                               textInput("hcpcClusterTitle", "Titre carte des clusters:", 
                                         value = "Carte des clusters HCPC"),
                               textInput("hcpcClusterXLabel", "Label axe X:", value = ""),
                               textInput("hcpcClusterYLabel", "Label axe Y:", value = ""),
                               checkboxInput("hcpcCenterAxes", "Centrer sur (0,0)", TRUE)
                        ),
                        column(6,
                               textInput("hcpcDendTitle", "Titre dendrogramme:", 
                                         value = "Dendrogramme HCPC"),
                               # Note: pas de centrage pour le dendrogramme
                               p(style = "font-style: italic; color: #666;", 
                                 "Le dendrogramme n'est pas centré sur (0,0)")
                        )
                      )
                    ),
                    actionButton("runHCPC", "Exécuter HCPC", class = "btn-success", icon = icon("sitemap"), width = "100%"),
                    hr(),
                    fluidRow(
                      column(6,
                             div(class = "box box-solid box-success",
                                 div(class = "box-header with-border",
                                     h4(class = "box-title", "Carte des clusters")
                                 ),
                                 div(class = "box-body",
                                     plotlyOutput("hcpcClusterPlot", height = "500px"),
                                     downloadButton("downloadHcpcClusterPlot", "Télécharger carte des clusters")
                                 )
                             )
                      ),
                      column(6,
                             div(class = "box box-solid box-success",
                                 div(class = "box-header with-border",
                                     h4(class = "box-title", "Dendrogramme")
                                 ),
                                 div(class = "box-body",
                                     plotlyOutput("hcpcDendPlot", height = "500px"),
                                     downloadButton("downloadHcpcDendPlot", "Télécharger dendrogramme")
                                 )
                             )
                      )
                    ),
                    br(),
                    div(class = "box box-solid",
                        div(class = "box-header with-border", style = "background-color: #5cb85c; color: white;",
                            h4(class = "box-title", "Résultats détaillés HCPC", style = "color: white; font-weight: bold;")
                        ),
                        div(class = "box-body", style = "background-color: #f9f9f9;",
                            div(style = "max-height: 500px; overflow-y: auto; font-family: 'Courier New', monospace; font-size: 11px; background-color: white; padding: 15px; border-radius: 5px;",
                                verbatimTextOutput("hcpcSummary"))
                        )
                    )
                )
              ),
              fluidRow(
                box(title = "Analyse Factorielle Discriminante (AFD)", status = "primary", width = 12, solidHeader = TRUE,
                    fluidRow(
                      column(4,
                             uiOutput("afdFactorSelect")),
                      column(8,
                             uiOutput("afdVarSelect"))
                    ),
                    # Option pour analyses avec moyennes
                    checkboxInput("afdUseMeans", "Utiliser les moyennes par groupe", FALSE),
                    conditionalPanel(
                      condition = "input.afdUseMeans == true",
                      uiOutput("afdMeansGroupSelect")
                    ),
                    # Options de personnalisation pour AFD
                    conditionalPanel(
                      condition = "input.runAFD > 0",
                      hr(),
                      h5("Personnalisation graphique:", style = "font-weight: bold; color: #337ab7;"),
                      fluidRow(
                        column(6,
                               textInput("afdIndTitle", "Titre projection individus:", 
                                         value = "AFD - Projection des individus"),
                               textInput("afdIndXLabel", "Label axe X:", value = ""),
                               textInput("afdIndYLabel", "Label axe Y:", value = ""),
                               checkboxInput("afdIndCenterAxes", "Centrer sur (0,0)", TRUE)
                        ),
                        column(6,
                               textInput("afdVarTitle", "Titre contribution variables:", 
                                         value = "AFD - Contribution des variables"),
                               textInput("afdVarXLabel", "Label axe X:", value = ""),
                               textInput("afdVarYLabel", "Label axe Y:", value = ""),
                               checkboxInput("afdVarCenterAxes", "Centrer sur (0,0)", TRUE)
                        )
                      )
                    ),
                    hr(),
                    actionButton("runAFD", "Exécuter AFD", class = "btn-primary", icon = icon("project-diagram"), width = "100%"),
                    hr(),
                    fluidRow(
                      column(6,
                             div(class = "box box-solid box-primary",
                                 div(class = "box-header with-border",
                                     h4(class = "box-title", "Projection des individus", style = "color: #fff;")
                                 ),
                                 div(class = "box-body",
                                     plotlyOutput("afdIndPlot", height = "500px"),
                                     downloadButton("downloadAfdIndPlot", "Télécharger projection individus")
                                 )
                             )
                      ),
                      column(6,
                             div(class = "box box-solid box-primary",
                                 div(class = "box-header with-border",
                                     h4(class = "box-title", "Contribution des variables", style = "color: #fff;")
                                 ),
                                 div(class = "box-body",
                                     plotlyOutput("afdVarPlot", height = "500px"),
                                     downloadButton("downloadAfdVarPlot", "Télécharger contribution variables")
                                 )
                             )
                      )
                    ),
                    br(),
                    div(class = "box box-solid",
                        div(class = "box-header with-border", style = "background-color: #d9534f; color: white;",
                            h4(class = "box-title", "Résultats détaillés de l'AFD", style = "color: white; font-weight: bold;")
                        ),
                        div(class = "box-body", style = "background-color: #f9f9f9;",
                            div(style = "max-height: 700px; overflow-y: auto; font-family: 'Courier New', monospace; font-size: 11px; background-color: white; padding: 15px; border-radius: 5px;",
                                verbatimTextOutput("afdSummary"))
                        )
                    )
                )
              )
      ),
      
      # ---- Tests statistiques ----
      tabItem(tabName = "tests",
              fluidRow(
                box(title = "Paramètres des tests", status = "danger", width = 12, solidHeader = TRUE,
                    fluidRow(
                      column(4,
                             uiOutput("responseVarSelect"),
                             uiOutput("factorVarSelect"),
                             checkboxInput("interaction", "Inclure les interactions (ANOVA/Scheirer-Ray-Hare)", FALSE)
                      ),
                      column(4,
                             h4("Tests sur données brutes", style = "color: #3c8dbc;"),
                             actionButton("testNormalityRaw", "Test de normalité", class = "btn-warning", icon = icon("chart-line")),
                             br(), br(),
                             actionButton("testHomogeneityRaw", "Test d'homogénéité", class = "btn-warning", icon = icon("balance-scale")),
                             br(), br(),
                             h4("Tests paramétriques", style = "color: #00a65a;"),
                             actionButton("testT", "Test t de Student", class = "btn-success", icon = icon("check")),
                             actionButton("testANOVA", "ANOVA", class = "btn-success", icon = icon("check")),
                             actionButton("testLM", "Régression linéaire", class = "btn-success", icon = icon("check")),
                             actionButton("testGLM", "Modèle linéaire généralisé", class = "btn-success", icon = icon("check"))
                      ),
                      column(4,
                             h4("Tests non-paramétriques", style = "color: #f39c12;"),
                             actionButton("testWilcox", "Test de Wilcoxon", class = "btn-warning", icon = icon("check")),
                             actionButton("testKruskal", "Test de Kruskal-Wallis", class = "btn-warning", icon = icon("check")),
                             actionButton("testScheirerRayHare", "Test de Scheirer-Ray-Hare", class = "btn-warning", icon = icon("check"))
                      )
                    )
                )
              ),
              fluidRow(
                box(title = "Résultats des tests", status = "danger", width = 12, solidHeader = TRUE,
                    DTOutput("testResultsDF"),
                    br(),
                    downloadButton("downloadTestsExcel", "Télécharger les résultats (Excel)", class = "btn-info"))
              ),
              conditionalPanel(
                condition = "output.showParametricDiagnostics",
                fluidRow(
                  box(title = "Diagnostics des modèles", status = "info", width = 6, solidHeader = TRUE,
                      conditionalPanel(
                        condition = "output.showModelNavigation",
                        wellPanel(
                          h6("Navigation des modèles", style = "margin-top: 0; margin-bottom: 10px;"),
                          div(style = "text-align: center;",
                              uiOutput("modelDiagNavigation")
                          )
                        )
                      ),
                      plotOutput("modelDiagnostics", height = "500px"),
                      br(),
                      downloadButton("downloadModelDiagnostics", "Télécharger (PNG)", class = "btn-success"),
                      htmlOutput("modelDiagnosticsInterpretation")
                  ),
                  box(title = "Résidus et validation", status = "info", width = 6, solidHeader = TRUE,
                      conditionalPanel(
                        condition = "output.showResidNavigation",
                        wellPanel(
                          h6("Navigation des variables", style = "margin-top: 0; margin-bottom: 10px;"),
                          div(style = "text-align: center;",
                              uiOutput("residNavigation")
                          )
                        )
                      ),
                      tabBox(
                        title = "Analyses des résidus",
                        id = "residualTabs", width = 12,
                        tabPanel("QQ-plot", 
                                 plotOutput("qqPlotResiduals", height = "320px"),
                                 br(),
                                 downloadButton("downloadQQPlot", "Télécharger (PNG)", class = "btn-success"),
                                 htmlOutput("qqPlotInterpretation")),
                        tabPanel("Normalité", 
                                 verbatimTextOutput("normalityResult"),
                                 htmlOutput("normalityResidInterpretation")),
                        tabPanel("Homogénéité", 
                                 verbatimTextOutput("leveneResidResult"),
                                 htmlOutput("homogeneityResidInterpretation")),
                        tabPanel("Autocorrélation", 
                                 verbatimTextOutput("autocorrResult"),
                                 htmlOutput("autocorrInterpretation")),
                        tabPanel("Summary", verbatimTextOutput("modelSummary"))
                      )
                  )
                )
              )
      ),
      
      # ---- Comparaisons multiples PostHoc ----
      tabItem(tabName = "multiple",
              fluidRow(
                box(title = "Configuration", status = "info", width = 4, solidHeader = TRUE,
                    uiOutput("multiResponseSelect"),
                    uiOutput("multiFactorSelect"),
                    radioButtons("testType", "Type de comparaisons",
                                 choices = list("Paramétrique (ANOVA)" = "param", 
                                                "Non paramétrique (Kruskal/Scheirer)" = "nonparam"),
                                 selected = "param"
                    ),
                    conditionalPanel(
                      condition = "input.testType == 'param'",
                      selectInput("multiTest", "Méthode post-hoc",
                                  choices = list("Tukey HSD" = "tukey", 
                                                 "LSD" = "lsd", 
                                                 "Duncan" = "duncan", 
                                                 "SNK" = "snk",
                                                 "Scheffe" = "scheffe",
                                                 "REGW" = "regw",
                                                 "Waller-Duncan" = "waller",
                                                 "Bonferroni" = "bonferroni",
                                                 "Dunnett" = "dunnett", 
                                                 "Games-Howell" = "games"),
                                  selected = "tukey"
                      )
                    ),
                    conditionalPanel(
                      condition = "input.testType == 'nonparam'",
                      selectInput("multiTestNonParam", "Méthode post-hoc",
                                  choices = list("Kruskal" = "kruskal",
                                                 "Dunn" = "dunn",
                                                 "Conover" = "conover",
                                                 "Nemenyi" = "nemenyi"),
                                  selected = "kruskal"
                      )
                    ),
                    div(style = "border: 2px solid #3498db; border-radius: 5px; padding: 10px; margin: 10px 0; background-color: #ecf0f1;",
                        checkboxInput("posthocInteraction", 
                                      HTML("<strong>Post-hoc sur interactions</strong><br/>
                                    <small style='color: #7f8c8d;'>Analyse les interactions significatives entre facteurs</small>"), 
                                      value = FALSE),
                        conditionalPanel(
                          condition = "input.posthocInteraction == true",
                          div(style = "margin-top: 10px; padding: 8px; background-color: #d5dbdb; border-radius: 3px;",
                              icon("info-circle"), 
                              HTML(" <strong>Note:</strong> Nécessite au moins 2 facteurs. 
                             Seules les interactions significatives (p < 0.05) seront analysées.")
                          )
                        )
                    ),
                    hr(),
                    h4("Options graphiques"),
                    selectInput("boxColor", "Couleurs du graphique",
                                choices = c("Default ggplot2" = "default",
                                            "Bleu" = "Blues", "Vert" = "Greens", "Rouge" = "Reds", 
                                            "Couleurs Set1" = "Set1", "Pastel" = "Pastel1", "Paired" = "Paired"),
                                selected = "Set1"
                    ),
                    radioButtons("plotType", "Type de graphique",
                                 choices = c("Boxplot" = "box", "Violon" = "violin", "Point" = "point",
                                             "Histogramme" = "hist"), selected = "box"),
                    radioButtons("errorType", "Barres d'erreur",
                                 choices = c("Erreur-type (SE)" = "se", 
                                             "Ã‰cart-type (SD)" = "sd", 
                                             "IC 95%" = "ci",
                                             "Aucune" = "none"), selected = "se"),
                    checkboxInput("colorByGroups", "Colorer par groupes statistiques", value = FALSE),
                    hr(),
                    h5("Personnalisation des titres"),
                    textInput("customTitle", "Titre du graphique", placeholder = "Titre automatique"),
                    textInput("customXLabel", "Titre axe X", placeholder = "Nom de variable"),
                    textInput("customYLabel", "Titre axe Y", placeholder = "Nom de variable"),
                    textInput("customLegendTitle", "Titre de la légende", placeholder = "Nom de variable"),
                    checkboxInput("rotateXLabels", "Incliner les labels à  45°", value = TRUE),
                    hr(),
                    h5("Personnalisation du thème"),
                    sliderInput("titleSize", "Taille du titre", min = 8, max = 32, value = 16),
                    sliderInput("axisTitleSize", "Taille titres axes", min = 8, max = 28, value = 14),
                    sliderInput("axisTextSize", "Taille textes axes", min = 6, max = 24, value = 12),
                    actionButton("runMultiple", "Exécuter l'analyse", class = "btn-info btn-lg", 
                                 icon = icon("play"), style = "width: 100%; margin-top: 15px;"),
                    hr(),
                    h5("Export graphique"),
                    numericInput("plotWidth", "Largeur (pouces)", value = 8, min = 3, max = 20),
                    numericInput("plotHeight", "Hauteur (pouces)", value = 6, min = 3, max = 20),
                    numericInput("plotDPI", "Résolution (DPI)", value = 300, min = 72, max = 2000),
                    downloadButton("downloadPlot", "Télécharger graphique (PNG)", class = "btn-success", 
                                   style = "width: 100%;")
                ),
                box(title = "Résultats des comparaisons", status = "info", width = 8, solidHeader = TRUE,
                    # Panneau de sélection du type de résultats avec informations
                    conditionalPanel(
                      condition = "output.showPosthocResults",
                      div(style = "margin-bottom: 20px;",
                          fluidRow(
                            column(6,
                                   selectInput("resultTypeDisplay", "Type de résultats Ã  afficher:",
                                               choices = list("Effets principaux" = "main",
                                                              "Interactions significatives" = "interaction"),
                                               selected = "main",
                                               width = "100%"
                                   )
                            ),
                            column(6,
                                   # Indicateurs de disponibilité des résultats
                                   div(id = "resultIndicators", style = "padding-top: 5px;",
                                       uiOutput("resultTypeIndicators")
                                   )
                            )
                          )
                      )
                    ),
                    
                    # Panneau d'information contextuelle
                    conditionalPanel(
                      condition = "output.showPosthocResults && input.resultTypeDisplay == 'interaction'",
                      div(style = "margin-bottom: 15px; padding: 10px; background-color: #e8f4fd; border-left: 4px solid #3498db; border-radius: 4px;",
                          div(style = "display: flex; align-items: center;",
                              icon("info-circle", style = "color: #3498db; margin-right: 8px;"),
                              div(
                                strong("Interactions affichées"),
                                br(),
                                span("Seules les interactions avec p < 0.05 sont analysées et affichées.", 
                                     style = "font-size: 0.9em; color: #5a6c7d;")
                              )
                          )
                      )
                    ),
                    
                    # Panneau de résumé des analyses
                    conditionalPanel(
                      condition = "output.showPosthocResults",
                      div(style = "margin-bottom: 15px;",
                          uiOutput("analysisSummary")
                      )
                    ),
                    
                    # Tableau des résultats avec chargement
                    div(id = "resultsContainer",
                        DTOutput("multipleResults"),
                        br()
                    ),
                    
                    # Message si pas de résultats d'interaction
                    conditionalPanel(
                      condition = "output.showPosthocResults && input.resultTypeDisplay == 'interaction'",
                      div(id = "noInteractionMessage",
                          uiOutput("noInteractionMsg")
                      )
                    ),
                    
                    # Section de téléchargements améliorée
                    div(style = "border-top: 2px solid #bdc3c7; padding-top: 20px; margin-top: 20px; background-color: #f8f9fa; border-radius: 5px; padding: 15px;",
                        h5("Téléchargements", style = "color: #2c3e50; margin-bottom: 15px; display: flex; align-items: center;",
                           icon("download", style = "margin-right: 8px;")),
                        fluidRow(
                          column(4, 
                                 downloadButton("downloadMultiResultsExcel", 
                                                div(icon("file-excel"), br(), "Tous les résultats"), 
                                                class = "btn-info btn-sm", style = "width: 100%; height: 60px;")
                          ),
                          column(4,
                                 downloadButton("downloadCurrentResults", 
                                                div(icon("file-alt"), br(), "Résultats actuels"), 
                                                class = "btn-outline-info btn-sm", style = "width: 100%; height: 60px;")
                          ),
                          column(4,
                                 downloadButton("downloadSummaryStats", 
                                                div(icon("chart-bar"), br(), "Statistiques résumées"), 
                                                class = "btn-outline-success btn-sm", style = "width: 100%; height: 60px;")
                          )
                        )
                    ),
                    br(), br(),
                    
                    # Navigation des variables améliorée
                    conditionalPanel(
                      condition = "output.showVariableNavigation",
                      fluidRow(
                        column(12,
                               wellPanel(style = "background-color: #f1f2f6; border: 1px solid #ddd;",
                                         div(style = "display: flex; align-items: center; justify-content: center;",
                                             h5("Navigation des variables", style = "margin: 0; color: #2c3e50;"),
                                             div(style = "margin-left: 20px;",
                                                 uiOutput("variableNavigation")
                                             )
                                         )
                               )
                        )
                      )
                    ),
                    
                    # Graphique avec titre dynamique
                    div(
                      h5(uiOutput("plotTitle"), style = "text-align: center; color: #2c3e50; margin-bottom: 20px;"),
                      plotlyOutput("multiPlot", height = "500px"),
                      br(),
                      div(style = "text-align: center;",
                          downloadButton("downloadMultiPlot", "Télécharger le graphique actuel (PNG)", 
                                         class = "btn-success", icon = icon("image"))
                      )
                    )
                )
              )
      ),
      # ---- Tableaux croisés dynamiques ----
      tabItem(tabName = "crosstab",
              fluidRow(
                box(title = "Configuration du tableau croisé", status = "primary", width = 4, solidHeader = TRUE,
                    uiOutput("crosstabRowVarSelect"),
                    uiOutput("crosstabColVarSelect"),
                    uiOutput("crosstabFilterVarSelect"),
                    hr(),
                    h5("Options d'analyse", style = "font-weight: bold; color: #337ab7;"),
                    checkboxGroupInput("analysisOptions", "Types d'analyses :",
                                       choices = list(
                                         "Proportions en lignes" = "row_prop",
                                         "Proportions en colonnes" = "col_prop", 
                                         "Proportions totales" = "total_prop",
                                         "Test du Chi²" = "chi_test",
                                         "Test exact de Fisher" = "fisher_test",
                                         "Résidus standardisés" = "residuals"
                                       ),
                                       selected = c("row_prop", "col_prop", "chi_test")),
                    hr(),
                    h5("Options graphiques", style = "font-weight: bold; color: #27ae60;"),
                    radioButtons("plotType", "Type de graphique :",
                                 choices = c("Histogramme groupé" = "bar",
                                             "Histogramme empilé" = "stacked_bar",
                                             "Graphique en secteurs" = "pie",
                                             "Graphique mosaïque" = "mosaic"),
                                 selected = "bar"),
                    hr(),
                    h5("Personnalisation", style = "font-weight: bold; color: #f39c12;"),
                    textInput("crosstabTitle", "Titre principal :", value = ""),
                    textInput("crosstabXLabel", "Label axe X :", value = ""),
                    textInput("crosstabYLabel", "Label axe Y :", value = ""),
                    sliderInput("titleSize", "Taille titre :", min = 8, max = 24, value = 14),
                    sliderInput("axisTextSize", "Taille texte axes :", min = 6, max = 16, value = 10),
                    sliderInput("legendTextSize", "Taille légende :", min = 6, max = 16, value = 10),
                    checkboxInput("showPercentages", "Afficher pourcentages sur graphique", TRUE),
                    actionButton("generateCrosstab", "Générer l'analyse", 
                                 class = "btn-primary btn-lg", icon = icon("table"),
                                 style = "width: 100%; margin-top: 15px;")
                ),
                box(title = "Résultats - Tableau croisé", status = "primary", width = 8, solidHeader = TRUE,
                    tabBox(
                      title = "Analyses", id = "crosstabTabs", width = 12,
                      tabPanel("Effectifs", 
                               DTOutput("crosstabTable"),
                               br(),
                               downloadButton("downloadCrosstab", "Télécharger (Excel)", 
                                              class = "btn-success")),
                      tabPanel("Proportions lignes", 
                               DTOutput("crosstabRowProp"),
                               br(),
                               downloadButton("downloadRowProp", "Télécharger (Excel)", 
                                              class = "btn-success")),
                      tabPanel("Proportions colonnes", 
                               DTOutput("crosstabColProp"),
                               br(),
                               downloadButton("downloadColProp", "Télécharger (Excel)", 
                                              class = "btn-success")),
                      tabPanel("Proportions totales", 
                               DTOutput("crosstabTotalProp"),
                               br(),
                               downloadButton("downloadTotalProp", "Télécharger (Excel)", 
                                              class = "btn-success")),
                      tabPanel("Tests statistiques", 
                               verbatimTextOutput("crosstabTests"),
                               br(),
                               downloadButton("downloadTests", "Télécharger (Excel)", 
                                              class = "btn-success")),
                      tabPanel("Résidus", 
                               DTOutput("crosstabResiduals"),
                               br(),
                               downloadButton("downloadResiduals", "Télécharger (Excel)", 
                                              class = "btn-success"))
                    )
                )
              ),
              fluidRow(
                box(title = "Visualisations", status = "success", width = 12, solidHeader = TRUE,
                    fluidRow(
                      column(6,
                             div(class = "box box-solid box-success",
                                 div(class = "box-header with-border",
                                     h4(class = "box-title", "Graphique principal")
                                 ),
                                 div(class = "box-body",
                                     plotOutput("crosstabPlot", height = "500px"),
                                     br(),
                                     fluidRow(
                                       column(6,
                                              numericInput("plotWidth", "Largeur (pouces):", value = 10, min = 4, max = 20)),
                                       column(6,
                                              numericInput("plotHeight", "Hauteur (pouces):", value = 8, min = 4, max = 20))
                                     ),
                                     downloadButton("downloadPlot", "Télécharger graphique (PNG)", 
                                                    class = "btn-info", style = "width: 100%;")
                                 )
                             )
                      ),
                      column(6,
                             div(class = "box box-solid box-warning",
                                 div(class = "box-header with-border",
                                     h4(class = "box-title", "Graphique en secteurs")
                                 ),
                                 div(class = "box-body",
                                     plotOutput("crosstabPiePlot", height = "500px"),
                                     br(),
                                     selectInput("pieVariable", "Variable pour secteurs :",
                                                 choices = c("Variable ligne" = "row", "Variable colonne" = "col")),
                                     downloadButton("downloadPiePlot", "Télécharger secteurs (PNG)", 
                                                    class = "btn-warning", style = "width: 100%;")
                                 )
                             )
                      )
                    )
                )
              )
      ),
      # ---- Visualisation des données ----
      tabItem(tabName = "visualization",
              fluidRow(
                box(title = "Sélection des variables", status = "primary", width = 3, solidHeader = TRUE,
                    uiOutput("vizXVarSelect"),
                    uiOutput("vizYVarSelect"),
                    uiOutput("vizColorVarSelect"),
                    uiOutput("vizFacetVarSelect"),
                    selectInput("vizType", "Type de visualisation:",
                                choices = c("Nuage de points" = "scatter", "Boxplot" = "box", "Violon" = "violin",
                                            "Barres" = "bar", "Lignes" = "line", "Densité" = "density",
                                            "Histogramme" = "histogram", "Heatmap" = "heatmap")),
                    actionButton("generateViz", "Générer la visualisation", class = "btn-primary", icon = icon("chart-line"))
                ),
                box(title = "Visualisation", status = "primary", width = 9, solidHeader = TRUE,
                    plotlyOutput("advancedPlot", height = "600px")
                )
              ),
              fluidRow(
                box(title = "Options de personnalisation", status = "info", width = 12, solidHeader = TRUE,
                    fluidRow(
                      column(3,
                             textInput("plotTitle", "Titre du graphique:", value = ""),
                             textInput("plotXLab", "Label axe X:", value = ""),
                             textInput("plotYLab", "Label axe Y:", value = "")
                      ),
                      column(3,
                             sliderInput("plotAlpha", "Transparence:", min = 0.1, max = 1, value = 0.7),
                             sliderInput("plotSize", "Taille des points:", min = 1, max = 10, value = 3)
                      ),
                      column(3,
                             selectInput("plotTheme", "Thème:",
                                         choices = c("Classique" = "classic", "Minimal" = "minimal", "Gris" = "gray",
                                                     "Sombre" = "dark", "Ligne" = "linedraw")),
                             selectInput("plotPalette", "Palette de couleurs:",
                                         choices = c("Défaut" = "default", "Set1" = "Set1", "Set2" = "Set2",
                                                     "Set3" = "Set3", "Pastel1" = "Pastel1", "Pastel2" = "Pastel2",
                                                     "Paired" = "Paired", "Dark2" = "Dark2", "Accent" = "Accent"))
                      ),
                      column(3,
                             numericInput("plotWidthInteractive", "Largeur (px):", value = 800),
                             numericInput("plotHeightInteractive", "Hauteur (px):", value = 500),
                             downloadButton("downloadInteractivePlot", "Télécharger", class = "btn-success")
                      )
                    )
                )
              )
      ),
      
      # ---- Rapport ----
      tabItem(tabName = "report",
              fluidRow(
                box(title = "Génération de rapport", status = "success", width = 12, solidHeader = TRUE,
                    textInput("reportTitle", "Titre du rapport:", value = "Analyse Statistique"),
                    textInput("reportAuthor", "Auteur:", value = ""),
                    selectInput("reportFormat", "Format du rapport:",
                                choices = c("HTML" = "html_document", "PDF" = "pdf_document", "Word" = "word_document")),
                    actionButton("generateReport", "Générer le rapport", class = "btn-success", icon = icon("file-pdf")),
                    downloadButton("downloadReport", "Télécharger le rapport", class = "btn-info")
                )
              ),
              fluidRow(
                box(title = "Aperçu du rapport", status = "info", width = 12, solidHeader = TRUE,
                    uiOutput("reportPreview")
                )
              )
      )
    )
  )
)

################################################################################
#
# Server
# Ce Bloc définit la logique serveur de l'application Shiny
#
################################################################################

server <- function(input, output, session) {
  values <- reactiveValues(
    data = NULL, cleanData = NULL, filteredData = NULL, descStats = NULL,
    normResults = NULL, leveneResults = NULL, testResults = NULL,
    anovaModel = NULL, lastKruskal = NULL, multiResults = NULL, multiGroups = NULL,
    currentPlot = NULL, residualsNorm = NULL, leveneResid = NULL,
    multiNormResults = NULL, multiLeveneResults = NULL,
    pcaResult = NULL, clusterResult = NULL, currentModel = NULL,
    testInterpretation = NULL, cahResult = NULL, currentInteractivePlot = NULL,
    cahClusters = NULL,
    testResultsDF = NULL,
    multiResultsMain = NULL, multiResultsInteraction = NULL,
    normalityResults = NULL, homogeneityResults = NULL,
    currentVarIndex = 1, currentValidationVar = 1,
    allTestResults = list(),  # To accumulate multiple test results for Excel
    allPostHocResults = list(),  # To accumulate multiple post-hoc results for Excel
    modelsList = list(),  # Models per variable for diagnostics/resid
    normalityResultsPerVar = list(), homogeneityResultsPerVar = list(),  # Per variable for validation
    currentDiagVar = 1, currentResidVar = 1  # Navigation for diagnostics and resid
  )
  
  # ---- Aide et réinitialisation ----
  observeEvent(input$helpBtn, {
    shinyalert(
      title = "Aide",
      text = "Cette application permet d'analyser statistiquement des données expérimentales. 
              Naviguez à travers les onglets de gauche pour charger, explorer, nettoyer, filtrer et analyser vos données.
              Utilisez le bouton 'Exemple' pour télécharger un jeu de données d'exemple.",
      type = "info"
    )
  })
  
  observeEvent(input$resetBtn, {
    shinyalert(
      title = "Réinitialiser",
      text = "Êtes-vous sûr de vouloir réinitialiser l'application ? Toutes les données seront perdues.",
      type = "warning",
      showCancelButton = TRUE,
      confirmButtonText = "Oui",
      cancelButtonText = "Non",
      callbackR = function(value) {
        if (value) {
          values$data <- NULL
          values$cleanData <- NULL
          values$filteredData <- NULL
          values$descStats <- NULL
          values$normResults <- NULL
          values$leveneResults <- NULL
          values$testResults <- NULL
          values$anovaModel <- NULL
          values$lastKruskal <- NULL
          values$multiResults <- NULL
          values$multiGroups <- NULL
          values$currentPlot <- NULL
          values$residualsNorm <- NULL
          values$leveneResid <- NULL
          values$multiNormResults <- NULL
          values$multiLeveneResults <- NULL
          values$pcaResult <- NULL
          values$clusterResult <- NULL
          values$currentModel <- NULL
          values$testInterpretation <- NULL
          values$cahResult <- NULL
          values$currentInteractivePlot <- NULL
          values$allTestResults <- list()
          values$allPostHocResults <- list()
          values$modelsList <- list()
          values$normalityResultsPerVar <- list()
          values$homogeneityResultsPerVar <- list()
          
          reset("file")
          updateTabItems(session, "tabs", "load")
          
          showNotification("Application réinitialisée", type = "message")
        }
      }
    )
  })
  
  # ---- Exemple de données ----
  output$downloadExample <- downloadHandler(
    filename = function() {
      "exemple_donnees.csv"
    },
    content = function(file) {
      set.seed(123)
      n <- 100
      exemple <- data.frame(
        Traitement = rep(c("A", "B", "C", "D"), each = n/4),
        Bloc = rep(1:4, each = n/4),
        Genre = sample(c("M", "F"), n, replace = TRUE),
        Age = sample(18:65, n, replace = TRUE),
        Variable1 = c(rnorm(n/4, 10, 2), rnorm(n/4, 12, 2), rnorm(n/4, 15, 2), rnorm(n/4, 11, 2)),
        Variable2 = c(rnorm(n/4, 20, 3), rnorm(n/4, 22, 3), rnorm(n/4, 25, 3), rnorm(n/4, 21, 3)),
        Variable3 = c(rnorm(n/4, 5, 1), rnorm(n/4, 6, 1), rnorm(n/4, 7, 1), rnorm(n/4, 5.5, 1)),
        Variable4 = c(rnorm(n/4, 100, 10), rnorm(n/4, 110, 10), rnorm(n/4, 120, 10), rnorm(n/4, 105, 10))
      )
      for (i in 5:8) {
        exemple[sample(1:n, 5), i] <- NA
      }
      write.csv(exemple, file, row.names = FALSE)
    }
  )
  
  # ---- Chargement ----
  output$sheetUI <- renderUI({
    req(input$file)
    ext <- tools::file_ext(input$file$datapath)
    if (ext %in% c("xlsx", "xls")) {
      sheets <- readxl::excel_sheets(input$file$datapath)
      selectInput("sheet", "Feuille Excel :", choices = sheets, selected = sheets[1])
    }
  })
  
  observeEvent(input$loadData, {
    req(input$file)
    ext <- tools::file_ext(input$file$datapath)
    tryCatch({
      withProgress(message = 'Chargement des données', value = 0, {
        incProgress(0.3)
        if (ext %in% c("csv", "txt")) {
          values$data <- read.csv(input$file$datapath, header = input$header, sep = input$sep, check.names = FALSE)
        } else if (ext %in% c("xlsx", "xls")) {
          values$data <- readxl::read_excel(path = input$file$datapath, sheet = input$sheet %||% 1)
        } else if (ext == "sav") {
          values$data <- haven::read_sav(input$file$datapath)
        } else if (ext == "dta") {
          values$data <- haven::read_dta(input$file$datapath)
        } else if (ext == "rds") {
          values$data <- readRDS(input$file$datapath)
        } else {
          stop("Format non supporté.")
        }
        incProgress(0.7)
        values$cleanData <- as.data.frame(values$data)
        values$filteredData <- values$cleanData
        incProgress(1)
      })
      showNotification("Données chargées avec succès", type = "message")
    }, error = function(e) {
      showNotification(paste("Erreur de chargement :", e$message), type = "error")
    })
  })
  
  output$nrowBox <- renderValueBox({
    req(values$data)
    valueBox(
      nrow(values$data), "Lignes", icon = icon("list"),
      color = "purple"
    )
  })
  
  output$ncolBox <- renderValueBox({
    req(values$data)
    valueBox(
      ncol(values$data), "Colonnes", icon = icon("columns"),
      color = "purple"
    )
  })
  
  output$naBox <- renderValueBox({
    req(values$data)
    na_count <- sum(is.na(values$data))
    valueBox(
      na_count, "Valeurs manquantes", icon = icon("question"),
      color = ifelse(na_count > 0, "red", "green")
    )
  })
  
  output$memBox <- renderValueBox({
    req(values$data)
    mem_size <- format(object.size(values$data), units = "auto")
    valueBox(
      mem_size, "Taille mémoire", icon = icon("memory"),
      color = "blue"
    )
  })
  
  output$preview <- renderDT({
    req(values$data)
    datatable(head(values$data, 50), options = list(scrollX = TRUE))
  })
  
  # ---- Exploration  ----
  output$dataStructure <- renderPrint({
    req(values$data)
    cat("STRUCTURE DES DONNÉES\n")
    cat("=====================\n\n")
    str(values$data)
  })
  
  output$dataSummary <- renderPrint({
    req(values$data)
    cat("RÉSUMÉ STATISTIQUE\n")
    cat("==================\n\n")
    summary(values$data)
  })
  
  output$corrVarSelect <- renderUI({
    req(values$data)
    num_cols <- names(values$data)[sapply(values$data, is.numeric)]
    if (length(num_cols) == 0) return(NULL)
    
    tagList(
      pickerInput(
        inputId = "corrVars",
        label = "Sélectionnez les variables numériques:", 
        choices = num_cols,
        multiple = TRUE,
        selected = num_cols[1:min(5, length(num_cols))],
        options = list(`actions-box` = TRUE)
      ),
      actionButton("selectAllCorrVars", "Tout sélectionner", class = "btn-success btn-sm"),
      actionButton("deselectAllCorrVars", "Tout désélectionner", class = "btn-danger btn-sm")
    )
  })
  
  observeEvent(input$selectAllCorrVars, {
    num_cols <- names(values$data)[sapply(values$data, is.numeric)]
    updatePickerInput(session, "corrVars", selected = num_cols)
  })
  
  observeEvent(input$deselectAllCorrVars, {
    updatePickerInput(session, "corrVars", selected = character(0))
  })
  
  output$corrPlot <- renderPlot({
    req(values$data, input$corrVars)
    cor_data <- values$data[, input$corrVars, drop = FALSE]
    
    # Calcul de la corrélation selon la méthode choisie
    cor_method <- if (!is.null(input$corrMethod)) input$corrMethod else "pearson"
    cor_matrix <- cor(cor_data, use = "complete.obs", method = cor_method)
    
    # Titre personnalisé ou par défaut avec méthode
    method_label <- switch(cor_method,
                           "pearson" = "Pearson",
                           "spearman" = "Spearman", 
                           "kendall" = "Kendall")
    
    plot_title <- if (!is.null(input$corrTitle) && input$corrTitle != "") {
      input$corrTitle
    } else {
      paste("Matrice de corrélation -", method_label)
    }
    
    # Mode d'affichage
    display_method <- if (!is.null(input$corrDisplay)) input$corrDisplay else "number"
    
    # Type d'affichage (complet, upper, lower)
    display_type <- if (!is.null(input$corrType)) input$corrType else "upper"
    
    # Créer le graphique avec les paramètres personnalisables
    corrplot(cor_matrix, 
             method = display_method,
             type = display_type,
             order = "hclust", 
             tl.cex = if (!is.null(input$corrLabelSize)) input$corrLabelSize else 0.8, 
             tl.col = "black",
             number.cex = if (!is.null(input$corrTextSize)) input$corrTextSize else 0.8,
             title = plot_title,
             mar = c(0, 0, if (!is.null(input$corrCenterTitle) && input$corrCenterTitle) 2 else 1, 0),
             # Paramètres additionnels pour améliorer l'affichage
             addCoef.col = if (display_method %in% c("circle", "square", "ellipse", "color", "pie")) "black" else NULL,
             tl.srt = 45)  # Rotation des labels pour plus de lisibilité
  })
  
  # Bouton de mise à jour pour la corrélation
  observeEvent(input$updateCorrPlot, {
    output$corrPlot <- renderPlot({
      req(values$data, input$corrVars)
      cor_data <- values$data[, input$corrVars, drop = FALSE]
      
      # Calcul de la corrélation selon la méthode choisie
      cor_matrix <- cor(cor_data, use = "complete.obs", method = input$corrMethod)
      
      # Titre avec méthode
      method_label <- switch(input$corrMethod,
                             "pearson" = "Pearson",
                             "spearman" = "Spearman", 
                             "kendall" = "Kendall")
      
      plot_title <- if (!is.null(input$corrTitle) && input$corrTitle != "") {
        input$corrTitle
      } else {
        paste("Matrice de corrélation -", method_label)
      }
      
      # Créer le graphique avec tous les paramètres
      corrplot(cor_matrix, 
               method = input$corrDisplay,
               type = input$corrType,
               order = "hclust", 
               tl.cex = input$corrLabelSize, 
               tl.col = "black",
               number.cex = input$corrTextSize,
               title = plot_title,
               mar = c(0, 0, if (input$corrCenterTitle) 2 else 1, 0),
               addCoef.col = if (input$corrDisplay %in% c("circle", "square", "ellipse", "color", "pie")) "black" else NULL,
               tl.srt = 45)
    })
  })
  
  output$distVarSelect <- renderUI({
    req(values$data)
    num_cols <- names(values$data)[sapply(values$data, is.numeric)]
    if (length(num_cols) == 0) return(NULL)
    selectInput("distVar", "Sélectionnez une variable:", choices = num_cols)
  })
  
  output$distPlot <- renderPlot({
    req(values$data, input$distVar)
    
    # Titre personnalisé ou par défaut
    plot_title <- if (!is.null(input$distTitle) && input$distTitle != "") {
      input$distTitle
    } else {
      paste("Distribution de", input$distVar)
    }
    
    p <- ggplot(values$data, aes_string(x = input$distVar)) +
      geom_histogram(aes(y = ..density..), fill = "lightblue", color = "black", alpha = 0.7, bins = 30)
    
    # Ajouter la courbe de densité si demandée
    if (is.null(input$distShowDensity) || input$distShowDensity) {
      p <- p + geom_density(color = "red", size = 1.2)
    }
    
    p <- p + theme_minimal() +
      labs(title = plot_title, x = input$distVar, y = "Densité") +
      theme(
        plot.title = element_text(
          size = if (!is.null(input$distTitleSize)) input$distTitleSize else 14,
          hjust = if (is.null(input$distCenterTitle) || input$distCenterTitle) 0.5 else 0
        ),
        axis.title = element_text(size = if (!is.null(input$distAxisTitleSize)) input$distAxisTitleSize else 12),
        axis.text = element_text(size = if (!is.null(input$distAxisTextSize)) input$distAxisTextSize else 10),
        legend.text = element_text(size = if (!is.null(input$distLegendTextSize)) input$distLegendTextSize else 10),
        legend.title = element_text(size = if (!is.null(input$distLegendTextSize)) input$distLegendTextSize else 10)
      )
    
    print(p)
  })
  
  # Bouton de mise à jour pour la distribution
  observeEvent(input$updateDistPlot, {
    output$distPlot <- renderPlot({
      req(values$data, input$distVar)
      
      plot_title <- if (!is.null(input$distTitle) && input$distTitle != "") {
        input$distTitle
      } else {
        paste("Distribution de", input$distVar)
      }
      
      p <- ggplot(values$data, aes_string(x = input$distVar)) +
        geom_histogram(aes(y = ..density..), fill = "lightblue", color = "black", alpha = 0.7, bins = 30)
      
      if (input$distShowDensity) {
        p <- p + geom_density(color = "red", size = 1.2)
      }
      
      p <- p + theme_minimal() +
        labs(title = plot_title, x = input$distVar, y = "Densité") +
        theme(
          plot.title = element_text(
            size = input$distTitleSize,
            hjust = if (input$distCenterTitle) 0.5 else 0
          ),
          axis.title = element_text(size = input$distAxisTitleSize),
          axis.text = element_text(size = input$distAxisTextSize),
          legend.text = element_text(size = input$distLegendTextSize),
          legend.title = element_text(size = input$distLegendTextSize)
        )
      
      print(p)
    })
  })
  
  output$missingPlot <- renderPlot({
    req(values$data)
    
    missing_data <- values$data %>%
      summarise(across(everything(), ~sum(is.na(.)))) %>%
      pivot_longer(everything(), names_to = "Variable", values_to = "Missing") %>%
      mutate(PctMissing = Missing / nrow(values$data) * 100)
    
    # Titre personnalisé ou par défaut
    plot_title <- if (!is.null(input$missingTitle) && input$missingTitle != "") {
      input$missingTitle
    } else {
      "Analyse des valeurs manquantes"
    }
    
    p <- ggplot(missing_data, aes(x = reorder(Variable, -Missing), y = Missing)) +
      geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
      geom_text(aes(label = paste0(round(PctMissing, 1), "%")), 
                vjust = -0.5, 
                size = 3.5) +
      theme_minimal() +
      labs(title = plot_title, 
           x = "Variable", 
           y = "Nombre de valeurs manquantes") +
      theme(
        plot.title = element_text(
          size = if (!is.null(input$missingTitleSize)) input$missingTitleSize else 14,
          hjust = if (is.null(input$missingCenterTitle) || input$missingCenterTitle) 0.5 else 0
        ),
        axis.title = element_text(size = if (!is.null(input$missingAxisTitleSize)) input$missingAxisTitleSize else 12),
        axis.text = element_text(size = if (!is.null(input$missingAxisTextSize)) input$missingAxisTextSize else 10),
        axis.text.x = element_text(
          angle = if (is.null(input$missingRotateLabels) || input$missingRotateLabels) 45 else 0, 
          hjust = if (is.null(input$missingRotateLabels) || input$missingRotateLabels) 1 else 0.5
        )
      )
    
    print(p)
  })
  
  # Bouton de téléchargement pour la corrélation
  observeEvent(input$updateMissingPlot, {
    output$missingPlot <- renderPlot({
      req(values$data)
      
      missing_data <- values$data %>%
        summarise(across(everything(), ~sum(is.na(.)))) %>%
        pivot_longer(everything(), names_to = "Variable", values_to = "Missing") %>%
        mutate(PctMissing = Missing / nrow(values$data) * 100)
      
      plot_title <- if (!is.null(input$missingTitle) && input$missingTitle != "") {
        input$missingTitle
      } else {
        "Analyse des valeurs manquantes"
      }
      
      p <- ggplot(missing_data, aes(x = reorder(Variable, -Missing), y = Missing)) +
        geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
        geom_text(aes(label = paste0(round(PctMissing, 1), "%")), 
                  vjust = -0.5, 
                  size = 3.5) +
        theme_minimal() +
        labs(title = plot_title, 
             x = "Variable", 
             y = "Nombre de valeurs manquantes") +
        theme(
          plot.title = element_text(
            size = input$missingTitleSize,
            hjust = if (input$missingCenterTitle) 0.5 else 0
          ),
          axis.title = element_text(size = input$missingAxisTitleSize),
          axis.text = element_text(size = input$missingAxisTextSize),
          axis.text.x = element_text(
            angle = if (input$missingRotateLabels) 45 else 0, 
            hjust = if (input$missingRotateLabels) 1 else 0.5
          )
        )
      
      print(p)
    })
  })
  
  # ---- FONCTIONS DE TÉLÉCHARGEMENT ----
  
  # Téléchargement matrice de corrélation
  output$downloadCorrPlot <- downloadHandler(
    filename = function() {
      paste0("matrice_correlation_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(values$data, input$corrVars)
      
      # Recréer le graphique avec les paramètres actuels
      cor_data <- values$data[, input$corrVars, drop = FALSE]
      cor_method <- input$corrMethod %||% "pearson"
      cor_matrix <- cor(cor_data, use = "complete.obs", method = cor_method)
      
      method_label <- switch(cor_method,
                             "pearson" = "Pearson",
                             "spearman" = "Spearman", 
                             "kendall" = "Kendall")
      
      plot_title <- if (!is.null(input$corrTitle) && input$corrTitle != "") {
        input$corrTitle
      } else {
        paste("Matrice de corrélation -", method_label)
      }
      
      display_method <- input$corrDisplay %||% "number"
      display_type <- input$corrType %||% "upper"
      
      # Créer le fichier PNG avec le DPI spécifié
      png(file, width = 2400, height = 2400, res = input$corrDPI %||% 300, type = "cairo")
      
      corrplot(cor_matrix, 
               method = display_method,
               type = display_type,
               order = "hclust", 
               tl.cex = input$corrLabelSize %||% 0.8, 
               tl.col = "black",
               number.cex = input$corrTextSize %||% 0.8,
               title = plot_title,
               mar = c(0, 0, if (input$corrCenterTitle %||% TRUE) 2 else 1, 0),
               addCoef.col = if (display_method %in% c("circle", "square", "ellipse", "color", "pie")) "black" else NULL,
               tl.srt = 45)
      
      dev.off()
    }
  )
  
  # Téléchargement distribution
  output$downloadDistPlot <- downloadHandler(
    filename = function() {
      paste0("distribution_", input$distVar, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(values$data, input$distVar)
      
      # Recréer le graphique avec les paramètres actuels
      plot_title <- if (!is.null(input$distTitle) && input$distTitle != "") {
        input$distTitle
      } else {
        paste("Distribution de", input$distVar)
      }
      
      p <- ggplot(values$data, aes_string(x = input$distVar)) +
        geom_histogram(aes(y = ..density..), fill = "lightblue", color = "black", alpha = 0.7, bins = 30)
      
      if (is.null(input$distShowDensity) || input$distShowDensity) {
        p <- p + geom_density(color = "red", size = 1.2)
      }
      
      p <- p + theme_minimal() +
        labs(title = plot_title, x = input$distVar, y = "Densité") +
        theme(
          plot.title = element_text(
            size = input$distTitleSize %||% 14,
            hjust = if (is.null(input$distCenterTitle) || input$distCenterTitle) 0.5 else 0
          ),
          axis.title = element_text(size = input$distAxisTitleSize %||% 12),
          axis.text = element_text(size = input$distAxisTextSize %||% 10),
          legend.text = element_text(size = input$distLegendTextSize %||% 10),
          legend.title = element_text(size = input$distLegendTextSize %||% 10)
        )
      
      # Sauvegarder avec le DPI spécifié
      ggsave(file, plot = p, width = 10, height = 8, dpi = input$distDPI %||% 300, type = "cairo-png")
    }
  )
  
  # Téléchargement valeurs manquantes
  output$downloadMissingPlot <- downloadHandler(
    filename = function() {
      paste0("valeurs_manquantes_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(values$data)
      
      # Recréer le graphique avec les paramètres actuels
      missing_data <- values$data %>%
        summarise(across(everything(), ~sum(is.na(.)))) %>%
        pivot_longer(everything(), names_to = "Variable", values_to = "Missing") %>%
        mutate(PctMissing = Missing / nrow(values$data) * 100)
      
      plot_title <- if (!is.null(input$missingTitle) && input$missingTitle != "") {
        input$missingTitle
      } else {
        "Analyse des valeurs manquantes"
      }
      
      p <- ggplot(missing_data, aes(x = reorder(Variable, -Missing), y = Missing)) +
        geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
        geom_text(aes(label = paste0(round(PctMissing, 1), "%")), 
                  vjust = -0.5, 
                  size = 3.5) +
        theme_minimal() +
        labs(title = plot_title, 
             x = "Variable", 
             y = "Nombre de valeurs manquantes") +
        theme(
          plot.title = element_text(
            size = input$missingTitleSize %||% 14,
            hjust = if (is.null(input$missingCenterTitle) || input$missingCenterTitle) 0.5 else 0
          ),
          axis.title = element_text(size = input$missingAxisTitleSize %||% 12),
          axis.text = element_text(size = input$missingAxisTextSize %||% 10),
          axis.text.x = element_text(
            angle = if (is.null(input$missingRotateLabels) || input$missingRotateLabels) 45 else 0, 
            hjust = if (is.null(input$missingRotateLabels) || input$missingRotateLabels) 1 else 0.5
          )
        )
      
      # Sauvegarder avec le DPI spécifié
      ggsave(file, plot = p, width = 12, height = 8, dpi = input$missingDPI %||% 300, type = "cairo-png")
    }
  )
  # ---- Nettoyage ----
  output$varTypeUI <- renderUI({
    req(values$data)
    lapply(names(values$data), function(col) {
      selectInput(paste0("type_", col), col,
                  choices = c("Numérique" = "numeric", "Facteur" = "factor", "Texte" = "character", "Date" = "date"),
                  selected = ifelse(is.numeric(values$data[[col]]), "numeric", 
                                    ifelse(is.factor(values$data[[col]]), "factor", 
                                           ifelse(inherits(values$data[[col]], "Date"), "date", "character"))))
    })
  })
  
  observeEvent(input$applyTypes, {
    data_temp <- values$cleanData
    for (col in names(data_temp)) {
      type_input <- input[[paste0("type_", col)]]
      if (!is.null(type_input)) {
        if (type_input == "numeric") data_temp[[col]] <- suppressWarnings(as.numeric(as.character(data_temp[[col]])))
        else if (type_input == "factor") data_temp[[col]] <- as.factor(data_temp[[col]])
        else if (type_input == "character") data_temp[[col]] <- as.character(data_temp[[col]])
        else if (type_input == "date") data_temp[[col]] <- as.Date(data_temp[[col]])
      }
    }
    values$cleanData <- data_temp
    values$filteredData <- values$cleanData
    showNotification("Types de variables appliqués avec succès", type = "message")
  })
  
  output$removeVarUI <- renderUI({
    req(values$cleanData)
    selectInput("removeVarName", "Supprimer variable :", choices = names(values$cleanData))
  })
  
  observeEvent(input$removeVar, {
    req(input$removeVarName)
    values$cleanData <- values$cleanData[, !(names(values$cleanData) %in% input$removeVarName), drop = FALSE]
    values$filteredData <- values$cleanData
    showNotification(paste("Variable", input$removeVarName, "supprimée"), type = "message")
  })
  
  observeEvent(input$addVar, {
    req(input$newVarName)
    if (input$newVarName %in% names(values$cleanData)) {
      showNotification("La variable existe déjà", type = "warning")
    } else {
      values$cleanData[[input$newVarName]] <- rep(input$newVarValue, nrow(values$cleanData))
      values$filteredData <- values$cleanData
      showNotification(paste("Variable", input$newVarName, "ajoutée"), type = "message")
    }
  })
  
  output$colPicker <- renderUI({
    req(values$cleanData)
    selectInput("colInsert", "Insérer colonne :", choices = names(values$cleanData))
  })
  
  observeEvent(input$colInsert, {
    updateTextInput(session, "calcFormula", value = paste0(input$calcFormula %||% "", ifelse(nchar(input$calcFormula %||% "") > 0, " ", ""), input$colInsert, " "))
  })
  
  observeEvent(input$insertPlus, { updateTextInput(session, "calcFormula", value = paste0(input$calcFormula %||% "", " + ")) })
  observeEvent(input$insertMoins, { updateTextInput(session, "calcFormula", value = paste0(input$calcFormula %||% "", " - ")) })
  observeEvent(input$insertMult, { updateTextInput(session, "calcFormula", value = paste0(input$calcFormula %||% "", " * ")) })
  observeEvent(input$insertDiv, { updateTextInput(session, "calcFormula", value = paste0(input$calcFormula %||% "", " / ")) })
  observeEvent(input$insertLog, { updateTextInput(session, "calcFormula", value = paste0(input$calcFormula %||% "", " log() ")) })
  observeEvent(input$insertSqrt, { updateTextInput(session, "calcFormula", value = paste0(input$calcFormula %||% "", " sqrt() ")) })
  
  observeEvent(input$addCalcVar, {
    req(input$calcVarName, input$calcFormula)
    tryCatch({
      new_col <- with(values$cleanData, eval(parse(text = input$calcFormula)))
      if (length(new_col) != nrow(values$cleanData)) {
        showNotification("Erreur : la formule ne renvoie pas une colonne valide", type = "error")
        return()
      }
      values$cleanData[[input$calcVarName]] <- new_col
      values$filteredData <- values$cleanData
      showNotification(paste("Variable", input$calcVarName, "créée"), type = "message")
    }, error = function(e) {
      showNotification(paste("Erreur formule:", e$message), type = "error")
    })
  })
  
  output$naVarSelect <- renderUI({
    req(values$cleanData)
    pickerInput(
      inputId = "naVars",
      label = "Sélectionnez les variables à traiter:", 
      choices = names(values$cleanData),
      multiple = TRUE,
      options = list(`actions-box` = TRUE)
    )
  })
  
  observeEvent(input$applyNA, {
    req(values$cleanData, input$naVars)
    data_temp <- values$cleanData
    
    for (col in input$naVars) {
      if (input$naMethod == "remove") {
        data_temp <- data_temp[!is.na(data_temp[[col]]), ]
      } else if (input$naMethod == "mean") {
        mean_val <- mean(data_temp[[col]], na.rm = TRUE)
        data_temp[[col]][is.na(data_temp[[col]])] <- mean_val
      } else if (input$naMethod == "median") {
        median_val <- median(data_temp[[col]], na.rm = TRUE)
        data_temp[[col]][is.na(data_temp[[col]])] <- median_val
      } else if (input$naMethod == "value") {
        data_temp[[col]][is.na(data_temp[[col]])] <- input$naValue
      }
    }
    
    values$cleanData <- data_temp
    values$filteredData <- values$cleanData
    showNotification("Traitement des valeurs manquantes appliqué", type = "message")
  })
  
  output$cleanedData <- renderDT({
    req(values$cleanData)
    datatable(values$cleanData, options = list(scrollX = TRUE))
  })
  
  # ---- Filtrage ----
  output$filterFactorA <- renderUI({
    req(values$cleanData)
    fac_cols <- names(values$cleanData)[sapply(values$cleanData, is.factor)]
    if (length(fac_cols) == 0) {
      helpText("Aucun facteur. Convertissez d'abord des variables en facteurs dans l'onglet Nettoyage.")
    } else {
      selectInput("factorA", "Facteur A :", choices = fac_cols)
    }
  })
  
  output$filterFactorB <- renderUI({
    req(values$cleanData)
    fac_cols <- names(values$cleanData)[sapply(values$cleanData, is.factor)]
    if (length(fac_cols) == 0) {
      NULL
    } else {
      selectInput("factorB", "Facteur B :", choices = fac_cols, selected = fac_cols[min(2, length(fac_cols))])
    }
  })
  
  observeEvent(input$applyCrossFilter, {
    req(values$cleanData, input$factorA, input$factorB)
    validate(need(input$factorA != input$factorB, "Choisissez deux facteurs distincts."))
    tryCatch({
      df <- values$cleanData
      filtered <- filter_complete_cross(df, input$factorA, input$factorB,
                                        reqA = isTRUE(input$requireA),
                                        reqB = isTRUE(input$requireB))
      values$filteredData <- filtered
      showNotification(paste("Filtrage (2 facteurs) appliqué. Lignes:", nrow(filtered)), type = "message")
    }, error = function(e) {
      showNotification(paste("Erreur filtrage:", e$message), type = "error")
    })
  })
  
  output$filterFactorsN <- renderUI({
    req(values$cleanData)
    fac_cols <- names(values$cleanData)[sapply(values$cleanData, is.factor)]
    selectInput("factorsN", "Facteurs (>=2) :", choices = fac_cols, multiple = TRUE)
  })
  
  observeEvent(input$applyCrossFilterN, {
    req(values$cleanData, input$factorsN)
    validate(need(length(input$factorsN) >= 2, "Sélectionnez au moins deux facteurs."))
    tryCatch({
      filtered <- filter_complete_cross_n(values$cleanData, input$factorsN)
      values$filteredData <- filtered
      showNotification(paste("Filtrage (N facteurs) appliqué. Lignes:", nrow(filtered)), type = "message")
    }, error = function(e) {
      showNotification(paste("Erreur filtrage N facteurs:", e$message), type = "error")
    })
  })
  
  observeEvent(input$resetFilter, {
    values$filteredData <- values$cleanData
    showNotification("Filtre réinitialisé", type = "message")
  })
  
  output$originalRows <- renderValueBox({
    req(values$cleanData)
    valueBox(
      nrow(values$cleanData), "Lignes originales", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$filteredRows <- renderValueBox({
    req(values$filteredData)
    valueBox(
      nrow(values$filteredData), "Lignes filtrées", icon = icon("filter"),
      color = "green"
    )
  })
  
  output$removedRows <- renderValueBox({
    req(values$cleanData, values$filteredData)
    removed <- nrow(values$cleanData) - nrow(values$filteredData)
    valueBox(
      removed, "Lignes supprimées", icon = icon("trash"),
      color = ifelse(removed > 0, "red", "green")
    )
  })
  
  output$completeCases <- renderValueBox({
    req(values$filteredData)
    complete <- sum(complete.cases(values$filteredData))
    valueBox(
      complete, "Cas complets", icon = icon("check"),
      color = "blue"
    )
  })
  
  output$filteredData <- renderDT({
    req(values$filteredData)
    datatable(values$filteredData, options = list(scrollX = TRUE))
  })
  
  output$filterSummary <- renderPrint({
    req(values$cleanData, values$filteredData)
    cat("Nombre de lignes originales :", nrow(values$cleanData), "\n")
    cat("Nombre de lignes filtrées :", nrow(values$filteredData), "\n")
    cat("Nombre de lignes supprimées :", nrow(values$cleanData) - nrow(values$filteredData), "\n")
  })
  
  # ---- Analyse descriptives ----
  output$numVarSelect <- renderUI({
    req(values$filteredData)
    num_cols <- names(values$filteredData)[sapply(values$filteredData, is.numeric)]
    if (length(num_cols) == 0) return(NULL)
    
    tagList(
      pickerInput(
        inputId = "numVars",
        label = "Sélectionnez les variables numériques:", 
        choices = num_cols,
        multiple = TRUE,
        selected = num_cols[1:min(5, length(num_cols))],
        options = list(`actions-box` = TRUE)
      ),
      actionButton("selectAllNumVars", "Tout sélectionner", class = "btn-success btn-sm"),
      actionButton("deselectAllNumVars", "Tout désélectionner", class = "btn-danger btn-sm")
    )
  })
  
  observeEvent(input$selectAllNumVars, {
    num_cols <- names(values$filteredData)[sapply(values$filteredData, is.numeric)]
    updatePickerInput(session, "numVars", selected = num_cols)
  })
  
  observeEvent(input$deselectAllNumVars, {
    updatePickerInput(session, "numVars", selected = character(0))
  })
  
  output$descFactorUI <- renderUI({
    req(values$filteredData)
    fac_cols <- names(values$filteredData)[sapply(values$filteredData, is.factor)]
    tagList(
      pickerInput("descFactors", "Calcul par facteurs (optionnel)", 
                  choices = fac_cols, multiple = TRUE,
                  options = list(`actions-box` = TRUE)),
      helpText("Laissez vide pour des descriptives globales. Sélectionnez un ou plusieurs facteurs pour des descriptives par traitement/facteur.")
    )
  })
  
  observeEvent(input$selectAllNum, {
    req(values$filteredData)
    num_cols <- names(values$filteredData)[sapply(values$filteredData, is.numeric)]
    if (input$selectAllNum) {
      updatePickerInput(session, "numVars", selected = num_cols)
    } else {
      updatePickerInput(session, "numVars", selected = character(0))
    }
  })
  
  observeEvent(input$calcDesc, {
    req(input$numVars)
    tryCatch({
      stats_sel <- input$descStats
      
      make_summ_global <- function(df_in, num_vars, stats_sel) {
        summ_list <- lapply(num_vars, function(var) {
          x <- df_in[[var]]
          stats <- list()
          if ("mean" %in% stats_sel) stats$mean = mean(x, na.rm = TRUE)
          if ("median" %in% stats_sel) stats$median = median(x, na.rm = TRUE)
          if ("sd" %in% stats_sel) stats$sd = sd(x, na.rm = TRUE)
          if ("var" %in% stats_sel) stats$var = var(x, na.rm = TRUE)
          if ("cv" %in% stats_sel) stats$cv = calc_cv(x)
          if ("min" %in% stats_sel) stats$min = min(x, na.rm = TRUE)
          if ("max" %in% stats_sel) stats$max = max(x, na.rm = TRUE)
          if ("q1" %in% stats_sel) stats$q1 = quantile(x, 0.25, na.rm = TRUE)
          if ("q3" %in% stats_sel) stats$q3 = quantile(x, 0.75, na.rm = TRUE)
          
          data.frame(Facteurs = "Global", Variable = var, as.data.frame(stats), check.names = FALSE)
        })
        do.call(rbind, summ_list)
      }
      
      make_summ_grouped <- function(df_in, group_vars, num_vars, stats_sel) {
        df_grouped <- df_in %>%
          group_by(across(all_of(group_vars))) %>%
          summarise(across(all_of(num_vars),
                           list(
                             mean = ~mean(.x, na.rm = TRUE),
                             median = ~median(.x, na.rm = TRUE),
                             sd = ~sd(.x, na.rm = TRUE),
                             var = ~var(.x, na.rm = TRUE),
                             cv = ~calc_cv(.x),
                             min = ~min(.x, na.rm = TRUE),
                             max = ~max(.x, na.rm = TRUE),
                             q1 = ~quantile(.x, 0.25, na.rm = TRUE),
                             q3 = ~quantile(.x, 0.75, na.rm = TRUE)
                           ),
                           .names = "{.col}__{.fn}"),
                    .groups = "drop")
        
        df_long <- df_grouped %>%
          pivot_longer(cols = -all_of(group_vars),
                       names_to = c("Variable", ".value"),
                       names_sep = "__")
        
        selected_cols <- c(group_vars, "Variable", stats_sel)
        df_long <- df_long[, selected_cols, drop = FALSE]
        
        return(df_long)
      }
      
      if (!is.null(input$descFactors) && length(input$descFactors) > 0) {
        values$descStats <- make_summ_grouped(values$filteredData, input$descFactors, input$numVars, stats_sel)
      } else {
        values$descStats <- make_summ_global(values$filteredData, input$numVars, stats_sel)
      }
    }, error = function(e) {
      showNotification(paste("Erreur dans les descriptives :", e$message), type = "error")
    })
  })
  
  output$descResults <- renderDT({
    req(values$descStats)
    datatable(values$descStats, options = list(scrollX = TRUE))
  })
  
  output$downloadDesc <- downloadHandler(
    filename = function() {
      paste0("descriptives_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      write.csv(values$descStats, file, row.names = FALSE)
    }
  )
  
  output$descPlotVarSelect <- renderUI({
    req(values$filteredData)
    num_cols <- names(values$filteredData)[sapply(values$filteredData, is.numeric)]
    selectInput("descPlotVar", "Variable à visualiser:", choices = num_cols)
  })
  
  output$descPlotFactorSelect <- renderUI({
    req(values$filteredData)
    fac_cols <- names(values$filteredData)[sapply(values$filteredData, is.factor)]
    selectInput("descPlotFactor", "Grouper par:", choices = c("Aucun", fac_cols))
  })
  
  output$descPlot <- renderPlotly({
    req(values$filteredData, input$descPlotVar)
    p <- if (input$descPlotFactor == "Aucun") {
      ggplot(values$filteredData, aes_string(x = input$descPlotVar)) +
        geom_histogram(aes(y = ..density..), alpha = 0.7) +
        geom_density(size = 1) +
        theme_minimal() +
        labs(title = paste("Distribution de", input$descPlotVar)) +
        theme(axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black"))
    } else {
      ggplot(values$filteredData, aes_string(x = input$descPlotFactor, y = input$descPlotVar, fill = input$descPlotFactor)) +
        geom_boxplot(alpha = 0.7) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = paste(input$descPlotVar, "par", input$descPlotFactor)) +
        theme(axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black"))
    }
    ggplotly(p)
  })
  
  
  # ---- Analyses multivariées ----
  
  # Sélection du groupe pour les moyennes (ACP)
  output$pcaMeansGroupSelect <- renderUI({
    req(values$filteredData)
    fac_cols <- names(values$filteredData)[sapply(values$filteredData, is.factor)]
    if (length(fac_cols) == 0) return(NULL)
    
    selectInput("pcaMeansGroup", "Variable de groupement pour les moyennes:", 
                choices = fac_cols)
  })
  
  output$pcaVarSelect <- renderUI({
    req(values$filteredData)
    num_cols <- names(values$filteredData)[sapply(values$filteredData, is.numeric)]
    if (length(num_cols) == 0) return(NULL)
    
    pickerInput(
      inputId = "pcaVars",
      label = "Sélectionnez les variables pour l'ACP:",
      choices = num_cols,
      multiple = TRUE,
      selected = num_cols,
      options = list(`actions-box` = TRUE)
    )
  })
  
  output$pcaQualiSupSelect <- renderUI({
    req(values$filteredData)
    fac_cols <- names(values$filteredData)[sapply(values$filteredData, is.factor)]
    if (length(fac_cols) == 0) return(NULL)
    
    pickerInput(
      inputId = "pcaQualiSup",
      label = "Variables qualitatives supplémentaires:",
      choices = fac_cols,
      multiple = TRUE,
      options = list(`actions-box` = TRUE)
    )
  })
  
  output$pcaIndSupSelect <- renderUI({
    req(values$filteredData)
    num_cols <- names(values$filteredData)[sapply(values$filteredData, is.numeric)]
    if (length(num_cols) == 0) return(NULL)
    
    pickerInput(
      inputId = "pcaIndSup",
      label = "Individus supplémentaires (optionnel):",
      choices = rownames(values$filteredData),
      multiple = TRUE,
      options = list(`actions-box` = TRUE)
    )
  })
  
  # Select source for individual labels
  output$pcaLabelSourceSelect <- renderUI({
    req(values$filteredData)
    all_cols <- names(values$filteredData)
    selectInput("pcaLabelSource", "Source des labels pour individus (optionnel):",
                choices = c("Rownames" = "rownames", all_cols), selected = "rownames")
  })
  
  # Fonction pour calculer les moyennes par groupe
  calculate_group_means <- function(data, vars, group_var) {
    if (is.null(group_var) || !group_var %in% names(data)) {
      return(data)
    }
    
    # Calculer les moyennes par groupe
    means_data <- data %>%
      group_by(!!sym(group_var)) %>%
      summarise(across(all_of(vars), mean, na.rm = TRUE), .groups = 'drop') %>%
      column_to_rownames(group_var)
    
    return(means_data)
  }
  
  # ACP avec les nouvelles fonctions
  observeEvent(input$runPCA, {
    req(values$filteredData, input$pcaVars)
    tryCatch({
      # Préparation des données
      if (input$pcaUseMeans && !is.null(input$pcaMeansGroup)) {
        # Utiliser les moyennes par groupe
        pca_data <- calculate_group_means(values$filteredData, input$pcaVars, input$pcaMeansGroup)
      } else {
        # Utiliser les données originales
        pca_data <- values$filteredData[, input$pcaVars, drop = FALSE]
      }
      
      # Variables qualitatives supplémentaires
      quali_sup_indices <- NULL
      if (!is.null(input$pcaQualiSup) && !input$pcaUseMeans) {
        quali_sup_indices <- which(names(values$filteredData) %in% input$pcaQualiSup)
      }
      
      # Individus supplémentaires
      ind_sup_indices <- NULL
      if (!is.null(input$pcaIndSup) && !input$pcaUseMeans) {
        ind_sup_indices <- which(rownames(values$filteredData) %in% input$pcaIndSup)
      }
      
      # Combinaison des données
      if (!input$pcaUseMeans) {
        all_data <- cbind(pca_data, values$filteredData[, input$pcaQualiSup, drop = FALSE])
        
        # Set custom labels for individuals, handling duplicates
        if (input$pcaLabelSource != "rownames") {
          custom_labels <- as.character(values$filteredData[[input$pcaLabelSource]])
          rownames(all_data) <- make.unique(custom_labels)
        }
      } else {
        all_data <- pca_data
      }
      
      # Exécution de l'ACP avec les paramètres demandés
      res.pca <- PCA(all_data,
                     scale.unit = input$pcaScale,
                     quali.sup = quali_sup_indices,
                     ind.sup = ind_sup_indices,
                     ncp = input$pcaComponents,
                     graph = FALSE)
      
      values$pcaResult <- res.pca
      
      # Résumé
      output$pcaSummary <- renderPrint({
        summary(res.pca)
      })
      
      # Visualisation avec les fonctions fviz demandées
      output$pcaPlot <- renderPlotly({
        req(values$pcaResult)
        
        tryCatch({
          # Calcul des pourcentages de variance
          eigenvals <- get_eigenvalue(values$pcaResult)
          pc1_var <- round(eigenvals[1, "variance.percent"], 1)
          pc2_var <- round(eigenvals[2, "variance.percent"], 1)
          
          # Labels par défaut ou personnalisés
          x_label <- if (!is.null(input$pcaXLabel) && input$pcaXLabel != "") {
            input$pcaXLabel
          } else {
            paste0("PC1 (", pc1_var, "%)")
          }
          
          y_label <- if (!is.null(input$pcaYLabel) && input$pcaYLabel != "") {
            input$pcaYLabel
          } else {
            paste0("PC2 (", pc2_var, "%)")
          }
          
          # Titre personnalisé
          plot_title <- if (!is.null(input$pcaPlotTitle) && input$pcaPlotTitle != "") {
            input$pcaPlotTitle
          } else {
            "ACP - Analyse en Composantes Principales"
          }
          
          if (input$pcaPlotType == "var") {
            # Graphique des variables avec fviz_pca_var
            p <- fviz_pca_var(values$pcaResult, 
                              col.var = "cos2",
                              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                              repel = TRUE,
                              title = plot_title) +
              labs(x = x_label, y = y_label)
            
          } else if (input$pcaPlotType == "ind") {
            # Graphique des individus avec fviz_pca_ind
            has_ind_sup <- !is.null(values$pcaResult$ind.sup)
            
            if (has_ind_sup) {
              n_active <- nrow(values$pcaResult$ind$coord)
              col_vector <- rep("Active", n_active)
              
              p <- fviz_pca_ind(values$pcaResult,
                                geom.ind = c("point", "text"),
                                col.ind = col_vector,
                                palette = c("Active" = "#00AFBB"),
                                addEllipses = FALSE,
                                repel = TRUE,
                                title = plot_title,
                                legend.title = "Type") +
                labs(x = x_label, y = y_label)
            } else {
              p <- fviz_pca_ind(values$pcaResult, 
                                col.ind = "contrib",
                                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                repel = TRUE,
                                title = plot_title) +
                labs(x = x_label, y = y_label)
            }
            
          } else if (input$pcaPlotType == "biplot") {
            # Biplot avec fviz_pca_biplot
            has_ind_sup <- !is.null(values$pcaResult$ind.sup)
            
            if (has_ind_sup) {
              n_active <- nrow(values$pcaResult$ind$coord)
              col_vector <- rep("Active", n_active)
              
              p <- fviz_pca_biplot(values$pcaResult,
                                   geom.ind = "point",
                                   col.ind = col_vector,
                                   palette = c("Active" = "#00AFBB"),
                                   col.var = "#FC4E07",
                                   repel = TRUE,
                                   addEllipses = FALSE,
                                   title = plot_title,
                                   legend.title = "Type") +
                labs(x = x_label, y = y_label)
            } else {
              p <- fviz_pca_biplot(values$pcaResult, 
                                   repel = TRUE,
                                   col.var = "#FC4E07", 
                                   col.ind = "#00AFBB",
                                   label = "all",
                                   title = plot_title) +
                labs(x = x_label, y = y_label)
            }
          }
          
          # Centrage sur (0,0) si demandé
          if (!is.null(input$pcaCenterAxes) && input$pcaCenterAxes) {
            # Obtenir les limites actuelles
            if (input$pcaPlotType == "var") {
              coords <- values$pcaResult$var$coord[, 1:2]
            } else if (input$pcaPlotType == "ind") {
              coords <- values$pcaResult$ind$coord[, 1:2]
            } else { # biplot
              coords_ind <- values$pcaResult$ind$coord[, 1:2]
              coords_var <- values$pcaResult$var$coord[, 1:2]
              coords <- rbind(coords_ind, coords_var)
            }
            
            # Calculer les limites symétriques
            max_range <- max(abs(range(coords, na.rm = TRUE)))
            p <- p + xlim(-max_range, max_range) + ylim(-max_range, max_range)
          }
          
          # Convertir en plotly
          suppressWarnings({
            plotly_obj <- ggplotly(p) %>% 
              layout(showlegend = TRUE) %>%
              config(displayModeBar = TRUE)
          })
          
          plotly_obj
          
        }, error = function(e) {
          # Version de base en cas d'erreur
          if (input$pcaPlotType == "var") {
            p <- fviz_pca_var(values$pcaResult, 
                              col.var = "cos2",
                              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                              repel = FALSE,
                              title = "ACP - Cercle de corrélation des variables")
          } else if (input$pcaPlotType == "ind") {
            p <- fviz_pca_ind(values$pcaResult,
                              geom.ind = "point",
                              col.ind = "#00AFBB",
                              addEllipses = FALSE,
                              title = "ACP - Projection des individus")
          } else {
            p <- fviz_pca_biplot(values$pcaResult,
                                 geom.ind = "point",
                                 geom.var = c("arrow", "text"),
                                 col.var = "#FC4E07",
                                 col.ind = "#00AFBB",
                                 repel = FALSE,
                                 addEllipses = FALSE,
                                 title = "ACP - Biplot")
          }
          
          suppressWarnings({
            ggplotly(p) %>% 
              layout(showlegend = TRUE) %>%
              config(displayModeBar = TRUE)
          })
        })
      })
    }, error = function(e) {
      showNotification(paste("Erreur ACP :", e$message), type = "error")
    })
  })
  
  # Download for PCA plot
  output$downloadPcaPlot <- downloadHandler(
    filename = function() { paste("pca_plot", Sys.Date(), ".png", sep = "") },
    content = function(file) {
      eigenvals <- get_eigenvalue(values$pcaResult)
      pc1_var <- round(eigenvals[1, "variance.percent"], 1)
      pc2_var <- round(eigenvals[2, "variance.percent"], 1)
      
      x_label <- if (!is.null(input$pcaXLabel) && input$pcaXLabel != "") {
        input$pcaXLabel
      } else {
        paste0("PC1 (", pc1_var, "%)")
      }
      
      y_label <- if (!is.null(input$pcaYLabel) && input$pcaYLabel != "") {
        input$pcaYLabel
      } else {
        paste0("PC2 (", pc2_var, "%)")
      }
      
      plot_title <- if (!is.null(input$pcaPlotTitle) && input$pcaPlotTitle != "") {
        input$pcaPlotTitle
      } else {
        "ACP - Analyse en Composantes Principales"
      }
      
      p <- switch(input$pcaPlotType,
                  "var" = fviz_pca_var(values$pcaResult, col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE, title = plot_title) + labs(x = x_label, y = y_label),
                  "ind" = fviz_pca_ind(values$pcaResult, col.ind = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE, addEllipses = !is.null(input$pcaQualiSup), title = plot_title) + labs(x = x_label, y = y_label),
                  "biplot" = fviz_pca_biplot(values$pcaResult, repel = TRUE, col.var = "#FC4E07", col.ind = "#00AFBB", label = "all", addEllipses = !is.null(input$pcaQualiSup), title = plot_title) + labs(x = x_label, y = y_label))
      
      # Appliquer le centrage si demandé
      if (!is.null(input$pcaCenterAxes) && input$pcaCenterAxes) {
        if (input$pcaPlotType == "var") {
          coords <- values$pcaResult$var$coord[, 1:2]
        } else if (input$pcaPlotType == "ind") {
          coords <- values$pcaResult$ind$coord[, 1:2]
        } else {
          coords_ind <- values$pcaResult$ind$coord[, 1:2]
          coords_var <- values$pcaResult$var$coord[, 1:2]
          coords <- rbind(coords_ind, coords_var)
        }
        max_range <- max(abs(range(coords, na.rm = TRUE)))
        p <- p + xlim(-max_range, max_range) + ylim(-max_range, max_range)
      }
      
      ggsave(file, plot = p, device = "png", width = 10, height = 8)
    }
  )
  
  output$downloadPCA <- downloadHandler(
    filename = function() {
      paste0("pca_results_", Sys.Date(), ".txt")
    },
    content = function(file) {
      sink(file)
      print(values$pcaResult)
      sink()
    }
  )
  
  # HCPC - Classification Hiérarchique sur Composantes Principales
  observeEvent(input$runHCPC, {
    req(values$pcaResult)
    tryCatch({
      # HCPC avec les paramètres demandés
      res.hcpc <- HCPC(values$pcaResult,
                       nb.clust = input$hcpcClusters,
                       graph = FALSE)
      
      values$hcpcResult <- res.hcpc
      
      # Dendrogramme avec fviz_dend (pas de centrage)
      output$hcpcDendPlot <- renderPlotly({
        dend_title <- if (!is.null(input$hcpcDendTitle) && input$hcpcDendTitle != "") {
          input$hcpcDendTitle
        } else {
          "Dendrogramme HCPC"
        }
        
        p_dend <- fviz_dend(res.hcpc,
                            cex = 0.7,
                            palette = "jco",
                            rect = TRUE,
                            rect_fill = TRUE,
                            rect_border = "jco",
                            main = dend_title,
                            sub = paste("Nombre de clusters:", input$hcpcClusters))
        
        ggplotly(p_dend) %>%
          layout(margin = list(b = 100))
      })
      
      # Carte des clusters avec fviz_cluster
      output$hcpcClusterPlot <- renderPlotly({
        cluster_title <- if (!is.null(input$hcpcClusterTitle) && input$hcpcClusterTitle != "") {
          input$hcpcClusterTitle
        } else {
          "Carte des clusters HCPC"
        }
        
        # Labels des axes
        eigenvals <- get_eigenvalue(values$pcaResult)
        pc1_var <- round(eigenvals[1, "variance.percent"], 1)
        pc2_var <- round(eigenvals[2, "variance.percent"], 1)
        
        x_label <- if (!is.null(input$hcpcClusterXLabel) && input$hcpcClusterXLabel != "") {
          input$hcpcClusterXLabel
        } else {
          paste0("PC1 (", pc1_var, "%)")
        }
        
        y_label <- if (!is.null(input$hcpcClusterYLabel) && input$hcpcClusterYLabel != "") {
          input$hcpcClusterYLabel
        } else {
          paste0("PC2 (", pc2_var, "%)")
        }
        
        p_cluster <- fviz_cluster(res.hcpc,
                                  repel = TRUE,
                                  show.clust.cent = TRUE,
                                  palette = "jco",
                                  ggtheme = theme_minimal(),
                                  main = cluster_title) +
          labs(x = x_label, y = y_label)
        
        # Centrage sur (0,0) si demandé
        if (!is.null(input$hcpcCenterAxes) && input$hcpcCenterAxes) {
          coords <- values$pcaResult$ind$coord[, 1:2]
          max_range <- max(abs(range(coords, na.rm = TRUE)))
          p_cluster <- p_cluster + xlim(-max_range, max_range) + ylim(-max_range, max_range)
        }
        
        ggplotly(p_cluster) %>%
          layout(showlegend = TRUE)
      })
      
      # Résultats détaillés
      output$hcpcSummary <- renderPrint({
        cat("=== CLASSIFICATION HIÉRARCHIQUE SUR COMPOSANTES PRINCIPALES (HCPC) ===\n\n")
        
        cat("Nombre de clusters:", input$hcpcClusters, "\n\n")
        
        # Affectation des individus aux clusters
        cluster_results <- data.frame(
          Individual = rownames(res.hcpc$data.clust),
          Cluster = res.hcpc$data.clust$clust
        )
        cat("=== AFFECTATION DES INDIVIDUS AUX CLUSTERS ===\n")
        print(cluster_results)
        cat("\n")
        
        # Variables discriminantes par cluster
        cat("=== VARIABLES LES PLUS DISCRIMINANTES PAR CLUSTER ===\n")
        if (!is.null(res.hcpc$desc.var$quanti)) {
          for (i in 1:length(res.hcpc$desc.var$quanti)) {
            if (!is.null(res.hcpc$desc.var$quanti[[i]])) {
              cat("\n--- CLUSTER", i, "---\n")
              cluster_vars <- res.hcpc$desc.var$quanti[[i]]
              print(round(cluster_vars, 4))
            }
          }
        }
        
        # Description des axes par cluster
        if (!is.null(res.hcpc$desc.axes$quanti)) {
          cat("\n=== DESCRIPTION DES AXES PAR CLUSTER ===\n")
          for (i in 1:length(res.hcpc$desc.axes$quanti)) {
            if (!is.null(res.hcpc$desc.axes$quanti[[i]])) {
              cat("\n--- CLUSTER", i, " - AXES ---\n")
              axes_desc <- res.hcpc$desc.axes$quanti[[i]]
              print(round(axes_desc, 4))
            }
          }
        }
        
        # Parangons (individus les plus représentatifs)
        if (!is.null(res.hcpc$desc.ind$para)) {
          cat("\n=== INDIVIDUS LES PLUS REPRÉSENTATIFS (PARANGONS) ===\n")
          for (i in 1:length(res.hcpc$desc.ind$para)) {
            if (!is.null(res.hcpc$desc.ind$para[[i]])) {
              cat("\n--- CLUSTER", i, " - PARANGONS ---\n")
              print(res.hcpc$desc.ind$para[[i]])
            }
          }
        }
        
        # Individus les plus éloignés du centre du cluster
        if (!is.null(res.hcpc$desc.ind$dist)) {
          cat("\n=== INDIVIDUS LES PLUS ÉLOIGNÉS DU CENTRE ===\n")
          for (i in 1:length(res.hcpc$desc.ind$dist)) {
            if (!is.null(res.hcpc$desc.ind$dist[[i]])) {
              cat("\n--- CLUSTER", i, " - INDIVIDUS ÉLOIGNÉS ---\n")
              print(res.hcpc$desc.ind$dist[[i]])
            }
          }
        }
      })
    }, error = function(e) {
      showNotification(paste("Erreur HCPC :", e$message), type = "error")
    })
  })
  
  # Downloads for HCPC plots
  output$downloadHcpcClusterPlot <- downloadHandler(
    filename = function() { paste("hcpc_cluster_plot", Sys.Date(), ".png", sep = "") },
    content = function(file) {
      cluster_title <- if (!is.null(input$hcpcClusterTitle) && input$hcpcClusterTitle != "") {
        input$hcpcClusterTitle
      } else {
        "Carte des clusters HCPC"
      }
      
      eigenvals <- get_eigenvalue(values$pcaResult)
      pc1_var <- round(eigenvals[1, "variance.percent"], 1)
      pc2_var <- round(eigenvals[2, "variance.percent"], 1)
      
      x_label <- if (!is.null(input$hcpcClusterXLabel) && input$hcpcClusterXLabel != "") {
        input$hcpcClusterXLabel
      } else {
        paste0("PC1 (", pc1_var, "%)")
      }
      
      y_label <- if (!is.null(input$hcpcClusterYLabel) && input$hcpcClusterYLabel != "") {
        input$hcpcClusterYLabel
      } else {
        paste0("PC2 (", pc2_var, "%)")
      }
      
      p <- fviz_cluster(values$hcpcResult, repel = TRUE, show.clust.cent = TRUE, palette = "jco", ggtheme = theme_minimal(), main = cluster_title) +
        labs(x = x_label, y = y_label)
      
      if (!is.null(input$hcpcCenterAxes) && input$hcpcCenterAxes) {
        coords <- values$pcaResult$ind$coord[, 1:2]
        max_range <- max(abs(range(coords, na.rm = TRUE)))
        p <- p + xlim(-max_range, max_range) + ylim(-max_range, max_range)
      }
      
      ggsave(file, plot = p, device = "png", width = 10, height = 8)
    }
  )
  
  output$downloadHcpcDendPlot <- downloadHandler(
    filename = function() { paste("hcpc_dend_plot", Sys.Date(), ".png", sep = "") },
    content = function(file) {
      dend_title <- if (!is.null(input$hcpcDendTitle) && input$hcpcDendTitle != "") {
        input$hcpcDendTitle
      } else {
        "Dendrogramme HCPC"
      }
      
      p <- fviz_dend(values$hcpcResult, cex = 0.7, palette = "jco", rect = TRUE, rect_fill = TRUE, rect_border = "jco", main = dend_title)
      ggsave(file, plot = p, device = "png", width = 10, height = 8)
    }
  )
  
  # AFD - Sélection du groupe pour les moyennes
  output$afdMeansGroupSelect <- renderUI({
    req(values$filteredData)
    fac_cols <- names(values$filteredData)[sapply(values$filteredData, is.factor)]
    # Exclure le facteur discriminant des choix
    if (!is.null(input$afdFactor)) {
      fac_cols <- fac_cols[fac_cols != input$afdFactor]
    }
    if (length(fac_cols) == 0) return(NULL)
    
    selectInput("afdMeansGroup", "Variable de groupement pour les moyennes:", 
                choices = fac_cols)
  })
  
  output$afdFactorSelect <- renderUI({
    req(values$filteredData)
    fac_cols <- names(values$filteredData)[sapply(values$filteredData, is.factor)]
    selectInput("afdFactor", "Facteur discriminant:", choices = fac_cols)
  })
  
  output$afdVarSelect <- renderUI({
    req(values$filteredData)
    num_cols <- names(values$filteredData)[sapply(values$filteredData, is.numeric)]
    pickerInput(
      inputId = "afdVars",
      label = "Variables pour l'AFD:",
      choices = num_cols,
      multiple = TRUE,
      selected = num_cols,
      options = list(`actions-box` = TRUE)
    )
  })
  
  observeEvent(input$runAFD, {
    req(values$filteredData, input$afdFactor, input$afdVars)
    tryCatch({
      if (input$afdUseMeans && !is.null(input$afdMeansGroup)) {
        # Utiliser les moyennes par groupe
        temp_data <- values$filteredData[, c(input$afdFactor, input$afdVars, input$afdMeansGroup)]
        temp_data <- na.omit(temp_data)
        
        # Calculer les moyennes par groupe de moyennes, mais garder le facteur discriminant
        afd_data <- temp_data %>%
          group_by(!!sym(input$afdMeansGroup), !!sym(input$afdFactor)) %>%
          summarise(across(all_of(input$afdVars), mean, na.rm = TRUE), .groups = 'drop') %>%
          select(-!!sym(input$afdMeansGroup))
        
      } else {
        # Utiliser les données originales
        afd_data <- values$filteredData[, c(input$afdFactor, input$afdVars)]
        afd_data <- na.omit(afd_data)
      }
      
      # Vérification du nombre de groupes
      if (nlevels(afd_data[[input$afdFactor]]) < 2) {
        showNotification("L'AFD nécessite au moins 2 groupes", type = "error")
        return()
      }
      
      # AFD
      afd_formula <- as.formula(paste(input$afdFactor, "~ ."))
      afd_result <- lda(afd_formula, data = afd_data)
      values$afdResult <- afd_result
      
      # Prédictions
      afd_predict <- predict(afd_result, afd_data)
      
      # Calcul des pourcentages de variance
      eigenvals <- afd_result$svd^2
      prop_var <- eigenvals / sum(eigenvals) * 100
      
      # Préparation des données pour la visualisation
      afd_plot_data <- data.frame(
        LD1 = afd_predict$x[, 1],
        Group = afd_data[[input$afdFactor]],
        Individual = rownames(afd_data)
      )
      
      if (ncol(afd_predict$x) > 1) {
        afd_plot_data$LD2 = afd_predict$x[, 2]
      } else {
        afd_plot_data$LD2 = 0
      }
      
      # Graphique des individus
      output$afdIndPlot <- renderPlotly({
        # Titre et labels personnalisés
        ind_title <- if (!is.null(input$afdIndTitle) && input$afdIndTitle != "") {
          input$afdIndTitle
        } else {
          "AFD - Projection des individus"
        }
        
        x_label <- if (!is.null(input$afdIndXLabel) && input$afdIndXLabel != "") {
          input$afdIndXLabel
        } else {
          if (length(prop_var) >= 1) paste0("LD1 (", round(prop_var[1], 2), "%)") else "LD1"
        }
        
        y_label <- if (!is.null(input$afdIndYLabel) && input$afdIndYLabel != "") {
          input$afdIndYLabel
        } else {
          if (length(prop_var) >= 2) paste0("LD2 (", round(prop_var[2], 2), "%)") else "LD2"
        }
        
        p_ind <- ggplot(afd_plot_data, aes(x = LD1, y = LD2, color = Group)) +
          geom_point(size = 4, alpha = 0.7) +
          stat_ellipse(type = "norm", level = 0.68, size = 1) +
          geom_text_repel(aes(label = Individual),
                          size = 3.5, box.padding = 0.1, point.padding = 0.03,
                          segment.color = "grey50", max.overlaps = Inf, force = 3,
                          fontface = "bold") +
          geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
          geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
          labs(title = ind_title, x = x_label, y = y_label, color = input$afdFactor) +
          scale_color_brewer(type = "qual", palette = "Set1") +
          theme_minimal() +
          theme(legend.position = "bottom", panel.grid = element_blank(),
                plot.title = element_text(hjust = 0.5))
        
        # Centrage sur (0,0) si demandé
        if (!is.null(input$afdIndCenterAxes) && input$afdIndCenterAxes) {
          coords <- afd_plot_data[, c("LD1", "LD2")]
          max_range <- max(abs(range(coords, na.rm = TRUE)))
          p_ind <- p_ind + xlim(-max_range, max_range) + ylim(-max_range, max_range)
        }
        
        ggplotly(p_ind, tooltip = "text") %>%
          layout(showlegend = TRUE)
      })
      
      # Graphique des variables (loadings)
      if (ncol(afd_predict$x) >= 1) {
        loadings <- afd_result$scaling
        if (ncol(loadings) == 1) {
          var_contrib <- data.frame(
            Variable = rownames(loadings),
            LD1 = loadings[, 1],
            LD2 = 0
          )
        } else {
          var_contrib <- data.frame(
            Variable = rownames(loadings),
            LD1 = loadings[, 1],
            LD2 = loadings[, 2]
          )
        }
        
        output$afdVarPlot <- renderPlotly({
          # Titre et labels personnalisés
          var_title <- if (!is.null(input$afdVarTitle) && input$afdVarTitle != "") {
            input$afdVarTitle
          } else {
            "AFD - Contribution des variables"
          }
          
          x_label <- if (!is.null(input$afdVarXLabel) && input$afdVarXLabel != "") {
            input$afdVarXLabel
          } else {
            if (length(prop_var) >= 1) paste0("LD1 (", round(prop_var[1], 2), "%)") else "LD1"
          }
          
          y_label <- if (!is.null(input$afdVarYLabel) && input$afdVarYLabel != "") {
            input$afdVarYLabel
          } else {
            if (length(prop_var) >= 2) paste0("LD2 (", round(prop_var[2], 2), "%)") else "LD2"
          }
          
          p_var <- ggplot(var_contrib, aes(x = LD1, y = LD2)) +
            geom_segment(aes(x = 0, y = 0, xend = LD1, yend = LD2),
                         arrow = arrow(length = unit(0.3, "cm")),
                         color = "blue", size = 1) +
            geom_text_repel(aes(label = Variable), size = 3.5, max.overlaps = Inf,
                            color = "blue", fontface = "bold",
                            box.padding = 0.3, segment.color = "blue", force = 2) +
            geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
            geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
            labs(title = var_title, x = x_label, y = y_label) +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5), panel.grid = element_blank())
          
          # Centrage sur (0,0) si demandé
          if (!is.null(input$afdVarCenterAxes) && input$afdVarCenterAxes) {
            coords <- var_contrib[, c("LD1", "LD2")]
            max_range <- max(abs(range(coords, na.rm = TRUE)))
            p_var <- p_var + xlim(-max_range, max_range) + ylim(-max_range, max_range)
          }
          
          ggplotly(p_var, tooltip = "text")
        })
      }
      
      # Métriques complètes de l'AFD
      output$afdSummary <- renderPrint({
        cat("ANALYSE FACTORIELLE DISCRIMINANTE (AFD)\n")
        cat("=====================================\n\n")
        
        # Informations générales
        cat("Facteur discriminant:", input$afdFactor, "\n")
        cat("Variables utilisées:", paste(input$afdVars, collapse = ", "), "\n")
        cat("Nombre d'observations:", nrow(afd_data), "\n")
        cat("Nombre de groupes:", nlevels(afd_data[[input$afdFactor]]), "\n")
        if (input$afdUseMeans && !is.null(input$afdMeansGroup)) {
          cat("Analyse basée sur les moyennes par groupe:", input$afdMeansGroup, "\n")
        }
        cat("\n")
        
        # 1. Tests globaux
        cat("1. TESTS GLOBAUX\n")
        cat("================\n")
        
        # Test de Wilks
        eigenvals <- afd_result$svd^2
        wilks_lambda <- prod(1 / (1 + eigenvals))
        cat("Lambda de Wilks:", round(wilks_lambda, 4), "\n")
        
        # Valeurs propres et variance expliquée
        cat("\nValeurs propres:\n")
        print(round(eigenvals, 4))
        
        prop_var <- eigenvals / sum(eigenvals) * 100
        cat("\nVariance expliquée (%):\n")
        for (i in seq_along(prop_var)) {
          cat("LD", i, ": ", round(prop_var[i], 2), "%\n", sep = "")
        }
        cat("Variance cumulée:", round(cumsum(prop_var), 2), "%\n")
        
        # Corrélation canonique
        can_cor <- sqrt(eigenvals / (1 + eigenvals))
        cat("\nCorrélations canoniques:\n")
        print(round(can_cor, 4))
        
        # Coefficients standardisés (loadings)
        cat("\n2. POIDS DES VARIABLES\n")
        cat("======================\n")
        
        cat("Coefficients des fonctions discriminantes:\n")
        print(round(afd_result$scaling, 4))
        
        # Structure matrix (corrélations variables-fonctions)
        X_std <- scale(afd_data[, input$afdVars])
        scores <- as.matrix(X_std) %*% afd_result$scaling
        structure_matrix <- cor(X_std, scores)
        cat("\nMatrice de structure (corrélations):\n")
        print(round(structure_matrix, 4))
        
        # F-tests pour chaque variable
        cat("\nTests F univariés:\n")
        f_tests <- data.frame(Variable = input$afdVars)
        for (var in input$afdVars) {
          aov_result <- aov(as.formula(paste(var, "~", input$afdFactor)), data = afd_data)
          f_stat <- summary(aov_result)[[1]][1, "F value"]
          p_val <- summary(aov_result)[[1]][1, "Pr(>F)"]
          f_tests[f_tests$Variable == var, "F_statistic"] <- round(f_stat, 4)
          f_tests[f_tests$Variable == var, "p_value"] <- round(p_val, 4)
        }
        print(f_tests)
        
        # Matrice de confusion
        cat("\n3. QUALITÉ DE CLASSIFICATION\n")
        cat("=============================\n")
        
        confusion_matrix <- table(Réel = afd_data[[input$afdFactor]],
                                  Prédit = afd_predict$class)
        cat("Matrice de confusion:\n")
        print(confusion_matrix)
        
        # Taux de bonne attribution
        accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
        cat("\nTaux de classification correcte:", round(accuracy * 100, 2), "%\n")
        
        # Taux par groupe
        cat("\nTaux de classification par groupe:\n")
        for (i in 1:nrow(confusion_matrix)) {
          group_acc <- confusion_matrix[i,i] / sum(confusion_matrix[i,])
          cat(rownames(confusion_matrix)[i], ":", round(group_acc * 100, 2), "%\n")
        }
        
        # Validation croisée (Leave-One-Out)
        cat("\n4. VALIDATION CROISÉE (Leave-One-Out)\n")
        cat("=====================================\n")
        
        cv_predictions <- numeric(nrow(afd_data))
        for (i in 1:nrow(afd_data)) {
          # Données d'entraînement (sans l'observation i)
          train_data <- afd_data[-i, ]
          test_data <- afd_data[i, , drop = FALSE]
          
          # Modèle sur données d'entraînement
          cv_model <- lda(afd_formula, data = train_data)
          cv_pred <- predict(cv_model, test_data)
          cv_predictions[i] <- as.character(cv_pred$class)
        }
        
        cv_predictions <- factor(cv_predictions, levels = levels(afd_data[[input$afdFactor]]))
        cv_confusion <- table(Réel = afd_data[[input$afdFactor]],
                              Prédit = cv_predictions)
        
        cat("Matrice de confusion (validation croisée):\n")
        print(cv_confusion)
        
        cv_accuracy <- sum(diag(cv_confusion)) / sum(cv_confusion)
        cat("\nTaux de classification en validation croisée:",
            round(cv_accuracy * 100, 2), "%\n")
        
        cat("\n5. CENTROÏDES DES GROUPES\n")
        cat("=========================\n")
        print(round(afd_result$means, 4))
        
        cat("\n6. PROBABILITÉS A PRIORI\n")
        cat("========================\n")
        print(round(afd_result$prior, 4))
      })
    }, error = function(e) {
      showNotification(paste("Erreur AFD :", e$message), type = "error")
    })
  })
  
  # Downloads for AFD plots
  output$downloadAfdIndPlot <- downloadHandler(
    filename = function() { paste("afd_ind_plot", Sys.Date(), ".png", sep = "") },
    content = function(file) {
      afd_predict <- predict(values$afdResult, values$filteredData[, input$afdVars])
      eigenvals <- values$afdResult$svd^2
      prop_var <- eigenvals / sum(eigenvals) * 100
      afd_plot_data <- data.frame(LD1 = afd_predict$x[,1], LD2 = if(ncol(afd_predict$x)>1) afd_predict$x[,2] else 0,
                                  Group = values$filteredData[[input$afdFactor]], Individual = rownames(values$filteredData))
      
      ind_title <- if (!is.null(input$afdIndTitle) && input$afdIndTitle != "") {
        input$afdIndTitle
      } else {
        "AFD - Projection des individus"
      }
      
      x_label <- if (!is.null(input$afdIndXLabel) && input$afdIndXLabel != "") {
        input$afdIndXLabel
      } else {
        if (length(prop_var) >= 1) paste0("LD1 (", round(prop_var[1], 2), "%)") else "LD1"
      }
      
      y_label <- if (!is.null(input$afdIndYLabel) && input$afdIndYLabel != "") {
        input$afdIndYLabel
      } else {
        if (length(prop_var) >= 2) paste0("LD2 (", round(prop_var[2], 2), "%)") else "LD2"
      }
      
      p_ind <- ggplot(afd_plot_data, aes(x = LD1, y = LD2, color = Group)) +
        geom_point() +
        stat_ellipse() +
        geom_text_repel(aes(label = Individual)) +
        geom_hline(yintercept = 0) +
        geom_vline(xintercept = 0) +
        labs(title = ind_title, x = x_label, y = y_label) +
        theme_minimal()
      
      if (!is.null(input$afdIndCenterAxes) && input$afdIndCenterAxes) {
        coords <- afd_plot_data[, c("LD1", "LD2")]
        max_range <- max(abs(range(coords, na.rm = TRUE)))
        p_ind <- p_ind + xlim(-max_range, max_range) + ylim(-max_range, max_range)
      }
      
      ggsave(file, plot = p_ind, device = "png", width = 10, height = 8)
    }
  )
  
  output$downloadAfdVarPlot <- downloadHandler(
    filename = function() { paste("afd_var_plot", Sys.Date(), ".png", sep = "") },
    content = function(file) {
      loadings <- values$afdResult$scaling
      var_contrib <- data.frame(Variable = rownames(loadings), LD1 = loadings[,1], LD2 = if(ncol(loadings)>1) loadings[,2] else 0)
      
      eigenvals <- values$afdResult$svd^2
      prop_var <- eigenvals / sum(eigenvals) * 100
      
      var_title <- if (!is.null(input$afdVarTitle) && input$afdVarTitle != "") {
        input$afdVarTitle
      } else {
        "AFD - Contribution des variables"
      }
      
      x_label <- if (!is.null(input$afdVarXLabel) && input$afdVarXLabel != "") {
        input$afdVarXLabel
      } else {
        if (length(prop_var) >= 1) paste0("LD1 (", round(prop_var[1], 2), "%)") else "LD1"
      }
      
      y_label <- if (!is.null(input$afdVarYLabel) && input$afdVarYLabel != "") {
        input$afdVarYLabel
      } else {
        if (length(prop_var) >= 2) paste0("LD2 (", round(prop_var[2], 2), "%)") else "LD2"
      }
      
      p_var <- ggplot(var_contrib, aes(x = LD1, y = LD2)) +
        geom_segment(aes(x = 0, y = 0, xend = LD1, yend = LD2), arrow = arrow()) +
        geom_text_repel(aes(label = Variable)) +
        geom_hline(yintercept = 0) +
        geom_vline(xintercept = 0) +
        labs(title = var_title, x = x_label, y = y_label) +
        theme_minimal()
      
      if (!is.null(input$afdVarCenterAxes) && input$afdVarCenterAxes) {
        coords <- var_contrib[, c("LD1", "LD2")]
        max_range <- max(abs(range(coords, na.rm = TRUE)))
        p_var <- p_var + xlim(-max_range, max_range) + ylim(-max_range, max_range)
      }
      
      ggsave(file, plot = p_var, device = "png", width = 10, height = 8)
    }
  )
  
  
  # ---- Tests statistiques ----
  output$responseVarSelect <- renderUI({
    req(values$filteredData)
    num_cols <- names(values$filteredData)[sapply(values$filteredData, is.numeric)]
    tagList(
      pickerInput("responseVar", "Variable(s) réponse:", 
                  choices = num_cols, 
                  multiple = TRUE,
                  options = list(`actions-box` = TRUE)),
      actionButton("selectAllResponse", "Tout sélectionner", class = "btn-success btn-sm"),
      actionButton("deselectAllResponse", "Tout désélectionner", class = "btn-danger btn-sm")
    )
  })
  
  observeEvent(input$selectAllResponse, {
    num_cols <- names(values$filteredData)[sapply(values$filteredData, is.numeric)]
    updatePickerInput(session, "responseVar", selected = num_cols)
  })
  
  observeEvent(input$deselectAllResponse, {
    updatePickerInput(session, "responseVar", selected = character(0))
  })
  
  output$factorVarSelect <- renderUI({
    req(values$filteredData)
    fac_cols <- names(values$filteredData)[sapply(values$filteredData, is.factor)]
    tagList(
      pickerInput("factorVar", "Facteur(s):", 
                  choices = fac_cols, 
                  multiple = TRUE,
                  options = list(`actions-box` = TRUE)),
      actionButton("selectAllFactors", "Tout sélectionner", class = "btn-success btn-sm"),
      actionButton("deselectAllFactors", "Tout désélectionner", class = "btn-danger btn-sm")
    )
  })
  
  observeEvent(input$selectAllFactors, {
    fac_cols <- names(values$filteredData)[sapply(values$filteredData, is.factor)]
    updatePickerInput(session, "factorVar", selected = fac_cols)
  })
  
  observeEvent(input$deselectAllFactors, {
    updatePickerInput(session, "factorVar", selected = character(0))
  })
  
  # ---- Tests de normalité et homogénéité sur données brutes ----
  observeEvent(input$testNormalityRaw, {
    req(input$responseVar)
    
    results_list <- list()
    
    for (var in input$responseVar) {
      tryCatch({
        data_values <- values$filteredData[[var]]
        data_values <- data_values[!is.na(data_values)]
        
        if (length(data_values) >= 3 && length(data_values) <= 5000) {
          norm_test <- shapiro.test(data_values)
          results_list[[var]] <- data.frame(
            Test = "Normalité (données brutes)",
            Variable = var,
            Facteur = "Global",
            Statistique = round(norm_test$statistic, 4),
            ddl = NA,
            p_value = norm_test$p.value,
            Interpretation = interpret_test_results("shapiro", norm_test$p.value),
            stringsAsFactors = FALSE
          )
        } else {
          results_list[[var]] <- data.frame(
            Test = "Normalité (données brutes)",
            Variable = var,
            Facteur = "Global",
            Statistique = NA,
            ddl = NA,
            p_value = NA,
            Interpretation = "Échantillon trop petit/grand pour Shapiro-Wilk",
            stringsAsFactors = FALSE
          )
        }
      }, error = function(e) {
        results_list[[var]] <- data.frame(
          Test = "Normalité (données brutes)",
          Variable = var,
          Facteur = "Global",
          Statistique = NA,
          ddl = NA,
          p_value = NA,
          Interpretation = paste("Erreur:", e$message),
          stringsAsFactors = FALSE
        )
      })
    }
    
    values$testResultsDF <- do.call(rbind, results_list)
    values$normalityResults <- NULL
    values$homogeneityResults <- NULL
    values$currentTestType <- "non-parametric"
  })
  
  observeEvent(input$testHomogeneityRaw, {
    req(input$responseVar, input$factorVar)
    
    if (length(input$factorVar) != 1) {
      showNotification("Le test d'homogénéité nécessite exactement un facteur", type = "warning")
      return()
    }
    
    results_list <- list()
    
    for (var in input$responseVar) {
      tryCatch({
        fvar <- input$factorVar[1]
        formula_str <- as.formula(paste(var, "~", fvar))
        levene_test <- car::leveneTest(formula_str, data = values$filteredData)
        
        results_list[[var]] <- data.frame(
          Test = "Homogénéité (données brutes)",
          Variable = var,
          Facteur = fvar,
          Statistique = round(levene_test$`F value`[1], 4),
          ddl = paste(levene_test$Df[1], ",", levene_test$Df[2]),
          p_value = levene_test$`Pr(>F)`[1],
          Interpretation = interpret_test_results("levene", levene_test$`Pr(>F)`[1]),
          stringsAsFactors = FALSE
        )
      }, error = function(e) {
        results_list[[var]] <- data.frame(
          Test = "Homogénéité (données brutes)",
          Variable = var,
          Facteur = input$factorVar[1],
          Statistique = NA,
          ddl = NA,
          p_value = NA,
          Interpretation = paste("Erreur:", e$message),
          stringsAsFactors = FALSE
        )
      })
    }
    
    values$testResultsDF <- do.call(rbind, results_list)
    values$normalityResults <- NULL
    values$homogeneityResults <- NULL
    values$currentTestType <- "non-parametric"
  })
  
  # Test t-student
  observeEvent(input$testT, {
    req(input$responseVar, input$factorVar)
    if (length(input$factorVar) > 1) {
      showNotification("Le test t nécessite un seul facteur", type = "warning")
      return()
    }
    
    results_list <- list()
    normality_results <- list()
    homogeneity_results <- list()
    model_list <- list()
    
    for (var in input$responseVar) {
      tryCatch({
        fvar <- input$factorVar[1]
        factor_levels <- levels(as.factor(values$filteredData[[fvar]]))
        
        if (length(factor_levels) != 2) {
          next
        }
        
        # Tests de validation
        group1_data <- values$filteredData[values$filteredData[[fvar]] == factor_levels[1], var]
        group2_data <- values$filteredData[values$filteredData[[fvar]] == factor_levels[2], var]
        
        group1_data <- group1_data[!is.na(group1_data)]
        group2_data <- group2_data[!is.na(group2_data)]
        
        # Test de normalité
        normality_group1 <- if(length(group1_data) >= 3 && length(group1_data) <= 5000) {
          shapiro.test(group1_data)
        } else {
          list(p.value = NA)
        }
        
        normality_group2 <- if(length(group2_data) >= 3 && length(group2_data) <= 5000) {
          shapiro.test(group2_data)
        } else {
          list(p.value = NA)
        }
        
        # Test d'homogénéité
        test_data <- data.frame(
          values = c(group1_data, group2_data),
          group = factor(c(rep(factor_levels[1], length(group1_data)), 
                           rep(factor_levels[2], length(group2_data))))
        )
        
        homogeneity_test <- car::leveneTest(values ~ group, data = test_data)
        
        # Stocker les résultats de validation
        normality_results[[var]] <- list(
          group1 = normality_group1,
          group2 = normality_group2,
          group1_name = factor_levels[1],
          group2_name = factor_levels[2]
        )
        
        homogeneity_results[[var]] <- homogeneity_test
        
        # Exécuter le t-test et créer un modèle factice pour les diagnostics
        formula_str <- as.formula(paste(var, "~", fvar))
        test_result <- t.test(formula_str, data = values$filteredData)
        
        # Créer un modèle lm pour les diagnostics
        lm_model <- lm(formula_str, data = values$filteredData)
        model_list[[var]] <- lm_model
        
        # Créer le dataframe de résultats
        results_list[[var]] <- data.frame(
          Test = "t-test",
          Variable = var,
          Facteur = fvar,
          Statistique = round(test_result$statistic, 4),
          ddl = round(test_result$parameter, 2),
          p_value = test_result$p.value,
          Interpretation = interpret_test_results("t.test", test_result$p.value),
          stringsAsFactors = FALSE
        )
        
      }, error = function(e) {
        results_list[[var]] <- data.frame(
          Test = "t-test",
          Variable = var,
          Facteur = fvar,
          Statistique = NA,
          ddl = NA,
          p_value = NA,
          Interpretation = paste("Erreur:", e$message),
          stringsAsFactors = FALSE
        )
      })
    }
    
    values$testResultsDF <- do.call(rbind, results_list)
    values$normalityResults <- normality_results
    values$homogeneityResults <- homogeneity_results
    values$currentValidationVar <- 1
    values$modelList <- model_list
    values$currentModelVar <- 1
    values$currentTestType <- "parametric"
  })
  
  # Test de Wilcoxon
  observeEvent(input$testWilcox, {
    req(input$responseVar, input$factorVar)
    if (length(input$factorVar) > 1) {
      showNotification("Le test de Wilcoxon nécessite un seul facteur", type = "warning")
      return()
    }
    
    results_list <- list()
    
    for (var in input$responseVar) {
      tryCatch({
        fvar <- input$factorVar[1]
        formula_str <- as.formula(paste(var, "~", fvar))
        test_result <- wilcox.test(formula_str, data = values$filteredData, exact = FALSE)
        
        results_list[[var]] <- data.frame(
          Test = "Wilcoxon",
          Variable = var,
          Facteur = fvar,
          Statistique = round(test_result$statistic, 4),
          ddl = NA,
          p_value = test_result$p.value,
          Interpretation = interpret_test_results("wilcox.test", test_result$p.value),
          stringsAsFactors = FALSE
        )
      }, error = function(e) {
        results_list[[var]] <- data.frame(
          Test = "Wilcoxon",
          Variable = var,
          Facteur = fvar,
          Statistique = NA,
          ddl = NA,
          p_value = NA,
          Interpretation = paste("Erreur:", e$message),
          stringsAsFactors = FALSE
        )
      })
    }
    
    values$testResultsDF <- do.call(rbind, results_list)
    values$normalityResults <- NULL
    values$homogeneityResults <- NULL
    values$currentTestType <- "non-parametric"
  })
  
  # Test de Kruskal-Wallis
  observeEvent(input$testKruskal, {
    req(input$responseVar, input$factorVar)
    if (length(input$factorVar) > 1) {
      showNotification("Kruskal-Wallis nécessite un seul facteur", type = "warning")
      return()
    }
    
    results_list <- list()
    
    for (var in input$responseVar) {
      tryCatch({
        fvar <- input$factorVar[1]
        formula_str <- as.formula(paste(var, "~", fvar))
        test_result <- kruskal.test(formula_str, data = values$filteredData)
        
        results_list[[var]] <- data.frame(
          Test = "Kruskal-Wallis",
          Variable = var,
          Facteur = fvar,
          Statistique = round(test_result$statistic, 4),
          ddl = test_result$parameter,
          p_value = test_result$p.value,
          Interpretation = interpret_test_results("kruskal.test", test_result$p.value),
          stringsAsFactors = FALSE
        )
      }, error = function(e) {
        results_list[[var]] <- data.frame(
          Test = "Kruskal-Wallis",
          Variable = var,
          Facteur = fvar,
          Statistique = NA,
          ddl = NA,
          p_value = NA,
          Interpretation = paste("Erreur:", e$message),
          stringsAsFactors = FALSE
        )
      })
    }
    
    values$testResultsDF <- do.call(rbind, results_list)
    values$normalityResults <- NULL
    values$homogeneityResults <- NULL
    values$currentTestType <- "non-parametric"
  })
  
  # Test Scheirer-Ray-Hare
  observeEvent(input$testScheirerRayHare, {
    req(input$responseVar, input$factorVar)
    
    if (length(input$factorVar) < 2) {
      showNotification("Scheirer-Ray-Hare nécessite au moins 2 facteurs", type = "warning")
      return()
    }
    
    results_list <- list()
    
    for (var in input$responseVar) {
      tryCatch({
        # Préparer la formule
        if (input$interaction && length(input$factorVar) == 2) {
          formula_str <- as.formula(paste(var, "~", paste(input$factorVar, collapse = "*")))
        } else {
          formula_str <- as.formula(paste(var, "~", paste(input$factorVar, collapse = "+")))
        }
        
        # Exécuter le test Scheirer-Ray-Hare
        test_result <- rcompanion::scheirerRayHare(formula_str, data = values$filteredData)
        
        # Extraire les résultats pour chaque effet
        for (i in 1:nrow(test_result)) {
          effect_name <- rownames(test_result)[i]
          if (effect_name != "Residuals") {
            results_list[[paste(var, effect_name, sep = "_")]] <- data.frame(
              Test = "Scheirer-Ray-Hare",
              Variable = var,
              Facteur = effect_name,
              Statistique = round(test_result$H[i], 4),
              ddl = test_result$Df[i],
              p_value = test_result$`p.value`[i],
              Interpretation = interpret_test_results("scheirerRayHare", test_result$`p.value`[i]),
              stringsAsFactors = FALSE
            )
          }
        }
        
        # Stocker le résultat complet pour les post-hoc
        values$scheirerResults <- test_result
        
      }, error = function(e) {
        results_list[[var]] <- data.frame(
          Test = "Scheirer-Ray-Hare",
          Variable = var,
          Facteur = paste(input$factorVar, collapse = "+"),
          Statistique = NA,
          ddl = NA,
          p_value = NA,
          Interpretation = paste("Erreur:", e$message),
          stringsAsFactors = FALSE
        )
      })
    }
    
    values$testResultsDF <- do.call(rbind, results_list)
    values$normalityResults <- NULL
    values$homogeneityResults <- NULL
    values$currentTestType <- "non-parametric"
  })
  
  # Test ANOVA
  observeEvent(input$testANOVA, {
    req(input$responseVar, input$factorVar)
    
    results_list <- list()
    normality_results <- list()
    homogeneity_results <- list()
    model_list <- list()
    
    tryCatch({
      df <- values$filteredData
      for (f in input$factorVar) {
        if (!is.factor(df[[f]])) df[[f]] <- factor(df[[f]])
      }
      
      for (var in input$responseVar) {
        formula_str <- paste(var, "~", paste(input$factorVar, collapse = ifelse(input$interaction, "*", "+")))
        model <- aov(as.formula(formula_str), data = df)
        anova_table <- summary(model)[[1]]
        
        # Stocker le modèle
        model_list[[var]] <- model
        
        # Créer le dataframe de résultats pour chaque effet
        for (i in 1:(nrow(anova_table) - 1)) {
          effect_name <- rownames(anova_table)[i]
          results_list[[paste(var, effect_name, sep = "_")]] <- data.frame(
            Test = "ANOVA",
            Variable = var,
            Facteur = effect_name,
            Statistique = round(anova_table$`F value`[i], 4),
            ddl = paste(anova_table$Df[i], ",", anova_table$Df[nrow(anova_table)]),
            p_value = anova_table$`Pr(>F)`[i],
            Interpretation = interpret_test_results("anova", anova_table$`Pr(>F)`[i]),
            stringsAsFactors = FALSE
          )
        }
        
        # Tests de validation des résidus
        residuals_data <- residuals(model)
        if (length(residuals_data) > 3) {
          normality_results[[var]] <- shapiro.test(residuals_data)
        }
        
        fitted_data <- fitted(model)
        fitted_factor <- cut(fitted_data, breaks = 2, labels = c("Bas", "Haut"))
        test_data <- data.frame(residuals = residuals_data, fitted_group = fitted_factor)
        homogeneity_results[[var]] <- car::leveneTest(residuals ~ fitted_group, data = test_data)
      }
      
      values$testResultsDF <- do.call(rbind, results_list)
      values$anovaModel <- model
      values$currentModel <- model
      values$modelList <- model_list
      values$currentModelVar <- 1
      values$normalityResults <- normality_results
      values$homogeneityResults <- homogeneity_results
      values$currentValidationVar <- 1
      values$currentTestType <- "parametric"
      
    }, error = function(e) {
      showNotification(paste("Erreur ANOVA :", e$message), type = "error")
    })
  })
  
  # Test de régression linéaire
  observeEvent(input$testLM, {
    req(input$responseVar, input$factorVar)
    
    results_list <- list()
    model_list <- list()
    
    tryCatch({
      df <- values$filteredData
      for (var in input$responseVar) {
        formula_str <- paste(var, "~", paste(input$factorVar, collapse = "+"))
        model <- lm(as.formula(formula_str), data = df)
        summary_model <- summary(model)
        
        # Stocker le modèle
        model_list[[var]] <- model
        
        # Résultat global du modèle
        results_list[[paste(var, "global", sep = "_")]] <- data.frame(
          Test = "Régression linéaire",
          Variable = var,
          Facteur = "Modèle global",
          Statistique = round(summary_model$fstatistic[1], 4),
          ddl = paste(summary_model$fstatistic[2], ",", summary_model$fstatistic[3]),
          p_value = pf(summary_model$fstatistic[1], summary_model$fstatistic[2], 
                       summary_model$fstatistic[3], lower.tail = FALSE),
          Interpretation = paste("R² =", round(summary_model$r.squared, 4)),
          stringsAsFactors = FALSE
        )
        
        # Coefficients
        coef_table <- summary_model$coefficients
        for (i in 2:nrow(coef_table)) {
          results_list[[paste(var, rownames(coef_table)[i], sep = "_")]] <- data.frame(
            Test = "Régression linéaire",
            Variable = var,
            Facteur = rownames(coef_table)[i],
            Statistique = round(coef_table[i, "t value"], 4),
            ddl = summary_model$df[2],
            p_value = coef_table[i, "Pr(>|t|)"],
            Interpretation = interpret_test_results("lm", coef_table[i, "Pr(>|t|)"]),
            stringsAsFactors = FALSE
          )
        }
      }
      
      values$testResultsDF <- do.call(rbind, results_list)
      values$currentModel <- model
      values$modelList <- model_list
      values$currentModelVar <- 1
      values$currentTestType <- "parametric"
      
    }, error = function(e) {
      showNotification(paste("Erreur régression :", e$message), type = "error")
    })
  })
  
  # Test GLM
  observeEvent(input$testGLM, {
    req(input$responseVar, input$factorVar)
    
    results_list <- list()
    model_list <- list()
    
    tryCatch({
      df <- values$filteredData
      for (var in input$responseVar) {
        formula_str <- paste(var, "~", paste(input$factorVar, collapse = "+"))
        model <- glm(as.formula(formula_str), data = df, family = gaussian())
        summary_model <- summary(model)
        
        # Stocker le modèle
        model_list[[var]] <- model
        
        # Coefficients
        coef_table <- summary_model$coefficients
        for (i in 2:nrow(coef_table)) {
          results_list[[paste(var, rownames(coef_table)[i], sep = "_")]] <- data.frame(
            Test = "GLM",
            Variable = var,
            Facteur = rownames(coef_table)[i],
            Statistique = round(coef_table[i, "z value"], 4),
            ddl = NA,
            p_value = coef_table[i, "Pr(>|z|)"],
            Interpretation = interpret_test_results("glm", coef_table[i, "Pr(>|z|)"]),
            stringsAsFactors = FALSE
          )
        }
      }
      
      values$testResultsDF <- do.call(rbind, results_list)
      values$currentModel <- model
      values$modelList <- model_list
      values$currentModelVar <- 1
      values$currentTestType <- "parametric"
      
    }, error = function(e) {
      showNotification(paste("Erreur GLM :", e$message), type = "error")
    })
  })
  
  # Affichage du dataframe des résultats
  output$testResultsDF <- renderDT({
    req(values$testResultsDF)
    datatable(values$testResultsDF, 
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE)
  })
  
  # Contrôle de l'affichage de la validation
  output$showValidation <- reactive({
    !is.null(values$normalityResults) || !is.null(values$homogeneityResults)
  })
  outputOptions(output, "showValidation", suspendWhenHidden = FALSE)
  
  # Contrôle de l'affichage des diagnostics (uniquement pour les tests paramétriques)
  output$showParametricDiagnostics <- reactive({
    !is.null(values$currentTestType) && values$currentTestType == "parametric" && !is.null(values$modelList)
  })
  outputOptions(output, "showParametricDiagnostics", suspendWhenHidden = FALSE)
  
  # Navigation pour la validation
  output$showValidationNavigation <- reactive({
    length(input$responseVar) > 1 && !is.null(values$normalityResults)
  })
  outputOptions(output, "showValidationNavigation", suspendWhenHidden = FALSE)
  
  output$validationNavigation <- renderUI({
    req(input$responseVar, length(input$responseVar) > 1)
    
    current_idx <- if (is.null(values$currentValidationVar)) 1 else values$currentValidationVar
    total_vars <- length(input$responseVar)
    
    div(style = "display: inline-block;",
        actionButton("prevValidationVar", "", icon = icon("chevron-left"), 
                     style = "margin-right: 10px;", class = "btn-sm"),
        span(paste("Variable", current_idx, "sur", total_vars, ":", input$responseVar[current_idx]),
             style = "vertical-align: middle; margin: 0 15px; font-weight: bold;"),
        actionButton("nextValidationVar", "", icon = icon("chevron-right"), 
                     style = "margin-left: 10px;", class = "btn-sm")
    )
  })
  
  # Navigation pour les diagnostics de modèles
  output$showModelNavigation <- reactive({
    !is.null(values$modelList) && length(values$modelList) > 1
  })
  outputOptions(output, "showModelNavigation", suspendWhenHidden = FALSE)
  
  output$modelDiagNavigation <- renderUI({
    req(values$modelList, length(values$modelList) > 1)
    
    current_idx <- if (is.null(values$currentModelVar)) 1 else values$currentModelVar
    total_vars <- length(values$modelList)
    var_names <- names(values$modelList)
    
    div(style = "display: inline-block; margin-bottom: 15px;",
        actionButton("prevModelVar", "", icon = icon("chevron-left"), 
                     style = "margin-right: 10px;", class = "btn-sm"),
        span(paste("Modèle", current_idx, "sur", total_vars, ":", var_names[current_idx]),
             style = "vertical-align: middle; margin: 0 15px; font-weight: bold;"),
        actionButton("nextModelVar", "", icon = icon("chevron-right"), 
                     style = "margin-left: 10px;", class = "btn-sm")
    )
  })
  
  # Navigation pour les résidus
  output$showResidNavigation <- reactive({
    !is.null(values$modelList) && length(values$modelList) > 1
  })
  outputOptions(output, "showResidNavigation", suspendWhenHidden = FALSE)
  
  output$residNavigation <- renderUI({
    req(values$modelList, length(values$modelList) > 1)
    
    current_idx <- if (is.null(values$currentModelVar)) 1 else values$currentModelVar
    total_vars <- length(values$modelList)
    var_names <- names(values$modelList)
    
    div(style = "display: inline-block; margin-bottom: 15px;",
        actionButton("prevResidVar", "", icon = icon("chevron-left"), 
                     style = "margin-right: 10px;", class = "btn-sm"),
        span(paste("Variable", current_idx, "sur", total_vars, ":", var_names[current_idx]),
             style = "vertical-align: middle; margin: 0 15px; font-weight: bold;"),
        actionButton("nextResidVar", "", icon = icon("chevron-right"), 
                     style = "margin-left: 10px;", class = "btn-sm")
    )
  })
  
  # Gestion des événements de navigation
  observeEvent(input$prevValidationVar, {
    current <- if (is.null(values$currentValidationVar)) 1 else values$currentValidationVar
    total <- length(input$responseVar)
    values$currentValidationVar <- if (current > 1) current - 1 else total
  })
  
  observeEvent(input$nextValidationVar, {
    current <- if (is.null(values$currentValidationVar)) 1 else values$currentValidationVar
    total <- length(input$responseVar)
    values$currentValidationVar <- if (current < total) current + 1 else 1
  })
  
  observeEvent(input$prevModelVar, {
    current <- if (is.null(values$currentModelVar)) 1 else values$currentModelVar
    total <- length(values$modelList)
    values$currentModelVar <- if (current > 1) current - 1 else total
    values$currentModel <- values$modelList[[values$currentModelVar]]
  })
  
  observeEvent(input$nextModelVar, {
    current <- if (is.null(values$currentModelVar)) 1 else values$currentModelVar
    total <- length(values$modelList)
    values$currentModelVar <- if (current < total) current + 1 else 1
    values$currentModel <- values$modelList[[values$currentModelVar]]
  })
  
  observeEvent(input$prevResidVar, {
    current <- if (is.null(values$currentModelVar)) 1 else values$currentModelVar
    total <- length(values$modelList)
    values$currentModelVar <- if (current > 1) current - 1 else total
    values$currentModel <- values$modelList[[values$currentModelVar]]
  })
  
  observeEvent(input$nextResidVar, {
    current <- if (is.null(values$currentModelVar)) 1 else values$currentModelVar
    total <- length(values$modelList)
    values$currentModelVar <- if (current < total) current + 1 else 1
    values$currentModel <- values$modelList[[values$currentModelVar]]
  })
  
  # Affichage des résultats de normalité
  output$normalityResults <- renderPrint({
    req(values$normalityResults, input$responseVar)
    current_var <- input$responseVar[values$currentValidationVar]
    norm <- values$normalityResults[[current_var]]
    
    if (is.null(norm)) {
      cat("Aucun résultat de normalité disponible pour cette variable.\n")
    } else if ("group1" %in% names(norm)) {
      cat("Groupe 1 (", norm$group1_name, "): p = ", norm$group1$p.value, "\n")
      cat("Groupe 2 (", norm$group2_name, "): p = ", norm$group2$p.value, "\n")
    } else {
      cat("Résidus : p = ", norm$p.value, "\n")
    }
  })
  
  output$normalityInterpretation <- renderUI({
    req(values$normalityResults, input$responseVar)
    current_var <- input$responseVar[values$currentValidationVar]
    norm <- values$normalityResults[[current_var]]
    
    if (is.null(norm)) {
      interp_text <- "Aucun résultat de normalité disponible pour cette variable."
    } else if ("group1" %in% names(norm)) {
      interp1 <- interpret_normality(norm$group1$p.value)
      interp2 <- interpret_normality(norm$group2$p.value)
      interp_text <- paste0("Groupe 1: ", interp1, "<br>Groupe 2: ", interp2)
    } else {
      interp_text <- interpret_normality(norm$p.value)
    }
    HTML(paste0("<div class='interpretation-box'>", interp_text, "</div>"))
  })
  
  # Affichage des résultats d'homogénéité
  output$homogeneityResults <- renderPrint({
    req(values$homogeneityResults, input$responseVar)
    current_var <- input$responseVar[values$currentValidationVar]
    hom <- values$homogeneityResults[[current_var]]
    
    if (is.null(hom)) {
      cat("Aucun résultat d'homogénéité disponible pour cette variable.\n")
    } else {
      cat("p = ", hom$`Pr(>F)`[1], "\n")
    }
  })
  
  output$homogeneityInterpretation <- renderUI({
    req(values$homogeneityResults, input$responseVar)
    current_var <- input$responseVar[values$currentValidationVar]
    hom <- values$homogeneityResults[[current_var]]
    
    if (is.null(hom)) {
      interp_text <- "Aucun résultat d'homogénéité disponible pour cette variable."
    } else {
      interp_text <- interpret_homogeneity(hom$`Pr(>F)`[1])
    }
    HTML(paste0("<div class='interpretation-box'>", interp_text, "</div>"))
  })
  
  # Diagnostics des modèles
  output$modelDiagnostics <- renderPlot({
    req(values$currentModel)
    par(mfrow = c(2, 2))
    plot(values$currentModel)
  })
  
  output$modelDiagnosticsInterpretation <- renderUI({
    req(values$currentModel)
    interp_text <- "Vérifiez les graphiques pour les violations des hypothèses (normalité, homoscédasticité, etc.)."
    HTML(paste0("<div class='interpretation-box'>", interp_text, "</div>"))
  })
  
  # Téléchargement des diagnostics de modèles
  output$downloadModelDiagnostics <- downloadHandler(
    filename = function() {
      paste0("diagnostics_modele_", Sys.Date(), ".png")
    },
    content = function(file) {
      png(file, width = 3200, height = 2400, res = 300, type = "cairo")
      par(mfrow = c(2, 2))
      plot(values$currentModel)
      dev.off()
    }
  )
  
  # Téléchargement du QQ-plot
  output$downloadQQPlot <- downloadHandler(
    filename = function() {
      paste0("qqplot_residus_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(values$currentModel)
      residuals_data <- residuals(values$currentModel)
      df <- data.frame(sample = residuals_data)
      
      p <- ggplot(df, aes(sample = sample)) +
        qqplotr::stat_qq_band(distribution = "norm", bandType = "pointwise", alpha = 0.2) +
        qqplotr::stat_qq_line(distribution = "norm") +
        qqplotr::stat_qq_point(distribution = "norm") +
        theme_minimal() +
        labs(title = "QQ-plot des résidus", 
             x = "Quantiles théoriques", 
             y = "Quantiles observés")
      
      ggsave(file, plot = p, width = 10, height = 8, dpi = 2000, type = "cairo-png")
    }
  )
  
  # QQ-plot des résidus
  output$qqPlotResiduals <- renderPlot({
    req(values$currentModel)
    residuals_data <- residuals(values$currentModel)
    df <- data.frame(sample = residuals_data)
    
    ggplot(df, aes(sample = sample)) +
      qqplotr::stat_qq_band(distribution = "norm", bandType = "pointwise", alpha = 0.2) +
      qqplotr::stat_qq_line(distribution = "norm") +
      qqplotr::stat_qq_point(distribution = "norm") +
      theme_minimal() +
      labs(title = "QQ-plot des résidus", 
           x = "Quantiles théoriques", 
           y = "Quantiles observés")
  })
  
  output$qqPlotInterpretation <- renderUI({
    interp_text <- "Les points devraient suivre la ligne droite pour une normalité des résidus."
    HTML(paste0("<div class='interpretation-box'>", interp_text, "</div>"))
  })
  
  # Normalité des résidus
  output$normalityResult <- renderPrint({
    req(values$currentModel)
    residuals_data <- residuals(values$currentModel)
    if (length(residuals_data) >= 3 && length(residuals_data) <= 5000) {
      shapiro.test(residuals_data)
    } else {
      cat("Nombre d'observations insuffisant pour le test de Shapiro-Wilk.\n")
    }
  })
  
  output$normalityResidInterpretation <- renderUI({
    req(values$currentModel)
    residuals_data <- residuals(values$currentModel)
    if (length(residuals_data) >= 3 && length(residuals_data) <= 5000) {
      norm_test <- shapiro.test(residuals_data)
      interp_text <- interpret_normality_resid(norm_test$p.value)
    } else {
      interp_text <- "Nombre d'observations insuffisant pour le test de Shapiro-Wilk."
    }
    HTML(paste0("<div class='interpretation-box'>", interp_text, "</div>"))
  })
  
  # Homogénéité des résidus
  output$leveneResidResult <- renderPrint({
    req(values$currentModel)
    residuals_data <- residuals(values$currentModel)
    fitted_data <- fitted(values$currentModel)
    fitted_factor <- cut(fitted_data, breaks = 2, labels = c("Bas", "Haut"))
    test_data <- data.frame(residuals = residuals_data, fitted_group = fitted_factor)
    car::leveneTest(residuals ~ fitted_group, data = test_data)
  })
  
  output$homogeneityResidInterpretation <- renderUI({
    req(values$currentModel)
    residuals_data <- residuals(values$currentModel)
    fitted_data <- fitted(values$currentModel)
    fitted_factor <- cut(fitted_data, breaks = 2, labels = c("Bas", "Haut"))
    test_data <- data.frame(residuals = residuals_data, fitted_group = fitted_factor)
    hom_test <- car::leveneTest(residuals ~ fitted_group, data = test_data)
    interp_text <- interpret_homogeneity_resid(hom_test$`Pr(>F)`[1])
    HTML(paste0("<div class='interpretation-box'>", interp_text, "</div>"))
  })
  
  # Autocorrélation
  output$autocorrResult <- renderPrint({
    req(values$currentModel)
    lmtest::dwtest(values$currentModel)
  })
  
  output$autocorrInterpretation <- renderUI({
    req(values$currentModel)
    dw_test <- lmtest::dwtest(values$currentModel)
    interp_text <- if (dw_test$p.value > 0.05) {
      "Pas d'autocorrélation significative des résidus (p > 0.05)."
    } else {
      "Autocorrélation significative des résidus (p < 0.05). Vérifiez l'indépendance des observations."
    }
    HTML(paste0("<div class='interpretation-box'>", interp_text, "</div>"))
  })
  
  # Summary du modèle
  output$modelSummary <- renderPrint({
    req(values$currentModel)
    summary(values$currentModel)
  })
  
  # Télécharger les résultats des tests en Excel
  output$downloadTestsExcel <- downloadHandler(
    filename = function() {
      paste0("resultats_tests_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      wb <- openxlsx::createWorkbook()
      
      # Feuille des résultats
      openxlsx::addWorksheet(wb, "Resultats")
      openxlsx::writeData(wb, "Resultats", values$testResultsDF)
      
      # Feuille de validation si disponible
      if (!is.null(values$normalityResults)) {
        validation_df <- data.frame(Variable = names(values$normalityResults))
        validation_df$Normality_p <- sapply(values$normalityResults, function(x) x$p.value %||% NA)
        validation_df$Homogeneity_p <- sapply(values$homogeneityResults, function(x) x$`Pr(>F)`[1] %||% NA)
        
        openxlsx::addWorksheet(wb, "Validation")
        openxlsx::writeData(wb, "Validation", validation_df)
      }
      
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  # ---- Comparaisons multiples PostHoc ----
  
  # Fonction pour calculer le coefficient de variation (à ajouter si elle n'existe pas)
  calc_cv <- function(x) {
    if (length(x) <= 1 || sd(x, na.rm = TRUE) == 0) return(0)
    return((sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)) * 100)
  }
  
  observeEvent(input$runMultiple, {
    req(input$multiResponse, input$multiFactor)
    
    # Afficher la notification de chargement
    showNotification("Analyse en cours...", type = "message", duration = NULL, id = "loading")
    
    multi_results_list <- list()
    df <- values$filteredData
    
    for (var in input$multiResponse) {
      # Effet principal
      for (fvar in input$multiFactor) {
        tryCatch({
          if (input$testType == "param") {
            model <- aov(as.formula(paste(var, "~", fvar)), data = df)
            if (input$multiTest %in% c("lsd", "tukey", "duncan", "snk", "scheffe", "regw", "waller")) {
              mc_func <- switch(input$multiTest,
                                "lsd" = agricolae::LSD.test,
                                "tukey" = agricolae::HSD.test,
                                "duncan" = agricolae::duncan.test,
                                "snk" = agricolae::SNK.test,
                                "scheffe" = agricolae::scheffe.test,
                                "regw" = agricolae::REGW.test,
                                "waller" = agricolae::waller.test)
              mc <- mc_func(model, fvar, group = TRUE)
              groups <- mc$groups
              colnames(groups) <- c("means", "groups")
              groups[[fvar]] <- rownames(groups)
            } else if (input$multiTest == "bonferroni") {
              emm <- emmeans::emmeans(model, as.formula(paste("~", fvar)))
              mc <- pairs(emm, adjust = "bonferroni")
              pmat <- as.matrix(summary(mc)$p.value)
              if (is.null(dim(pmat))) {
                groups <- data.frame(groups = rep("a", length(levels(df[[fvar]]))))
                groups[[fvar]] <- levels(df[[fvar]])
              } else {
                pmat[is.na(pmat)] <- t(pmat)[is.na(pmat)]
                diag(pmat) <- 1
                groups_letters <- multcompView::multcompLetters(pmat, threshold = 0.05)$Letters
                groups <- data.frame(groups = groups_letters)
                groups[[fvar]] <- names(groups_letters)
              }
            } else if (input$multiTest == "dunnett") {
              emm <- emmeans::emmeans(model, as.formula(paste("~", fvar)))
              groups <- multcomp::cld(emm, Letters = letters)
              groups <- as.data.frame(groups)
              groups <- groups[, c(fvar, ".group")]
              colnames(groups) <- c(fvar, "groups")
              groups$groups <- trimws(groups$groups)
            } else if (input$multiTest == "games") {
              mc <- PMCMRplus::gamesHowellTest(as.formula(paste(var, "~", fvar)), data = df)
              pmat <- as.matrix(mc$p.value)
              pmat[is.na(pmat)] <- t(pmat)[is.na(pmat)]
              diag(pmat) <- 1
              groups_letters <- multcompView::multcompLetters(pmat, threshold = 0.05)$Letters
              groups <- data.frame(groups = groups_letters)
              groups[[fvar]] <- names(groups_letters)
            }
          } else {  # nonparam
            if (input$multiTestNonParam == "kruskal") {
              mc <- agricolae::kruskal(df[[var]], df[[fvar]], group = TRUE)
              groups <- mc$groups
              colnames(groups) <- c("means", "groups")
              groups[[fvar]] <- rownames(groups)
            } else if (input$multiTestNonParam == "dunn") {
              
              tryCatch({
                mc <- PMCMRplus::dunnTest(df[[var]], df[[fvar]])
                pmat <- as.matrix(mc$p.value)
                pmat[is.na(pmat)] <- t(pmat)[is.na(pmat)]
                diag(pmat) <- 1
                groups_letters <- multcompView::multcompLetters(pmat, threshold = 0.05)$Letters
                groups <- data.frame(groups = groups_letters)
                groups[[fvar]] <- names(groups_letters)
              }, error = function(e) {
                showNotification(paste("Erreur dunnTest pour", var, fvar, "- Utilisation de Kruskal"), type = "warning")
                mc <- agricolae::kruskal(df[[var]], df[[fvar]], group = TRUE)
                groups <- mc$groups
                colnames(groups) <- c("means", "groups")
                groups[[fvar]] <- rownames(groups)
              })
            } else if (input$multiTestNonParam == "conover") {
              tryCatch({
                mc <- PMCMRplus::kwAllPairsConoverTest(df[[var]], df[[fvar]])
                pmat <- as.matrix(mc$p.value)
                pmat[is.na(pmat)] <- t(pmat)[is.na(pmat)]
                diag(pmat) <- 1
                groups_letters <- multcompView::multcompLetters(pmat, threshold = 0.05)$Letters
                groups <- data.frame(groups = groups_letters)
                groups[[fvar]] <- names(groups_letters)
              }, error = function(e) {
                showNotification(paste("Erreur Conover pour", var, fvar, "- Utilisation de Kruskal"), type = "warning")
                mc <- agricolae::kruskal(df[[var]], df[[fvar]], group = TRUE)
                groups <- mc$groups
                colnames(groups) <- c("means", "groups")
                groups[[fvar]] <- rownames(groups)
              })
            } else if (input$multiTestNonParam == "nemenyi") {
              tryCatch({
                mc <- PMCMRplus::kwAllPairsNemenyiTest(df[[var]], df[[fvar]])
                pmat <- as.matrix(mc$p.value)
                pmat[is.na(pmat)] <- t(pmat)[is.na(pmat)]
                diag(pmat) <- 1
                groups_letters <- multcompView::multcompLetters(pmat, threshold = 0.05)$Letters
                groups <- data.frame(groups = groups_letters)
                groups[[fvar]] <- names(groups_letters)
              }, error = function(e) {
                showNotification(paste("Erreur Nemenyi pour", var, fvar, "- Utilisation de Kruskal"), type = "warning")
                mc <- agricolae::kruskal(df[[var]], df[[fvar]], group = TRUE)
                groups <- mc$groups
                colnames(groups) <- c("means", "groups")
                groups[[fvar]] <- rownames(groups)
              })
            }
          }
          
          # Descriptive stats
          desc <- df %>%
            group_by(across(all_of(fvar))) %>%
            summarise(
              Moyenne = mean(.data[[var]], na.rm = TRUE),
              Ecart_type = sd(.data[[var]], na.rm = TRUE),
              N = n(),
              Erreur_type = Ecart_type / sqrt(N),
              CV = calc_cv(.data[[var]]),
              .groups = "drop"
            )
          
          # Vérifier que les noms correspondent avant la fusion
          if (fvar %in% colnames(groups) && fvar %in% colnames(desc)) {
            res <- merge(desc, groups, by = fvar, all.x = TRUE)
          } else {
            showNotification(paste("Problème de correspondance des noms pour", var, fvar), type = "warning")
            next
          }
          
          res <- res %>%
            mutate(
              Moyenne = round(Moyenne, 2),
              Ecart_type = round(Ecart_type, 2),
              Erreur_type = round(Erreur_type, 2),
              CV = round(CV, 2),
              `Moyenne±Ecart_type` = paste0(Moyenne, "±", Ecart_type, " ", groups),
              `Moyenne±Erreur_type` = paste0(Moyenne, "±", Erreur_type, " ", groups),
              Variable = var,
              Facteur = fvar,
              Type = "main"
            )
          
          multi_results_list[[paste(var, fvar, "main", sep = "_")]] <- res
          
        }, error = function(e) {
          showNotification(paste("Erreur posthoc pour", var, fvar, "(main):", e$message), type = "error")
        })
      }
      
      # Interactions (if selected and significant)
      if (input$posthocInteraction && length(input$multiFactor) > 1) {
        factor_combinations <- combn(input$multiFactor, 2, simplify = FALSE)
        for (fcomb in factor_combinations) {
          fvar1 <- fcomb[1]
          fvar2 <- fcomb[2]
          interaction_term <- paste(fvar1, fvar2, sep = ":")
          formula_str <- paste(var, "~", fvar1, "*", fvar2)
          
          tryCatch({
            # Créer une copie locale du dataframe pour éviter les conflits
            df_temp <- df
            
            # Create interaction column in temporary dataframe
            df_temp[[interaction_term]] <- as.factor(paste(df_temp[[fvar1]], df_temp[[fvar2]], sep = ":"))
            
            if (input$testType == "param") {
              model <- aov(as.formula(formula_str), data = df_temp)
              anova_res <- summary(model)[[1]]
              interaction_row <- paste(fvar1, fvar2, sep = ":")
              
              # Check if interaction exists in the model
              if (interaction_row %in% rownames(anova_res)) {
                interaction_pvalue <- anova_res[interaction_row, "Pr(>F)"]
                
                if (!is.na(interaction_pvalue) && interaction_pvalue < 0.05) {
                  # Create a model with the interaction as a single factor
                  model_int <- aov(as.formula(paste(var, "~", interaction_term)), data = df_temp)
                  
                  # Initialiser groups à NULL pour éviter les erreurs
                  groups <- NULL
                  
                  if (input$multiTest %in% c("lsd", "tukey", "duncan", "snk", "scheffe", "regw", "waller")) {
                    mc_func <- switch(input$multiTest,
                                      "lsd" = agricolae::LSD.test,
                                      "tukey" = agricolae::HSD.test,
                                      "duncan" = agricolae::duncan.test,
                                      "snk" = agricolae::SNK.test,
                                      "scheffe" = agricolae::scheffe.test,
                                      "regw" = agricolae::REGW.test,
                                      "waller" = agricolae::waller.test)
                    mc <- mc_func(model_int, interaction_term, group = TRUE)
                    groups <- mc$groups
                    # Standardiser les noms de colonnes
                    if ("means" %in% colnames(groups) || ncol(groups) >= 2) {
                      colnames(groups)[1:2] <- c("means", "groups")
                      groups[[interaction_term]] <- rownames(groups)
                    }
                  } else if (input$multiTest == "bonferroni") {
                    emm <- emmeans::emmeans(model_int, as.formula(paste("~", interaction_term)))
                    mc <- pairs(emm, adjust = "bonferroni")
                    pmat_summary <- summary(mc)
                    if ("p.value" %in% colnames(pmat_summary)) {
                      pmat <- as.matrix(pmat_summary$p.value)
                    } else {
                      pmat <- as.matrix(pmat_summary[, "p.value"])
                    }
                    if (is.null(dim(pmat)) || ncol(pmat) <= 1) {
                      groups <- data.frame(groups = rep("a", length(levels(df_temp[[interaction_term]]))))
                      groups[[interaction_term]] <- levels(df_temp[[interaction_term]])
                    } else {
                      pmat[is.na(pmat)] <- t(pmat)[is.na(pmat)]
                      diag(pmat) <- 1
                      groups_letters <- multcompView::multcompLetters(pmat, threshold = 0.05)$Letters
                      groups <- data.frame(groups = groups_letters)
                      groups[[interaction_term]] <- names(groups_letters)
                    }
                  } else if (input$multiTest == "dunnett") {
                    emm <- emmeans::emmeans(model_int, as.formula(paste("~", interaction_term)))
                    groups_cld <- multcomp::cld(emm, Letters = letters)
                    groups <- as.data.frame(groups_cld)
                    if (".group" %in% colnames(groups)) {
                      groups <- groups[, c(interaction_term, ".group")]
                      colnames(groups) <- c(interaction_term, "groups")
                    } else {
                      groups$groups <- "a"
                    }
                    groups$groups <- trimws(groups$groups)
                  } else if (input$multiTest == "games") {
                    mc <- PMCMRplus::gamesHowellTest(as.formula(paste(var, "~", interaction_term)), data = df_temp)
                    pmat <- as.matrix(mc$p.value)
                    pmat[is.na(pmat)] <- t(pmat)[is.na(pmat)]
                    diag(pmat) <- 1
                    groups_letters <- multcompView::multcompLetters(pmat, threshold = 0.05)$Letters
                    groups <- data.frame(groups = groups_letters)
                    groups[[interaction_term]] <- names(groups_letters)
                  }
                  
                  # Vérifier que groups n'est pas NULL avant de continuer
                  if (!is.null(groups) && nrow(groups) > 0) {
                    desc <- df_temp %>%
                      group_by(across(all_of(c(fvar1, fvar2)))) %>%
                      summarise(
                        Moyenne = mean(.data[[var]], na.rm = TRUE),
                        Ecart_type = sd(.data[[var]], na.rm = TRUE),
                        N = n(),
                        Erreur_type = Ecart_type / sqrt(N),
                        CV = calc_cv(.data[[var]]),
                        .groups = "drop"
                      ) %>%
                      mutate(!!interaction_term := paste(.data[[fvar1]], .data[[fvar2]], sep = ":"))
                    
                    # Vérifier la correspondance avant la fusion
                    if (interaction_term %in% colnames(groups) && interaction_term %in% colnames(desc)) {
                      res <- merge(desc, groups, by = interaction_term, all.x = TRUE)
                      
                      # Gérer les valeurs manquantes dans groups
                      if (!"groups" %in% colnames(res)) {
                        res$groups <- "a"
                      }
                      res$groups[is.na(res$groups)] <- "a"
                      
                      res <- res %>%
                        mutate(
                          Moyenne = round(Moyenne, 2),
                          Ecart_type = round(Ecart_type, 2),
                          Erreur_type = round(Erreur_type, 2),
                          CV = round(CV, 2),
                          `Moyenne±Ecart_type` = paste0(Moyenne, "±", Ecart_type, " ", groups),
                          `Moyenne±Erreur_type` = paste0(Moyenne, "±", Erreur_type, " ", groups),
                          Variable = var,
                          Facteur = interaction_term,
                          Type = "interaction"
                        )
                      
                      multi_results_list[[paste(var, interaction_term, "interaction", sep = "_")]] <- res
                    }
                  }
                }
              }
            } else {  # nonparam 
              # Créer d'abord la colonne d'interaction dans df_temp
              df_temp[[interaction_term]] <- as.factor(paste(df_temp[[fvar1]], df_temp[[fvar2]], sep = ":"))
              
              kw <- kruskal.test(as.formula(paste(var, "~", interaction_term)), data = df_temp)
              if (!is.na(kw$p.value) && kw$p.value < 0.05) {
                groups <- NULL
                
                if (input$multiTestNonParam == "kruskal") {
                  mc <- agricolae::kruskal(df_temp[[var]], df_temp[[interaction_term]], group = TRUE)
                  groups <- mc$groups
                  colnames(groups)[1:2] <- c("means", "groups")
                  groups[[interaction_term]] <- rownames(groups)
                } else if (input$multiTestNonParam == "dunn") {
                  # CORRECTION: Syntaxe corrigée pour dunnTest avec gestion d'erreurs
                  tryCatch({
                    mc <- PMCMRplus::dunnTest(df_temp[[var]], df_temp[[interaction_term]])
                    pmat <- as.matrix(mc$p.value)
                    pmat[is.na(pmat)] <- t(pmat)[is.na(pmat)]
                    diag(pmat) <- 1
                    groups_letters <- multcompView::multcompLetters(pmat, threshold = 0.05)$Letters
                    groups <- data.frame(groups = groups_letters)
                    groups[[interaction_term]] <- names(groups_letters)
                  }, error = function(e) {
                    showNotification(paste("Erreur dunnTest interaction:", e$message, "- Utilisation de Kruskal"), type = "warning")
                    mc <- agricolae::kruskal(df_temp[[var]], df_temp[[interaction_term]], group = TRUE)
                    groups <- mc$groups
                    colnames(groups)[1:2] <- c("means", "groups")
                    groups[[interaction_term]] <- rownames(groups)
                  })
                } else if (input$multiTestNonParam == "conover") {
                  tryCatch({
                    mc <- PMCMRplus::kwAllPairsConoverTest(df_temp[[var]], df_temp[[interaction_term]])
                    pmat <- as.matrix(mc$p.value)
                    pmat[is.na(pmat)] <- t(pmat)[is.na(pmat)]
                    diag(pmat) <- 1
                    groups_letters <- multcompView::multcompLetters(pmat, threshold = 0.05)$Letters
                    groups <- data.frame(groups = groups_letters)
                    groups[[interaction_term]] <- names(groups_letters)
                  }, error = function(e) {
                    showNotification(paste("Erreur Conover interaction:", e$message, "- Utilisation de Kruskal"), type = "warning")
                    mc <- agricolae::kruskal(df_temp[[var]], df_temp[[interaction_term]], group = TRUE)
                    groups <- mc$groups
                    colnames(groups)[1:2] <- c("means", "groups")
                    groups[[interaction_term]] <- rownames(groups)
                  })
                } else if (input$multiTestNonParam == "nemenyi") {
                  tryCatch({
                    mc <- PMCMRplus::kwAllPairsNemenyiTest(df_temp[[var]], df_temp[[interaction_term]])
                    pmat <- as.matrix(mc$p.value)
                    pmat[is.na(pmat)] <- t(pmat)[is.na(pmat)]
                    diag(pmat) <- 1
                    groups_letters <- multcompView::multcompLetters(pmat, threshold = 0.05)$Letters
                    groups <- data.frame(groups = groups_letters)
                    groups[[interaction_term]] <- names(groups_letters)
                  }, error = function(e) {
                    showNotification(paste("Erreur Nemenyi interaction:", e$message, "- Utilisation de Kruskal"), type = "warning")
                    mc <- agricolae::kruskal(df_temp[[var]], df_temp[[interaction_term]], group = TRUE)
                    groups <- mc$groups
                    colnames(groups)[1:2] <- c("means", "groups")
                    groups[[interaction_term]] <- rownames(groups)
                  })
                }
                
                if (!is.null(groups) && nrow(groups) > 0) {
                  desc <- df_temp %>%
                    group_by(across(all_of(c(fvar1, fvar2)))) %>%
                    summarise(
                      Moyenne = mean(.data[[var]], na.rm = TRUE),
                      Ecart_type = sd(.data[[var]], na.rm = TRUE),
                      N = n(),
                      Erreur_type = Ecart_type / sqrt(N),
                      CV = calc_cv(.data[[var]]),
                      .groups = "drop"
                    ) %>%
                    mutate(!!interaction_term := paste(.data[[fvar1]], .data[[fvar2]], sep = ":"))
                  
                  if (interaction_term %in% colnames(groups) && interaction_term %in% colnames(desc)) {
                    res <- merge(desc, groups, by = interaction_term, all.x = TRUE)
                    
                    if (!"groups" %in% colnames(res)) {
                      res$groups <- "a"
                    }
                    res$groups[is.na(res$groups)] <- "a"
                    
                    res <- res %>%
                      mutate(
                        Moyenne = round(Moyenne, 2),
                        Ecart_type = round(Ecart_type, 2),
                        Erreur_type = round(Erreur_type, 2),
                        CV = round(CV, 2),
                        `Moyenne±Ecart_type` = paste0(Moyenne, "±", Ecart_type, " ", groups),
                        `Moyenne±Erreur_type` = paste0(Moyenne, "±", Erreur_type, " ", groups),
                        Variable = var,
                        Facteur = interaction_term,
                        Type = "interaction"
                      )
                    
                    multi_results_list[[paste(var, interaction_term, "interaction", sep = "_")]] <- res
                  }
                }
              }
            }
          }, error = function(e) {
            showNotification(paste("Erreur posthoc pour", var, interaction_term, "(interaction):", e$message), type = "error")
          })
        }
      }
    }
    
    # Améliorer la gestion des résultats finaux
    removeNotification("loading")
    
    if (length(multi_results_list) > 0) {
      # Vérifier que tous les éléments de la liste ont les mêmes colonnes
      all_cols <- unique(unlist(lapply(multi_results_list, colnames)))
      
      # Standardiser les colonnes pour tous les dataframes
      multi_results_list <- lapply(multi_results_list, function(df) {
        missing_cols <- setdiff(all_cols, colnames(df))
        for (col in missing_cols) {
          df[[col]] <- NA
        }
        return(df[, all_cols])
      })
      
      values$allPostHocResults[[length(values$allPostHocResults) + 1]] <- do.call(rbind, multi_results_list)
      values$multiResultsMain <- values$allPostHocResults[[length(values$allPostHocResults)]]
      values$currentVarIndex <- 1  
      showNotification("Comparaisons multiples exécutées", type = "message")
    } else {
      showNotification("Aucun résultat post-hoc généré", type = "warning")
    }
  })
  
  # UI Outputs pour Multi-Selection
  output$multiResponseSelect <- renderUI({
    req(values$filteredData)
    num_cols <- names(values$filteredData)[sapply(values$filteredData, is.numeric)]
    tagList(
      pickerInput("multiResponse", "Variable(s) réponse:", choices = num_cols, multiple = TRUE, 
                  options = list(`actions-box` = TRUE, `selected-text-format` = "count > 3")),
      actionButton("selectAllMultiResponse", "Tout sélectionner", class = "btn-success btn-sm"),
      actionButton("deselectAllMultiResponse", "Tout désélectionner", class = "btn-danger btn-sm")
    )
  })
  
  observeEvent(input$selectAllMultiResponse, {
    num_cols <- names(values$filteredData)[sapply(values$filteredData, is.numeric)]
    updatePickerInput(session, "multiResponse", selected = num_cols)
  })
  
  observeEvent(input$deselectAllMultiResponse, {
    updatePickerInput(session, "multiResponse", selected = character(0))
  })
  
  output$multiFactorSelect <- renderUI({
    req(values$filteredData)
    fac_cols <- names(values$filteredData)[sapply(values$filteredData, is.factor)]
    tagList(
      pickerInput("multiFactor", "Facteur(s):", choices = fac_cols, multiple = TRUE, 
                  options = list(`actions-box` = TRUE, `selected-text-format` = "count > 3")),
      actionButton("selectAllMultiFactors", "Tout sélectionner", class = "btn-success btn-sm"),
      actionButton("deselectAllMultiFactors", "Tout désélectionner", class = "btn-danger btn-sm")
    )
  })
  
  observeEvent(input$selectAllMultiFactors, {
    fac_cols <- names(values$filteredData)[sapply(values$filteredData, is.factor)]
    updatePickerInput(session, "multiFactor", selected = fac_cols)
  })
  
  observeEvent(input$deselectAllMultiFactors, {
    updatePickerInput(session, "multiFactor", selected = character(0))
  })
  
  # Results Table - 
  output$multipleResults <- renderDT({
    req(values$multiResultsMain)
    display_data <- values$multiResultsMain
    if (nrow(display_data) == 0) return(NULL)
    
    # Filtrer selon le type sélectionné
    if (input$resultTypeDisplay == "main") {
      display_data <- display_data[display_data$Type == "main", ]
    } else {
      display_data <- display_data[display_data$Type == "interaction", ]
    }
    
    if (nrow(display_data) == 0) {
      if (input$resultTypeDisplay == "interaction") {
        showNotification("Aucune interaction significative trouvée ou l'option interaction n'était pas activée", 
                         type = "message", duration = 3000)
      }
      return(NULL)
    }
    
    # Colonnes de base à afficher
    base_cols <- c("Variable", "Facteur", "Moyenne", "Ecart_type", "Erreur_type", "CV", "groups", "N")
    
    # Ajouter les colonnes spécifiques au facteur si elles existent
    factor_specific_cols <- c()
    
    # Pour les effets principaux
    if (input$resultTypeDisplay == "main") {
      available_factors <- unique(display_data$Facteur)
      for (factor in available_factors) {
        if (factor %in% colnames(display_data)) {
          factor_specific_cols <- c(factor_specific_cols, factor)
        }
      }
    } else {
      # Pour les interactions
      for (col in colnames(display_data)) {
        if (grepl(":", col) && col %in% colnames(display_data)) {
          factor_specific_cols <- c(factor_specific_cols, col)
        }
        if (col %in% input$multiFactor) {
          factor_specific_cols <- c(factor_specific_cols, col)
        }
      }
    }
    
    # Colonnes résumées
    summary_cols <- c("Moyenne±Ecart_type", "Moyenne±Erreur_type")
    
    # Combiner toutes les colonnes
    all_potential_cols <- c(base_cols, factor_specific_cols, summary_cols)
    cols_to_show <- all_potential_cols[all_potential_cols %in% colnames(display_data)]
    
    # S'assurer qu'on a au moins les colonnes essentielles
    essential_cols <- c("Variable", "Facteur", "Moyenne", "groups")
    missing_essential <- setdiff(essential_cols, cols_to_show)
    
    if (length(missing_essential) > 0) {
      showNotification(paste("Colonnes manquantes:", paste(missing_essential, collapse = ", ")), 
                       type = "warning")
      cols_to_show <- colnames(display_data)
    }
    
    # Titre informatif
    title_text <- if (input$resultTypeDisplay == "main") {
      paste("Effets principaux -", nrow(display_data), "comparaisons")
    } else {
      paste("Interactions significatives -", nrow(display_data), "comparaisons")
    }
    
    # Créer le tableau avec formatage amélioré - CORRECTION: caption au lieu de formatCaption
    dt <- datatable(
      display_data[, cols_to_show, drop = FALSE], 
      options = list(
        scrollX = TRUE, 
        pageLength = 15,
        lengthMenu = c(10, 15, 25, 50),
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel'),
        columnDefs = list(
          list(className = 'dt-center', targets = '_all'),
          list(width = '150px', targets = c(0, 1)),
          list(width = '80px', targets = which(colnames(display_data[, cols_to_show, drop = FALSE]) %in% 
                                                 c("Moyenne", "Ecart_type", "CV", "N"))),
          list(width = '60px', targets = which(colnames(display_data[, cols_to_show, drop = FALSE]) == "groups"))
        )
      ), 
      rownames = FALSE,
      extensions = 'Buttons',
      class = 'cell-border stripe',
      # CORRECTION: Utiliser caption directement
      caption = htmltools::tags$caption(
        title_text,
        style = "caption-side: top; text-align: center; font-weight: bold; font-size: 16px; margin-bottom: 10px;"
      )
    ) %>%
      formatRound(columns = c("Moyenne", "Ecart_type", "Erreur_type", "CV"), digits = 2) %>%
      formatStyle(
        'groups',
        backgroundColor = styleEqual(unique(display_data$groups), 
                                     rainbow(length(unique(display_data$groups)), alpha = 0.3)),
        fontWeight = 'bold'
      )
    
    return(dt)
  })
  
  # Indicateurs de type de résultats
  output$resultTypeIndicators <- renderUI({
    req(values$multiResultsMain)
    
    main_count <- sum(values$multiResultsMain$Type == "main", na.rm = TRUE)
    interaction_count <- sum(values$multiResultsMain$Type == "interaction", na.rm = TRUE)
    
    tagList(
      div(style = "display: flex; gap: 10px;",
          # Indicateur effets principaux
          div(style = paste0("padding: 5px 10px; border-radius: 15px; font-size: 0.8em; color: white; ",
                             if(main_count > 0) "background-color: #27ae60;" else "background-color: #95a5a6;"),
              icon("check"), paste("Principaux:", main_count)
          ),
          # Indicateur interactions
          div(style = paste0("padding: 5px 10px; border-radius: 15px; font-size: 0.8em; color: white; ",
                             if(interaction_count > 0) "background-color: #e74c3c;" else "background-color: #95a5a6;"),
              icon(if(interaction_count > 0) "check" else "times"), 
              paste("Interactions:", interaction_count)
          )
      )
    )
  })
  
  # Résumé des analyses
  output$analysisSummary <- renderUI({
    req(values$multiResultsMain)
    
    total_comparisons <- nrow(values$multiResultsMain)
    variables_analyzed <- length(unique(values$multiResultsMain$Variable))
    factors_analyzed <- length(unique(values$multiResultsMain$Facteur))
    
    div(style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; border-left: 4px solid #3498db;",
        strong("Résumé de l'analyse:"), br(),
        sprintf("• %d variable(s) analysée(s)", variables_analyzed), br(),
        sprintf("• %d facteur(s)/interaction(s) testée(s)", factors_analyzed), br(),
        sprintf("• %d comparaison(s) au total", total_comparisons)
    )
  })
  
  # Message pour interactions manquantes
  output$noInteractionMsg <- renderUI({
    req(values$multiResultsMain)
    
    interaction_count <- sum(values$multiResultsMain$Type == "interaction", na.rm = TRUE)
    
    if (interaction_count == 0) {
      div(style = "text-align: center; padding: 20px; color: #7f8c8d; font-style: italic;",
          icon("info-circle", style = "font-size: 1.5em; color: #3498db; margin-bottom: 10px;"), br(),
          "Aucune interaction significative détectée.", br(),
          "Les interactions ne sont testées que si p < 0.05 dans l'ANOVA globale."
      )
    }
  })
  
  # Titre dynamique du graphique
  output$plotTitle <- renderUI({
    req(input$multiResponse, values$currentVarIndex)
    
    current_var_idx <- values$currentVarIndex %||% 1
    current_var <- input$multiResponse[current_var_idx]
    type_text <- if(input$resultTypeDisplay == "main") "Effets principaux" else "Interactions"
    
    paste("Graphique:", type_text, "-", current_var)
  })
  
  output$showPosthocResults <- reactive({
    !is.null(values$multiResultsMain) && nrow(values$multiResultsMain) > 0
  })
  outputOptions(output, "showPosthocResults", suspendWhenHidden = FALSE)
  
  # Variable Navigation
  output$showVariableNavigation <- reactive({
    length(input$multiResponse) > 1
  })
  outputOptions(output, "showVariableNavigation", suspendWhenHidden = FALSE)
  
  output$variableNavigation <- renderUI({
    req(length(input$multiResponse) > 1)
    current_idx <- values$currentVarIndex %||% 1
    total_vars <- length(input$multiResponse)
    div(style = "display: inline-block;",
        actionButton("prevMultiVar", "", icon = icon("chevron-left"), class = "btn-sm"),
        span(paste("Variable", current_idx, "sur", total_vars, ":", input$multiResponse[current_idx]), 
             style = "margin: 0 15px; font-weight: bold;"),
        actionButton("nextMultiVar", "", icon = icon("chevron-right"), class = "btn-sm")
    )
  })
  
  observeEvent(input$prevMultiVar, {
    values$currentVarIndex <- if (values$currentVarIndex > 1) values$currentVarIndex - 1 else length(input$multiResponse)
  })
  
  observeEvent(input$nextMultiVar, {
    values$currentVarIndex <- if (values$currentVarIndex < length(input$multiResponse)) values$currentVarIndex + 1 else 1
  })
  
  # Plot avec coloration par groupe
  output$multiPlot <- renderPlotly({
    req(input$multiResponse, input$multiFactor, values$multiResultsMain)
    
    current_var_idx <- values$currentVarIndex %||% 1
    resp_var <- input$multiResponse[current_var_idx]
    
    # Déterminer quel facteur utiliser
    if (input$resultTypeDisplay == "main") {
      fvar <- input$multiFactor[1]
      plot_data <- values$filteredData
    } else {
      # Pour interactions
      if (length(input$multiFactor) < 2) {
        showNotification("Deux facteurs minimum requis pour afficher les interactions", type = "warning")
        return(NULL)
      }
      fvar <- paste(input$multiFactor[1], input$multiFactor[2], sep = ":")
      plot_data <- values$filteredData
      # Créer une colonne d’interaction si elle n’existe pas
      if (!(fvar %in% names(plot_data))) {
        plot_data[[fvar]] <- as.factor(paste(plot_data[[input$multiFactor[1]]], 
                                             plot_data[[input$multiFactor[2]]], sep = ":"))
      }
    }
    
    # Obtenir les données agrégées pour la variable et le facteur actuels
    agg <- values$multiResultsMain[values$multiResultsMain$Variable == resp_var & 
                                     values$multiResultsMain$Facteur == fvar, ]
    
    if (nrow(agg) == 0) {
      showNotification("Aucune donnée disponible pour le graphique", type = "warning")
      return(NULL)
    }
    
    # Calculer la valeur maximale de y pour positionner les lettres
    max_y <- max(agg$Moyenne + agg$Ecart_type, na.rm = TRUE)
    min_y <- min(agg$Moyenne - agg$Ecart_type, na.rm = TRUE)
    y_position_groups <- max_y + (max_y - min_y) * 0.05
    
    # Définir le titre de la légende
    legend_title <- ifelse(is.null(input$customLegendTitle) || input$customLegendTitle == "", 
                           "Groupes statistiques", input$customLegendTitle)
    
    # Créer le thème de base du graphique
    base_theme <- theme_minimal() +
      theme(
        plot.title = element_text(size = input$titleSize),
        axis.title = element_text(size = input$axisTitleSize),
        axis.text = element_text(size = input$axisTextSize),
        axis.text.x = if (input$rotateXLabels) element_text(angle = 45, hjust = 1) else element_text(),
        legend.position = if (input$colorByGroups) "right" else "none"
      )
    
    # Base labels
    base_labels <- labs(
      title = input$customTitle %||% paste("Comparaisons pour", resp_var, "par", fvar),
      x = input$customXLabel %||% fvar,
      y = input$customYLabel %||% resp_var
    )
    
    # Générer des graphiques selon le type
    if (input$plotType == "box") {
      # Calculer la position pour les lettres des groupes
      agg$y_position_groups <- max(plot_data[[resp_var]], na.rm = TRUE) + 
        (max(plot_data[[resp_var]], na.rm = TRUE) - 
           min(plot_data[[resp_var]], na.rm = TRUE)) * 0.05
      
      # Fusionner les informations de groupes avec plot_data pour la coloration
      if (input$colorByGroups) {
        plot_data_with_groups <- merge(plot_data, agg[, c(fvar, "groups")], by = fvar, all.x = TRUE)
        p <- ggplot(plot_data_with_groups, aes_string(x = fvar, y = resp_var, fill = "groups")) +
          geom_boxplot(alpha = 0.7) +
          scale_fill_discrete(name = legend_title) +
          theme(legend.position = "right")
      } else {
        p <- ggplot(plot_data, aes_string(x = fvar, y = resp_var, fill = fvar)) +
          geom_boxplot(alpha = 0.7) +
          theme(legend.position = "none")
      }
      
      p <- p + base_theme + base_labels
      
      # Ajouter les lettres de groupes uniquement si la coloration n’est pas faite par groupes
      if (!input$colorByGroups) {
        p <- p + geom_text(data = agg, 
                           aes_string(x = fvar, y = "y_position_groups", label = "groups"),
                           size = 5, fontface = "bold", color = "red", inherit.aes = FALSE)
      }
      
    } else if (input$plotType == "violin") {
      # Calculer la position pour les lettres des groupes
      agg$y_position_groups <- max(plot_data[[resp_var]], na.rm = TRUE) + 
        (max(plot_data[[resp_var]], na.rm = TRUE) - 
           min(plot_data[[resp_var]], na.rm = TRUE)) * 0.05
      
      # Fusionner les informations de groupe avec plot_data pour la coloration
      if (input$colorByGroups) {
        plot_data_with_groups <- merge(plot_data, agg[, c(fvar, "groups")], by = fvar, all.x = TRUE)
        p <- ggplot(plot_data_with_groups, aes_string(x = fvar, y = resp_var, fill = "groups")) +
          geom_violin(alpha = 0.7) +
          geom_boxplot(width = 0.1, alpha = 0.5, fill = "white") +
          scale_fill_discrete(name = legend_title) +
          theme(legend.position = "right")
      } else {
        p <- ggplot(plot_data, aes_string(x = fvar, y = resp_var, fill = fvar)) +
          geom_violin(alpha = 0.7) +
          geom_boxplot(width = 0.1, alpha = 0.5, fill = "white") +
          theme(legend.position = "none")
      }
      
      p <- p + base_theme + base_labels
      
      # Ajouter les lettres de groupes uniquement si la coloration n’est pas effectuée par groupes
      if (!input$colorByGroups) {
        p <- p + geom_text(data = agg, 
                           aes_string(x = fvar, y = "y_position_groups", label = "groups"),
                           size = 5, fontface = "bold", color = "red", inherit.aes = FALSE)
      }
      
    } else if (input$plotType == "point") {
      # Utiliser les moyennes issues des résultats du post-hoc
      if (input$colorByGroups) {
        p <- ggplot(agg, aes_string(x = fvar, y = "Moyenne", fill = "groups", color = "groups")) +
          geom_point(size = 4, shape = 21, stroke = 2) +
          scale_fill_discrete(name = legend_title) +
          scale_color_discrete(name = legend_title) +
          theme(legend.position = "right")
      } else {
        p <- ggplot(agg, aes_string(x = fvar, y = "Moyenne", fill = fvar, color = fvar)) +
          geom_point(size = 4, shape = 21, stroke = 2) +
          theme(legend.position = "none")
      }
      
      p <- p + base_theme + base_labels
      
      # Ajouter les barres d’erreur selon le choix
      if (input$errorType == "se") {
        p <- p + geom_errorbar(aes(ymin = Moyenne - Erreur_type, ymax = Moyenne + Erreur_type), 
                               width = 0.2, color = "black")
      } else if (input$errorType == "sd") {
        p <- p + geom_errorbar(aes(ymin = Moyenne - Ecart_type, ymax = Moyenne + Ecart_type), 
                               width = 0.2, color = "black")
      } else if (input$errorType == "ci") {
        ci_margin <- 1.96 * agg$Erreur_type  # 95% CI
        p <- p + geom_errorbar(aes(ymin = Moyenne - ci_margin, ymax = Moyenne + ci_margin), 
                               width = 0.2, color = "black")
      }
      
      # Ajouter les lettres de groupes uniquement si la coloration n’est pas effectuée par groupes
      if (!input$colorByGroups) {
        y_text_position <- max(agg$Moyenne + 
                                 if(input$errorType == "se") agg$Erreur_type 
                               else if(input$errorType == "sd") agg$Ecart_type 
                               else if(input$errorType == "ci") 1.96 * agg$Erreur_type
                               else 0, na.rm = TRUE) * 1.05
        
        agg$y_text_position <- y_text_position
        p <- p + geom_text(data = agg, aes_string(x = fvar, y = "y_text_position", label = "groups"),
                           size = 5, fontface = "bold", color = "red", inherit.aes = FALSE)
      }
      
    } else if (input$plotType == "hist") {
      # Utiliser les moyennes issues du post-hoc avec uniquement la barre d’erreur supérieure
      if (input$colorByGroups) {
        p <- ggplot(agg, aes_string(x = fvar, y = "Moyenne", fill = "groups")) +
          geom_col(alpha = 0.7, color = "black") +
          scale_fill_discrete(name = legend_title) +
          theme(legend.position = "right")
      } else {
        p <- ggplot(agg, aes_string(x = fvar, y = "Moyenne", fill = fvar)) +
          geom_col(alpha = 0.7, color = "black") +
          theme(legend.position = "none")
      }
      
      p <- p + base_theme + base_labels 
      
      # Ajouter uniquement la barre d’erreur supérieure
      if (input$errorType != "none") {
        if (input$errorType == "se") {
          p <- p + geom_errorbar(aes(ymin = Moyenne, ymax = Moyenne + Erreur_type), 
                                 width = 0.2, color = "black")
          y_text_pos <- max(agg$Moyenne + agg$Erreur_type, na.rm = TRUE) * 1.05
        } else if (input$errorType == "sd") {
          p <- p + geom_errorbar(aes(ymin = Moyenne, ymax = Moyenne + Ecart_type), 
                                 width = 0.2, color = "black")
          y_text_pos <- max(agg$Moyenne + agg$Ecart_type, na.rm = TRUE) * 1.05
        } else if (input$errorType == "ci") {
          ci_margin <- 1.96 * agg$Erreur_type
          p <- p + geom_errorbar(aes(ymin = Moyenne, ymax = Moyenne + ci_margin), 
                                 width = 0.2, color = "black")
          y_text_pos <- max(agg$Moyenne + ci_margin, na.rm = TRUE) * 1.05
        }
      } else {
        y_text_pos <- max(agg$Moyenne, na.rm = TRUE) * 1.05
      }
      
      # Ajouter les moyennes au centre des barres
      p <- p + geom_text(data = agg, 
                         aes_string(x = fvar, y = "Moyenne/2", label = "round(Moyenne, 2)"),
                         size = 4, fontface = "bold", color = "white", inherit.aes = FALSE)
      
      # Ajouter les grupes à 80% de la hauteur 
      if (!input$colorByGroups) {
        agg$y_text_pos_groups <- agg$Moyenne * 0.8
        p <- p + geom_text(data = agg, 
                           aes_string(x = fvar, y = "y_text_pos_groups", label = "groups"),
                           size = 5, fontface = "bold", color = "red", inherit.aes = FALSE)
      }
    }
    
    # Appliquer une échelle de couleurs si ce n’est pas la valeur par défaut 
    # et si la coloration n’est pas faite par groupes
    if (input$boxColor != "default" && !input$colorByGroups) {
      p <- p + scale_fill_brewer(palette = input$boxColor) +
        scale_color_brewer(palette = input$boxColor)
    }
    
    values$currentPlot <- p
    ggplotly(p) %>%
      layout(showlegend = if (input$colorByGroups) TRUE else FALSE)
  })
  
  # Gestionnaires de téléchargement
  output$downloadMultiResultsExcel <- downloadHandler(
    filename = function() { paste0("comparaisons_multiples_", Sys.Date(), ".xlsx") },
    content = function(file) {
      wb <- openxlsx::createWorkbook()
      for (i in seq_along(values$allPostHocResults)) {
        if (!is.null(values$allPostHocResults[[i]]) && nrow(values$allPostHocResults[[i]]) > 0) {
          sheet_name <- paste("Analyse", i)
          openxlsx::addWorksheet(wb, sheet_name)
          openxlsx::writeData(wb, sheet_name, values$allPostHocResults[[i]])
        }
      }
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  output$downloadMultiPlot <- downloadHandler(
    filename = function() { paste0("posthoc_plot_", Sys.Date(), ".png") },
    content = function(file) {
      if (!is.null(values$currentPlot)) {
        ggsave(file, plot = values$currentPlot, width = input$plotWidth, 
               height = input$plotHeight, dpi = input$plotDPI)
      }
    }
  )
  
  output$downloadPlot <- downloadHandler(
    filename = function() { paste0("posthoc_plot_", Sys.Date(), ".png") },
    content = function(file) {
      if (!is.null(values$currentPlot)) {
        ggsave(file, plot = values$currentPlot, width = input$plotWidth, 
               height = input$plotHeight, dpi = input$plotDPI)
      }
    }
  )
  
  output$downloadCurrentResults <- downloadHandler(
    filename = function() { 
      current_var_idx <- values$currentVarIndex %||% 1
      resp_var <- input$multiResponse[current_var_idx]
      paste0("posthoc_", resp_var, "_", Sys.Date(), ".xlsx") 
    },
    content = function(file) {
      req(values$multiResultsMain)
      current_var_idx <- values$currentVarIndex %||% 1
      resp_var <- input$multiResponse[current_var_idx]
      
      current_data <- values$multiResultsMain[values$multiResultsMain$Variable == resp_var, ]
      
      # Créer un workbook Excel
      wb <- openxlsx::createWorkbook()
      
      # Ajouter une feuille pour les effets principaux
      main_data <- current_data[current_data$Type == "main", ]
      if (nrow(main_data) > 0) {
        openxlsx::addWorksheet(wb, "Effets_principaux")
        openxlsx::writeData(wb, "Effets_principaux", main_data)
      }
      
      # Ajouter une feuille pour les interactions
      interaction_data <- current_data[current_data$Type == "interaction", ]
      if (nrow(interaction_data) > 0) {
        openxlsx::addWorksheet(wb, "Interactions")
        openxlsx::writeData(wb, "Interactions", interaction_data)
      }
      
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  output$downloadSummaryStats <- downloadHandler(
    filename = function() { paste0("statistiques_resumees_", Sys.Date(), ".xlsx") },
    content = function(file) {
      req(values$multiResultsMain)
      
      # Créer les statistiques résumées
      summary_stats <- values$multiResultsMain %>%
        group_by(Variable, Facteur, Type) %>%
        summarise(
          Nb_groupes = n(),
          Moyenne_generale = round(mean(Moyenne, na.rm = TRUE), 2),
          CV_moyen = round(mean(CV, na.rm = TRUE), 2),
          Ecart_type_moyen = round(mean(Ecart_type, na.rm = TRUE), 2),
          .groups = "drop"
        )
      
      # Statistiques par variable
      var_summary <- values$multiResultsMain %>%
        group_by(Variable) %>%
        summarise(
          Nb_facteurs_testes = n_distinct(Facteur),
          Nb_total_groupes = n(),
          Moyenne_min = round(min(Moyenne, na.rm = TRUE), 2),
          Moyenne_max = round(max(Moyenne, na.rm = TRUE), 2),
          CV_min = round(min(CV, na.rm = TRUE), 2),
          CV_max = round(max(CV, na.rm = TRUE), 2),
          .groups = "drop"
        )
      
      # Créer un workbook Excel
      wb <- openxlsx::createWorkbook()
      
      # Feuille 1: Résumé par facteur
      openxlsx::addWorksheet(wb, "Resume_par_facteur")
      openxlsx::writeData(wb, "Resume_par_facteur", summary_stats)
      
      # Feuille 2: Résumé par variable
      openxlsx::addWorksheet(wb, "Resume_par_variable")
      openxlsx::writeData(wb, "Resume_par_variable", var_summary)
      
      # Feuille 3: Données complètes
      openxlsx::addWorksheet(wb, "Donnees_completes")
      openxlsx::writeData(wb, "Donnees_completes", values$multiResultsMain)
      
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  # ---- Tableaux croisés dynamiques ----
  
  # Variables réactives pour les tableaux croisés
  crosstab_values <- reactiveValues(
    contingency_table = NULL,
    row_proportions = NULL,
    col_proportions = NULL,
    total_proportions = NULL,
    chi_test = NULL,
    fisher_test = NULL,
    residuals = NULL,
    current_plot = NULL,
    current_pie_plot = NULL
  )
  
  # UI pour sélection des variables
  output$crosstabRowVarSelect <- renderUI({
    req(values$filteredData)
    all_cols <- names(values$filteredData)
    selectInput("crosstabRowVar", "Variable en lignes :", choices = all_cols)
  })
  
  output$crosstabColVarSelect <- renderUI({
    req(values$filteredData)
    all_cols <- names(values$filteredData)
    selectInput("crosstabColVar", "Variable en colonnes :", choices = all_cols)
  })
  
  output$crosstabFilterVarSelect <- renderUI({
    req(values$filteredData)
    fac_cols <- names(values$filteredData)[sapply(values$filteredData, is.factor)]
    selectInput("crosstabFilterVar", "Filtrer par (optionnel) :", 
                choices = c("Aucun" = "", fac_cols))
  })
  
  # Génération des analyses
  observeEvent(input$generateCrosstab, {
    req(input$crosstabRowVar, input$crosstabColVar)
    
    showNotification("Génération des analyses en cours...", type = "message", duration = 2)
    
    tryCatch({
      # Préparer les données
      df <- values$filteredData
      
      # Appliquer le filtre si sélectionné
      if (!is.null(input$crosstabFilterVar) && input$crosstabFilterVar != "") {
        # Pour cet exemple, on prend toutes les données, mais on pourrait ajouter une UI pour sélectionner les niveaux
        df <- df[!is.na(df[[input$crosstabFilterVar]]), ]
      }
      
      # Nettoyer les données
      df <- df[!is.na(df[[input$crosstabRowVar]]) & !is.na(df[[input$crosstabColVar]]), ]
      
      # Convertir en facteurs si nécessaire
      if (!is.factor(df[[input$crosstabRowVar]])) {
        df[[input$crosstabRowVar]] <- as.factor(df[[input$crosstabRowVar]])
      }
      if (!is.factor(df[[input$crosstabColVar]])) {
        df[[input$crosstabColVar]] <- as.factor(df[[input$crosstabColVar]])
      }
      
      # Tableau de contingence
      contingency_table <- table(df[[input$crosstabRowVar]], df[[input$crosstabColVar]])
      crosstab_values$contingency_table <- addmargins(contingency_table)
      
      # Proportions
      if ("row_prop" %in% input$analysisOptions) {
        row_prop <- prop.table(contingency_table, margin = 1) * 100
        crosstab_values$row_proportions <- addmargins(row_prop, margin = 2)
      }
      
      if ("col_prop" %in% input$analysisOptions) {
        col_prop <- prop.table(contingency_table, margin = 2) * 100  
        crosstab_values$col_proportions <- addmargins(col_prop, margin = 1)
      }
      
      if ("total_prop" %in% input$analysisOptions) {
        total_prop <- prop.table(contingency_table) * 100
        crosstab_values$total_proportions <- addmargins(total_prop)
      }
      
      # Tests statistiques
      if ("chi_test" %in% input$analysisOptions) {
        if (all(contingency_table >= 5)) {
          crosstab_values$chi_test <- chisq.test(contingency_table)
        } else {
          crosstab_values$chi_test <- "Conditions non remplies (effectifs < 5)"
        }
      }
      
      if ("fisher_test" %in% input$analysisOptions) {
        if (min(dim(contingency_table)) == 2) {
          crosstab_values$fisher_test <- fisher.test(contingency_table)
        } else {
          crosstab_values$fisher_test <- "Test de Fisher disponible uniquement pour tableaux 2x2"
        }
      }
      
      # Résidus standardisés
      if ("residuals" %in% input$analysisOptions) {
        if (is.list(crosstab_values$chi_test)) {
          residuals_std <- crosstab_values$chi_test$stdres
          crosstab_values$residuals <- residuals_std
        }
      }
      
      showNotification("Analyses générées avec succès!", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Erreur lors de l'analyse :", e$message), type = "error")
    })
  })
  
  # Affichage des tableaux
  output$crosstabTable <- renderDT({
    req(crosstab_values$contingency_table)
    datatable(as.data.frame.matrix(crosstab_values$contingency_table), 
              options = list(scrollX = TRUE, pageLength = -1),
              caption = "Tableau de contingence (effectifs)")
  })
  
  output$crosstabRowProp <- renderDT({
    req(crosstab_values$row_proportions)
    datatable(round(as.data.frame.matrix(crosstab_values$row_proportions), 2), 
              options = list(scrollX = TRUE, pageLength = -1),
              caption = "Proportions en lignes (%)")
  })
  
  output$crosstabColProp <- renderDT({
    req(crosstab_values$col_proportions)
    datatable(round(as.data.frame.matrix(crosstab_values$col_proportions), 2), 
              options = list(scrollX = TRUE, pageLength = -1),
              caption = "Proportions en colonnes (%)")
  })
  
  output$crosstabTotalProp <- renderDT({
    req(crosstab_values$total_proportions)
    datatable(round(as.data.frame.matrix(crosstab_values$total_proportions), 2), 
              options = list(scrollX = TRUE, pageLength = -1),
              caption = "Proportions totales (%)")
  })
  
  output$crosstabTests <- renderPrint({
    tests_results <- list()
    
    if (!is.null(crosstab_values$chi_test)) {
      if (is.list(crosstab_values$chi_test)) {
        cat("=== TEST DU CHI-DEUX ===\n")
        cat("Statistique X² :", round(crosstab_values$chi_test$statistic, 4), "\n")
        cat("Degrés de liberté :", crosstab_values$chi_test$parameter, "\n") 
        cat("p-value :", format.pval(crosstab_values$chi_test$p.value), "\n")
        cat("Interprétation :", ifelse(crosstab_values$chi_test$p.value < 0.05, 
                                       "Association significative", 
                                       "Pas d'association significative"), "\n\n")
      } else {
        cat("=== TEST DU CHI-DEUX ===\n")
        cat(crosstab_values$chi_test, "\n\n")
      }
    }
    
    if (!is.null(crosstab_values$fisher_test)) {
      if (is.list(crosstab_values$fisher_test)) {
        cat("=== TEST EXACT DE FISHER ===\n")
        cat("p-value :", format.pval(crosstab_values$fisher_test$p.value), "\n")
        cat("Interprétation :", ifelse(crosstab_values$fisher_test$p.value < 0.05, 
                                       "Association significative", 
                                       "Pas d'association significative"), "\n\n")
      } else {
        cat("=== TEST EXACT DE FISHER ===\n")
        cat(crosstab_values$fisher_test, "\n\n")
      }
    }
  })
  
  output$crosstabResiduals <- renderDT({
    req(crosstab_values$residuals)
    datatable(round(as.data.frame.matrix(crosstab_values$residuals), 2), 
              options = list(scrollX = TRUE, pageLength = -1),
              caption = "Résidus standardisés") %>%
      formatStyle(names(as.data.frame.matrix(crosstab_values$residuals)),
                  backgroundColor = styleInterval(c(-2, 2), c("lightblue", "white", "lightcoral")))
  })
  
  # Graphiques
  output$crosstabPlot <- renderPlot({
    req(crosstab_values$contingency_table, input$crosstabRowVar, input$crosstabColVar)
    
    # Préparer les données pour ggplot
    df_plot <- as.data.frame(crosstab_values$contingency_table)
    df_plot <- df_plot[df_plot$Var1 != "Sum" & df_plot$Var2 != "Sum", ]
    names(df_plot) <- c("Row_Var", "Col_Var", "Freq")
    
    # Titre
    title <- if (!is.null(input$crosstabTitle) && input$crosstabTitle != "") {
      input$crosstabTitle
    } else {
      paste("Tableau croisé :", input$crosstabRowVar, "vs", input$crosstabColVar)
    }
    
    # Labels
    x_label <- if (!is.null(input$crosstabXLabel) && input$crosstabXLabel != "") {
      input$crosstabXLabel  
    } else {
      input$crosstabRowVar
    }
    
    y_label <- if (!is.null(input$crosstabYLabel) && input$crosstabYLabel != "") {
      input$crosstabYLabel
    } else {
      "Effectifs"
    }
    
    if (input$plotType == "bar") {
      p <- ggplot(df_plot, aes(x = Row_Var, y = Freq, fill = Col_Var)) +
        geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
        labs(title = title, x = x_label, y = y_label, fill = input$crosstabColVar) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = input$titleSize, hjust = 0.5),
          axis.text = element_text(size = input$axisTextSize),
          legend.text = element_text(size = input$legendTextSize),
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
      
      if (input$showPercentages) {
        p <- p + geom_text(aes(label = Freq), position = position_dodge(0.9), vjust = -0.5)
      }
      
    } else if (input$plotType == "stacked_bar") {
      p <- ggplot(df_plot, aes(x = Row_Var, y = Freq, fill = Col_Var)) +
        geom_bar(stat = "identity", position = "stack", alpha = 0.8) +
        labs(title = title, x = x_label, y = y_label, fill = input$crosstabColVar) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = input$titleSize, hjust = 0.5),
          axis.text = element_text(size = input$axisTextSize),
          legend.text = element_text(size = input$legendTextSize),
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
      
      if (input$showPercentages) {
        p <- p + geom_text(aes(label = Freq), position = position_stack(vjust = 0.5))
      }
      
    } else if (input$plotType == "mosaic") {
      # Graphique mosaïque simplifié
      p <- ggplot(df_plot, aes(x = Row_Var, y = Freq, fill = Col_Var)) +
        geom_bar(stat = "identity", position = "fill", alpha = 0.8) +
        labs(title = title, x = x_label, y = "Proportions", fill = input$crosstabColVar) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = input$titleSize, hjust = 0.5),
          axis.text = element_text(size = input$axisTextSize),
          legend.text = element_text(size = input$legendTextSize),
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
    }
    
    crosstab_values$current_plot <- p
    print(p)
  })
  
  output$crosstabPiePlot <- renderPlot({
    req(crosstab_values$contingency_table, input$pieVariable)
    
    if (input$pieVariable == "row") {
      # Sommes par ligne
      pie_data <- rowSums(crosstab_values$contingency_table)
      pie_data <- pie_data[names(pie_data) != "Sum"]
      var_name <- input$crosstabRowVar
    } else {
      # Sommes par colonne  
      pie_data <- colSums(crosstab_values$contingency_table)
      pie_data <- pie_data[names(pie_data) != "Sum"]
      var_name <- input$crosstabColVar
    }
    
    df_pie <- data.frame(
      Category = names(pie_data),
      Count = as.numeric(pie_data)
    )
    df_pie$Percentage <- round(df_pie$Count / sum(df_pie$Count) * 100, 1)
    
    p <- ggplot(df_pie, aes(x = "", y = Count, fill = Category)) +
      geom_bar(stat = "identity", width = 1, alpha = 0.8) +
      coord_polar(theta = "y") +
      labs(title = paste("Répartition de", var_name), fill = var_name) +
      theme_void() +
      theme(
        plot.title = element_text(size = input$titleSize, hjust = 0.5),
        legend.text = element_text(size = input$legendTextSize)
      )
    
    if (input$showPercentages) {
      p <- p + geom_text(aes(label = paste0(Percentage, "%")), 
                         position = position_stack(vjust = 0.5))
    }
    
    crosstab_values$current_pie_plot <- p
    print(p)
  })
  
  # Téléchargements
  output$downloadCrosstab <- downloadHandler(
    filename = function() paste0("tableau_croise_", Sys.Date(), ".xlsx"),
    content = function(file) {
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Effectifs")
      openxlsx::writeData(wb, "Effectifs", as.data.frame.matrix(crosstab_values$contingency_table), rowNames = TRUE)
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  output$downloadRowProp <- downloadHandler(
    filename = function() paste0("proportions_lignes_", Sys.Date(), ".xlsx"),
    content = function(file) {
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Prop_Lignes")
      openxlsx::writeData(wb, "Prop_Lignes", round(as.data.frame.matrix(crosstab_values$row_proportions), 2), rowNames = TRUE)
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  output$downloadColProp <- downloadHandler(
    filename = function() paste0("proportions_colonnes_", Sys.Date(), ".xlsx"),
    content = function(file) {
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Prop_Colonnes")
      openxlsx::writeData(wb, "Prop_Colonnes", round(as.data.frame.matrix(crosstab_values$col_proportions), 2), rowNames = TRUE)
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  output$downloadTotalProp <- downloadHandler(
    filename = function() paste0("proportions_totales_", Sys.Date(), ".xlsx"),
    content = function(file) {
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Prop_Totales")
      openxlsx::writeData(wb, "Prop_Totales", round(as.data.frame.matrix(crosstab_values$total_proportions), 2), rowNames = TRUE)
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  output$downloadTests <- downloadHandler(
    filename = function() paste0("tests_statistiques_", Sys.Date(), ".xlsx"),
    content = function(file) {
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Tests")
      
      tests_df <- data.frame(
        Test = character(),
        Statistique = numeric(),
        p_value = numeric(),
        Interpretation = character(),
        stringsAsFactors = FALSE
      )
      
      if (!is.null(crosstab_values$chi_test) && is.list(crosstab_values$chi_test)) {
        tests_df <- rbind(tests_df, data.frame(
          Test = "Chi-deux",
          Statistique = crosstab_values$chi_test$statistic,
          p_value = crosstab_values$chi_test$p.value,
          Interpretation = ifelse(crosstab_values$chi_test$p.value < 0.05, "Significatif", "Non significatif")
        ))
      }
      
      if (!is.null(crosstab_values$fisher_test) && is.list(crosstab_values$fisher_test)) {
        tests_df <- rbind(tests_df, data.frame(
          Test = "Fisher exact",
          Statistique = NA,
          p_value = crosstab_values$fisher_test$p.value,
          Interpretation = ifelse(crosstab_values$fisher_test$p.value < 0.05, "Significatif", "Non significatif")
        ))
      }
      
      openxlsx::writeData(wb, "Tests", tests_df)
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  output$downloadResiduals <- downloadHandler(
    filename = function() paste0("residus_", Sys.Date(), ".xlsx"),
    content = function(file) {
      req(crosstab_values$residuals)
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Residus")
      openxlsx::writeData(wb, "Residus", round(as.data.frame.matrix(crosstab_values$residuals), 2), rowNames = TRUE)
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  output$downloadPlot <- downloadHandler(
    filename = function() paste0("graphique_croise_", Sys.Date(), ".png"),
    content = function(file) {
      req(crosstab_values$current_plot)
      ggsave(file, plot = crosstab_values$current_plot, 
             width = input$plotWidth, height = input$plotHeight, dpi = 300)
    }
  )
  
  output$downloadPiePlot <- downloadHandler(
    filename = function() paste0("graphique_secteurs_", Sys.Date(), ".png"),
    content = function(file) {
      req(crosstab_values$current_pie_plot)
      ggsave(file, plot = crosstab_values$current_pie_plot, 
             width = input$plotWidth, height = input$plotHeight, dpi = 300)
    }
  )
  # ---- Visualisation des données ----
  output$vizXVarSelect <- renderUI({
    req(values$filteredData)
    all_cols <- names(values$filteredData)
    selectInput("vizXVar", "Variable X:", choices = all_cols)
  })
  
  output$vizYVarSelect <- renderUI({
    req(values$filteredData)
    all_cols <- names(values$filteredData)
    selectInput("vizYVar", "Variable Y:", choices = all_cols)
  })
  
  output$vizColorVarSelect <- renderUI({
    req(values$filteredData)
    all_cols <- names(values$filteredData)
    selectInput("vizColorVar", "Variable de couleur (optionnel):", choices = c("Aucun", all_cols))
  })
  
  output$vizFacetVarSelect <- renderUI({
    req(values$filteredData)
    fac_cols <- names(values$filteredData)[sapply(values$filteredData, is.factor)]
    selectInput("vizFacetVar", "Variable de facetting (optionnel):", choices = c("Aucun", fac_cols))
  })
  
  observeEvent(input$generateViz, {
    req(values$filteredData, input$vizXVar, input$vizYVar)
    
    x_var <- input$vizXVar
    y_var <- input$vizYVar
    color_var <- if (input$vizColorVar != "Aucun") input$vizColorVar else NULL
    facet_var <- if (input$vizFacetVar != "Aucun") input$vizFacetVar else NULL
    
    p <- ggplot(values$filteredData, aes_string(x = x_var, y = y_var)) +
      theme(axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black"))
    
    if (input$vizType == "scatter") {
      if (!is.null(color_var)) {
        p <- p + geom_point(aes_string(color = color_var), alpha = input$plotAlpha, size = input$plotSize)
      } else {
        p <- p + geom_point(alpha = input$plotAlpha, size = input$plotSize)
      }
    } else if (input$vizType == "box") {
      if (!is.null(color_var)) {
        p <- p + geom_boxplot(aes_string(fill = color_var), alpha = input$plotAlpha)
      } else {
        p <- p + geom_boxplot(alpha = input$plotAlpha)
      }
    } else if (input$vizType == "violin") {
      if (!is.null(color_var)) {
        p <- p + geom_violin(aes_string(fill = color_var), alpha = input$plotAlpha)
      } else {
        p <- p + geom_violin(alpha = input$plotAlpha)
      }
    } else if (input$vizType == "bar") {
      if (!is.null(color_var)) {
        p <- p + geom_bar(aes_string(fill = color_var), stat = "identity", alpha = input$plotAlpha)
      } else {
        p <- p + geom_bar(stat = "identity", alpha = input$plotAlpha)
      }
    } else if (input$vizType == "line") {
      if (!is.null(color_var)) {
        p <- p + geom_line(aes_string(color = color_var), alpha = input$plotAlpha, size = input$plotSize)
      } else {
        p <- p + geom_line(alpha = input$plotAlpha, size = input$plotSize)
      }
    } else if (input$vizType == "density") {
      if (!is.null(color_var)) {
        p <- p + geom_density(aes_string(fill = color_var), alpha = input$plotAlpha)
      } else {
        p <- p + geom_density(alpha = input$plotAlpha)
      }
    } else if (input$vizType == "histogram") {
      if (!is.null(color_var)) {
        p <- p + geom_histogram(aes_string(fill = color_var), alpha = input$plotAlpha, bins = 30)
      } else {
        p <- p + geom_histogram(alpha = input$plotAlpha, bins = 30)
      }
    } else if (input$vizType == "heatmap") {
      heatmap_data <- values$filteredData %>%
        group_by(across(c(x_var, y_var))) %>%
        summarise(Count = n(), .groups = "drop")
      
      p <- ggplot(heatmap_data, aes_string(x = x_var, y = y_var, fill = "Count")) +
        geom_tile() +
        scale_fill_gradient(low = "white", high = "red")
    }
    
    if (!is.null(facet_var)) {
      p <- p + facet_wrap(as.formula(paste("~", facet_var)))
    }
    
    if (input$plotTheme == "classic") {
      p <- p + theme_classic()
    } else if (input$plotTheme == "minimal") {
      p <- p + theme_minimal()
    } else if (input$plotTheme == "gray") {
      p <- p + theme_gray()
    } else if (input$plotTheme == "dark") {
      p <- p + theme_dark()
    } else if (input$plotTheme == "linedraw") {
      p <- p + theme_linedraw()
    }
    
    if (input$plotTitle != "") {
      p <- p + labs(title = input$plotTitle)
    }
    if (input$plotXLab != "") {
      p <- p + labs(x = input$plotXLab)
    }
    if (input$plotYLab != "") {
      p <- p + labs(y = input$plotYLab)
    }
    
    if (!is.null(color_var) && input$plotPalette != "default") {
      p <- p + scale_fill_brewer(palette = input$plotPalette) +
        scale_color_brewer(palette = input$plotPalette)
    }
    
    values$currentInteractivePlot <- p
  })
  
  output$advancedPlot <- renderPlotly({
    req(values$currentInteractivePlot)
    ggplotly(values$currentInteractivePlot, width = input$plotWidthInteractive, height = input$plotHeightInteractive)
  })
  
  output$downloadInteractivePlot <- downloadHandler(
    filename = function() {
      paste0("visualisation_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(values$currentInteractivePlot)
      ggsave(file, plot = values$currentInteractivePlot,
             width = input$plotWidthInteractive/100, height = input$plotHeightInteractive/100,
             dpi = 2000, units = "in", device = "png")
    }
  )
  
  # ---- Rapport ----
  observeEvent(input$generateReport, {
    showNotification("Génération du rapport en cours...", type = "message")
    
    report_path <- tempfile(fileext = ".Rmd")
    
    writeLines(c(
      "---",
      paste0("title: \"", input$reportTitle, "\""),
      paste0("author: \"", input$reportAuthor, "\""),
      paste0("date: \"", Sys.Date(), "\""),
      paste0("output: ", input$reportFormat),
      "---",
      "",
      "```{r setup, include=FALSE}",
      "knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)",
      "```",
      "",
      "# Analyse Statistique des Données",
      "",
      "## Résumé des données",
      "```{r data-summary}",
      "summary(values$filteredData)",
      "```",
      "",
      "## Analyses descriptives",
      "```{r descriptive}",
      "if (!is.null(values$descStats)) {",
      "  knitr::kable(values$descStats, caption = \"Statistiques descriptives\")",
      "}",
      "```",
      "",
      "## Tests statistiques",
      "```{r tests}",
      "if (!is.null(values$testResultsDF)) {",
      "  knitr::kable(values$testResultsDF, caption = \"Résultats des tests\")",
      "}",
      "```",
      "",
      "## Visualisations",
      "```{r plots, fig.cap=\"Visualisations des résultats\"}",
      "if (!is.null(values$currentPlot)) {",
      "  print(values$currentPlot)",
      "}",
      "```"
    ), report_path)
    
    output_file <- rmarkdown::render(report_path, switch(
      input$reportFormat,
      "html_document" = rmarkdown::html_document(),
      "pdf_document" = rmarkdown::pdf_document(),
      "word_document" = rmarkdown::word_document()
    ))
    
    output$reportPreview <- renderUI({
      if (input$reportFormat == "html_document") {
        includeHTML(output_file)
      } else {
        tags$iframe(src = output_file, width = "100%", height = "600px")
      }
    })
    
    output$downloadReport <- downloadHandler(
      filename = function() {
        paste0("rapport_", Sys.Date(), switch(
          input$reportFormat,
          "html_document" = ".html",
          "pdf_document" = ".pdf",
          "word_document" = ".docx"
        ))
      },
      content = function(file) {
        file.copy(output_file, file)
      }
    )
    
    showNotification("Rapport généré avec succès!", type = "message")
  })
}


shinyApp(ui, server)