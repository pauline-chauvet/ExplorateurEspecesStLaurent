#####################################
# Liste des espèces potentiellement #
# observables dans et au abords du  #
# Saint-Laurent                     #
# par Pauline Chauvet-OGSL          #
# ui.R file                         #
#####################################
#rsconnect::deployApp('C:/Users/Cédric/Documents/gitlab.com')
# UI = portion du code qui contient les éléments graphique de l'interface de l'application Shiny : 
## 1-les éléments de mise en page (titre, nombre et taille des éléments, boutons et sliders, etc.). 
## 2-les composantes de l'application : des objets qui communiquent avec la partie serveur, 
## pour venir afficher ces éléments (tableaux, dataviz, code.) dans votre interface graphique.. 


library(collapsibleTree)
library(DT)
library(dplyr)
library(ggplot2)
library(leaflet)
library(shiny)
library(shinycssloaders)
library(shinydashboard)
library(shinydashboardPlus)
library(tidyr)
library(udunits2)

# Définition de l'interface utilisateur de l'application 
shinyUI(fluidPage(
    # charger la customisation css
    includeCSS("www/custom.css"),
    
    # charger le fond de page
    dashboardPage(
        #Couleur de la page
        skin = "blue",
        #Titre
        dashboardHeader(disable=T),
    
        #Bar de menu gauche
        dashboardSidebar(
            sidebarMenu(
                # Logo OGSL
                HTML("<a href='https://ogsl.ca/fr/accueil/' target='_blank'><img style = 'display: block; margin-left: 1vw; margin-right: 1vw;padding-top:3.5vw;padding-bottom:4vw;text-align: center;width: 13vw' src='OGSL_Logo.svg' ></a>"),
                # Différents Items dans la barre du menu
                menuItem("Accueil", tabName = "Accueil", icon = icon("home", lib = "font-awesome", style="font-size: 1.2vw;padding-right:1.5vw")),
                menuItem("Arbre taxonomique", tabName = "table_tree", icon = icon("sitemap", lib = "font-awesome", style="font-size: 1.2vw;padding-right:1.5vw")),
                menuItem("Table générale", tabName = "table", icon = icon("table", lib = "font-awesome", style="font-size: 1.2vw;padding-right:1.5vw")),
                menuItem("Statistiques", tabName = "graphes", icon = icon("chart-pie", style="font-size: 1.2vw;padding-right:1.5vw")),
                menuItem("Sources", tabName = "source", icon = icon("leanpub",lib = "font-awesome", style="font-size: 1.2vw;padding-right:0vw")),
                # Liens vers fb, twitter et site de l'OGSL
                HTML(paste0(
                    "<br><br><br><br><br><br><br><br><br><br><br><br><br><br>",
                    "<table style='margin-left:auto; margin-right:auto;'>",
                    "<tr>",
                    "<td style='padding: 5px;'><a href='https://www.facebook.com/ogsl.slgo/' target='_blank'><i class='fab fa-facebook fa-lg'></i></a></td>",
                    "<td style='padding: 5px;'><a href='https://twitter.com/OGSL_SLGO' target='_blank'><i class='fab fa-twitter fa-lg'></i></a></td>",
                    "<td style='padding: 5px;'><a href='https://fr.linkedin.com/company/observatoire-global-du-saint-laurent---ogsl' target='_blank'><i class='fab fa-linkedin fa-lg'></i></a></td>",
                    "</tr>",
                    "</table>")
                ),
                HTML(paste0(
                        "<script>",
                        "var today = new Date();",
                        "var yyyy = today.getFullYear();",
                        "</script>",
                        "<p style = 'text-align: center;font-size: 1vw;fontcolor='#8cc63e';'>  <a href='https://ogsl.ca/fr/accueil/' target='_blank'>&copy - ogsl.ca - 2022</a></p>")
                )
            )# end sidebarmenu
        ), # end dashboardSidebar
            
        dashboardBody(
            tags$head(tags$link(rel = "shortcut icon", href = "O_Favicon.ico")),
            tabItems(
                # PAGE D'ACCUEIL            
                tabItem(tabName = "Accueil",
                    fluidRow(column(12,
                    fluidRow(style="padding-top:4.5vw;text-align: center;font-size: 4vw;line-height: 4.2vw","Explorateur des espèces",fluidRow("du Saint-Laurent")),
                    fluidRow(style="padding-top:50px;text-align: center;font-size: 1.35vw","L'application de visualisation dynamique de la liste des espèces marines recensées dans le Saint-Laurent."),
                    fluidRow(HTML("<a href='https://ogsl.ca/fr/accueil/' target='_blank'><img style = 'display: block; margin-left: 15vw; margin-right: 1vw;padding-top:1vw;padding-bottom:1vw;text-align: center;width: 50vw' src='illustration_accueil2.png' ></img></a>")),
                    fluidRow(style='padding-left:10vw; padding-right:10vw;',
                        fluidRow(style='padding-left:2vw; padding-right:2vw; padding-top:2vw; padding-bottom:0px',includeMarkdown("www/Pourquoi.md")),
                        fluidRow(style="padding-left:2vw; padding-right:2vw; padding-top:2vw; padding-bottom:0px",includeMarkdown("www/Comment.md")),
                        fluidRow(style='padding-left:2vw; padding-right:2vw; padding-top:2vw; padding-bottom:0px; text-align:justify;font-size: 1.2vw',includeMarkdown("www/Accueil2.MD")))))
                ),

                # CHOIX DES CONDITIONS + AFFICHAGE ARBRE 
                tabItem(tabName = "table_tree",
                    fluidRow(style='padding-left:10vw; padding-right:15vw; padding-top:2vw; padding-bottom:0px;height:20vw;font-size: 1vw',includeMarkdown("www/tree.md")),
                    ##choix rang
                    fluidRow(style='padding-left:2vw; padding-right:0px; padding-top:2vw; padding-bottom:0px;height:50vw',
                            #choix type de rang
                            column(3,style="height:50vw; padding-top:5vw",
                                fluidRow(uiOutput("SelectRang"),div(style = "height:2vw")),
                            #choix conditionne par type de rang     
                                fluidRow(style="height:2vw",conditionalPanel(condition = "input.rang == 'Règne'",
                                    uiOutput("SelectRegne"),div(style = "height:2vw"))),
                                fluidRow(style="height:2vw",conditionalPanel(condition = "input.rang == 'Embranchement'",
                                    uiOutput("SelectEmbranchement"),div(style = "height:2vw"))),
                                fluidRow(style="height:2vw",conditionalPanel(condition = "input.rang == 'Classe'",
                                    uiOutput("SelectClasse"),div(style = "height:2vw"))),
                                fluidRow(style="height:2vw",conditionalPanel(condition = "input.rang == 'Ordre'",
                                    uiOutput("SelectOrdre"),div(style = "height:2vw")))
                            ),
                            conditionalPanel(condition = "input.rang == 'Règne'",column(9,style="height:50vw" ,collapsibleTreeOutput("tree_R",height = 900)%>% withSpinner(color="#1e4659", color.background="#ffffff", size=2,type=2)
                            )),
                            conditionalPanel(condition = "input.rang == 'Embranchement'",column(9,style="height:50vw" ,collapsibleTreeOutput("tree_E",height = 900)%>% withSpinner(color="#1e4659", color.background="#ffffff", size=2,type=2)
                            )),
                            conditionalPanel(condition = "input.rang == 'Classe'",column(9,style="height:50vw" ,collapsibleTreeOutput("tree_C",height = 900)%>% withSpinner(color="#1e4659", color.background="#ffffff", size=2,type=2)
                            )),
                            conditionalPanel(condition = "input.rang == 'Ordre'",column(9,style="height:50vw" ,collapsibleTreeOutput("tree_O",height = 900)%>% withSpinner(color="#1e4659", color.background="#ffffff", size=2,type=2)
                            ))
                            ),
                ),#end tabitem(table_tree)
            
                # TABLE GENERALE TELECHARGEABLE
                tabItem(tabName = "table",
                        fluidRow(style='padding-left:10vw; padding-right:20vw; padding-top:2vw; padding-bottom:0px;font-size: 1vw',includeMarkdown("www/table.md")),
                        fluidRow(style='padding-left:20vw; padding-right:20vw; padding-top:2vw; padding-bottom:0px',downloadButton('download',"Téléchargez la table")),
                        fluidRow(style='padding-left:1vw; padding-right:3vw; padding-top:2vw; padding-bottom:0px;text-align:start;font-size: 0.7vw',box(dataTableOutput("table_complete"),width=12))
                ),
                # GRAPHIQUES
                tabItem(tabName = "graphes",
                        fluidRow(style='padding-left:10vw; padding-right:15vw; padding-top:2vw; padding-bottom:0px;height:20vw;font-size: 1vw',includeMarkdown("www/Graph.md")),
                        fluidRow(style='padding-left:10vw; padding-right:10vw; padding-top:2vw; padding-bottom:0px;text-align:center;font-size: 1vw', plotOutput("plot_nbsp_regne")),
                        fluidRow(style='padding-left:10vw; padding-right:10vw; padding-top:2vw; padding-bottom:0px;text-align:center;font-size: 1vw', plotOutput("plot_nbsp_noANIM")),
                        fluidRow(style='height:100vw;padding-left:15vw; padding-right:15vw; padding-top:4vw; padding-bottom:0px;text-align:center;font-size: 1vw', plotOutput("plot_nbsp_emb_anim",width = "50vw" ,height ="30vw" ))
                ),
                # SOURCES
                tabItem(tabName = "source",
                        fluidRow(style='padding-left:10vw; padding-right:15vw; padding-top:2vw; padding-bottom:0px;text-align:justify;font-size: 1vw',includeMarkdown("www/Source.md"))
                )        
            ) #end tabItems
        ) # end dashboardBody
    )# end dashboardPage
)#end fluidpage
)#end shinyUI
