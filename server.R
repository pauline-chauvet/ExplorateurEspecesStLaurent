#####################################
# Liste des espèces potentiellement #
# observables dans et au abords du  #
# Saint-Laurent                     #
# par Pauline Chauvet-OGSL          #
# server.R file                     #
#####################################

# Contient tout le code R qui s'exécute avant tout affichage de l'application.
# Autrement dit, c'est lui qui crée les éléments qui sont affichés dans l'interface.
# C'est dans cette partie que sont appelées les composantes.

library(shiny)

#Lire la table
species<-read.table("www/table_sp.txt",header=T,sep=";",encoding = 'UTF-8')

    
# Définir le serveur logique requis pour dessiner l'arbre et sortir une table
server<- shinyServer(function(input, output) {
    
    # Définir la table a montrer
    speciesR<-reactive({species})
    output$table_complete <- renderDataTable(species[,-13:-14], selection = list(target = 'column'))
    
    #Définir les choix
    output$SelectRang <- renderUI({selectInput(inputId = "rang", 
                                                label = "Quel rang souhaitez-vous explorer ?", 
                                                choice = c('Règne','Embranchement', 'Classe', 'Ordre')
    )})
    
    output$SelectRegne <- renderUI({conditionalPanel(condition = "input.rang == 'Règne'",selectInput(inputId = "groupe", 
                                                label = "Quel règne souhaitez-vous explorer ?", 
                                                choice = c("Animalia","Chromista","Plantae" )
    ))})
    
    output$SelectEmbranchement <- renderUI({conditionalPanel(condition = "input.rang == 'Embranchement'",selectInput(inputId = "groupe2", 
                                                        label = "Quel embranchement-souhaitez vous explorer ?", 
                                                        choice = c("Acanthocephala","Annelida","Arthropoda","Brachiopoda","Bryozoa","Chaetognatha",   
                                                                   "Chlorophyta","Chordata","Cnidaria","Ctenophora","Echinodermata","Entoprocta",     
                                                                   "Hemichordata","Mollusca","Nematoda","Nemertea","Phoronida","Platyhelminthes",
                                                                   "Porifera","Priapulida","Sipuncula","Tracheophyta","Ochrophyta","Rhodophyta" ))
    )})
    
    output$SelectClasse <- renderUI({conditionalPanel(condition = "input.rang == 'Classe'",selectInput(inputId = "groupe3", 
                                                 label = "Quelle classe souhaitez-vous explorer ?", 
                                                 choice = c("Eoacanthocephala","Palaeacanthocephala","Clitellata","Polychaeta",
                                                            "Arachnida","Branchiopoda","Collembola","Hexanauplia","Ichthyostraca",
                                                            "Insecta","Malacostraca","Maxillopoda","Merostomata","Ostracoda",
                                                            "Pycnogonida","Thecostraca","Rhynchonellata","Gymnolaemata","Stenolaemata",
                                                            "Sagittoidea","Ulvophyceae","Actinopteri","Appendicularia","Ascidiacea",
                                                            "Aves","Elasmobranchii","Holocephali","Mammalia","Myxini",
                                                            "Petromyzonti","Anthozoa","Hydrozoa","Scyphozoa",
                                                            "Staurozoa","Nuda","Tentaculata","Asteroidea","Crinoidea",
                                                            "Echinoidea","Holothuroidea","Ophiuroidea","Enteropneusta",
                                                            "Bivalvia","Caudofoveata","Cephalopoda","Gastropoda","Polyplacophora",
                                                            "Scaphopoda","Chromadorea","Enoplea","Hoplonemertea","Palaeonemertea",
                                                            "Pilidiophora","Cestoda","Monogenea","Trematoda","Calcarea","Demospongiae",
                                                            "Hexactinellida","Sipunculidea","Magnoliopsida","Bacillariophyceae","Phaeophyceae","Florideophyceae" )
    ))})
    
    output$SelectOrdre <- renderUI({conditionalPanel(condition = "input.rang == 'Ordre'",selectInput(inputId = "groupe4", 
                                                label = "Quelle ordre souhaitez-vous explorer ? (200+ choix)", 
                                                choice = c("Acanthuriformes","Acipenseriformes",
                                                           "Acropomatiformes","Albuliformes","Alepocephaliformes","Amiiformes","Anguilliformes","Argentiniformes",
                                                           "Atheriniformes","Aulopiformes","Batrachoidiformes","Beloniformes","Beryciformes","Blenniiformes",
                                                           "Callionymiformes","Carangariaincertaesedis","Carangiformes","Centrarchiformes","Clupeiformes","Cypriniformes",
                                                           "Cyprinodontiformes","Dactylopteriformes","Elopiformes","Esociformes","Eupercariaincertaesedis","Gadiformes",
                                                           "Hiodontiformes","Holocentriformes","Kurtiformes","Lampriformes","Lepisosteiformes","Lophiiformes",
                                                           "Mugiliformes","Mulliformes","Myctophiformes","Notacanthiformes","Ophidiiformes","Osmeriformes",
                                                           "Ovalentariaincertaesedis","Perciformes","Percopsiformes","Pleuronectiformes","Polymixiiformes","Saccopharyngiformes",
                                                           "Salmoniformes","Scombriformes","Siluriformes","Stomiiformes","Stylephoriformes","Syngnathiformes",
                                                           "Tetraodontiformes","Trachichthyiformes","Zeiformes","Actiniaria","Alcyonacea","Pennatulacea",
                                                           "Scleractinia","Spirularia","Zoantharia","Copelata","Trombidiformes","Aplousobranchia",
                                                           "Phlebobranchia","Stolidobranchia","Brisingida","Forcipulatida","Paxillosida","Spinulosida",
                                                           "Valvatida","Velatida","Anseriformes","Charadriiformes","Gaviiformes","Pelecaniformes",
                                                           "Procellariiformes","Naviculales","Adapedonta","Arcida","Cardiida","Carditida",
                                                           "Galeommatida","Limida","Lucinida","Myida","Mytilida","Nuculanida","Nuculida","Ostreida",
                                                           "Pectinida","Solemyida","Venerida","Anomopoda","Ctenopoda","Diplostraca","Onychopoda","Clathrinida","Leucosolenida",
                                                           "Chaetodermatida","Bathyteuthida","Myopsida","Octopoda","Oegopsida","Sepiida",
                                                           "Spirulida","Teuthida","Vampyromorpha","Bothriocephalidea","Diphyllidea","Diphyllobothriidea",
                                                           "Onchoproteocephalidea","Phyllobothriidea","Rhinebothriidea","Spathebothriidea","Tetrabothriidea","Tetraphyllidea",
                                                           "Trypanorhyncha","Araeolaimida","Chromadorida","Desmodorida","Monhysterida","Plectida",
                                                           "Rhabditida","Strongylida","Arhynchobdellida","Haplotaxida","Rhynchobdellida",
                                                           "Comatulida","Axinellida","Biemnida","Clionaida","Haplosclerida","Poecilosclerida",
                                                           "Polymastiida","Suberitida","Tetractinellida","Arbacioida","Camarodonta","Clypeasteroida",
                                                           "Spatangoida","Carcharhiniformes","Hexanchiformes","Lamniformes","Myliobatiformes","Rajiformes",
                                                           "Squaliformes","Squatiniformes","Torpediniformes","Enoplida","Trichinellida","[unassignedEnteropneusta",
                                                           "Neoechinorhynchida","Ceramiales","Gigartinales","[un]Caenogastropoda","Cephalaspidea","Ellobiida",
                                                           "Lepetellida","Littorinimorpha","Neogastropoda","Nudibranchia","Pteropoda",
                                                           "Trochida","Cheilostomatida","Ctenostomatida","Lyssacinosida","Calanoida","Canuelloida",
                                                           "Cyclopoida","Harpacticoida","Monstrilloida","Siphonostomatoida","Chimaeriformes","Apodida",
                                                           "Dendrochirotida","Elasipodida","Molpadida","Monostilifera","Anthoathecata","Leptothecata",
                                                           "Limnomedusae","Narcomedusae","Siphonophorae","Trachymedusae","Arguloida","Coleoptera",
                                                           "Diptera","Hemiptera","Phthiraptera","Trichoptera","Alismatales","Amphipoda",
                                                           "Cumacea","Decapoda","Euphausiacea","Isopoda","Leptostraca","Lophogastrida",
                                                           "Mysida","Tanaidacea","Carnivora","Cetartiodactyla","Rodentia","Harpacticoida",
                                                           "Pedunculata","Xiphosurida","Capsalidea","Gyrodactylidea","Mazocraeidea","Myxiniformes",
                                                           "Beroida","Amphilepidida","Euryalida","Ophiacanthida","Ophioscolecida","Ophiurida",
                                                           "Halocyprida","Myodocopida","Podocopida","Echinorhynchida","Polymorphida",
                                                           "Archinemertea","Petromyzontiformes","Desmarestiales","Fucales","Laminariales","Heteronemertea",
                                                           "Amphinomida","Echiuroidea","Eunicida","Phyllodocida","Sabellida",
                                                           "Spionida","Terebellida","Chitonida","Lepidopleurida","Pantopoda","Rhynchonellida",
                                                           "Terebratulida","Aphragmophora","Phragmophora","Dentaliida","Gadilida","Coronatae",
                                                           "Semaeostomeae","Golfingiida","Golfingiiformes","Stauromedusae","Cyclostomatida","Cydippida","Lobata","Balanomorpha","Dendrogastrida",
                                                           "Lithoglyptida","Scalpellomorpha","Digeneaincertaesedis","Diplostomida","Plagiorchiida","Ulvales" )
    ))})
    
    # Définir les arbres hiérarchique
    
    ## Définir la table
    test_table_R<-reactive(species%>%dplyr::filter(Règne==as.character(input$groupe)))
    test_table_E<-reactive(species%>%dplyr::filter(Embranchement==as.character(input$groupe2)))
    test_table_C<-reactive(species%>%dplyr::filter(Classe==as.character(input$groupe3)))
    test_table_O<-reactive(species%>%dplyr::filter(Ordre==as.character(input$groupe4)))
    test_table_F<-reactive(species%>%dplyr::filter(Famille==as.character(input$groupe5)))
    ##Afficher les premières lignes de la table
    output$taBLE_R<-renderTable({head(test_table_R())})
    output$taBLE_E<-renderTable({head(test_table_E())})
    output$taBLE_C<-renderTable({head(test_table_C())})
    output$taBLE_O<-renderTable({head(test_table_O())})
    output$taBLE_F<-renderTable({head(test_table_F())})
    ## Définir les arbres
    output$tree_R<-renderCollapsibleTree({collapsibleTree(
        test_table_R(),
        root=input$groupe,
        attribute = 'Nom.scientifique',
        hierarchy = c('Embranchement', 'Classe', 'Ordre', 'Famille','Nom.scientifique'),
        fill="#1e4659",
        fontSize = 18,
        linkLength = 200,
        height = 5000)
        
    })
    output$tree_E <- renderCollapsibleTree({
        collapsibleTree(
            test_table_E(),
            root=input$groupe2,
            attribute = 'Nom.scientifique',
            hierarchy = c('Classe', 'Ordre', 'Famille','Nom.scientifique'),
            fill="#1e4659",
            fontSize = 18,
            linkLength = 200,
            height = 5000)
        
    })
   output$tree_C <- renderCollapsibleTree({
        collapsibleTree(
            test_table_C(),
            root=input$groupe3,
            attribute = 'Nom.scientifique',
            hierarchy = c('Ordre', 'Famille','Nom.scientifique'),
            zoomable=T,
            fill="#1e4659",
            fontSize = 18, 
            linkLength = 200)
        
    })
    output$tree_O <- renderCollapsibleTree({
        collapsibleTree(
            test_table_O(),
            root=input$groupe4,
            attribute = 'Nom.scientifique',
            hierarchy = c('Famille','Nom.scientifique'),
            zoomable=T,
            fill="#1e4659",
            fontSize = 18, 
            linkLength = 200)
        
    })
    
    ### Rendre la table téléchargeable
    output$download <- downloadHandler(
        filename = function(){"liste_sp_StLaurent_2021.csv"}, 
        content = function(fname){
            write.table(species[,-13:-14], fname,sep=";",row.names = FALSE)
        }
    )
    ### Test Graphiques
    speciesnoAnim<-reactive({species[species$Règne!="Animalia",]})
   output$plot_nbsp_regne <- renderPlot({
        
        ggplot(data=speciesR(),aes(x=factor(1),fill=factor(Règne)))+
            geom_bar(stat="count")+
            labs(title=~ atop(paste("Répartition du nombre d'espèces recensées"),
                              paste(" dans les différents règnes du vivant")),y="",x="")+
            coord_polar(theta="y")+
            guides(fill = guide_legend(title = "Règne"))+
            theme(legend.text = element_text(size = 16),
                  legend.title = element_blank(),
                  plot.title= element_text(size = 20),
                  panel.background = element_blank(),axis.text = element_blank(),
                  axis.ticks = element_blank(),
                  axis.title = element_blank(),
                  panel.grid = element_blank(),
                  text=element_text(family="sans"))+
                 scale_fill_manual(values = c("#6497b1","#005b96","#b3cde0"))
            
        
    })
   
   output$plot_nbsp_noANIM <- renderPlot({
       
       ggplot(data=speciesnoAnim(),aes(x=factor(1),fill=factor(Règne)))+
           geom_bar(stat="count")+
           labs(title=~ atop(paste("Répartition du nombre d'espèces non animales"),
                             paste(" dans les différents règnes du vivant")),y="",x="")+
           coord_polar(theta="y")+
           guides(fill = guide_legend(title = "Règne"))+
           theme(legend.text = element_text(size = 16),
                 legend.title = element_blank(),
                 plot.title= element_text(size = 20,hjust=0.5),
                 panel.background = element_blank(),axis.text = element_blank(),
                 axis.ticks = element_blank(),
                 axis.title = element_blank(),
                 panel.grid = element_blank(),
                 text=element_text(family="sans"))+
           scale_fill_manual(values = c("#005b96","#b3cde0"))
       
       
   })
   
   table_anim<-reactive(species%>%dplyr::filter(Règne=="Animalia"))
   output$plot_nbsp_emb_anim <- renderPlot({
       
       ggplot(data = table_anim()) + 
           stat_count(mapping = aes(x=forcats::fct_rev(Embranchement)),fill="lightblue")+
           scale_y_continuous(limits = c(0, 1200),breaks = c(200, 400, 600,800,1000,1200))+
           labs(title=~ atop(paste("Répartition du nombre d'espèces recensées"),
                             paste(" au sein de chaque embranchement du règne animal")),y="Nombre d'espèces",x="Embranchements")+
           theme(plot.title= element_text(size = 20,hjust=0.5),
                 panel.background = element_blank(),
                 axis.title = element_text(size=18),
                 axis.text.y = element_text(size=16),
                 axis.text.x = element_text(angle=45,vjust=.8, hjust=0.8,size=16),
                 axis.line = element_line(size = 1, colour = "black"),
                 axis.ticks.length = unit(.25,"cm"),
                 text=element_text(family="sans"))
                
       
   })
    
})
