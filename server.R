### ---------------------------------------------------------------------------
### --- WDCM Semantics Dashboard, v. Beta 0.1
### --- Script: server.R, v. Beta 0.1
### ---------------------------------------------------------------------------

### --- Setup

### --------------------------------
### --- general
library(shiny)
library(shinydashboard)
library(RMySQL)
library(data.table)
library(DT)
library(stringr)
library(tidyr)
library(dplyr)
library(reshape2)
library(XML)
### --- compute
library(parallelDist)
### --- visualization
library(visNetwork)
library(RColorBrewer)
library(networkD3)
library(ggplot2)
library(ggrepel)
library(scales)
library(rbokeh)
### --- connect
library(httr)
library(curl)

### --- Server (Session) Scope
### --------------------------------

### --- Config File
params <- xmlParse('wdcmConfig_wdcmSemanticsDashboard.xml')
params <- xmlToList(params)

### --- functions
get_WDCM_table <- function(url_dir, filename, row_names) {
  read.csv(paste0(url_dir, filename), 
           header = T, 
           stringsAsFactors = F,
           check.names = F)
}

# - projectType() to determine project type
projectType <- function(projectName) {
  unname(sapply(projectName, function(x) {
    if (grepl("commons", x, fixed = T)) {"Commons"
    } else if (grepl("mediawiki|meta|species|wikidata", x)) {"Other"
    } else if (grepl("wiki$", x)) {"Wikipedia"
    } else if (grepl("quote$", x)) {"Wikiquote"
    } else if (grepl("voyage$", x)) {"Wikivoyage"
    } else if (grepl("news$", x)) {"Wikinews"
    } else if (grepl("source$", x)) {"Wikisource"
    } else if (grepl("wiktionary$", x)) {"Wiktionary"
    } else if (grepl("versity$", x)) {"Wikiversity"
    } else if (grepl("books$", x)) {"Wikibooks"
    } else {"Other"}
  }))
}

### --- shinyServer
shinyServer(function(input, output, session) {
  
  ### --- DATA
  
  ### --- list remote ml directory
  
  # - download ML and ETL files:
  withProgress(message = 'Downloading data', detail = "Please be patient.", value = 0, {
    
    # - list files:
    incProgress(1/8, detail = "Access data repository.")
    url <- params$remoteMLDir
    page <- as.character(GET(url), config = httr::config(http_version = 2))
    links <- str_extract_all(page, "<a href=.+>.+</a>")
    links <- sapply(links, function(x) {str_extract_all(x, ">.+<")})
    links <- sapply(links, function(x) {gsub('^>|"|<$|>|<', "", x)})
    links <- links[3:length(links)]
    tSNEmapsFiles <- links[which(grepl("wdcm2_tsne2D_project_", links, fixed = T))]
    itemTopicTables <- links[which(grepl("wdcm2_itemtopic_", links, fixed = T))]
    projectTopicTables <- links[which(grepl("wdcm2_projecttopic_", links, fixed = T))]

    ### --- fetch wdcm2_project
    incProgress(2/8, detail = "ETL dataset.")
    wdcmProject <- get_WDCM_table(params$remoteETLDir, 'wdcm_project.csv')
    wdcmProject$type <- projectType(wdcmProject$eu_project)
    colnames(wdcmProject) <- c('Project', 'Usage', 'Project Type')
    
    ### --- fetch 	wdcm_category_item.csv
    incProgress(3/8, detail = "ETL dataset.")
    wdcm_category_item <- get_WDCM_table(
      params$remoteETLDir, 'wdcm_category_item.csv')
    colnames(wdcm_category_item) <- c('Entity', 'Usage', 'Label', 'Category')
    
    ### --- fetch wdcm2_project_category
    incProgress(4/8, detail = "ETL dataset.")
    wdcmProjectCategory <- get_WDCM_table(params$remoteETLDir, 'wdcm_project_category.csv', row_names = F)
    wdcmProjectCategory$type <- projectType(wdcmProjectCategory$eu_project)
    colnames(wdcmProjectCategory) <- c('Project', 'Category', 'Usage', 'Project Type')
    # - fix `Wikimedia_Internal` to `Wikimedia`
    wdcmProjectCategory$Category[wdcmProjectCategory$Category == 'Wikimedia_Internal'] <- 
      'Wikimedia'

    ### --- fetch wdcm2_category
    incProgress(5/8, detail = "ETL dataset.")
    wdcmCategory <- get_WDCM_table(params$remoteETLDir, 'wdcm_category.csv')
    colnames(wdcmCategory) <- c('Category', 'Usage')
    # - fix `Wikimedia_Internal` to `Wikimedia`
    wdcmCategory$Category[wdcmCategory$Category == 'Wikimedia_Internal'] <- 
      'Wikimedia'
    
    ### --- fetch wdcm2_projects_2dmaps
    incProgress(6/8, detail = "tSNE Semantic Maps.")
    wdcm2_projects_2dmaps <- lapply(tSNEmapsFiles, function(x) {
      get_WDCM_table(url_dir = params$remoteMLDir, x, row_names = T)
    })
    # - enter category info:
    for (i in 1:length(tSNEmapsFiles)) {
      wdcm2_projects_2dmaps[[i]]$Category <- gsub(".csv", "",
        strsplit(tSNEmapsFiles[i], "_")[[1]][4], fixed = T)
    }
    wdcm2_projects_2dmaps <- rbindlist(wdcm2_projects_2dmaps)
    wdcm2_projects_2dmaps$V1 <- NULL
    colnames(wdcm2_projects_2dmaps) <- c('D1', 'D2', 'Project', 'Project Type', 'Category')
    wdcm2_projects_2dmaps$Category[wdcm2_projects_2dmaps$Category == "Wikimedia_Internal"] <- 
      'Wikimedia'
    
    ### --- fetch projectTopic tables
    incProgress(7/8, detail = "Projects data.")
    projectTopic <- lapply(projectTopicTables, function(x) {
      d <- get_WDCM_table(url_dir = params$remoteMLDir, x, row_names = T)
      colnames(d)[1] <- 'id'
      d
    })
    names(projectTopic) <- sapply(projectTopicTables, function(x) {
      strsplit(strsplit(x, split = ".", fixed = T)[[1]][1],
               split = "_",
               fixed = T)[[1]][3]
    })
    
    ### --- Fetch update info
    incProgress(8/8, detail = "Update info.")
    update <- read.csv(params$update_File_Path,
                       header = T,
                       check.names = F,
                       stringsAsFactors = F,
                       row.names = 1)
    })

  ### - Determine Constants
  # - determine Projects
  projects <- wdcmProject$Project
  # - determine present Project Types
  projectTypes <- unique(wdcmProject$`Project Type`)
  # - and assign Brewer colors
  lengthProjectColor <- length(unique(wdcmProject$`Project Type`))
  projectTypeColor <- brewer.pal(lengthProjectColor, "Set1")
  names(projectTypeColor) <- unique(wdcmProject$`Project Type`)
  # - determine Categories
  categories <- wdcmCategory$Category
  # - totalUsage
  totalUsage <- sum(wdcmProject$Usage)
  totalProjects <- length(wdcmProject$Project)
  totalCategories <- length(wdcmCategory$Category)
  totalProjectTypes <- length(unique(wdcmProject$`Project Type`))
  
  ### --- prepare search constants for Tabs/Crosstabs
  search_projectTypes <- paste("_", projectTypes, sep = "")
  unzip_projectTypes <- lapply(projectTypes, function(x) {
    wdcmProject$Project[which(wdcmProject$`Project Type` %in% x)]
  })
  names(unzip_projectTypes) <- search_projectTypes
  
  
  ### --- OUTPUTS
  
  ### --- output: updateString
  output$updateString <- renderText({
    date <- update[max(which(grepl("Orchestra END", update$Step))), ]$Time
    date <- paste0(date, " UTC")
    return(paste('<p style="font-size:80%;"align="right"><b>Last update: </b><i>', date, '</i></p>', sep = ""))
  })
  
  ### ------------------------------------------
  ### --- TAB: tabPanel Semantic Models
  ### ------------------------------------------

  ### --- SELECT: update select 'selectCategory'
  updateSelectizeInput(session,
                       'selectCategory',
                       "Select Semantic Category:",
                       choices = categories,
                       selected = categories[round(runif(1, 1, length(categories)))],
                       server = TRUE)
  
  ### --- REACTIVE itemTopic: current items x topics matrix (category-specific)
  itemTopic <- reactive({
    withProgress(message = 'Downloading data', detail = "Items x Topics Matrix", value = 0, {
      incProgress(1/2, detail = "Items x Topics Matrix")
      dFile <- itemTopicTables[which(grepl(input$selectCategory, itemTopicTables))]
      incProgress(2/2, detail = "Items x Topics Matrix")
      iT <- get_WDCM_table(url_dir = params$remoteMLDir, dFile, row_names = T)
      colnames(iT)[1] <- "Entity"
      iT
    })
  })
  
  ### --- REACTIVE itemTopicsNum: how many topics in a given category?
  itemTopicsNum <- reactive({
    tops <- colnames(itemTopic())
    sum(grepl("topic", tops))
  })
  
  ### --- SELECT: updateSelectizeInput 'selectCatTopic'
  ### --- select a particular topic from a category-specific LDA model
  output$selectCatTopic <-
    renderUI({
      if ((is.null(input$selectCategory)) | (length(input$selectCategory) == 0)) {
        selectInput(inputId = "selectCategoryTopic",
                    label = "Select Semantic Topic:",
                    choices = NULL,
                    selected = NULL)
      } else {
        cH <- paste("Topic", 1:itemTopicsNum(), sep = " ")
        selectInput(inputId = "selectCategoryTopic",
                    label = "Select Semantic Topic:",
                    choices = cH,
                    selected = cH[1])
      }
    })
  
  ### --- OUTPUT output$topItemsTopic
  output$topItemsTopic <- renderPlot({
    if (!is.null(itemTopic())) {
      cTopic <- tolower(gsub(" ", "", input$selectCategoryTopic))
      plotFrame <- as.data.frame(itemTopic()$Entity)
      colnames(plotFrame) <- 'Entity'
      plotFrame <- cbind(plotFrame, select(itemTopic(), cTopic))
      colnames(plotFrame) <- c('Id', 'Probability')
      plotFrame <- arrange(plotFrame, desc(Probability)) %>% head(50)
      plotFrame$Label <- paste(1:dim(plotFrame)[1], ". ", plotFrame$Id, sep = "")
      plotFrame$Label <- factor(plotFrame$Label, 
                                levels = plotFrame$Label[order(plotFrame$Probability)])
      plotFrame$Sign <- paste("(", 1:dim(plotFrame)[1], ") ", plotFrame$Id, sep = "")
      ggplot(plotFrame, aes(x = Probability, 
                            y = Label, 
                            label = Sign)) +
        geom_line(size = .25, color = "#4c8cff", group = 1) + 
        geom_point(size = 1.5, color = "#4c8cff") + 
        geom_point(size = 1, color = "white") + 
        geom_label_repel(size = 3, segment.size = .25, show.legend = FALSE) +
        ylab("Items Labels") + xlab("Item Importance\n(Item Probability in Topic)") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, size = 12, hjust = 1)) + 
        theme(axis.text.y = element_text(size = 12, hjust = 1)) +
        theme(axis.title.x = element_text(size = 12)) +
        theme(axis.title.y = element_text(size = 12)) %>% 
        withProgress(message = 'Generating plot',
                     min = 0,
                     max = 1,
                     value = 1, {incProgress(amount = 0)})
    } else {return(NULL)}
  }) 
  
  # - output$networkItemsTopic
  output$networkItemsTopic <- renderVisNetwork({
    
    if (!is.null(itemTopic())) {
      cTopic <- tolower(gsub(" ", "", input$selectCategoryTopic))
      w <- which(colnames(itemTopic()) == cTopic)
      itemNames <- itemTopic()[, c(1, w)]
      itemNames <- itemNames[order(-itemNames[, 2]), ]
      itemNames <- itemNames$Entity
      root <- dplyr::select(itemTopic(), starts_with('topic'))
      selItems <- wdcm_category_item %>% 
        filter(grepl(input$selectCategory, wdcm_category_item$Category)) %>% 
        arrange(desc(Usage)) %>% 
        head(50) %>% 
        select(Entity, Label)
      w <- which(itemNames %in% selItems$Entity)
      root <- root[w, ]
      root <- as.matrix(parDist(as.matrix(root), method = "euclidean"))
      w <- which(selItems$Entity %in% itemNames)
      rownames(root) <- paste0(selItems$Label[w], " (", selItems$Entity[w], ")") 
      colnames(root) <- paste0(selItems$Label[w], " (", selItems$Entity[w], ")")
      indexMinDist <- sapply(rownames(root), function(x) {
        w <- which(rownames(root) %in% x)
        y <- sort(root[w, -w], decreasing = T)
        names(y)[length(y)]
      })
      id <- 1:length(colnames(root))
      label <- colnames(root)
      nodes <- data.frame(id = id,
                          label = label,
                          stringsAsFactors = F)
      conceptsStruct <- data.frame(from = names(indexMinDist),
                                   to = unname(indexMinDist),
                                   stringsAsFactors = F)
      conceptsStruct$from <- sapply(conceptsStruct$from, function(x) {
        nodes$id[which(nodes$label %in% x)]
      })
      conceptsStruct$to <- sapply(conceptsStruct$to, function(x) {
        nodes$id[which(nodes$label %in% x)]
      })
      conceptsStruct$arrows <- rep("to", length(conceptsStruct$to))
      # nodes$label <- sapply(nodes$label, function(x) {
      #   itemTopic()$eu_label[itemTopic()$eu_entity_id == x]
      # })
      visNetwork(nodes = nodes,
                 edges = conceptsStruct,
                 width = "100%",
                 height = "100%") %>%
        visEvents(type = "once",
                  startStabilizing = "function() {this.moveTo({scale:0.65})}") %>%
        visPhysics(stabilization = FALSE) %>%
        withProgress(message = 'Generating plot',
                     min = 0,
                     max = 1,
                     value = 1, {incProgress(amount = 0)})
    } else {return(NULL)}
  })
  
  ### --- REACTIVE current pTopic data.frame:
  pTopic <- reactive({
    
    withProgress(message = 'Downloading data', detail = "Projects x Topics Matrix", value = 0, {
      incProgress(1/2, detail = "Projects x Topics Matrix")
      dFile <- projectTopicTables[which(grepl(input$selectCategory, projectTopicTables))]
      incProgress(2/2, detail = "Projects x Topics Matrix")
      pTopic <- get_WDCM_table(url_dir = params$remoteMLDir, dFile, row_names = T)
      colnames(pTopic)[1] <- "Project"
    })
    
    if (!is.null(pTopic)) {
      cTopic <- which(colnames(pTopic) %in% tolower(gsub(" ", "", input$selectCategoryTopic)))
      if (!length(cTopic) == 0) {
        pTopic <- pTopic %>% 
          select(cTopic, project, projecttype) %>% 
          arrange(desc(pTopic[, cTopic]))
        pTopic <- pTopic[1:50, ]
        if (sum(is.na(pTopic$project)) > 0) {
          pTopic <- pTopic[-which(is.na(pTopic$project)), ]
        }
        colnames(pTopic) <- c('Probability', 'Project', 'Project Type')
        pTopic$Label <- paste(1:dim(pTopic)[1], ". ", pTopic$Project, sep = "")
        pTopic$Label <- factor(pTopic$Label, 
                               levels = pTopic$Label[order(pTopic$Probability)])
        return(pTopic)
      } else {return(NULL)}
    } else {return(NULL)}
  })
  
  ### --- OUTPUT output$topProjectsTopic
  output$topProjectsTopic <- renderPlot({
    if (!is.null(pTopic())) {
      ggplot(pTopic(), aes(x = Probability, 
                            y = Label, 
                            label = Label)) +
        geom_line(size = .25, color = "#4c8cff", group = 1) + 
        geom_point(size = 1.5, color = "#4c8cff") + 
        geom_point(size = 1, color = "white") + 
        geom_label_repel(size = 3, segment.size = .25, show.legend = FALSE) +
        ylab("Projects") + xlab("Topic Importance in Project\n(Topic Probability in Project)") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, size = 12, hjust = 1)) + 
        theme(axis.text.y = element_text(size = 12, hjust = 1)) +
        theme(axis.title.x = element_text(size = 12)) +
        theme(axis.title.y = element_text(size = 12)) %>% 
        withProgress(message = 'Generating plot',
                     min = 0,
                     max = 1,
                     value = 1, {incProgress(amount = 0)})
    } else {return(NULL)}
  }) 
  
  ### ------------------------------------------
  ### --- TAB: tabPanel Projects
  ### ------------------------------------------
  
  ### --- SELECT: update select 'selectProject'
  updateSelectizeInput(session,
                       'selectProject',
                       choices = c(projects, paste("_", projectTypes, sep = "")),
                       selected = "_Wikipedia",
                       server = TRUE)
  
  ### --- REACTIVE: selectedProjects
  selectedProjects <- reactive({
    ### --- selected projects:
    if (!is.null(input$selectProject)) {
      wUnzip <- which(names(unzip_projectTypes) %in% input$selectProject)
      if (length(wUnzip > 0)) {
        selectedProjects <- unname(do.call('c', unzip_projectTypes[wUnzip]))
      }
      wSel <- which(projects %in% input$selectProject)
      if (length(wSel > 0)) {
        selectedProjects <- c(selectedProjects, projects[wSel])
      }
      selectedProjects <- unique(selectedProjects)
      return(selectedProjects)
    } else {return(NULL)}
  })
  
  ### --- OBSERVE: input$applySelection
  observeEvent(input$applySelection, {
  
    #### ---  Chart: projectTopicImportance
    output$projectTopicImportance <- renderPlot({
      # - Plot Frame for projectTopicImportance
      projList <- lapply(names(projectTopic), function(x) {
        cCategory <- which(names(projectTopic) %in% x)
        cProj <- projectTopic[[cCategory]]
        if (sum(which(cProj$project %in% isolate(selectedProjects()))) == 0) {
          return(NULL)
        } else {
          cProj <- cProj %>% 
            filter(project %in% isolate(selectedProjects())) %>% 
            select(starts_with("topic"), project) %>% 
            gather(key = Topic, 
                   value = Probability,
                   starts_with('topic'))
          catName <- gsub("([[:lower:]])([[:upper:]])", "\\1 \\2", names(projectTopic)[cCategory])
          # - FIX THIS:
          cProj$Category <- catName
          cProj <- cProj %>% 
            select(Topic, Probability, Category) %>% 
            group_by(Category, Topic) %>% 
            summarise(Proportion = sum(Probability))
          cProj$Proportion <- round(cProj$Proportion/sum(cProj$Proportion)*100, 2)
          return(cProj)
        }
      })
      wEl <- sapply(projList, function(x) {
        !is.null(x)
        })
      projList <- as.data.frame(rbindlist(projList[wEl]))
      # - factor projList$Topic:
      projList$Topic <- str_to_title(gsub("([[:alpha:]]+)", "\\1 ", projList$Topic))
      topicLevels <- unique(projList$Topic)
      topicLevelsOrd <- as.numeric(str_extract(topicLevels, "[[:digit:]]+"))
      topicLevels <- topicLevels[order(topicLevelsOrd)]
      projList$Topic <- factor(projList$Topic, levels = topicLevels)
      # - visualize w. ggplot2
      ggplot(projList,
             aes(x = Topic, 
                 y = Proportion, 
                 label = paste(Proportion, "%", sep = ""))
             ) +
        geom_line(group = 1, size = .25, color = "#4c8cff") +
        geom_point(size = 1.5, color = "#4c8cff", fill = "#4c8cff") + 
        geom_point(size = 1, color = "white") + 
        geom_text_repel(size = 4) +
        facet_wrap(~ Category, ncol = 3, scales = "free_y") +
        xlab('Topic') + ylab('Topic Engagement (%)') +
        scale_y_continuous(labels = comma) + 
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, size = 12, hjust = 1)) +
        theme(axis.title.x = element_text(size = 12)) +
        theme(axis.title.y = element_text(size = 12)) +
        theme(strip.text = element_text(size = 13)) %>%
        withProgress(message = 'Generating plot',
                     min = 0,
                     max = 1,
                     value = 1, {incProgress(amount = 0)})
    })
  
  }, ignoreNULL = FALSE)
  
  ### ------------------------------------------
  ### --- TAB: tabPanel Similarity
  ### ------------------------------------------
  
  ### --- SELECT: update select 'selectCategory2'
  updateSelectizeInput(session,
                       'selectCategory2',
                       "Select Semantic Category:",
                       choices = categories,
                       selected = categories[round(runif(1, 1, length(categories)))],
                       server = TRUE)
  
  ### --- OBSERVE: input$selectCategory2
  observeEvent(input$selectCategory2, {
    
    if (!is.null(input$selectCategory2)) {
      
      wdcmP <- wdcmProject %>% 
        select(Project, Usage)
    
      projCatFrame <- wdcm2_projects_2dmaps %>% 
        filter(Category %in% input$selectCategory2) %>% 
        left_join(wdcmP, by = 'Project')
      
      ### --- output$overviewPlotDynamic
      output$overviewPlotDynamic <- renderRbokeh({
        outFig <- figure(width = 1400, height = 900, logo = NULL) %>%
          ly_points(D1, D2, 
                    data = projCatFrame,
                    size = log(Usage), 
                    color = 'Project Type', 
                    hover = list(Project, Usage)) %>% 
          x_axis(visible = F) %>% 
          y_axis(visible = F) %>% 
          theme_grid(which = c("x", "y"), 
                     grid_line_color = "white") %>% 
          theme_plot(outline_line_alpha = 0) %>% 
          set_palette(discrete_color = pal_color(unname(projectTypeColor)))
        outFig
      }) %>% withProgress(message = 'Generating plot',
                          min = 0,
                          max = 1,
                          value = 1, {incProgress(amount = 1)})
      
    } else {return(NULL)}
    
  }, ignoreNULL = FALSE)
  
  
  
}) ### --- END shinyServer










