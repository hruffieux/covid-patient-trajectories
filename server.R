require(shiny)

server <- function(input, output, session) {
  
  source("fun_utils.R")
  
  load("FPCA_output.RData")
  
  
  # TAB 1
  # -----  
  
  selectedDfComb <- reactive({
    list_score_types[[input$score_type]]$df_comb
  })
  
  selectedData <- reactive({
    list_score_types[[input$score_type]]$data
  })
  
  selectedFit <- reactive({
    list_score_types[[input$score_type]]$fit
  })
  
  selectedScores <- reactive({
    list_score_types[[input$score_type]]$scores
  })
  
  selectedVecVar <- reactive({
    list_score_types[[input$score_type]]$vec_var
  })
  
  selectedColor <- reactive({
    input$color_type
  })
  
  
  # TAB 2
  # -----  
  
  selectedDataTab2 <- reactive({
    list_score_types[[input$score_type_tab2]]$data
  })
  
  selectedFitTab2 <- reactive({
    list_score_types[[input$score_type_tab2]]$fit
  })
  
  selectedVecVarTab2 <- reactive({
    list_score_types[[input$score_type_tab2]]$vec_var
  })
  
  
  # TAB 3
  # -----  
  
  selectedDataTypeTab3 <- reactive({
    input$data_type_tab3
  })
  
  
  # HOVER INFO
  # ----------
  
  displayed_text <- reactive({
    req(input$plot_hover)
    hover <- input$plot_hover
    dist <- sqrt((hover$x - selectedScores()$EF1)^2 + (hover$y - selectedScores()$EF2)^2)
    
    if(min(dist) < 0.3) {
      rownames(selectedScores())[which.min(dist)]
    } else {
      NULL
    }
  })
  
  hover_reactive <- reactiveVal()  ## initialize
  
  observe({
    hover_data <- displayed_text()
    if (!is.null(hover_data))
      hover_reactive(hover_data)  ## set
  })
  
  output$hover_info <- renderText({
    subj <- as.character(hover_reactive())
    
    if (length(subj)>0) {
      
      score_1 <- format(selectedScores()$EF1[selectedScores()$subject_id %in% subj], digits = 2)
      score_2 <- format(selectedScores()$EF2[selectedScores()$subject_id %in% subj], digits = 2)
      
      sev <- unique(selectedDfComb()$severity[selectedDfComb()$subject_id %in% subj]) 
      group <- unique(df_scores$clusters_mclust[df_scores$subject_id %in% subj])
      prob_assignment <- unique(df_scores$mclust_prob_assignment[df_scores$subject_id %in% subj])
      
      gender <- unique(selectedDfComb()$gender[selectedDfComb()$subject_id %in% subj])
      age <- unique(selectedDfComb()$age[selectedDfComb()$subject_id %in% subj])
      bmi <- unique(selectedDfComb()$bmi[selectedDfComb()$subject_id %in% subj])
      ethnicity <- unique(selectedDfComb()$ethnicity[selectedDfComb()$subject_id %in% subj])
      vec_other_infection <- unique(selectedDfComb()$other_infection[selectedDfComb()$subject_id %in% subj])
      if ("proven" %in% vec_other_infection) {
        other_infection <- "Proven secondary infection. "
      } else if ("suspected" %in% vec_other_infection) {
        other_infection <- "Suspected secondary infection. "
      } else {
        other_infection <- ""
      }
      
      
      paste0("Patient: ", subj, 
             ", severity score: ", score_1, ", recovery score: ", score_2, ".\n", 
             "Severity class: ", sev, " (", 
             names(selected_severity_groups)[selected_severity_groups==sev], 
             "). ", other_infection, "Recovery group: ", group, 
             " (probability of assignment: ",format(prob_assignment, digits = 2), ").\n", 
             "Gender: ", gender, ", age: ", age, " years old", 
             ifelse(length(bmi) == 1 && !is.na(bmi), paste0(", BMI at the time of enrollment: ", bmi), ""),
             ifelse(length(ethnicity) == 1 && !is.na(ethnicity), paste0(", ", ethnicity, " ethnicity"), ""),".")
    }   
  })  ## get
  
  
  # SCORES
  # ------
  
  output$plot_scores <- renderPlot({
    
    if (selectedColor() == "Severity classes") {
      
      color <- selectedScores()$color_severity
      
    } else if (selectedColor() == "Recovery groups") {
      
      color <- vec_col_gr_3[df_scores$clusters_mclust[match(selectedScores()$subject_id, 
                                                                   df_scores$subject_id)]]
      
      color[is.na(color)] <- "grey80" # for subjects which are not assigned to 
                                      # any recovery group because data not 
                                      # available for the CRP FPC analysis
    }
    
    par(mfrow=c(1,1), mar = c(4.5, 4, 3, 0), pty = "s") 
    
    plot(selectedScores()$EF1, 
         selectedScores()$EF2, 
         col = color,
         pch = 20, 
         cex = 1.75, 
         xlim = c(-max(abs(selectedScores()$EF1)), 
                  max(abs(selectedScores()$EF1))),
         ylim = c(-max(abs(selectedScores()$EF2)), 
                  max(abs(selectedScores()$EF2))),
         xlab = paste0("Severity scores - FPC 1 (", 
                       format(get_pve(selectedFit(), 1), digits = 3), "%)"),
         ylab = paste0(" \n Recovery scores - FPC 2 (", 
                       format(get_pve(selectedFit(), 2), digits = 3), "%)"),
         main = "Patient scores"
    )
    
    points(selectedScores()$EF1[!is.na(selectedScores()$hospital_outcome) & 
                              selectedScores()$hospital_outcome == "dead"], 
           selectedScores()$EF2[!is.na(selectedScores()$hospital_outcome) & 
                              selectedScores()$hospital_outcome == "dead"], 
           pch = 13, col = adjustcolor("black", alpha.f = 0.4), cex = 2)
    
    subj <- as.character(hover_reactive())
    
    if (length(subj)>0) {
      
      score_1 <- selectedScores()$EF1[selectedScores()$subject_id %in% subj]
      score_2 <- selectedScores()$EF2[selectedScores()$subject_id %in% subj]
      points(score_1, score_2, pch = 10, cex = 3, lwd = 2)
      
    }
    
  })
  
  
  # LEGEND
  # ------
  
  output$legend <- renderPlot({
    
    if (selectedColor() == "Severity classes") {
      leg <- names(vec_col)[-c(1,2)]
      col <- vec_col[-c(1,2)]
    } else if (selectedColor() == "Recovery groups") {
      leg <- names(vec_col_gr_3)
      col <- vec_col_gr_3
    }
    
    par(mar = c(0, 0, 0, 0)) 
    plot(NULL, xaxt='n', yaxt='n', bty='n', ylab='', xlab='', xlim=0:1, ylim=0:1)
    legend("topleft", legend = c("dead", leg),  pt.cex = 1.75, 
           col = c("grey50", col), pch = c(13, rep(20, length(leg))), 
           bty='n', cex = 0.85)
    
  })
  
  
  # TRAJECTORIES
  # ------------
  
  output$plot_trajectories <- renderPlot({
    
    if (selectedVecVar()[[1]] == "log_CRP") {
      var_disp_names <- "CRP"
    } else {
      var_disp_names <- NULL
    }
    
    nb_var <- length(selectedVecVar())
    
    mm <- max(2, 5-nb_var)
    
    if (nb_var > 8) {
      mfrow <- c(3, ceiling(length(selectedVecVar())/3))
    } else if (nb_var > 2) {
      mfrow <- c(2, ceiling(length(selectedVecVar())/2))
    } else {
      mfrow <- c(1, length(selectedVecVar()))
    }
    
    if (is.data.frame(selectedData())) {
      data_y1 <- list("y1" =  selectedData()) # in case of univariate fpca
    } else {
      data_y1 <-  selectedData()# in case of multivariate fpca
    }
    
    if (length(hover_reactive())>0 && as.character(hover_reactive()) %in% data_y1$y1$subj) {
      subject <- as.character(hover_reactive())
    } else {
      subject <- NULL
    }
    
    if (!is.null(subject)) {
      
      par(mfrow=mfrow, mar = c(4.5, 4, 3, 1.5), pty = "s")
      plot_subject_trajectories(selectedFit(),
                                selectedScores(),
                                tnew,
                                selectedData(),
                                subjects_to_show = subject,
                                vec_var = selectedVecVar(),
                                df_comb = selectedDfComb(),
                                var_disp_names = var_disp_names,
                                bool_trunc = TRUE,
                                bool_par = FALSE, 
                                main_plus = "",
                                sub = "",
                                ylab = "log-scale",
                                cex_main = ifelse(nb_var > 8, 0.8, 1.3), 
                                cex_lab = 0.9)
    }
  }, height = 500)
  
  
  # AUTOCORRELATION
  # ---------------
  
  output$plot_correlation <- renderPlot({
    
    if (selectedVecVarTab2()[[1]] == "log_CRP") {
      var_disp_names <- "CRP"
    } else {
      var_disp_names <- NULL
    }
    
    nb_var <- length(selectedVecVarTab2())
    
    if (nb_var > 8) { # lymphocytes - select just a subset of them # TODO: allow the user to select them
      sub_var <- 9:12 
    } else {
      sub_var <- NULL
    }
    
    par(mfrow=c(1,1), mar = c(4.5, 4, 3, 0), pty = "s") 
    
    nb_ticks <- 4
    nb_spaces <- 100
    plot_correlation(selectedFitTab2(), selectedDataTab2(), tnew, 
                     selectedVecVarTab2(), nb_spaces, nb_ticks, 
                     var_disp_names = var_disp_names, sub_var = sub_var)
    
  })
  
  
  # VARIANCE
  # --------
  
  output$plot_variance <- renderPlot({
    
    if (selectedVecVarTab2()[[1]] == "log_CRP") {
      var_disp_names <- "CRP"
    } else {
      var_disp_names <- NULL
    }
    
    nb_var <- length(selectedVecVarTab2())
    
    if (nb_var > 8) {
      mfrow <- c(3, ceiling(length(selectedVecVarTab2())/3))
    } else if (nb_var > 2) {
      mfrow <- c(2, ceiling(length(selectedVecVarTab2())/2))
    } else {
      mfrow <- c(1, length(selectedVecVarTab2()))
    }
    
    par(mfrow=mfrow, mar = c(4.5, 4, 3, 1.5), pty = "s")
    
    plot_variance(selectedFitTab2(), 
                  selectedDataTab2(), 
                  tnew, 
                  selectedVecVarTab2(), 
                  disp_param = FALSE, 
                  var_disp_names = var_disp_names)
    
  })
  
  
  # GROUP-LEVEL TRAJECTORIES
  # ------------------------
  

  output$legend_group_level_trajectories <- renderImage({
    list(
      src = "legend_tab3.png",
      width = "40%", height = "4%"
    )
  }, deleteFile = FALSE)

  
  observe({
    
    resp_var <- list.files(pattern = paste0("group_level_longitudinal_", selectedDataTypeTab3()[[1]]))

    for (i in resp_var)
    {
      local({
        imagename = i
        output[[imagename]] <-
          renderImage({
            list(src = imagename, 
                 width = "90%", height = "80%",
                 alt = "Image failed to render")
          }, deleteFile = FALSE)
      })
    }
  })
  
  
  output$plot_group_level_trajectories <- renderUI({
    
    resp_var <- list.files(pattern = paste0("group_level_longitudinal_", selectedDataTypeTab3()[[1]]))
    
    vec_id <- rep(1:4, length.out = length(resp_var))
    
    image_output_list <- 
      lapply(resp_var[vec_id == 1],
             function(i)
             {
               imagename = i
               imageOutput(imagename)
             })
    
    do.call(tagList, image_output_list)
  })
  
  output$plot_group_level_trajectories2 <- renderUI({
    
    resp_var <- list.files(pattern = paste0("group_level_longitudinal_", selectedDataTypeTab3()[[1]]))
    
    vec_id <- rep(1:4, length.out = length(resp_var))
    
    image_output_list <- 
      lapply(resp_var[vec_id == 2],
             function(i)
             {
               imagename = i
               imageOutput(imagename)
             })
    
    do.call(tagList, image_output_list)
  })
  
  output$plot_group_level_trajectories3 <- renderUI({
    
    resp_var <- list.files(pattern = paste0("group_level_longitudinal_", selectedDataTypeTab3()[[1]]))
    
    vec_id <- rep(1:4, length.out = length(resp_var))
    
    image_output_list <- 
      lapply(resp_var[vec_id == 3],
             function(i)
             {
               imagename = i
               imageOutput(imagename)
             })
    
    do.call(tagList, image_output_list)
  })
  
  output$plot_group_level_trajectories4 <- renderUI({
    
    withProgress(message = 'Rendering plots', value = 1, {
      Sys.sleep(3)
    })

    resp_var <- list.files(pattern = paste0("group_level_longitudinal_", selectedDataTypeTab3()[[1]]))
    
    vec_id <- rep(1:4, length.out = length(resp_var))
    
    image_output_list <- 
      lapply(resp_var[vec_id == 4],
             function(i)
             {
               imagename = i
               imageOutput(imagename)
             })
    
    do.call(tagList, image_output_list)
  })
  
}
