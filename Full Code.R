#################################################################################
# 0) LIBRARIES
#################################################################################


library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(pROC) # For making AUC
library(ranger) # My favorit Random Forest package
library(writexl)
library(pROC)
library(PRROC)
library(forcats)
library(purrr)
library(betareg) #Beta regression LGD
library(gbm) #Gradient Boosting

###############################################################################
# 1) DATA
#########################################################################

df <- read_excel("IFRS_Simulation.xlsx")

# Making sure that import read correctly the type of data
df <- df %>%
  mutate(
    Date   = as.Date(Date),
    YEAR   = year(Date),
    MONTH  = month(Date),
    Id_client = as.numeric(Id_client),
    SIZE   = factor(SIZE),
    Program    = factor(Program),
    Cred_Type  = factor(Cred_Type),
    Sector     = factor(Sector),
    Activity   = factor(Activity)
  )


#Changing types for variables on text (Sector, Program, Cred_Type)

df <- df %>%
  mutate(
    # categoricals
    SIZE      = factor(SIZE),                       # 1/2/3(/4)
    Program   = factor(Program, levels = c(0,1)),   # 0/1 as factor
    Cred_Type = factor(Cred_Type),                  # "Rev","Simple"
    Sector    = factor(Sector),                     # text labels → factor
    NPL       = factor(NPL, levels = 0:4, ordered = TRUE),  # ordered categorical
    
    # lump rare sectors for glm (logistic regression for Model 1 above)
    Sector    = fct_lump_min(Sector, min = 50, other_level = "Other")
   )


################################################################################
# 2) Training Label (default next-month)
################################################################################

df <- df %>%
  arrange(Id_client, Date) %>%
  group_by(Id_client) %>%
  mutate(
    DefaultFlag = as.integer(NPL >= 4),
    y_next      = lead(DefaultFlag, 1)  # default en el mes siguiente
  ) %>%
  ungroup()

# (opcional) version “Stage”
df <- df %>%
  mutate(Stage = case_when(
    NPL == 0 ~ "Stage 1",
    NPL == 1 ~ "Stage 2",
    NPL >= 2 ~ "Stage 3",
    TRUE     ~ NA_character_
  ))

################################################################################
# 3) Columns for Analysis
###############################################################################


df <- df %>%
  mutate(
    Utilization         = ifelse(Loan_Amount_Local > 0, pmin(EAI / Loan_Amount_Local, 5), NA_real_),
    Debt_to_Assets      = ifelse(Assets > 0, Total_Debt / Assets, NA_real_),
    FinDebt_to_Assets   = ifelse(Assets > 0, F_Debt / Assets, NA_real_),
    Debt_to_Equity      = ifelse(Equity != 0, Total_Debt / Equity, NA_real_),  # cuidado si Equity≈0
    Rate_Spread         = I_rate,
    Score               = `Internal Score`,
    log_EAI             = log1p(EAI),
    log_Loan            = log1p(Loan_Amount_Local)
  )

# med numéricas avioding NAs

num_cols <- c("Utilization","Debt_to_Assets","FinDebt_to_Assets","Debt_to_Equity",
              "Rate_Spread","Score","log_EAI","log_Loan","LGD","EAI","Loan_Amount_Local","PD_Reg")
for (v in num_cols) {
  if (v %in% names(df)) {
    med <- median(df[[v]], na.rm = TRUE)
    df[[v]][is.na(df[[v]])] <- med
  }
}

################################################################################
# 4) Train / Test (Not so much data to test neither to training is possible bootstrap later or simulations)
#################################################################################


cut_date <- as.Date("2023-12-01")  # train until dic-2023, test = 2024
train <- df %>% filter(Date <= cut_date, !is.na(y_next))
test  <- df %>% filter(Date >  cut_date, !is.na(y_next))

###############################################################################
# 5A) Model 1: Log Regression
################################################################################

    # Harmonize factor levels between train/test
      align_levels <- function(tr, te, vars){
      for(v in vars){
      lv <- union(levels(tr[[v]]), levels(te[[v]]))  # superset
      tr[[v]] <- factor(tr[[v]], levels = lv, ordered = is.ordered(tr[[v]]))
      te[[v]] <- factor(te[[v]], levels = lv, ordered = is.ordered(te[[v]]))
      }
      list(train = tr, test = te)
      }
      al <- align_levels(train, test, c("SIZE","Program","Cred_Type","Sector","NPL"))
      train <- al$train; test <- al$test

    # Drop any factor that ended single-level in TRAINset

      drop_if_one_level <- function(data, vars){
      bad <- vapply(data[vars], function(x) nlevels(droplevels(x)) < 2, logical(1))
      vars[!bad]
      }
      cat_vars <- drop_if_one_level(train, c("SIZE","Program","Cred_Type","Sector","NPL"))

      
    # Inspect factor levels in the training slice
    # ___________________________________________________________________________________

      f_logit <- as.formula(paste(
      "y_next ~",
      paste(c("Score",
          "Rate_Spread","Utilization",
          "Debt_to_Assets","FinDebt_to_Assets","Debt_to_Equity",
          "log_EAI","log_Loan",
          cat_vars),
        collapse = " + ")
      ))

      glm_fit <- glm(f_logit, data = train, family = binomial())

      summary(glm_fit)
      exp(coef(glm_fit))  # Odds ratios


    # Predict PD (next-month)
      
      train$PD_logit <- predict(glm_fit, newdata = train, type = "response")
      test$PD_logit  <- predict(glm_fit, newdata = test,  type = "response")

      auc_train <- auc(train$y_next, train$PD_logit)
      auc_test  <- auc(test$y_next,  test$PD_logit)
      cat(sprintf("Logit AUC — Train: %.3f | Test: %.3f\n", auc_train, auc_test))

    #Checking results_____________________________________________________________________________

    # Class balance
      prop.table(table(train$y_next))

    # ROC & KS (train/test)
      
      roc_tr  <- roc(train$y_next, train$PD_logit)
      roc_te  <- roc(test$y_next,  test$PD_logit)
      ks_tr   <- max(abs(roc_tr$sensitivities - (1 - roc_tr$specificities)))
      ks_te   <- max(abs(roc_te$sensitivities - (1 - roc_te$specificities)))
      cat(sprintf("AUC train=%.3f  test=%.3f  |  KS train=%.3f  test=%.3f\n",
            auc(roc_tr), auc(roc_te), ks_tr, ks_te))

      #Just only me because of my windows (only if presented error on graphs)
      #while (dev.cur() > 1) dev.off()
      #windows(width=8, height=5)   # on Windows
      #par(mfrow = c(1,1), mar = c(5,4,2,1))  # sane margins  
      
    # --- ROC curve plot (test)
      plot(roc_te, col="blue", main="ROC — Logistic (test)")
      
    # PR curve (test) — NOTE: class0 = negatives, class1 = positives
      pr <- PRROC::pr.curve(
        scores.class0 = test$PD_logit[test$y_next==0],
        scores.class1 = test$PD_logit[test$y_next==1],
        curve = TRUE
      )
      plot(pr, main="PR curve — Logistic (test)")

  #Monthly results____________________________________________________________________________
      
      monthly_results <- df %>%
        filter(!is.na(y_next)) %>%
        group_split(YEAR, MONTH) %>%
        map_df(function(d){
          if(nrow(d) < 100 | length(unique(d$y_next)) < 2) return(NULL)
          
          fit <- glm(f_logit, data = d, family = binomial())
          d$PD_logit <- predict(fit, newdata = d, type="response")
          auc_val <- pROC::auc(d$y_next, d$PD_logit)
          
          tibble(
            year  = unique(d$YEAR),
            month = unique(d$MONTH),
            auc   = as.numeric(auc_val)
          )
        }) %>%
        mutate(date = as.Date(paste(year, month, "01", sep="-"))) %>%   # fecha real
        arrange(date)        
      
      
        ggplot(monthly_results, aes(x=date, y=auc, group=1)) +
          geom_line() + geom_point() +
          theme_minimal() +
          labs(title="Monthly AUC — Logistic Regression", x="Month", y="AUC") +
          scale_x_date(date_labels="%Y-%m", date_breaks="2 months") +
          theme(axis.text.x = element_text(angle=90, hjust=1))
      
    #Add PD in dataset___________________________________________________________________
      
      pd_results <- df %>%
        filter(!is.na(y_next)) %>%
        group_split(YEAR, MONTH) %>%
        map_df(function(d){
          if(nrow(d) < 100 | length(unique(d$y_next)) < 2) return(NULL)
          
          fit <- glm(f_logit, data = d, family = binomial())
          d$PD_logit <- predict(fit, newdata = d, type = "response")
          
          d %>%
            transmute(
              Id_client,
              Date,
              YEAR,
              MONTH,
              y_next,
              PD_logit
            )
        }) %>%
        arrange(Date, Id_client)
      
      # PD monthly to OG df
      df <- df %>%
        left_join(pd_results %>% select(Id_client, Date, PD_logit_month = PD_logit),
                  by = c("Id_client","Date"))
      
      # To excel
      writexl::write_xlsx(pd_results, "PD_logit_by_client_month.xlsx")
   
      
###############################################################################
# 5B) Model 2: Random Forest
################################################################################
    
      # Checking label dummy para ranger con probability=TRUE
        train <- train %>% mutate(y_next = factor(y_next, levels = c(0,1)))
        test  <- test  %>% mutate(y_next = factor(y_next, levels = c(0,1)))
      
      # Fórmula: using the same logit specification
        f_rf <- f_logit  # same specification of predictors
      
      # Ponderación por desbalance
        pos_rate <- mean(as.numeric(as.character(train$y_next)))
        class_w  <- c("0" = 0.5/(1 - pos_rate + 1e-9),
                    "1" = 0.5/(pos_rate + 1e-9))
      
        set.seed(202)  # reproducibilidad
        rf_fit <- ranger::ranger(
        formula         = f_rf,
        data            = train,
        num.trees       = 500,
        mtry            = floor(sqrt(ncol(train))),  # regla base
        min.node.size   = 20,
        sample.fraction = 0.8,
        importance      = "permutation",
        classification  = TRUE,
        probability     = TRUE,      # ¡para obtener PD!
        class.weights   = class_w,   # quita si no quieres ponderar
        seed            = 202
        )
      
      # PD prediction (class 1 probability)
        get_pd <- function(pred) {
        
        # ranger return 1,0 matrix
        if(is.matrix(pred$predictions)) pred$predictions[, "1"] else pred$predictions
        }
      
        pd_tr <- get_pd(predict(rf_fit, data = train, type = "response"))
        pd_te <- get_pd(predict(rf_fit, data = test,  type = "response"))
      
        train$PD_rf <- pd_tr
        test$PD_rf  <- pd_te
      
      # AUC / KS / PR
        auc_tr_rf <- pROC::auc(as.numeric(as.character(train$y_next)), train$PD_rf)
        auc_te_rf <- pROC::auc(as.numeric(as.character(test$y_next)),  test$PD_rf)
      
        roc_tr_rf <- pROC::roc(as.numeric(as.character(train$y_next)), train$PD_rf)
        roc_te_rf <- pROC::roc(as.numeric(as.character(test$y_next)),  test$PD_rf)
        ks_tr_rf  <- max(abs(roc_tr_rf$sensitivities - (1 - roc_tr_rf$specificities)))
        ks_te_rf  <- max(abs(roc_te_rf$sensitivities - (1 - roc_te_rf$specificities)))
      
        cat(sprintf("RF — AUC Train: %.3f | Test: %.3f | KS Train: %.3f | KS Test: %.3f\n",
                  auc_tr_rf, auc_te_rf, ks_tr_rf, ks_te_rf))
      
      # PR (test)
        pr_rf <- PRROC::pr.curve(
        scores.class0 = test$PD_rf[as.numeric(as.character(test$y_next))==1],
        scores.class1 = test$PD_rf[as.numeric(as.character(test$y_next))==0],
        curve = TRUE
        )
        plot(pr_rf, main = "PR curve — Random Forest (test)")
      
      # Importancia de variables (permutation)
        imp <- as.data.frame(rf_fit$variable.importance, stringsAsFactors = FALSE)
        colnames(imp) <- "Importance"
        imp$Variable <- rownames(imp)
        imp <- imp %>% arrange(desc(Importance))
        print(head(imp, 20))
        ggplot(imp %>% slice_max(Importance, n = 20),
             aes(x=reorder(Variable, Importance), y=Importance)) +
        geom_col() + coord_flip() +
        labs(title="Variable Importance — Random Forest", x=NULL, y="Permutation Importance") +
        theme_minimal()
      
     
      # Monthly review with RF (monthly AUC)
      # _______________________________________________________________________________________
      
        monthly_results_rf <- df %>%
        filter(!is.na(y_next)) %>%
        group_split(YEAR, MONTH) %>%
        purrr::map_df(function(d){
          if(nrow(d) < 100 | length(unique(d$y_next)) < 2) return(NULL)
          
      # asegurar niveles y tipos como en train
          for (v in c("SIZE","Program","Cred_Type","Sector","NPL")) {
          d[[v]] <- factor(d[[v]], levels = levels(train[[v]]), ordered = is.ordered(train[[v]]))
          }
          d$y_next <- factor(d$y_next, levels = c(0,1))
          
          fit_m <- ranger::ranger(
            formula       = f_rf,
            data          = d,
            num.trees     = 400,
            mtry          = floor(sqrt(ncol(d))),
            min.node.size = 20,
            classification= TRUE,
            probability   = TRUE,
            importance    = "none",
            seed          = 99
          )
          pd_m <- get_pd(predict(fit_m, data = d, type = "response"))
          
          auc_val <- pROC::auc(as.numeric(as.character(d$y_next)), pd_m)
          
          tibble(
            year  = unique(d$YEAR),
            month = unique(d$MONTH),
            auc   = as.numeric(auc_val)
          )
          }) %>%
          mutate(date = as.Date(paste(year, month, "01", sep = "-"))) %>%
          arrange(date)
      
        ggplot(monthly_results_rf, aes(x=date, y=auc, group=1)) +
        geom_line() + geom_point() +
        theme_minimal() +
        labs(title="Monthly AUC — Random Forest", x="Month", y="AUC") +
        scale_x_date(date_labels="%Y-%m", date_breaks="2 months") +
        theme(axis.text.x = element_text(angle=90, hjust=1))
      
      
      # Guardar PD por cliente-mes (RF mensual)
      # ________________________________________________________________________________________
        
        pd_results_rf <- df %>%
        filter(!is.na(y_next)) %>%
        group_split(YEAR, MONTH) %>%
        purrr::map_df(function(d){
          if(nrow(d) < 100 | length(unique(d$y_next)) < 2) return(NULL)
          
          for (v in c("SIZE","Program","Cred_Type","Sector","NPL")) {
            d[[v]] <- factor(d[[v]], levels = levels(train[[v]]), ordered = is.ordered(train[[v]]))
          }
          d$y_next <- factor(d$y_next, levels = c(0,1))
          
          fit_m <- ranger::ranger(
            formula       = f_rf,
            data          = d,
            num.trees     = 400,
            mtry          = floor(sqrt(ncol(d))),
            min.node.size = 20,
            classification= TRUE,
            probability   = TRUE,
            seed          = 101
          )
          pd_m <- get_pd(predict(fit_m, data = d, type = "response"))
          
          d %>%
            mutate(PD_rf = pd_m) %>%
            transmute(Id_client, Date, YEAR, MONTH,
                      y_next = as.integer(as.character(y_next)),
                      PD_rf)
        }) %>%
        arrange(Date, Id_client)
      
      # Add df (column PD_rf_month) export
        df <- df %>%
        left_join(pd_results_rf %>% select(Id_client, Date, PD_rf_month = PD_rf),
                  by = c("Id_client","Date"))
      
        write.csv(pd_results_rf, "PD_rf_by_client_month.csv", row.names = FALSE)
        write.csv(monthly_results_rf, "Monthly_AUC_RF.csv", row.names = FALSE)
      
      
  #Comparing models (log vs RF)___________________________________________________________       
    
        compare_monthly <- monthly_results %>%
        transmute(date, auc, model = "Logit") %>%
        bind_rows(monthly_results_rf %>% transmute(date, auc, model = "Random Forest"))
      
        ggplot(compare_monthly, aes(x = date, y = auc, color = model)) +
        geom_line() + geom_point() +
        theme_minimal() +
        labs(title = "Monthly AUC — Logit vs Random Forest", x = "Month", y = "AUC") +
        scale_x_date(date_labels = "%Y-%m", date_breaks = "2 months") +
        theme(axis.text.x = element_text(angle=90, hjust=1))
      
   #Anual comparission avg AUC YEAR for each models
      
      annual_auc <- compare_monthly %>%
        mutate(year = lubridate::year(date)) %>%
        group_by(model, year) %>%
        summarise(mean_auc = mean(auc, na.rm = TRUE), .groups = "drop")
        print(annual_auc)
      
    # Optimal THRESHOLD KS in test____________________________________________________________  
      
      # Find threshold that max KS
        
        df_thr <- tibble(
        thr  = seq(0, 1, by = 0.001),
        tpr  = map_dbl(thr, ~ mean(test$PD_rf[test$y_next=="1"] >= .x)),
        fpr  = map_dbl(thr, ~ mean(test$PD_rf[test$y_next=="0"] >= .x))
        ) %>% mutate(ks = tpr - fpr)
      
        best_thr <- df_thr$thr[which.max(df_thr$ks)]
        cat(sprintf("Best threshold (KS) = %.3f | KS = %.3f\n", best_thr, max(df_thr$ks)))
      
      # Matriz de  confusión en test con ese umbral
        
        pred_label <- ifelse(test$PD_rf >= best_thr, 1, 0)
        table(Pred = pred_label, Real = as.integer(as.character(test$y_next)))
      
      #Recalib curve 
      
        calib <- tibble(
        pd = test$PD_rf,
        y  = as.integer(as.character(test$y_next))
        ) %>%
        mutate(bin = ntile(pd, 10)) %>%
        group_by(bin) %>%
        summarise(
          pd_mean = mean(pd),
          rate    = mean(y),
          .groups = "drop"
        )
      
        ggplot(calib, aes(x = pd_mean, y = rate)) +
        geom_point() + geom_line() +
        geom_abline(slope = 1, intercept = 0, linetype = 2) +
        coord_equal() +
        labs(title = "Calibration — Random Forest (test)", x = "Predicted PD (bin mean)", y = "Observed default rate") +
        theme_minimal()
      
      #Tunning of RF________________________________________________________________________________
      
        train <- train %>% mutate(y_next = factor(y_next, levels = c(0,1))) #Labels and levels
        test  <- test  %>% mutate(y_next  = factor(y_next,  levels = c(0,1)))
      
      # predictors extractor function
      
        p <- length(attr(terms(f_rf), "term.labels"))
        mtry_default <- floor(sqrt(p))
      
      # Grid definition
        grid <- expand_grid(
          mtry          = c(mtry_default, max(1, round(1.5 * mtry_default))),
          min.node.size = c(10, 20, 50),
          num.trees     = c(300, 600)
        )
      
      # Monthly validation (last 6 months of train)
      
        train_months <- train %>% mutate(ym = floor_date(Date, "month")) %>% pull(ym) %>% unique() %>% sort()
        n_valid <- min(6, length(train_months) - 3)  #make sure that it is months to train
        valid_months <- tail(train_months, n_valid)
      
      # class weight fo unbalanced
        pos_rate <- mean(as.numeric(as.character(train$y_next)))
        class_w  <- c("0" = 0.5/(1 - pos_rate + 1e-9),
                    "1" = 0.5/(pos_rate + 1e-9))
      
        get_pd <- function(pred) {
        if (is.matrix(pred$predictions)) pred$predictions[, "1"] else pred$predictions
        } #get the pd of RF
      
      # Evaluation loop rolling-origin
        evals <- map_df(seq_len(nrow(grid)), function(i){
          pars <- grid[i, ]
          aucs  <- c()
        
          for (vm in valid_months) {
            # datos de validación = mes vm
            d_val <- train %>% filter(floor_date(Date, "month") == vm)
            # datos de entrenamiento = meses anteriores a vm
            d_trn <- train %>% filter(floor_date(Date, "month") <  vm)
            
            # seguridad: mismos niveles de factores
            for (v in c("SIZE","Program","Cred_Type","Sector","NPL")) {
              lv <- union(levels(d_trn[[v]]), levels(d_val[[v]]))
              d_trn[[v]] <- factor(d_trn[[v]], levels = lv, ordered = is.ordered(d_trn[[v]]))
              d_val[[v]] <- factor(d_val[[v]], levels = lv, ordered = is.ordered(d_val[[v]]))
            }
            
            fit <- ranger(
              formula         = f_rf,
              data            = d_trn,
              num.trees       = pars$num.trees,
              mtry            = pars$mtry,
              min.node.size   = pars$min.node.size,
              classification  = TRUE,
              probability     = TRUE,
              importance      = "none",
              class.weights   = class_w,
              seed            = 123
            )
            
            pd_val <- get_pd(predict(fit, data = d_val, type = "response"))
            y_val  <- as.numeric(as.character(d_val$y_next))
            auc_vm <- tryCatch(pROC::auc(y_val, pd_val), error = function(e) NA_real_)
            aucs   <- c(aucs, as.numeric(auc_vm))
          }
          
          tibble(
            mtry          = pars$mtry,
            min.node.size = pars$min.node.size,
            num.trees     = pars$num.trees,
            mean_auc      = mean(aucs, na.rm = TRUE),
            sd_auc        = sd(aucs,  na.rm = TRUE),
            n_valid       = length(aucs)
          )
        }) %>% arrange(desc(mean_auc))
        
        print(evals)
        
        # Choosing best
        best <- evals %>% slice(1)
        cat(sprintf("Best RF params -> mtry=%d, min.node.size=%d, num.trees=%d | mean AUC=%.3f\n",
                    best$mtry, best$min.node.size, best$num.trees, best$mean_auc))
        
        # 6) Retraining with best hiperparámetros y evaluation on test
        rf_best <- ranger(
          formula         = f_rf,
          data            = train,
          num.trees       = best$num.trees,
          mtry            = best$mtry,
          min.node.size   = best$min.node.size,
          classification  = TRUE,
          probability     = TRUE,
          importance      = "permutation",
          class.weights   = class_w,
          seed            = 2025
          )
        
          pd_tr_best <- get_pd(predict(rf_best, data = train, type = "response"))
          pd_te_best <- get_pd(predict(rf_best, data = test,  type = "response"))
        
          train$PD_rf_best <- pd_tr_best
          test$PD_rf_best  <- pd_te_best
        
          roc_tr_best <- pROC::roc(as.numeric(as.character(train$y_next)), train$PD_rf_best, quiet = TRUE)
          roc_te_best <- pROC::roc(as.numeric(as.character(test$y_next)),  test$PD_rf_best,  quiet = TRUE)
          auc_tr_best <- pROC::auc(roc_tr_best)
          auc_te_best <- pROC::auc(roc_te_best)
          ks_tr_best  <- max(abs(roc_tr_best$sensitivities - (1 - roc_tr_best$specificities)))
          ks_te_best  <- max(abs(roc_te_best$sensitivities - (1 - roc_te_best$specificities)))
        
          cat(sprintf("RF (best) — AUC Train: %.3f | Test: %.3f | KS Train: %.3f | KS Test: %.3f\n",
                    auc_tr_best, auc_te_best, ks_tr_best, ks_te_best))
        
        # Visualize ranking of combinations
          
          ggplot(evals, aes(x = interaction(mtry, min.node.size, num.trees, sep="/"), y = mean_auc)) +
          geom_col() + coord_flip() +
          coord_cartesian(ylim = c(0.981, 0.984)) +  # zoom
          geom_text(aes(label = sprintf("%.3f", mean_auc)), hjust = -0.1, size = 3) +
          labs(title="RF grid — mean AUC (rolling-origin)", x="mtry / min.node.size / num.trees", y="Mean AUC") +
          theme_minimal()
        
 #FINAL MODEL______________________________________________________________________       
        
        
          best <- evals %>% slice(1)  # already your top row
        
          rf_best <- ranger(
            formula         = f_rf,
            data            = train,
            num.trees       = best$num.trees,
            mtry            = best$mtry,
            min.node.size   = best$min.node.size,
            classification  = TRUE,
            probability     = TRUE,
            importance      = "permutation",
            class.weights   = class_w,
            seed            = 2025
          )
        
        # Evaluate on test
        
          get_pd <- function(pred) if(is.matrix(pred$predictions)) pred$predictions[, "1"] else pred$predictions
          test$PD_rf_best  <- get_pd(predict(rf_best, data = test,  type = "response"))
          train$PD_rf_best <- get_pd(predict(rf_best, data = train, type = "response"))
        
          roc_tr_best <- pROC::roc(as.numeric(as.character(train$y_next)), train$PD_rf_best, quiet=TRUE)
          roc_te_best <- pROC::roc(as.numeric(as.character(test$y_next)),  test$PD_rf_best,  quiet=TRUE)
          cat(sprintf("RF(best) AUC — Train: %.3f | Test: %.3f\n", pROC::auc(roc_tr_best), pROC::auc(roc_te_best)))
        
        # Redoing monthly AUC with best hiperparámeters
        
          monthly_results_rf_best <- df %>%
          filter(!is.na(y_next)) %>%
          group_split(YEAR, MONTH) %>%
          purrr::map_df(function(d){
            if(nrow(d) < 100 | length(unique(d$y_next)) < 2) return(NULL)
            for (v in c("SIZE","Program","Cred_Type","Sector","NPL")) {
              d[[v]] <- factor(d[[v]], levels = levels(train[[v]]), ordered = is.ordered(train[[v]]))
            }
            d$y_next <- factor(d$y_next, levels = c(0,1))
            fit_m <- ranger(
              formula       = f_rf,
              data          = d,
              num.trees     = best$num.trees,
              mtry          = best$mtry,
              min.node.size = best$min.node.size,
              classification= TRUE,
              probability   = TRUE,
              seed          = 99
            )
            pd_m <- get_pd(predict(fit_m, data = d, type = "response"))
            tibble(
              year  = unique(d$YEAR),
              month = unique(d$MONTH),
              auc   = as.numeric(pROC::auc(as.numeric(as.character(d$y_next)), pd_m))
            )
          }) %>%
          mutate(date = as.Date(paste(year, month, "01", sep="-"))) %>%
          arrange(date)
        
        # Plot
        
          ggplot(monthly_results_rf_best, aes(x=date, y=auc, group=1)) +
          geom_line() + geom_point() +
          theme_minimal() +
          labs(title="Monthly AUC — Random Forest (best params)", x="Month", y="AUC") +
          scale_x_date(date_labels="%Y-%m", date_breaks="2 months") +
          theme(axis.text.x = element_text(angle=90, hjust=1))
        
          # exportING BEST RESULTS CALIBRATION
          write.csv(monthly_results_rf_best, "Monthly_AUC_RF_best.csv", row.names = FALSE)
      
        
        #Brier Score______________________________________________________________________
        
          brier <- function(y, p) {
            y <- as.numeric(y)
            if (all(y %in% c(0,1)) == FALSE) y <- as.numeric(as.character(y))
            mean((p - y)^2, na.rm = TRUE)
          }
        
          evals_brier <- purrr::map_df(seq_len(nrow(grid)), function(i){
            pars <- grid[i, ]
            bs   <- c()
          
          for (vm in valid_months) {
            d_val <- train %>% dplyr::filter(lubridate::floor_date(Date,"month") == vm)
            d_trn <- train %>% dplyr::filter(lubridate::floor_date(Date,"month") <  vm)
            
            for (v in c("SIZE","Program","Cred_Type","Sector","NPL")) {
              lv <- union(levels(d_trn[[v]]), levels(d_val[[v]]))
              d_trn[[v]] <- factor(d_trn[[v]], levels = lv, ordered = is.ordered(d_trn[[v]]))
              d_val[[v]] <- factor(d_val[[v]], levels = lv, ordered = is.ordered(d_val[[v]]))
            }
            
            fit <- ranger::ranger(
              formula         = f_rf,
              data            = d_trn,
              num.trees       = pars$num.trees,
              mtry            = pars$mtry,
              min.node.size   = pars$min.node.size,
              classification  = TRUE,
              probability     = TRUE,
              class.weights   = class_w,
              seed            = 321
            )
            
            p_val <- get_pd(predict(fit, data = d_val, type = "response"))
            y_val <- as.numeric(as.character(d_val$y_next))
            bs_vm <- brier(y_val, p_val)
            bs    <- c(bs, bs_vm)
          }
          
          tibble(
            mtry          = pars$mtry,
            min.node.size = pars$min.node.size,
            num.trees     = pars$num.trees,
            mean_brier    = mean(bs, na.rm = TRUE),
            sd_brier      = sd(bs,   na.rm = TRUE),
            n_valid       = length(bs)
          )
        }) %>% arrange(mean_brier)  # menor es mejor
        
        print(evals_brier)
        
        ggplot(evals_brier,
               aes(x = interaction(mtry, min.node.size, num.trees, sep="/"), y = mean_brier)) +
          geom_col() + coord_flip() +
          coord_cartesian(ylim = range(evals_brier$mean_brier) + c(-1,1)*0.001) +
          geom_text(aes(label = sprintf("%.4f", mean_brier)), hjust = -0.1, size = 3) +
          labs(title="RF grid — mean Brier (rolling-origin)",
               x="mtry / min.node.size / num.trees", y="Mean Brier") +
          theme_minimal()
        
        #TRAIN THE BEST AND EVALUATE ON TEST
        
        best_b <- evals_brier %>% slice(1)
        cat(sprintf("Best-by-Brier -> mtry=%d, min.node.size=%d, num.trees=%d | mean Brier=%.5f\n",
                    best_b$mtry, best_b$min.node.size, best_b$num.trees, best_b$mean_brier))
        
        rf_best_brier <- ranger::ranger(
          formula         = f_rf,
          data            = train,
          num.trees       = best_b$num.trees,
          mtry            = best_b$mtry,
          min.node.size   = best_b$min.node.size,
          classification  = TRUE,
          probability     = TRUE,
          importance      = "permutation",
          class.weights   = class_w,
          seed            = 2026
        )
        
        train$PD_rf_b  <- get_pd(predict(rf_best_brier, data = train, type="response"))
        test$PD_rf_b   <- get_pd(predict(rf_best_brier,  data = test,  type="response"))
        
        brier_tr <- brier(as.numeric(as.character(train$y_next)), train$PD_rf_b)
        brier_te <- brier(as.numeric(as.character(test$y_next)),  test$PD_rf_b)
        
        roc_tr_b <- pROC::roc(as.numeric(as.character(train$y_next)), train$PD_rf_b, quiet=TRUE)
        roc_te_b <- pROC::roc(as.numeric(as.character(test$y_next)),  test$PD_rf_b,  quiet=TRUE)
        
        cat(sprintf("RF (best-by-Brier) — Brier Train: %.5f | Test: %.5f | AUC Train: %.3f | Test: %.3f\n",
                    brier_tr, brier_te, pROC::auc(roc_tr_b), pROC::auc(roc_te_b)))
        
        #Monthly Brier for RF (best-by-Brier)
        
        monthly_brier_rf <- df %>%
          filter(!is.na(y_next)) %>%
          group_split(YEAR, MONTH) %>%
          purrr::map_df(function(d){
            if(nrow(d) < 100 | length(unique(d$y_next)) < 2) return(NULL)
            
            for (v in c("SIZE","Program","Cred_Type","Sector","NPL")) {
              d[[v]] <- factor(d[[v]], levels = levels(train[[v]]), ordered = is.ordered(train[[v]]))
            }
            d$y_next <- factor(d$y_next, levels = c(0,1))
            
            fit_m <- ranger::ranger(
              formula       = f_rf,
              data          = d,
              num.trees     = best_b$num.trees,
              mtry          = best_b$mtry,
              min.node.size = best_b$min.node.size,
              classification= TRUE,
              probability   = TRUE,
              seed          = 77
            )
            p_m <- get_pd(predict(fit_m, data = d, type="response"))
            y_m <- as.numeric(as.character(d$y_next))
            
            tibble(
              year  = unique(d$YEAR),
              month = unique(d$MONTH),
              brier = brier(y_m, p_m)
            )
          }) %>%
          mutate(date = as.Date(paste(year, month, "01", sep="-"))) %>%
          arrange(date)
        
        ggplot(monthly_brier_rf, aes(x = date, y = brier, group = 1)) +
          geom_line() + geom_point() +
          theme_minimal() +
          labs(title="Monthly Brier — Random Forest (best-by-Brier)", x="Month", y="Brier") +
          scale_x_date(date_labels="%Y-%m", date_breaks="2 months") +
          theme(axis.text.x = element_text(angle=90, hjust=1))
        
        
        #Comparing Logit vs BF Brier
        
        brier_logit_test <- brier(as.numeric(as.character(test$y_next)), test$PD_logit)
        brier_rf_test    <- brier(as.numeric(as.character(test$y_next)), test$PD_rf_b)
        cat(sprintf("Test Brier — Logit: %.5f | RF(best-by-Brier): %.5f\n",
                    brier_logit_test, brier_rf_test))
        
        
        
      #CALIBRATION CURVES_____________________________________________________________________
        
        # Helper: build calibration table
          calibration_table <- function(df, pd_col, y_col, n_bins = 10) {
          df %>%
            mutate(
              pd = !!sym(pd_col),
              y  = as.numeric(as.character(!!sym(y_col))),
              bin = ntile(pd, n_bins)   # split into deciles (default=10 bins)
            ) %>%
            group_by(bin) %>%
            summarise(
              pd_mean   = mean(pd, na.rm=TRUE),
              obs_rate  = mean(y, na.rm=TRUE),
              n         = n(),
              .groups="drop"
            )
          }
        
        # Calibration tables for Logit and RF
          calib_logit <- calibration_table(test, "PD_logit", "y_next") %>% mutate(model="Logit")
          calib_rf    <- calibration_table(test, "PD_rf_b", "y_next")  %>% mutate(model="RF")
        
          calib_all <- bind_rows(calib_logit, calib_rf)
        
        # Plot calibration curves
          ggplot(calib_all, aes(x=pd_mean, y=obs_rate, color=model)) +
            geom_point(size=3) + geom_line() +
            geom_abline(slope=1, intercept=0, linetype=2, color="black") +
            scale_x_continuous(limits=c(0,max(calib_all$pd_mean)*1.1)) +
            scale_y_continuous(limits=c(0,max(calib_all$obs_rate)*1.1)) +
            labs(title="Calibration Curve — Test Set",
               x="Predicted PD (bin mean)", y="Observed Default Rate") +
            theme_minimal()
        

##############################################################################################        
#6 BETA REGRESSION LGD and GRADIENT BOOSTING
############################################################################################
  
  #Beta________________________________________________________________________________________            
    
        # Preprocesamiento: LGD en [0,1]
          
          df <- df %>%
            mutate(LGD_ratio = LGD / 100) %>%
            filter(!is.na(LGD_ratio))
        
        # Ajuste (ejemplo con pocas variables)
        
          f_beta <- as.formula("LGD_ratio ~ Utilization + Debt_to_Assets + log_EAI + log_Loan + Score")
        
          beta_fit <- betareg(f_beta, data=df, link="logit")
        
          summary(beta_fit)
        
        # Predicción (en %)
          
          df$LGD_pred_beta <- predict(beta_fit, newdata=df, type="response") * 100
        
        #betareg use link logit and direct modeling the Beta distribution.
        #interpretation (coeficients, efects) is perfect.
        #I use R² pseudo, RMSE, log-likelihood. 
        
  #Gradient Boosting_____________________________________________________________________________      
        
        # LGD tratada como un problema de regresión gaussian since LGD is not 1 or 0.
        # Regulatory LGD Calculation (In Mexico is mandatory, I follow the law to establish LGD)
        
        # Transform LGD a [0,1]
          
          df <- df %>% mutate(LGD_ratio = pmin(pmax(LGD/100, 0), 1)) %>% filter(!is.na(LGD_ratio))

        
          set.seed(2025)
          gbm_fit <- gbm(
            LGD_ratio ~ Utilization + Debt_to_Assets + log_EAI + log_Loan + Score + SIZE + Sector,
            data = df,
            distribution = "gaussian",    # <- NO bernoulli
            n.trees = 3000,
            interaction.depth = 3,
            shrinkage = 0.01,
            bag.fraction = 0.8,
            train.fraction = 0.8,
            n.cores = 1                   # optional on Windows don't run GPUs
            )
        
        # Select optimal number of trees for OOB/inten validation
          
          best_iter <- gbm.perf(gbm_fit, method = "OOB", plot.it = FALSE)
        
        # Prediction [0,1]
          
          df$LGD_pred_gbm <- predict(gbm_fit, newdata = df, n.trees = best_iter, type = "response")
          df$LGD_pred_gbm <- pmin(pmax(df$LGD_pred_gbm, 0), 1) * 100  # en %
        
        # Checking
          
          print(summary(gbm_fit, n.trees = best_iter))
        
          rmse <- function(y, p) sqrt(mean((p - y)^2))
          mae  <- function(y, p) mean(abs(p - y))
        
          lgd_true <- df$LGD_ratio * 100
          lgd_gbm  <- df$LGD_pred_gbm
        
          cat(sprintf("RMSE=%.3f | MAE=%.3f\n", rmse(lgd_true, lgd_gbm), mae(lgd_true, lgd_gbm)))
        
        #GBM is flexible and capture non linearity and interactions.
        #I use RMSE, MAE, R²
      
        
##############################################################################################        
#7 EAI Expected Annual Income / Expected Annual Interest
############################################################################################        
        
        #Remove clients with zero amount (churn)
        
        # Variables definition
        
        vars <- c("EAI","Utilization","Debt_to_Assets","log_EAI","log_Loan","Score","SIZE","Sector")
        
        # complete clients no NA 
          
        idx <- complete.cases(df[, vars])
        
        # 3) linear reg with no churn
        lm_fit <- lm(EAI ~ Utilization + Debt_to_Assets + log_EAI + log_Loan + Score + SIZE + Sector,
                     data = df[idx, ])
        
        summary(lm_fit)
        
        # 4) Predict
        pred_clean <- predict(lm_fit, newdata = df[idx, ])
        
        # 5) make the column on dataset df
        df$EAI_pred_lm <- NA_real_
        df$EAI_pred_lm[idx] <- pred_clean
        
        # 6) Métrics 
        rmse <- function(y, p) sqrt(mean((p - y)^2))
        mae  <- function(y, p) mean(abs(p - y))
        
        cat(sprintf("Linear Model — RMSE: %.3f | MAE: %.3f\n",
                    rmse(df$EAI[idx], df$EAI_pred_lm[idx]),
                    mae(df$EAI[idx], df$EAI_pred_lm[idx])))
        
        
##################################################################################################        
#8 EXPECTED LOSS CALCULATION
#########################################################################################
        
        
        # EL_i = PD_i * LGD_i * EAD_i
        # where:
        #   - PD: usa RF mensual si existe, luego Logit mensual, luego Logit (o PD_Reg/100)
        #   - LGD: usa GBM si existe, luego Beta, luego LGD (% regulatoria)/100
        #   - EAD: 2 ways
        #       i.Principal expuesto: use Capital
        #       ii.Ingreso esperado (tipo interés): use EAI_pred_lm if NA then EAI

        # Normalization to [0,1] of all the variables %
        to_prob <- function(x) {
          if (is.null(x)) return(NULL)
          if (any(x > 1, na.rm = TRUE)) x <- x / 100
          pmin(pmax(x, 0), 1)
        }
        
        # 1) Expected Loss by client and adding to main dataset df
        # ____________________________________________________________________________
        
        df <- df %>%
          mutate(
            ym = floor_date(Date, "month"),
            
            # PDs (all on [0,1])
            PD_logit = to_prob(PD_logit_month),
            PD_rf    = to_prob(PD_rf_month),
            PD_reg   = to_prob(PD_Reg),
            
            # LGD reg (Regulatory Standard Approach) vs “Predicted” (Beta), en [0,1]
            LGD_reg  = to_prob(LGD),
            LGD_pred_beta  = to_prob(LGD_pred_beta),
            
            # EAD (i, ii)
            EAD_principal = pmax(coalesce(Capital, Loan_Amount_Local), 0),
            EAD_income    = pmax(coalesce(EAI_pred_lm, EAI), 0),
            
            # EL by client — Regulatory
            EL_reg_principal   = PD_reg   * LGD_reg * EAD_principal,
            EL_reg_income      = PD_reg   * LGD_reg * EAD_income,
            
            # EL by client — Internal model (Logit y RF) with LGD of beta pred
            
            EL_logit_principal = PD_logit * LGD_pred_beta * EAD_principal,
            EL_rf_principal    = PD_rf    * LGD_pred_beta * EAD_principal,
            EL_logit_income    = PD_logit * LGD_pred_beta * EAD_income,
            EL_rf_income       = PD_rf    * LGD_pred_beta * EAD_income
          )
        
        # Exporting
        
         write.csv(df %>% select(Id_client, Date, ym,
          PD_reg, PD_logit, PD_rf, LGD_reg, LGD_use,
          EAD_principal, EAD_income,
          EL_reg_principal, EL_logit_principal, EL_rf_principal,
          EL_reg_income,    EL_logit_income,    EL_rf_income),
          "EL_by_client.csv", row.names = FALSE)
        
        
        # 2) Monthly summary (totals and ratios)
        # ______________________________________________________________________________
        
        monthly_summary <- df %>%
          group_by(ym) %>%
          summarise(
            
            # Totals EL
            EL_reg_principal   = sum(EL_reg_principal,   na.rm = TRUE),
            EL_logit_principal = sum(EL_logit_principal, na.rm = TRUE),
            EL_rf_principal    = sum(EL_rf_principal,    na.rm = TRUE),
            EL_reg_income      = sum(EL_reg_income,      na.rm = TRUE),
            EL_logit_income    = sum(EL_logit_income,    na.rm = TRUE),
            EL_rf_income       = sum(EL_rf_income,       na.rm = TRUE),
            
            # Totals EAD
            
            EAD_principal = sum(EAD_principal, na.rm = TRUE),
            EAD_income    = sum(EAD_income,    na.rm = TRUE),
            .groups = "drop"
          ) %>%
          mutate(
            
            # Ratios ELR (EL / EAD)
            
            ELR_reg_principal   = EL_reg_principal   / EAD_principal,
            ELR_logit_principal = EL_logit_principal / EAD_principal,
            ELR_rf_principal    = EL_rf_principal    / EAD_principal,
            ELR_reg_income      = EL_reg_income      / EAD_income,
            ELR_logit_income    = EL_logit_income    / EAD_income,
            ELR_rf_income       = EL_rf_income       / EAD_income
          ) %>%
          arrange(ym)
        
        print(head(monthly_summary, 12))
        
        #Exporting
        
        write.csv(monthly_summary, "EL_monthly_summary.csv", row.names = FALSE)
        
        
        # 3) Summary o portafolio (total and ratios finals)
        # ________________________________________________________________________________
        EL_portfolio <- tibble(
          Metric = c("EL_reg_principal","EL_logit_principal","EL_rf_principal",
                     "EL_reg_income","EL_logit_income","EL_rf_income"),
          Value  = c(sum(df$EL_reg_principal,   na.rm=TRUE),
                     sum(df$EL_logit_principal, na.rm=TRUE),
                     sum(df$EL_rf_principal,    na.rm=TRUE),
                     sum(df$EL_reg_income,      na.rm=TRUE),
                     sum(df$EL_logit_income,    na.rm=TRUE),
                     sum(df$EL_rf_income,       na.rm=TRUE))
        ) %>%
          tidyr::pivot_wider(names_from = Metric, values_from = Value) %>%
          mutate(
            EAD_principal = sum(df$EAD_principal, na.rm=TRUE),
            EAD_income    = sum(df$EAD_income,    na.rm=TRUE),
            ELR_reg_principal   = EL_reg_principal   / EAD_principal,
            ELR_logit_principal = EL_logit_principal / EAD_principal,
            ELR_rf_principal    = EL_rf_principal    / EAD_principal,
            ELR_reg_income      = EL_reg_income      / EAD_income,
            ELR_logit_income    = EL_logit_income    / EAD_income,
            ELR_rf_income       = EL_rf_income       / EAD_income
          )
        
        print(EL_portfolio)
        
        # exporting portfolio summary
         write.csv(EL_portfolio, "EL_portfolio_summary.csv", row.names = FALSE)
        
  
        # 4)Plots monthly ELR (principal)
        # ____________________________________________________________________________________
        
        # Comparation Regulatory vs Logit vs RF
        
         ggplot(monthly_summary, aes(x = ym)) +
          geom_line(aes(y = ELR_reg_principal,   color = "Regulatorio")) +
          geom_point(aes(y = ELR_reg_principal,  color = "Regulatorio")) +
          geom_line(aes(y = ELR_logit_principal, color = "Logit")) +
          geom_point(aes(y = ELR_logit_principal, color = "Logit")) +
          geom_line(aes(y = ELR_rf_principal,    color = "RF")) +
          geom_point(aes(y = ELR_rf_principal,   color = "RF")) +
          scale_y_continuous(labels = scales::percent_format(accuracy = 0.01)) +
          labs(title = "EL Ratio (principal) — Reg vs Logit vs RF",
               x = "Mes", y = "EL / EAD", color = "Modelo PD") +
          theme_minimal()
        
        
        
#################################################################################################
#9. Unexpected Losses (UL) Dependencies
        

    #Function annualize
    # _________________________________________________________________________________________
        
        
        to_prob <- function(x) { if (is.null(x)) return(NULL); if (any(x > 1, na.rm=TRUE)) x <- x/100; pmin(pmax(x,0),1) }
        annualize_monthly_pd <- function(p_m) { p_m <- to_prob(p_m); 1 - (1 - p_m)^12 }  # ~constant on year
        
        
        # Mapa PD(1y) -> Rating (this part is adjustable)
        
        rating_from_pd1y <- function(p1y){
          p1y <- pmin(pmax(p1y, 0), 0.999999)
          # AAA, AA, A, BBB, BB, B, CCC   (D final state)
          breaks <- c(0, 0.0005, 0.0015, 0.005, 0.015, 0.05, 0.15, 1) # en prob
          labels <- c("AAA","AA","A","BBB","BB","B","CCC")
          cut(p1y, breaks=breaks, labels=labels, include.lowest=TRUE, right=TRUE)
        }
        
        # Dada una matriz de transición 1y (rows=start, cols=end), genera umbrales probit por fila
        
        make_thresholds <- function(tmat, ratings){
          stopifnot(all(rownames(tmat)==ratings), all(colnames(tmat)==ratings))
          
          # Para cada fila, use cumsum hasta la penúltima col; qnorm(1)=Inf con findInterval
          
          thr <- lapply(ratings, function(r){
            row <- as.numeric(tmat[r, ])
            cs  <- cumsum(row)
            qnorm(cs[-length(cs)])  # umbrales intermedios
          })
          names(thr) <- ratings
          thr
        }
        
        apply_transition <- function(A, rating_start, thresholds, ratings_levels){
          n <- length(A)
          out <- character(n)
          for (i in seq_len(n)) {
            r <- as.character(rating_start[i])
            thr <- thresholds[[r]]
            if (is.null(thr)) {
              stop(sprintf("No existen umbrales para el rating inicial '%s'. Revisa nombres/levels.", r))
            }
            j <- findInterval(A[i], thr) + 1  # 1..K
            out[i] <- ratings_levels[j]
          }
          factor(out, levels = ratings_levels)
        }
        
        # 1) MATRIX DE TRANSICIÓN 1-AÑO
        # ____________________________________________________________________________________
        
        # This matrix is and example, the markov transition is more plausible taking
        # underdevelop countries. In this case is a really healthy portfolio
        
        
          ratings <- c("AAA","AA","A","BBB","BB","B","CCC","D")
        
          tmat <- matrix(c(
          #   AAA     AA      A     BBB     BB      B     CCC     D
          0.92,  0.06,  0.01,  0.00,  0.00,  0.00,  0.00,  0.01,  # AAA
          0.02,  0.91,  0.05,  0.01,  0.00,  0.00,  0.00,  0.01,  # AA
          0.00,  0.04,  0.89,  0.06,  0.00,  0.00,  0.00,  0.01,  # A
          0.00,  0.00,  0.06,  0.86,  0.06,  0.01,  0.00,  0.01,  # BBB
          0.00,  0.00,  0.00,  0.07,  0.82,  0.08,  0.01,  0.02,  # BB
          0.00,  0.00,  0.00,  0.01,  0.08,  0.80,  0.05,  0.06,  # B
          0.00,  0.00,  0.00,  0.00,  0.02,  0.08,  0.72,  0.18,  # CCC
          0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  1.00   # D (absorbing)
        ), nrow=8, byrow=TRUE)
        rownames(tmat) <- ratings; colnames(tmat) <- ratings
        
        # Checking
          
          stopifnot(all(abs(rowSums(tmat)-1) < 1e-8))
        
        # Umbrales probit por rating inicial
        thr_list <- make_thresholds(tmat, ratings)
        
        # SPREADS y DURACIONES por rating (para MTM no‑default)
        # ___________________________________________________________________________
        
        spread_by_rating <- c(
          AAA = 0.005, AA = 0.007, A = 0.010, BBB = 0.015,
          BB  = 0.025, B  = 0.040, CCC = 0.070, D = 1.00 # D don't use on MTM
        )
        duration_by_rating <- c(
          AAA = 4.5, AA = 4.5, A = 4.3, BBB = 4.0,
          BB  = 3.5, B  = 3.0, CCC = 2.0, D = 0.0
        )
        
        #EAD and LGD by crédit + initial rating from PD
        #    I use coalesce in dyplyr because I keep forgetting the names of my PD (not necessary): PD_Reg / PD_logit_month / PD_rf_month
        #_________________________________________________________________________________________
        
        # Check EAD/LGD consistentcy
        
        df <- df %>%
          mutate(
            EAD_principal = pmax(dplyr::coalesce(EAD_principal, Capital, Loan_Amount_Local), 0),
            LGD_sim       = to_prob(dplyr::coalesce(LGD_pred_beta, LGD))
          )
        
        # Choose PD monthly for initial rating
        
        pd_month_for_rating <- to_prob(dplyr::coalesce(df$PD_logit_month,df$PD_Reg,df$PD_rf_month))
        #First I take PD_logit (is the best choice on this project until now)
        
        # PD 1 year and initial rating
        
        pd_1y <- annualize_monthly_pd(pd_month_for_rating)
        rating_init <- rating_from_pd1y(pd_1y)
        
        # No NA and without D as initial rating
        
        rating_init <- addNA(rating_init)
        if (any(is.na(rating_init))) {
          
          #IF NA exist, goes to BBB by default
          
          rating_init[is.na(rating_init)] <- factor("BBB", levels=levels(rating_init))
        }
        rating_init <- droplevels(rating_init)
        levels(rating_init) <- intersect(levels(rating_init), ratings)
        rating_init <- factor(as.character(rating_init), levels=ratings[ratings!="D"]) # sin D al inicio
        
        # Exposition for simulation
        
        expo <- df %>%
          transmute(
            Id_client,
            EAD = EAD_principal,
            LGD = LGD_sim,
            Rating0 = rating_init
          ) %>%
          filter(!is.na(Rating0), EAD > 0, !is.na(LGD))
        
        stopifnot(nrow(expo) > 0)
        
        # make sure all initial ratings exist on the thresholds
        
          missing_r <- setdiff(unique(as.character(expo$Rating0)), names(thr_list))
          if (length(missing_r) > 0) {
          stop(sprintf("Faltan umbrales para ratings iniciales: %s", paste(missing_r, collapse=", ")))
          }
        
        # Make sure names on spread/duration in ratings
        missing_s <- setdiff(unique(as.character(expo$Rating0)), names(spread_by_rating))
        missing_d <- setdiff(unique(as.character(expo$Rating0)), names(duration_by_rating))
        if (length(missing_s) > 0 || length(missing_d) > 0) {
          stop(sprintf("Faltan spreads o duraciones para: spreads=%s | durations=%s",
                       paste(missing_s, collapse=", "), paste(missing_d, collapse=", ")))
        }
        
        # AVOID "D" RATING
        expo <- expo %>% filter(Rating0 %in% names(thr_list), Rating0 != "D")
        stopifnot(nrow(expo) > 0)
        
        
        #SIMULATION WITh gaussian factor (crosssectors)
        #_________________________________________________________________________________

        simulate_losses <- function(expo,
                                    n_sims      = 100, #Choose 100 for simplicity on simulation
                                    rho         = 0.15,
                                    ratings_lvls,
                                    thr_list_in,
                                    spread_tbl,
                                    duration_tbl,
                                    keep_gains  = TRUE) {
          
          n <- nrow(expo)
          
          # Useful index about initial rating
          
          spread_start <- spread_tbl[as.character(expo$Rating0)]
          dur_start    <- duration_tbl[as.character(expo$Rating0)]
          
          # Pre-checking
          
          if (any(is.na(spread_start)) || any(is.na(dur_start))) {
            stop("Faltan spreads/durations para algunos ratings iniciales.")
          }
          
          port_losses <- numeric(n_sims)
          
          for (s in seq_len(n_sims)) {
            #common factor + idios
            
            Z  <- rnorm(1)
            ei <- rnorm(n)
            A  <- sqrt(rho)*Z + sqrt(1 - rho)*ei   # shock con correlación
            
            #final rating by thresholds (use arguments *_in / *_lvls)
            
            rating_end <- apply_transition(A, expo$Rating0, thr_list_in, ratings_lvls)
            
            # Losses__________________________________________________________________________
            
            spread_end <- spread_tbl[as.character(rating_end)]
            is_default <- (rating_end == "D")
            
            # MTM for no default
            
            ds <- (spread_end - spread_start)      # Δspread
            mtm_loss <- dur_start * ds * expo$EAD
            if (!keep_gains) mtm_loss <- pmax(mtm_loss, 0)  # solo pérdidas (opcional)
            
            # Default
            
            def_loss <- ifelse(is_default, expo$LGD * expo$EAD, 0)
            
            # losses each credit
            
            loss_i <- ifelse(is_default, def_loss, mtm_loss)
            
            # losses portfolio
            port_losses[s] <- sum(loss_i, na.rm = TRUE)
            }
          
            port_losses
            }
        
          # Parámeters of the simulation
        
          set.seed(1999)
          N_SIM <- 100 # Again I choose 100 for simplicity
          RHO   <- 0.20      # asset correlation
        
          losses <- simulate_losses(
            expo,
            n_sims      = N_SIM,
            rho         = RHO,
            ratings_lvls= ratings,
            thr_list_in = thr_list,
            spread_tbl  = spread_by_rating,
            duration_tbl= duration_by_rating,
            keep_gains  = FALSE   # TRUE if see gains
          )
        
        
        # EL(sim), VaR, Unexpected Losses, ECAP
        #_________________________________________________________________________________
        
        
        EL_sim   <- mean(losses)
        VaR_999  <- as.numeric(quantile(losses, probs = 0.999))
        UL_999   <- VaR_999 - EL_sim
        ECAP_999 <- UL_999  # económica a 99.9%
        
        cat(sprintf("\nSimulaciones: %d | rho=%.2f\n", N_SIM, RHO))
        cat(sprintf("EL(sim)     : %.2f\n", EL_sim))
        cat(sprintf("VaR 99.9%%   : %.2f\n", VaR_999))
        cat(sprintf("UL (99.9%%) : %.2f\n", UL_999))
        cat(sprintf("ECap 99.9%% : %.2f\n", ECAP_999))
        
        # Export distribution
         
        write.csv(data.frame(loss=losses), "UL_losses_distribution.csv", row.names=FALSE)
        
        # Plotting
        
        if (requireNamespace("ggplot2", quietly = TRUE)) {
          library(ggplot2)
          ggplot(data.frame(loss=losses), aes(x=loss)) +
            geom_histogram(bins=60) +
            geom_vline(xintercept = VaR_999, linetype=2) +
            labs(title = "Distribución de pérdidas simuladas",
                 subtitle = sprintf("VaR 99.9%% = %.0f | EL = %.0f", VaR_999, EL_sim),
                 x="Pérdida de portafolio", y="Frecuencia") +
            theme_minimal()
        }
        
        
        