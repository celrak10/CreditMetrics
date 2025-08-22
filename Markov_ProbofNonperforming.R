

library(tibble)
library(writexl)
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(purrr)
library(stringr)


Migration<- read_excel("DataMigration.xlsx")
tm_file <- "TransitionMatrix.xlsx"    


# Datasets________________________________________________________________________

  Migration <- read_excel("DataMigration.xlsx") %>%
    mutate(
    start_date = ymd(`Nuevo en`),     # transforms AAAA-MM-DD a Date, in dataset sometimes dont recognize my excel format
    start_state = as.integer(NPL)     
    )

#Horizon_________________________________________________________________________________

    horizon <- seq(ymd("2022-01-01"), ymd("2024-12-01"), by = "1 month")
    Tn <- length(horizon)


  # --- Función para encontrar la columna de inicio de cada cliente ---
  # (primera columna cuyo mes >= start_date; NA si todo el horizonte es anterior)
  start_col <- function(d) {
  idx <- which(horizon >= d)[1]
  if (is.na(idx)) return(NA_integer_)
  idx
  }

  # Vectorizamos para todo el DF
  Migration$start_col <- vapply(Migration$start_date, start_col, integer(1))


# ---- READ PROBABILITY MATRIX (Sheet1 of DataMigration) ----
  # The numeric block I use is in C3:AE27 and row labels (state 0..25) are in B3:B27,
  # column labels (0..25) are in C2:AE2. Adjust the range if your file differs.
  
  P_raw <- read_excel(tm_file, sheet = "Sheet1", range = "C2:AD27", col_names = FALSE)
  row_lab <- read_excel(tm_file, sheet = "Sheet1", range = "B2:B27", col_names = FALSE)[[1]]
  col_lab <- read_excel(tm_file, sheet = "Sheet1", range = "C1:AD1", col_names = FALSE)[1,] %>% unlist()

  P <- as.matrix(P_raw) |> `storage.mode<-`("double")
  rownames(P) <- as.character(row_lab)   # "0","1",...,"25"
  colnames(P) <- as.character(col_lab)   # "0","1",...,"25"

# Quick check each row sums ~1
  
  row_ok <- which(abs(rowSums(P) - 1) > 1e-6)
  if(length(row_ok)) warning("Rows not summing to 1: ", paste(row_ok, collapse=", "))

# Cumulative matrix (Sheet2) ----
  
  Pc_raw <- read_excel(tm_file, sheet = "Sheet2", range = "C2:AD27", col_names = FALSE)
  Pc <- as.matrix(Pc_raw) |> `storage.mode<-`("double")
  rownames(Pc) <- as.character(row_lab)
  colnames(Pc) <- as.character(col_lab)


# Simulation_______________________________________________________________________________
  
  sample_next_state <- function(curr_state, Pc, P){
  u <- runif(length(curr_state))
  rows <- if(!is.null(Pc)) Pc[as.character(curr_state), , drop=FALSE]
  else t(apply(P[as.character(curr_state), , drop=FALSE], 1, cumsum))
  next_idx <- findInterval(u, rows) + 1
  as.integer(colnames(rows)[next_idx])
  }


  simulate_paths <- function(df, horizon, Pc=NULL, P=NULL){
    N <- nrow(df); Tn <- length(horizon)
    states <- matrix(NA_integer_, nrow=N, ncol=Tn,
                   dimnames=list(df$Id_client, format(horizon, "%b-%y")))
    for(i in seq_len(N)){
      sc <- df$start_col[i]
      if(is.na(sc)) next                      # empieza después del horizonte → todo NA
      states[i, sc] <- df$start_state[i]      # estado en el mes de arranque
      if(sc < Tn){
      for(t in (sc+1):Tn){
        prev <- states[i, t-1]
        states[i, t] <- sample_next_state(prev, Pc, P)
      }
      }
    }
    states
  }


#CLEAN TRANSITION MATRIX (replace NA by 0, renormalize, cumulate)__________________________

  # Start from the raw probability matrix P you already loaded
  
  P_clean <- P
  P_clean[is.na(P_clean)] <- 0                     # impossible = 0%, not NA

  P_clean["25", ] <- 0; P_clean["25", "25"] <- 1   #Absorbing state

  # Re normalize rows to sum to 1 (in case your sheet has rounding zeros/edits)
  
  rs <- rowSums(P_clean)
  P_clean <- sweep(P_clean, 1, rs, "/")

  # Build a cumulative matrix that is non-decreasing and ends at 1
  
  Pc_clean <- t(apply(P_clean, 1, cumsum))
  Pc_clean <- pmin(Pc_clean, 1)                   # guard against tiny >1
  Pc_clean[, ncol(Pc_clean)] <- 1                 # force last column to 1 exactly


##################################################################################################
#RUNNING MARKOV TRANSITION PROBABILITIES ON NPL
#############################################################################################

  
  set.seed(123)

  #i. Run the Markov simulation using the cumulative matrix Pc
  states <- simulate_paths(Migration, horizon, Pc = Pc_clean, P = NULL)

  # quick sanity checks
  dim(states)                         # N x T
  table(states[, 1], useNA = "ifany") # distribution in first simulated month per client
  any(is.na(states))                  # some NAs allowed (clients starting after horizon)

  #ii Tidy result to long form (Id_client, date, state)

    sim_long <-
    as.data.frame(states) |>
    tibble::rownames_to_column("Id_client") |>
    tidyr::pivot_longer(
    cols = -Id_client,
    names_to = "month_lbl",
    values_to = "state"
    ) |>
    mutate(
    # month labels were set as format(horizon, "%b-%y"); parse back to a Date:
    date = lubridate::my(month_lbl),          # first day of month
    state = as.integer(state)
    ) |>
    select(Id_client, date, state) |>
    arrange(Id_client, date)

  #iii Join any client attributes you need (loan amount, type, etc.)
    
    sim_long <- sim_long %>%
    mutate(Id_client = as.numeric(Id_client))


  ##iiii Join attributes and build a working table (sim_full) ---

    # make sure both sides use the same Id type
      
      Migration <- Migration %>% mutate(Id_client = as.numeric(Id_client))
      sim_long  <- sim_long  %>% mutate(Id_client = as.numeric(Id_client))

      sim_full <- sim_long %>%
      left_join(
      Migration %>%
      select(Id_client, Loan_Amount_Local, Cred_Type, start_col),
      by = "Id_client"
      ) %>%
      # match each row’s date to the horizon index (1..Tn)
      mutate(month_idx = match(date, horizon))

  ##Simple EAD policy example ---
    # - Simple: 100% draw only in the start month
    # - Revolvent: constant exposure = loan amount

    sim_full <- sim_full %>%
    group_by(Id_client) %>%
    mutate(
    EAD = case_when(
      Cred_Type == "Simple"    ~ if_else(month_idx == start_col, Loan_Amount_Local, 0),
      Cred_Type == "Revolvent" ~ Loan_Amount_Local,
      TRUE ~ 0
    )
    ) %>%
    ungroup()

  ##Portfolio metrics (example: NPL = states >= 3) ---

    npl_cut <- 3L  # adjust if your definition differs

    port_monthly <- sim_full %>%
    group_by(date) %>%
    summarise(
      total_EAD = sum(EAD, na.rm = TRUE),
      NPL_EAD   = sum(EAD[state >= npl_cut], na.rm = TRUE),
      cur_EAD   = sum(EAD[state == 0], na.rm = TRUE),
      .groups = "drop"
      ) %>%
      mutate(NPL_ratio = if_else(total_EAD > 0, NPL_EAD / total_EAD, NA_real_))

    head(port_monthly)

  # state distribution by month, exposure-weighted
    state_expo <- sim_full %>%
    group_by(date, state) %>%
    summarise(EAD = sum(EAD, na.rm = TRUE), .groups = "drop")
    head(state_expo)




  # 4) Simple EAD policy just to have a number:
    
    #TO CHECK IN BUT NOT THE EAD I use on Full Code
    #    - Simple: draw 100% in the first active month only
    #    - Revolvent: keep constant exposure at loan amount (you can refine later)
    sim_full <- sim_full |>
    group_by(Id_client) |>
    mutate(
    first_active_col = first(start_col[!is.na(start_col)]),
    EAD = case_when(
      Cred_Type == "Simple"    ~ if_else(month_idx == first_active_col, Loan_Amount_Local, 0),
      Cred_Type == "Revolvent" ~ Loan_Amount_Local,
      TRUE ~ 0
    )
    ) |>
    ungroup()

  #Quick look at portfolio state mix through time
    
    portfolio_mix <- sim_full %>%
    group_by(date, state) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(date) %>%
    mutate(pct = n / sum(n)) %>%
    arrange(date, state)

    head(portfolio_mix)



################################################################################################
#INDIVIDUAL PORTFOLIO TO ADD TO CREDIT DATASET

    ## ---- Build labeled client-month table (for Merton/KMV merge) ----
    ## State → label + DPD band
      state_map <- tibble(
      state = 0:25,
      label = c(
      "Current","1-7","8-14","15-21","22-28","29-35","36-42","43-49",
      "50-56","57-63","64-70","71-77","78-84","85-91","92-98","99-105",
      "106-112","113-119","120-126","127-133","134-140","141-147",
      "148-154","155-161","162-168","169-175"
      )
      ) %>%
      mutate(
      dpd_min = if_else(state == 0, 0L, (state - 1L)*7L + 1L),
      dpd_max = if_else(state == 0, 0L, state*7L)
      )

      attrs <- Migration %>%
        select(
        Id_client, Activity, Sector, SIZE, Program,
    `   Internal Score`, LGD, Loan_Amount_Local, Cred_Type, start_date
        )

      client_monthly <- sim_full %>%
      # add state label/dpd
       left_join(state_map, by = "state") %>%
      
        # bring attributes (use suffix + coalesce in case of duplicate columns)
        
       left_join(attrs, by = "Id_client", suffix = c(".sim", ".attr")) %>%
        mutate(
        # If both simulations and attrs carry same column names, keep the .sim/.attr coalesced value
        Loan_Amount_Local = coalesce(Loan_Amount_Local.sim, Loan_Amount_Local.attr),
        Cred_Type         = coalesce(Cred_Type.sim,         Cred_Type.attr),
        Stage = case_when(
        is.na(state) ~ NA_character_,
        state <= 4   ~ "Stage 1",   # 0–30 dpd
        state <= 12  ~ "Stage 2",   # 31–89 dpd
        TRUE         ~ "Stage 3"    # ≥90 dpd
        ),
        DefaultFlag = if_else(state >= 13, 1L, 0L, missing = 0L)
        ) %>%
        select(
        Id_client, date,
        state_code = state, state_label = label,
        dpd_min, dpd_max, Stage, DefaultFlag,
        EAD, Loan_Amount_Local, Cred_Type,
        Activity, Sector, SIZE, Program, `Internal Score`, LGD, start_date
        ) %>%
        arrange(Id_client, date)

  ##Portfolio summary_______________________________________________________________
      
      npl_cut <- 3L  # consider states >=3 as NPL (>=15 dpd); change if needed
      port_monthly <- client_monthly %>%
      group_by(date) %>%
      summarise(
      total_EAD = sum(EAD, na.rm = TRUE),
      NPL_EAD   = sum(EAD[state_code >= npl_cut], na.rm = TRUE),
      cur_EAD   = sum(EAD[state_code == 0], na.rm = TRUE),
      .groups   = "drop"
      ) %>%
      mutate(NPL_ratio = if_else(total_EAD > 0, NPL_EAD / total_EAD, NA_real_))

  ## Export________________________________________________________________________
      write_xlsx(
      list(
      client_monthly = client_monthly,
      portfolio_monthly = port_monthly
      ),
      path = "ClientMonthly_States.xlsx"
      )


