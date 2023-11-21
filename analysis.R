library(tidyverse)
library(psych)
messagef <- function(...) message(sprintf(...))
printf <- function(...) print(sprintf(...))

num_predictors <- c("GMS.general", "ART.points", "RAT.ability",  "age")
cat_predictors <- c("gender")


dummy <- list()
dummy[["RAT"]] <- tibble(RAT.ability  = NA, RAT.ability_sem  = NA, RAT.num_items = NA)
dummy[["SRS"]] <- tibble(SRS.perc_correct  = NA, SRS.num_correct = NA, SRS.num_items = NA)
dummy[["ART"]] <- tibble(ART.perc_correct  = NA, ART.num_correct = NA, ART.num_items = NA, ART.points = NA)
dummy[["DEG"]] <- tibble(DEG.age = NA,   GIN.gender = NA)
dummy[["GMS"]] <- tibble(GMS.general = NA)


mds_labels <- c(
  "social.not_authentic",
  "music.too_disharmonic",
  "emo.expressionless",
  "music.too_uniform",
  "emo.no_impact",
  "music.too_little_melodious",
  "body.missing_danceability",
  "social.no_identification",
  "lyrics.too_realistic",
  "music.too_unrhythmic",
  "music.too_fast",
  "body.displeasure",
  "emo.no_feelings",
  "emo.bad_mood",
  "music.too_schematic",
  "social.bad_experiences",
  "music.too_melodious",
  "emo.fake",
  "music.too_chaotic",
  "music.too_variable",
  "music.disliked_instruments",
  "lyrics.too_simple",
  "social.not_peer_approved",
  "lyrics.too_complex",
  "music.too_soft",
  "music.too_rhythmic",
  "music.too_little_tension",
  "social.reject_fanbase",
  "music.too_slow",
  "music.too_loud",
  "music.bad_vocals",
  "music.too_niche",
  "social.too_often_heard",
  "music.too_mainstream",
  "lyrics.too_unrealistic",
  "music.too_complex",
  "social.incongruent_ideology",
  "music.too_much_change",
  "emo.bad_feelings",
  "emo.too_emotional",
  "music.too_little_change",
  "music.too_simple"
)

parse_generic_entry <- function(q_entry, label){
  #browser()
  dummy_entry <- dummy[[label]]
  stopifnot(!is.null(dummy_entry))
  if(is.null(q_entry)){
    return(dummy_entry)
  }
  names <- names(q_entry)
  if(length(names) == 0){
    return(dummy_entry)
  }
  sum_data <- names[!stringr::str_detect(names, "q[0-9]+")]
  ret <- q_entry[sum_data]
  names(ret) <- sprintf("%s.%s", label, names(ret) %>% stringr::str_to_lower() %>% stringr::str_replace(" ", "_"))
  ret %>% tibble::as_tibble()
}

get_parameters <- function(data, input, keep_pseudo_na = T, var_data){

  vars <- c("x" = input$bv_variable1, "y" = input$bv_variable2)
  var_info1 <- var_data %>% filter(variable == input$bv_variable1)
  var_info2 <- var_data %>% filter(variable == input$bv_variable2)
  sub_type <- sprintf("%s-%s", substr(var_info1$type, 1, 3), substr(var_info2$type, 1, 3))
  list(vars = vars, sub_type = sub_type)
}


split_multi_entry <- function(entry){
  if(length(entry) == 1){
    ret <- str_replace_all(entry, "',", "@") %>%
      str_replace_all("'", "") %>%
      str_split("@") %>%
      unlist()
  }
  else{
    ret <- NULL
  }
  ret
}

join_rows <- function(data){
  if(is.null(data[["p_id"]])){
    return(data)
  }
  #browser()
  ids <- data %>% count(p_id) %>% filter(n > 1) %>% pull(p_id)
  ret <- data %>% filter(!(p_id %in% ids))
  mds_digest <- data %>%
    filter(!is.na(MDS.rating)) %>%
    group_by(p_id, style) %>%
    summarise(MDS.mean_ratings = mean(MDS.rating, na.rm = T),
              .groups = "drop")
  # fixed_rows <-
  #   map_dfr(ids, function(i){
  #   tmp <- data %>% filter(p_id == i)
  #   completed <- which(tmp$complete == TRUE)
  #   if(length(completed) == 0){
  #     tmp  <- tmp[nrow(tmp),]
  #   }
  #   else{
  #     tmp <- tmp[max(completed), ]
  #   }
  #   tmp
  # })
  # ret %>%
  #   bind_rows(fixed_rows) %>%
  data  %>%
    left_join(mds_digest, by = c("p_id", "style")) %>%
    select(-MDS.item, -MDS.rating) %>%
    distinct()
}

parse_smp <- function(smp){
  # assign("smp", smp, globalenv())
  if(is.null(smp) || length(smp) == 0){
    return(NULL)
  }
  #styles <- smp$order_liking
  #browser()
  liking <- smp[str_detect(names(smp), "style_liking")]
  likings_idz <- str_extract(names(smp)[str_detect(names(smp), "style_liking")], "[0-9]+")  %>% as.numeric()
  likings <- tibble(style = all_styles$style[likings_idz],
                    SMP.liking = as.numeric(liking))

  familiarity <- smp[str_detect(names(smp), "style_familiarity")]
  fam_idz <- str_extract(names(smp)[str_detect(names(smp), "style_familiarity")], "[0-9]+")  %>% as.numeric()
  fam <- tibble(style = all_styles$style[fam_idz],
                SMP.familiarity = as.numeric(familiarity))
  fam %>% left_join(likings, by = "style")
  # tibble(SMP.style_liking = as.numeric(liking),
  #        SMP.style_familiarity = as.numeric(familiarity),
  #        style = styles)
}

parse_mds <- function(mds){
  target <- mds$target
  items <- names(mds)[str_detect(names(mds), "Item")]
  ids <- str_extract(items, "[0-9]+")
  ratings <- mds[items] %>% unlist() %>% as.numeric()
  tibble(MDS.item = ids,
         MDS.rating = ratings,
         style = target)
}

parse_crt <-function(crt_data){
  if(is.null(crt_data)){
    return(crt_data)
  }
  if("Which Problems" %in% names(crt_data)){
    crt_data[["Which Problems"]] <- paste(crt_data[["Which Problems"]], collapse = ";")
  }
  res <- crt_data %>%
    as_tibble() %>%
    select(-starts_with("q")) %>%
    set_names(names(.) %>% str_replace_all(" ", "_") %>% tolower()) %>%
    distinct()
  #browser()
  res <- res %>%
    mutate(correct1 = as.numeric(bat_and_ball == "5"),
           correct2 = as.numeric(widgets == "5"),
           correct3 = as.numeric(lily_pads == "47"),
           num_correct = correct1 + correct2 + correct3)
  res %>%
    select(-starts_with("correct")) %>%
    set_names(sprintf("CRT.%s", names(.)))
}

parse_mds_data <- function(results){
  results <- results %>% as.list()
  nr <- names(results)
  mds_names <- nr[str_detect(nr, "MDS")]
  #browser()
  mds <- map_dfr(mds_names, ~{parse_mds(results[[.x]]) %>% mutate(MDS.no = str_extract(.x, "[0-9]+"))})
  smp <- parse_smp(results$SMP)
  crt <- parse_crt(results$CRT)
  person_data <-
    map_dfc(1:length(results), function(i){
      if(nr[i] %in% c("DEG", "MET", "TPI", "GMS")){
        #browser()
        results[[i]] %>%
          as_tibble() %>%
          select(-starts_with("q")) %>%
          distinct() %>%
          set_names(sprintf("%s.%s", nr[i], names(.) %>% str_replace_all(" ", "_") %>% tolower()))
      }
      else if(nr[i] == "GIN"){
        results[[i]]$gender_inclusive %>%
          as_tibble() %>%
          distinct() %>%
          set_names("GIN.gender", "GIN.self_description")
      }
    })
  if(!is.null(results$session)){
    person_data <- bind_cols(person_data,
                             crt,
                             results$session %>%
                               as_tibble() %>%
                               select(p_id, time_started, complete))
  }
  if(!is.null(results$reflections)){
    person_data <- bind_cols(person_data,
                             results$reflections$reflection %>%
                               as_tibble() %>%
                               set_names(sprintf("REF.%s", names(.))))
  }
  #messagef("Names(smp): %s", paste(names(smp)))
  #messagef("Names(mds): %s", paste(names(mds)))
  if(is.null(person_data) || nrow(person_data) == 0){
    person_data <- tibble(p_id = NA)
  }
  if(nrow(mds) > 0){
    ret <- smp %>% left_join(mds, by = "style") %>% bind_cols(person_data)
  }
  else{
    ret <- smp %>%  bind_cols(person_data)
  }
  # if(length(mds_names) > 1){
  #   browser()
  # }
  ret
}

remove_doublets <- function(results){
  #browser()
  diagnostic <-
    map_dfr(1:length(results), function(i){
      if(!"session" %in% names(results[[i]])){
        return(NULL)
      }
      tmp <- results[[i]]$session %>%
        as_tibble %>% select(p_id, current_time, complete) %>% mutate(idx = i)
    })
  incompletes <- which(!diagnostic$complete)
  p_ids <- unique(diagnostic$p_id)
  not_last <- diagnostic %>%
    group_by(p_id) %>%
    mutate(r = rank(current_time), last = (r == max(r))) %>%
    ungroup() %>%
    filter(!last)
  p_ids_after <- unique(not_last$p_id)
  if(length(setdiff(p_ids_after, p_ids)) != 0){
    browser()
  }
  remove_idx <- union(incompletes, not_last %>% pull(idx))
  messagef("Removing %d doublets", length(remove_idx))
  results[setdiff(1:length(results), remove_idx)]
}

add_scores_from_key <- function(data,
                                key_file = "data/keys_df.xlsx",
                                sheet = "keys_v3",
                                impute_method = c("no", "mice", "median")){
  orig_data <- data
  data <- data %>% select(all_of(mds_labels))
  #browser()
  # if(impute_method[1] == "mice"){
  #   data <- impute_mice(data )
  # }
  # if(impute_method[1] == "median"){
  #   data <- impute_median(data, vars = num_vars)
  # }
  key_tmp <- readxl::read_xlsx(key_file, sheet = sheet)
  keys_cleaned <- key_tmp %>% select(-1) %>% as.matrix()
  var_names <- key_tmp %>% pull(1)
  factor_names <- key_tmp %>% select(-1) %>% names()
  row.names(keys_cleaned) <- var_names
  item_scores <- psych::scoreItems(keys_cleaned, data)
  bind_cols(orig_data %>%
              select(-all_of(intersect(orig_data %>% names(),
                                       key_tmp %>% names()))),
            item_scores$scores %>%
              as.data.frame() %>%
              as_tibble() %>%
              set_names(factor_names))
}

extract_wide_mds <- function(df){
  df <- df %>% filter(!is.na(MDS.no))
  ids <- df$p_id %>% unique()
  map_dfr(ids, function(pid){
    messagef("Processing %s", pid)
    df %>% filter(p_id == pid)   %>%
       pivot_wider(id_cols = c("style",
                               "p_id",
                               "SMP.familiarity",
                               "SMP.liking",
                               "MDS.no",
                               "DEG.country_of_residence"),
                   names_from = c(MDS.item),
                   values_from = MDS.rating, names_prefix = "MDS.")

  })
}

read_data <- function(result_dir = "data/from_server"){
  messagef("Setting up data from %s", result_dir)

  results <- purrr::map(list.files(result_dir, pattern = "*.rds", full.names = T), ~{readRDS(.x) %>% as.list()})
  results <- remove_doublets(results)
  #browser()
  ret <-
    purrr::map_dfr(results, function(x){
      parse_mds_data(x)

    }) %>%
    #join_rows() %>%
    dplyr::arrange(time_started)

  ret
}

DEG.financial_labels <- c("Certainly not", "Probably not", "Probably yes", "Certainly yes")

DEG.life_circumstances_labels <- c("Among the worst",
                                   "Much worse than average",
                                   "Worse than average",
                                   "Average",
                                   "Better than average",
                                   "Much better than average",
                                   "Among the best")
dress_up_styles <- function(styles){
  str_split(styles, "\\(") %>%
    lapply(function(x) trimws(x[[1]])) %>%
    str_replace_all("[/ -]+", "_")  %>%
    tolower()
}

setup_workspace <- function(results = "data_raw",
                            all_styles_path = "data/SMP_AUS_styles.xlsx",
                            key_file = "data/keys_df.xlsx",
                            reload = F){
  #browser()
  all_styles <<- readxl::read_xlsx(all_styles_path)
  assign("all_styles", all_styles, globalenv())

  if(reload || !file.exists("data/master.rds")){
    # master <- read_data(results)
    # master <- master %>% mutate(age = round(DEG.age/12),
    #                             gender = factor(GIN.gender),
    #                             DEG.financial = factor(DEG.financial_labels[as.integer(DEG.financial)], levels = DEG.financial_labels),
    #                             DEG.life_circumstances = factor(DEG.life_circumstances_labels[as.integer(DEG.life_circumstances)], levels = DEG.life_circumstances_labels)
    # )
    master <- readRDS("data/master.rds")

    mds_wide <- extract_wide_mds(master)
    #browser()
    names(mds_wide)[str_detect(names(mds_wide), "^MDS.[0-9]+")] <- mds_labels
    mds_wide <- mds_wide %>% select(p_id, style,
                                    familiarity = SMP.familiarity,
                                    liking = SMP.liking,
                                    country = DEG.country_of_residence,
                                    everything()) %>%
      add_scores_from_key(key_file)


    smp <- master %>%
      select(p_id,
             style,
             familiarity = SMP.familiarity,
             liking = SMP.liking,
             country = DEG.country_of_residence,
             age,
             gender
      ) %>%
      distinct()
    #master <- master %>% filter(is.na(MDS.no)) %>% select(!starts_with("MDS"))
    metadata <- master %>%
      #filter(complete) %>%
      select(-c(style, SMP.familiarity, SMP.liking, complete, time_started, MDS.item, MDS.rating, MDS.no)) %>%
      mutate(SES.economic_status = (as.integer(factor(DEG.financial)) - 2.5)/1.5 +
                (as.integer(as.factor(DEG.life_circumstances)) - 4)/3) %>%
      group_by(REF.most_liked) %>%
      mutate(n_liked = n_distinct(p_id)) %>%
      ungroup() %>%
      group_by(REF.most_disliked) %>%
      mutate(n_disliked = n_distinct(p_id)) %>%
      ungroup() %>%
      distinct()
    saveRDS(smp, "data/smp.rds")
    saveRDS(mds_wide, "data/mds_wide.rds")
    saveRDS(metadata, "data/metadata.rds")
    saveRDS(master, "data/master.rds")
  }
  else{
    master <- readRDS("data/master.rds")
    metadata <- readRDS("data/metadata.rds")
    smp <- readRDS("data/smp.rds")
    mds_wide <- readRDS("data/mds_wide.rds")

  }
  DS_vars <- sort(names(mds_wide)[str_detect(names(mds_wide), "DS_")])
  assign("DS_vars", DS_vars, globalenv())
  assign("smp", smp, globalenv())
  assign("mds_wide", mds_wide, globalenv())
  assign("metadata", metadata, globalenv())
  assign("master", master, globalenv())
}

get_correlations <- function(data, var_x, var_y, method = "pearson"){
  f <- as.formula(sprintf("~ %s + %s", var_x, var_y))
  ct <- cor.test(f, data = data, method = method)
  return(ct %>% broom::tidy())
}

