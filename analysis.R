library(tidyverse)

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
  ids <- data %>% count(p_id) %>% filter(n > 1) %>% pull(p_id)
  ret <- data %>% filter(!(p_id %in% ids))
  fixed_rows <-
    map_dfr(ids, function(i){
    tmp <- data %>% filter(p_id == i)
    completed <- which(tmp$complete == TRUE)
    if(length(completed) == 0){
      tmp  <- tmp[nrow(tmp),]
    }
    else{
      tmp <- tmp[max(completed), ]
    }
    tmp
  })
  ret %>% bind_rows(fixed_rows)
}

parse_smp <- function(smp){
  # assign("smp", smp, globalenv())
  if(is.null(smp) || length(smp) == 0){
    return(NULL)
  }
  #styles <- smp$order_liking
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
    mutate(correct1 = as.numeric(bat_and_ball == "10"),
           correct2 = as.numeric(widgets == "5"),
           correct3 = as.numeric(lily_pads == "24"),
           num_correct = correct1 + correct2 + correct3)
  res %>%
    select(-starts_with("correct")) %>%
    set_names(sprintf("CRT.%s", names(.)))
}

parse_mds_data <- function(results){
  results <- results %>% as.list()
  nr <- names(results)
  mds_names <- nr[str_detect(nr, "MDS")]
  mds <- map_dfr(mds_names, ~{parse_mds(results[[.x]])})
  smp <- parse_smp(results$SMP)
  crt <- parse_crt(results$CRT)
  person_data <-
    map_dfc(1:length(results), function(i){
      if(nr[i] %in% c("DEG", "MET", "BFI", "GMS")){
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
    smp %>% left_join(mds, by = "style") %>% bind_cols(person_data)
  }
  else{
    smp %>%  bind_cols(person_data)
  }
}

read_data <- function(result_dir = "data/from_server"){
  messagef("Setting up data from %s", result_dir)

  results <- purrr::map(list.files(result_dir, pattern = "*.rds", full.names = T), ~{readRDS(.x) %>% as.list()})
  #browser()
  purrr::map_dfr(results, function(x){
    parse_mds_data(x)

    }) %>%
    join_rows() %>%
    dplyr::arrange(time_started)
}


setup_workspace <- function(results = "data/from_server"){
  master <- read_data(results)
  master <- master %>% mutate(age = round(DEG.age/12),
                              gender = factor(GIN.gender))
  assign("master", master, globalenv())
}

get_correlations <- function(data, var_x, var_y, method = "pearson"){
  f <- as.formula(sprintf("~ %s + %s", var_x, var_y))
  ct <- cor.test(f, data = data, method = method)
  return(ct %>% broom::tidy())
}

