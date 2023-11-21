filter_non_numeric <- function(data, vars){
  if(is.null(vars) || length(vars) == 0){
    return(character(0))
  }
  ret <- map_chr(vars, function(v){
    if(is.numeric(data[[v]])){
      v
    }
    else{
      ""
    }
  })
  ret[nchar(ret) > 0]
}


to_perc <- function(x){
  round(100 * x, 1)
}

freq_table <- function(x, prop_var) {
  prop_var  <- enquo(prop_var)
  tmp <- x %>%
    group_by( !!prop_var) %>%
    summarise(n = n()) %>%
    mutate(freq = n /sum(n)) %>%
    ungroup
  tmp
  #tmp %>% ggplot(aes_string(x = rlang::quo_text(prop_var), y= "freq")) + geom_col()
}

se <- function(x){
  sd(x)/sqrt(length(x))
}

freq2_table <- function(x, group_var, prop_var) {
  group_var <- enquo(group_var)
  prop_var  <- enquo(prop_var)
  tmp <- x %>%
    group_by(!!group_var) %>%
    mutate(n_group = n(), n_total = nrow(.)) %>%
    ungroup() %>%
    group_by(!!group_var, !!prop_var, n_group, n_total) %>%
    summarise(n = n(), .groups = "drop") %>%
    mutate(freq = n /n_group, freq_tot = n/n_total) %>%
    ungroup()
  tmp
  #tmp %>% ggplot(aes_string(x = rlang::quo_text(prop_var), y= "freq")) + geom_col()
}

impute_mice <- function(data){
  impute <- mice::mice(data = data, m = 1, method = "pmm", maxit = 10, seed = 500)
  complete(impute, 1) %>% as_tibble()

}

impute_median <- function(data, vars, method = "median"){
  for(v in vars){
    data[[v]][is.na(data[[v]])] <- median(data[[v]][!is.na(data[[v]])])
  }
  data
}

impute_all_vars <- function(data){
  impute_median(data, names(data)[str_detect(names(data), "[.]")])
}

get_sig_stars <- Vectorize(function(p_val){
  if(is.na(p_val)) return("")
  if(p_val <.001) return("***")
  if(p_val <.01) return("**")
  if(p_val <.05) return("*")
  return("")

})

lump_countries <- function(countries){
  fct_lump_lowfreq(countries, other_level = "OTHER")
}

comp_cor_mat_entries <- function(data){
  cor_mat <-  data %>% corrr::correlate()
  vars <- unique(cor_mat$term)
  map_dfr(vars, function(v1){
    map_dfr(vars, function(v2){
      if(v1 >= v2){
        return(NULL)
      }
      #browser()
      var_cor <- cor_mat %>% filter(term == v1) %>% pull(all_of(v2))
      row1 <- cor_mat %>% filter(term == v1) %>% select(-term, -all_of(c(v1, v2))) %>% t() %>% as.vector()
      row2 <- cor_mat %>% filter(term == v2) %>% select(-term, -all_of(c(v1, v2))) %>% t() %>% as.vector()
      cos_sim <- sum(row1 * row2)/(sqrt(sum(row1^2)))/sqrt(sum(row2^2))
      euclid_d <- sqrt(sum((row1 - row2)^2))
      max_d <- max(abs((row1 - row2)))
      tibble(var1 = v1, var2 = v2, var_cor = var_cor, cos_sim = cos_sim, d = euclid_d, max_d = max_d)
    })
  })
}
# analyse_lpa_classes <- function(data){
#   lpa_style_strong <- add_lpa_class(master, "style", "strong") %>% rename(lpa_class = lpa_class_style_strong)
#   lpa_style_slight <- add_lpa_class(master, "style", "slight") %>% rename(lpa_class = lpa_class_style_sligth)
#   lpa_artist_strong <- add_lpa_class(master, "artist", "strong") %>% rename(lpa_class = lpa_class_artist_strong)
#   lpa_artist_slight <- add_lpa_class(master, "artist", "slight") %>% rename(lpa_class = lpa_class_artist_sligth)
#   lpa_style_strong_means <- lpa_style_strong %>%
#     pivot_longer(cols = starts_with("PC")) %>%
#     group_by(lpa_class, name) %>% summarise(m = mean(value))
#   lpa_style_slight_means <- lpa_style_slight %>%
#     pivot_longer(cols = starts_with("PC")) %>%
#     group_by(lpa_class, name) %>% summarise(m = mean(value))
#   lpa_artist_strong_means <- lpa_artist_strong %>%
#     pivot_longer(cols = starts_with("PC")) %>%
#     group_by(lpa_class, name) %>% summarise(m = mean(value))
#   lpa_artist_slight_means <- lpa_artist_slight %>%
#     pivot_longer(cols = starts_with("PC")) %>%
#     group_by(lpa_class, name) %>% summarise(m = mean(value))
# }
fashion_subscale_names <- function(subscale_names){
  str_remove_all(subscale_names, "DS_") %>%
    str_remove_all("PC_") %>%
    str_replace_all("_", " ") %>%
    str_to_title()
}

fashion_full_type <- function(full_type){
    str_replace_all(full_type, "_", " ") %>%
    str_to_title() %>%
    str_replace_all(" ", "/")
}

scale_definition_from_keys <- function(key_file = "data/keys_df.xlsx", sheet = "keys_v3"){
  key_tmp <- readxl::read_xlsx(key_file, sheet = sheet)
  factor_names <- key_tmp %>% select(-1) %>% names()
  map_dfr(factor_names, function(fn){
    #browser()
    items <- key_tmp %>%  filter(!!sym(fn) > 0)  %>% pull(rowname) %>% sort()
    tibble(subscale = fn, n_items = length(items), items = items %>% paste(collapse = ", "))
  }) %>% mutate(subscale = fashion_subscale_names(subscale)) %>%
    set_names(c("Subscale", "No. Items", "Items"))
}

get_items_for_subscale <- function(key_file = "data/keys_df.xlsx", sheet = "keys_v3", subscales){
  key_tmp <- readxl::read_xlsx(key_file, sheet = sheet) %>% select(all_of(subscales), rowname)
  if(length(subscales) == 0){
    stop()
  }
  ret <- map(unique(subscales), function(sc){
    key_tmp <- key_tmp %>% filter(!!sym(sc) != 0) %>% pull(rowname)
  })
  if(length(subscales) == 1){
    ret <- unlist(ret)
  }
  ret
}

get_optimal_factors <- function(data, plot = F){

  ev <- eigen(cor(data, use = "pairwise.complete.obs")) # get eigenvalues
  ap <- parallel(subject = nrow(data), var = ncol(data),
                 rep = 100, cent = .05)
  nS <- tryCatch(nScree(x = ev$values, aparallel = ap$eigen$qevpea), error = function(x) {
    list(Components = data.frame(noc = 1, naf = 1, nparallel = 1, nkaiser = 1))
  })
  if(plot){
    plotnScree(nS)
  }
  #print(nS$Components)
  return(nS$Components)
}

get_efa <- function(data,
                    quest_type = "" ,
                    n_factors = 3,
                    rotate = "oblimin",
                    method = c("principal", "fa"),
                    with_optimal = T,
                    with_panels = F,
                    only_factors = F){
  messagef("extracting efa for %s",quest_type)
  tmp <-
    data %>%
    select(where(is.numeric))
  if(any(str_detect("PC_", names(tmp)))){
    messagef("Warning: data already contains factors")
  }
  #browser()
  if(quest_type != ""){
    tmp <- tmp %>% select(starts_with(quest_type))
    stopifnot(nrow(tmp) > 0)
  }
  #browser()
  if(with_optimal){
    of <- get_optimal_factors(tmp, plot = F)
    n_factors <- of$nkaiser
  }
  if(with_panels){
    psych::pairs.panels(tmp)
  }
  if(only_factors){
    return(n_factors)
  }
  #browser()
  kmo <- psych::KMO(tmp)
  bart <- psych::cortest.bartlett(tmp)
  #print(kmo)
  #print(bart)
  if(method[1] == "princpal"){
    psych::principal(tmp, n_factors, rotate = rotate)
  }
  else{
    psych::fa(tmp, n_factors, rotate = rotate)

  }
}
tidy_wilcox_test <- function(x, y){
  d <<- NA
  wt <<- tibble(statistic = NA, p.value = NA, method = NA, alternative = NA)
  tryCatch({
    d <- mean(x, na.rm = T) - mean(y, na.rm = T)
    wt <- suppressWarnings(wilcox.test(x, y)) %>% broom::tidy()
    },
    error = function(x){
      })
  wt %>% mutate(d = d) %>% select(d, statistic, p.value)
}

get_mean_differences <- function(data, group_var, target_var, vars, fn = mean, name_prefix = "", scale = F){
  groups <- unique(data[[group_var]])
  if(length(groups) > 2){
    stop("Only for two groups")
  }
  if(scale)data <- data %>% mutate(across(all_of(vars), ~{scale(.x) %>% as.vector()}))
  map_dfr(unique(data[[target_var]]), function(tv){
    map_dfr(vars, function(v){
      v1 <- data %>% filter(!!sym(group_var) == groups[1], !!sym(target_var) == tv) %>% pull(v)
      v2 <- data %>% filter(!!sym(group_var) == groups[2], !!sym(target_var) == tv) %>% pull(v)
      #browser()

      d_v <- fn(v1, na.rm =T) - fn(v2, na.rm = T)
      new_name <- sprintf("%s%s", name_prefix, v)
      tidy_wilcox_test(v1, v2) %>% mutate(var = v, target = tv, N1 = length(v1), N2 = length(v2)) %>%
        set_names(names(.) %>%
                    str_replace_all("N1", sprintf("N_%s", groups[[1]])) %>%
                    str_replace_all("N2", sprintf("N_%s", groups[[2]]))
      )
    })

  }) %>%
    mutate(grouping = sprintf("%s-%s", groups[1], groups[2])) %>%
    mutate(p_adj = p.adjust(p.value)) %>%
    select(target, var, grouping, everything())
}

get_kripp_alphas <- function(data = mds_wide, vars = DS_vars){
  data <- data %>%
    mutate(style = dress_up_styles(style),
           COUNTRY = lump_countries(country) %>% as.character())
  countries <- unique(data$COUNTRY)
  print(countries)
  map_dfr(vars, function(v){
    map_dfr(countries, function(c1){
      map_dfr(countries, function(c2){
        if(c1 <= c2) {
          return(NULL)
        }
        messagef("Checking %s for %s/%s", v, c1, c2)
        #if(c1 == "US" && c2 == "UK"){
        #  browser()
        #}
        ka <- data %>%
          filter(COUNTRY %in% c(c1, c2))%>%
          group_by(style, COUNTRY) %>%
          summarise(m = mean(!!sym(v)), .groups = "drop")  %>%
          pivot_wider(id_cols = COUNTRY,
                      names_from = style,
                      values_from = m) %>%
          select(-COUNTRY) %>%
          as.matrix() %>%
          irr::kripp.alpha(method = "interval")
        tibble(country1 = c1, country2 = c2, var = v, kripp_alpha = ka$value)
      })
    })
  })
}


fisher_trafo <- function(x){
  log((1 + x)/(1 - x)) * .5
}

fisher_inv_trafo <- function(x){
  (exp(2 * x) - 1)/(exp( 2 * x) + 1)
}

make_age_range <- function(age_groups){
  map(age_groups %>%
        str_replace("\\+", "-99") %>%
        str_split("-"),
      function(lo_up){
        lo_up <- suppressWarnings(as.integer(lo_up))
        if(length(lo_up) != 2 | any(is.na(lo_up))){
          return(NULL)
        }
        seq(lo_up[1], lo_up[2])
      }) %>%
    unlist() %>%
    sort()
}

make_shiny_table <- function(data){
  data <- data %>% slice(1) %>% pivot_longer(-p_id)
  var_names <- sprintf("%s:",
                       (data$name %>%
                         str_split_fixed("[.]", 2))[,2] %>%
                         str_replace_all("_", " ") %>%
                         str_to_title())
  map2(var_names, data$value, function(x, y){
    shiny::span(
      shiny::tags$b(x),
      y, shiny::br())
  }) %>% shiny::tagList()
}

get_var_group <- function(var_names){
  (str_split_fixed(var_names, "[.]", 2))[,1]
}
