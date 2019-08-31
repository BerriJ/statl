wine_test <- read_csv("00_data/wine_test.csv")

wine_test <- wine_test %>%
  as_tibble() %>% 
  mutate(date = as.Date(date, format = "%d %b %y")) %>%
  ##############################################################################
############################## Characters ####################################
##############################################################################
mutate_at(
  vars(-name, -vintage, -country, -region, -date, -taste_segment, -segm,
       -price_segm, -time_segm_price, -v10_dn, -v10_exp, -v10_svd, -v10_aom), 
  .funs = as.numeric) %>%
  ##############################################################################
############################## Logicals ######################################
##############################################################################
mutate_at(
  vars(v10_dn, v10_exp, v10_svd, v10_aom, rev_all_hi, rev_all_lo, rev_eve, 
       rev_eve_hi, rev_eve_lo, rev_ex, rev_ex_hi, rev_ex_lo, rev_nyaom, 
       rev_nyaom_hi, rev_nyaom_lo, rev_all_p50, rev_all_p80, rev_all_p20,
       m_rev, ), 
  .funs = as.logical) %>%
  ##############################################################################
############################## Factors ######################################
##############################################################################
mutate_at(
  vars(country, region, dist, taste_segment, segm, price_segm, time_segm_price, artikpr, name, vintage), 
  .funs = as.factor) %>%
  dplyr::select(-time_segm_price, -artikpr) %>% dplyr::select_if(.predicate = function(x) mean(is.na(x)) < 0.5) %>%
  # Drop llitre because we are using litre
  dplyr::select(-llitre) %>%
  # Only keep complete cases
  drop_na() %>%
  # Remove unused levels from factor variables
  droplevels()

# Create Model Matrices

wine_train <- wine
wine_test <- wine_test
x.train <- model.matrix(litre~., data = wine_train)
x.test <- model.matrix(litre~., data = wine_test)
y.train <- wine_train$litre
y.test <- wine_test$litre
intsct <- intersect(colnames(x.train),colnames(x.test))
x.train <- x.train[,intsct]
x.test <- x.test[, intsct]


