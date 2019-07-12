# Load the dataset

rm(list = ls())

load("00_data/Wein_train.rda")

# Remove Backslashes

remove_backslashes <- function(x){
  if(is.character(x)){
    str_replace_all(x, pattern = "\"", replacement =  "")
  }
}

wine <- apply(Wein_train, 2, FUN = remove_backslashes) %>%
  as_tibble() %>% 
  mutate(date = as.Date(date)) %>%
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
    vars(country, region, dist, taste_segment, segm, price_segm, time_segm_price, artikpr), 
    .funs = as.factor)

save(file = "00_data/wine_preprocessed.rda", wine)

# We have to give special attention to: -v10_dn, -v10_exp, -v10_svd, -v10_aom
# Those are logical for whatever reason...

# Also: look at price_segm ... this has 6 level except of 4

# time_segm_price is a combination of other variables. We should take care of that... and maybe omit it?
# artikpr is also a combined variable. omit it?

# what does "old" mean?

wine$v10_di %>% unique() # What the hell? We definitely have to do something about that!

wine$v10_am %>% unique() # What the hell? We definitely have to do something about that!

wine$v10_expm %>% unique() # What the hell? We definitely have to do something about that!
