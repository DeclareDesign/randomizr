

context("Sampling Declarations")

# Complete Random Assignments ----------------------------------------------

declaration <- declare_rs(N = 100)
table(declaration$rs_function())
S <- draw_rs(declaration)
obtain_inclusion_probabilities(rs_declaration = declaration)
declaration$probabilities_vector

declaration <- declare_rs(N = 101, prob = .34)
table(declaration$rs_function())
S <- draw_rs(declaration)
obtain_inclusion_probabilities(rs_declaration = declaration)
declaration$probabilities_vector

declaration <- declare_rs(N = 100, n = 50)
table(declaration$rs_function())
S <- draw_rs(declaration)
obtain_inclusion_probabilities(rs_declaration = declaration)
declaration$probabilities_vector

declaration <- declare_rs(N = 100, simple = TRUE)
table(declaration$rs_function())
S <- draw_rs(declaration)
obtain_inclusion_probabilities(rs_declaration = declaration)
declaration$probabilities_vector

declaration <- declare_rs(N = 100, prob = .4, simple = TRUE)
table(declaration$rs_function())
S <- draw_rs(declaration)
obtain_inclusion_probabilities(rs_declaration = declaration)
declaration$probabilities_vector

strata_var <- rep(c("A", "B", "C"), times = c(50, 100, 200))

declaration <- declare_rs(strata_var = strata_var)
table(declaration$rs_function())
declaration$probabilities_vector

# Two Group Designs
clust_var <- rep(letters, times = 1:26)
declaration <- declare_rs(clust_var = clust_var)
table(declaration$rs_function())
S <- draw_rs(declaration)
obtain_inclusion_probabilities(rs_declaration = declaration)
declaration$probabilities_vector

declaration <- declare_rs(clust_var = clust_var, n = 10)
table(declaration$rs_function())
S <- draw_rs(declaration)
obtain_inclusion_probabilities(rs_declaration = declaration)
declaration$probabilities_vector


clust_var <- rep(letters, times = 1:26)
strata_var <- rep(NA, length(clust_var))
strata_var[clust_var %in% letters[1:5]] <- "stratum_1"
strata_var[clust_var %in% letters[6:10]] <- "stratum_2"
strata_var[clust_var %in% letters[11:15]] <- "stratum_3"
strata_var[clust_var %in% letters[16:20]] <- "stratum_4"
strata_var[clust_var %in% letters[21:26]] <- "stratum_5"

table(strata_var, clust_var)

declaration <-
  declare_rs(clust_var = clust_var, strata_var = strata_var)
table(declaration$rs_function())
S <- draw_rs(declaration)
obtain_inclusion_probabilities(rs_declaration = declaration)
declaration$probabilities_vector
