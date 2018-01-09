

context("Sampling Declarations")

# Complete Random Assignments ----------------------------------------------

declaration <- declare_rs(N = 100)
table(declaration$rs_function())
S <- draw_rs(declaration)
obtain_inclusion_probabilities(declaration = declaration)
declaration$probabilities_vector

declaration <- declare_rs(N = 101, prob = .34)
table(declaration$rs_function())
S <- draw_rs(declaration)
obtain_inclusion_probabilities(declaration = declaration)
declaration$probabilities_vector

declaration <- declare_rs(N = 100, n = 50)
table(declaration$rs_function())
S <- draw_rs(declaration)
obtain_inclusion_probabilities(declaration = declaration)
declaration$probabilities_vector

declaration <- declare_rs(N = 100, simple = TRUE)
table(declaration$rs_function())
S <- draw_rs(declaration)
obtain_inclusion_probabilities(declaration = declaration)
declaration$probabilities_vector

declaration <- declare_rs(N = 100, prob = .4, simple = TRUE)
table(declaration$rs_function())
S <- draw_rs(declaration)
obtain_inclusion_probabilities(declaration = declaration)
declaration$probabilities_vector

strata <- rep(c("A", "B", "C"), times = c(50, 100, 200))

declaration <- declare_rs(strata = strata)
table(declaration$rs_function())
declaration$probabilities_vector

# Two Group Designs
clusters <- rep(letters, times = 1:26)
declaration <- declare_rs(clusters = clusters)
table(declaration$rs_function())
S <- draw_rs(declaration)
obtain_inclusion_probabilities(declaration = declaration)
declaration$probabilities_vector

declaration <- declare_rs(clusters = clusters, n = 10)
table(declaration$rs_function())
S <- draw_rs(declaration)
obtain_inclusion_probabilities(declaration = declaration)
declaration$probabilities_vector


clusters <- rep(letters, times = 1:26)
strata <- rep(NA, length(clusters))
strata[clusters %in% letters[1:5]] <- "stratum_1"
strata[clusters %in% letters[6:10]] <- "stratum_2"
strata[clusters %in% letters[11:15]] <- "stratum_3"
strata[clusters %in% letters[16:20]] <- "stratum_4"
strata[clusters %in% letters[21:26]] <- "stratum_5"

table(strata, clusters)

declaration <-
  declare_rs(clusters = clusters, strata = strata)
table(declaration$rs_function())
S <- draw_rs(declaration)
obtain_inclusion_probabilities(declaration = declaration)
declaration$probabilities_vector
