
context("Declarations: Complete Random Assignments")

# Complete Random Assignments ----------------------------------------------

declaration <- declare_ra(N=100)
table(declaration$ra_function())
Z <- conduct_ra(declaration)
obtain_condition_probabilities(declaration = declaration, assignment = Z)
declaration$probabilities_matrix

declaration <- declare_ra(N=101, prob = .34)
table(declaration$ra_function())
Z <- conduct_ra(declaration)
obtain_condition_probabilities(declaration = declaration, assignment = Z)
declaration$probabilities_matrix

declaration <- declare_ra(N=100, m=50)
table(declaration$ra_function())
Z <- conduct_ra(declaration)
obtain_condition_probabilities(declaration = declaration, assignment = Z)
declaration$probabilities_matrix

declaration <- declare_ra(N=100, m_each = c(30, 70), 
                          condition_names = c("control", "treatment"))
table(declaration$ra_function())
Z <- conduct_ra(declaration)
obtain_condition_probabilities(declaration = declaration, assignment = Z)
declaration$probabilities_matrix


# Multi-arm Designs
declaration <- declare_ra(N=100, m_each=c(30, 30, 40))
table(declaration$ra_function())
Z <- conduct_ra(declaration)
obtain_condition_probabilities(declaration = declaration, assignment = Z)
declaration$probabilities_matrix

declaration <- declare_ra(N=100, m_each=c(30, 30, 40), 
                          condition_names=c("control", "placebo", "treatment"))
table(declaration$ra_function())
Z <- conduct_ra(declaration)
obtain_condition_probabilities(declaration = declaration, assignment = Z)
declaration$probabilities_matrix

declaration <- declare_ra(N=100, condition_names=c("control", "placebo", "treatment"))
table(declaration$ra_function())
Z <- conduct_ra(declaration)
obtain_condition_probabilities(declaration = declaration, assignment = Z)
declaration$probabilities_matrix

declaration <- declare_ra(N=100, num_arms=3)
table(declaration$ra_function())
Z <- conduct_ra(declaration)
obtain_condition_probabilities(declaration = declaration, assignment = Z)
declaration$probabilities_matrix


context("Declarations: Simple Random Assignments")

# Simple Random Assignmetn ------------------------------------------------


declaration <- declare_ra(N=100, simple = TRUE)
table(declaration$ra_function())
Z <- conduct_ra(declaration)
obtain_condition_probabilities(declaration = declaration, assignment = Z)
declaration$probabilities_matrix

declaration <- declare_ra(N=100, prob = .4, simple = TRUE)
table(declaration$ra_function())
Z <- conduct_ra(declaration)
obtain_condition_probabilities(declaration = declaration, assignment = Z)
declaration$probabilities_matrix

declaration <- declare_ra(N=100, prob_each = c(0.3, 0.7), 
                          condition_names = c("control", "treatment"), simple=TRUE)
table(declaration$ra_function())
Z <- conduct_ra(declaration)
obtain_condition_probabilities(declaration = declaration, assignment = Z)
declaration$probabilities_matrix

declaration <- declare_ra(N=100, num_arms=3, simple=TRUE)
table(declaration$ra_function())
Z <- conduct_ra(declaration)
obtain_condition_probabilities(declaration = declaration, assignment = Z)
declaration$probabilities_matrix

declaration <- declare_ra(N=100, prob_each=c(0.3, 0.3, 0.4), simple=TRUE)
table(declaration$ra_function())
Z <- conduct_ra(declaration)
obtain_condition_probabilities(declaration = declaration, assignment = Z)
declaration$probabilities_matrix

declaration <- declare_ra(N=100, prob_each=c(0.3, 0.3, 0.4), 
                          condition_names=c("control", "placebo", "treatment"), simple=TRUE)
table(declaration$ra_function())
Z <- conduct_ra(declaration)
obtain_condition_probabilities(declaration = declaration, assignment = Z)
declaration$probabilities_matrix

declaration <- declare_ra(N=100, condition_names=c("control", "placebo", "treatment"), simple=TRUE)
table(declaration$ra_function())
Z <- conduct_ra(declaration)
obtain_condition_probabilities(declaration = declaration, assignment = Z)
declaration$probabilities_matrix


context("Declarations: Block Random Assignments")
# Blocked Designs ---------------------------------------------------------

blocks <- rep(c("A", "B","C"), times=c(50, 100, 200))

declaration <- declare_ra(blocks=blocks)
table(declaration$ra_function())
declaration$probabilities_matrix

block_m_each <- rbind(c(25, 25),
                 c(50, 50),
                 c(100, 100))

declaration <- declare_ra(blocks=blocks, block_m_each=block_m_each)
table(declaration$ra_function())
Z <- conduct_ra(declaration)
obtain_condition_probabilities(declaration = declaration, assignment = Z)
declaration$probabilities_matrix

block_m_each <- rbind(c(10, 40),
                 c(30, 70),
                 c(50, 150))

declaration <- declare_ra(blocks=blocks, block_m_each=block_m_each)
table(declaration$ra_function())
Z <- conduct_ra(declaration)
obtain_condition_probabilities(declaration = declaration, assignment = Z)
declaration$probabilities_matrix

declaration <- declare_ra(blocks=blocks, block_m_each=block_m_each, 
                          condition_names=c("control", "treatment"))
table(declaration$ra_function())
Z <- conduct_ra(declaration)
obtain_condition_probabilities(declaration = declaration, assignment = Z)
declaration$probabilities_matrix


# Multi-arm Designs
block_m_each <- rbind(c(10, 20, 20),
                 c(30, 50, 20),
                 c(50, 75, 75))
declaration <- declare_ra(blocks=blocks, block_m_each=block_m_each)
table(declaration$ra_function())
Z <- conduct_ra(declaration)
obtain_condition_probabilities(declaration = declaration, assignment = Z)
declaration$probabilities_matrix

declaration <- declare_ra(blocks=blocks, num_arms=3)
table(declaration$ra_function())
Z <- conduct_ra(declaration)
obtain_condition_probabilities(declaration = declaration, assignment = Z)
declaration$probabilities_matrix

declaration <- declare_ra(blocks=blocks, block_m_each=block_m_each, 
                          condition_names=c("control", "placebo", "treatment"))
table(declaration$ra_function())
Z <- conduct_ra(declaration)
obtain_condition_probabilities(declaration = declaration, assignment = Z)
declaration$probabilities_matrix

declaration <- declare_ra(blocks=blocks, prob_each=c(.1, .1, .8))
table(declaration$ra_function())
Z <- conduct_ra(declaration)
obtain_condition_probabilities(declaration = declaration, assignment = Z)
declaration$probabilities_matrix

context("Declarations: Cluster Random Assignments")

# Cluster Random Assignments ----------------------------------------------


# Two Group Designs
clusters <- rep(letters, times=1:26)
declaration <- declare_ra(clusters=clusters)
table(declaration$ra_function())
Z <- conduct_ra(declaration)
obtain_condition_probabilities(declaration = declaration, assignment = Z)
declaration$probabilities_matrix

declaration <- declare_ra(clusters=clusters, m=13)
table(declaration$ra_function())
Z <- conduct_ra(declaration)
obtain_condition_probabilities(declaration = declaration, assignment = Z)
declaration$probabilities_matrix

declaration <- declare_ra(clusters=clusters, m_each = c(10, 16), 
                condition_names = c("control", "treatment"))
table(declaration$ra_function())
Z <- conduct_ra(declaration)
obtain_condition_probabilities(declaration = declaration, assignment = Z)
declaration$probabilities_matrix

# Multi-arm Designs
declaration <- declare_ra(clusters=clusters, num_arms=3)
table(declaration$ra_function())
Z <- conduct_ra(declaration)
obtain_condition_probabilities(declaration = declaration, assignment = Z)
declaration$probabilities_matrix

declaration <- declare_ra(clusters=clusters, m_each=c(7, 7, 12))
table(declaration$ra_function())
Z <- conduct_ra(declaration)
obtain_condition_probabilities(declaration = declaration, assignment = Z)
declaration$probabilities_matrix

declaration <- declare_ra(clusters=clusters, m_each=c(7, 7, 12), 
                condition_names=c("control", "placebo", "treatment"))
table(declaration$ra_function())
Z <- conduct_ra(declaration)
obtain_condition_probabilities(declaration = declaration, assignment = Z)
declaration$probabilities_matrix

declaration <- declare_ra(clusters=clusters, 
                condition_names=c("control", "placebo", "treatment"))
table(declaration$ra_function())
Z <- conduct_ra(declaration)
obtain_condition_probabilities(declaration = declaration, assignment = Z)
declaration$probabilities_matrix

declaration <- declare_ra(clusters=clusters, prob_each = c(.1, .2, .7))
table(declaration$ra_function())
Z <- conduct_ra(declaration)
obtain_condition_probabilities(declaration = declaration, assignment = Z)
declaration$probabilities_matrix


context("Declarations: Blocked and Cluster Random Assignments")
# Blocked and cluster -----------------------------------------------------

clusters <- rep(letters, times=1:26)
blocks <- rep(NA, length(clusters))
blocks[clusters %in% letters[1:5]] <- "block_1"
blocks[clusters %in% letters[6:10]] <- "block_2"
blocks[clusters %in% letters[11:15]] <- "block_3"
blocks[clusters %in% letters[16:20]] <- "block_4"
blocks[clusters %in% letters[21:26]] <- "block_5"

table(blocks, clusters)

declaration <- declare_ra(clusters = clusters, blocks = blocks)
table(declaration$ra_function())
Z <- conduct_ra(declaration)
obtain_condition_probabilities(declaration = declaration, assignment = Z)
declaration$probabilities_matrix

declaration <- declare_ra(clusters = clusters, blocks = blocks, num_arms = 3)
table(declaration$ra_function())
Z <- conduct_ra(declaration)
obtain_condition_probabilities(declaration = declaration, assignment = Z)
declaration$probabilities_matrix

declaration <- declare_ra(clusters = clusters, blocks = blocks, prob_each = c(.2, .5, .3))
table(declaration$ra_function())
Z <- conduct_ra(declaration)
obtain_condition_probabilities(declaration = declaration, assignment = Z)
declaration$probabilities_matrix

block_m_each <- rbind(c(2, 3),
                 c(1, 4),
                 c(3, 2),
                 c(2, 3),
                 c(5, 1))

declaration <- declare_ra(clusters = clusters, blocks = blocks, block_m_each = block_m_each)
table(declaration$ra_function())
Z <- conduct_ra(declaration)
obtain_condition_probabilities(declaration = declaration, assignment = Z)
declaration$probabilities_matrix

