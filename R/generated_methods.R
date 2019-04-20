ra_function.ra_blocked <-
  function (this)
    block_ra(
      blocks = this[["blocks"]],
      prob = this[["prob"]],
      prob_unit = this[["prob_unit"]],
      prob_each = this[["prob_each"]],
      m = this[["m"]],
      m_unit = this[["m_unit"]],
      block_m = this[["block_m"]],
      block_m_each = this[["block_m_each"]],
      block_prob = this[["block_prob"]],
      block_prob_each = this[["block_prob_each"]],
      num_arms = this[["num_arms"]],
      conditions = this[["conditions"]],
      check_inputs = this[["check_inputs"]]
    )
ra_function.ra_blocked_and_clustered <-
  function (this)
    block_and_cluster_ra(
      blocks = this[["blocks"]],
      clusters = this[["clusters"]],
      prob = this[["prob"]],
      prob_unit = this[["prob_unit"]],
      prob_each = this[["prob_each"]],
      m = this[["m"]],
      m_unit = this[["m_unit"]],
      block_m = this[["block_m"]],
      block_m_each = this[["block_m_each"]],
      block_prob = this[["block_prob"]],
      block_prob_each = this[["block_prob_each"]],
      num_arms = this[["num_arms"]],
      conditions = this[["conditions"]],
      check_inputs = this[["check_inputs"]]
    )
ra_function.ra_clustered <-
  function (this)
    cluster_ra(
      clusters = this[["clusters"]],
      m = this[["m"]],
      m_unit = this[["m_unit"]],
      m_each = this[["m_each"]],
      prob = this[["prob"]],
      prob_unit = this[["prob_unit"]],
      prob_each = this[["prob_each"]],
      num_arms = this[["num_arms"]],
      conditions = this[["conditions"]],
      simple = this[["simple"]],
      check_inputs = this[["check_inputs"]]
    )
ra_function.ra_complete <-
  function (this)
    complete_ra(
      N = this[["N"]],
      m = this[["m"]],
      m_unit = this[["m_unit"]],
      m_each = this[["m_each"]],
      prob = this[["prob"]],
      prob_unit = this[["prob_unit"]],
      prob_each = this[["prob_each"]],
      num_arms = this[["num_arms"]],
      conditions = this[["conditions"]],
      check_inputs = this[["check_inputs"]]
    )
ra_function.ra_custom <-
  function (this)
    custom_ra(permutation_matrix = this[["permutation_matrix"]])
ra_function.ra_simple <-
  function (this)
    simple_ra(
      N = this[["N"]],
      prob = this[["prob"]],
      prob_unit = this[["prob_unit"]],
      prob_each = this[["prob_each"]],
      num_arms = this[["num_arms"]],
      conditions = this[["conditions"]],
      check_inputs = this[["check_inputs"]],
      simple = this[["simple"]]
    )
ra_probabilities.ra_blocked <-
  function (this)
    block_ra_probabilities(
      blocks = this[["blocks"]],
      prob = this[["prob"]],
      prob_unit = this[["prob_unit"]],
      prob_each = this[["prob_each"]],
      m = this[["m"]],
      m_unit = this[["m_unit"]],
      block_m = this[["block_m"]],
      block_m_each = this[["block_m_each"]],
      block_prob = this[["block_prob"]],
      block_prob_each = this[["block_prob_each"]],
      num_arms = this[["num_arms"]],
      conditions = this[["conditions"]],
      check_inputs = this[["check_inputs"]]
    )
ra_probabilities.ra_blocked_and_clustered <-
  function (this)
    block_and_cluster_ra_probabilities(
      blocks = this[["blocks"]],
      clusters = this[["clusters"]],
      prob = this[["prob"]],
      prob_unit = this[["prob_unit"]],
      prob_each = this[["prob_each"]],
      m = this[["m"]],
      m_unit = this[["m_unit"]],
      block_m = this[["block_m"]],
      block_m_each = this[["block_m_each"]],
      block_prob = this[["block_prob"]],
      block_prob_each = this[["block_prob_each"]],
      num_arms = this[["num_arms"]],
      conditions = this[["conditions"]],
      check_inputs = this[["check_inputs"]]
    )
ra_probabilities.ra_clustered <-
  function (this)
    cluster_ra_probabilities(
      clusters = this[["clusters"]],
      m = this[["m"]],
      m_unit = this[["m_unit"]],
      m_each = this[["m_each"]],
      prob = this[["prob"]],
      prob_unit = this[["prob_unit"]],
      prob_each = this[["prob_each"]],
      num_arms = this[["num_arms"]],
      conditions = this[["conditions"]],
      simple = this[["simple"]],
      check_inputs = this[["check_inputs"]]
    )
ra_probabilities.ra_complete <-
  function (this)
    complete_ra_probabilities(
      N = this[["N"]],
      m = this[["m"]],
      m_unit = this[["m_unit"]],
      m_each = this[["m_each"]],
      prob = this[["prob"]],
      prob_unit = this[["prob_unit"]],
      prob_each = this[["prob_each"]],
      num_arms = this[["num_arms"]],
      conditions = this[["conditions"]],
      check_inputs = this[["check_inputs"]]
    )
ra_probabilities.ra_custom <-
  function (this)
    custom_ra_probabilities(permutation_matrix = this[["permutation_matrix"]])
ra_probabilities.ra_simple <-
  function (this)
    simple_ra_probabilities(
      N = this[["N"]],
      prob = this[["prob"]],
      prob_unit = this[["prob_unit"]],
      prob_each = this[["prob_each"]],
      num_arms = this[["num_arms"]],
      conditions = this[["conditions"]],
      check_inputs = this[["check_inputs"]],
      simple = this[["simple"]]
    )
rs_function.rs_clustered <-
  function (this)
    cluster_rs(
      clusters = this[["clusters"]],
      n = this[["n"]],
      n_unit = this[["n_unit"]],
      prob = this[["prob"]],
      prob_unit = this[["prob_unit"]],
      simple = this[["simple"]],
      check_inputs = this[["check_inputs"]]
    )
rs_function.rs_complete <-
  function (this)
    complete_rs(
      N = this[["N"]],
      n = this[["n"]],
      n_unit = this[["n_unit"]],
      prob = this[["prob"]],
      prob_unit = this[["prob_unit"]],
      check_inputs = this[["check_inputs"]]
    )
rs_function.rs_simple <-
  function (this)
    simple_rs(N = this[["N"]],
              prob = this[["prob"]],
              prob_unit = this[["prob_unit"]],
              check_inputs = this[["check_inputs"]],
              simple = this[["simple"]])
rs_function.rs_stratified <-
  function (this)
    strata_rs(
      strata = this[["strata"]],
      prob = this[["prob"]],
      prob_unit = this[["prob_unit"]],
      n = this[["n"]],
      n_unit = this[["n_unit"]],
      strata_n = this[["strata_n"]],
      strata_prob = this[["strata_prob"]],
      check_inputs = this[["check_inputs"]]
    )
rs_function.rs_stratified_and_clustered <-
  function (this)
    strata_and_cluster_rs(
      strata = this[["strata"]],
      clusters = this[["clusters"]],
      prob = this[["prob"]],
      prob_unit = this[["prob_unit"]],
      n = this[["n"]],
      n_unit = this[["n_unit"]],
      strata_n = this[["strata_n"]],
      strata_prob = this[["strata_prob"]],
      check_inputs = this[["check_inputs"]]
    )
rs_probabilities.rs_clustered <-
  function (this)
    cluster_rs_probabilities(
      clusters = this[["clusters"]],
      n = this[["n"]],
      n_unit = this[["n_unit"]],
      prob = this[["prob"]],
      prob_unit = this[["prob_unit"]],
      simple = this[["simple"]],
      check_inputs = this[["check_inputs"]]
    )
rs_probabilities.rs_complete <-
  function (this)
    complete_rs_probabilities(
      N = this[["N"]],
      n = this[["n"]],
      n_unit = this[["n_unit"]],
      prob = this[["prob"]],
      prob_unit = this[["prob_unit"]],
      check_inputs = this[["check_inputs"]]
    )
rs_probabilities.rs_simple <-
  function (this)
    simple_rs_probabilities(N = this[["N"]],
                            prob = this[["prob"]],
                            prob_unit = this[["prob_unit"]],
                            check_inputs = this[["check_inputs"]],
                            simple = this[["simple"]])
rs_probabilities.rs_stratified <-
  function (this)
    strata_rs_probabilities(
      strata = this[["strata"]],
      prob = this[["prob"]],
      prob_unit = this[["prob_unit"]],
      n = this[["n"]],
      n_unit = this[["n_unit"]],
      strata_n = this[["strata_n"]],
      strata_prob = this[["strata_prob"]],
      check_inputs = this[["check_inputs"]]
    )
rs_probabilities.rs_stratified_and_clustered <-
  function (this)
    strata_and_cluster_rs_probabilities(
      strata = this[["strata"]],
      clusters = this[["clusters"]],
      prob = this[["prob"]],
      prob_unit = this[["prob_unit"]],
      n = this[["n"]],
      n_unit = this[["n_unit"]],
      strata_n = this[["strata_n"]],
      strata_prob = this[["strata_prob"]],
      check_inputs = this[["check_inputs"]]
    )
