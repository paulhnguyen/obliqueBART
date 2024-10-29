#ifndef GUARD_update_tree_lp_h
#define GUARD_update_tree_lp_h

#include "update_tree.h"
#include "rule_funs.h"




// same as other grow/prune/update, but without the analytic center stuff
void grow_tree_lp(tree &t,suff_stat &ss_train, suff_stat &ss_test, int &accept, rule_diag_t &rule_diag, double &sigma, data_info &di_train, data_info &di_test, tree_prior_info &tree_pi, RNG &gen);
void prune_tree_lp(tree &t,suff_stat &ss_train, suff_stat &ss_test, int &accept, double &sigma, data_info &di_train, data_info &di_test, tree_prior_info &tree_pi, RNG &gen);
void update_tree_lp(tree &t, suff_stat &ss_train, suff_stat &ss_test, int &accept, rule_diag_t &rule_diag, double &sigma, data_info &di_train, data_info &di_test, tree_prior_info &tree_pi, RNG &gen);
#endif /* update_tree__lph */
