#ifndef GUARD_rule_funs_h
#define GUARD_rule_funs_h

#include "tree.h"
#include "graph_funs.h"
#include "polytope_funs.h"
#include "test_opt_lib.h"
#include "rng.h"
// works if you use the ../../testing/test_opt_lib.h, but not the one in the src..
void draw_cat_rule(rule_t &rule, tree &t, int &nid, data_info &di, tree_prior_info &tree_pi, RNG &gen);
void draw_cont_rule(rule_t &rule, tree &t, int &nid, std::vector<double> &x0, data_info &di, tree_prior_info &tree_pi, RNG &gen);
void draw_cont_rule_lp(rule_t &rule, tree &t, int &nid, data_info &di, tree_prior_info &tree_pi, RNG &gen); //linear programming
void draw_rule(rule_t &rule, tree &t, int &nid, std::vector<double> &x0, data_info &di, tree_prior_info &tree_pi, RNG &gen);
void draw_rule_lp(rule_t &rule, tree &t, int &nid, data_info &di, tree_prior_info &tree_pi, RNG &gen); //linear programming version


#endif
