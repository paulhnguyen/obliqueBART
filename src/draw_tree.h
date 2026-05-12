#ifndef GUARD_draw_tree_h
#define GUARD_draw_tree_h

//#include "rule_funs.h"
#include "update_tree.h"
//version without tracking the data in each laef
void draw_tree(tree &t, std::map<int, std::vector<double>> &analytic_centers,
               data_info &di, tree_prior_info &tree_pi, RNG &gen);

// version that uses linear programming
void draw_tree_lp(tree &t, std::map<int, std::vector<double>> &analytic_centers,
               data_info &di, tree_prior_info &tree_pi, RNG &gen);

// version that tracks which observations go to which leaf
void draw_tree(tree &t, suff_stat &ss,
               std::map<int, std::vector<double>> &analytic_centers,
               data_info &di, tree_prior_info &tree_pi, RNG &gen);


#endif
