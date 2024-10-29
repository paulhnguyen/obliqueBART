

#ifndef GUARD_funs_h
#define GUARD_funs_h

#include "tree.h"


void tree_traversal(suff_stat &ss, tree &t, data_info &di);


//void fit_single_tree(double* ftemp, tree &t, data_info &di);
void fit_ensemble(std::vector<double> &fit, std::vector<tree> &t_vec, data_info &di);

void update_theta_combination(std::vector<double> &theta_combination, double &u_comb, 
    std::vector<int> &combination_count, int &p, double &a_u, double &b_u, RNG &gen);

void update_theta_u(std::vector<double> &theta, double &u, std::vector<int> &var_count, int &p, double &a_u, double &b_u, RNG &gen);

std::map<int,double> perform_draw(int alpha_dp, int p, std::map<int, std::map<int, double>> &combinations_map, std::vector<int> &urn, int &index, RNG &gen);



#endif /* funs_h */
