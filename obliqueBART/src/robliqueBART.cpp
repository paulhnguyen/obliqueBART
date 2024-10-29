#include "draw_tree.h"
#include "data_parsing_funs.h"
#include "funs.h"
Rcpp::List drawTree(Rcpp::NumericMatrix tX_cont,
                    Rcpp::IntegerMatrix tX_cat,
                    Rcpp::LogicalVector unif_cuts,
                    Rcpp::Nullable<Rcpp::List> cutpoints_list,
                    Rcpp::Nullable<Rcpp::List> cat_levels_list,
                    Rcpp::Nullable<Rcpp::List> edge_mat_list,
                    Rcpp::LogicalVector graph_split, int graph_cut_type,
                    double aa_prob,
                    int phi_option,
                    int x0_option,
                    double alpha, double beta,
                    double mu0, double tau)
{
  Rcpp::RNGScope scope;
  RNG gen;
  set_str_conversion set_str; // for converting sets of integers into string
  
  int n = 0;
  int p_cont = 0;
  int p_cat = 0;
  parse_training_data(n,p_cont, p_cat, tX_cont, tX_cat);
  int p = p_cont + p_cat;
  Rcpp::Rcout << "n = " << n << " p_cont = " << p_cont << " p_cat = " << p_cat << std::endl;
  
  std::vector<std::set<double>> cutpoints;
  if(p_cont > 0){
    if(cutpoints_list.isNotNull()){
      Rcpp::List tmp_cutpoints  = Rcpp::List(cutpoints_list);
      parse_cutpoints(cutpoints, p_cont, tmp_cutpoints, unif_cuts);
    }
  }
  
  std::vector<std::set<int>> cat_levels;
  std::vector<int> K; // number of levels for the different categorical variables
  std::vector<std::vector<edge>> edges;
  
  if(p_cat > 0){
    if(cat_levels_list.isNotNull()){
      Rcpp::List tmp_cat_levels = Rcpp::List(cat_levels_list);
      parse_cat_levels(cat_levels, K, p_cat, tmp_cat_levels);
    } else{
      Rcpp::stop("Must provide list of categorical levels!");
    }
    if(edge_mat_list.isNotNull()){
      Rcpp::List tmp_edge_mat = Rcpp::List(edge_mat_list);
      parse_graphs(edges, p_cat, K, tmp_edge_mat, graph_split);
    }
  }

  data_info di;
  di.n = n;
  di.p_cont = p_cont;
  di.p_cat = p_cat;
  di.p = p;
  if(p_cont > 0) di.x_cont = tX_cont.begin();
  if(p_cat > 0) di.x_cat = tX_cat.begin();

  std::vector<double> theta(p, 1.0/ (double) p);
  // double u = 1.0/(1.0 + (double) p); unused. consider adding back in if we turn on sparse
  std::vector<int> var_count(p, 0); // count how many times a variable has been used in a splitting rule

  double prob_cat = (double) p_cat/p;
  double prob_aa = aa_prob;
  
  tree_prior_info tree_pi;
  tree_pi.theta = &theta;
  tree_pi.prob_cat = &prob_cat;
  tree_pi.prob_aa = &prob_aa;

  if(p_cont > 0){
    tree_pi.unif_cuts = unif_cuts.begin(); // do we use uniform cutpoints?
    tree_pi.cutpoints = &cutpoints;
    tree_pi.phi_option = phi_option;
    tree_pi.x0_option = x0_option;
  }
  
  if(p_cat > 0){
    tree_pi.cat_levels = &cat_levels;
    tree_pi.edges = &edges;
    tree_pi.K = &K;
    tree_pi.graph_split = graph_split.begin();
    tree_pi.graph_cut_type = graph_cut_type;
  }
  tree_pi.mu0 = mu0;
  tree_pi.tau = tau;
  
  
  tree t;
  std::map<int, std::vector<double>> analytic_centers;
  analytic_centers.insert(std::pair<int,std::vector<double>>(1, std::vector<double>(di.p_cont, 0.0)));

  suff_stat ss;
  tree_traversal(ss, t, di);
  draw_tree(t, ss, analytic_centers, di, tree_pi, gen);
  
  double tmp_mu;
  Rcpp::NumericVector fit(n);
  draw_tree(t, analytic_centers, di, tree_pi, gen);
  t.print();
  

  tree_traversal(ss, t, di);

  for(suff_stat_it ss_it = ss.begin(); ss_it != ss.end(); ++ss_it){
    tmp_mu = t.get_ptr(ss_it->first)->get_mu(); // get the value of mu in the leaf
    if(ss_it->second.size() > 0){
      for(int_it it = ss_it->second.begin(); it != ss_it->second.end(); ++it){
        fit[*it] = tmp_mu;
      }
    }
  }

  Rcpp::List results;
  results["fit"] = fit;
  //results["trees"] = tree_string;
  return results;
}


/*
// [[Rcpp::export(".drawEnsemble")]]
Rcpp::List drawEnsemble(Rcpp::NumericMatrix tX_cont,
                        Rcpp::IntegerMatrix tX_cat,
                        Rcpp::LogicalVector unif_cuts,
                        Rcpp::Nullable<Rcpp::List> cutpoints_list,
                        Rcpp::Nullable<Rcpp::List> cat_levels_list,
                        Rcpp::Nullable<Rcpp::List> edge_mat_list,
                        Rcpp::LogicalVector graph_split, int graph_cut_type,
                        double a_cat, double b_cat,
                        bool rc_split, double prob_rc, double a_rc, double b_rc,
                        double alpha, double beta,
                        double mu0, double tau, int M,
                        bool verbose, int print_every)
{
  Rcpp::RNGScope scope;
  RNG gen;
  set_str_conversion set_str; // for converting sets of integers into string
  
  int n = 0;
  int p_cont = 0;
  int p_cat = 0;
  parse_training_data(n,p_cont, p_cat, tX_cont, tX_cat);
  int p = p_cont + p_cat;
  //Rcpp::Rcout << "n = " << n << " p_cont = " << p_cont << " p_cat" << std::endl;
  
  std::vector<std::set<double>> cutpoints;
  if(p_cont > 0){
    if(cutpoints_list.isNotNull()){
      Rcpp::List tmp_cutpoints  = Rcpp::List(cutpoints_list);
      parse_cutpoints(cutpoints, p_cont, tmp_cutpoints, unif_cuts);
    }
  }
  
  std::vector<std::set<int>> cat_levels;
  std::vector<int> K; // number of levels for the different categorical variables
  std::vector<std::vector<edge>> edges;
  
  if(p_cat > 0){
    if(cat_levels_list.isNotNull()){
      Rcpp::List tmp_cat_levels = Rcpp::List(cat_levels_list);
      parse_cat_levels(cat_levels, K, p_cat, tmp_cat_levels);
    } else{
      Rcpp::stop("Must provide list of categorical levels!");
    }
    if(edge_mat_list.isNotNull()){
      Rcpp::List tmp_edge_mat = Rcpp::List(edge_mat_list);
      parse_graphs(edges, p_cat, K, tmp_edge_mat, graph_split);
    }
  }

  data_info di;
  di.n = n;
  di.p_cont = p_cont;
  di.p_cat = p_cat;
  di.p = p;
  if(p_cont > 0) di.x_cont = tX_cont.begin();
  if(p_cat > 0) di.x_cat = tX_cat.begin();

  std::vector<double> theta(p, 1.0/ (double) p);
  //double u = 1.0/(1.0 + (double) p); // unused; consider adding it back in if we turn on sparse
  std::vector<int> var_count(p, 0); // count how many times a variable has been used in a splitting rule
  int rule_count = 0; // how many total decision rules are there in the ensemble
  int rc_rule_count = 0; // how many random combination rules are there in the ensemble
  int rc_var_count = 0; // only when we are using random combination rules
  double theta_rc = 0.0; // prob of including a variable in a random combination rule
  if(p_cont >= 2 && rc_split){
    theta_rc = 2.0/( (double) p_cont);
  }
  
  tree_prior_info tree_pi;
  tree_pi.alpha = alpha;
  tree_pi.beta = beta;
  tree_pi.theta = &theta;
  tree_pi.var_count = &var_count;
  tree_pi.rule_count = &rule_count;
  
  if(p_cont > 0){
    tree_pi.unif_cuts = unif_cuts.begin(); // do we use uniform cutpoints?
    tree_pi.cutpoints = &cutpoints;
  }
  
  if(p_cat > 0){
    tree_pi.cat_levels = &cat_levels;
    tree_pi.edges = &edges;
    tree_pi.K = &K;
    tree_pi.graph_split = graph_split.begin();
    tree_pi.graph_cut_type = graph_cut_type;
    tree_pi.a_cat = a_cat;
    tree_pi.b_cat = b_cat;
  }
  tree_pi.rc_split = rc_split;
  tree_pi.prob_rc = &prob_rc;
  tree_pi.theta_rc = &theta_rc;
  tree_pi.rc_var_count = &rc_var_count;
  tree_pi.rc_rule_count = &rc_rule_count;
  tree_pi.mu0 = mu0;
  tree_pi.tau = tau;
  
  Rcpp::NumericMatrix tree_fits(n,M);
  Rcpp::IntegerMatrix leaf_id(n,M);
  Rcpp::NumericVector fit(n);
  arma::mat kernel = arma::zeros<arma::mat>(n,n); // kernel(i,ii) counts #times obs i & j in same leaf
  
  for(int i = 0; i < n; i++) fit[i] = 0.0;
  Rcpp::CharacterVector tree_strings(M);
  
  
  tree t;
  suff_stat ss;
  double tmp_mu;
  
  for(int m = 0; m < M; m++){
    if(verbose && m % print_every == 0) Rcpp::Rcout << "Drawing tree " << m+1 << " of " << M << std::endl; 
    t.to_null();
    draw_tree(t, di, tree_pi, gen);
    ss.clear();
    tree_traversal(ss,t,di);
    for(suff_stat_it ss_it = ss.begin(); ss_it != ss.end(); ++ss_it){
      tmp_mu = t.get_ptr(ss_it->first)->get_mu();
      for(int_it it = ss_it->second.begin(); it != ss_it->second.end(); ++it){
        tree_fits(*it,m) = tmp_mu;
        fit[*it] += tmp_mu;
        leaf_id(*it,m) = ss_it->first; // id of the leaf
        
        for(int_it iit = it; iit != ss_it->second.end(); ++iit){
          if(*it != *iit){
            kernel(*it, *iit) += 1.0;
            kernel(*iit, *it) += 1.0;
          } else{
            kernel(*it, *iit) += 1.0;
          }
        }
      }
    }
    tree_strings[m] = write_tree(t, tree_pi, set_str);
  }
  kernel /= (double) M;
  Rcpp::List results;
  results["fit"] = fit;
  results["trees"] = tree_strings;
  results["tree_fits"] = tree_fits;
  results["leaf"] = leaf_id;
  results["kernel"] = kernel;
  return results;
}
*/
