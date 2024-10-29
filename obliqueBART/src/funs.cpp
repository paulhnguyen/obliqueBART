#include "funs.h"
#include <algorithm>
#include <random>

void tree_traversal(suff_stat &ss, tree &t, data_info &di)
{
  double* xx_cont = 0;
  int* xx_cat = 0;
  tree::tree_cp bn;
  int nid;
  ss.clear(); // clear out the sufficient statistic map
  
  tree::npv bnv;
  t.get_bots(bnv);
  suff_stat_it ss_it; // used to look up to which element of ss a particular bottom node corresponds
  
  // add an element to suff stat map for each bottom node
  for(tree::npv_it it = bnv.begin(); it != bnv.end(); ++it){
    nid = (*it)->get_nid(); // bnv is a vector of pointers. it points to elements of bnv so we need (*it) to access the members of these elements
    // add element to sufficient stats map with key = nid, value = empty vector to hold index of observations assigned to this node
    ss.insert(std::pair<int,std::vector<int>>(nid,std::vector<int>()));
  }
  
  // ready to loop over all observations
  for(int i = 0; i < di.n; ++i){
    if(di.x_cont != 0) xx_cont = di.x_cont + i * di.p_cont;
    if(di.x_cat != 0) xx_cat = di.x_cat + i * di.p_cat;
        
    bn = t.get_bn(xx_cont, xx_cat);
    if(bn == 0){
      Rcpp::Rcout << "i = " << i << std::endl;
      t.print();
      Rcpp::stop("[tree_traversal]: could not find bottom node!");
    }
    else{
      nid = bn->get_nid();
      if(ss.count(nid) != 1) Rcpp::stop("[tree_traversal]: bottom node not included in sufficient statistic map!"); // should never be encountered
      else{
        ss_it = ss.find(nid); // iterator now set to element of ss corresponding to the bottom node holding observation i
        ss_it->second.push_back(i);
      } // closes if/else checking that i-th observation's bottom node was in our map
    } // closes if/else checking that i-th observation maps to valid bottom node
  } // closes loop over all observation
}


void fit_ensemble(std::vector<double> &fit, std::vector<tree> &t_vec, data_info &di){
  if(fit.size() != di.n) Rcpp::stop("[fit_ensemble]: size of fit must be equal to di.n!"); // honestly should never get triggered
  double* xx_cont = 0;
  int* xx_cat = 0;
  for(int i = 0; i < di.n; i++){
    if(di.x_cont != 0) xx_cont = di.x_cont + i * di.p_cont;
    if(di.x_cat != 0) xx_cat = di.x_cat + i * di.p_cat;
    fit[i] = 0.0;
    for(int m = 0; m < t_vec.size(); m++) fit[i] += t_vec[m].evaluate(xx_cont, xx_cat); // involves a tree traversal
  }
}


void update_theta_combination(std::vector<double> &theta_combination, double &u_comb, std::vector<int> &combination_count, int &p, double &a_u, double &b_u, RNG &gen)
{
  int pc2 = (p-1) * p / 2;
  if(theta_combination.size() != pc2){
    Rcpp::Rcout << "theta_combination has size " << theta_combination.size() << "  p choose 2 = " << pc2 << std::endl;
    Rcpp::stop("theta_combination must have size p choose 2!");
  } else{
    double tmp_sum = 0.0;
    double tmp_concentration = 0.0;
    double sum_log_theta = 0.0;
    int comb_count;
    std::vector<double> tmp_gamma(pc2, 0.0);
    
    // update theta first
    double u_orig = u_comb;
    for(int j = 0; j < pc2; j++){
      comb_count = combination_count[j];
      tmp_concentration = u_orig/(1.0 - u_orig) + (double) comb_count;
      tmp_gamma[j] = gen.gamma(tmp_concentration, 1.0);
      tmp_sum += tmp_gamma[j];
    }
    for(int j = 0; j < pc2; j++){
      theta_combination[j] = tmp_gamma[j]/tmp_sum;
      sum_log_theta += log(theta_combination[j]);
      if (theta_combination[j] != theta_combination[j])
      { 
         Rcpp::stop("NAN in theta");
      }
      
    }
    
    // we're now ready to update u
    double u_prop = gen.beta(a_u,b_u);
    double log_like_prop = (u_prop)/(1.0 - u_prop) * sum_log_theta;
    double log_like_orig = (u_orig)/(1.0 - u_orig) * sum_log_theta;
    
    log_like_prop += lgamma( (double) pc2 * u_prop/(1.0 - u_prop)) - ((double) pc2) * lgamma(u_prop/(1.0 - u_prop));
    log_like_orig += lgamma( (double) pc2 * u_orig/(1.0 - u_orig)) - ((double) pc2) * lgamma(u_orig/(1.0 - u_orig));
    double log_accept = log_like_prop - log_like_orig;
    if(gen.log_uniform() <= log_accept) u_comb = u_prop;
    else u_comb = u_orig;
  }
}


void update_theta_u(std::vector<double> &theta, double &u, std::vector<int> &var_count, int &p, double &a_u, double &b_u, RNG &gen)
{
    double tmp_sum = 0.0;
    double tmp_concentration = 0.0;
    double sum_log_theta = 0.0;
    int v_count;
    std::vector<double> tmp_gamma(p, 0.0);
    
    // update theta first
    double u_orig = u;
    for(int j = 0; j < p; j++){
      v_count = var_count[j];
      tmp_concentration = u_orig/(1.0 - u_orig) + (double) v_count;
      tmp_gamma[j] = gen.gamma(tmp_concentration, 1.0);
      tmp_sum += tmp_gamma[j];
    }
    for(int j = 0; j < p; j++){
      theta[j] = tmp_gamma[j]/tmp_sum;
      sum_log_theta += log(theta[j]);
    }
    
    // we're now ready to update u
    double u_prop = gen.beta(a_u,b_u);
    double log_like_prop = (u_prop)/(1.0 - u_prop) * sum_log_theta;
    double log_like_orig = (u_orig)/(1.0 - u_orig) * sum_log_theta;
    
    log_like_prop += lgamma( (double) p * u_prop/(1.0 - u_prop)) - ((double) p) * lgamma(u_prop/(1.0 - u_prop));
    log_like_orig += lgamma( (double) p * u_orig/(1.0 - u_orig)) - ((double) p) * lgamma(u_orig/(1.0 - u_orig));
    double log_accept = log_like_prop - log_like_orig;
    if(gen.log_uniform() <= log_accept) u = u_prop;
    else u = u_orig;
}



// function for dirichlet process draw
std::map<int,double> perform_draw(int alpha_dp, int p, std::map<int, std::map<int, double>> &combinations_map, std::vector<int> &urn, int &index, RNG &gen){
  std::vector<double> draw_probs;
  for (size_t i = 0; i < urn.size(); i++)
  {
    draw_probs.push_back((double) 1/urn.size());
  }
  int draw = gen.categorical(draw_probs);
  std::map<int, double> new_combo;
  // Rcpp::Rcout << "[funs] Draw: " << draw << std::endl;
  if (urn[draw] == -1) // corresponds to black ball, need to draw a new phi
  {
    index = index + 1;
    int length = ceil(p * gen.beta(2, p)); // get length of new combination
    // Rcpp::Rcout << "[funs] length: " << length << "\n";
    std::vector<int> new_combo_indices(p);
    std::iota (std::begin(new_combo_indices), std::end(new_combo_indices), 0); 
    // obtain a time-based seed:
    unsigned seed = std::chrono::system_clock::now().time_since_epoch().count();
    std::default_random_engine e(seed);
    std::shuffle(new_combo_indices.begin(), new_combo_indices.end(), e);
    // Rcpp::Rcout << "[funs] Shuffled Indices vector: ";
    // for (size_t i = 0; i < new_combo_indices.size(); i++)
    // {
    //   Rcpp::Rcout << new_combo_indices[i] << ", ";
    // }
    // Rcpp::Rcout << "\n";
    
    new_combo_indices.resize(length);
    // Rcpp::Rcout << "[funs] Resized Indices vector: ";
    // for (size_t i = 0; i < new_combo_indices.size(); i++)
    // {
    //   Rcpp::Rcout << new_combo_indices[i] << ", ";
    // }
    
    // Rcpp::Rcout << "\n";
    vector<double> new_combo_vals(length);
    for (int i = 0; i < length; i++)
    {
      new_combo_vals[i] = gen.normal();
    }
    
    if (new_combo_vals.size() != new_combo_indices.size())
    {
      Rcpp::stop("[funs] [dp_adaptive_imp]: new combo vals size != new combo indices size");
    }
    for (size_t i = 0; i < new_combo_indices.size(); ++i){
      new_combo.insert(std::pair<int,double>(new_combo_indices[i], new_combo_vals[i]));
    }
    // Rcpp::Rcout << "[funs] Old Urn Vec: ";
    // for (size_t i = 0; i < urn.size(); i++)
    // {
    //   Rcpp::Rcout << urn[i] << ", ";
    // }
    // Rcpp::Rcout << "\n";
    
    urn.push_back(index);
    // Rcpp::Rcout << "[funs] New Urn Vec: ";
    // for (size_t i = 0; i < urn.size(); i++)
    // {
    //   Rcpp::Rcout << urn[i] << ", ";
    // }
    // Rcpp::Rcout << "\n";
    // Rcpp::Rcout << "[funs] New map: " << std::endl;
    // for (const auto& [key, value] : new_combo)
    // {
    //   Rcpp::Rcout << key << ": " << value << '\n';
    // }
    combinations_map.insert(std::pair<int, std::map<int, double>>(index, new_combo));
  } else{
    new_combo = combinations_map.at(urn[draw]);
    // Rcpp::Rcout << "[funs] Returning previous map: " <<std::endl;
    // for (const auto& [key, value] : new_combo)
    // {
    //   Rcpp::Rcout << key << ": " << value << '\n';
    // }
    
    urn.push_back(urn[draw]);
    // Rcpp::Rcout << "[funs] New Urn Vec: ";
    // for (size_t i = 0; i < urn.size(); i++)
    // {
    //   Rcpp::Rcout << urn[i] << ", ";
    // }
    // Rcpp::Rcout << "\n";
  }
  // Rcpp::Rcout << "[funs] Returning map: " <<std::endl;
  //   for (const auto& [key, value] : new_combo)
  //   {
  //     Rcpp::Rcout << key << ": " << value << '\n';
  //   }
  return new_combo;
} 

/*
// functions to help draw oblique rules
void calc_norms(std::vector<std::map<int,double>> &phi_vec, std::vector<double> &phi_norms){
  phi_norms.clear(); // we are changing the value of phi_norms!!!
  
  std::map<int,double> phi_i;
  double tmp_norm;
  std::map<int,double>::iterator it; // create iterator for std::map
  
  for(std::vector<std::map<int,double>>::iterator phi_it = phi_vec.begin();
      phi_it != phi_vec.end(); ++phi_it){
    tmp_norm = 0.0;
    for(std::map<int,double>::iterator it = phi_it->begin(); it != phi_it->end(); ++it){
      tmp_norm += pow(it->second, 2.0);
    }
    phi_norms.push_back(std::sqrt(tmp_norm));
  }
}


//helper function calculates the dot product between a sparse vector phi_i saved as an std::map<int,double>
// and a dense vector x saved as a std::vector<double>
// the dot product is returned as a double to the variable passes as dot_prod
void calc_dot_prod(std::map<int,double> &phi_i, std::vector<double> &x, double &dot_prod){
  dot_prod = 0.0;
  for(std::map<int,double>::iterator it = phi_i.begin(); it != phi_i.end(); ++it){
    dot_prod += (it->second) * x[it->first];
  }
}
*/



/*
void update_theta_u(std::vector<double> &theta, double &u, std::vector<int> &var_count, int &p, double &a_u, double &b_u, RNG &gen)
{
  if(theta.size() != p){
    Rcpp::Rcout << "theta has size " << theta.size() << "  p = " << p << std::endl;
    Rcpp::stop("theta must have size p!");
  } else{
    double tmp_sum = 0.0;
    double tmp_concentration = 0.0;
    double sum_log_theta = 0.0;
    int v_count;
    std::vector<double> tmp_gamma(p, 0.0);
    
    // update theta first
    double u_orig = u;
    for(int j = 0; j < p; j++){
      v_count = var_count[j];
      tmp_concentration = u_orig/(1.0 - u_orig) + (double) v_count;
      tmp_gamma[j] = gen.gamma(tmp_concentration, 1.0);
      tmp_sum += tmp_gamma[j];
    }
    for(int j = 0; j < p; j++){
      theta[j] = tmp_gamma[j]/tmp_sum;
      sum_log_theta += log(theta[j]);
    }
    
    // we're now ready to update u
    double u_prop = gen.beta(a_u,b_u);
    double log_like_prop = (u_prop)/(1.0 - u_prop) * sum_log_theta;
    double log_like_orig = (u_orig)/(1.0 - u_orig) * sum_log_theta;
    
    log_like_prop += lgamma( (double) p * u_prop/(1.0 - u_prop)) - ((double) p) * lgamma(u_prop/(1.0 - u_prop));
    log_like_orig += lgamma( (double) p * u_orig/(1.0 - u_orig)) - ((double) p) * lgamma(u_orig/(1.0 - u_orig));
    double log_accept = log_like_prop - log_like_orig;
    if(gen.log_uniform() <= log_accept) u = u_prop;
    else u = u_orig;
  }
}

void update_theta_rc(double& theta_rc, int &rc_var_count, int &rc_rule_count, double &a_rc, double &b_rc, int &p_cont, RNG &gen)
{
  // since we disallow rc rules with 0 or 1 variable, the posterior is proportional to
  // theta^(a + rc_var_count - 1) * (1 - theta)^(b + p_cont * rc_rule_count - rc_var_count - 1)/(1 - (1- theta)^p_cont - p_cont * theta * 1 - theta)^(p_cont - 1))
  // note that the denominator here is just P(Bin(p_cont, theta) >= 2) as a function of theta
  
  // use independence MH: transition proposal is Beta(a  + rc_var_count, b + p_cont * rc_rule_count)
  // acceptance ratio turns out to:
  // P( Bin(p_cont, theta_orig) >= 2) / P(Bin(p_cont, theta_prop))
  
  double a_post = a_rc + (double) rc_var_count;
  double b_post = b_rc + ((double) p_cont) * rc_rule_count - (double) rc_var_count;
  
  double theta_orig = theta_rc;
  double theta_prop = gen.beta(a_post, b_post);
  
  double log_post_prop = log(1.0 - pow(1.0 - theta_prop, p_cont) - (double) p_cont * theta_prop * pow(1.0 - theta_prop, p_cont-1));
  double log_post_orig = log(1.0 - pow(1.0 - theta_orig, p_cont) - (double) p_cont * theta_orig * pow(1.0 - theta_orig, p_cont-1));
 
  double log_alpha = log_post_orig - log_post_prop;
  if(gen.log_uniform() < log_alpha) theta_rc = theta_prop;
  else theta_rc = theta_orig;
}
*/
/* eventually we will add this functionality back in.


void update_theta_u_cat(std::vector<double> &theta_cat, std::vector<int> &cat_var_count, double &u_cat, double& a_cat, double& b_cat, int &p_cat, RNG &gen)
{
  // stuff for updating theta
  double tmp_sum = 0.0;
  int v_count = 0;
  double tmp_concentration = 0.0;
  std::vector<double> tmp_gamma(p_cat);
  
  // stuff for updating u
  double u_prop = 0.0;
  double u_orig = 0.0;
  double sum_log_theta = 0.0;
  double log_like_prop = 0.0;
  double log_like_orig = 0.0;
  double log_accept = 0.0;
  
  u_orig = u_cat;
  for(int j = 0; j < p_cat; j++){
    v_count = cat_var_count[j];
    tmp_concentration = u_orig/(1.0 - u_orig) + (double) v_count;
    tmp_gamma[j] = gen.gamma(tmp_concentration, 1.0);
    tmp_sum += tmp_gamma[j];
  }
  for(int j = 0; j < p_cat; j++){
    theta_cat[j] = tmp_gamma[j]/tmp_sum;
    sum_log_theta += log(theta_cat[j]);
  }
  
  u_prop = gen.beta(a_cat, b_cat);
  log_like_prop = (u_prop)/(1.0 - u_prop) * sum_log_theta;
  log_like_prop += lgamma( ((double) p_cat) * u_prop/(1.0 - u_prop)) - ((double) p_cat) * lgamma(u_prop/(1.0 - u_prop));
  
  log_like_orig = (u_orig)/(1.0 - u_orig) * sum_log_theta;
  log_like_orig += lgamma( ((double) p_cat) * u_orig/(1.0 - u_orig)) - ( (double) p_cat) * lgamma(u_orig/(1.0 - u_orig));
  
  log_accept = log_like_prop - log_like_orig;
  if(log_accept >= 0.0) log_accept = 0.0;
  if(gen.log_uniform() <= log_accept) u_cat = u_prop;
  else u_cat = u_orig;
}
 
 
 
 void update_theta_cont(std::vector<double> &theta_cont, std::vector<int> &cont_var_count, int &cont_rule_count, double &a_cont, double &b_cont, int &p_cont, RNG &gen)
 {
   if(theta_cont.size() != p_cont) Rcpp::stop("[update_theta_cont]: theta_cont must have size p_cont");
   double a_post = a_cont;
   double b_post = b_cont;
   for(int j = 0; j < p_cont; j++){
     a_post = a_cont + (double) cont_var_count[j];
     b_post = b_cont + (double)(cont_rule_count - cont_var_count[j]);
     theta_cont[j] = gen.beta(a_post, b_post);
   }
   
 }
*/


