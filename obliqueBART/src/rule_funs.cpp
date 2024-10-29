
#include "rule_funs.h"
#include "funs.h"

void draw_rule(rule_t &rule, tree &t, int &nid, std::vector<double> &x0, data_info &di, tree_prior_info &tree_pi, RNG &gen){
  rule.clear();
  if(gen.uniform() < *(tree_pi.prob_cat)) draw_cat_rule(rule, t, nid, di, tree_pi, gen);
  else draw_cont_rule(rule, t, nid, x0, di, tree_pi, gen);
}

void draw_rule_lp(rule_t &rule, tree &t, int &nid, data_info &di, tree_prior_info &tree_pi, RNG &gen){
  rule.clear();
  if(gen.uniform() < *(tree_pi.prob_cat)) draw_cat_rule(rule, t, nid, di, tree_pi, gen);
  else draw_cont_rule_lp(rule, t, nid, di, tree_pi, gen);
}

void draw_cat_rule(rule_t &rule, tree &t, int &nid, data_info &di, tree_prior_info &tree_pi, RNG &gen){
  rule.is_cat = true;
  rule.v_cat = floor(di.p_cat * gen.uniform());
  tree::tree_p nx = t.get_ptr(nid); // at what node are we proposing this rule.
  int rule_counter = 0;
  std::set<int> avail_levels = tree_pi.cat_levels->at(rule.v_cat); // get the full set of levels for this variable
  nx->get_rg_cat(rule.v_cat, avail_levels); // determine the set of levels available at nx.
  // if there is only one level left for this variable at nx, we will just propose a trivial split
  // and will reset the value of avail_levels to be the full set of all levels for the variable
  if(avail_levels.size() <= 1) avail_levels = tree_pi.cat_levels->at(rule.v_cat);
  
  rule.l_vals.clear();
  rule.r_vals.clear();
  
  if(tree_pi.graph_split[rule.v_cat] == 1 && tree_pi.edges->at(rule.v_cat).size() > 0){
    // if we explicitly say to use the graph to split the variables
    //graph_partition(avail_levels, rule.l_vals, rule.r_vals, tree_pi.edges->at(rule.v_cat), tree_pi.K->at(rule.v_cat), tree_pi.graph_cut_type, gen);
    graph_partition(rule.l_vals, rule.r_vals, tree_pi.edges->at(rule.v_cat), avail_levels, tree_pi.graph_cut_type, gen);
  } else{
    // otherwise we default to splitting the available levels uniformly at random: prob 0.5 to go to each child
    rule_counter = 0;
    //double tmp_prob = 0.5;
    //if(tree_pi.a_cat > 0  && tree_pi.b_cat > 0) tmp_prob = gen.beta(tree_pi.a_cat, tree_pi.b_cat);
    //else tmp_prob = 0.5;
    
    while( ((rule.l_vals.size() == 0) || (rule.r_vals.size() == 0)) && rule_counter < 1000 ){
      rule.l_vals.clear();
      rule.r_vals.clear();
      for(set_it it = avail_levels.begin(); it != avail_levels.end(); ++it){
        //if(gen.uniform() <= tmp_prob) rule.l_vals.insert(*it);
        if(gen.uniform() <= 0.5) rule.l_vals.insert(*it);
        else rule.r_vals.insert(*it);
      }
      ++(rule_counter);
    }
    if(rule_counter == 1000){
      Rcpp::stop("[draw_cat_rule]: failed to generate valid categorical split in 1000 attempts"); // this should almost surely not get triggered.
    }
  }
  if( (rule.l_vals.size() == 0) || (rule.r_vals.size() == 0) ){
    Rcpp::stop("[draw_cat_rule]: proposed an invalid categorical rule!");
  }
}

// by construction x0 is a point inside the polytope already
void draw_cont_rule(rule_t &rule, tree &t, int &nid, std::vector<double> &x0, data_info &di, tree_prior_info &tree_pi, RNG &gen)
{
  Rcpp::Rcout << "Check point 1"  << std::endl;
  rule.is_cat = false;

  // get constraints
  std::vector<std::map<int,double>> phi_vec;
  std::vector<double> c_vec;
  std::vector<double> aa_lower(di.p_cont, -1.0);
  std::vector<double> aa_upper(di.p_cont, 1.0);
  t.get_ptr(nid)->get_cont_constraints(phi_vec, c_vec, aa_lower, aa_upper, di.p_cont);
  Rcpp::Rcout << "Check point 2"  << std::endl;

  // before we continue, double check that x0 satisfies the parent constraints
  if (!check_constraints(phi_vec, c_vec, x0)){
    Rcpp::Rcout << "[draw_cont_rule]: supplied x0 does not satisfy the constraints of node " << nid << "." << std::endl;
    t.print();
    Rcpp::Rcout << "  The constraints of child polytope are:" << std::endl;
    print_constraints(phi_vec, c_vec);
    Rcpp::Rcout << " The putative point in the polytope is :";
    for(std::vector<double>::iterator x0_it = x0.begin(); x0_it != x0.end(); ++x0_it) Rcpp::Rcout << " " << *x0_it;
    Rcpp::Rcout << std::endl;
    Rcpp::stop("[draw_cont_rule]: supplied parent analytic center does not satisfy constraints!");
  }
   Rcpp::Rcout << "Check point 3"  << std::endl;
  // draw point randomly in polytope
  std::vector<double> split_pt;

  // x0_option == 1: draw constraint through parent analytic center
  // x0_option == 2: draw constraint through random point inside the polytope
  if (tree_pi.x0_option == 1){
    // x0_option == 1: draw constraint through parent analytic center
    for(std::vector<double>::iterator it = x0.begin(); it != x0.end(); ++it) split_pt.push_back(*it);
  }
  else if (tree_pi.x0_option == 2){
    // x0_option == 2: draw constraint through random point inside the polytope
    // use lin_ess to select a random point inside the constrained space
    int d = t.get_ptr(nid)->get_depth() + 1;
    lin_ess(split_pt, phi_vec, c_vec, x0, d, di.p_cont, gen);
    // double check that the point returned by lin_ess satisfies the constraints
    if (!check_constraints(phi_vec, c_vec, split_pt)){
      Rcpp::Rcout << "[draw_cont_rule]: inital point outside polytope!" << std::endl;
      t.print();
      Rcpp::Rcout << "  The constraints of child polytope are:" << std::endl;
      print_constraints(phi_vec, c_vec);
      Rcpp::Rcout << " The putative point in the polytope is :";
      for(std::vector<double>::iterator x0_it = split_pt.begin(); x0_it != split_pt.end(); ++x0_it) Rcpp::Rcout << " " << *x0_it;
      Rcpp::Rcout << std::endl;
      Rcpp::stop("[draw_cont_rule]: lin_ess returned point outside polytope!");
    }
  }
  else {
    Rcpp::Rcout << "x0_option " << tree_pi.x0_option << " passed." << std::endl;
    Rcpp::stop(" blabla [draw_cont_rule]: invalid x0_option! (must be 1 or 2)");
  }

  // we successfully drawn our random point!

  // pick a direction
  rule.phi.clear();

  // decide if the split will be axis-aligned or oblique
  if (gen.uniform() < *tree_pi.prob_aa){
    // axis-aligned split
    // pick a random axis
    int v_aa = floor(di.p_cont * gen.uniform());
    rule.phi.insert(std::pair<int,double>(v_aa, 1.0));
  } else {
    // oblique split
    // phi_option == 1: two-sparse phi
    // phi_option == 2: NOT YET IMPLEMENTED
    // phi_option == 3: NOT YET IMPLEMENTED
    // phi_option == 4: dense phi
    // phi_option == 5: bias towards pi / 4 axes
    if (tree_pi.phi_option == 1){
      // phi_option == 1: two-sparse phi
      // pick two axes at random
      int v1 = floor(di.p_cont * gen.uniform());
      int v2 = floor((di.p_cont - 1) * gen.uniform());
      if (v1 <= v2) ++v2;

      // set these axis to some random nonzero number
      rule.phi.insert(std::pair<int,double>(v1, gen.normal()));
      rule.phi.insert(std::pair<int,double>(v2, gen.normal()));

      // NOTE: SKIP OPTIONS 2 AND 3 FOR NOW
      // we were not using them in our experiments
      // and it is beter to get something up and running first

    } else if (tree_pi.phi_option == 2){
      // phi_option == 2: random element of the null space of random hyperplane for the new rule
      Rcpp::stop("[draw_cont_rule]: sorry, phi_option 2 is not available at this time.");
    } else if (tree_pi.phi_option == 3){
      // phi_option == 3: random linear combination of elements of the null space
      Rcpp::stop("[draw_cont_rule]: sorry, phi_option 3 is not available at this time.");
    } else if (tree_pi.phi_option == 4){
      // phi_option == 4: dense phi
      gen.unif_direction(rule.phi, di.p_cont);
    } else if (tree_pi.phi_option == 5){
      // phi_option == 5: bias towards pi / 4 axes
      // pick two axes at random
      int v1 = floor(di.p_cont * gen.uniform());
      int v2 = floor((di.p_cont - 1) * gen.uniform());
      if (v1 <= v2) ++v2;

      // draw angle of phi from beta(a, b) prior scaled by pi / 2 so we are drawing from the interval (0, pi / 2)
      double theta = gen.beta(tree_pi.a_theta, tree_pi.b_theta) * (M_PI / 2);
      // draw random reflection constant from 2 * bernoulli(0.5) - 1 where the scaling ensures we are drawing from {-1, 1}
      double c = 2 * gen.bernoulli(0.5) - 1;

      // set phi
      rule.phi.insert(std::pair<int,double>(v1, cos(c * theta)));
      rule.phi.insert(std::pair<int,double>(v2, sin(c * theta)));
    } else {
      Rcpp::Rcout << "phi_option " << tree_pi.phi_option << " passed." << std::endl;
      Rcpp::stop("[draw_cont_rule]: invalid phi_option!");
    }
  }

  // normalize phi
  rule.normalize_phi();

  // we have drawn our random direction!

  // set c = phi'x0
  rule.c = 0.0;
  for(rc_it it = rule.phi.begin(); it != rule.phi.end(); ++it) rule.c += it->second * split_pt[it->first];
  
  
}


void draw_cont_rule_lp(rule_t &rule, tree &t, int &nid, data_info &di, tree_prior_info &tree_pi, RNG &gen)
{
  //Rcpp::Rcout << "Check point 1"  << std::endl;
  rule.is_cat = false;

  // get constraints
  std::vector<std::map<int,double>> phi_vec;
  std::vector<double> c_vec;
  std::vector<double> aa_lower(di.p_cont, -1.0);
  std::vector<double> aa_upper(di.p_cont, 1.0);
  t.get_ptr(nid)->get_cont_constraints(phi_vec, c_vec, aa_lower, aa_upper, di.p_cont);
 // Rcpp::Rcout << "Check point 2"  << std::endl;

  //Rcpp::Rcout << "Using cutpoint drawn uniformly from max/min c^T X such that AX < b"  << std::endl;
  // pick a direction
  rule.phi.clear();

  // decide if the split will be axis-aligned or oblique
  if (gen.uniform() < *tree_pi.prob_aa){
    // axis-aligned split
    // pick a random axis
    int v_aa = floor(di.p_cont * gen.uniform());
    rule.phi.insert(std::pair<int,double>(v_aa, 1.0));
  } else {
    // oblique split
    // phi_option == 1: two-sparse phi
    // phi_option == 2: adaptive 2 sparse phi
    // phi_option == 3: three sparse
    // phi_option == 4: dense phi
    // phi_option == 5: bias towards pi / 4 axes
    // phi_option == 6: adaptive dp phi
    // phi_option == 7: spike slab phi
    if (tree_pi.phi_option == 1){
      // phi_option == 1: two-sparse phi
      // pick two axes at random
      int v1 = floor(di.p_cont * gen.uniform());
      int v2 = floor((di.p_cont - 1) * gen.uniform());
      if (v1 <= v2) ++v2;

      // set these axis to some random nonzero number
      rule.phi.insert(std::pair<int,double>(v1, gen.normal()));
      rule.phi.insert(std::pair<int,double>(v2, gen.normal()));

      // NOTE: SKIP OPTIONS 2 AND 3 FOR NOW
      // we were not using them in our experiments
      // and it is beter to get something up and running first

    } else if (tree_pi.phi_option == 2){
       // phi_option == 2: adaptive splits
        // steps: 
        // draw an integer a vector from 1 to p choose 2, with probabilites from theta combination. these probabilities are dependent on the (combination counts). then, draw 2 normal coefficients and assign them to two variables, which are dependent on which integer we drew. then update theta.

        std::vector<double> prob_vec = *(tree_pi.theta_combination);
        // Rcpp::Rcout <<  "[rule_funs] Prob Vec: ";
        // for (size_t i = 0; i < prob_vec.size(); i++)
        // {
        //    Rcpp::Rcout << prob_vec[i] << ", ";
        // }
        //  Rcpp::Rcout << std::endl;
        
        int combination_index = gen.categorical(prob_vec); // draw integer based on probs
        // 0 == 0,1; 1 == 0,2; 2 == 0,3; ... ; p choose 2 - 1 == p-2,p-1
        // Rcpp::Rcout << "[rule_funs] combination index: " << combination_index << std::endl;
        if ((combination_index < 0) || combination_index > prob_vec.size() - 1 )
        {
          Rcpp::Rcout << "Combination index: " << combination_index << std::endl;
          Rcpp::stop("Error drawing combination index");
        }
        // Iterate to find the correct i
        int i = 0;
        while (combination_index >= di.p_cont - i - 1) {
            combination_index -= (di.p_cont - i - 1);
            i++;
        }
        // The remaining k is the position in the list of pairs starting with i
        int j = i + combination_index + 1;  
        rule.phi.insert(std::pair<int,double>(i, gen.normal()));
        rule.phi.insert(std::pair<int,double>(j, gen.normal()));
        // // Rcpp::Rcout << "[rule_funs] Phi: "  << std::endl;
        // for (const auto& pair : rule.phi) {
        //   Rcpp::Rcout << "Key: " << pair.first << ", Value: " << pair.second << std::endl;
        // }

    } else if (tree_pi.phi_option == 3){
      // phi_option == 3: three-sparse phi
      // check that we have at least 3 continuous variables to choose from 
      if (di.p_cont <= 2)
      {
        Rcpp::stop("[draw_cont_rule]: sorry, phi_option 3 requires at least 3 continuous variables");
      }
      else{


        // Sample three numbers
      int v1 = floor(di.p_cont * gen.uniform());
      int v2 = floor((di.p_cont - 1) * gen.uniform());
      int v3 = floor((di.p_cont - 2) * gen.uniform());
      // make sure we dont use same numbers
      if (v2 == v3) {
      ++v2;
      }
      while ((v1 == v3) || (v1 == v2))
      {
        ++v1;
      }

       // set these axis to some random nonzero number
      rule.phi.insert(std::pair<int,double>(v1, gen.normal()));
      rule.phi.insert(std::pair<int,double>(v2, gen.normal()));
      rule.phi.insert(std::pair<int,double>(v3, gen.normal()));

      }
    // else if (tree_pi.phi_option == 3){
    //   // phi_option == 3: random linear combination of elements of the null space
      // Rcpp::stop("[draw_cont_rule]: sorry, phi_option 3 is not available at this time.");
    } else if (tree_pi.phi_option == 4){
      // phi_option == 4: dense phi
      gen.unif_direction(rule.phi, di.p_cont);
    } else if (tree_pi.phi_option == 5){
      // phi_option == 5: bias towards pi / 4 axes
      // pick two axes at random
      int v1 = floor(di.p_cont * gen.uniform());
      int v2 = floor((di.p_cont - 1) * gen.uniform());
      if (v1 <= v2) ++v2;

      // draw angle of phi from beta(a, b) prior scaled by pi / 2 so we are drawing from the interval (0, pi / 2)
      double theta = gen.beta(tree_pi.a_theta, tree_pi.b_theta) * (M_PI / 2);
      // draw random reflection constant from 2 * bernoulli(0.5) - 1 where the scaling ensures we are drawing from {-1, 1}
      double c = 2 * gen.bernoulli(0.5) - 1;

      // set phi
      rule.phi.insert(std::pair<int,double>(v1, cos(c * theta)));
      rule.phi.insert(std::pair<int,double>(v2, sin(c * theta)));
    } else if (tree_pi.phi_option == 6){
      // adaptive dp phi
      // have a vector of indices, and a map with corresponding index + map for phi
      // Rcpp::Rcout << "[rule_funs] Drawing Rule" << "\n";
      std::vector<int> urn = *(tree_pi.urn);
      Rcpp::Rcout << "[rule_funs] Printing Urn:" << "\n";
      for (size_t i = 0; i < urn.size(); i++)
      {
        Rcpp::Rcout << urn[i] << ", ";
      }
      Rcpp::Rcout << "\n";
      // std::map<int, std::map<int, double>> combinations_map =  *(tree_pi.combinations_map);
      // Rcpp::Rcout << "Performing Draw" << "\n";
      rule.phi = perform_draw(tree_pi.alpha_dp, di.p_cont, *tree_pi.combinations_map, *tree_pi.urn, tree_pi.index, gen);
      // Rcpp::Rcout << "[rule_funs] printing Phi:" << "\n";
      // for(auto it = rule.phi.cbegin(); it != rule.phi.cend(); ++it)
      // {
      //     std::cout << it->first << ": " << it->second <<  "\n";
      // }

    } else if (tree_pi.phi_option == 7) //spike slab phi
    {
      // do not do binomial beta update, because that will mess up the conjugate update
      double spike_slab_theta = tree_pi.spike_slab_theta;
      // Rcpp::Rcout << "spike and slab theta: " << spike_slab_theta << std::endl;
      // Rcpp::Rcout << "di.p_cont - 1: " << di.p_cont - 1 << std::endl;
      for (int i = 0; i < di.p_cont; i++)
      {
        int on = gen.bernoulli(spike_slab_theta);
        if (on)
        {
          rule.phi.insert(std::pair<int, double>(i, gen.normal()));
        }
      }
    
      // Rcpp::Rcout << "[rule_funs] printing Phi (Size: " << rule.phi.size() << ")" << "\n";
      //   for(auto it = rule.phi.cbegin(); it != rule.phi.cend(); ++it)
      //   {
      //       Rcpp::Rcout << it->first << ": " << it->second <<  "\n";
      //   }
    }
    else {
      Rcpp::Rcout << "phi_option " << tree_pi.phi_option << " passed." << std::endl;
      Rcpp::stop("[draw_cont_rule]: invalid phi_option!");
    }
  }

  if (rule.phi.size() > 0) // check that phi size > 0, else set equal to 1 and pseudo rule
  {
    // normalize phi
  rule.normalize_phi();

  // set c = phi'x0
  rule.c = 0.0;

  // need to convert rule.phi to be a vector. {{0, 1}, {2, 2}} = [1, 0, 2]
  std::vector<double> coef_vec = map_to_vec(rule.phi, di.p_cont); // most recent phi
//   Rcpp::Rcout << "coef_vec: " ;
// for (size_t i = 0; i < coef_vec.size(); i++)
// {
//   Rcpp::Rcout << coef_vec[i] << " x" << i <<  "  ";
// }
// Rcpp::Rcout << std::endl;

  std::vector<double> max_vec = solve_problem(phi_vec,
                                              c_vec, // Ax < c?
                                              coef_vec, // max/min c^T X; ie coef vec 
                                              di.p_cont,
                                              false);
  // Rcpp::Rcout << "max_vec: " ;
  // for (size_t i = 0; i < max_vec.size(); i++)
  // {
  //   Rcpp::Rcout << max_vec[i] << " ";
  // }
  // Rcpp::Rcout << std::endl;
  
  std::vector<double> min_vec = solve_problem(phi_vec,
                                              c_vec,
                                              coef_vec,
                                              di.p_cont,
                                              true);
  // Rcpp::Rcout << "min_vec: " ;
  // for (size_t i = 0; i < min_vec.size(); i++)
  // {
  //   Rcpp::Rcout << min_vec[i] << " ";
  // }
  // Rcpp::Rcout << std::endl;

  double max_cut = 0;
  double min_cut = 0;
  for (size_t i = 0; i < max_vec.size(); i++)
  {
    max_cut = max_cut + (max_vec[i] * coef_vec[i]);
    min_cut = min_cut + (min_vec[i] * coef_vec[i]);
  }
  if(max_cut != max_cut || min_cut != min_cut) Rcpp::stop("nan in linear program");
  
  // Rcpp::Rcout << "min_cut = : " << min_cut << std::endl;
  // Rcpp::Rcout << "max_cut = : " << max_cut << std::endl;
  rule.c = gen.uniform(std::min(min_cut, max_cut), std::max(min_cut, max_cut));
  if (rule.c != rule.c)
    {
      Rcpp::Rcout << "rule c = " << rule.c << std::endl;
      Rcpp::Rcout << "max_cut = " << max_cut << std::endl;
      Rcpp::Rcout << "min_cut = " << min_cut << std::endl;
    }

  // Rcpp::Rcout << "rule.c: " << rule.c << std::endl;
//   Rcpp::Rcout << "phi (post): " << std::endl;
//   for(const auto& elem : rule.phi)
// {
//    Rcpp::Rcout << elem.first << ": "  << " " << elem.second << "\n";
// }
  }
  else{
    rule.c = 1;
  }
  
  
  
  
  
  
  

}
