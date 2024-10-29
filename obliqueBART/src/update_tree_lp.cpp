#include "update_tree_lp.h"



void grow_tree_lp(tree &t, suff_stat &ss_train, suff_stat &ss_test, int &accept, rule_diag_t &rule_diag, double &sigma, data_info &di_train, data_info &di_test, tree_prior_info &tree_pi, RNG &gen)
{
  
  std::vector<int> bn_nid_vec; // vector to hold the id's of all of the bottom nodes in the tree
  for(suff_stat_it ss_it = ss_train.begin(); ss_it != ss_train.end(); ++ss_it) bn_nid_vec.push_back(ss_it->first);
  
  int ni = floor(gen.uniform() * bn_nid_vec.size()); // randomly pick the index of the node from which we will grow
  int nx_nid = bn_nid_vec[ni]; // id of the node from which we are growing.
  tree::tree_p nx = t.get_ptr(nx_nid); // pointer to the node from which we are growing. refer to this node as nx
  tree::tree_cp nxp = nx->get_p(); // pointer to parent of nx in tree
  
  // we are ready to compute the log transition ratio:
  double q_grow_old = tree_pi.prob_b; // transition prob. of growing old tree into new tree
  double q_prune_new = 1.0 - tree_pi.prob_b; // transition prob. of pruning new true into old tree
  
  int nleaf_old = t.get_nbots(); // number of leaves in old tree
  int nnog_old = t.get_nnogs(); // number of nodes in old tree with no grandchildren (nog node)
  int nnog_new = nnog_old; // number of nodes in new tree with no grandchildren
  
  if(nxp == 0){
    // nx is the root node so transition always propose growing it
    q_grow_old = 1.0; //
  } else if(nxp->is_nog()){
    // parent of nx has no grandchildren in old tree
    // in new tree nxp has grandchildren but nx does not
    // hence nnog_new = nnod_old
    nnog_new = nnog_old;
  } else{
    // parent of nx has grandchildren in old tree and will continue to do so in new tree
    // nx has no grandchildren in the new tree
    nnog_new = 1 + nnog_old;
  }
  
  // numerator of transition ratio: P(uniformly pick a nog node in new tree) * P(decide to prune new tree)
  // denominator of transition rate: P(uniformly pick a leaf node in old tree) * P(decide to grow old tree)
  
  double log_trans_ratio = (log(q_prune_new) - log( (double) nnog_new)) - (log(q_grow_old) - log( (double) nleaf_old));

  // for prior ratio:
  // numerator: p(grow at nx) * (1 - p(grow at nxl)) * (1 - p(grow at nxr))
  // denominator: (1 - p(grow at nx))
  // we need 1 - P(grow at nx in old tree) = 1 - alpha(1 + depth(nx))^(-beta) in denominator
  // we need P(grow at nx in new) (1 - P(grow at nxl in Tnew))(1 - P(grow at nxr in Tnew)) in numerator
  
  double p_grow_nx = tree_pi.alpha/pow(1.0 + (double) nx->get_depth(), tree_pi.beta); // prior prob of growing tree at nx
  double p_grow_nxl = tree_pi.alpha/pow(2.0 + (double) nx->get_depth(), tree_pi.beta); // prior prob of growing tree at nxl. remember depth of nxl is 1 + depth of nx
  double p_grow_nxr = tree_pi.alpha/pow(2.0 + (double) nx->get_depth(), tree_pi.beta); // prior prob of growing tree at nxr. remember depth of nxr is 1 + depth of nx
  
  double log_prior_ratio = log(p_grow_nx) + log(1.0 - p_grow_nxl) + log(1.0 - p_grow_nxr) - log(1.0 - p_grow_nx);
  
  // we now are ready to draw a decision rule
  rule_t rule;
  draw_rule_lp(rule, t, nx_nid, di_train, tree_pi, gen); // draw the rule.
  // at this point we have the proposed rule and are ready to update our sufficient statistic map
  suff_stat prop_ss_train;
  compute_suff_stat_grow(ss_train, prop_ss_train, nx_nid, rule, t, di_train); // figure out which training observations from nx move to nxl and nxr
  suff_stat prop_ss_test;
  if(di_test.n > 0){
    compute_suff_stat_grow(ss_test, prop_ss_test, nx_nid, rule, t, di_test); // figure out which testing observation from nx more to nxl and nxr
  }

  /*
     25 Dec: temporarily, we can reject proposals that do not have one observation in each leaf
     this vastly simplifies the analytic center computation
   
   */

  
  int nxl_nid = 2*nx_nid; // id for the left child of nx
  int nxr_nid = 2*nx_nid+1; // id for right child of nx
  
  double nxl_lil = compute_lil(prop_ss_train, nxl_nid, sigma, di_train, tree_pi); // nxl's contribution to log marginal likelihood of new tree
  double nxr_lil = compute_lil(prop_ss_train, nxr_nid, sigma, di_train, tree_pi); // nxr's contribution to log marginal likelihood of new tree
  double nx_lil = compute_lil(ss_train, nx_nid, sigma, di_train, tree_pi); // nx's contribution to log marginal likelihood of old tree
  
  // likelihood ratio also needs to include some constants from prior on jumps condition on tree
  // in GROW move, the new tree has one extra leaf so there's an additional factor of tau^(-1) * exp(-mu0^2/2tau^2) from leaf prior in the numerator
  double log_like_ratio = nxl_lil + nxr_lil - nx_lil - 1.0 * log(tree_pi.tau) - 0.5 * pow(tree_pi.mu0/tree_pi.tau,2.0);
  
  double log_alpha = log_like_ratio + log_prior_ratio + log_trans_ratio; // MH ratio
  if(log_alpha > 0) log_alpha = 0.0; // if MH ratio greater than 1, we set it equal to 1. this is almost never needed
  if(gen.log_uniform() <= log_alpha){ 
    // accept the transition!
    ++(*tree_pi.rule_count); // increment running count of total number of splitting rules
    
    if(rule.is_cat){
      // we accepted a categorical rule
      ++(rule_diag.cat_prop); // increment count of proposed categorical rules
      int v_raw = rule.v_cat + di_train.p_cont;
      ++(tree_pi.var_count->at(v_raw));
    } else{
      // continuous rule
      // update entry counts (first is number of nonzero entries, second is number of zero entries for phi)
      tree_pi.entry_counts->first = tree_pi.entry_counts->first + rule.phi.size(); // add size of phi to non zero part
      // Rcpp::Rcout << "[update_tree_lp] Entry Counts (First): " << tree_pi.entry_counts->first << std::endl;
      tree_pi.entry_counts->second = tree_pi.entry_counts->second + (di_train.p_cont - rule.phi.size()); // add rest to zeros
      // Rcpp::Rcout << "[update_tree_lp] Entry Counts (Second): " << tree_pi.entry_counts->second << std::endl;

      if(rule.phi.size() == 1){
        // turned out to be an axis-aligned rule
        ++(rule_diag.aa_prop);
        // need to increment count for the variable involed
        // rule.phi.begin() points to first (and only) element in phi
        // variable index is the key.
        ++(tree_pi.var_count->at(rule.phi.begin()->first));
        // increment count of axis-aligned rules
        ++(*tree_pi.aa_count);
      } else if (rule.phi.size() > 1){
        // increment count of oblique rules
        ++(*tree_pi.obl_count);
        ++(rule_diag.obl_prop); // increment count of propose oblique rules
      }
      
      if (rule.phi.size() == 2) // update the combination counter if phi.size() = 2 (used for adaptive splits)
      {
        std::pair<int,int> phi_vars;
        std::map<int, double>::iterator phi_it = rule.phi.begin();
        int first_var = phi_it->first; // First key
        ++phi_it; // Move to the second element
        int second_var = phi_it->first; // Second key
        // need to make sure the order is right. make sure smaller number is first
        if (first_var < second_var)
        {
          phi_vars.first = first_var;
          phi_vars.second = second_var;
        } else {
          phi_vars.first = second_var;
          phi_vars.second = first_var;
        }
        // now get index of the pair.
        int index = 0;
        if ((first_var < 0) || (first_var > second_var))
        {
          Rcpp::stop("[grow_rule] Error when getting phi coefficients");
        }
        int p = di_train.p_cont;
        for (int i = 0; i < first_var; i++)
        {
          index = index + p - i - 1;
        }
        index = second_var - first_var - 1; // (0,1) -> 1-0-1= 0 = first element of combination count
        // Rcpp::Rcout << "[update_tree_lp (grow_tree_lp)] phi vars: " << first_var << ", " << second_var << std::endl;
        // Rcpp::Rcout << "[update_tree_lp (grow_tree_lp)] index: " << index << std::endl;
        // Check if the pair was found
        if ((index < 0) || (index > ((p * (p-1)) / 2) - 1 )) {
           Rcpp::stop("[grow_tree] Could not find index of phi variable pair");
            // Rcpp::Rcout << "Pair not found" << std::endl;
        }
        // now index is the index of the pair in phi. need to increment.
          (tree_pi.combination_count)->at(index)++;
        }
    }
    // we need to update ss, the sufficient statistic object
    // this accounting is checked in test_grow_tree();

    suff_stat_it nxl_it = prop_ss_train.find(nxl_nid); // iterator at element for nxl in the proposed suff_stat map
    suff_stat_it nxr_it = prop_ss_train.find(nxr_nid); // iterator at element for nxr in the proposed suff_stat map
    
    if(nxl_it == prop_ss_train.end() || nxr_it == prop_ss_train.end()){
      // couldn't find a key in prop_ss_train equal to nxl_nid or nxr_nid
      Rcpp::Rcout << "[grow_tree]: sufficient stat map for training data not updated correctly in grow move!" << std::endl;
      Rcpp::Rcout << "  left child id = " << nxl_nid << "  right child = " << nxr_nid << std::endl;
      Rcpp::Rcout << "  available ids in map:";
      for(suff_stat_it it = prop_ss_train.begin(); it != prop_ss_train.end(); ++it) Rcpp::Rcout << " " << it->first;
      Rcpp::Rcout << std::endl;
      Rcpp::stop("missing id for either left or right child in proposed suff_stat_map!");
    }
    
    ss_train.insert(std::pair<int,std::vector<int>>(nxl_nid, nxl_it->second)); // add element for nxl in sufficient stat map
    ss_train.insert(std::pair<int,std::vector<int>>(nxr_nid, nxr_it->second)); // add element for nxr in sufficient stat map
    ss_train.erase(nx_nid); // remove element for nx in sufficient stat map
    if(di_test.n > 0){
      nxl_it = prop_ss_test.find(nxl_nid);
      nxr_it = prop_ss_test.find(nxr_nid);
      if(nxl_it == prop_ss_test.end() || nxr_it == prop_ss_test.end()){
        // couldn't find a key in prop_ss_train equal to nxl_nid or nxr_nid
        Rcpp::Rcout << "[grow_tree]: sufficient stat map for testing data not updated correctly in grow move!" << std::endl;
        Rcpp::Rcout << "  left child id = " << nxl_nid << "  right child = " << nxr_nid << std::endl;
        Rcpp::Rcout << "  available ids in map:";
        for(suff_stat_it it = prop_ss_test.begin(); it != prop_ss_test.end(); ++it) Rcpp::Rcout << " " << it->first;
        Rcpp::Rcout << std::endl;
        Rcpp::stop("missing id for either left or right child in proposed suff_stat_map!");
      }
      ss_test.insert(std::pair<int,std::vector<int>>(nxl_nid, nxl_it->second));
      ss_test.insert(std::pair<int,std::vector<int>>(nxr_nid, nxr_it->second));
      ss_test.erase(nx_nid);
    }

    t.birth(nx_nid, rule); // actually do the birth
    accept = 1;
    
   
  } else{
    accept = 0;
    // Rcpp::Rcout << "[update_tree: grow] Did not accept grow" << std::endl;
    if(rule.is_cat){
      ++rule_diag.cat_prop;
      ++rule_diag.cat_rej;
    } else{
      if(rule.phi.size() == 1){
        ++rule_diag.aa_prop;
        ++rule_diag.aa_rej;
      } else{
        ++rule_diag.obl_prop;
        ++rule_diag.obl_rej;
      }
      if (tree_pi.phi_option == 6)
      {
        // if rule is not accepted, then do not include it in the combinations map
        // Rcpp::Rcout << "[update_tree_lp] [grow_tree_lp] Removing rule with index: " << tree_pi.index;
        // tree_pi.combinations_map->erase(tree_pi.index);
        // tree_pi.index = tree_pi.index - 1;
        tree_pi.urn->pop_back();
        // Rcpp::Rcout << "[update_tree_lp] [grow_tree_lp] Urn should remain the same: " << "\n";
        // std::vector<int> urn = *(tree_pi.urn);
        //  for (size_t i = 0; i < tree_pi.urn->size(); i++)
        // {
        //   Rcpp::Rcout << urn[i] << ", ";
        // }
        // Rcpp::Rcout << "\n";
      }
      
    }
    // don't do anything with rule counters or variable splitting counters etc.
  }
}

void prune_tree_lp(tree &t, suff_stat &ss_train, suff_stat &ss_test, int &accept, double &sigma, data_info &di_train, data_info &di_test, tree_prior_info &tree_pi, RNG &gen)
{
  // first we randomly select a nog node
  tree::npv nogs_vec; // vector of pointers to nodes w/ no grandchildren
  t.get_nogs(nogs_vec);

  int ni = floor(gen.uniform() * nogs_vec.size());
  tree::tree_p nx = nogs_vec[ni]; // pointer to node whose children we will prune
  tree::tree_p nxl = nx->get_l(); // left child that will be pruned
  tree::tree_p nxr = nx->get_r(); // right child that will be pruned
  
  // transition ratio stuff
  double q_prune_old = 1.0 - tree_pi.prob_b; // transition prob that we prune old tree
  double q_grow_new = tree_pi.prob_b; // transition prob that we grow new tree
  tree::tree_p nxp = nx->get_p(); // pointer to parent node of nx in old tree
  if(nxp == 0) q_grow_new = 1.0; // nx is top node so new tree is just root and we'd always propose a GROW when encountering new tree
  else q_grow_new = tree_pi.prob_b; // nx is not top node so  given T_new, we propose grow with prob tree_pi.pb

  int nleaf_new = t.get_nbots() - 1; // new tree has one less leaf node than old tree
  int nnog_old = t.get_nnogs(); // number of nodes with no grandchildren in old tree
  
  // numerator of transition ratio: P(uniformly pick a leaf node in new tree) * P(decide to grow newtree)
  // denominator of transition ratio: P(uniformly pick a nog node in old tree) * P(decide to prune old tree)

  double log_trans_ratio = (log(q_grow_new) - log(nleaf_new)) - (log(q_prune_old) - log(nnog_old));

  // prior ratio
  // numerator: we need [1 - P(grow at nx in Tnew)] = 1 - tree_pi.alpha/pow(1 + nx->get_depth(), tree_pi.beta)
  // denom: we need [P(grow at nx in Told)] x [1 - P(grow at nxl in Told)] x [1 - P(grow at nxr in Told)]
  double p_grow_nx = tree_pi.alpha/pow(1.0 + (double) nx->get_depth(), tree_pi.beta); // prior prob of growing tree at nx
  double p_grow_nxl = tree_pi.alpha/pow(2.0 + (double) nx->get_depth(), tree_pi.beta); // prior prob of growing tree at nxl, left child of nx
  double p_grow_nxr = tree_pi.alpha/pow(2.0 + (double) nx->get_depth(), tree_pi.beta); // prior prob of growing tree nxr, right child of nx
  double log_prior_ratio = log(1.0 - p_grow_nx) - (log(1.0 - p_grow_nxl) + log(1.0 - p_grow_nxr) + log(p_grow_nx));
  
  // likelihood ratio
  suff_stat prop_ss_train;
  suff_stat prop_ss_test;
  int nx_nid = nx->get_nid(); // id for nx
  int nxl_nid = nxl->get_nid(); // id for nxl
  int nxr_nid = nxr->get_nid(); // id for nxr
  
  compute_suff_stat_prune(ss_train, prop_ss_train, nxl_nid, nxr_nid, nx_nid, t, di_train); // create a sufficient statistic map for the new tree
  if(di_test.n > 0) compute_suff_stat_prune(ss_test, prop_ss_test, nxl_nid, nxr_nid, nx_nid, t, di_test);
  
  double nxl_lil = compute_lil(ss_train, nxl_nid, sigma, di_train, tree_pi);
  double nxr_lil = compute_lil(ss_train, nxr_nid, sigma, di_train, tree_pi);
  double nx_lil = compute_lil(prop_ss_train, nx_nid, sigma, di_train, tree_pi);
  
  // old tree has one more leaf node than new tree so there is additional factor of
  // (tau)^(-1) * exp(-mu0^2/(2 * tau^2)) in denominator of likelihood ratio that comes from the prior on leafs
  double log_like_ratio = nx_lil - nxl_lil - nxr_lil + log(tree_pi.tau) + 0.5 * pow(tree_pi.mu0/tree_pi.tau,2.0);
  
  double log_alpha = log_like_ratio + log_prior_ratio + log_trans_ratio;
  if(log_alpha > 0) log_alpha = 0; // if MH greater than we, set it equal to 1
  if(gen.log_uniform() <= log_alpha){
    // accept the proposal!
    accept = 1;
    // need to decrement several counters
    --(*tree_pi.rule_count);
    
    

    if(nx->get_is_cat()){
      // we pruned away a categorical rule
      --(tree_pi.var_count->at((nx->get_v_cat()) + di_train.p_cont));
    } else{
      rule_t tmp_rule = nx->get_rule();
      // update entry counts (first is number of nonzero entries, second is number of zero entries for phi)
      tree_pi.entry_counts->first = tree_pi.entry_counts->first - tmp_rule.phi.size(); // add size of phi to non zero part
      // Rcpp::Rcout << "[update_tree_lp] Entry Counts (First): " << tree_pi.entry_counts->first << std::endl;
      tree_pi.entry_counts->second = tree_pi.entry_counts->second - (di_train.p_cont - tmp_rule.phi.size()); // add rest to zeros
      // Rcpp::Rcout << "[update_tree_lp] Entry Counts (Second): " << tree_pi.entry_counts->second << std::endl;

      if(tmp_rule.phi.size() == 1){
        // we pruned away an axis-aligned rule
        --(tree_pi.var_count->at(tmp_rule.phi.begin()->first));
        // deincrement count of axis-align rules
        --(*tree_pi.aa_count);
      } else if (tmp_rule.phi.size() > 1){
        // deincrement count of oblique rules
        --(*tree_pi.obl_count);
      }
      if (tmp_rule.phi.size() == 2) // update the combination counter if phi.size() = 2 (used for adaptive splits)
      {
        std::pair<int,int> phi_vars;
        std::map<int, double>::iterator phi_it = tmp_rule.phi.begin();
        int first_var = phi_it->first; // First key
        ++phi_it; // Move to the second element
        int second_var = phi_it->first; // Second key
        // need to make sure the order is right. make sure smaller number is first
        if (first_var < second_var)
        {
          phi_vars.first = first_var;
          phi_vars.second = second_var;
        } else {
          phi_vars.first = second_var;
          phi_vars.second = first_var;
        }
        // now get index of the pair.
        int index = 0;
        if ((first_var < 0) || (first_var > second_var))
        {
          Rcpp::stop("[prune_rule] Error when getting phi coefficients");
        }
        int p = di_train.p_cont;
        for (int i = 0; i < first_var; i++)
        {
          index = index + p - i - 1;
        }
        index = second_var - first_var - 1;
        // Rcpp::Rcout << "[update_tree_lp (prune_tree_lp)] phi vars: " << first_var << ", " << second_var << std::endl;
        // Rcpp::Rcout << "[update_tree_lp (prune_tree_lp)] index: " << index << std::endl;
        // Check if the pair was found
        if ((index < 0) || (index > ((p * (p-1)) / 2) - 1 )) {
           Rcpp::stop("[prune_rule] Could not find index of phi variable pair");
            // Rcpp::Rcout << "Pair not found" << std::endl;
        }
        // now index is the index of the pair in phi. need to increment.
          (tree_pi.combination_count)->at(index)--;
        }
    }
    
  
    
    // need to adjust ss
    // this accounting is checked in test_prune_tree();
    suff_stat_it nx_it = prop_ss_train.find(nx_nid); // iterator at element for nx in suff stat map for new tree
    if(nx_it == prop_ss_train.end()){
      // did not find nx_nid in the keys of prop_ss_train
      Rcpp::Rcout << "[prune_tree]: did not find id of new leaf node in the keys of training sufficient stat map" << std::endl;
      Rcpp::Rcout << "  id of new leaf: " << nx_nid << std::endl;
      Rcpp::Rcout << "  ids in map:";
      for(suff_stat_it it = prop_ss_train.begin(); it != prop_ss_train.end(); ++it) Rcpp::Rcout << " " << it->first;
      Rcpp::Rcout << std::endl;
      Rcpp::stop("missing id for new leaf node in prune move in training sufficient stat map");
    } else{
      ss_train.erase(nxl_nid); // delete entry for nxl in suff stat map
      ss_train.erase(nxr_nid); // delete entry for nxr in suff stat map
      ss_train.insert(std::pair<int, std::vector<int>>(nx_nid, nx_it->second)); // add an entry for nx in suff stat map
    }
    
    if(di_test.n > 0){
      nx_it = prop_ss_test.find(nx_nid);
      if(nx_it == prop_ss_test.end()){
        // did not find nx_nid in the keys of prop_ss_test
        Rcpp::Rcout << "[prune_tree]: did not find id of new leaf node in the keys of testing sufficient stat map" << std::endl;
        Rcpp::Rcout << "  id of new leaf: " << nx_nid << std::endl;
        Rcpp::Rcout << "  ids in map:";
        for(suff_stat_it it = prop_ss_test.begin(); it != prop_ss_test.end(); ++it) Rcpp::Rcout << " " << it->first;
        Rcpp::Rcout << std::endl;
        Rcpp::stop("missing id for new leaf node in prune move in training sufficient stat map");
      } else{
        ss_test.erase(nxl_nid); // delete entry for nxl in suff stat map
        ss_test.erase(nxr_nid); // delete entry for nxr in suff stat map
        ss_test.insert(std::pair<int, std::vector<int>>(nx_nid, nx_it->second)); // add an entry for nx in suff stat map
      }
    }
    t.death(nx_nid); // actually perform the death
  } else{
    accept = 0;
    //  Rcpp::Rcout << "[update_tree: prune] Did not accept prune" << std::endl;
  }
}

void update_tree_lp(tree &t, suff_stat &ss_train, suff_stat &ss_test, int &accept, rule_diag_t &rule_diag, double &sigma, data_info &di_train, data_info &di_test, tree_prior_info &tree_pi, RNG &gen)
{
  accept = 0; // initialize indicator of MH acceptance to 0 (reject)
  double PBx = tree_pi.prob_b; // prob of proposing a birth move (typically 0.5)
  if(t.get_treesize() == 1) PBx = 1.0; // if tree is just the root, we must always GROW
  
  if(gen.uniform() < PBx) grow_tree_lp(t, ss_train, ss_test, accept, rule_diag, sigma, di_train, di_test, tree_pi, gen);
  else prune_tree_lp(t, ss_train, ss_test, accept, sigma, di_train, di_test, tree_pi, gen);

  // by this point, the decision tree has been updated so we can draw new jumps.
  draw_mu(t, ss_train, sigma, di_train, tree_pi, gen);
}
