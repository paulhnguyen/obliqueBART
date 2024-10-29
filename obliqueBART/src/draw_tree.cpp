#include "draw_tree.h"



void draw_tree(tree &t, std::map<int, std::vector<double>> &analytic_centers,
               data_info &di, tree_prior_info &tree_pi, RNG &gen)
{
  //Rcpp::Rcout << "[draw tree]: started!" << std::endl;
  if(t.get_treesize() > 1){
    t.to_null(); // prune tree back to root
    analytic_centers.clear();
  }
  // tree is currently a root node, so we need to update its analytic center
  analytic_centers.insert(std::pair<int, std::vector<double>>(1, std::vector<double>(di.p_cont, 0.0)));
  
  tree::npv bnv; // bottom node vector
  int dnx; // depth of node nx
  int nx_nid;
  int max_depth = 0; // depth of deepest leaf
  int prev_max_depth = 0; // max depth from previous iteration
  
  double PGnx = 0.0; // probability of growing at node nx
  bool grow = true;
  int counter = 0;
  
  // stuff used for the rule
  rule_t rule;
  
  while(grow && counter < 100){
    prev_max_depth = max_depth;
    bnv.clear();
    t.get_bots(bnv); // get the bottom nodes of the tree (could be done more efficiently but not super important)
 
    for(tree::npv_it l_it = bnv.begin(); l_it != bnv.end(); ++l_it){
      dnx = (*l_it)->get_depth(); // remember l_it is a pointer to an element in bnv, which is itself a pointer, hence the need for (*)->
      if(dnx > max_depth) max_depth = dnx; // the node we're at is deeper than the maximum depth of the tree in the previous iteration
    }
    
    if( (max_depth < prev_max_depth) || (max_depth > 1 + prev_max_depth) ){
      // each time through the loop we try to grow only from the deepest nodes
      // we should *never* encounter this condition but to be safe
      Rcpp::Rcout << "max_depth = " << max_depth << " prev_max_depth = " << prev_max_depth << std::endl;
      Rcpp::stop("[draw_tree]: max depth should be prev_max_depth or prev_max_depth + 1!");
    } else if(max_depth == prev_max_depth && max_depth != 0){
      // tree didn't grow in the last iteration so we should break out of the loop
      break;
    } else{
      grow = false; // initialize flag
      for(tree::npv_it l_it = bnv.begin(); l_it != bnv.end(); ++l_it){
        dnx = (*l_it)->get_depth();
        nx_nid = (*l_it)->get_nid();
        if(dnx == max_depth){
          // current node nx is at the maximum depth, we will try to grow the tree from nx
          PGnx = tree_pi.alpha/pow(1.0 + (double) dnx, tree_pi.beta);
          if(gen.uniform() < PGnx){
            // we're actually going to grow the tree!
            grow = true;
            std::vector<double> tmp_x0 = analytic_centers.find(nx_nid)->second;
            draw_rule(rule, t, nx_nid, tmp_x0, di, tree_pi, gen);
            // actually perform the birth
            t.birth(nx_nid, rule);
            // now that we have birthed the tree, we need to compute analytic centers of two children
            get_child_analytic_centers(analytic_centers, nx_nid, t, di.p_cont, gen);
          } // closes if checking that we're actually trying to grow the tree
        } else{
          // do nothing
        }// closes if/else checking that we're at node at the deepest level of the tree
      } // closes for loop over all bottom nodes in the tree
    } // closes if/else checking that max depth is valid
    ++(counter);
  } // closes main while loop
  
  // now that we have drawn the decision tree, let's draw the jumps
  bnv.clear();
  t.get_bots(bnv);
  for(tree::npv_it l_it = bnv.begin(); l_it != bnv.end(); ++l_it){
    (*l_it)->set_mu(gen.normal(tree_pi.mu0, tree_pi.tau));
  }
  
}

void draw_tree_lp(tree &t, std::map<int, std::vector<double>> &analytic_centers,
               data_info &di, tree_prior_info &tree_pi, RNG &gen)
{
  //Rcpp::Rcout << "[draw tree]: started!" << std::endl;
  if(t.get_treesize() > 1){
    t.to_null(); // prune tree back to root
    analytic_centers.clear();
  }
  // tree is currently a root node, so we need to update its analytic center
  analytic_centers.insert(std::pair<int, std::vector<double>>(1, std::vector<double>(di.p_cont, 0.0)));
  
  tree::npv bnv; // bottom node vector
  int dnx; // depth of node nx
  int nx_nid;
  int max_depth = 0; // depth of deepest leaf
  int prev_max_depth = 0; // max depth from previous iteration
  
  double PGnx = 0.0; // probability of growing at node nx
  bool grow = true;
  int counter = 0;
  
  // stuff used for the rule
  rule_t rule;
  
  while(grow && counter < 100){
    prev_max_depth = max_depth;
    bnv.clear();
    t.get_bots(bnv); // get the bottom nodes of the tree (could be done more efficiently but not super important)
 
    for(tree::npv_it l_it = bnv.begin(); l_it != bnv.end(); ++l_it){
      dnx = (*l_it)->get_depth(); // remember l_it is a pointer to an element in bnv, which is itself a pointer, hence the need for (*)->
      if(dnx > max_depth) max_depth = dnx; // the node we're at is deeper than the maximum depth of the tree in the previous iteration
    }
    
    if( (max_depth < prev_max_depth) || (max_depth > 1 + prev_max_depth) ){
      // each time through the loop we try to grow only from the deepest nodes
      // we should *never* encounter this condition but to be safe
      Rcpp::Rcout << "max_depth = " << max_depth << " prev_max_depth = " << prev_max_depth << std::endl;
      Rcpp::stop("[draw_tree]: max depth should be prev_max_depth or prev_max_depth + 1!");
    } else if(max_depth == prev_max_depth && max_depth != 0){
      // tree didn't grow in the last iteration so we should break out of the loop
      break;
    } else{
      grow = false; // initialize flag
      for(tree::npv_it l_it = bnv.begin(); l_it != bnv.end(); ++l_it){
        dnx = (*l_it)->get_depth();
        nx_nid = (*l_it)->get_nid();
        if(dnx == max_depth){
          // current node nx is at the maximum depth, we will try to grow the tree from nx
          PGnx = tree_pi.alpha/pow(1.0 + (double) dnx, tree_pi.beta);
          if(gen.uniform() < PGnx){
            // we're actually going to grow the tree!
            grow = true;
            draw_rule_lp(rule, t, nx_nid, di, tree_pi, gen);
            // actually perform the birth
            t.birth(nx_nid, rule);
            // now that we have birthed the tree, we need to compute analytic centers of two children
            // get_child_analytic_centers(analytic_centers, nx_nid, t, di.p_cont, gen);
          } // closes if checking that we're actually trying to grow the tree
        } else{
          // do nothing
        }// closes if/else checking that we're at node at the deepest level of the tree
      } // closes for loop over all bottom nodes in the tree
    } // closes if/else checking that max depth is valid
    ++(counter);
  } // closes main while loop
  
  // now that we have drawn the decision tree, let's draw the jumps
  bnv.clear();
  t.get_bots(bnv);
  for(tree::npv_it l_it = bnv.begin(); l_it != bnv.end(); ++l_it){
    (*l_it)->set_mu(gen.normal(tree_pi.mu0, tree_pi.tau));
  }
  
}

void draw_tree(tree &t, suff_stat &ss,
               std::map<int, std::vector<double>> &analytic_centers,
               data_info &di, tree_prior_info &tree_pi, RNG &gen)
{
  //Rcpp::Rcout << "[draw tree]: started!" << std::endl;
  if(t.get_treesize() > 1){
    t.to_null(); // prune tree back to root
    analytic_centers.clear();
    ss.clear();
  }
  // tree is currently a root node, so we need to update its analytic center
  analytic_centers.insert(std::pair<int, std::vector<double>>(1, std::vector<double>(di.p_cont, 0.0)));
  
  tree::npv bnv; // bottom node vector
  int dnx; // depth of node nx
  int nx_nid;
  int max_depth = 0; // depth of deepest leaf
  int prev_max_depth = 0; // max depth from previous iteration
  
  double PGnx = 0.0; // probability of growing at node nx
  bool grow = true;
  int counter = 0;
  
  // stuff used for the rule
  rule_t rule;
  
  while(grow && counter < 100){
    prev_max_depth = max_depth;
    bnv.clear();
    t.get_bots(bnv); // get the bottom nodes of the tree (could be done more efficiently but not super important)
 
    for(tree::npv_it l_it = bnv.begin(); l_it != bnv.end(); ++l_it){
      dnx = (*l_it)->get_depth(); // remember l_it is a pointer to an element in bnv, which is itself a pointer, hence the need for (*)->
      if(dnx > max_depth) max_depth = dnx; // the node we're at is deeper than the maximum depth of the tree in the previous iteration
    }
    
    if( (max_depth < prev_max_depth) || (max_depth > 1 + prev_max_depth) ){
      // each time through the loop we try to grow only from the deepest nodes
      // we should *never* encounter this condition but to be safe
      Rcpp::Rcout << "max_depth = " << max_depth << " prev_max_depth = " << prev_max_depth << std::endl;
      Rcpp::stop("[draw_tree]: max depth should be prev_max_depth or prev_max_depth + 1!");
    } else if(max_depth == prev_max_depth && max_depth != 0){
      // tree didn't grow in the last iteration so we should break out of the loop
      break;
    } else{
      grow = false; // initialize flag
      for(tree::npv_it l_it = bnv.begin(); l_it != bnv.end(); ++l_it){
        dnx = (*l_it)->get_depth();
        nx_nid = (*l_it)->get_nid();
        if(dnx == max_depth){
          // current node nx is at the maximum depth, we will try to grow the tree from nx
          PGnx = tree_pi.alpha/pow(1.0 + (double) dnx, tree_pi.beta);
          if(gen.uniform() < PGnx){
            // we're actually going to grow the tree!
            grow = true;
            std::vector<double> tmp_x0 = analytic_centers.find(nx_nid)->second;
            draw_rule(rule, t, nx_nid, tmp_x0, di, tree_pi, gen);
            // actually perform the birth
            t.birth(nx_nid, rule);
            // now that we birthed the tree, we need to update the suff_stat_maps (similar to what's in grow_tree)
            suff_stat prop_ss;
            compute_suff_stat_grow(ss, prop_ss, nx_nid, rule, t, di);
            int nxl_nid = 2*nx_nid;
            int nxr_nid = 2*nx_nid+1;
            
            suff_stat_it nxl_it = prop_ss.find(nxl_nid); // iterator at element for nxl in the proposed suff_stat map
            suff_stat_it nxr_it = prop_ss.find(nxr_nid); // iterator at element for nxr in the proposed suff_stat map
            
            ss.insert(std::pair<int, std::vector<int>>(nxl_nid, nxl_it->second));
            ss.insert(std::pair<int, std::vector<int>>(nxr_nid, nxr_it->second));
            ss.erase(nx_nid);
            
            // now that we have grown tree and have updated the suff_stat_map,
            // we are ready to compute the analytic centers of the two child polytopes
            get_child_analytic_center(analytic_centers, ss, nx_nid, nxl_nid, t, di, gen);
            get_child_analytic_center(analytic_centers, ss, nx_nid, nxr_nid, t, di, gen);
          } // closes if checking that we're actually trying to grow the tree
        } else{
          // do nothing
        }// closes if/else checking that we're at node at the deepest level of the tree
      } // closes for loop over all bottom nodes in the tree
    } // closes if/else checking that max depth is valid
    ++(counter);
  } // closes main while loop
  
  // now that we have drawn the decision tree, let's draw the jumps
  bnv.clear();
  t.get_bots(bnv);
  for(tree::npv_it l_it = bnv.begin(); l_it != bnv.end(); ++l_it){
    (*l_it)->set_mu(gen.normal(tree_pi.mu0, tree_pi.tau));
  }
}
