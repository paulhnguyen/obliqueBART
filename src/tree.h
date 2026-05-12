#ifndef GUARD_tree_h
#define GUARD_tree_h

#include "structs.h"
#include "rng.h"




class tree {
public:
   //------------------------------
   //friends
  //friend std::string write_tree(tree &t, tree_prior_info &tree_pi, set_str_conversion &set_str);
  //friend void read_tree(tree &t, std::string &tree_string, set_str_conversion &set_str);
  
  friend void grow_tree(tree &t, std::map<int, std::vector<double>> &analytic_centers, suff_stat &ss_train, suff_stat &ss_test, int &accept, rule_diag_t &rule_diag, double &sigma, data_info &di_train, data_info &di_test, tree_prior_info &tree_pi, RNG &gen);
  friend void prune_tree(tree &t, std::map<int, std::vector<double>> &analytic_centers, suff_stat &ss_train, suff_stat &ss_test, int &accept, double &sigma, data_info &di_train, data_info &di_test, tree_prior_info &tree_pi, RNG &gen);
  friend void update_tree(tree &t, std::map<int, std::vector<double>> &analytic_centers, suff_stat &ss_train, suff_stat &ss_test, int &accept, rule_diag_t &rule_diag, double &sigma, data_info &di_train, data_info &di_test, tree_prior_info &tree_pi, RNG &gen);
  
  //------------------------------
  //typedefs
  typedef tree* tree_p;
  typedef const tree* tree_cp;
  typedef std::vector<tree_p> npv; // vector of pointers to nodes in a tree
  typedef npv::iterator npv_it; // iterator for vector of node pointers
  typedef std::vector<tree_cp> cnpv; //vector of constant node pointers const
  typedef cnpv::iterator cnpv_it; // iterator for vector of constant node pointer

  //------------------------------
  //tree constructors, destructors
  tree();
  ~tree() {to_null();}

  //------------------------------
  //operators
  tree& operator=(const tree&);

  // tree-level functions
  void to_null(); //like a "clear", null tree has just one node, the root
  void print() const;

  //node-level sets
  void set_mu(double mu) {this->mu=mu;}
  
  //node-level gets
  double get_mu() const {return mu;}
  
  rule_t get_rule() const{return rule;} // pull out the entire decision rule
  //bool get_is_aa() const{return rule.is_aa;} // are we doing an axis-aligned rule?
  bool get_is_cat() const{return rule.is_cat;} // are we doing a categorical split?
  //int get_v_aa() const{return rule.v_aa;} // if we do axis-aligned split, on which variable are we splitting?
  std::map<int,double> get_phi() const{return rule.phi;} // if we do random combination split, what are the weights?
  double get_cutpoint() const{return rule.c;} // if we have continuous rule, what is the cutpoint?
  
  int get_v_cat() const{return rule.v_cat;} // if we do categorical split, on which variable are we splitting?
  std::set<int> get_lvals() const {return rule.l_vals;} // which levels go to left child?
  std::set<int> get_rvals() const {return rule.r_vals;} // which levels go to right child?
  
  tree_p get_p() const {return p;}  //get the parent of a node
  tree_p get_l() const {return l;} // get the left child of a node
  tree_p get_r() const {return r;} // get the right child of a node
  
  // tree-level gets
  void get_bots(npv& bv); //get bottom nodes
  void get_nogs(npv& nv); //get nog nodes (no granchildren)
  bool is_nog() const;

  void get_nodes(npv& v); //get vector of all nodes
  void get_nodes(cnpv& v) const; //get vector of all nodes but make it constant
  
  int get_treesize() const; // get number of nodes in tree
  int get_nnogs() const; // get number of nog nodes (nodes with no grandchildren)
  int get_nbots() const; // get number of bottom nodes
  
  int get_depth() const; //depth of a node
  int get_overall_depth(); //depth of overall tree structure
  int get_nid() const;   //node id
  char get_ntype() const;   //t:top;b:bottom;n:nog;i:interior, carefull a t can be bot
  tree_p get_ptr(int nid); //get node pointer from node id, 0 if not there.
  tree_cp get_bn(double* x_cont, int* x_cat); // get the pointer corresponding to the bottom node containing x
  
  double evaluate(double* x_cont, int* x_cat); // evaluate tree at a single point
  

  //birth death using nid----------
  void birth(int nid, rule_t rule);
  void death(int nid);

  void get_recent_phi(std::map<int,double> &last_phi, double &last_c, int &last_nid, bool &is_left);
 
  void get_rg_cat(int &v, std::set<int> &levels); // what are the available levels of a categorical variable at some node
  void get_rg_aa(int &v, double &c_lower, double &c_upper);
  
  void get_cont_constraints(std::vector<std::map<int, double>> &phi_vec, std::vector<double> &b_vec,
                                  std::vector<double> &aa_lower, std::vector<double> &aa_upper, int &p_cont);
  
  //void get_prev_constraint_c(std::vector<std::map<int, double>> &phis, std::vector<double> &cs, int &p_cont);
  //void get_prev_constraint_x0(std::vector<std::map<int, double>> &phis, std::vector<std::vector<double>> &x0s, int &p_cont);
  
  /*
   SKD: we will need to write a get_rg_rc for random combination / oblique splits.
   */
private:
   //------------------------------
   //parameter for node
  double mu;
  rule_t rule; // decision rule

  //------------------------------
  //tree structure
  tree_p p; //parent
  tree_p l; //left child
  tree_p r; //right child
  //------------------------------
 
  void cp(tree_p n,  tree_cp o); //copy tree o to n
  
};
#endif
