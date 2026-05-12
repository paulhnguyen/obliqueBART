#ifndef GUARD_test_opt_lib_h
#define GUARD_test_opt_lib_h

#include "stdafx.h"
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "optimization.h"
#include "string"
#include "rng.h"

using namespace alglib;

// Function to convert vector of maps to a real_2d_array
real_2d_array maps_to_array(const std::vector<std::map<int, double>>& vector_of_maps,
                             int p_cont = 0);

std::vector<double> map_to_vec(std::map<int,double> phi,
                                int p_cont = 0);

// Function to convert vector of maps to a real_2d_array, except we add the constraint at the end to do one sided constraints
real_2d_array maps_b_to_array(const std::vector<std::map<int, double>>& vector_of_maps,
                            std::vector<double> b_vec,
                            int p_cont = 0);


// Function to solve our problem: need to give it: phi_vec (vector of maps), b_vec (vector of cutpoints), c_vec (vector of coefficients), p_cont (int). we will return a vector of doubles, converted from alglib's 1d array
// assumes box -1 < xi < 1 for all i
// assumes all constraints take form ax <= b
// set max = false if we want minimizer instead

std::vector<double> solve_problem(const std::vector<std::map<int, double>>& vector_of_maps,
                            std::vector<double> b_vec,
                            std::vector<double> coef_vec,
                            int p_cont = 0,
                            bool min = true);
#endif 