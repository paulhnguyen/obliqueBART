#include "test_opt_lib.h"

// ex. vector of maps:

// num_variables = 2, num_rows = 4
// std::vector<std::map<int, double>> phi_vec = {
//         {{0, 1}, {1, -1}},
//         {{0, 3}, {1, 1}},
//         {{0, 4}, {1, 3}},
//         {{1, 1}}
//     };
// std::vector<double> b_vec{2, 5, 7, .1};

// real_2d_array a = "[[1,-1,2],[3,1,5],[4,3,7],[0,1,.1]]";
// integer_1d_array ct = "[-1,-1]";
// Function to create matrix A from a vector of maps
arma::mat create_matrix_a(const std::vector<std::map<int, double>>& vector_of_maps, int p_cont = 0) {
    // Determine the number of variables (columns) in the matrix
    int num_variables = 0;
    int num_rows = vector_of_maps.size();
    for (const std::map<int, double>& map : vector_of_maps) {
        for (const std::pair<int, double>& pair : map) {
            num_variables = std::max(num_variables, pair.first + 1);
        }
    }
    if (p_cont != 0)
    {
        num_variables = p_cont;
    }
    // Initialize an empty matrix
    arma::mat matrix_A(num_rows, num_variables);

    // Populate the matrix based on the vector of maps
    for (size_t row = 0; row < vector_of_maps.size(); ++row) {
        const std::map<int, double>& map = vector_of_maps[row];
        for (const std::pair<int, double>& pair : map) {
            matrix_A(row, pair.first) = pair.second;
        }
    }
    // matrix_A.print("A:");

    return matrix_A;
}


// Function to create relevant strings from a vector of maps
// assumes: all constraints are Ax <= b
// assumes default value p_cont = 0. if p_cont is given, use that for number of variables. else find largest key value in map and use as number of variables. 
std::string create_a_string(const std::vector<std::map<int, double>>& vector_of_maps,
 const std::vector<double> b_vec,
 int p_cont) {
    // Determine the number of variables (columns) in the matrix
    int num_variables = 0;
    int num_rows = vector_of_maps.size();
    for (const std::map<int, double>& map : vector_of_maps) {
        for (const std::pair<int, double>& pair : map) {
            num_variables = std::max(num_variables, pair.first + 1);
        }
    }
    if (p_cont != 0)
    {
        num_variables = p_cont;
    }
    
    Rcpp::Rcout << "Num variables: " << num_variables << std::endl;
    Rcpp::Rcout << "Num rows: " << num_rows << std::endl;

    // we actually add an extra addition at the end of each row: the b component for Ax < b
    arma::mat matrix_a = create_matrix_a(vector_of_maps, p_cont);
    matrix_a.print("Matrix A: ");
    Rcpp::Rcout << "Num Cols: " << matrix_a.n_cols << std::endl;

    // Initialize an starting string
    std::string s0 ("[");
    // add rows to string, each element seperated by a comma
    // build string for individual row
    for (int row_num = 0; row_num < matrix_a.n_rows; ++row_num) {
        std::string block ("[");
        for (int col_num = 0; col_num < matrix_a.n_cols; ++col_num) {
            block = block + std::to_string(matrix_a(row_num, col_num)) + ",";
        }
        block = block + std::to_string(b_vec[row_num]);
        if (!block.empty()) {
            block.replace(block.size() - 1, 1, "]");
        }
        Rcpp::Rcout << "Block: " << block << std::endl;
        s0 = s0 + block;
        if (row_num != matrix_a.n_rows - 1)
        {
            s0 = s0 + ",";
        }
        
    }
    s0 = s0 + "]";

    // Populate the matrix based on the vector of maps
    // for (size_t row = 0; row < vector_of_maps.size(); ++row) {
    //     const std::map<int, double>& map = vector_of_maps[row];
    //     for (const std::pair<int, double>& pair : map) {
    //         matrix_A(row, pair.first) = pair.second;
    //     }
    // }


    return s0;
}

// Function to convert vector of maps to a real_2d_array
real_2d_array maps_to_array(const std::vector<std::map<int, double>>& vector_of_maps,
                             int p_cont) {
    // Get the size of the vector (number of rows)
    int rows = static_cast<int>(vector_of_maps.size());
    if (rows == 0) {
        std::cerr << "Error: Empty vector." << std::endl;
        return real_2d_array(); // Return an empty array
    }

    // Assume all rows have the same number of columns
    int num_variables = 0;
    for (const std::map<int, double>& map : vector_of_maps) {
        for (const std::pair<int, double>& pair : map) {
            num_variables = std::max(num_variables, pair.first + 1);
        }
    }
    if (p_cont != 0)
    {
        num_variables = p_cont;
    }

    // Create the real_2d_array and set its size
    real_2d_array a;
    a.setlength(rows, num_variables);

    // Fill the array with values from the vector of maps
    for (int i = 0; i < rows; ++i) {
        const std::map<int, double>& row_map = vector_of_maps[i];
        for (int j = 0; j < num_variables; ++j) {
            auto it = row_map.find(j);
            if (it != row_map.end()) {
                // If key j exists in the map, set the value in the array
                a[i][j] = it->second;
            } else {
                // If key j does not exist, set to 0 or any default value
                a[i][j] = 0.0; // Default to 0.0
            }
        }
    }

    return a;
}

std::vector<double> map_to_vec(std::map<int,double> phi,
                               int p_cont){
    std::vector<double> phi_vec(p_cont, 0);
    // for each element in the map, replace the vector element with the double from map
    for (const auto& pair : phi) {
        int index = pair.first;
        double value = pair.second;

        // Check if the index is within the bounds of phi_vec
        if (index >= 0 && index < p_cont) {
            // Assign the value to the corresponding element of phi_vec
            phi_vec[index] = value;
        }
    }

    return phi_vec;
}


// Function to convert vector of maps to a real_2d_array, except we add the constraint at the end to do one sided constraints
real_2d_array maps_b_to_array(const std::vector<std::map<int, double>>& vector_of_maps,
                            std::vector<double> b_vec,
                            int p_cont) {
    // Get the size of the vector (number of rows)
    if (b_vec.size() != vector_of_maps.size()) {
        std::cerr << "Phi and b should be same size." << std::endl;
        return real_2d_array(); // Return an empty array
    }
    int rows = static_cast<int>(vector_of_maps.size());
    if (rows == 0) {
        std::cerr << "Error: Empty vector." << std::endl;
        return real_2d_array(); // Return an empty array
    }
    // finding the number of columns. If p_cont provided, use p_cont. else find max in vector of maps
    int num_variables = 0;
    for (const std::map<int, double>& map : vector_of_maps) {
        for (const std::pair<int, double>& pair : map) {
            num_variables = std::max(num_variables, pair.first + 1);
        }
    }
    
    if (p_cont != 0)
    {
        num_variables = p_cont;
    }
    // Rcpp::Rcout << "Num Variables: " << num_variables << std::endl;
    // Create the real_2d_array and set its size. one extra column for c
    real_2d_array a;
    a.setlength(rows, num_variables + 1);

    // Fill the array with values from the vector of maps
    for (int i = 0; i < rows; ++i) {
        const std::map<int, double>& row_map = vector_of_maps[i];
        for (int j = 0; j < num_variables; ++j) {
            auto it = row_map.find(j);
            if (it != row_map.end()) {
                // If key j exists in the map, set the value in the array
                a[i][j] = it->second;
            } else {
                // If key j does not exist, set to 0 or any default value
                a[i][j] = 0.0; // Default to 0.0
            }
        }
        // setting last column to be the element from c
        a[i][num_variables] = b_vec[i]; 
    }

    return a;
}


// Function to solve our problem: need to give it: phi_vec (vector of maps), b_vec (vector of cutpoints), coef_vec (vector of coefficients), p_cont (int). we will return a vector of doubles, converted from alglib's 1d array
// assumes box -1 < xi < 1 for all i
// assumes all constraints take form ax <= b
// set min = true if we want minimizer instead

std::vector<double> solve_problem(const std::vector<std::map<int, double>>& vector_of_maps,
                            std::vector<double> b_vec,
                            std::vector<double> coef_vec,
                            int p_cont,
                            bool min){
    // checking sizes of inputs
    std::vector<double> empty_vec;
    if (b_vec.size() != vector_of_maps.size()) {
        std::cerr << "Phi and b should be same size." << std::endl;
        return empty_vec; // Return an empty vector
    }
    int num_variables = 0;
    for (const std::map<int, double>& map : vector_of_maps) {
        for (const std::pair<int, double>& pair : map) {
            num_variables = std::max(num_variables, pair.first + 1);
        }
    }
    
    if (p_cont != 0)
    {
        num_variables = p_cont;
    }
    if (coef_vec.size() != p_cont) {
        std::cerr << "c should have size p_cont" << std::endl;
        return empty_vec; // Return an empty vector
    }
    std::vector<double> solution(p_cont);
    // converting vector to 2d array
    real_2d_array a = maps_b_to_array(vector_of_maps, b_vec, p_cont); 
    integer_1d_array ct; // direction of constraints
    ct.setlength(long(a.rows()));
    for (size_t i = 0; i < (long)ct.length(); i++)
    {
        ct[i] = -1;
    }
    real_1d_array c; // coefficients for objective
    c.setlength(p_cont);
    for (size_t i = 0; i < coef_vec.size(); i++)
    {
        c[i] = coef_vec[i];
        if (!min) // if max, then min -c x
        {
            c[i] = -c[i];
        }
        
    }
    real_1d_array s; // scale for each x
    s.setlength(p_cont);
    for (size_t i = 0; i < coef_vec.size(); i++)
    {
        s[i] = 1;
    }
    real_1d_array bndl; // lower bound
    bndl.setlength(p_cont);
    for (size_t i = 0; i < p_cont; i++)
    {
        bndl[i] = -1;
    }
    real_1d_array bndu; // upper bound
    bndu.setlength(p_cont);
    for (size_t i = 0; i < p_cont; i++)
    {
        bndu[i] = 1;
    }
    real_1d_array x;
    minlpstate state;
    minlpreport rep;
    minlpcreate(p_cont, state); // 2 = problem size = number var
    minlpsetcost(state, c); // c = coefficients for cost
    minlpsetbc(state, bndl, bndu); // lower and upper bounds
    minlpsetlc(state, a, ct); // A matrix, and lower and upper bounds
    // Set scale of the parameters.
    minlpsetscale(state, s);

    // Solve
    try
    {
        minlpoptimize(state);
        minlpresults(state, x, rep);
        // printf("%s\n", x.tostring(3).c_str()); // EXPECTED: [1.6,.2]
        // Rcpp::Rcout << "Solution: ";
        // for (size_t i = 0; i < solution.size(); i++)
        // {
        //     Rcpp::Rcout << solution[i] << " ";
        // }
        // Rcpp::Rcout << std::endl;
    }
    catch(alglib::ap_error alglib_exception)
    {
        printf("ALGLIB exception with message '%s'\n", alglib_exception.msg.c_str());
        return solution;
    }
    

   // Rcpp::Rcout << "Solution: ";
    for (int i = 0; i < x.length(); i++)
    {
        solution[i] = x[i];
      //  Rcpp::Rcout << solution[i] << ", ";
    }
    //Rcpp::Rcout << std::endl;
    return solution;
}

// This example demonstrates how to maximize
//
//     F(x0,x1) = 3*x0 + 2*x1 - 1*x2
// aka minimize
//     F(x0,x1) = -3*x0 + -2*x1 + 1*x2
//
// subject to box constraints
//
//     0 <= x0,x1,x2 <= inf 
//
// and general linear constraints
//
//     x0  + x2 <= 2
//     3x0 + x1 + 2x2 <= 5
//     4x0 + 3x1 - x2 <= 7

// test solver
// [[Rcpp::export()]]
int test_solver() {
    // Initialize the vector of maps
    std::vector<std::map<int, double>> phi_vec = {
        {{0, 1}, {2, 1}},
        {{0, 3}, {1, 1}, {2, 2}},
        {{0, 4}, {1, 3}, {2, -1}}
    };
    std::vector<double> b_vec {2, 5, 7};
    std::vector<double> coef_vec {-3, -2, +1, 0};
    int p_cont = 4;
    std::vector<double> solution = solve_problem(phi_vec,
                  b_vec,
                  coef_vec,
                  p_cont,
                  false);
    Rcpp::Rcout << "Max Solution: ";
    for (size_t i = 0; i < solution.size(); i++)
    {
        Rcpp::Rcout << solution[i] << " ";
    }
    Rcpp::Rcout << std::endl;

    solution = solve_problem(phi_vec,
                  b_vec,
                  coef_vec,
                  p_cont,
                  true);
    Rcpp::Rcout << "Min Solution: ";
    for (size_t i = 0; i < solution.size(); i++)
    {
        Rcpp::Rcout << solution[i] << " ";
    }
    Rcpp::Rcout << std::endl;

    return 0;
}

 // This example demonstrates how to minimize/maximize
        //
        //     F(x0,x1) = -0.1*x0 - x1
        //
        // subject to box constraints
        //
        //     -1 <= x0,x1 <= +1 
        //
        // and general linear constraints
        //
        //     x0 - x1 >= -1
        //     x0 + x1 <=  -.8
        // Expect: [-1, 0] if maximize, [-1, .2] if minimize
// [[Rcpp::export()]]
int test_solver2() {
    // Initialize the vector of maps
    std::vector<std::map<int, double>> phi_vec = {
        {{0, 1}, {1, -1}},
        {{0, 1}, {1, 1}}
    };
    std::vector<double> b_vec {-1, -.8};
    std::vector<double> coef_vec {-.1, -1};
    int p_cont = 2;
    bool min = false;
    std::vector<double> solution = solve_problem(phi_vec,
                  b_vec,
                  coef_vec,
                  p_cont,
                  min);
     Rcpp::Rcout << "Max Solution: ";
    for (size_t i = 0; i < solution.size(); i++)
    {
        Rcpp::Rcout << solution[i] << " ";
    }
    Rcpp::Rcout << std::endl;
    min = true;
    solution = solve_problem(phi_vec,
                  b_vec,
                  coef_vec,
                  p_cont,
                  min);
    Rcpp::Rcout << "Min Solution: ";
    for (size_t i = 0; i < solution.size(); i++)
    {
        Rcpp::Rcout << solution[i] << " ";
    }
    Rcpp::Rcout << std::endl;



    return 0;

}


// [[Rcpp::export()]]
int test_maps_b_to_array() {
    // Initialize the vector of maps
    std::vector<std::map<int, double>> phi_vec = {
        {{0, 1}, {2, 1}},
        {{0, 3}, {1, 1}, {2, 2}},
        {{0, 4}, {1, 3}, {2, -1}}
    };
    std::vector<double> b_vec {2, 5};
    int p_cont = 4;
   
    real_2d_array a = maps_b_to_array(phi_vec, b_vec, 4);


    // Rcpp::Rcout << "A matrix: " << std::endl;
    // for(int i=0; i< (long)a.rows();i++) {
	// 	for(int j=0;j< (long)a.cols();j++) {
    //         Rcpp::Rcout << a[i][j] << "  ";
    //     }
	// 	Rcpp::Rcout << std::endl;
	// }
	

    try
    {
        //
        // This example demonstrates how to maximize
        //
        //     F(x0,x1) = 3*x0 + 2*x1 - 1*x2
        // aka minimize
        //     F(x0,x1) = -3*x0 + -2*x1 - 1*x2
        //
        // subject to box constraints
        //
        //     0 <= x0,x1,x2 <= inf 
        //
        // and general linear constraints
        //
        //     x0  + x2 <= 2
        //     3x0 + x1 + 2x2 <= 5
        //     4x0 + 3x1 - x2 <= 7
        //
        // We use dual simplex solver provided by ALGLIB for this task. Box
        // constraints are specified by means of constraint vectors bndl and
        // bndu (we have bndl<=x<=bndu). General linear constraints are
        // specified as AL<=A*x<=AU, with AL/AU being 2x1 vectors and A being
        // 2x2 matrix.
        //
        // NOTE: some/all components of AL/AU can be +-INF, same applies to
        //       bndl/bndu. You can also have AL[I]=AU[i] (as well as
        //       BndL[i]=BndU[i]).
        //
        // real_2d_array a = "[[1,0,1,2],[3,1,2,5],[4,3,-1,7]]";
        integer_1d_array ct = "[-1,-1,-1]"; // constraint types all <=
        real_1d_array c = "[-3,-2,-1,0]"; // make sure length  = p_cont
        real_1d_array s = "[1,1,1,1]"; // make sure length  = p_cont
        real_1d_array bndl = "[0,0,0,0]"; // make sure length  = p_cont
        real_1d_array bndu = "[inf,inf,inf,inf]"; // make sure length  = p_cont
        real_1d_array x;
        minlpstate state;
        minlpreport rep;
        minlpcreate(p_cont, state); // 2 = problem size = number var
        minlpsetcost(state, c); // c = coefficients for cost
        minlpsetbc(state, bndl, bndu); // lower and upper bounds
        minlpsetlc(state, a, ct); // A matrix, and lower and upper bounds
        // Set scale of the parameters.
        minlpsetscale(state, s);

        // Solve
        minlpoptimize(state);
        minlpresults(state, x, rep);
        printf("%s\n", x.tostring(3).c_str()); // EXPECTED: [1.6,.2]
    }
    catch(alglib::ap_error alglib_exception)
    {
        printf("ALGLIB exception with message '%s'\n", alglib_exception.msg.c_str());
        return 1;
    }
    return 0;

}



// [[Rcpp::export()]]
int main()
{
    try
    {
        //
        // This example demonstrates how to minimize
        //
        //     F(x0,x1) = -0.1*x0 - x1
        //
        // subject to box constraints
        //
        //     -1 <= x0,x1 <= +1 
        //
        // and general linear constraints
        //
        //     x0 - x1 >= -1
        //     x0 + x1 <=  1
        //
        // We use dual simplex solver provided by ALGLIB for this task. Box
        // constraints are specified by means of constraint vectors bndl and
        // bndu (we have bndl<=x<=bndu). General linear constraints are
        // specified as AL<=A*x<=AU, with AL/AU being 2x1 vectors and A being
        // 2x2 matrix.
        //
        // NOTE: some/all components of AL/AU can be +-INF, same applies to
        //       bndl/bndu. You can also have AL[I]=AU[i] (as well as
        //       BndL[i]=BndU[i]).
        //
        real_2d_array a = "[[1,-1],[1,1]]";
        real_1d_array al = "[-1,-inf]";
        real_1d_array au = "[+inf,1]";
        real_1d_array c = "[-0.1,-1]";
        real_1d_array s = "[1,1]";
        real_1d_array bndl = "[-1,-1]";
        real_1d_array bndu = "[1,1]";
        real_1d_array x;
        minlpstate state;
        minlpreport rep;

        minlpcreate(2, state); // 2 = problem size

        //
        // Set cost vector, box constraints, general linear constraints.
        //
        // Box constraints can be set in one call to minlpsetbc() or minlpsetbcall()
        // (latter sets same constraints for all variables and accepts two scalars
        // instead of two vectors).
        //
        // General linear constraints can be specified in several ways:
        // * minlpsetlc2dense() - accepts dense 2D array as input; sometimes this
        //   approach is more convenient, although less memory-efficient.
        // * minlpsetlc2() - accepts sparse matrix as input
        // * minlpaddlc2dense() - appends one row to the current set of constraints;
        //   row being appended is specified as dense vector
        // * minlpaddlc2() - appends one row to the current set of constraints;
        //   row being appended is specified as sparse set of elements
        // Independently from specific function being used, LP solver uses sparse
        // storage format for internal representation of constraints.
        //
        minlpsetcost(state, c); // c = coefficients for cost
        minlpsetbc(state, bndl, bndu); // lower and upper bounds
        minlpsetlc2dense(state, a, al, au, 2); // A matrix, and lower and upper bounds

        //
        // Set scale of the parameters.
        //
        // It is strongly recommended that you set scale of your variables.
        // Knowing their scales is essential for evaluation of stopping criteria
        // and for preconditioning of the algorithm steps.
        // You can find more information on scaling at http://www.alglib.net/optimization/scaling.php
        //
        minlpsetscale(state, s);

        // Solve
        minlpoptimize(state);
        minlpresults(state, x, rep);
        printf("%s\n", x.tostring(3).c_str()); // EXPECTED: [0,1]
    }
    catch(alglib::ap_error alglib_exception)
    {
        // printf("ALGLIB exception with message '%s'\n", alglib_exception.msg.c_str());
        return 1;
    }
    return 0;
}

// [[Rcpp::export()]]
int test_max()
{
    try
    {
        //
        // This example demonstrates how to maximize
        //
        //     F(x0,x1) = 3*x0 + 2*x1
        //     aka minimize F(x0, x1) = -3*x0 + 2*x1
        //
        // subject to box constraints
        //
        //     0 <= x0,x1 <= inf 
        //
        // and general linear constraints
        //
        //     x0 - x1 <= 2
        //     3x0 + x1 <= 5
        //     4x0 + 3x1 <= 7
        //
        // We use dual simplex solver provided by ALGLIB for this task. Box
        // constraints are specified by means of constraint vectors bndl and
        // bndu (we have bndl<=x<=bndu). General linear constraints are
        // specified as AL<=A*x<=AU, with AL/AU being 2x1 vectors and A being
        // 2x2 matrix.
        //
        // NOTE: some/all components of AL/AU can be +-INF, same applies to
        //       bndl/bndu. You can also have AL[I]=AU[i] (as well as
        //       BndL[i]=BndU[i]).
        //
        real_2d_array a = "[[1,-1],[3,1],[4,3],[1,0]]";
        real_1d_array al = "[-inf,-inf,-inf,-inf]";
        real_1d_array au = "[2,5,7,1]";
        real_1d_array c = "[-3,-2]";
        real_1d_array s = "[1,1]";
        real_1d_array bndl = "[0,0]";
        real_1d_array bndu = "[inf,inf]";
        real_1d_array x;
        minlpstate state;
        minlpreport rep;

        minlpcreate(2, state); // 2 = problem size
        minlpsetcost(state, c); // c = coefficients for cost
        minlpsetbc(state, bndl, bndu); // lower and upper bounds
        minlpsetlc2dense(state, a, al, au, 4); // A matrix, and lower and upper bounds
        // Set scale of the parameters.
        minlpsetscale(state, s);

        // Solve
        minlpoptimize(state);
        minlpresults(state, x, rep);
        printf("%s\n", x.tostring(3).c_str()); // EXPECTED: [1.6,.2]
    }
    catch(alglib::ap_error alglib_exception)
    {
        printf("ALGLIB exception with message '%s'\n", alglib_exception.msg.c_str());
        return 1;
    }
    return 0;
}


// we actually use one sided constraints. use minlpsetlc. going to now try to add multiple (more than 2 variables)
// [[Rcpp::export()]]
int test_simplex3()
{
    try
    {
        //
        // This example demonstrates how to maximize
        //
        //     F(x0,x1) = 3*x0 + 2*x1 - 1*x2
        // aka minimize
        //     F(x0,x1) = -3*x0 + -2*x1 - 1*x2
        //
        // subject to box constraints
        //
        //     0 <= x0,x1,x2 <= inf 
        //
        // and general linear constraints
        //
        //     x0  + x2 <= 2
        //     3x0 + x1 + 2x2 <= 5
        //     4x0 + 3x1 - x2 <= 7
        //
        // We use dual simplex solver provided by ALGLIB for this task. Box
        // constraints are specified by means of constraint vectors bndl and
        // bndu (we have bndl<=x<=bndu). General linear constraints are
        // specified as AL<=A*x<=AU, with AL/AU being 2x1 vectors and A being
        // 2x2 matrix.
        //
        // NOTE: some/all components of AL/AU can be +-INF, same applies to
        //       bndl/bndu. You can also have AL[I]=AU[i] (as well as
        //       BndL[i]=BndU[i]).
        //
        real_2d_array a = "[[1,0,1,2],[3,1,2,5],[4,3,-1,7]]";
        integer_1d_array ct = "[-1,-1,-1]"; // constraint types all <=
        real_1d_array c = "[-3,-2,-1]";
        real_1d_array s = "[1,1,1]";
        real_1d_array bndl = "[0,0,0]";
        real_1d_array bndu = "[inf,inf,inf]";
        real_1d_array x;
        minlpstate state;
        minlpreport rep;
        minlpcreate(3, state); // 2 = problem size = number var
        minlpsetcost(state, c); // c = coefficients for cost
        minlpsetbc(state, bndl, bndu); // lower and upper bounds
        minlpsetlc(state, a, ct); // A matrix, and lower and upper bounds
        // Set scale of the parameters.
        minlpsetscale(state, s);

        // Solve
        minlpoptimize(state);
        minlpresults(state, x, rep);
        printf("%s\n", x.tostring(3).c_str()); // EXPECTED: [1.6,.2]
    }
    catch(alglib::ap_error alglib_exception)
    {
        printf("ALGLIB exception with message '%s'\n", alglib_exception.msg.c_str());
        return 1;
    }
    return 0;
}


// [[Rcpp::export()]]
int test_write_string() {
    // Initialize the vector of maps
    std::vector<std::map<int, double>> phi_vec = {
        {{0, 1}, {1, -1}},
        {{0, 3}, {1, 1}},
        {{0, 4}, {2, 3}},
        {{1, 1}}
    };
    int p_cont = 3;
    std::vector<double> b_vec{2, 5, 7, .1};
    create_matrix_a(phi_vec, p_cont);
    std::string phi_string = create_a_string(phi_vec, b_vec, p_cont);
    Rcpp::Rcout << " 'a' String: " << phi_string << std::endl;

    //real_2d_array a = phi_string;
    return 0;

}