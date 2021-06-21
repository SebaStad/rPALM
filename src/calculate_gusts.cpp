// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
#include <RcppArmadilloExtensions/sample.h>
using namespace Rcpp;
using namespace arma;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
  //
  //   http://www.rcpp.org/
  //   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
  //


  // [[Rcpp::export]]
double meanC(NumericVector x) {
  int n = x.size();
  double total = 0;

  for(int i = 0; i < n; ++i) {
    total += x[i];
  }
  return total / n;
}


// [[Rcpp::export]]
NumericVector getslicecpp(arma::cube wholearray,
                          int x_pos,
                          int y_pos,
                          int z_start,
                          int z_end){

  // NumericVector vecArray(wholearray);
  // IntegerVector arrayDims = vecArray.attr("dim");
  // int col_x = arrayDims[0];
  // int col_y = arrayDims[1];
  // int col_t = arrayDims[2];
  arma::cube data = wholearray;

  int x = x_pos;
  int y = y_pos;
  int zs = z_start;
  int ze = z_end;

  IntegerVector vec = seq(zs, ze);
  arma::mat first_dim = data.row(x);
  NumericVector second_dim = Rcpp::wrap(first_dim.row(y));
  NumericVector end_slice = second_dim[vec];
  // NumericVector sub_slice = Rcpp::wrap(data.col(y).row(x).slices(zs,ze));

  // Rprintf("%i", end_slice.size());

  return end_slice;


}


// [[Rcpp::export]]
NumericVector getslice(NumericVector wholearray,
                       int x_pos,
                       int y_pos,
                       int z_start,
                       int z_end){

  NumericVector vecArray(wholearray);
  IntegerVector arrayDims = vecArray.attr("dim");
  int col_x = arrayDims[0];
  int col_y = arrayDims[1];
  int col_t = arrayDims[2];
  arma::cube data(vecArray.begin(), col_x, col_y, col_t, false);

  int x = x_pos;
  int y = y_pos;
  int zs = z_start;
  int ze = z_end;

  IntegerVector vec = seq(zs, ze);
  arma::mat first_dim = data.row(x);
  NumericVector second_dim = Rcpp::wrap(first_dim.row(y));
  NumericVector end_slice = second_dim[vec];
  // NumericVector sub_slice = Rcpp::wrap(data.col(y).row(x).slices(zs,ze));

  //Rprintf("%i", end_slice.size());

  return end_slice;


}




// [[Rcpp::export]]
NumericVector f_evaluate_gusts(NumericVector myArray,
                               double gust_strength,
                               int gust_mean_period,
                               int gust_persistance,
                               int gust_max_delta_time) {

  NumericVector vecArray(myArray);
  IntegerVector arrayDims = vecArray.attr("dim");
  int col_x = arrayDims[0];
  int col_y = arrayDims[1];
  int col_t = arrayDims[2];
  int maximum_addition_on_timestep = gust_max_delta_time;
  double min_difference_in_wind = gust_strength;
  int period_to_mean = gust_mean_period;
  int max_period_to_loop = gust_persistance;

  double mean_wind;

  int continous_counter = 0;
  int inter_counter = 0;
  int h;

  // time variable!
    // e.g. how long do i look into the future?!
    // --> 1s/5s/10s
  //maximum_addition_on_timestep = gust_max_delta_time;
  // gust strength variable!
    // how large must the change in wind be?
    // --> 5 m/s
  //min_difference_in_wind = gust_strength;

  // how many timesteps does the gust persis?
    //max_period_to_loop = gust_persistance;

    //
      //period_to_mean = gust_mean_period


      // int hist_matrix[col_x][col_y];
      // NumericVector hist_matrix;
      // hist_matrix = arma::zeros(arrayDims[0], arrayDims[1], true, true) ;

      arma::mat hist_matrix(arrayDims[0], arrayDims[1]);
      hist_matrix.zeros();

      arma::cube data(vecArray.begin(), col_x, col_y, col_t, false);

      // loop over both axis
      for (int i = 0; i < col_x; i++) {
        for (int j = 0; j < col_y; j++) {

          // skip NA values
          if(!NumericVector::is_na(data(i,j,1))){
            inter_counter = 0;

            // loop over third, starting with mean_period
            for(int k = period_to_mean; k < (col_t-1 - maximum_addition_on_timestep); k++) {
              continous_counter = 0;
              mean_wind = meanC(getslicecpp(data, i, j, k - period_to_mean + 1, k));

              h = 0;
              do {
                h += 1;
                if(data(i,j,k+h-1 ) - mean_wind >= min_difference_in_wind){
                  continous_counter += 1;
                } else if(continous_counter == 0){
                  continue;
                } else if(continous_counter < max_period_to_loop){
                  continous_counter = 0;
                  continue;
                } else if(continous_counter >= max_period_to_loop & continous_counter <= maximum_addition_on_timestep){
                  inter_counter += 1;
                  k += continous_counter;
                  continous_counter = 0;
                  break;
                } else {
                  k += continous_counter;
                  continous_counter = 0;
                  break;
                }
                // if(continous_counter > maximum_addition_on_timestep){
                //   k += continous_counter;
                //   continous_counter = 0;
                //   break;
                // }
                if((h + k) > col_t - (maximum_addition_on_timestep + 2)){
                  break;
                }
              } while(h < maximum_addition_on_timestep | continous_counter != 0);

              // for(int h = 1; h < (maximum_addition_on_timestep + 2) ; h++){
              //  // wir brauchen hier repeat loop!
              //     // mean_wind = meanC(data(i,j,Rcpp::Range(k - period_to_mean, k)))
              //     // mean_wind = meanC(Rcpp::wrap(data.slice(i).row(j)));
              //
              //     if(data(i,j,k+h-1 ) - mean_wind >= min_difference_in_wind){
              //       continous_counter += 1;
              //
              //     } else if(continous_counter == 0){
              //       continue;
              //    } else if(continous_counter < max_period_to_loop){
              //      continous_counter = 0;
               //       continue;
               //     } else if(continous_counter >= max_period_to_loop & continous_counter <= maximum_addition_on_timestep){
               //       inter_counter += 1;
               //       k += continous_counter;
               //       continous_counter = 0;
               //       break;
               //     } else {
               //       continous_counter = 0;
               //       continue;
               //     }
               //
               //     if(continous_counter > maximum_addition_on_timestep){
               //       k += continous_counter;
               //       continous_counter = 0;
               //       break;
               //     }
               //
               //
               //     if((h + k) > col_t - (maximum_addition_on_timestep + 2)){
               //       break;
               //     }
               // }

              if((k+1) >= (col_t-1 - maximum_addition_on_timestep)){
                break;
              }

            }
            hist_matrix(i,j) = inter_counter ;
          }
        }
      }

      return Rcpp::wrap(hist_matrix);
}
