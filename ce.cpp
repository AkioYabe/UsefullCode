// [[Rcpp::plugins("cpp11")]]

#include <Rcpp.h>
#include <string>
#include <vector>
#include <time.h>

using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
DataFrame support_persistency(DataFrame df){
  vector<string> pred_y(as<vector<string>> (df["pred_y"]))
  ,pred_y_max(as<vector<string>>(df["pred_y_max"]))
  ,initial_vec(as<vector<string>>(df["initial"]));
  
  vector<double> death_t(as<vector<double>>(df["death_t"]))
    ,death_tp
    ,cancel_t(as<vector<string>>(df["cancel_t"]))
    ,cancel_tp
    ,vanish_t(as<vector<string>>(df["vanish_t"]))
    ,vanish_tp
    ,renew_t(as<vector<string>>(df["renew_t"]))
    ,renew_tp
    ,single_persistency_t(as<vector<string>>(df["single_persistency_t"]))
    ,single_persistency_tp
    ,stock_persistency,stock_persistency_tm;
  
  for(int i=0;i<df.nrows();++i){
    if(pred_y[i]==initial_vec[i]){
      stock_persistency_tm.emplace_back(1.);
    }else{
      stock_persistency_tm.emplace_back(stock_persistency_t[i-1]);
    }
    
    stock_persistency_t.emplace_back(stock_persistency_tm[i]*single_persistency_t[i]);
    
    if(pred_y[i]==pred_y_max[i]){
      death_tp.emplace_back(0.);
      cancel_tp.emplace_back(0.);
      vanish_tp.emplace_back(0.);
      renew_tp.emplace_back(0.);
      single_persistency_tp.emplace_back(0.);
    }else{
      death_tp.emplace_back(death_t[i+1]);
      cancel_tp.emplace_back(cancel_t[i+1]);
      vanish_tp.emplace_back(vanish_t[i+1]);
      renew_tp.emplace_back(renew_t[i+1]);
      single_persistency_tp.emplace_back(single_persistency_t[i+1]);
    }
  }
  
  DataFrame Df=DataFrame::create(
    Named("death_tp")=death_tp
    ,Named("cancel_tp")=cancel_tp
    ,Named("vanish_tp")=vanish_tp
    ,Named("renew_tp")=renew_tp
    ,Named("single_persistency_tp")=single_persistency_tp
    ,Named("stock_persistency_tm")=stock_persistency_tm
    ,Named("stock_persistency_t")=stock_persistency_t);
  )
  return Df;
}
