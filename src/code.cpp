#include <Rcpp.h>
using namespace Rcpp;

List eifSigma(List eif){
  const int n = eif.size();
  List out(n);
  for (int i = 0; i < n; i++) {
    NumericVector cnt = eif[i];
    out[i] = sqrt(var(cnt));
  }
  return out;
}

List Rademacher(const int n, const int reps){
  List out(reps);
  for (int i = 0; i < reps; i++){
    out[i] = 2*rbinom(n, 1, 0.5) - 1;
  }
  return(out);
}

List centerEstim(List x, List eif){
  const int k = eif.size();
  List sig = eifSigma(eif);
  List out(k);
  for (int i = 0; i < k; i++){
    NumericVector e = eif[i];
    double s = sig[i];
    double p = x[i];
    out[i] = (e - p) / s;
  }
  return out;
}

// [[Rcpp::export]]
NumericVector multBoot(List x,
                       List eif,
                       const int n,
                       const int reps){
  List mbs = Rademacher(n, reps);
  List val = centerEstim(x, eif);
  const double denom = sqrt(n);
  NumericVector clct(x.size());
  NumericVector res(reps);
  for (int i = 0; i < reps; i++){
    NumericVector m = mbs[i];
    for (int j = 0; j < x.size(); j++){
      NumericVector v = val[j];
      clct[j] = abs(sum(m * v) / denom);
    }
    res[i] = max(clct);
  }
  return res;
}
