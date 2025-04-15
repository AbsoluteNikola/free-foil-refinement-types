
type olist('a) =
  | ONil
  | OCons (x:'a, xs:olist('a[v| x < v]))
  ;

/*@ val mkOList : lo:int => hi:int => olist(int[v|lo <= v && v < hi]) */
let rec mkOList = (lo, hi) => {
  let leq = lo < hi;
  if (leq) {
    let lo1 = lo + 1;
    let tl  = mkOList(lo1, hi);
    OCons(lo, tl)
  } else {
    ONil
  }
};
0
