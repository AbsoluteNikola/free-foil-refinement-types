
/*@ val cassert : arg0:bool[b|b] => int[v|true] */
let cassert = (b) => {
  0
};

/*@ val main : arg0:int[v|true] => int[v|true] */
let main = (x) => {
  let x1 = x + 1;
  let cond = x < x1;
  cassert(cond)
};
main(0)
